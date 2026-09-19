!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> Kernel density estimation in one dimension: `pf_kde`, which fits a density to an array it
!> retains and answers it exactly anywhere, and `pf_kde_grid`, which accumulates one on a fixed
!> grid of cells from points streamed through it and forgets them.
!!
!! **This module is Arrow-free and must stay that way.** Nothing in its closure reaches
!! `parquet_bindings`; `check_parquet_kde_stays_arrow_free` (tools/check_source_conventions.py)
!! walks the closure, submodules included. Its library tier edges are `parquet_stats`, whose
!! `pf_stddev` and `pf_iqr` are the bandwidth rules' scale and whose exclusion pass and argument
!! checkers are this module's population rules, so that "in the population" means here exactly
!! what it means for every `pf_*` statistic, and `parquet_random`, the generator `%sample` draws
!! from. `tools/module_footprints.txt` records the cost.
!!
!! **`bandwidth` is the standard deviation of the kernel, whichever kernel is chosen.** Each
!! kernel is stored with the scale factor that gives it unit variance, so one bandwidth rule
!! serves every kernel and a number means the same smoothing under all four. The Gaussian is cut
!! at five standard deviations and renormalised, which gives every kernel compact support: the
!! sorted sample is searched for the points within reach of a query, and nothing else is summed.
!!
!! **The adaptive kernel gives each point its own bandwidth**, `h_j = h * (p(x_j)/g)**(-alpha)`,
!! read from a pilot density `p` -- a `pf_kde_grid` at the global bandwidth -- whose geometric mean
!! over its own density is `g`. Each point's kernel keeps unit mass, so the estimate is still a
!! density, and only the point's bandwidth has to be known when it is deposited, which is what lets
!! the streaming form have it too: `pf_kde%fit(adaptive=.true.)` builds the pilot itself, and
!! `pf_kde_grid%init(pilot=)` takes one the caller built in a first pass. Both read it through the
!! same table and the same look-up, so one pilot gives both forms the same bandwidths.
!!
!! **The population rules are the family's**: a null is excluded, then a NaN (by default), then
!! a zero weight; a negative, NaN or infinite weight aborts; `skipnan = .false.` keeps a NaN as a
!! value that makes the whole estimate NaN. The one rule this module adds is the SUPPORT: a point
!! outside `[lower, upper]` is excluded and counted in `n_outside`, and an infinite point is
!! outside every support, bounded or not.
!!
!! **Data conditions are quiet, caller mistakes abort.** An empty population, a constant sample
!! under a bandwidth rule and a poisoned NaN each leave the object fitted with `ok = .false.`,
!! and every query then answers a quiet NaN. Every abort names the binding it came from
!! (`pf_kde%fit: ...`) and is taken under one named `critical`, so a caller fitting one object
!! per thread inside a parallel region aborts once.
!!
!! **`%sample` draws from the estimate, addressed by `(seed, stream, k)`.** Element `k` of a sample
!! reads its own stream of the generator, `pf_random_key(stream, k)` under a key this module derives
!! from `seed` with a label of its own, so a caller's own draws at `(seed, stream)` are untouched, a
!! rejected draw is redrawn from the same element's stream, and a sample is the same bits however it
!! is split among threads.
!!
!! **A fitted object is read-only under every query**, so any number of threads may share one.
!! `%fit` and `%clear` are the only writes to a `pf_kde`; `%init`, `%add`, `%merge` and `%clear` the
!! only writes to a `pf_kde_grid`, whose queries are read-only too. The bulk queries take
!! `threads=`, and every element of them is computed by one thread alone, so their answers are the
!! same bits at every thread count.
!!
!! **Nothing is printed unasked.** `%print` is solicited output, silenced by
!! `verbosity = "silent"` and written where `message_stream` says; that is why this module
!! re-exports the verbosity and message-stream pair.
!!
!! User guide: doc/pages/utilities/kernel-density.md. Design: feature_kde.md.
module parquet_kde

    ! The tier edge. `pf_stddev` and `pf_iqr` are the rules' scale; `stats_compact` applies the
    ! family's exclusion order and hands back the survivors; the three checkers compose the
    ! family's own abort texts from the `what` passed to them. The last four are plumbing that
    ! `parquet_stats` publishes for this module and the `parquet` facade hides.
    use parquet_stats, only : pf_stddev, pf_iqr, stats_compact, stats_check_sizes, &
        stats_check_weight, stats_weight_kind
    ! The sort of the retained sample, and the thread-count resolver `%add` shares with every
    ! threaded pass of the library; `parquet_argsort` is already beneath `parquet_stats`.
    use parquet_argsort, only : pf_argsort, resolve_thread_count
    ! The Gaussian kernel is the library's `phi` and `Phi`, never a second spelling of them.
    use parquet_utils, only : pf_norm_pdf, pf_norm_cdf
    ! `%sample`'s draws: the coordinate-addressed generator, so that a draw is a pure function of
    ! its coordinates and a sample splits among threads anywhere.
    use parquet_random, only : pf_random_at, pf_random_int_at, pf_random_normal_at, pf_random_key
    ! `%print` is solicited output; the verbosity and message-stream pair is re-exported because
    ! this module reads it.
    use parquet_settings_base, only : parquet_output_is_suppressed, parquet_message_unit, &
        parquet_set_verbosity, parquet_get_verbosity, parquet_set_message_stream, &
        parquet_get_message_stream
    use iso_fortran_env, only : int32, int64, real32, real64
    use, intrinsic :: ieee_arithmetic, only : ieee_value, ieee_quiet_nan, ieee_positive_inf, &
        ieee_negative_inf, ieee_is_nan, ieee_is_finite

    implicit none
    private

    public :: pf_kde, pf_kde_grid
    public :: parquet_debug_kde_threads_used, parquet_debug_set_kde_pilot_cells, parquet_debug_kde_fit_nanos
    public :: parquet_set_verbosity, parquet_get_verbosity
    public :: parquet_set_message_stream, parquet_get_message_stream

    ! ---- the kernel table -------------------------------------------------------------------

    !> Kernel codes, resolved once from the `kernel=` token.
    integer, parameter :: KDE_GAUSSIAN = 1, KDE_EPANECHNIKOV = 2, KDE_BSPLINE = 3, KDE_BOX = 4

    !> Each kernel's scale per unit standard deviation: its unit-scale form `K(u)` has variance
    !! `1/C**2`, so `K(z/C)/C` has variance one. The Gaussian is already unit; the Epanechnikov
    !! kernel `3/4 (1 - u**2)` on `|u| < 1` has variance `1/5`; the cubic B-spline on `|u| < 2` and
    !! the box `1/2` on `|u| <= 1` each have variance `1/3`.
    real(real64), parameter :: KDE_SCALE(4) = [1.0_real64, sqrt(5.0_real64), sqrt(3.0_real64), &
        sqrt(3.0_real64)]

    !> Each kernel's support radius in standard deviations: the Gaussian's cut, and `C` times the
    !! unit-scale half-width (1, 2 and 1) for the other three.
    real(real64), parameter :: KDE_RADIUS(4) = [5.0_real64, sqrt(5.0_real64), &
        2.0_real64*sqrt(3.0_real64), sqrt(3.0_real64)]

    !> The Gaussian's cut, in standard deviations.
    real(real64), parameter :: KDE_GAUSS_CUT = 5.0_real64
    !> `Phi(-5)`, the Gaussian mass below the cut: `erfc(5/sqrt(2))/2`, computed at 50 digits.
    real(real64), parameter :: KDE_GAUSS_TAIL = 2.8665157187919391e-7_real64
    !> `Phi(5) - Phi(-5) = erf(5/sqrt(2))`, the mass the cut keeps, by which the cut Gaussian is
    !! renormalised; folded at compile time from the tail above, so the two cannot disagree.
    real(real64), parameter :: KDE_GAUSS_MASS = 1.0_real64 - 2.0_real64*KDE_GAUSS_TAIL

    !> Bandwidth rule codes. `KDE_RULE_EXPLICIT` records that `bandwidth=` was given as a number.
    integer, parameter :: KDE_RULE_EXPLICIT = 0, KDE_RULE_SILVERMAN = 1, KDE_RULE_SCOTT = 2

    !> Boundary correction codes. `KDE_BOUNDARY_NONE` is the unbounded estimate.
    integer, parameter :: KDE_BOUNDARY_NONE = 0, KDE_BOUNDARY_RENORMALISE = 1, &
        KDE_BOUNDARY_REFLECT = 2

    !> The robust scale's divisor, `IQR/1.349`: the interquartile range of a unit normal.
    real(real64), parameter :: KDE_IQR_NORMAL = 1.349_real64

    !> The rules' constants: Silverman's rule of thumb and Scott's normal-reference rule.
    real(real64), parameter :: KDE_SILVERMAN_C = 0.9_real64, KDE_SCOTT_C = 1.06_real64

    ! ---- the adaptive rule --------------------------------------------------------------------

    !> The adaptive rule's default sensitivity: the square root of the pilot's inverse.
    real(real64), parameter :: KDE_ALPHA_DEFAULT = 0.5_real64

    !> The pilot `pf_kde%fit(adaptive=.true.)` builds reaches this many global bandwidths beyond the
    !! extreme points, clipped to the support.
    real(real64), parameter :: KDE_PILOT_REACH = 4.0_real64

    !> Its cells are a quarter of the global bandwidth wide: this many to a bandwidth.
    real(real64), parameter :: KDE_PILOT_PER_H = 4.0_real64

    !> The fewest and the most cells that pilot is given. The lower clamp keeps a sample narrower
    !! than sixteen bandwidths from getting a coarse table; the upper one bounds its memory when
    !! far outliers stretch its range, and its cells are then wider than a quarter bandwidth.
    integer, parameter :: KDE_PILOT_MIN_CELLS = 64, KDE_PILOT_MAX_CELLS = 65536

    !> The most pieces that pilot's deposit is cut into. The cut is decided by the sample and the
    !! pilot alone, never by the team that runs it, so that the fit answers the same bits at every
    !! `threads=`; the team takes the pieces in turn.
    integer(int64), parameter :: KDE_PILOT_PARTS = 64_int64

    ! ---- sampling and the query teams ---------------------------------------------------------

    !> The label `pf_kde%sample` derives its key from `seed` with, so that its draws share no bit
    !! with a caller's own `pf_random_at(seed, ...)` at the same coordinates (`feature_risks.md`
    !! Risk-123: a new construction over the generator takes its own label). Distinct from every
    !! other label in the library; it needs only to differ from them and from 0.
    integer(int64), parameter :: KDE_FAMILY_LABEL = 7089359947230782746_int64

    !> The label `pf_kde_grid%sample` derives its key with: its own, so that the two forms' samples
    !! at one `(seed, stream)` are two draws and not two functions of one uniform.
    integer(int64), parameter :: KDE_GRID_FAMILY_LABEL = 8280789554566260118_int64

    !> How many times a draw the kernel's cut or the support rejects is redrawn from the kernel
    !! before the point's own distribution function is inverted instead. Either way the draw is from
    !! the corrected kernel; the inversion bounds the cost where a kernel far wider than the support
    !! would otherwise reject almost every draw.
    integer, parameter :: KDE_SAMPLE_TRIES = 32

    !> The least work, in kernel evaluations, worth giving one thread of a bulk query's team: about
    !! a few hundred microseconds, well above what opening the team costs.
    real(real64), parameter :: KDE_QUERY_MIN_WORK = 32768.0_real64

    !> What one draw of `%sample` costs, in kernel evaluations, about: a point chosen, a variate
    !! drawn and, rarely, a rejection.
    real(real64), parameter :: KDE_DRAW_WORK = 32.0_real64

    ! ---- the test observable ------------------------------------------------------------------

    !> The team the most recent threaded pass ran on; 1 when it ran serially.
    !!
    !! Read by `parquet_debug_kde_threads_used` and by nothing else. An answer comparison cannot
    !! see threading -- `%add` at one thread and at eight differ only by rounding, and not at all
    !! against a pass that never opened its team -- so the threaded tests assert this beside the
    !! answer. It is written from INSIDE the region, by `omp_get_num_threads`, so that it reports
    !! the team that ran rather than the one that was decided on. Process-global and
    !! unsynchronised, like every counter of its kind.
    integer, save :: kde_team_used = 1

    !> The cell count `parquet_debug_set_kde_pilot_cells` forces on the pilot `pf_kde%fit` builds;
    !! 0, the default, means the rule. Test-only, process-global and unsynchronised: the suite that
    !! sets it runs serially.
    integer, save :: kde_pilot_cells_forced = 0

    !> Nanoseconds the most recent `pf_kde%fit` spent sorting, building the pilot and assigning the
    !! bandwidths (`parquet_debug_kde_fit_nanos`); 0 for a phase it did not reach. Process-global
    !! and unsynchronised, like the team counter.
    integer(int64), save :: kde_fit_ns(3) = 0_int64

    ! ---- the types --------------------------------------------------------------------------

    !> The adaptive rule's state, which both forms hold: the pilot's cells, copied as a look-up
    !! table, and the rule's settings.
    !!
    !! A copy of the pilot's accumulation rather than of the pilot: a `pf_kde_grid` cannot hold one
    !! of its own kind (a recursive allocatable component, which gfortran copies wrongly beyond one
    !! level), and reading the pilot through the same table in both forms is what gives one pilot
    !! the same bandwidths in either.
    type :: kde_adapt
        logical :: on = .false.                   !! the rule is in use
        real(real64) :: alpha = KDE_ALPHA_DEFAULT !! the sensitivity, in `[0, 1]`
        logical :: has_bmax = .false.             !! a cap was given
        real(real64) :: bmax = 0.0_real64         !! the cap on every point's bandwidth
        integer :: nc = 0                         !! the pilot's number of cells
        real(real64) :: x0 = 0.0_real64           !! the pilot's first cell's left edge
        real(real64) :: x1 = 0.0_real64           !! the pilot's last cell's right edge
        real(real64) :: dx = 0.0_real64           !! the pilot's cell width
        real(real64) :: wt = 0.0_real64           !! the pilot's total weight
        logical :: unreadable = .false.           !! the pilot has no density to read: poisoned, or empty cells
        real(real64) :: logg = 0.0_real64         !! `log g`: the mean of `log p` over the pilot's own density
        real(real64) :: pmin = 0.0_real64         !! the pilot's smallest positive cell density
        real(real64), allocatable :: acc(:)
        !! the pilot's accumulation, cell for cell
    end type kde_adapt

    !> A kernel density estimate accumulated on a fixed grid of cells from points streamed through
    !! it, which are forgotten.
    !!
    !! `%init` fixes the geometry -- `ncells` cells of width `step = (xmax - xmin)/ncells`, centred
    !! on `xmin + (i - 1/2)*step` -- with the bandwidth, the kernel and the support, and with
    !! `pilot=` the adaptive rule. Each point `%add` accepts deposits its kernel on the cell centres
    !! it reaches, normalised so that the point adds exactly its weight: what the kernel puts beyond
    !! `xmin` or `xmax` is counted there but not located. `%merge` adds one grid to another. The
    !! queries read the cells: `%density` at the centres, `%pdf` interpolated between them, `%cdf`
    !! its integral and `%quantile` the inverse. The queries are read-only, so any number of
    !! threads may share a finished grid.
    type :: pf_kde_grid
        private
        logical :: initialised = .false.             !! `%init` has run
        logical :: poisoned = .false.                !! a NaN kept by `skipnan = .false.` arrived
        integer :: nc = 0                            !! the number of cells
        real(real64) :: x0 = 0.0_real64              !! `xmin`, the first cell's left edge
        real(real64) :: x1 = 0.0_real64              !! `xmax`, the last cell's right edge
        real(real64) :: dx = 0.0_real64              !! the cell width
        real(real64) :: h = 0.0_real64               !! the bandwidth; the global one when adaptive
        integer :: kernel_code = KDE_GAUSSIAN        !! the kernel
        integer :: boundary_code = KDE_BOUNDARY_NONE !! the boundary correction
        logical :: has_lower = .false.               !! a lower bound was given
        logical :: has_upper = .false.               !! an upper bound was given
        real(real64) :: lo = 0.0_real64              !! the lower bound, when given
        real(real64) :: hi = 0.0_real64              !! the upper bound, when given
        real(real64) :: w_total = 0.0_real64         !! `sum(w)` over every point accepted
        real(real64) :: w_below = 0.0_real64         !! the weight the kernels put below `xmin`
        real(real64) :: w_above = 0.0_real64         !! the weight the kernels put above `xmax`
        integer(int64) :: cnt_all = 0_int64          !! `size(x)` over every `%add`
        integer(int64) :: cnt_valid = 0_int64        !! the population's size
        integer(int64) :: cnt_null = 0_int64         !! excluded as null
        integer(int64) :: cnt_nan = 0_int64          !! excluded as NaN
        integer(int64) :: cnt_out = 0_int64          !! excluded as outside the support
        real(real64), allocatable :: acc(:)
        !! each cell's accumulated weight per unit length; `acc(i)*step` is the weight in cell `i`
        type(kde_adapt) :: adapt
        !! the adaptive rule, from the pilot `%init` was given; off for a fixed bandwidth
    contains
        procedure :: init => grid_init !! fixes the geometry, the bandwidth, the kernel and the support
        generic :: add => grid_add_f64_r0, grid_add_f64_r1, grid_add_f32_r0, grid_add_f32_r1 !! adds points
        procedure, private :: grid_add_f64_r0 !! one `real64` point
        procedure, private :: grid_add_f64_r1 !! an array of `real64` points
        procedure, private :: grid_add_f32_r0 !! one `real32` point, widened
        procedure, private :: grid_add_f32_r1 !! an array of `real32` points, widened
        procedure :: merge => grid_merge !! adds another grid of the same geometry and settings
        procedure :: density => grid_density !! the density at every cell centre
        generic :: pdf => grid_pdf_r0, grid_pdf_r1 !! the density interpolated between the centres
        procedure, private :: grid_pdf_r0 !! at one point
        procedure, private :: grid_pdf_r1 !! at each point of an array
        generic :: cdf => grid_cdf_r0, grid_cdf_r1 !! `P(X <= x)`, the integral of `%pdf`
        procedure, private :: grid_cdf_r0 !! at one point
        procedure, private :: grid_cdf_r1 !! at each point of an array
        generic :: quantile => grid_quantile_r0, grid_quantile_r1 !! the inverse of `%cdf`
        procedure, private :: grid_quantile_r0 !! at one probability
        procedure, private :: grid_quantile_r1 !! at each probability of an array
        generic :: sample => grid_sample_s32, grid_sample_s64 !! draws from the density `%pdf` describes
        procedure, private :: grid_sample_s32 !! `stream` absent or `int32`
        procedure, private :: grid_sample_s64 !! `stream` `int64`
        procedure :: grid => grid_centres !! the cell centres
        procedure :: ncells => grid_ncells !! the number of cells
        procedure :: step => grid_step !! the cell width
        procedure :: bandwidth => grid_bandwidth !! the bandwidth
        procedure :: kernel => grid_kernel_name !! the kernel's token
        procedure :: bounds => grid_bounds !! the support; infinite where unbounded
        procedure :: n => grid_n !! `size(x)` over every `%add`
        procedure :: n_valid => grid_n_valid !! the population's size
        procedure :: n_null => grid_n_null !! how many were excluded as null
        procedure :: n_nan => grid_n_nan !! how many were excluded as NaN
        procedure :: n_outside => grid_n_outside !! how many were outside the support
        procedure :: sum_weights => grid_sum_weights !! `sum(w)` over the population
        procedure :: is_initialised => grid_is_initialised !! `%init` has run
        procedure :: is_adaptive => grid_is_adaptive !! each point takes its own bandwidth
        procedure :: print => grid_print !! a one-block summary
        procedure :: clear => grid_clear !! zeroes the accumulation and keeps the geometry
    end type pf_kde_grid

    !> A kernel density estimate over a retained, sorted sample.
    !!
    !! `%fit` chooses the bandwidth, applies the population rules and keeps the survivors in
    !! ascending order with their weights -- and, when adaptive, each one's own bandwidth and the
    !! pilot they were read from. Every query afterwards is exact: `%pdf` and `%cdf` sum the kernels
    !! of the points within reach of each query point, and `%quantile` inverts `%cdf`. A fitted
    !! object is read-only under every query and may be shared by any number of threads.
    type :: pf_kde
        private
        logical :: fitted = .false.                  !! `%fit` has run since the last `%clear`
        logical :: defined = .false.                 !! the estimate is defined (`ok` of `%fit`)
        integer :: kernel_code = KDE_GAUSSIAN        !! the kernel
        integer :: rule_code = KDE_RULE_SILVERMAN    !! how the bandwidth was chosen
        integer :: boundary_code = KDE_BOUNDARY_NONE !! the boundary correction
        logical :: has_lower = .false.               !! a lower bound was given
        logical :: has_upper = .false.               !! an upper bound was given
        real(real64) :: lo = 0.0_real64              !! the lower bound, when given
        real(real64) :: hi = 0.0_real64              !! the upper bound, when given
        real(real64) :: h = 0.0_real64               !! the global bandwidth, after `adjust`
        real(real64) :: hinv = 0.0_real64            !! one over the global bandwidth
        real(real64) :: hmax = 0.0_real64            !! the largest point bandwidth: how far a query reaches
        real(real64) :: w_total = 0.0_real64         !! `sum(w)` over the population
        logical :: weighted = .false.                !! `weights=` was given
        integer(int64) :: cnt_all = 0_int64          !! `size(x)` at `%fit`
        integer(int64) :: cnt_valid = 0_int64        !! the population's size
        integer(int64) :: cnt_null = 0_int64         !! excluded as null
        integer(int64) :: cnt_nan = 0_int64          !! excluded as NaN
        integer(int64) :: cnt_out = 0_int64          !! excluded as outside the support
        integer(int64) :: hstride = 0_int64          !! 1 when `hb` and `hr` hold one per point, 0 one for all
        real(real64), allocatable :: x(:)
        !! the population, ascending
        real(real64), allocatable :: w(:)
        !! their weights, in the same order; allocated only when weighted
        real(real64), allocatable :: cw(:)
        !! the running sum of `w`, so that `cw(k)` is the weight of `x(1:k)`; allocated only
        !! when weighted, and built here rather than on first use so that no query writes
        real(real64), allocatable :: mass(:)
        !! each point's kernel mass inside the support, by which its kernel is divided; allocated
        !! only when a correction can make it differ from one
        real(real64), allocatable :: hb(:)
        !! each point's bandwidth, in the same order, read at `1 + (j - 1)*hstride`: one per point
        !! when adaptive, and the global bandwidth alone otherwise, so that both estimates run one
        !! loop and `alpha = 0` is the fixed estimate bit for bit
        real(real64), allocatable :: hr(:)
        !! one over each of `hb`, formed once at `%fit` by one scalar division each, so that a query
        !! multiplies by it instead of dividing
        type(kde_adapt) :: adapt
        !! the adaptive rule; off for a fixed bandwidth
        type(pf_kde_grid) :: pilot_grid
        !! the pilot the adaptive rule reads, which `%pilot` hands back; uninitialised otherwise
    contains
        generic :: fit => kde_fit_f64, kde_fit_f32 !! fits the estimate to a `real64` or `real32` sample
        procedure, private :: kde_fit_f64 !! the `real64` sample
        procedure, private :: kde_fit_f32 !! the `real32` sample, widened
        generic :: pdf => kde_pdf_r0, kde_pdf_r1 !! the density at a point or at each of an array
        procedure, private :: kde_pdf_r0 !! at one point
        procedure, private :: kde_pdf_r1 !! at each point of an array
        generic :: cdf => kde_cdf_r0, kde_cdf_r1 !! `P(X <= x)` at a point or at each of an array
        procedure, private :: kde_cdf_r0 !! at one point
        procedure, private :: kde_cdf_r1 !! at each point of an array
        generic :: quantile => kde_quantile_r0, kde_quantile_r1 !! the inverse of `%cdf`
        procedure, private :: kde_quantile_r0 !! at one probability
        procedure, private :: kde_quantile_r1 !! at each probability of an array
        procedure :: curve => kde_curve !! the density on equally spaced points
        generic :: sample => kde_sample_s32, kde_sample_s64 !! draws from the estimate
        procedure, private :: kde_sample_s32 !! `stream` absent or `int32`
        procedure, private :: kde_sample_s64 !! `stream` `int64`
        procedure :: bandwidths => kde_bandwidths !! every retained point's bandwidth, and the points
        generic :: bandwidth_at => kde_bandwidth_at_r0, kde_bandwidth_at_r1 !! the bandwidth the rule gives a point
        procedure, private :: kde_bandwidth_at_r0 !! at one point
        procedure, private :: kde_bandwidth_at_r1 !! at each point of an array
        procedure :: pilot => kde_pilot_copy !! a copy of the pilot an adaptive fit read
        procedure :: bandwidth => kde_bandwidth !! the resolved global bandwidth
        procedure :: kernel => kde_kernel_name !! the kernel's token
        procedure :: rule => kde_rule_name !! the bandwidth rule's token, or `"explicit"`
        procedure :: bounds => kde_bounds !! the support; infinite where unbounded
        procedure :: n => kde_n !! `size(x)` at `%fit`
        procedure :: n_valid => kde_n_valid !! the population's size
        procedure :: n_null => kde_n_null !! how many were excluded as null
        procedure :: n_nan => kde_n_nan !! how many were excluded as NaN
        procedure :: n_outside => kde_n_outside !! how many were outside the support
        procedure :: sum_weights => kde_sum_weights !! `sum(w)` over the population
        procedure :: is_fitted => kde_is_fitted !! `%fit` has run since the last `%clear`
        procedure :: is_adaptive => kde_is_adaptive !! the fit is adaptive
        procedure :: print => kde_print !! a one-block summary
        procedure :: clear => kde_clear !! releases everything; the object is unfitted again
    end type pf_kde

    ! ---- fitting, implemented in parquet_kde_fit.f90 ----------------------------------------

    interface

        !> Fits the estimate to a `real64` sample: `call k%fit(x, [bandwidth], [rule], [adjust],
        !! [kernel], [adaptive], [alpha], [bandwidth_max], [lower], [upper], [boundary], [is_valid],
        !! [weights], [weight_type], [skipnan], [n_null], [n_nan], [n_outside], [ok], [threads])`.
        !!
        !! `x` is the sample, retained as a sorted copy of its population. `bandwidth` is the
        !! kernel's standard deviation, a finite positive number; without it the bandwidth comes
        !! from `rule`, `"silverman"` (the default) or `"scott"`, and the two cannot both be given.
        !! `adjust` multiplies the bandwidth however it was chosen (default 1). `kernel` is
        !! `"gaussian"` (the default), `"epanechnikov"`, `"bspline"` or `"box"`. `adaptive =
        !! .true.` gives each point its own bandwidth, `h * (p(x_j)/g)**(-alpha)`, from a pilot
        !! estimate at the global bandwidth `h`: `alpha` in `[0, 1]` (default 0.5) sets how far
        !! the bandwidths follow the pilot, `0` being the fixed estimate, and `bandwidth_max` caps
        !! every one; both need `adaptive = .true.`. `lower` and `upper` bound the support: a
        !! point outside is excluded and counted in `n_outside`, and a kernel crossing a bound is
        !! corrected by `boundary`, `"renormalise"` (the default) or `"reflect"`. `is_valid`,
        !! `weights`, `weight_type` and `skipnan` are the `pf_*` family's population arguments;
        !! `weight_type` decides the effective sample size the rules use. `n_null`, `n_nan` and
        !! `n_outside` report what each exclusion removed, and `ok` is `.false.` when the estimate
        !! is undefined. `threads` is the team for the sort, the rules' statistics and the pilot;
        !! the answer does not depend on it. Tokens are matched without regard to case.
        module subroutine kde_fit_f64(self, x, bandwidth, rule, adjust, kernel, adaptive, alpha, &
                bandwidth_max, lower, upper, boundary, is_valid, weights, weight_type, skipnan, n_null, &
                n_nan, n_outside, ok, threads)
            implicit none
            class(pf_kde), intent(inout)           :: self          !! the estimate; refitted
            real(real64), intent(in)               :: x(:)          !! the sample
            real(real64), intent(in), optional     :: bandwidth     !! the kernel's standard deviation
            character(len=*), intent(in), optional :: rule          !! `"silverman"` or `"scott"`
            real(real64), intent(in), optional     :: adjust        !! a factor on the bandwidth
            character(len=*), intent(in), optional :: kernel        !! the kernel's token
            logical, intent(in), optional          :: adaptive      !! each point takes its own bandwidth
            real(real64), intent(in), optional     :: alpha         !! the adaptive rule's sensitivity
            real(real64), intent(in), optional     :: bandwidth_max !! caps every point's bandwidth
            real(real64), intent(in), optional     :: lower         !! the support's lower bound
            real(real64), intent(in), optional     :: upper         !! the support's upper bound
            character(len=*), intent(in), optional :: boundary      !! the boundary correction
            logical, intent(in), optional          :: is_valid(:)   !! `.false.` marks a null
            real(real64), intent(in), optional     :: weights(:)    !! per-element weights
            character(len=*), intent(in), optional :: weight_type   !! `"reliability"` or `"frequency"`
            logical, intent(in), optional          :: skipnan       !! `.false.` lets a NaN poison
            integer(int64), intent(out), optional  :: n_null        !! excluded as null
            integer(int64), intent(out), optional  :: n_nan         !! excluded as NaN
            integer(int64), intent(out), optional  :: n_outside     !! excluded as outside the support
            logical, intent(out), optional         :: ok            !! the estimate is defined
            integer, intent(in), optional          :: threads       !! the team for the sort and the pilot
        end subroutine kde_fit_f64

        !> The `real32` sample, widened to `real64` first; every other argument as the `real64`
        !! form.
        module subroutine kde_fit_f32(self, x, bandwidth, rule, adjust, kernel, adaptive, alpha, &
                bandwidth_max, lower, upper, boundary, is_valid, weights, weight_type, skipnan, n_null, &
                n_nan, n_outside, ok, threads)
            implicit none
            class(pf_kde), intent(inout)           :: self          !! the estimate; refitted
            real(real32), intent(in)               :: x(:)          !! the sample
            real(real64), intent(in), optional     :: bandwidth     !! the kernel's standard deviation
            character(len=*), intent(in), optional :: rule          !! `"silverman"` or `"scott"`
            real(real64), intent(in), optional     :: adjust        !! a factor on the bandwidth
            character(len=*), intent(in), optional :: kernel        !! the kernel's token
            logical, intent(in), optional          :: adaptive      !! each point takes its own bandwidth
            real(real64), intent(in), optional     :: alpha         !! the adaptive rule's sensitivity
            real(real64), intent(in), optional     :: bandwidth_max !! caps every point's bandwidth
            real(real64), intent(in), optional     :: lower         !! the support's lower bound
            real(real64), intent(in), optional     :: upper         !! the support's upper bound
            character(len=*), intent(in), optional :: boundary      !! the boundary correction
            logical, intent(in), optional          :: is_valid(:)   !! `.false.` marks a null
            real(real64), intent(in), optional     :: weights(:)    !! per-element weights
            character(len=*), intent(in), optional :: weight_type   !! `"reliability"` or `"frequency"`
            logical, intent(in), optional          :: skipnan       !! `.false.` lets a NaN poison
            integer(int64), intent(out), optional  :: n_null        !! excluded as null
            integer(int64), intent(out), optional  :: n_nan         !! excluded as NaN
            integer(int64), intent(out), optional  :: n_outside     !! excluded as outside the support
            logical, intent(out), optional         :: ok            !! the estimate is defined
            integer, intent(in), optional          :: threads       !! the team for the sort and the pilot
        end subroutine kde_fit_f32

    end interface

    ! ---- queries, implemented in parquet_kde_fit.f90 ----------------------------------------

    interface

        !> The density at one point: `call k%pdf(x, f)`. Exact: the sum of the kernels of every
        !! retained point within reach of `x`. Zero outside the support; a quiet NaN when the
        !! estimate is undefined.
        module subroutine kde_pdf_r0(self, x, f)
            implicit none
            class(pf_kde), intent(in) :: self !! the fitted estimate
            real(real64), intent(in)  :: x    !! where to evaluate
            real(real64), intent(out) :: f    !! the density there
        end subroutine kde_pdf_r0

        !> The density at each point of `x`: `call k%pdf(x, f, [threads])`, `f(i)` at `x(i)`.
        !! `size(f) /= size(x)` aborts. `threads` is the team the points are shared among; each
        !! point is computed by one thread alone, so the answer does not depend on it, and a team
        !! opens only where the work would pay for it.
        module subroutine kde_pdf_r1(self, x, f, threads)
            implicit none
            class(pf_kde), intent(in)     :: self    !! the fitted estimate
            real(real64), intent(in)      :: x(:)    !! where to evaluate
            real(real64), intent(out)     :: f(:)    !! the density at each point
            integer, intent(in), optional :: threads !! the team for the points
        end subroutine kde_pdf_r1

        !> `P(X <= x)` at one point: `call k%cdf(x, p)`. Exact: the sum of the kernels' own CDFs,
        !! boundary-corrected, so that it is 0 at and below the lower end of the support and 1 at
        !! and above the upper end.
        module subroutine kde_cdf_r0(self, x, p)
            implicit none
            class(pf_kde), intent(in) :: self !! the fitted estimate
            real(real64), intent(in)  :: x    !! where to evaluate
            real(real64), intent(out) :: p    !! the probability at or below `x`
        end subroutine kde_cdf_r0

        !> `P(X <= x)` at each point of `x`: `call k%cdf(x, p, [threads])`. `size(p) /= size(x)`
        !! aborts; `threads` as `%pdf` takes it.
        module subroutine kde_cdf_r1(self, x, p, threads)
            implicit none
            class(pf_kde), intent(in)     :: self    !! the fitted estimate
            real(real64), intent(in)      :: x(:)    !! where to evaluate
            real(real64), intent(out)     :: p(:)    !! the probability at or below each point
            integer, intent(in), optional :: threads !! the team for the points
        end subroutine kde_cdf_r1

        !> The quantile at probability `p`: `call k%quantile(p, x)`, the smallest `x` at which
        !! `%cdf` reaches `p`. `p = 0` and `p = 1` give the two ends of the estimate's support.
        !! `p` outside `[0, 1]`, or NaN, aborts.
        module subroutine kde_quantile_r0(self, p, x)
            implicit none
            class(pf_kde), intent(in) :: self !! the fitted estimate
            real(real64), intent(in)  :: p    !! the probability
            real(real64), intent(out) :: x    !! the quantile
        end subroutine kde_quantile_r0

        !> The quantile at each probability of `p`: `call k%quantile(p, x, [threads])`.
        !! `size(x) /= size(p)` aborts; `threads` as `%pdf` takes it.
        module subroutine kde_quantile_r1(self, p, x, threads)
            implicit none
            class(pf_kde), intent(in)     :: self    !! the fitted estimate
            real(real64), intent(in)      :: p(:)    !! the probabilities
            real(real64), intent(out)     :: x(:)    !! the quantile at each
            integer, intent(in), optional :: threads !! the team for the probabilities
        end subroutine kde_quantile_r1

        !> The density on `size(x)` equally spaced points: `call k%curve(x, f, [xmin], [xmax],
        !! [cut], [threads])`.
        !!
        !! `x` receives the points, from `xmin` to `xmax` inclusive, and `f` the density at each.
        !! The default range is the sample's minimum minus `cut` bandwidths to its maximum plus
        !! `cut` bandwidths (`cut` defaults to 3), clipped to the support. `size(f) /= size(x)`,
        !! `xmin >= xmax` and a negative `cut` abort. On an undefined estimate `f` is NaN, and so
        !! is `x` unless both ends were given. `threads` as `%pdf` takes it.
        module subroutine kde_curve(self, x, f, xmin, xmax, cut, threads)
            implicit none
            class(pf_kde), intent(in)          :: self    !! the fitted estimate
            real(real64), intent(out)          :: x(:)    !! the points
            real(real64), intent(out)          :: f(:)    !! the density at each point
            real(real64), intent(in), optional :: xmin    !! the first point
            real(real64), intent(in), optional :: xmax    !! the last point
            real(real64), intent(in), optional :: cut     !! bandwidths beyond the data, by default
            integer, intent(in), optional      :: threads !! the team for the points
        end subroutine kde_curve

        !> Draws from the estimate: `call k%sample(v, seed, [stream], [threads])` fills `v` with
        !! `size(v)` independent draws, `stream` absent (stream 0) or `int32`.
        !!
        !! The smoothed bootstrap: a retained point chosen in proportion to its weight, then its
        !! kernel, at its own bandwidth, drawn about it. Under `"renormalise"` a draw beyond a bound
        !! is drawn again from the same point's kernel, and under `"reflect"` it is mirrored back,
        !! so the draws follow the corrected estimate `%pdf` describes. Element `k` of `v` is a
        !! function of `seed`, `stream` and `k` alone: a longer sample starts with a shorter one,
        !! and `threads` changes nothing but the time. `v` is NaN on an undefined estimate.
        module subroutine kde_sample_s32(self, v, seed, stream, threads)
            implicit none
            class(pf_kde), intent(in)            :: self    !! the fitted estimate
            real(real64), intent(out)            :: v(:)    !! the draws
            integer(int64), intent(in)           :: seed    !! the seed
            integer(int32), intent(in), optional :: stream  !! the stream; 0 when absent
            integer, intent(in), optional        :: threads !! the team for the draws
        end subroutine kde_sample_s32

        !> Draws from the estimate with an `int64` `stream`: `call k%sample(v, seed, stream,
        !! [threads])`; otherwise as the `int32` form, and the two agree at every stream they share.
        module subroutine kde_sample_s64(self, v, seed, stream, threads)
            implicit none
            class(pf_kde), intent(in)     :: self    !! the fitted estimate
            real(real64), intent(out)     :: v(:)    !! the draws
            integer(int64), intent(in)    :: seed    !! the seed
            integer(int64), intent(in)    :: stream  !! the stream
            integer, intent(in), optional :: threads !! the team for the draws
        end subroutine kde_sample_s64

        !> Every retained point's bandwidth: `call k%bandwidths(h, [x])`, in the object's own
        !! ascending order, with the points themselves in `x` when it is given.
        !!
        !! Every bandwidth is the global one unless the fit is adaptive. `size(h)` and `size(x)`
        !! other than `%n_valid()` abort. On an undefined estimate `h` is NaN, and so is `x` where
        !! no point was kept.
        module subroutine kde_bandwidths(self, h, x)
            implicit none
            class(pf_kde), intent(in)           :: self !! the fitted estimate
            real(real64), intent(out)           :: h(:) !! each retained point's bandwidth
            real(real64), intent(out), optional :: x(:) !! the retained points, ascending
        end subroutine kde_bandwidths

        !> The bandwidth the adaptive rule gives a point at `x`: `call k%bandwidth_at(x, h)`, read
        !! from the pilot exactly as `%fit` read it for each retained point; the global bandwidth
        !! when the fit is not adaptive. A quiet NaN at a NaN `x` or on an undefined estimate.
        module subroutine kde_bandwidth_at_r0(self, x, h)
            implicit none
            class(pf_kde), intent(in) :: self !! the fitted estimate
            real(real64), intent(in)  :: x    !! the point
            real(real64), intent(out) :: h    !! its bandwidth
        end subroutine kde_bandwidth_at_r0

        !> The bandwidth the rule gives each point of `x`: `call k%bandwidth_at(x, h)`.
        !! `size(h) /= size(x)` aborts.
        module subroutine kde_bandwidth_at_r1(self, x, h)
            implicit none
            class(pf_kde), intent(in) :: self !! the fitted estimate
            real(real64), intent(in)  :: x(:) !! the points
            real(real64), intent(out) :: h(:) !! the bandwidth at each
        end subroutine kde_bandwidth_at_r1

        !> A copy of the pilot an adaptive fit read its bandwidths from: `call k%pilot(g)`. Given to
        !! `pf_kde_grid%init(pilot=g)` with the same global bandwidth and `alpha`, it gives every
        !! point the bandwidth `%fit` gave it. Aborts on a fit that is not adaptive; an adaptive fit
        !! with nothing to build a pilot from answers a grid that was never initialised.
        module subroutine kde_pilot_copy(self, g)
            implicit none
            class(pf_kde), intent(in)       :: self !! the fitted estimate
            type(pf_kde_grid), intent(out)  :: g    !! the pilot
        end subroutine kde_pilot_copy

        !> The resolved global bandwidth, after `adjust`: the kernel's standard deviation. NaN
        !! when a rule could not produce one.
        module function kde_bandwidth(self) result(h)
            implicit none
            class(pf_kde), intent(in) :: self !! the fitted estimate
            real(real64)              :: h    !! the bandwidth
        end function kde_bandwidth

        !> The kernel in use, as its token: `call k%kernel(name)`.
        module subroutine kde_kernel_name(self, name)
            implicit none
            class(pf_kde), intent(in)                  :: self !! the fitted estimate
            character(len=:), allocatable, intent(out) :: name !! the kernel's token
        end subroutine kde_kernel_name

        !> How the bandwidth was chosen, as a token: `call k%rule(name)` answers `"silverman"` or
        !! `"scott"`, or `"explicit"` when `bandwidth=` was given as a number.
        module subroutine kde_rule_name(self, name)
            implicit none
            class(pf_kde), intent(in)                  :: self !! the fitted estimate
            character(len=:), allocatable, intent(out) :: name !! the rule's token
        end subroutine kde_rule_name

        !> The support: `call k%bounds(lo, hi)`, with `-Infinity`/`+Infinity` where unbounded.
        module subroutine kde_bounds(self, lo, hi)
            implicit none
            class(pf_kde), intent(in) :: self !! the fitted estimate
            real(real64), intent(out) :: lo   !! the lower end of the support
            real(real64), intent(out) :: hi   !! the upper end of the support
        end subroutine kde_bounds

        !> `size(x)` at `%fit`.
        module function kde_n(self) result(n)
            implicit none
            class(pf_kde), intent(in) :: self !! the fitted estimate
            integer(int64)            :: n    !! the count
        end function kde_n

        !> The population's size: what survived every exclusion.
        module function kde_n_valid(self) result(n)
            implicit none
            class(pf_kde), intent(in) :: self !! the fitted estimate
            integer(int64)            :: n    !! the count
        end function kde_n_valid

        !> How many elements were excluded as null.
        module function kde_n_null(self) result(n)
            implicit none
            class(pf_kde), intent(in) :: self !! the fitted estimate
            integer(int64)            :: n    !! the count
        end function kde_n_null

        !> How many elements were excluded as NaN and were not already null.
        module function kde_n_nan(self) result(n)
            implicit none
            class(pf_kde), intent(in) :: self !! the fitted estimate
            integer(int64)            :: n    !! the count
        end function kde_n_nan

        !> How many elements were excluded as outside the support, or infinite.
        module function kde_n_outside(self) result(n)
            implicit none
            class(pf_kde), intent(in) :: self !! the fitted estimate
            integer(int64)            :: n    !! the count
        end function kde_n_outside

        !> `sum(w)` over the population; the population's size when unweighted.
        module function kde_sum_weights(self) result(s)
            implicit none
            class(pf_kde), intent(in) :: self !! the fitted estimate
            real(real64)              :: s    !! the total weight
        end function kde_sum_weights

        !> `.true.` once `%fit` has run, until `%clear`. Never aborts.
        module function kde_is_fitted(self) result(res)
            implicit none
            class(pf_kde), intent(in) :: self !! the estimate
            logical                   :: res  !! it is fitted
        end function kde_is_fitted

        !> `.true.` when the fit was asked for the adaptive kernel.
        module function kde_is_adaptive(self) result(res)
            implicit none
            class(pf_kde), intent(in) :: self !! the fitted estimate
            logical                   :: res  !! the fit is adaptive
        end function kde_is_adaptive

        !> Writes a one-block summary: `call k%print([unit])`. The kernel, the rule, the
        !! bandwidth, the counts, the support and the correction, and the adaptive rule's settings
        !! and the range of its bandwidths. Silenced by
        !! `verbosity = "silent"`; written to `unit` or, by default, where `message_stream` says.
        !! An unfitted object prints one line saying so rather than aborting.
        module subroutine kde_print(self, unit)
            implicit none
            class(pf_kde), intent(in)     :: self !! the estimate
            integer, intent(in), optional :: unit !! where to write
        end subroutine kde_print

        !> Releases everything; the object is unfitted again.
        module subroutine kde_clear(self)
            implicit none
            class(pf_kde), intent(inout) :: self !! the estimate
        end subroutine kde_clear

    end interface

    ! ---- the grid: set-up and accumulation, implemented in parquet_kde_grid.f90 ---------------

    interface

        !> Fixes the grid: `call g%init(ncells, xmin, xmax, bandwidth, [kernel], [pilot], [alpha],
        !! [bandwidth_max], [lower], [upper], [boundary])`.
        !!
        !! `ncells` cells of width `step = (xmax - xmin)/ncells` span `[xmin, xmax]`, cell `i`
        !! centred on `xmin + (i - 1/2)*step`. `bandwidth` is the kernel's standard deviation and is
        !! always a number: a grid has no data to apply a rule to before the points arrive. `pilot`
        !! selects the adaptive kernel: an initialised grid whose range covers this one's, from which
        !! each point `%add` accepts takes the bandwidth `bandwidth * (p(x)/g)**(-alpha)`; it is
        !! copied, so the caller may discard it. A pilot with no density to read -- nothing in its
        !! cells, or a kept NaN -- makes every answer of this grid NaN, as a data condition does
        !! everywhere else. `alpha` (default 0.5) and `bandwidth_max` are
        !! as `pf_kde%fit` takes them and need `pilot`. `kernel`, `lower`, `upper` and `boundary`
        !! are as `pf_kde%fit` takes them, and the range must lie inside the support. Every
        !! accumulated point and count is discarded; a grid may be initialised again.
        module subroutine grid_init(self, ncells, xmin, xmax, bandwidth, kernel, pilot, alpha, &
                bandwidth_max, lower, upper, boundary)
            implicit none
            class(pf_kde_grid), intent(inout)       :: self          !! the grid; reset
            integer, intent(in)                     :: ncells        !! cells, at least 1; held in memory
            real(real64), intent(in)                :: xmin          !! the first cell's left edge
            real(real64), intent(in)                :: xmax          !! the last cell's right edge
            real(real64), intent(in)                :: bandwidth     !! the kernel's standard deviation
            character(len=*), intent(in), optional  :: kernel        !! the kernel's token
            type(pf_kde_grid), intent(in), optional :: pilot         !! the pilot of the adaptive kernel
            real(real64), intent(in), optional      :: alpha         !! the adaptive rule's sensitivity
            real(real64), intent(in), optional      :: bandwidth_max !! caps every point's bandwidth
            real(real64), intent(in), optional      :: lower         !! the support's lower bound
            real(real64), intent(in), optional      :: upper         !! the support's upper bound
            character(len=*), intent(in), optional  :: boundary      !! the boundary correction
        end subroutine grid_init

        !> Accumulates one `real64` point: `call g%add(x, [is_valid], [weights], [skipnan],
        !! [n_null], [n_nan], [n_outside])`, with `is_valid` and `weights` scalars; otherwise as the
        !! array form.
        module subroutine grid_add_f64_r0(self, x, is_valid, weights, skipnan, n_null, n_nan, n_outside)
            implicit none
            class(pf_kde_grid), intent(inout)     :: self      !! the grid
            real(real64), intent(in)              :: x         !! the point
            logical, intent(in), optional         :: is_valid  !! `.false.` marks it null
            real(real64), intent(in), optional    :: weights   !! its weight
            logical, intent(in), optional         :: skipnan   !! `.false.` lets a NaN poison
            integer(int64), intent(out), optional :: n_null    !! excluded as null
            integer(int64), intent(out), optional :: n_nan     !! excluded as NaN
            integer(int64), intent(out), optional :: n_outside !! excluded as outside the support
        end subroutine grid_add_f64_r0

        !> Accumulates an array of `real64` points: `call g%add(x, [is_valid], [weights],
        !! [skipnan], [n_null], [n_nan], [n_outside], [threads])`.
        !!
        !! `is_valid`, `weights` and `skipnan` are the `pf_*` family's population arguments, and a
        !! point outside the support is excluded and counted; `n_null`, `n_nan` and `n_outside`
        !! report what THIS call excluded, while the grid's accessors report every call's total. A
        !! NaN kept by `skipnan = .false.` makes every later answer of the grid NaN. `threads`
        !! gives each thread of a team a static share of the points and a private partial grid,
        !! summed in thread order: at a given count the bits do not change, and between counts
        !! they differ by rounding.
        module subroutine grid_add_f64_r1(self, x, is_valid, weights, skipnan, n_null, n_nan, n_outside, &
                threads)
            implicit none
            class(pf_kde_grid), intent(inout)     :: self        !! the grid
            real(real64), intent(in)              :: x(:)        !! the points
            logical, intent(in), optional         :: is_valid(:) !! `.false.` marks a null
            real(real64), intent(in), optional    :: weights(:)  !! per-element weights
            logical, intent(in), optional         :: skipnan     !! `.false.` lets a NaN poison
            integer(int64), intent(out), optional :: n_null      !! excluded as null
            integer(int64), intent(out), optional :: n_nan       !! excluded as NaN
            integer(int64), intent(out), optional :: n_outside   !! excluded as outside the support
            integer, intent(in), optional         :: threads     !! the team for the deposit
        end subroutine grid_add_f64_r1

        !> Accumulates one `real32` point, widened to `real64` first; otherwise as the `real64` form.
        module subroutine grid_add_f32_r0(self, x, is_valid, weights, skipnan, n_null, n_nan, n_outside)
            implicit none
            class(pf_kde_grid), intent(inout)     :: self      !! the grid
            real(real32), intent(in)              :: x         !! the point
            logical, intent(in), optional         :: is_valid  !! `.false.` marks it null
            real(real64), intent(in), optional    :: weights   !! its weight
            logical, intent(in), optional         :: skipnan   !! `.false.` lets a NaN poison
            integer(int64), intent(out), optional :: n_null    !! excluded as null
            integer(int64), intent(out), optional :: n_nan     !! excluded as NaN
            integer(int64), intent(out), optional :: n_outside !! excluded as outside the support
        end subroutine grid_add_f32_r0

        !> Accumulates an array of `real32` points, widened to `real64` first; otherwise as the
        !! `real64` form.
        module subroutine grid_add_f32_r1(self, x, is_valid, weights, skipnan, n_null, n_nan, n_outside, &
                threads)
            implicit none
            class(pf_kde_grid), intent(inout)     :: self        !! the grid
            real(real32), intent(in)              :: x(:)        !! the points
            logical, intent(in), optional         :: is_valid(:) !! `.false.` marks a null
            real(real64), intent(in), optional    :: weights(:)  !! per-element weights
            logical, intent(in), optional         :: skipnan     !! `.false.` lets a NaN poison
            integer(int64), intent(out), optional :: n_null      !! excluded as null
            integer(int64), intent(out), optional :: n_nan       !! excluded as NaN
            integer(int64), intent(out), optional :: n_outside   !! excluded as outside the support
            integer, intent(in), optional         :: threads     !! the team for the deposit
        end subroutine grid_add_f32_r1

        !> Adds another grid's accumulation and counts to this one: `call g%merge(other)`. The two
        !! must share the number of cells, the range, the bandwidth, the kernel, the support, the
        !! boundary correction and the pilot -- the same copy, cell for cell, with the same `alpha`
        !! and `bandwidth_max` -- or the call aborts naming the first that differs. One grid per
        !! thread, merged at the end, is how a caller's own threads accumulate one estimate.
        module subroutine grid_merge(self, other)
            implicit none
            class(pf_kde_grid), intent(inout) :: self  !! the grid that receives
            class(pf_kde_grid), intent(in)    :: other !! the grid that is added; unchanged
        end subroutine grid_merge

    end interface

    ! ---- the grid: queries and accessors, implemented in parquet_kde_grid.f90 -----------------

    interface

        !> The density at every cell centre: `call g%density(f, [x], [normalise])`.
        !!
        !! `f(i)` is the accumulation in cell `i` over the total weight, which is the estimate at
        !! the cell's centre; `x`, when given, receives the centres. `normalise = .false.` skips the
        !! division and answers the weighted count per unit length. A grid with nothing
        !! accumulated answers zeros; a grid a kept NaN has poisoned answers NaN. `size(f)` and
        !! `size(x)` other than `ncells` abort.
        module subroutine grid_density(self, f, x, normalise)
            implicit none
            class(pf_kde_grid), intent(in)      :: self      !! the grid
            real(real64), intent(out)           :: f(:)      !! the density at each centre
            real(real64), intent(out), optional :: x(:)      !! the centres
            logical, intent(in), optional       :: normalise !! `.false.` answers the raw accumulation
        end subroutine grid_density

        !> The density at one point, interpolated between the cell centres: `call g%pdf(x, f)`.
        !!
        !! Linear between neighbouring centres, constant over the outer half of the first and the
        !! last cell, and zero outside `[xmin, xmax]`; exact at every centre. A quiet NaN when
        !! nothing has been accumulated or a kept NaN has poisoned the grid.
        module subroutine grid_pdf_r0(self, x, f)
            implicit none
            class(pf_kde_grid), intent(in) :: self !! the grid
            real(real64), intent(in)       :: x    !! where to evaluate
            real(real64), intent(out)      :: f    !! the density there
        end subroutine grid_pdf_r0

        !> The interpolated density at each point of `x`: `call g%pdf(x, f)`. `size(f) /= size(x)`
        !! aborts.
        module subroutine grid_pdf_r1(self, x, f)
            implicit none
            class(pf_kde_grid), intent(in) :: self !! the grid
            real(real64), intent(in)       :: x(:) !! where to evaluate
            real(real64), intent(out)      :: f(:) !! the density at each point
        end subroutine grid_pdf_r1

        !> `P(X <= x)` at one point: `call g%cdf(x, p)`, the integral of `%pdf` from `xmin` plus the
        !! weight the kernels put below `xmin`, over the total weight.
        !!
        !! Below `xmin` it is that weight's share and above `xmax` one less the share put above:
        !! weight outside the range is counted, not located. Exactly 0 at and below a lower bound
        !! and exactly 1 at and above an upper one. A quiet NaN when nothing has been accumulated or
        !! a kept NaN has poisoned the grid.
        module subroutine grid_cdf_r0(self, x, p)
            implicit none
            class(pf_kde_grid), intent(in) :: self !! the grid
            real(real64), intent(in)       :: x    !! where to evaluate
            real(real64), intent(out)      :: p    !! the probability at or below `x`
        end subroutine grid_cdf_r0

        !> `P(X <= x)` at each point of `x`: `call g%cdf(x, p)`. `size(p) /= size(x)` aborts.
        module subroutine grid_cdf_r1(self, x, p)
            implicit none
            class(pf_kde_grid), intent(in) :: self !! the grid
            real(real64), intent(in)       :: x(:) !! where to evaluate
            real(real64), intent(out)      :: p(:) !! the probability at or below each point
        end subroutine grid_cdf_r1

        !> The quantile at probability `p`: `call g%quantile(p, x)`, the smallest `x` in
        !! `[xmin, xmax]` at which `%cdf` reaches `p`, found by solving the quadratic `%cdf` is
        !! between two centres.
        !!
        !! A quantile the weight below `xmin` already covers answers `xmin`, and one only the
        !! weight above `xmax` reaches answers `xmax`. `p = 0` and `p = 1` answer the two ends of
        !! the accumulated density. `p` outside `[0, 1]`, or NaN, aborts.
        module subroutine grid_quantile_r0(self, p, x)
            implicit none
            class(pf_kde_grid), intent(in) :: self !! the grid
            real(real64), intent(in)       :: p    !! the probability
            real(real64), intent(out)      :: x    !! the quantile
        end subroutine grid_quantile_r0

        !> The quantile at each probability of `p`: `call g%quantile(p, x)`.
        !! `size(x) /= size(p)` aborts.
        module subroutine grid_quantile_r1(self, p, x)
            implicit none
            class(pf_kde_grid), intent(in) :: self !! the grid
            real(real64), intent(in)       :: p(:) !! the probabilities
            real(real64), intent(out)      :: x(:) !! the quantile at each
        end subroutine grid_quantile_r1

        !> Draws from the grid's density: `call g%sample(v, seed, [stream], [threads])` fills `v`
        !! with `size(v)` independent draws, `stream` absent (stream 0) or `int32`.
        !!
        !! Each draw inverts the distribution of the piecewise-linear density `%pdf` describes over
        !! `[xmin, xmax]`, a quadratic between two centres, so the draws follow `%pdf` exactly. Weight
        !! the grid counted beyond its range is not located, so no draw lands there: the draws
        !! follow the density inside the range, normalised to the weight it holds. Element `k` of
        !! `v` is a function of `seed`, `stream` and `k` alone, and `threads` changes nothing but the
        !! time. `v` is NaN when nothing has been accumulated inside the range or a kept NaN has
        !! poisoned the grid.
        module subroutine grid_sample_s32(self, v, seed, stream, threads)
            implicit none
            class(pf_kde_grid), intent(in)       :: self    !! the grid
            real(real64), intent(out)            :: v(:)    !! the draws
            integer(int64), intent(in)           :: seed    !! the seed
            integer(int32), intent(in), optional :: stream  !! the stream; 0 when absent
            integer, intent(in), optional        :: threads !! the team for the draws
        end subroutine grid_sample_s32

        !> Draws from the grid's density with an `int64` `stream`: `call g%sample(v, seed, stream,
        !! [threads])`; otherwise as the `int32` form, and the two agree at every stream they share.
        module subroutine grid_sample_s64(self, v, seed, stream, threads)
            implicit none
            class(pf_kde_grid), intent(in) :: self    !! the grid
            real(real64), intent(out)      :: v(:)    !! the draws
            integer(int64), intent(in)     :: seed    !! the seed
            integer(int64), intent(in)     :: stream  !! the stream
            integer, intent(in), optional  :: threads !! the team for the draws
        end subroutine grid_sample_s64

        !> The cell centres: `call g%grid(x)`, `x(i) = xmin + (i - 1/2)*step`. `size(x)` other
        !! than `ncells` aborts.
        module subroutine grid_centres(self, x)
            implicit none
            class(pf_kde_grid), intent(in) :: self !! the grid
            real(real64), intent(out)      :: x(:) !! the centres
        end subroutine grid_centres

        !> The number of cells.
        module function grid_ncells(self) result(n)
            implicit none
            class(pf_kde_grid), intent(in) :: self !! the grid
            integer                        :: n    !! the count
        end function grid_ncells

        !> The cell width, `(xmax - xmin)/ncells`.
        module function grid_step(self) result(s)
            implicit none
            class(pf_kde_grid), intent(in) :: self !! the grid
            real(real64)                   :: s    !! the width
        end function grid_step

        !> The bandwidth, the kernel's standard deviation, as `%init` was given it.
        module function grid_bandwidth(self) result(h)
            implicit none
            class(pf_kde_grid), intent(in) :: self !! the grid
            real(real64)                   :: h    !! the bandwidth
        end function grid_bandwidth

        !> The kernel in use, as its token: `call g%kernel(name)`.
        module subroutine grid_kernel_name(self, name)
            implicit none
            class(pf_kde_grid), intent(in)             :: self !! the grid
            character(len=:), allocatable, intent(out) :: name !! the kernel's token
        end subroutine grid_kernel_name

        !> The support: `call g%bounds(lo, hi)`, with `-Infinity`/`+Infinity` where unbounded.
        module subroutine grid_bounds(self, lo, hi)
            implicit none
            class(pf_kde_grid), intent(in) :: self !! the grid
            real(real64), intent(out)      :: lo   !! the lower end of the support
            real(real64), intent(out)      :: hi   !! the upper end of the support
        end subroutine grid_bounds

        !> `size(x)` summed over every `%add` since `%init` or `%clear`.
        module function grid_n(self) result(n)
            implicit none
            class(pf_kde_grid), intent(in) :: self !! the grid
            integer(int64)                 :: n    !! the count
        end function grid_n

        !> The population's size: every point that survived the exclusions.
        module function grid_n_valid(self) result(n)
            implicit none
            class(pf_kde_grid), intent(in) :: self !! the grid
            integer(int64)                 :: n    !! the count
        end function grid_n_valid

        !> How many points were excluded as null.
        module function grid_n_null(self) result(n)
            implicit none
            class(pf_kde_grid), intent(in) :: self !! the grid
            integer(int64)                 :: n    !! the count
        end function grid_n_null

        !> How many points were excluded as NaN and were not already null.
        module function grid_n_nan(self) result(n)
            implicit none
            class(pf_kde_grid), intent(in) :: self !! the grid
            integer(int64)                 :: n    !! the count
        end function grid_n_nan

        !> How many points were excluded as outside the support, or infinite.
        module function grid_n_outside(self) result(n)
            implicit none
            class(pf_kde_grid), intent(in) :: self !! the grid
            integer(int64)                 :: n    !! the count
        end function grid_n_outside

        !> `sum(w)` over the population; its size when unweighted.
        module function grid_sum_weights(self) result(s)
            implicit none
            class(pf_kde_grid), intent(in) :: self !! the grid
            real(real64)                   :: s    !! the total weight
        end function grid_sum_weights

        !> `.true.` once `%init` has run. Never aborts.
        module function grid_is_initialised(self) result(res)
            implicit none
            class(pf_kde_grid), intent(in) :: self !! the grid
            logical                        :: res  !! it is initialised
        end function grid_is_initialised

        !> `.true.` when each point takes its own bandwidth, from the pilot `%init` was given.
        module function grid_is_adaptive(self) result(res)
            implicit none
            class(pf_kde_grid), intent(in) :: self !! the grid
            logical                        :: res  !! the grid is adaptive
        end function grid_is_adaptive

        !> Writes a one-block summary: `call g%print([unit])`. The geometry, the kernel, the
        !! bandwidth, the counts, the support and the correction, and the adaptive rule's settings
        !! and its pilot's geometry. Silenced by
        !! `verbosity = "silent"`; written to `unit` or, by default, where `message_stream` says.
        !! An uninitialised grid prints one line saying so rather than aborting.
        module subroutine grid_print(self, unit)
            implicit none
            class(pf_kde_grid), intent(in) :: self !! the grid
            integer, intent(in), optional  :: unit !! where to write
        end subroutine grid_print

        !> Zeroes the accumulation and every count, keeping the geometry and the settings: the grid
        !! is initialised and empty. Does nothing to a grid that was never initialised.
        module subroutine grid_clear(self)
            implicit none
            class(pf_kde_grid), intent(inout) :: self !! the grid
        end subroutine grid_clear

    end interface

    ! ---- the adaptive rule's workers, implemented in parquet_kde_grid.f90 --------------------

    interface

        !> Builds the pilot `pf_kde%fit(adaptive=.true.)` reads: a grid of the survivors `x`,
        !! ascending, at the global bandwidth `h`, with the fit's kernel, support and correction.
        !!
        !! Its range reaches `KDE_PILOT_REACH` bandwidths beyond the extreme points, clipped to the
        !! support, and its cells are `1/KDE_PILOT_PER_H` of a bandwidth wide, their number clamped
        !! to `[KDE_PILOT_MIN_CELLS, KDE_PILOT_MAX_CELLS]` unless
        !! `parquet_debug_set_kde_pilot_cells` forces one. The deposit is cut into pieces the sample
        !! and the pilot decide, never the team, so the pilot's bits do not depend on `threads`.
        !! `ok` is `.false.` only when no range can be represented, which takes points beyond half
        !! the largest number.
        module subroutine kde_build_pilot(g, x, w, weighted, h, kernel_code, has_lower, lo, has_upper, &
                hi, boundary_code, w_total, threads, ok)
            implicit none
            type(pf_kde_grid), intent(inout) :: g             !! the pilot; set up afresh
            real(real64), intent(in)         :: x(:)          !! the survivors, ascending, inside the support
            real(real64), intent(in)         :: w(:)          !! their weights, when weighted
            logical, intent(in)              :: weighted      !! `w` holds one weight per survivor
            real(real64), intent(in)         :: h             !! the global bandwidth
            integer, intent(in)              :: kernel_code   !! the kernel
            logical, intent(in)              :: has_lower     !! a lower bound was given
            real(real64), intent(in)         :: lo            !! the lower bound
            logical, intent(in)              :: has_upper     !! an upper bound was given
            real(real64), intent(in)         :: hi            !! the upper bound
            integer, intent(in)              :: boundary_code !! the correction
            real(real64), intent(in)         :: w_total       !! the survivors' total weight
            integer, intent(in), optional    :: threads       !! the team for the deposit
            logical, intent(out)             :: ok            !! a range could be represented
        end subroutine kde_build_pilot

        !> Sets the adaptive rule from a pilot grid: copies its cells into the look-up table and
        !! forms `log g`, the mean of `log p` over the pilot's own density, and the smallest
        !! positive cell density, the stand-in for a point the pilot reads as zero. A pilot with no
        !! density to read -- poisoned, or no cell holding a positive density -- gives a table
        !! marked unreadable, from which every bandwidth is NaN.
        module subroutine kde_adapt_set(a, pilot, alpha, has_bmax, bmax)
            implicit none
            type(kde_adapt), intent(out)  :: a        !! the rule
            type(pf_kde_grid), intent(in) :: pilot    !! the pilot
            real(real64), intent(in)      :: alpha    !! the sensitivity, in `[0, 1]`
            logical, intent(in)           :: has_bmax !! a cap was given
            real(real64), intent(in)      :: bmax     !! the cap
        end subroutine kde_adapt_set

        !> The bandwidth the adaptive rule `a` gives each point of `x` under the global bandwidth
        !! `h`: `h * (p(x)/g)**(-alpha)`, with `p` the pilot interpolated between its centres, and
        !! capped at `bandwidth_max`. Exactly `h` when `alpha = 0`. Where the pilot reads zero the
        !! answer is the cap, or without one the pilot's smallest positive cell density stands in.
        !! NaN at a NaN point and everywhere on an unreadable table; `+Infinity` where the bandwidth
        !! is too large to represent.
        module subroutine kde_adapt_bandwidths(a, h, x, hb)
            implicit none
            type(kde_adapt), intent(in) :: a     !! the rule
            real(real64), intent(in)    :: h     !! the global bandwidth
            real(real64), intent(in)    :: x(:)  !! the points
            real(real64), intent(out)   :: hb(:) !! the bandwidth at each; `size(x)` elements
        end subroutine kde_adapt_bandwidths

    end interface

    ! ---- kernels, tokens, rules and the abort, implemented in parquet_kde_core.f90 ----------

    interface

        !> Aborts with `<entry>: <text>`, under the module's one named `critical`, so that one
        !! thread aborts when a caller fits objects inside a parallel region. Impure deliberately:
        !! a `pure` guard-only procedure's call is deleted by ifx at `-O0`.
        module subroutine kde_abort(entry, text)
            implicit none
            character(len=*), intent(in) :: entry !! the binding, e.g. `pf_kde%fit`
            character(len=*), intent(in) :: text  !! what went wrong
        end subroutine kde_abort

        !> The team a bulk query of `n` elements opens, into `team`: `resolve_thread_count`'s
        !! answer -- the caller's `threads` or the automatic count, never a nested team where one
        !! would deadlock, clamped to the processors available -- cut so that each thread gets at
        !! least `KDE_QUERY_MIN_WORK` of work, `work` being one element's cost in kernel
        !! evaluations. 1 means serial, and is the only answer without OpenMP. Aborts with the
        !! caller's `entry` when `threads` is below one, whatever `n` is.
        module subroutine kde_query_team(entry, threads, n, work, team)
            implicit none
            character(len=*), intent(in)  :: entry   !! the binding, for the message
            integer, intent(in), optional :: threads !! the caller's request; absent means automatic
            integer(int64), intent(in)    :: n       !! the elements
            real(real64), intent(in)      :: work    !! one element's cost, in kernel evaluations
            integer, intent(out)          :: team    !! threads to open
        end subroutine kde_query_team

        !> Records, from inside a parallel region, the team it runs on in the test observable
        !! `parquet_debug_kde_threads_used` reads. Every thread of the team must call it, once.
        module subroutine kde_record_team()
            implicit none
        end subroutine kde_record_team

        !> A caller's token, trimmed and lower-cased into a fixed buffer; blank when it is longer
        !! than the buffer, which no valid token is.
        pure module function kde_fold(token) result(folded)
            implicit none
            character(len=*), intent(in) :: token  !! the caller's token
            character(len=16)            :: folded !! trimmed and lower-cased
        end function kde_fold

        !> Resolves `kernel=` to its code, aborting on an unknown token.
        module subroutine kde_resolve_kernel(entry, token, code)
            implicit none
            character(len=*), intent(in) :: entry !! the binding, for the message
            character(len=*), intent(in) :: token !! the caller's token
            integer, intent(out)         :: code  !! the kernel's code
        end subroutine kde_resolve_kernel

        !> Resolves `rule=` to its code, aborting on an unknown token.
        module subroutine kde_resolve_rule(entry, token, code)
            implicit none
            character(len=*), intent(in) :: entry !! the binding, for the message
            character(len=*), intent(in) :: token !! the caller's token
            integer, intent(out)         :: code  !! the rule's code
        end subroutine kde_resolve_rule

        !> Resolves `boundary=` to its code, aborting on an unknown token.
        module subroutine kde_resolve_boundary(entry, token, code)
            implicit none
            character(len=*), intent(in) :: entry !! the binding, for the message
            character(len=*), intent(in) :: token !! the caller's token
            integer, intent(out)         :: code  !! the correction's code
        end subroutine kde_resolve_boundary

        !> Validates and resolves the settings `pf_kde%fit` and `pf_kde_grid%init` share -- the
        !! kernel, the two bounds and the boundary correction -- in that order, aborting with the
        !! caller's `entry` on the first mistake. Absent bounds leave the support unbounded on that
        !! side; any bound without `boundary` selects `"renormalise"`.
        module subroutine kde_resolve_setup(entry, kernel, lower, upper, boundary, kernel_code, &
                has_lower, lo, has_upper, hi, boundary_code)
            implicit none
            character(len=*), intent(in)           :: entry         !! the binding, for the message
            character(len=*), intent(in), optional :: kernel        !! the caller's kernel token
            real(real64), intent(in), optional     :: lower         !! the caller's lower bound
            real(real64), intent(in), optional     :: upper         !! the caller's upper bound
            character(len=*), intent(in), optional :: boundary      !! the caller's correction token
            integer, intent(out)                   :: kernel_code   !! the kernel
            logical, intent(out)                   :: has_lower     !! a lower bound was given
            real(real64), intent(out)              :: lo            !! the lower bound, or 0
            logical, intent(out)                   :: has_upper     !! an upper bound was given
            real(real64), intent(out)              :: hi            !! the upper bound, or 0
            integer, intent(out)                   :: boundary_code !! the correction
        end subroutine kde_resolve_setup

        !> `.true.` for a finite number above zero. The NaN is screened first, as its own test: an
        !! ordered comparison raises `IEEE_INVALID` on one.
        pure module function kde_positive_finite(v) result(res)
            implicit none
            real(real64), intent(in) :: v   !! the value
            logical                  :: res !! it is finite and positive
        end function kde_positive_finite

        !> `.true.` for a bandwidth the estimate can use: above zero, and finite once multiplied by
        !! the kernel's support radius, which is how far a kernel of it reaches. An adaptive rule
        !! can produce one that is neither, by overflow or underflow.
        pure module function kde_bandwidth_usable(code, h) result(res)
            implicit none
            integer, intent(in)      :: code !! the kernel
            real(real64), intent(in) :: h    !! the bandwidth
            logical                  :: res  !! it can be used
        end function kde_bandwidth_usable

        !> `.true.` when a value that is not NaN lies outside a support: beyond a bound that was
        !! given, or infinite, which is outside every support.
        pure module function kde_outside(has_lower, lo, has_upper, hi, v) result(res)
            implicit none
            logical, intent(in)      :: has_lower !! a lower bound was given
            real(real64), intent(in) :: lo        !! the lower bound
            logical, intent(in)      :: has_upper !! an upper bound was given
            real(real64), intent(in) :: hi        !! the upper bound
            real(real64), intent(in) :: v         !! the value, not NaN
            logical                  :: res       !! it is outside
        end function kde_outside

        !> The kernel's density at `z` standard deviations from its centre: `K(z/C)/C` for the
        !! unit-scale kernel `K` and its scale `C`, so that it integrates to one over `z`. Zero
        !! beyond the support radius.
        pure module function kde_kernel_pdf(code, z) result(k)
            implicit none
            integer, intent(in)      :: code !! the kernel
            real(real64), intent(in) :: z    !! the offset, in standard deviations
            real(real64)             :: k    !! the density there
        end function kde_kernel_pdf

        !> The kernel's density at every offset of `z`, into `k`: `kde_kernel_pdf` for a whole row
        !! of cells in one call, which is what the grid deposit evaluates per point. `k` has at
        !! least `size(z)` elements.
        pure module subroutine kde_kernel_pdf_many(code, z, k)
            implicit none
            integer, intent(in)       :: code !! the kernel
            real(real64), intent(in)  :: z(:) !! the offsets, in standard deviations
            real(real64), intent(out) :: k(:) !! the density at each
        end subroutine kde_kernel_pdf_many

        !> The kernel's cumulative distribution at `z` standard deviations: exactly 0 at and below
        !! minus the support radius and exactly 1 at and above it.
        pure module function kde_kernel_cdf(code, z) result(c)
            implicit none
            integer, intent(in)      :: code !! the kernel
            real(real64), intent(in) :: z    !! the offset, in standard deviations
            real(real64)             :: c    !! the kernel's mass at or below `z`
        end function kde_kernel_cdf

        !> The bandwidth a rule gives the population `x` with its `weights`: `C * A * n_eff**(-1/5)`
        !! with `A = min(s, IQR/1.349)`, or `s` alone where the interquartile range is zero, and
        !! `n_eff` as `weight_type` defines it. Not multiplied by `adjust`. NaN when the
        !! population is too small or too constant to have a scale.
        module subroutine kde_rule_bandwidth(rule_code, x, freq, weights, weight_type, threads, h)
            implicit none
            integer, intent(in)                    :: rule_code   !! the rule
            real(real64), intent(in)               :: x(:)        !! the population
            logical, intent(in)                    :: freq        !! frequency weights
            real(real64), intent(in), optional     :: weights(:)  !! its weights, when weighted
            character(len=*), intent(in), optional :: weight_type !! the caller's token, forwarded
            integer, intent(in), optional          :: threads     !! the statistics' team
            real(real64), intent(out)              :: h           !! the bandwidth
        end subroutine kde_rule_bandwidth

    end interface

    ! ---- the test hooks, implemented in parquet_kde_core.f90 -------------------------------

    interface

        !> The team the most recent threaded pass of this module actually ran on; 1 when it ran
        !! serially.
        !!
        !! Test-only, and public for that reason alone: an answer comparison between thread counts
        !! cannot tell a team that ran from one that never opened, so every threading assertion
        !! pairs its answer with this. Process-global and unsynchronised; read it from a serial
        !! context straight after the call it describes.
        module function parquet_debug_kde_threads_used() result(n)
            implicit none
            integer :: n !! the team size
        end function parquet_debug_kde_threads_used

        !> Forces the number of cells in the pilot `pf_kde%fit(adaptive=.true.)` builds; `n <= 0`
        !! restores the rule (a quarter of a bandwidth per cell, clamped to `[64, 65536]`).
        !!
        !! Test-only, and public for that reason alone: the rule gives a small sample a pilot too
        !! coarse to reproduce an exact pilot to more than three digits, and a finer one is how a
        !! test reaches the rule's own arithmetic. Process-global and unsynchronised; the suite that
        !! calls it runs serially.
        module subroutine parquet_debug_set_kde_pilot_cells(n)
            implicit none
            integer, intent(in) :: n !! the cell count; `<= 0` for the rule
        end subroutine parquet_debug_set_kde_pilot_cells

        !> Nanoseconds the most recent `pf_kde%fit` spent in each of its three costly phases: the
        !! sort, the pilot (built and summarised) and the bandwidths (each point's, and its mass
        !! inside the support). A fixed fit reports zero for the pilot.
        !!
        !! Test-and-bench only, and public for that reason: `bench/benchmark_kde.sh`'s `adaptive`
        !! mode reads it, so that its phase table is one run rather than a subtraction between
        !! runs. The clock is `system_clock` at `int64` kinds, read at the phase boundaries, so a
        !! phase shorter than one of its counts reads 0. Process-global and unsynchronised: read it
        !! from a serial context straight after the fit it describes.
        module subroutine parquet_debug_kde_fit_nanos(sort, pilot, lookup)
            implicit none
            integer(int64), intent(out) :: sort   !! ordering the survivors and their running weight
            integer(int64), intent(out) :: pilot  !! building the pilot and its look-up table
            integer(int64), intent(out) :: lookup !! each point's bandwidth and mass inside the support
        end subroutine parquet_debug_kde_fit_nanos

    end interface

end module parquet_kde ! GCOVR_EXCL_LINE
