!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> Kernel density estimation in one dimension: `pf_kde`, which fits a density to an array it
!> retains and answers it exactly anywhere.
!!
!! **This module is Arrow-free and must stay that way.** Nothing in its closure reaches
!! `parquet_bindings`; `check_parquet_kde_stays_arrow_free` (tools/check_source_conventions.py)
!! walks the closure, submodules included. Its one library tier edge is `parquet_stats`, whose
!! `pf_stddev` and `pf_iqr` are the bandwidth rules' scale and whose exclusion pass and argument
!! checkers are this module's population rules, so that "in the population" means here exactly
!! what it means for every `pf_*` statistic. `tools/module_footprints.txt` records the cost.
!!
!! **`bandwidth` is the standard deviation of the kernel, whichever kernel is chosen.** Each
!! kernel is stored with the scale factor that gives it unit variance, so one bandwidth rule
!! serves every kernel and a number means the same smoothing under all four. The Gaussian is cut
!! at five standard deviations and renormalised, which gives every kernel compact support: the
!! sorted sample is searched for the points within reach of a query, and nothing else is summed.
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
!! **A fitted object is read-only under every query**, so any number of threads may share one.
!! `%fit` and `%clear` are the only writes.
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
    ! The sort of the retained sample; `parquet_argsort` is already beneath `parquet_stats`.
    use parquet_argsort, only : pf_argsort
    ! The Gaussian kernel is the library's `phi` and `Phi`, never a second spelling of them.
    use parquet_utils, only : pf_norm_pdf, pf_norm_cdf
    ! `%print` is solicited output; the verbosity and message-stream pair is re-exported because
    ! this module reads it.
    use parquet_settings_base, only : parquet_output_is_suppressed, parquet_message_unit, &
        parquet_set_verbosity, parquet_get_verbosity, parquet_set_message_stream, &
        parquet_get_message_stream
    use iso_fortran_env, only : int64, real32, real64
    use, intrinsic :: ieee_arithmetic, only : ieee_value, ieee_quiet_nan, ieee_positive_inf, &
        ieee_negative_inf, ieee_is_nan, ieee_is_finite

    implicit none
    private

    public :: pf_kde
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

    ! ---- the type ---------------------------------------------------------------------------

    !> A kernel density estimate over a retained, sorted sample.
    !!
    !! `%fit` chooses the bandwidth, applies the population rules and keeps the survivors in
    !! ascending order with their weights. Every query afterwards is exact: `%pdf` and `%cdf` sum
    !! the kernels of the points within reach of each query point, and `%quantile` inverts `%cdf`.
    !! A fitted object is read-only under every query and may be shared by any number of threads.
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
        real(real64) :: w_total = 0.0_real64         !! `sum(w)` over the population
        logical :: weighted = .false.                !! `weights=` was given
        integer(int64) :: cnt_all = 0_int64          !! `size(x)` at `%fit`
        integer(int64) :: cnt_valid = 0_int64        !! the population's size
        integer(int64) :: cnt_null = 0_int64         !! excluded as null
        integer(int64) :: cnt_nan = 0_int64          !! excluded as NaN
        integer(int64) :: cnt_out = 0_int64          !! excluded as outside the support
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
        !! [kernel], [lower], [upper], [boundary], [is_valid], [weights], [weight_type], [skipnan],
        !! [n_null], [n_nan], [n_outside], [ok], [threads])`.
        !!
        !! `x` is the sample, retained as a sorted copy of its population. `bandwidth` is the
        !! kernel's standard deviation, a finite positive number; without it the bandwidth comes
        !! from `rule`, `"silverman"` (the default) or `"scott"`, and the two cannot both be given.
        !! `adjust` multiplies the bandwidth however it was chosen (default 1). `kernel` is
        !! `"gaussian"` (the default), `"epanechnikov"`, `"bspline"` or `"box"`. `lower` and
        !! `upper` bound the support: a point outside is excluded and counted in `n_outside`, and
        !! a kernel crossing a bound is corrected by `boundary`, `"renormalise"` (the default) or
        !! `"reflect"`. `is_valid`, `weights`, `weight_type` and `skipnan` are the `pf_*`
        !! family's population arguments; `weight_type` decides the effective sample size the
        !! rules use. `n_null`, `n_nan` and `n_outside` report what each exclusion removed, and
        !! `ok` is `.false.` when the estimate is undefined. `threads` is the team for the sort
        !! and the rules' statistics; the answer does not depend on it. Tokens are matched
        !! without regard to case.
        module subroutine kde_fit_f64(self, x, bandwidth, rule, adjust, kernel, lower, upper, boundary, &
                is_valid, weights, weight_type, skipnan, n_null, n_nan, n_outside, ok, threads)
            implicit none
            class(pf_kde), intent(inout)           :: self        !! the estimate; refitted
            real(real64), intent(in)               :: x(:)        !! the sample
            real(real64), intent(in), optional     :: bandwidth   !! the kernel's standard deviation
            character(len=*), intent(in), optional :: rule        !! `"silverman"` or `"scott"`
            real(real64), intent(in), optional     :: adjust      !! a factor on the bandwidth
            character(len=*), intent(in), optional :: kernel      !! the kernel's token
            real(real64), intent(in), optional     :: lower       !! the support's lower bound
            real(real64), intent(in), optional     :: upper       !! the support's upper bound
            character(len=*), intent(in), optional :: boundary    !! the boundary correction
            logical, intent(in), optional          :: is_valid(:) !! `.false.` marks a null
            real(real64), intent(in), optional     :: weights(:)  !! per-element weights
            character(len=*), intent(in), optional :: weight_type !! `"reliability"` or `"frequency"`
            logical, intent(in), optional          :: skipnan     !! `.false.` lets a NaN poison
            integer(int64), intent(out), optional  :: n_null      !! excluded as null
            integer(int64), intent(out), optional  :: n_nan       !! excluded as NaN
            integer(int64), intent(out), optional  :: n_outside   !! excluded as outside the support
            logical, intent(out), optional         :: ok          !! the estimate is defined
            integer, intent(in), optional          :: threads     !! the team for the sort
        end subroutine kde_fit_f64

        !> The `real32` sample, widened to `real64` first; every other argument as the `real64`
        !! form.
        module subroutine kde_fit_f32(self, x, bandwidth, rule, adjust, kernel, lower, upper, boundary, &
                is_valid, weights, weight_type, skipnan, n_null, n_nan, n_outside, ok, threads)
            implicit none
            class(pf_kde), intent(inout)           :: self        !! the estimate; refitted
            real(real32), intent(in)               :: x(:)        !! the sample
            real(real64), intent(in), optional     :: bandwidth   !! the kernel's standard deviation
            character(len=*), intent(in), optional :: rule        !! `"silverman"` or `"scott"`
            real(real64), intent(in), optional     :: adjust      !! a factor on the bandwidth
            character(len=*), intent(in), optional :: kernel      !! the kernel's token
            real(real64), intent(in), optional     :: lower       !! the support's lower bound
            real(real64), intent(in), optional     :: upper       !! the support's upper bound
            character(len=*), intent(in), optional :: boundary    !! the boundary correction
            logical, intent(in), optional          :: is_valid(:) !! `.false.` marks a null
            real(real64), intent(in), optional     :: weights(:)  !! per-element weights
            character(len=*), intent(in), optional :: weight_type !! `"reliability"` or `"frequency"`
            logical, intent(in), optional          :: skipnan     !! `.false.` lets a NaN poison
            integer(int64), intent(out), optional  :: n_null      !! excluded as null
            integer(int64), intent(out), optional  :: n_nan       !! excluded as NaN
            integer(int64), intent(out), optional  :: n_outside   !! excluded as outside the support
            logical, intent(out), optional         :: ok          !! the estimate is defined
            integer, intent(in), optional          :: threads     !! the team for the sort
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

        !> The density at each point of `x`: `call k%pdf(x, f)`, `f(i)` at `x(i)`.
        !! `size(f) /= size(x)` aborts.
        module subroutine kde_pdf_r1(self, x, f)
            implicit none
            class(pf_kde), intent(in) :: self !! the fitted estimate
            real(real64), intent(in)  :: x(:) !! where to evaluate
            real(real64), intent(out) :: f(:) !! the density at each point
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

        !> `P(X <= x)` at each point of `x`: `call k%cdf(x, p)`. `size(p) /= size(x)` aborts.
        module subroutine kde_cdf_r1(self, x, p)
            implicit none
            class(pf_kde), intent(in) :: self !! the fitted estimate
            real(real64), intent(in)  :: x(:) !! where to evaluate
            real(real64), intent(out) :: p(:) !! the probability at or below each point
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

        !> The quantile at each probability of `p`: `call k%quantile(p, x)`.
        !! `size(x) /= size(p)` aborts.
        module subroutine kde_quantile_r1(self, p, x)
            implicit none
            class(pf_kde), intent(in) :: self !! the fitted estimate
            real(real64), intent(in)  :: p(:) !! the probabilities
            real(real64), intent(out) :: x(:) !! the quantile at each
        end subroutine kde_quantile_r1

        !> The density on `size(x)` equally spaced points: `call k%curve(x, f, [xmin], [xmax],
        !! [cut])`.
        !!
        !! `x` receives the points, from `xmin` to `xmax` inclusive, and `f` the density at each.
        !! The default range is the sample's minimum minus `cut` bandwidths to its maximum plus
        !! `cut` bandwidths (`cut` defaults to 3), clipped to the support. `size(f) /= size(x)`,
        !! `xmin >= xmax` and a negative `cut` abort. On an undefined estimate `f` is NaN, and so
        !! is `x` unless both ends were given.
        module subroutine kde_curve(self, x, f, xmin, xmax, cut)
            implicit none
            class(pf_kde), intent(in)          :: self !! the fitted estimate
            real(real64), intent(out)          :: x(:) !! the points
            real(real64), intent(out)          :: f(:) !! the density at each point
            real(real64), intent(in), optional :: xmin !! the first point
            real(real64), intent(in), optional :: xmax !! the last point
            real(real64), intent(in), optional :: cut  !! bandwidths beyond the data, by default
        end subroutine kde_curve

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

        !> `.true.` when the fit gave each point its own bandwidth.
        module function kde_is_adaptive(self) result(res)
            implicit none
            class(pf_kde), intent(in) :: self !! the fitted estimate
            logical                   :: res  !! the fit is adaptive
        end function kde_is_adaptive

        !> Writes a one-block summary: `call k%print([unit])`. The kernel, the rule, the
        !! bandwidth, the counts, the support and the correction. Silenced by
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

        !> The kernel's density at `z` standard deviations from its centre: `K(z/C)/C` for the
        !! unit-scale kernel `K` and its scale `C`, so that it integrates to one over `z`. Zero
        !! beyond the support radius.
        pure module function kde_kernel_pdf(code, z) result(k)
            implicit none
            integer, intent(in)      :: code !! the kernel
            real(real64), intent(in) :: z    !! the offset, in standard deviations
            real(real64)             :: k    !! the density there
        end function kde_kernel_pdf

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

end module parquet_kde ! GCOVR_EXCL_LINE
