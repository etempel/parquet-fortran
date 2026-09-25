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
!! `pf_stddev` and `pf_iqr` are the bandwidth rules' scale, whose `pf_bin_linear` bins the sample
!! for the ISJ rule and whose exclusion pass and argument checkers are this module's population
!! rules, so that "in the population" means here exactly what it means for every `pf_*` statistic;
!! `parquet_root` and `parquet_transform`, whose root finder and discrete cosine transform the ISJ
!! rule is built on; and `parquet_random`, the generator
!! `%sample` draws from. `tools/module_footprints.txt` records the cost.
!!
!! **`bandwidth` is the standard deviation of the kernel, whichever kernel is chosen.** Each
!! kernel is stored with the scale factor that gives it unit variance, so one bandwidth rule
!! serves every kernel and a number means the same smoothing under all four. The Gaussian is cut
!! at five standard deviations and renormalised, which gives every kernel compact support: the
!! sorted sample is searched for the points within reach of a query, and nothing else is summed.
!!
!! **Without a number, the bandwidth comes from the Improved Sheather-Jones rule.** It solves a
!! fixed-point equation for the bandwidth that minimises the estimate's asymptotic mean integrated
!! squared error, reading the density's derivatives from the discrete cosine transform of the
!! sample binned onto a grid of `2**14` cells, so it follows a multimodal sample that Silverman's
!! and Scott's rules of thumb oversmooth. A sample the rule finds no bandwidth for at the grid's
!! resolution -- a few points, or values rounded to a step the grid resolves -- takes Silverman's
!! rule instead when the rule was not named, and is undefined when `rule = "isj"` asked for it.
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

    ! The tier edge. `pf_stddev` and `pf_iqr` are the rules' scale; `pf_bin_linear` bins the sample
    ! for the ISJ rule; `stats_compact` applies the family's exclusion order and hands back the
    ! survivors; the three checkers compose the family's own abort texts from the `what` passed to
    ! them; `col_to_real64` widens a column as the family does. The last five are plumbing that
    ! `parquet_stats` publishes for this module and the `parquet` facade hides.
    use parquet_stats, only : pf_stddev, pf_iqr, pf_bin_linear, stats_compact, stats_check_sizes, &
        stats_check_weight, stats_weight_kind, col_to_real64
    ! The column forms of `%fit` and `%add`; `parquet_columns` is already beneath `parquet_stats`.
    use parquet_columns, only : parquet_column, parquet_kind_name, PK_INT32, PK_INT64, PK_FLOAT32, &
        PK_FLOAT64
    ! The ISJ rule: its fixed point is solved by the library's root finder, over the discrete cosine
    ! transform of the binned sample. Two leaf modules, two files each.
    use parquet_root, only : pf_rootfun, pf_find_root, pf_bracket_expansion, pf_root_info, &
        PF_EXPAND_UP, PF_ROOT_OK
    use parquet_transform, only : pf_dct, pf_idct, pf_dst, pf_idst, pf_is_pow2, pf_next_pow2
    ! The sort of the retained sample, and the thread-count resolver `%add` shares with every
    ! threaded pass of the library; `parquet_argsort` is already beneath `parquet_stats`.
    use parquet_argsort, only : pf_argsort, resolve_thread_count
    ! The Gaussian kernel is the library's `phi` and `Phi`, never a second spelling of them.
    use parquet_utils, only : pf_norm_pdf, pf_norm_cdf
    ! `%sample`'s draws: the coordinate-addressed generator, so that a draw is a pure function of
    ! its coordinates and a sample splits among threads anywhere.
    use parquet_random, only : pf_random_at, pf_random_int_at, pf_random_normal_at, pf_random_key
    ! `%print` is solicited output; the verbosity and message-stream pair is re-exported because
    ! this module reads it. `parquet_emit_warning` carries the one finding this module makes about
    ! a caller's DATA that leaves nothing to read: R3's over-reaching adaptive bandwidths.
    use parquet_settings_base, only : parquet_output_is_suppressed, parquet_message_unit, &
        parquet_set_verbosity, parquet_get_verbosity, parquet_set_message_stream, &
        parquet_get_message_stream, parquet_emit_warning, parquet_emit_advice
    use iso_fortran_env, only : int32, int64, real32, real64
    use, intrinsic :: ieee_arithmetic, only : ieee_value, ieee_quiet_nan, ieee_positive_inf, &
        ieee_negative_inf, ieee_is_nan, ieee_is_finite

    implicit none
    private

    public :: pf_kde, pf_kde_grid
    public :: pf_kde_bandwidth
    public :: parquet_debug_kde_threads_used, parquet_debug_set_kde_pilot_cells, parquet_debug_kde_fit_nanos
    public :: parquet_debug_kde_scan_counts, parquet_debug_set_kde_scan_grid
    public :: parquet_debug_set_kde_lscv_grid, parquet_debug_kde_lscv_at
    public :: parquet_debug_set_kde_isj_cells, parquet_debug_set_kde_sample_tries
    public :: parquet_debug_set_kde_binned_classes
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
    integer, parameter :: KDE_RULE_EXPLICIT = 0, KDE_RULE_SILVERMAN = 1, KDE_RULE_SCOTT = 2, &
        KDE_RULE_ISJ = 3, KDE_RULE_LSCV = 4

    !> Boundary correction codes. `KDE_BOUNDARY_NONE` is the unbounded estimate.
    integer, parameter :: KDE_BOUNDARY_NONE = 0, KDE_BOUNDARY_RENORMALISE = 1, &
        KDE_BOUNDARY_REFLECT = 2, KDE_BOUNDARY_LINEAR = 3

    ! ---- the binned method --------------------------------------------------------------------

    !> Method codes, resolved once from the `method=` token of `pf_kde_grid%init` and
    !! `pf_kde%curve`. `KDE_METHOD_EXACT` is the deposit every grid makes today and the default.
    integer, parameter :: KDE_METHOD_EXACT = 0, KDE_METHOD_BINNED = 1

    !> `pi`, to 50 digits: the binned method's coefficient `k` carries the frequency
    !! `pi k/(L step)`, and nothing else in the module needs it.
    real(real64), parameter :: KDE_PI = 3.1415926535897932384626433832795028841971693993751_real64

    !> Below this argument a kernel transform is summed as its series rather than evaluated in
    !! closed form, because the closed form's terms cancel there. One standard deviation of
    !! frequency: the series needs about ten terms at it and the closed form has lost about a digit.
    real(real64), parameter :: KDE_FT_SERIES = 1.0_real64

    !> The most terms a kernel transform's series sums before it gives up on converging. It exits on
    !! the term falling below rounding, which happens by the tenth at `KDE_FT_SERIES`; the count is
    !! a bound on the loop, not a working value.
    integer, parameter :: KDE_FT_TERMS = 40

    !> The transform length the binned method refuses above, as a multiple of `ncells`: padding
    !! grows the length with the kernel's reach over the range, so a bandwidth about twice the whole
    !! range reaches this. A grid whose bandwidth is twice its own range is telling the user
    !! something, and an `%init` refusal says it better than a transform of a million cells does.
    integer, parameter :: KDE_BINNED_L_MAX = 32

    !> The ratio between neighbouring bandwidth classes under the binned adaptive kernel, `2**(1/5)`.
    !! What sets the bucketing error is that ratio, so fixing it makes the accuracy the same whatever
    !! spread the data produces; the class COUNT then follows the spread.
    real(real64), parameter :: KDE_BINNED_H_STEP = 2.0_real64**0.2_real64

    !> The most bandwidth classes the binned adaptive kernel resolves. A spread of 9 needs 16 and a
    !! spread of 99 needs 34, so the ceiling is reached only by a rule whose widest bandwidth is
    !! thousands of times its narrowest; `bandwidth_max=` is what bounds the spread.
    integer, parameter :: KDE_BINNED_CLASSES_MAX = 64

    ! ---- the linear boundary kernel -----------------------------------------------------------

    !> `phi(5)`, the Gaussian's density at its cut: `exp(-12.5)/sqrt(2*pi)`, computed at 50 digits.
    real(real64), parameter :: KDE_GAUSS_PHI_CUT = 1.4867195147342977079e-6_real64

    !> The cut Gaussian's variance, `1 - 10 phi(5)/erf(5/sqrt(2))`, computed at 50 digits: the cut
    !! removes tail mass, and the renormalisation cannot put that variance back.
    real(real64), parameter :: KDE_GAUSS_VAR = 0.99998513279632924242_real64

    !> Each kernel's variance in standard-deviation units, the second moment of its whole support:
    !! one, but for the cut Gaussian.
    real(real64), parameter :: KDE_VARIANCE(4) = [KDE_GAUSS_VAR, 1.0_real64, 1.0_real64, 1.0_real64]

    !> Below this width, in standard deviations, the two-sided moments of the linear kernel are
    !! formed centred on the interval's midpoint by quadrature rather than as differences of the
    !! closed forms, which cancel there: `D` falls as the fourth power of the width while each
    !! closed form stays of order one.
    real(real64), parameter :: KDE_CORR_NARROW = 1.0_real64

    !> The eight-point Gauss-Legendre rule on `[-1, 1]`: its nodes and weights, computed at 50
    !! digits. It forms the centred moments of a narrow interval, where it is exact for the three
    !! polynomial kernels on each piece between their knots and at rounding for the Gaussian.
    real(real64), parameter :: KDE_GL8_X(8) = [-0.96028985649753623168_real64, -0.79666647741362673959_real64, &
        -0.52553240991632898582_real64, -0.18343464249564980494_real64, 0.18343464249564980494_real64, &
        0.52553240991632898582_real64, 0.79666647741362673959_real64, 0.96028985649753623168_real64]
    real(real64), parameter :: KDE_GL8_W(8) = [0.10122853629037625915_real64, 0.22238103445337447054_real64, &
        0.31370664587788728734_real64, 0.36268378337836198297_real64, 0.36268378337836198297_real64, &
        0.31370664587788728734_real64, 0.22238103445337447054_real64, 0.10122853629037625915_real64]

    !> The sixteen-point Gauss-Legendre rule on `[-1, 1]`: its nodes and weights, computed at 50
    !! digits and verified exact through degree 31 and no further, which is what a sixteen-point
    !! Gauss rule is. Every per-point integral of a boundary correction is taken with it.
    real(real64), parameter :: KDE_GL16_X(16) = [-0.98940093499164993260_real64, -0.94457502307323257608_real64, &
        -0.86563120238783174388_real64, -0.75540440835500303390_real64, -0.61787624440264374845_real64, &
        -0.45801677765722738634_real64, -0.28160355077925891323_real64, -0.095012509837637440185_real64, &
        0.095012509837637440185_real64, 0.28160355077925891323_real64, 0.45801677765722738634_real64, &
        0.61787624440264374845_real64, 0.75540440835500303390_real64, 0.86563120238783174388_real64, &
        0.94457502307323257608_real64, 0.98940093499164993260_real64]
    real(real64), parameter :: KDE_GL16_W(16) = [0.027152459411754094852_real64, 0.062253523938647892863_real64, &
        0.095158511682492784810_real64, 0.12462897125553387205_real64, 0.14959598881657673208_real64, &
        0.16915651939500253819_real64, 0.18260341504492358887_real64, 0.18945061045506849629_real64, &
        0.18945061045506849629_real64, 0.18260341504492358887_real64, 0.16915651939500253819_real64, &
        0.14959598881657673208_real64, 0.12462897125553387205_real64, 0.095158511682492784810_real64, &
        0.062253523938647892863_real64, 0.027152459411754094852_real64]

    !> The widest sub-interval, in bandwidths, that one application of `KDE_GL16_X`'s rule is given.
    !!
    !! Each per-point integral is already cut at the term's own breakpoints -- the kernel's knots,
    !! its moments' knots and the two correction edges -- so every piece is analytic; a Gauss rule
    !! converges geometrically on such a piece, but the rate falls with its width in bandwidths, so
    !! a piece wider than this is split into equal parts no wider than it. The cost is then fixed
    !! and predictable, and no convergence test is made or needed. Not a setting: it changes what
    !! the library answers at the last digits, which an argument never does.
    real(real64), parameter :: KDE_GL_SPAN = 2.0_real64

    !> How many points to a bandwidth the fit's scan for the stretches the clip removes evaluates
    !! the raw estimate at, the bandwidth being the narrowest among the kernels that reach the point.
    !! Between two of them the raw estimate is smooth but for the kernel and correction edges, which
    !! the scan refines at where it comes near zero; what it can still miss is a dip of a smooth
    !! piece narrower than the spacing.
    real(real64), parameter :: KDE_LINEAR_SCAN_PER_H = 64.0_real64

    !> The most samples the corrected boundary scan's grid may carry. A zone is at most one reach of
    !! the widest kernel wide and the spacing is a fixed fraction of the narrowest, so the count is
    !! bounded by `KDE_LINEAR_SCAN_PER_H*KDE_RADIUS*spread_max` -- about 22 000 at the default cap
    !! and at the widest kernel's reach. This is the guard for a caller who raised `spread_max`
    !! themselves: past it the scan falls back to summing every point at every step, which is slow
    !! but is what the library did before the grid existed.
    integer, parameter :: KDE_SCAN_GRID_MAX = 1000000

    !> How far from zero, as a fraction of the zone's own largest value, the boundary scan's grid
    !! has to be before its sign is taken without checking. Nearer zero than this the exact
    !! estimator is asked instead.
    !!
    !! Generous on purpose, and the asymmetry is the reason. The binned estimate converges as the
    !! SQUARE of the cell width and the cells here are a fixed fraction of the narrowest bandwidth,
    !! so its error is orders below this band; but the estimate is near zero exactly where a
    !! negative stretch can be, so the cost of refusing to trust the grid there is one exact
    !! evaluation, while the cost of trusting it wrongly is a stretch the clip never removes.
    !! `bench/benchmark_kde.sh`'s `scan` mode reports how often the fallback fires.
    real(real64), parameter :: KDE_SCAN_GRID_BAND = 1.0e-2_real64

    !> Breakpoints one per-point integral can carry: its kernel's knots, its moments' knots, its two
    !! correction edges and, for the sampler, its sign change.
    integer, parameter :: KDE_CORR_MAX_BREAKS = 8

    !> The two zones' places in `pf_kde%zones`: the one at the lower bound and the one at the upper.
    integer, parameter :: KDE_ZONE_LO = 1, KDE_ZONE_HI = 2

    !> How much dearer a zone query is than a plain one, in kernel evaluations, so that a bulk
    !! `%cdf`, `%quantile` or `%sample` under `"linear"` opens a team where the plain estimate's
    !! work would not pay for one.
    real(real64), parameter :: KDE_CORR_CDF_WORK = 64.0_real64

    !> The robust scale's divisor, `IQR/1.349`: the interquartile range of a unit normal.
    real(real64), parameter :: KDE_IQR_NORMAL = 1.349_real64

    !> Silverman's rule of thumb: the constant he recommends for the robust scale, `0.9`.
    real(real64), parameter :: KDE_SILVERMAN_C = 0.9_real64

    !> Scott's normal-reference rule: `(4/3)**(1/5)`, the factor that minimises the asymptotic mean
    !! integrated squared error of a Gaussian kernel on a normal sample. Written as the fifth root
    !! rather than as a rounded decimal, so that the rule is the rule and not a two-digit
    !! approximation to it; a constant expression, folded by the front end.
    real(real64), parameter :: KDE_SCOTT_C = (4.0_real64/3.0_real64)**0.2_real64

    ! ---- the Improved Sheather-Jones rule -------------------------------------------------------

    !> The cells the ISJ rule bins the sample into: a power of two, as the transform needs. The
    !! binning's own smoothing is then far below any bandwidth the rule is asked to find.
    integer, parameter :: KDE_ISJ_CELLS = 16384

    !> The binning grid reaches this share of the sample's range beyond each extreme point, before it
    !! is clipped to the support.
    real(real64), parameter :: KDE_ISJ_WIDEN = 0.1_real64

    !> The fixed point's number of stages, `l`: the derivative whose norm is estimated first.
    integer, parameter :: KDE_ISJ_STAGES = 7

    !> The largest `t` the bracket search reaches, in units of the grid's span squared: a bandwidth
    !! as wide as the whole grid, far beyond any the rule can mean.
    real(real64), parameter :: KDE_ISJ_T_MAX = 1.0_real64

    !> The largest exponent `k**2 * pi**2 * t` a term of the fixed point's sums is formed at: beyond
    !! it `exp` falls below the normal range, and the terms are smaller than rounding can see.
    real(real64), parameter :: KDE_ISJ_EXP_LIMIT = 708.0_real64

    !> How many consecutive terms of a fixed-point sum take their exponential by recurrence, two
    !! multiplications each, before it is formed afresh by `exp`: the recurrence's rounding grows as
    !! the square of its length, to about `2e-13` here, and the sums cost a sixth of what one `exp` a
    !! term does.
    integer, parameter :: KDE_ISJ_RUN = 64

    !> The largest magnitude a sample's extreme point may have for the binning grid to be formed
    !! without overflowing in any order: a quarter of the largest number.
    real(real64), parameter :: KDE_ISJ_LIMIT = 0.25_real64*huge(1.0_real64)

    !> `pi**2`, and `2 * pi**(2s)` for `s = 2 .. 7`: the norm of the density's `s`-th derivative is
    !! `2 * pi**(2s) * sum_k k**(2s) * (y_k/2)**2 * exp(-k**2 * pi**2 * t)`, `y` the transform.
    !! Computed at 50 digits.
    real(real64), parameter :: KDE_ISJ_PI2 = 9.8696044010893586188_real64
    real(real64), parameter :: KDE_ISJ_NORM_C(2:7) = [194.81818206800487447_real64, &
        1922.7783871506088741_real64, 18977.062032141148014_real64, 187296.09495216604195_real64, &
        1848538.3630467483724_real64, 18244342.36350870634_real64]

    !> `log(2 * c_s * K0_s)` for `s = 2 .. 6`, with `c_s = (1 + 2**(-(s + 1/2)))/3` and
    !! `K0_s = (2s - 1)!!/sqrt(2*pi)`: the stage time `(2 c_s K0_s / (N |f^(s+1)|**2))**(2/(3 + 2s))`
    !! is formed through its logarithm, which cannot overflow. Computed at 50 digits.
    real(real64), parameter :: KDE_ISJ_LOG_C(2:6) = [-0.063012265988619525565_real64, &
        1.4683445817129673905_real64, 3.3728021712641165025_real64, 5.5486377704290425764_real64, &
        7.935664513152786701_real64]

    !> `log(2 * sqrt(pi))`: the last stage's `t = (2 N sqrt(pi) |f''|**2)**(-2/5)`, through its
    !! logarithm. Computed at 50 digits.
    real(real64), parameter :: KDE_ISJ_LOG_2SQRTPI = 1.2655121234846453965_real64

    ! ---- the adaptive rule --------------------------------------------------------------------

    !> The adaptive rule's default sensitivity: the square root of the pilot's inverse.
    real(real64), parameter :: KDE_ALPHA_DEFAULT = 0.5_real64

    !> How much wider the adaptive kernel's global bandwidth is than the same rule's for the fixed
    !! estimator, at `alpha = 0.5`: the factor is `KDE_ADAPT_INFLATE**(2*alpha)`, which is exactly
    !! one at `alpha = 0`, where the adaptive estimate IS the fixed one.
    !!
    !! A bandwidth rule answers the question the FIXED estimator asks. The adaptive kernel then
    !! narrows the kernel where the pilot is dense and widens it where the pilot is thin, and its
    !! own best global bandwidth is larger -- so a rule's number, used unchanged, oversharpens it.
    !!
    !! The value is measured, not chosen: `bench/benchmark_kde.sh`'s `mise` mode sweeps explicit
    !! bandwidths for both estimators over the Marron-Wand mixtures at two sample sizes and takes
    !! the median of `h*(adaptive)/h*(fixed)`. Re-run that mode after any change to the pilot or
    !! the rule; the individual ratios run from about 1.1 to about 2.4, so the median is a
    !! compromise and a run that moves it by a few per cent has not found a defect.
    real(real64), parameter :: KDE_ADAPT_INFLATE = 1.5_real64

    !> How many times the NARROWEST kernel the widest one may reach, when the caller caps neither
    !! `spread_max` nor `bandwidth_max`. The adaptive rule `h*(p/g)**(-alpha)` is unbounded as the
    !! pilot density goes to zero, so ANY density approaching zero anywhere -- at a bound or inside
    !! the support -- gives some point a kernel of unbounded width. Every later query then sums the
    !! points within reach of that kernel, and under a corrected boundary the zone it opens is
    !! SCANNED at the resolution of the narrowest kernel, which is what makes such a fit quadratic
    !! in the sample's size rather than linear.
    !!
    !! The SPREAD is the quantity that bounds that scan, which is why the default is written as one
    !! rather than as a multiple of the global bandwidth: the zone is `KDE_RADIUS*h_max` wide and
    !! the step is `h_min/KDE_LINEAR_SCAN_PER_H`, so the scan takes at most
    !! `KDE_LINEAR_SCAN_PER_H*KDE_RADIUS*KDE_SPREAD_MAX` steps whatever the sample is. A cap on
    !! `h_max/h` bounds nothing of the sort, the step being set by `h_min`.
    !!
    !! Loose on purpose, and not a tuning parameter. The library's own pilots stay inside a spread
    !! of a few, so the default bounds the pathological case and changes an ordinary estimate not
    !! at all; a caller who wants a cap that BINDS passes `spread_max=` or `bandwidth_max=`, and is
    !! not advised about it. `bench/benchmark_kde.sh`'s `scan` mode reports the spread each fit
    !! actually reaches.
    real(real64), parameter :: KDE_SPREAD_MAX = 100.0_real64

    ! ---- the cross-validation rule -------------------------------------------------------------

    !> The most points the LSCV criterion is evaluated over. The criterion is a double sum, so its
    !! cost is quadratic in this; capping it makes the rule's cost bounded and predictable whatever
    !! the sample's size, at the price of making the answer a draw rather than a function of the
    !! whole sample. The draw is addressed by `(seed, stream)` like `%sample`, so it is reproducible.
    !!
    !! **It bounds two things, and raising it must answer for both.** Besides the criterion's own
    !! double sum, it is what keeps the fixed arm's FALLBACK bounded: where the transform's geometry
    !! does not fit inside `KDE_LSCV_GRID_MAX` every candidate takes the pair sum instead, which is
    !! quadratic in whatever this cap allows through. The transform route does not lift the cap --
    !! it makes the cap cheap on the arm that has one.
    integer(int64), parameter :: KDE_LSCV_MAX = 2000_int64

    !> The seed the LSCV subsample is drawn at, so that one sample gives one bandwidth however
    !! often it is asked for, and two runs of a program agree.
    integer(int64), parameter :: KDE_LSCV_SEED = 20240917_int64

    !> How far either side of the starting bandwidth the criterion is minimised over, as a factor.
    !! Wide enough that the minimum is interior on every density this was measured against, narrow
    !! enough that the search does not spend its evaluations in the tails.
    real(real64), parameter :: KDE_LSCV_BRACKET = 4.0_real64

    !> How many golden-section steps the minimisation takes. The criterion is noisy near its
    !! minimum -- it is an estimate -- so refining far past a per cent of the bandwidth buys
    !! nothing a caller can use.
    integer, parameter :: KDE_LSCV_STEPS = 40

    !> Where the golden section stops, as a share of the bracket's midpoint.
    !!
    !! The criterion is an ESTIMATE of the integrated squared error, and it lands tens of per cent
    !! from the MISE optimum on an ordinary sample; four significant figures of its minimiser's
    !! POSITION is precision it does not have. A per cent costs about nine of the two dozen
    !! evaluations a ten-thousandth needs, and moves the chosen bandwidth by less than the
    !! criterion's own scatter.
    real(real64), parameter :: KDE_LSCV_TOL = 1.0e-2_real64

    !> The longest transform the fixed arm's criterion will lay over its points. Its cells resolve
    !! the NARROWEST candidate at `KDE_CURVE_BINNED_PER_H` to a bandwidth and its range covers the
    !! WIDEST candidate's reach beyond both ends, so a sample whose range is very many bandwidths
    !! wide asks for more than this; the criterion then falls back to the exact double sum, which
    !! answers the same question more slowly. That fallback is quadratic, and what bounds it is
    !! `KDE_LSCV_MAX` rather than anything here.
    integer, parameter :: KDE_LSCV_GRID_MAX = 262144

    !> `1/phi`, the golden section's ratio, at which the criterion's bracket is split.
    real(real64), parameter :: KDE_GOLDEN = 0.6180339887498949_real64

    !> `sqrt(2*pi)`, the normal density's normaliser, at 50 digits.
    real(real64), parameter :: KDE_ROOT_TWO_PI = 2.5066282746310002_real64

    !> Where the criterion's normal density is taken as zero, in standard deviations. The same cut
    !! the Gaussian kernel itself takes, so the criterion and the estimate agree about how far a
    !! kernel reaches.
    real(real64), parameter :: KDE_NORM_CUT = 5.0_real64

    ! ---- the curve's default method -------------------------------------------------------------

    !> Stands for "the caller named no method" between `%curve`'s argument checks and the point
    !! where its range is known, which is the first place the default can be decided.
    integer, parameter :: KDE_METHOD_AUTO = -1

    !> How finely the binned method has to resolve the narrowest kernel before its answer stops
    !! being distinguishable from the exact sum's: cells, or curve points, per bandwidth.
    !!
    !! The binned estimate's error falls as the SQUARE of its spacing, so the spacing decides the
    !! accuracy as much as the kernel does: on one sample it is a quarter of the peak density at
    !! three points, a twentieth at eleven, and only past a few hundred does it reach the exact
    !! sum's own agreement. Sixteen per bandwidth is where the difference stops being visible on a
    !! plot. Two places read it, and they ask the same question from opposite sides: `%curve`
    !! GATES on it, filling a curve the caller spaced at least this finely by the binned method
    !! rather than the exact sum when neither was named -- a curve too coarse to bin accurately is
    !! too short for the saving to matter -- and `%fit(method="binned")` CHOOSES by it, giving its
    !! grid this many cells to the narrowest kernel.
    real(real64), parameter :: KDE_CURVE_BINNED_PER_H = 16.0_real64

    ! ---- the binned fit's own grid --------------------------------------------------------------

    !> The fewest and the most cells the grid behind `pf_kde%fit(method="binned")` is given, either
    !! side of the count `KDE_CURVE_BINNED_PER_H` asks for over the estimate's extent.
    !!
    !! The lower clamp keeps an estimate narrower than a few bandwidths from getting a grid too
    !! coarse to interpolate; the upper one bounds the memory the transform's padded array takes,
    !! and is reached only where the extent is thousands of narrow kernels wide -- a spread near
    !! `KDE_SPREAD_MAX` over a range of many bandwidths. The cells are then wider than the
    !! sixteenth, so the fit SAYS the clamp bound rather than quietly answering a coarser estimate.
    integer, parameter :: KDE_FIT_MIN_CELLS = 64, KDE_FIT_MAX_CELLS = 65536

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

    !> What one point of a GRID's bulk query costs, in the same units the query team is sized in:
    !! after `%finish` has stored the total mass and the running integral, `%pdf` and `%cdf` are a
    !! segment look-up and a few operations, and `%quantile` adds a binary search over the cells.
    !! Far below one kernel evaluation, so a team opens only for a very long array.
    real(real64), parameter :: KDE_GRID_QUERY_WORK = 1.0_real64

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

    !> The cap `parquet_debug_set_kde_sample_tries` forces on `%sample`'s attempts before a draw
    !! falls back to an inversion; negative, the default, means `KDE_SAMPLE_TRIES`. Test-only,
    !! process-global and unsynchronised: the suite that sets it runs serially.
    integer, save :: kde_sample_tries_forced = -1

    !> The cell count `parquet_debug_set_kde_isj_cells` forces on the ISJ rule's binning grid; 0, the
    !! default, means `KDE_ISJ_CELLS`. Test-only, process-global and unsynchronised: the suite that
    !! sets it runs serially.
    integer, save :: kde_isj_cells_forced = 0

    !> The bandwidth-class count `parquet_debug_set_kde_binned_classes` forces on a binned adaptive
    !! grid; 0, the default, means the count `KDE_BINNED_H_STEP` and the rule's own spread give.
    !! Test-only, process-global and unsynchronised: the suite that sets it runs serially.
    integer, save :: kde_binned_classes_forced = 0

    !> Nanoseconds the most recent `pf_kde%fit` spent sorting, building the pilot and assigning the
    !! bandwidths (`parquet_debug_kde_fit_nanos`); 0 for a phase it did not reach. Process-global
    !! and unsynchronised, like the team counter.
    integer(int64), save :: kde_fit_ns(3) = 0_int64

    !> How many samples the most recent `pf_kde%fit`'s corrected boundary scan took, and how many of
    !! them it had to read from the exact estimator rather than from its grid
    !! (`parquet_debug_kde_scan_counts`). Process-global and unsynchronised, like the phase timers.
    integer(int64), save :: kde_scan_ns(2) = 0_int64

    !> Whether the corrected boundary scan may read its binned grid
    !! (`parquet_debug_set_kde_scan_grid`). Test-only, process-global and unsynchronised: the test
    !! that turns it off runs serially, and turns it back on.
    logical, save :: kde_scan_grid_on = .true.

    !> Whether the fixed arm's LSCV criterion may read its binning and transform
    !! (`parquet_debug_set_kde_lscv_grid`). Test-only, process-global and unsynchronised: the test
    !! that turns it off runs serially, and turns it back on.
    logical, save :: kde_lscv_grid_on = .true.

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
        logical :: has_bmax = .false.             !! an absolute cap was given
        real(real64) :: bmax = 0.0_real64         !! that cap, on every point's bandwidth
        real(real64) :: smax = KDE_SPREAD_MAX     !! the cap on the SPREAD `h_max/h_min`: the caller's
                                                  !! `spread_max`, or `KDE_SPREAD_MAX`. Always in force,
                                                  !! which is what bounds the corrected scan's step count
        real(real64) :: capf = 0.0_real64         !! the spread cap in units of the global bandwidth,
                                                  !! `smax*exp(-alpha*(log pmax - log g))`: `h_min` is
                                                  !! reached at the pilot's densest cell, so the cap is
                                                  !! known before the first point is looked at and the
                                                  !! rule stays one pass. Never above `smax`, since
                                                  !! `pmax >= g`
        integer :: nc = 0                         !! the pilot's number of cells
        real(real64) :: x0 = 0.0_real64           !! the pilot's first cell's left edge
        real(real64) :: x1 = 0.0_real64           !! the pilot's last cell's right edge
        real(real64) :: dx = 0.0_real64           !! the pilot's cell width
        real(real64) :: wt = 0.0_real64           !! the pilot's total weight
        logical :: unreadable = .false.           !! the pilot has no density to read: poisoned, or empty cells
        real(real64) :: logg = 0.0_real64         !! `log g`: the mean of `log p` over the pilot's own density
        real(real64) :: pmin = 0.0_real64         !! the pilot's smallest positive cell density
        real(real64) :: pmax = 0.0_real64         !! the pilot's largest cell density: with `pmin` it brackets
                                                  !! every bandwidth the rule can give, which is what the
                                                  !! binned method's classes are laid out over
        real(real64) :: digest = 0.0_real64       !! a position-weighted checksum of `acc`, formed once, so
                                                  !! that `%merge` refuses two rules that differ without
                                                  !! walking their cells; a match is still scanned in full
        real(real64), allocatable :: acc(:)
        !! the pilot's accumulation, cell for cell
    end type kde_adapt

    !> A local-polynomial boundary kernel at one query point for one bandwidth: there the kernel is
    !! `(a - b*(u - c))*dinv * K(u)`, `u` the offset in bandwidths from the point to the query,
    !! or `K(u)` itself where both bounds are a reach or more away (`plain`).
    !!
    !! **`"renormalise"` and `"linear"` are the `l = 0` and `l = 1` members of one family**, the
    !! kernel of a local polynomial fit of degree `l` at the query point, and this type carries
    !! both. From the truncated moments `a_l` of the kernel over the part of its support inside the
    !! bounds: the constant member is `a = 1`, `b = 0`, `c = 0` and `dinv = 1/a_0`, the kernel
    !! divided by the mass a kernel CENTRED AT THE QUERY POINT keeps inside the support; the linear
    !! member is `a = a_2`, `b = a_1`, `c = 0` and `dinv = 1/(a_0 a_2 - a_1**2)`. For a narrow
    !! two-sided interval both are written about the interval's midpoint `c` (`kde_corr_factor`).
    type :: kde_corr_kernel
        logical :: plain = .true.         !! both bounds a reach or more away: the plain kernel
        real(real64) :: a = 0.0_real64    !! the constant term
        real(real64) :: b = 0.0_real64    !! the slope, per bandwidth
        real(real64) :: c = 0.0_real64    !! the offset the slope is taken about
        real(real64) :: dinv = 0.0_real64 !! one over the moments' determinant
        real(real64) :: a0 = 1.0_real64
        !! `a_0`, the mass a kernel centred at the query point keeps inside the bounds: the constant
        !! member's whole correction, and what the `"renormalise"` sampler's envelope is bounded by.
        !! Exactly one for a plain kernel, which the bounds do not reach
    end type kde_corr_kernel

    !> One retained point's corrected term as a function: `g_j(x)`, the point's local-polynomial
    !! boundary kernel at its own bandwidth, or its magnitude for the sampler's envelope.
    !!
    !! It holds the point's SCALARS -- where it is, one over its bandwidth, the bounds and the
    !! kernel -- and nothing else: a query builds one on its stack per call, so nothing is copied but
    !! those and nothing points at the fitted object, which every query reads through `intent(in)`.
    type :: kde_point_fn
        integer :: code = KDE_GAUSSIAN     !! the kernel
        integer :: bcode = KDE_BOUNDARY_LINEAR !! which member of the family corrects it
        logical :: has_lower = .false.     !! a lower bound was given
        real(real64) :: lo = 0.0_real64    !! the lower bound
        logical :: has_upper = .false.     !! an upper bound was given
        real(real64) :: hi = 0.0_real64    !! the upper bound
        real(real64) :: xj = 0.0_real64    !! where the point is
        real(real64) :: hj = 0.0_real64    !! its bandwidth, whose reach bounds every integral of the
                                           !! term and whose knots break it into smooth pieces
        real(real64) :: r = 0.0_real64     !! one over its bandwidth
        logical :: absolute = .false.      !! evaluate `|g_j|`, which the zone sampler draws from
    contains
        procedure :: eval => kde_point_eval !! the term at one point
    end type kde_point_fn

    !> One zone's fit-time tables: the interval between a bound and the last correction edge that
    !! reaches into it, where the estimate is not the plain sum.
    !!
    !! `val(k)` is the integral over the zone of the term of point `j1 + k - 1`, and `cum` its
    !! running weighted sum from the zone's bound inwards, so that a query sums only its window and
    !! reads the rest. The stretches are where the raw estimate is negative and the clip removes it,
    !! ascending; `nu` is each one's integral of the raw estimate (negative) accumulated from the
    !! bound, and `base` the mass of the clipped estimate between the bound and the stretch's near
    !! end, which is what the distribution function answers inside it.
    type :: kde_zone
        logical :: on = .false.               !! the zone exists: a bound, and a kernel corrected there
        logical :: two_sided = .false.        !! the zones meet: a term can change sign twice here
        real(real64) :: edge = 0.0_real64     !! its inner end, `z_lo` or `z_hi`
        real(real64) :: mass = 0.0_real64     !! the clipped estimate's mass over it, over the total weight
        integer(int64) :: j1 = 1_int64        !! the first point that can reach it
        integer(int64) :: j2 = 0_int64        !! the last
        integer :: ns = 0                     !! how many stretches the clip removes
        real(real64), allocatable :: val(:)
        !! each point's integral of its term over the zone
        real(real64), allocatable :: cum(:)
        !! the running weighted sum of `val`, from the zone's bound inwards
        real(real64), allocatable :: s(:)
        !! each stretch's lower end
        real(real64), allocatable :: e(:)
        !! each stretch's upper end
        real(real64), allocatable :: nu(:)
        !! the stretches' integrals of the raw estimate, summed from the bound inwards
        real(real64), allocatable :: base(:)
        !! the clipped mass between the bound and each stretch's near end
        real(real64), allocatable :: sig(:)
        !! each point's sign change, where its term turns negative; its region's outer end where it
        !! does not turn at all. A term changes sign at most once in a one-sided zone, its factor
        !! increasing with the distance from the bound, so this is one number per point
        real(real64), allocatable :: neg(:)
        !! the magnitude of each point's term's integral over the part where it is negative
        real(real64), allocatable :: cabs(:)
        !! the running weighted sum of each point's integral of `|g_j|` over the zone, which the
        !! zone sampler chooses a point from
    end type kde_zone

    !> The ISJ rule's fixed-point function over one sample, for `pf_find_root`:
    !! `F(t) = t - xi*gamma^[l](t)`, which is negative below the rule's `t` and changes sign there.
    !!
    !! `t` is the squared bandwidth in units of the binning grid's span. `gamma^[l]` estimates the
    !! norm of the density's `l`-th derivative at the time `t`, then each lower derivative's at the
    !! time the one above it makes optimal, down to the second, and returns the time that minimises
    !! the asymptotic mean integrated squared error for that norm. The components hold everything
    !! that does not depend on `t`.
    type, extends(pf_rootfun) :: kde_isj_point
        real(real64) :: n_eff = 0.0_real64 !! the effective sample size, `N` of the fixed point
        integer :: kmax = 0                !! the highest frequency, one below the grid's cell count
        real(real64), allocatable :: kk(:)
        !! `k**2` for `k = 1 .. kmax`
        real(real64), allocatable :: p(:,:)
        !! `k**(2s) * (y_k/2)**2` for `k = 1 .. kmax` and `s = 2 .. KDE_ISJ_STAGES`, `y` the transform
        !! of the binned sample
    contains
        procedure :: eval => kde_isj_eval !! `F(t)`
    end type kde_isj_point

    !> A kernel density estimate accumulated on a fixed grid of cells from points streamed through
    !! it, which are forgotten.
    !!
    !! **The lifecycle is `%init` -> `%add`* -> `%finish` -> query.** `%init` fixes the geometry --
    !! `ncells` cells of width `step = (xmax - xmin)/ncells`, centred on `xmin + (i - 1/2)*step` --
    !! with the bandwidth, the kernel and the support, and with `pilot=` the adaptive rule. Each
    !! point `%add` accepts deposits its kernel on the cell centres it reaches; what the kernel puts
    !! beyond `xmin` or `xmax` is counted there but not located. `%merge` adds one grid to another.
    !! `%finish` closes the accumulation, and `finish=.true.` on the last `%add` or `%merge` is the
    !! one-line form of it. Only then do the queries answer: `%density` at the centres, `%pdf`
    !! interpolated between them, `%cdf` its integral and `%quantile` the inverse. They are
    !! read-only, so any number of threads may share a finished grid, and `%add` and `%merge` abort
    !! after it -- `%clear` reopens the grid, empty.
    type :: pf_kde_grid
        private
        logical :: initialised = .false.             !! `%init` has run
        logical :: finished = .false.                !! `%finish` has closed the accumulation; the
                                                     !! queries are open and `%add`/`%merge` are shut
        logical :: poisoned = .false.                !! a NaN kept by `skipnan = .false.` arrived
        logical :: reach_poisoned = .false.          !! under `"linear"`, a kernel wider than a grid with a
                                                     !! free edge crossed a bound: its weight beyond that edge
                                                     !! could not be counted by the plain kernel's mass
        integer :: nc = 0                            !! the number of cells
        real(real64) :: x0 = 0.0_real64              !! `xmin`, the first cell's left edge
        real(real64) :: x1 = 0.0_real64              !! `xmax`, the last cell's right edge
        real(real64) :: dx = 0.0_real64              !! the cell width
        real(real64) :: h = 0.0_real64               !! the bandwidth; the global one when adaptive
        integer :: kernel_code = KDE_BSPLINE         !! the kernel
        integer :: boundary_code = KDE_BOUNDARY_NONE !! the boundary correction
        integer :: method_code = KDE_METHOD_EXACT    !! how the cells are filled: the deposit or the
                                                     !! binning and the transform
        integer :: pad = 0                           !! under `"binned"`, the cells of pad at each end
                                                     !! of the transform's array, `J`
        integer :: ntr = 0                           !! under `"binned"`, the transform's length, `L`
        integer :: nclass = 0                        !! under `"binned"`, the bandwidth classes: 1 for a
                                                     !! fixed bandwidth, `C` for the adaptive kernel
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
        integer(int64) :: cnt_overreach = 0_int64    !! under `"linear"` with a free edge, how many points
                                                     !! had an adaptive bandwidth whose reach exceeded the
                                                     !! range's width, which is what `reach_poisoned` records
        real(real64) :: mass_total = 0.0_real64      !! what `%finish` left: the mass every query
                                                     !! normalises by, formed once
        real(real64), allocatable :: acc(:)
        !! each cell's accumulated weight per unit length; `acc(i)*step` is the weight in cell `i`
        real(real64), allocatable :: cum(:)
        !! what `%finish` left: `%pdf`'s integral from `xmin` to each cell centre, which `%cdf`,
        !! `%quantile` and `%sample` read instead of rebuilding it per call
        real(real64), allocatable :: hclass(:)
        !! under `"binned"`, each bandwidth class's representative bandwidth, ascending and spaced
        !! by `KDE_BINNED_H_STEP`; one entry for a fixed bandwidth
        real(real64), allocatable :: bins(:,:)
        !! under `"binned"`, the binned weight on each of the `L` padded centres, per bandwidth
        !! class: what `%add` fills and `%finish` transforms into `acc`. Released by `%finish`, so
        !! a finished binned grid holds what an exact one holds
        type(kde_adapt) :: adapt = kde_adapt()
        !! the adaptive rule, from the pilot `%init` was given; off for a fixed bandwidth. Default-
        !! initialised, so that the whole-object constructor `pf_kde_grid()` needs no value for it
    contains
        procedure :: init => grid_init !! fixes the geometry, the bandwidth, the kernel and the support
        generic :: add => grid_add_f64_r0, grid_add_f64_r1, grid_add_f32_r0, grid_add_f32_r1, &
            grid_add_col !! adds points
        procedure, private :: grid_add_f64_r0 !! one `real64` point
        procedure, private :: grid_add_f64_r1 !! an array of `real64` points
        procedure, private :: grid_add_f32_r0 !! one `real32` point, widened
        procedure, private :: grid_add_f32_r1 !! an array of `real32` points, widened
        procedure, private :: grid_add_col !! a numeric `parquet_column`, widened
        procedure :: merge => grid_merge !! adds another grid of the same geometry and settings
        procedure :: finish => grid_finish !! closes the accumulation; the queries open here
        procedure :: is_finished => grid_is_finished !! `%finish` has run
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
        procedure :: method => grid_method_name !! how the cells are filled
        procedure :: bounds => grid_bounds !! the support; infinite where unbounded
        procedure :: n => grid_n !! `size(x)` over every `%add`
        procedure :: n_valid => grid_n_valid !! the population's size
        procedure :: n_null => grid_n_null !! how many were excluded as null
        procedure :: n_nan => grid_n_nan !! how many were excluded as NaN
        procedure :: n_outside => grid_n_outside !! how many were outside the support
        procedure :: n_overreach => grid_n_overreach !! how many out-reached the range under `"linear"`
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
        integer :: kernel_code = KDE_BSPLINE         !! the kernel
        integer :: rule_code = KDE_RULE_ISJ          !! how the bandwidth was chosen
        integer :: boundary_code = KDE_BOUNDARY_NONE !! the boundary correction
        integer :: method_code = KDE_METHOD_EXACT    !! how the queries are answered: the exact sum over
                                                     !! the retained points, or the grid in `fit_grid`
        logical :: has_lower = .false.               !! a lower bound was given
        logical :: has_upper = .false.               !! an upper bound was given
        real(real64) :: lo = 0.0_real64              !! the lower bound, when given
        real(real64) :: hi = 0.0_real64              !! the upper bound, when given
        real(real64) :: h = 0.0_real64               !! the global bandwidth, after `adjust`
        real(real64) :: hinv = 0.0_real64            !! one over the global bandwidth
        real(real64) :: hmax = 0.0_real64            !! the largest point bandwidth
        real(real64) :: reach = 0.0_real64           !! `R*hmax`: how far a query reaches, formed once at
                                                     !! `%fit` rather than at the head of every query
        real(real64) :: ext_lo = 0.0_real64          !! the estimate's extent: the smallest `x_j - R h_j`, unclipped
        real(real64) :: ext_hi = 0.0_real64          !! and the largest `x_j + R h_j`, unclipped
        real(real64) :: dens_lo = 0.0_real64         !! where the density starts: the extent clipped to the
                                                     !! bounds, or, under `"linear"`, the crossing that ends a
                                                     !! stretch the clip removes there
        real(real64) :: dens_hi = 0.0_real64         !! where it stops, the mirror of `dens_lo`
        real(real64) :: zmid = 0.0_real64            !! the clipped estimate's mass between the zones, over `W`
        real(real64) :: znorm = 1.0_real64           !! `Z`: its mass over the whole support, which divides it
        real(real64) :: a0min = 1.0_real64           !! under `"renormalise"`, the smallest mass a kernel keeps
                                                     !! inside the support anywhere on it, which `%sample`
                                                     !! thins its proposals against; 1 under every other
                                                     !! correction, which do not use it
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
        real(real64), allocatable :: cdf_lo(:)
        !! each point's mass at or below the lower bound, summed over its images: a per-point
        !! constant `%cdf` subtracts at every query point, formed once at `%fit`. Allocated only
        !! under `"reflect"` with a lower bound, which is the one correction that reaches it
        real(real64), allocatable :: hb(:)
        !! each point's bandwidth, in the same order, read at `1 + (j - 1)*hstride`: one per point
        !! when adaptive, and the global bandwidth alone otherwise, so that both estimates run one
        !! loop and `alpha = 0` is the fixed estimate bit for bit
        real(real64), allocatable :: hr(:)
        !! one over each of `hb`, formed once at `%fit` by one scalar division each, so that a query
        !! multiplies by it instead of dividing
        real(real64), allocatable :: cwz(:)
        !! under `"linear"`, the running weighted sum of each point's kernel mass between the zones,
        !! which is what a query in the interior reads for the points left of its window
        type(kde_zone) :: zones(2)
        !! the two zones' tables, `KDE_ZONE_LO` the lower and `KDE_ZONE_HI` the upper, each off
        !! without its bound or without a kernel corrected there. An ARRAY rather than two
        !! components, so that every worker takes the fitted object and a zone INDEX: a component
        !! and its parent may not both be actual arguments of one call
        type(kde_adapt) :: adapt
        !! the adaptive rule; off for a fixed bandwidth
        type(pf_kde_grid) :: pilot_grid
        !! the pilot the adaptive rule reads, which `%pilot` hands back; uninitialised otherwise
        type(pf_kde_grid) :: fit_grid
        !! under `method = "binned"`, the finished grid every query is served from, over the whole
        !! extent the retained kernels reach; uninitialised under `"exact"`, which is the default
    contains
        generic :: fit => kde_fit_f64, kde_fit_f32, kde_fit_col !! fits the estimate to a sample
        procedure, private :: kde_fit_f64 !! the `real64` sample
        procedure, private :: kde_fit_f32 !! the `real32` sample, widened
        procedure, private :: kde_fit_col !! a numeric `parquet_column`, widened
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
        procedure :: method => kde_method_name !! how the queries are answered
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

    !> The bandwidth a rule gives a sample, without building an estimate over it:
    !! `call pf_kde_bandwidth(x, h, [rule], [adjust], [adaptive], [alpha], [lower], [upper],
    !! [is_valid], [weights], [weight_type], [skipnan], [n_null], [n_nan], [n_outside],
    !! [rule_used], [ok], [threads])`.
    !!
    !! Every argument means what it means to `pf_kde%fit`, and the answer is the number `%fit`
    !! would have resolved from the same arguments, bit for bit -- the two share one body, so they
    !! cannot drift. What it skips is everything AFTER the bandwidth: the pilot an adaptive fit
    !! would build, each point's own bandwidth and mass, and the boundary correction's zones.
    !!
    !! `adaptive = .true.` is worth giving when the number is meant for an adaptive estimate: the
    !! adaptive kernel's global bandwidth is wider than the same rule's for the fixed estimator,
    !! and this answers the one that will actually be used.
    !!
    !! **`kernel` is deliberately absent.** `bandwidth` is the kernel's standard deviation whatever
    !! the kernel, so one rule's number serves all four; accepting a kernel here would imply a
    !! dependence that does not exist.
    !!
    !! `rule_used` reports which rule produced the number, since the default falls back to
    !! Silverman's rule where the ISJ rule finds none; `ok` is `.false.` where no rule did, and `h`
    !! is then a quiet NaN. A subroutine rather than a function, because the rule actually used and
    !! the population counts are part of the answer.
    interface pf_kde_bandwidth
        module procedure kde_bandwidth_f64
        module procedure kde_bandwidth_f32
        module procedure kde_bandwidth_col
    end interface pf_kde_bandwidth

    interface

        !> Fits the estimate to a `real64` sample: `call k%fit(x, [bandwidth], [rule], [adjust],
        !! [kernel], [adaptive], [alpha], [bandwidth_max], [spread_max], [lower], [upper],
        !! [boundary], [is_valid], [weights], [weight_type], [skipnan], [n_null], [n_nan],
        !! [n_outside], [ok], [threads], [method])`.
        !!
        !! `x` is the sample, retained as a sorted copy of its population. `bandwidth` is the
        !! kernel's standard deviation, a finite positive number; without it the bandwidth comes
        !! from `rule`, `"isj"` (the default: the Improved Sheather-Jones rule), `"silverman"` or
        !! `"scott"`, and the two cannot both be given. When the rule is not named and the ISJ rule
        !! finds no bandwidth at its grid's resolution, Silverman's rule gives it instead, and
        !! `%rule` says so; when `rule = "isj"` is named, the estimate is then undefined.
        !! `adjust` multiplies the bandwidth however it was chosen (default 1). `kernel` is
        !! `"bspline"` (the default), `"gaussian"`, `"epanechnikov"` or `"box"`. `adaptive =
        !! .true.` gives each point its own bandwidth, `h * (p(x_j)/g)**(-alpha)`, from a pilot
        !! estimate at the global bandwidth `h`: `alpha` in `[0, 1]` (default 0.5) sets how far
        !! the bandwidths follow the pilot, `0` being the fixed estimate, and `bandwidth_max` caps
        !! every one; both need `adaptive = .true.`.
        !!
        !! `spread_max` caps the SPREAD instead: no point's bandwidth exceeds `spread_max` times
        !! the narrowest the rule can give. Unlike the other two it is in force whether it was
        !! named or not, at `KDE_SPREAD_MAX = 100` -- the adaptive rule is unbounded as the pilot
        !! density goes to zero, and an uncapped spread makes a `boundary = "linear"` fit quadratic
        !! in the sample's size. The default is far too loose to change an ordinary estimate. Where
        !! both caps are given the TIGHTER one binds, each being a statement about something
        !! different: `spread_max` about the estimator's shape, `bandwidth_max` about the data's
        !! scale. A cap that binds is reported by `%bandwidths`, and the fit advises when the
        !! DEFAULT one binds -- never when the caller set a cap themselves, an explicit request
        !! needing no advice. Given a rule rather than a number, an adaptive
        !! fit widens that rule's bandwidth, because a rule answers the question the fixed
        !! estimator asks and the adaptive kernel's own optimum is larger; an explicit `bandwidth`
        !! is never widened.
        !!
        !! `pilot` applies a smoothing MEASURED ON ANOTHER SAMPLE: a finished `pf_kde_grid`, as
        !! `pf_kde_grid%init` takes one, whose kernel, support and boundary correction must match
        !! this fit's. It also needs `adaptive = .true.`. Given alone it carries the SCALE too --
        !! the fit takes the pilot's own global bandwidth, so that "the same smoothing" means the
        !! same smoothing and not merely the same shape -- and `%rule` then answers `"explicit"`,
        !! since no rule was applied here. A `bandwidth` or `rule` beside it re-scales the
        !! transferred shape instead: the per-point ratios come from the pilot, the scale from
        !! this sample. `bandwidth_max` is worth giving with a foreign pilot, which can hand this
        !! sample's tail bandwidths orders of magnitude above the global one.
        !! `lower` and `upper` bound the support: a
        !! point outside is excluded and counted in `n_outside`, and a kernel crossing a bound is
        !! corrected by `boundary`, `"reflect"` (the default), `"renormalise"` or `"linear"`.
        !! `is_valid`,
        !! `weights`, `weight_type` and `skipnan` are the `pf_*` family's population arguments;
        !! `weight_type` decides the effective sample size the rules use. `n_null`, `n_nan` and
        !! `n_outside` report what each exclusion removed, and `ok` is `.false.` when the estimate
        !! is undefined. `threads` is the team for the sort, the rules' statistics and the pilot;
        !! the answer does not depend on it. Tokens are matched without regard to case.
        !!
        !! `method` chooses how every query is ANSWERED, and it is the one argument here that
        !! changes what the fitted object MEANS rather than only what it costs. `"exact"`, the
        !! default, sums the kernels of the points within reach of each query point. `"binned"`
        !! answers from one grid `%fit` builds over the estimate's whole extent: each point is split
        !! between the two cell centres around it and the cells are filled by one cosine transform,
        !! so `%pdf`, `%cdf`, `%quantile`, `%curve` and `%sample` cost the CELLS rather than the
        !! points, and a boundary correction costs nothing beyond them. It answers the estimate of a
        !! sample whose points have been moved to the cell centres, which converges to the exact one
        !! as the SQUARE of the cell width; the cells resolve the narrowest kernel, and the fit
        !! advises where the extent is too wide for that. The square is the `"gaussian"` and
        !! `"bspline"` kernels' rate; the other two converge more slowly, as they do on a binned
        !! `%curve`. `%bandwidths`, `%bandwidth_at`, `%pilot` and `%bandwidth` read the rule and
        !! answer the same under either method, and `%method` says which is in force.
        module subroutine kde_fit_f64(self, x, bandwidth, rule, adjust, kernel, adaptive, pilot, alpha, &
                bandwidth_max, spread_max, lower, upper, boundary, is_valid, weights, weight_type, skipnan, &
                n_null, n_nan, n_outside, ok, threads, method)
            implicit none
            class(pf_kde), intent(inout)           :: self          !! the estimate; refitted
            real(real64), intent(in)               :: x(:)          !! the sample
            real(real64), intent(in), optional     :: bandwidth     !! the kernel's standard deviation
            character(len=*), intent(in), optional :: rule          !! `"isj"`, `"silverman"` or `"scott"`
            real(real64), intent(in), optional     :: adjust        !! a factor on the bandwidth
            character(len=*), intent(in), optional :: kernel        !! the kernel's token
            logical, intent(in), optional          :: adaptive      !! each point takes its own bandwidth
            type(pf_kde_grid), intent(in), optional :: pilot        !! a pilot measured elsewhere, read
                                                                    !! instead of building one here
            real(real64), intent(in), optional     :: alpha         !! the adaptive rule's sensitivity
            real(real64), intent(in), optional     :: bandwidth_max !! caps every point's bandwidth
            real(real64), intent(in), optional     :: spread_max    !! caps the spread `h_max/h_min`
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
            character(len=*), intent(in), optional :: method        !! how the queries are answered
        end subroutine kde_fit_f64

        !> The shared body of `pf_kde%fit` and `pf_kde_bandwidth`: everything up to and including
        !! the global bandwidth, then -- unless `bandwidth_only` -- the pilot, each point's own
        !! bandwidth and mass, and the boundary correction's tables.
        !!
        !! One body deliberately. The population rules, the argument checks, the sort and the rules
        !! themselves are the part the two entry points must agree on to the bit, and the only way
        !! to guarantee that is for there to be one of each rather than two written alike.
        module subroutine kde_fit_core(self, x, bandwidth_only, bandwidth, rule, adjust, kernel, adaptive, &
                pilot, alpha, bandwidth_max, spread_max, lower, upper, boundary, is_valid, weights, weight_type, &
                skipnan, n_null, n_nan, n_outside, ok, threads, method)
            implicit none
            class(pf_kde), intent(inout)           :: self          !! the estimate; refitted
            real(real64), intent(in)               :: x(:)          !! the sample
            logical, intent(in)                    :: bandwidth_only !! stop once the bandwidth is known
            real(real64), intent(in), optional     :: bandwidth     !! the kernel's standard deviation
            character(len=*), intent(in), optional :: rule          !! `"isj"`, `"silverman"` or `"scott"`
            real(real64), intent(in), optional     :: adjust        !! a factor on the bandwidth
            character(len=*), intent(in), optional :: kernel        !! the kernel's token
            logical, intent(in), optional          :: adaptive      !! each point takes its own bandwidth
            type(pf_kde_grid), intent(in), optional :: pilot        !! a pilot measured elsewhere, read
                                                                    !! instead of building one here
            real(real64), intent(in), optional     :: alpha         !! the adaptive rule's sensitivity
            real(real64), intent(in), optional     :: bandwidth_max !! caps every point's bandwidth
            real(real64), intent(in), optional     :: spread_max    !! caps the spread `h_max/h_min`
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
            character(len=*), intent(in), optional :: method        !! how the queries are answered
        end subroutine kde_fit_core

        !> `pf_kde_bandwidth` over a `real64` sample; see the generic's own documentation.
        module subroutine kde_bandwidth_f64(x, h, rule, adjust, adaptive, alpha, lower, upper, &
                is_valid, weights, weight_type, skipnan, n_null, n_nan, n_outside, rule_used, ok, threads)
            implicit none
            real(real64), intent(in)               :: x(:)        !! the sample
            real(real64), intent(out)              :: h           !! the bandwidth; NaN where no rule found one
            character(len=*), intent(in), optional :: rule        !! `"isj"`, `"silverman"` or `"scott"`
            real(real64), intent(in), optional     :: adjust      !! a factor on the bandwidth
            logical, intent(in), optional          :: adaptive    !! the number is for an adaptive estimate
            real(real64), intent(in), optional     :: alpha       !! the adaptive rule's sensitivity
            real(real64), intent(in), optional     :: lower       !! the support's lower bound
            real(real64), intent(in), optional     :: upper       !! the support's upper bound
            logical, intent(in), optional          :: is_valid(:) !! `.false.` marks a null
            real(real64), intent(in), optional     :: weights(:)  !! per-element weights
            character(len=*), intent(in), optional :: weight_type !! `"reliability"` or `"frequency"`
            logical, intent(in), optional          :: skipnan     !! `.false.` lets a NaN poison
            integer(int64), intent(out), optional  :: n_null      !! excluded as null
            integer(int64), intent(out), optional  :: n_nan       !! excluded as NaN
            integer(int64), intent(out), optional  :: n_outside   !! excluded as outside the support
            character(len=:), allocatable, intent(out), optional :: rule_used !! which rule gave `h`
            logical, intent(out), optional         :: ok          !! a rule found a bandwidth
            integer, intent(in), optional          :: threads     !! the team for the sort and the rules
        end subroutine kde_bandwidth_f64

        !> `pf_kde_bandwidth` over a `real32` sample, widened first; otherwise as the `real64` form.
        module subroutine kde_bandwidth_f32(x, h, rule, adjust, adaptive, alpha, lower, upper, &
                is_valid, weights, weight_type, skipnan, n_null, n_nan, n_outside, rule_used, ok, threads)
            implicit none
            real(real32), intent(in)               :: x(:)        !! the sample
            real(real64), intent(out)              :: h           !! the bandwidth; NaN where no rule found one
            character(len=*), intent(in), optional :: rule        !! `"isj"`, `"silverman"` or `"scott"`
            real(real64), intent(in), optional     :: adjust      !! a factor on the bandwidth
            logical, intent(in), optional          :: adaptive    !! the number is for an adaptive estimate
            real(real64), intent(in), optional     :: alpha       !! the adaptive rule's sensitivity
            real(real64), intent(in), optional     :: lower       !! the support's lower bound
            real(real64), intent(in), optional     :: upper       !! the support's upper bound
            logical, intent(in), optional          :: is_valid(:) !! `.false.` marks a null
            real(real64), intent(in), optional     :: weights(:)  !! per-element weights
            character(len=*), intent(in), optional :: weight_type !! `"reliability"` or `"frequency"`
            logical, intent(in), optional          :: skipnan     !! `.false.` lets a NaN poison
            integer(int64), intent(out), optional  :: n_null      !! excluded as null
            integer(int64), intent(out), optional  :: n_nan       !! excluded as NaN
            integer(int64), intent(out), optional  :: n_outside   !! excluded as outside the support
            character(len=:), allocatable, intent(out), optional :: rule_used !! which rule gave `h`
            logical, intent(out), optional         :: ok          !! a rule found a bandwidth
            integer, intent(in), optional          :: threads     !! the team for the sort and the rules
        end subroutine kde_bandwidth_f32

        !> `pf_kde_bandwidth` over a numeric `parquet_column`, widened first. The column's own
        !! validity is the null mask, so `is_valid` cannot be given beside it.
        module subroutine kde_bandwidth_col(x, h, rule, adjust, adaptive, alpha, lower, upper, &
                is_valid, weights, weight_type, skipnan, n_null, n_nan, n_outside, rule_used, ok, threads)
            implicit none
            type(parquet_column), intent(in)       :: x           !! the sample, one numeric column
            real(real64), intent(out)              :: h           !! the bandwidth; NaN where no rule found one
            character(len=*), intent(in), optional :: rule        !! `"isj"`, `"silverman"` or `"scott"`
            real(real64), intent(in), optional     :: adjust      !! a factor on the bandwidth
            logical, intent(in), optional          :: adaptive    !! the number is for an adaptive estimate
            real(real64), intent(in), optional     :: alpha       !! the adaptive rule's sensitivity
            real(real64), intent(in), optional     :: lower       !! the support's lower bound
            real(real64), intent(in), optional     :: upper       !! the support's upper bound
            logical, intent(in), optional          :: is_valid(:) !! must be absent: the column's own
            real(real64), intent(in), optional     :: weights(:)  !! per-element weights
            character(len=*), intent(in), optional :: weight_type !! `"reliability"` or `"frequency"`
            logical, intent(in), optional          :: skipnan     !! `.false.` lets a NaN poison
            integer(int64), intent(out), optional  :: n_null      !! excluded as null
            integer(int64), intent(out), optional  :: n_nan       !! excluded as NaN
            integer(int64), intent(out), optional  :: n_outside   !! excluded as outside the support
            character(len=:), allocatable, intent(out), optional :: rule_used !! which rule gave `h`
            logical, intent(out), optional         :: ok          !! a rule found a bandwidth
            integer, intent(in), optional          :: threads     !! the team for the sort and the rules
        end subroutine kde_bandwidth_col

        !> The `real32` sample, widened to `real64` first; every other argument as the `real64`
        !! form.
        module subroutine kde_fit_f32(self, x, bandwidth, rule, adjust, kernel, adaptive, pilot, alpha, &
                bandwidth_max, spread_max, lower, upper, boundary, is_valid, weights, weight_type, skipnan, &
                n_null, n_nan, n_outside, ok, threads, method)
            implicit none
            class(pf_kde), intent(inout)           :: self          !! the estimate; refitted
            real(real32), intent(in)               :: x(:)          !! the sample
            real(real64), intent(in), optional     :: bandwidth     !! the kernel's standard deviation
            character(len=*), intent(in), optional :: rule          !! `"isj"`, `"silverman"` or `"scott"`
            real(real64), intent(in), optional     :: adjust        !! a factor on the bandwidth
            character(len=*), intent(in), optional :: kernel        !! the kernel's token
            logical, intent(in), optional          :: adaptive      !! each point takes its own bandwidth
            type(pf_kde_grid), intent(in), optional :: pilot        !! a pilot measured elsewhere, read
                                                                    !! instead of building one here
            real(real64), intent(in), optional     :: alpha         !! the adaptive rule's sensitivity
            real(real64), intent(in), optional     :: bandwidth_max !! caps every point's bandwidth
            real(real64), intent(in), optional     :: spread_max    !! caps the spread `h_max/h_min`
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
            character(len=*), intent(in), optional :: method        !! how the queries are answered
        end subroutine kde_fit_f32

        !> A numeric `parquet_column`, widened to `real64` first: `int32`, `int64`, `float32` or
        !! `float64`, and any other kind aborts, naming it. The column's own validity is the null
        !! mask, so `is_valid` cannot be given beside it; every other argument as the `real64` form.
        module subroutine kde_fit_col(self, x, bandwidth, rule, adjust, kernel, adaptive, pilot, alpha, &
                bandwidth_max, spread_max, lower, upper, boundary, is_valid, weights, weight_type, skipnan, &
                n_null, n_nan, n_outside, ok, threads, method)
            implicit none
            class(pf_kde), intent(inout)           :: self          !! the estimate; refitted
            type(parquet_column), intent(in)       :: x             !! the sample, one numeric column
            real(real64), intent(in), optional     :: bandwidth     !! the kernel's standard deviation
            character(len=*), intent(in), optional :: rule          !! `"isj"`, `"silverman"` or `"scott"`
            real(real64), intent(in), optional     :: adjust        !! a factor on the bandwidth
            character(len=*), intent(in), optional :: kernel        !! the kernel's token
            logical, intent(in), optional          :: adaptive      !! each point takes its own bandwidth
            type(pf_kde_grid), intent(in), optional :: pilot        !! a pilot measured elsewhere, read
                                                                    !! instead of building one here
            real(real64), intent(in), optional     :: alpha         !! the adaptive rule's sensitivity
            real(real64), intent(in), optional     :: bandwidth_max !! caps every point's bandwidth
            real(real64), intent(in), optional     :: spread_max    !! caps the spread `h_max/h_min`
            real(real64), intent(in), optional     :: lower         !! the support's lower bound
            real(real64), intent(in), optional     :: upper         !! the support's upper bound
            character(len=*), intent(in), optional :: boundary      !! the boundary correction
            logical, intent(in), optional          :: is_valid(:)   !! must be absent: the column's own
            real(real64), intent(in), optional     :: weights(:)    !! per-element weights
            character(len=*), intent(in), optional :: weight_type   !! `"reliability"` or `"frequency"`
            logical, intent(in), optional          :: skipnan       !! `.false.` lets a NaN poison
            integer(int64), intent(out), optional  :: n_null        !! excluded as null
            integer(int64), intent(out), optional  :: n_nan         !! excluded as NaN
            integer(int64), intent(out), optional  :: n_outside     !! excluded as outside the support
            logical, intent(out), optional         :: ok            !! the estimate is defined
            integer, intent(in), optional          :: threads       !! the team for the sort and the pilot
            character(len=*), intent(in), optional :: method        !! how the queries are answered
        end subroutine kde_fit_col

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

        !> The quantile at probability `p`: `call k%quantile(p, x)`, for `0 < p < 1` the smallest `x`
        !! at which `%cdf` reaches `p`. `p = 0` and `p = 1` answer the ends of the estimate's support,
        !! where its density starts and stops: the smallest `x_j - R h_j` and the largest
        !! `x_j + R h_j` over the retained points, `R` the kernel's reach in bandwidths and `h_j` each
        !! point's own bandwidth, clipped to the bounds. `p` outside `[0, 1]`, or NaN, aborts.
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
        !! [cut], [threads], [method])`.
        !!
        !! `x` receives the points, from `xmin` to `xmax` inclusive, and `f` the density at each.
        !! The default range is the sample's minimum minus `cut` bandwidths to its maximum plus
        !! `cut` bandwidths (`cut` defaults to 3), clipped to the support. `size(f) /= size(x)`,
        !! `xmin >= xmax` and a negative `cut` abort, and so does a DEFAULT range that has collapsed
        !! to one point -- a sample with no spread at `cut = 0`, say -- naming that rather than
        !! arguments the caller never passed. A single point is `xmin`, the range's first end, not
        !! its midpoint. On an undefined estimate `f` is NaN, and so is `x` unless both ends were
        !! given. `threads` as `%pdf` takes it.
        !!
        !! `method` is `"exact"`, the default, or `"binned"`, which splits each retained point
        !! between the two curve points around it and fills the curve with one cosine transform
        !! instead of summing kernels at every point: `O(n)` and one transform, against `O(n)`
        !! times the points one kernel reaches. It answers the estimate of a sample whose points
        !! have been moved to the curve's own points, converging to the exact curve as the SQUARE
        !! of their spacing, and it **honours the fit's boundary correction**, since `%curve` is
        !! `%pdf` on equally spaced points and the two must not disagree about what correction is
        !! in force. It needs at least two points, and aborts where the transform it would need is
        !! longer than `KDE_BINNED_L_MAX` times `size(x)`. `threads` governs the exact form only:
        !! the binned curve is one pass over the sample and one transform, with no per-point work
        !! for a team to share.
        module subroutine kde_curve(self, x, f, xmin, xmax, cut, threads, method)
            implicit none
            class(pf_kde), intent(in)          :: self    !! the fitted estimate
            real(real64), intent(out)          :: x(:)    !! the points
            real(real64), intent(out)          :: f(:)    !! the density at each point
            real(real64), intent(in), optional :: xmin    !! the first point
            real(real64), intent(in), optional :: xmax    !! the last point
            real(real64), intent(in), optional :: cut     !! bandwidths beyond the data, by default
            integer, intent(in), optional      :: threads !! the team for the points
            character(len=*), intent(in), optional :: method !! how the curve is filled
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

        !> How the bandwidth was chosen, as a token: `call k%rule(name)` answers the rule that gave
        !! it, `"isj"`, `"silverman"` or `"scott"`, or `"explicit"` when `bandwidth=` was given as a
        !! number. Under the default it answers `"silverman"` when the ISJ rule found no bandwidth
        !! and Silverman's rule gave it instead, and `"none"` on an undefined estimate, where no
        !! rule produced a bandwidth at all.
        module subroutine kde_rule_name(self, name)
            implicit none
            class(pf_kde), intent(in)                  :: self !! the fitted estimate
            character(len=:), allocatable, intent(out) :: name !! the rule's token
        end subroutine kde_rule_name

        !> How the queries are answered, as its token: `call k%method(name)`, `"exact"` or
        !! `"binned"` -- whichever `%fit`'s `method=` selected, which is `"exact"` by default.
        module subroutine kde_method_name(self, name)
            implicit none
            class(pf_kde), intent(in)                  :: self !! the fitted estimate
            character(len=:), allocatable, intent(out) :: name !! the method's token
        end subroutine kde_method_name

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

        !> One point's corrected term at `x`: its local-polynomial boundary kernel at its own
        !! bandwidth, `g_j(x)`, or `|g_j(x)|` when the object asks for the magnitude. What
        !! `term_piece`'s fixed rule evaluates for every per-point integral of the correction.
        module function kde_point_eval(this, x) result(f)
            implicit none
            class(kde_point_fn), intent(inout) :: this !! the point's scalars; never updated
            real(real64), intent(in)           :: x    !! where to evaluate
            real(real64)                       :: f    !! the term there
        end function kde_point_eval

        !> Releases everything; the object is unfitted again.
        module subroutine kde_clear(self)
            implicit none
            class(pf_kde), intent(inout) :: self !! the estimate
        end subroutine kde_clear

    end interface

    ! ---- the grid: set-up and accumulation, implemented in parquet_kde_grid.f90 ---------------

    interface

        !> Fixes the grid: `call g%init(ncells, xmin, xmax, bandwidth, [kernel], [pilot], [alpha],
        !! [bandwidth_max], [spread_max], [lower], [upper], [boundary], [method])`.
        !!
        !! `ncells` cells of width `step = (xmax - xmin)/ncells` span `[xmin, xmax]`, cell `i`
        !! centred on `xmin + (i - 1/2)*step`. `bandwidth` is the kernel's standard deviation and is
        !! always a number: a grid has no data to apply a rule to before the points arrive. `pilot`
        !! selects the adaptive kernel: an initialised grid whose range covers this one's, from which
        !! each point `%add` accepts takes the bandwidth `bandwidth * (p(x)/g)**(-alpha)`; it is
        !! copied, so the caller may discard it. A pilot with no density to read -- nothing in its
        !! cells, or a kept NaN -- makes every answer of this grid NaN, as a data condition does
        !! everywhere else. `alpha` (default 0.5), `bandwidth_max` and `spread_max` are
        !! as `pf_kde%fit` takes them; `alpha` and `bandwidth_max` need `pilot`, and `spread_max`
        !! is in force whether it was named or not. `kernel`, `lower`, `upper` and `boundary`
        !! are as `pf_kde%fit` takes them, and the range must lie inside the support; under
        !! `boundary="linear"` it must also START at `lower` and END at `upper` where those are
        !! given, and, where only one of them is given, be at least one kernel's reach wide, because
        !! the weight a grid counts beyond its own range is the plain kernel's mass there.
        !!
        !! `method` chooses how the cells are filled, and it is the one argument that changes what
        !! the grid COSTS rather than what it means. `"exact"`, the default, deposits each point's
        !! kernel on every cell centre it reaches. `"binned"` splits each point between the two
        !! centres around it and fills the cells with one cosine transform when `%finish` is called:
        !! `O(n)` to bin and `O(L log L)` once, against `O(n)` times the cells one kernel spans. It
        !! answers the estimate of a sample whose points have been moved to the cell centres, which
        !! converges to the exact estimate as the SQUARE of the cell width, and it aborts here where
        !! the transform it would need is longer than `KDE_BINNED_L_MAX` times `ncells`. Every
        !! accumulated point and count is discarded; a grid may be initialised again.
        module subroutine grid_init(self, ncells, xmin, xmax, bandwidth, kernel, pilot, alpha, &
                bandwidth_max, spread_max, lower, upper, boundary, method)
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
            real(real64), intent(in), optional      :: spread_max    !! caps the spread `h_max/h_min`
            real(real64), intent(in), optional      :: lower         !! the support's lower bound
            real(real64), intent(in), optional      :: upper         !! the support's upper bound
            character(len=*), intent(in), optional  :: boundary      !! the boundary correction
            character(len=*), intent(in), optional  :: method        !! how the cells are filled
        end subroutine grid_init

        !> Accumulates one `real64` point: `call g%add(x, [is_valid], [weights], [skipnan],
        !! [n_null], [n_nan], [n_outside])`, with `is_valid` and `weights` scalars; otherwise as the
        !! array form.
        module subroutine grid_add_f64_r0(self, x, is_valid, weights, skipnan, n_null, n_nan, n_outside, &
                n_overreach, finish)
            implicit none
            class(pf_kde_grid), intent(inout)     :: self      !! the grid
            real(real64), intent(in)              :: x         !! the point
            logical, intent(in), optional         :: is_valid  !! `.false.` marks it null
            real(real64), intent(in), optional    :: weights   !! its weight
            logical, intent(in), optional         :: skipnan   !! `.false.` lets a NaN poison
            integer(int64), intent(out), optional :: n_null    !! excluded as null
            integer(int64), intent(out), optional :: n_nan     !! excluded as NaN
            integer(int64), intent(out), optional :: n_outside !! excluded as outside the support
            integer(int64), intent(out), optional :: n_overreach !! out-reached the range under "linear"
            logical, intent(in), optional         :: finish    !! `.true.` finishes the grid after this call
        end subroutine grid_add_f64_r0

        !> Accumulates an array of `real64` points: `call g%add(x, [is_valid], [weights],
        !! [skipnan], [n_null], [n_nan], [n_outside], [threads])`.
        !!
        !! `is_valid`, `weights` and `skipnan` are the `pf_*` family's population arguments, and a
        !! point outside the support is excluded and counted; `n_null`, `n_nan`, `n_outside` and
        !! `n_overreach` report what THIS call excluded or found, while the grid's accessors report
        !! every call's total. A
        !! NaN kept by `skipnan = .false.` makes every later answer of the grid NaN. `threads`
        !! gives each thread of a team a static share of the points and a private partial grid,
        !! summed in thread order: at a given count the bits do not change, and between counts
        !! they differ by rounding.
        module subroutine grid_add_f64_r1(self, x, is_valid, weights, skipnan, n_null, n_nan, n_outside, &
                n_overreach, threads, finish)
            implicit none
            class(pf_kde_grid), intent(inout)     :: self        !! the grid
            real(real64), intent(in)              :: x(:)        !! the points
            logical, intent(in), optional         :: is_valid(:) !! `.false.` marks a null
            real(real64), intent(in), optional    :: weights(:)  !! per-element weights
            logical, intent(in), optional         :: skipnan     !! `.false.` lets a NaN poison
            integer(int64), intent(out), optional :: n_null      !! excluded as null
            integer(int64), intent(out), optional :: n_nan       !! excluded as NaN
            integer(int64), intent(out), optional :: n_outside   !! excluded as outside the support
            integer(int64), intent(out), optional :: n_overreach !! out-reached the range under "linear"
            integer, intent(in), optional         :: threads     !! the team for the deposit
            logical, intent(in), optional         :: finish      !! `.true.` finishes the grid after this call
        end subroutine grid_add_f64_r1

        !> Accumulates one `real32` point, widened to `real64` first; otherwise as the `real64` form.
        module subroutine grid_add_f32_r0(self, x, is_valid, weights, skipnan, n_null, n_nan, n_outside, &
                n_overreach, finish)
            implicit none
            class(pf_kde_grid), intent(inout)     :: self      !! the grid
            real(real32), intent(in)              :: x         !! the point
            logical, intent(in), optional         :: is_valid  !! `.false.` marks it null
            real(real64), intent(in), optional    :: weights   !! its weight
            logical, intent(in), optional         :: skipnan   !! `.false.` lets a NaN poison
            integer(int64), intent(out), optional :: n_null    !! excluded as null
            integer(int64), intent(out), optional :: n_nan     !! excluded as NaN
            integer(int64), intent(out), optional :: n_outside !! excluded as outside the support
            integer(int64), intent(out), optional :: n_overreach !! out-reached the range under "linear"
            logical, intent(in), optional         :: finish    !! `.true.` finishes the grid after this call
        end subroutine grid_add_f32_r0

        !> Accumulates an array of `real32` points, widened to `real64` first; otherwise as the
        !! `real64` form.
        module subroutine grid_add_f32_r1(self, x, is_valid, weights, skipnan, n_null, n_nan, n_outside, &
                n_overreach, threads, finish)
            implicit none
            class(pf_kde_grid), intent(inout)     :: self        !! the grid
            real(real32), intent(in)              :: x(:)        !! the points
            logical, intent(in), optional         :: is_valid(:) !! `.false.` marks a null
            real(real64), intent(in), optional    :: weights(:)  !! per-element weights
            logical, intent(in), optional         :: skipnan     !! `.false.` lets a NaN poison
            integer(int64), intent(out), optional :: n_null      !! excluded as null
            integer(int64), intent(out), optional :: n_nan       !! excluded as NaN
            integer(int64), intent(out), optional :: n_outside   !! excluded as outside the support
            integer(int64), intent(out), optional :: n_overreach !! out-reached the range under "linear"
            integer, intent(in), optional         :: threads     !! the team for the deposit
            logical, intent(in), optional         :: finish      !! `.true.` finishes the grid after this call
        end subroutine grid_add_f32_r1

        !> Accumulates a numeric `parquet_column`, widened to `real64` first: `int32`, `int64`,
        !! `float32` or `float64`, and any other kind aborts, naming it. The column's own validity is
        !! the null mask, so `is_valid` cannot be given beside it; otherwise as the `real64` array
        !! form.
        module subroutine grid_add_col(self, x, is_valid, weights, skipnan, n_null, n_nan, n_outside, &
                n_overreach, threads, finish)
            implicit none
            class(pf_kde_grid), intent(inout)     :: self        !! the grid
            type(parquet_column), intent(in)      :: x           !! the points, one numeric column
            logical, intent(in), optional         :: is_valid(:) !! must be absent: the column's own
            real(real64), intent(in), optional    :: weights(:)  !! per-element weights
            logical, intent(in), optional         :: skipnan     !! `.false.` lets a NaN poison
            integer(int64), intent(out), optional :: n_null      !! excluded as null
            integer(int64), intent(out), optional :: n_nan       !! excluded as NaN
            integer(int64), intent(out), optional :: n_outside   !! excluded as outside the support
            integer(int64), intent(out), optional :: n_overreach !! out-reached the range under "linear"
            integer, intent(in), optional         :: threads     !! the team for the deposit
            logical, intent(in), optional         :: finish      !! `.true.` finishes the grid after this call
        end subroutine grid_add_col

        !> Adds another grid's accumulation and counts to this one: `call g%merge(other)`. The two
        !! must share the number of cells, the range, the bandwidth, the kernel, the support, the
        !! boundary correction and the pilot -- the same copy, cell for cell, with the same `alpha`
        !! and `bandwidth_max` -- or the call aborts naming the first that differs. One grid per
        !! thread, merged at the end, is how a caller's own threads accumulate one estimate.
        module subroutine grid_merge(self, other, finish)
            implicit none
            class(pf_kde_grid), intent(inout) :: self   !! the grid that receives
            class(pf_kde_grid), intent(in)    :: other  !! the grid that is added; unchanged
            logical, intent(in), optional     :: finish !! `.true.` finishes the grid after this call
        end subroutine grid_merge

        !> Closes the accumulation: `call g%finish()`. The queries answer only after it, and
        !! `%add` and `%merge` abort after it -- `%clear` reopens the grid, empty.
        !!
        !! A grid has no `%fit` to be the seam between filling it and reading it, so the seam is
        !! named. Everything a query would otherwise rebuild per call is computed once here.
        !! A second `%finish` is a no-op, so a caller who cannot tell whether a helper already
        !! finished the grid may call it again. An uninitialised grid aborts.
        module subroutine grid_finish(self)
            implicit none
            class(pf_kde_grid), intent(inout) :: self !! the grid
        end subroutine grid_finish

        !> `.true.` once `%finish` has run, until `%clear`. Never aborts.
        module function grid_is_finished(self) result(res)
            implicit none
            class(pf_kde_grid), intent(in) :: self !! the grid
            logical                        :: res  !! it is finished
        end function grid_is_finished

    end interface

    ! ---- the binned method's own workers, implemented in parquet_kde_grid.f90 -----------------
    !
    ! Separate module procedures rather than helpers contained in that submodule, because
    ! `pf_kde%curve(method="binned")` reaches them from parquet_kde_fit.f90: it fills a grid whose
    ! cell centres are the curve's own points and reads the cells back, so the two callers share
    ! the binning and the transform rather than each writing one.

    interface

        !> Resolves everything the binned method fixes before a point arrives: the bandwidth
        !! classes, the pad and the transform's length, with the two refusals of that geometry.
        !! Called once the geometry and the adaptive rule are set, and before the bins are
        !! allocated, which are sized from it.
        !! `hlo` and `hhi` bracket every bandwidth a point can take. Absent, they are read from
        !! the grid's own adaptive rule, which is where a grid's bandwidths come from; the binned
        !! curve passes the FIT's own, one per retained point.
        module subroutine kde_binned_setup(self, entry, hlo, hhi, ok)
            implicit none
            class(pf_kde_grid), intent(inout)  :: self  !! the grid; its binned geometry is filled
            character(len=*), intent(in)       :: entry !! the binding, for the message
            real(real64), intent(in), optional :: hlo   !! the narrowest bandwidth, when known
            real(real64), intent(in), optional :: hhi   !! the widest, when known
            logical, intent(out), optional     :: ok    !! present: `.false.` where the transform would
                                                        !! be too long, instead of aborting. For a
                                                        !! caller with somewhere else to go; absent,
                                                        !! the call aborts as a caller's mistake
        end subroutine kde_binned_setup

        !> The transform array's cell centres, `%ntr` of them: the grid's own centres extended by
        !! `%pad` cells at each end, so that centre `pad + i` is cell `i`'s.
        pure module subroutine kde_padded_centres(self, cb)
            implicit none
            class(pf_kde_grid), intent(in) :: self  !! the grid
            real(real64), intent(out)      :: cb(:) !! the centres, `%ntr` of them
        end subroutine kde_padded_centres

        !> Bins the points `x(jlo:jhi)` into `bins`, one column per bandwidth class, and adds what
        !! lies beyond the transform array altogether to `below` and `above`.
        module subroutine kde_bin_range(self, cb, x, w, weighted, hb, hstride, jlo, jhi, bins, below, above)
            implicit none
            class(pf_kde_grid), intent(in) :: self      !! the grid, read for its geometry
            real(real64), intent(in)       :: cb(:)     !! the transform array's centres
            real(real64), intent(in)       :: x(:)      !! the points, inside the support
            real(real64), intent(in)       :: w(:)      !! their weights, when weighted
            logical, intent(in)            :: weighted  !! `w` holds one weight per point
            real(real64), intent(in)       :: hb(:)     !! the bandwidths, one per point or one for all
            integer(int64), intent(in)     :: hstride   !! 1 for one per point, 0 for one for all
            integer(int64), intent(in)     :: jlo       !! the first point to bin
            integer(int64), intent(in)     :: jhi       !! the last point to bin
            real(real64), intent(inout)    :: bins(:,:) !! the bins to add into, one column per class
            real(real64), intent(inout)    :: below     !! the weight below the array
            real(real64), intent(inout)    :: above     !! the weight above it
        end subroutine kde_bin_range

        !> Fills the cells from the bins: the one operation of the binned method that cannot
        !! stream, needing the last point to have arrived.
        module subroutine kde_binned_transform(self)
            implicit none
            class(pf_kde_grid), intent(inout) :: self !! the grid, its bins final
        end subroutine kde_binned_transform

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

        !> The interpolated density at each point of `x`: `call g%pdf(x, f, [threads])`.
        !! `size(f) /= size(x)` aborts. `threads` is the team the points are shared among; each
        !! point is computed by one thread alone, so the answer does not depend on it, and a team
        !! opens only where the work would pay for it.
        module subroutine grid_pdf_r1(self, x, f, threads)
            implicit none
            class(pf_kde_grid), intent(in) :: self    !! the grid
            real(real64), intent(in)       :: x(:)    !! where to evaluate
            real(real64), intent(out)      :: f(:)    !! the density at each point
            integer, intent(in), optional  :: threads !! the team for the points
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

        !> `P(X <= x)` at each point of `x`: `call g%cdf(x, p, [threads])`. `size(p) /= size(x)`
        !! aborts; `threads` as `%pdf` takes it.
        module subroutine grid_cdf_r1(self, x, p, threads)
            implicit none
            class(pf_kde_grid), intent(in) :: self    !! the grid
            real(real64), intent(in)       :: x(:)    !! where to evaluate
            real(real64), intent(out)      :: p(:)    !! the probability at or below each point
            integer, intent(in), optional  :: threads !! the team for the points
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

        !> The quantile at each probability of `p`: `call g%quantile(p, x, [threads])`.
        !! `size(x) /= size(p)` aborts; `threads` as `%pdf` takes it.
        module subroutine grid_quantile_r1(self, p, x, threads)
            implicit none
            class(pf_kde_grid), intent(in) :: self    !! the grid
            real(real64), intent(in)       :: p(:)    !! the probabilities
            real(real64), intent(out)      :: x(:)    !! the quantile at each
            integer, intent(in), optional  :: threads !! the team for the probabilities
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

        !> How the cells are filled, as its token: `call g%method(name)`, `"exact"` or `"binned"`.
        !! An uninitialised grid aborts, as `%kernel` does.
        module subroutine grid_method_name(self, name)
            implicit none
            class(pf_kde_grid), intent(in)             :: self !! the grid
            character(len=:), allocatable, intent(out) :: name !! the method's token
        end subroutine grid_method_name

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

        !> How many points, over every `%add`, had an adaptive bandwidth whose reach exceeded the
        !! grid's own range under `boundary = "linear"` with one bound free: the count behind R3,
        !! which leaves the grid undefined because the weight such a kernel puts beyond the free
        !! edge cannot be counted by the plain kernel's mass. Zero for every other configuration.
        !! `bandwidth_max`, or a wider range, is the remedy; the grid says so once per `%add`.
        module function grid_n_overreach(self) result(n)
            implicit none
            class(pf_kde_grid), intent(in) :: self !! the grid
            integer(int64)                 :: n    !! the count
        end function grid_n_overreach

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
        !! and its pilot's geometry. A poisoned grid says so in a line per reason: a kept NaN or an
        !! unreadable pilot, and, under `boundary="linear"`, a kernel wider than a grid with a free
        !! edge. Silenced by
        !! `verbosity = "silent"`; written to `unit` or, by default, where `message_stream` says.
        !! An uninitialised grid prints one line saying so rather than aborting.
        module subroutine grid_print(self, unit)
            implicit none
            class(pf_kde_grid), intent(in) :: self !! the grid
            integer, intent(in), optional  :: unit !! where to write
        end subroutine grid_print

        !> Zeroes the accumulation and every count, keeping the geometry and the settings: the grid
        !! is initialised, empty and UNFINISHED, so it accumulates again. Does nothing to a grid
        !! that was never initialised.
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
        module subroutine kde_adapt_set(a, pilot, alpha, has_bmax, bmax, smax)
            implicit none
            type(kde_adapt), intent(out)  :: a        !! the rule
            type(pf_kde_grid), intent(in) :: pilot    !! the pilot
            real(real64), intent(in)      :: alpha    !! the sensitivity, in `[0, 1]`
            logical, intent(in)           :: has_bmax !! an absolute cap was given
            real(real64), intent(in)      :: bmax     !! that cap
            real(real64), intent(in)      :: smax     !! the cap on the spread `h_max/h_min`, always in
                                                      !! force; `KDE_SPREAD_MAX` when the caller named none
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

        !> A `%print` row's label, blank-padded to the thirteen characters both printers give their
        !! label column -- the width of the longest label, `bandwidth_max` -- so that every row's value
        !! starts in the same column, text rows included.
        pure module function kde_label(text) result(lab)
            implicit none
            character(len=*), intent(in) :: text !! the label, at most thirteen characters
            character(len=13)            :: lab  !! the label, padded with blanks
        end function kde_label

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

        !> Resolves `method=` to its code, aborting on an unknown token.
        module subroutine kde_resolve_method(entry, token, code)
            implicit none
            character(len=*), intent(in) :: entry !! the binding, for the message
            character(len=*), intent(in) :: token !! the caller's token
            integer, intent(out)         :: code  !! the method's code
        end subroutine kde_resolve_method

        !> Validates and resolves the settings `pf_kde%fit` and `pf_kde_grid%init` share -- the
        !! kernel, the two bounds and the boundary correction -- in that order, aborting with the
        !! caller's `entry` on the first mistake. Absent bounds leave the support unbounded on that
        !! side; any bound without `boundary` selects `"reflect"`.
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

        !> `.true.` under a local-polynomial boundary correction: `"renormalise"` and `"linear"`,
        !! the `l = 0` and `l = 1` members of one family, which share the corrected kernel, the zone
        !! tables, `Z` and the quadrature. `"reflect"` and the unbounded estimate do not. The one
        !! home of that grouping; both forms read it.
        pure module function kde_is_corrected(bcode) result(res)
            implicit none
            integer, intent(in) :: bcode !! the boundary correction's code
            logical             :: res   !! it is a local-polynomial correction
        end function kde_is_corrected

        !> `.true.` for a finite number above zero. The NaN is screened first, as its own test: an
        !! ordered comparison raises `IEEE_INVALID` on one.
        pure module function kde_positive_finite(v) result(res)
            implicit none
            real(real64), intent(in) :: v   !! the value
            logical                  :: res !! it is finite and positive
        end function kde_positive_finite

        !> `.true.` for a bandwidth the estimate can use: NORMAL (so positive, and not subnormal),
        !! and finite once multiplied by the kernel's support radius, which is how far a kernel of
        !! it reaches. An adaptive rule can produce one that is neither, by overflow or underflow,
        !! and so can a caller. The one admission rule; both forms apply it.
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

        !> The kernel's truncated first moment, the integral of `u K(u)` from minus the support
        !! radius to `z` standard deviations: exactly 0 at and below minus the radius and at and
        !! above it, and even in `z`.
        pure module function kde_kernel_m1(code, z) result(m)
            implicit none
            integer, intent(in)      :: code !! the kernel
            real(real64), intent(in) :: z    !! the offset, in standard deviations
            real(real64)             :: m    !! the first moment at or below `z`
        end function kde_kernel_m1

        !> The kernel's truncated second moment, the integral of `u**2 K(u)` from minus the support
        !! radius to `z` standard deviations: exactly 0 at and below minus the radius and exactly the
        !! kernel's variance at and above it.
        pure module function kde_kernel_m2(code, z) result(m)
            implicit none
            integer, intent(in)      :: code !! the kernel
            real(real64), intent(in) :: z    !! the offset, in standard deviations
            real(real64)             :: m    !! the second moment at or below `z`
        end function kde_kernel_m2

        !> The multiplier one convolution of the binned method applies: the factor `pf_dct`
        !! coefficient `k` is multiplied by so that inverting gives the sample, binned onto cell
        !! centres `dx` apart, convolved with the kernel at bandwidth `hj`.
        !!
        !! **It is the SAMPLED kernel's transform, not the kernel's own.** The binned estimate at a
        !! centre is a sum of kernel values at offsets that are whole multiples of the cell width,
        !! so the filter is the kernel SAMPLED at that spacing, and the two differ by the aliases
        !! the sampling folds in. For a kernel whose transform decays fast the gap is invisible --
        !! `4.9e-07` of the peak for the Gaussian at eight cells per bandwidth -- and for one that
        !! decays slowly it is not: `8.4e-04` for the Epanechnikov kernel and `4.9e-02` for the box,
        !! whose transform falls off only as one over its argument.
        !!
        !! It is built by transforming the filter's own response to an impulse: the symmetric
        !! convolution of a unit at index 0 with the filter is `h(m) + h(m+1)`, whose transform
        !! divided by the impulse's own, `2 cos(pi k/(2 L))`, is the multiplier. That divisor
        !! vanishes only at `k = L`, which is one past the last coefficient.
        !!
        !! **Normalised by its own value at zero frequency**, which makes it exactly one there. So
        !! the convolution conserves the binned weight to the bit, and each point spreads exactly
        !! its own weight over the cells -- the same normalisation the exact deposit applies per
        !! point, which is what lets the two agree cell for cell.
        module subroutine kde_dct_filter(code, hj, dx, lam)
            implicit none
            integer, intent(in)      :: code   !! the kernel
            real(real64), intent(in) :: hj     !! the bandwidth
            real(real64), intent(in) :: dx     !! the cell width
            real(real64), intent(out) :: lam(:) !! the multiplier, one per coefficient
        end subroutine kde_dct_filter

        !> The multiplier the SECOND convolution of the binned local linear correction applies:
        !! the factor a `pf_dct` coefficient is multiplied by, one position down, so that inverting
        !! with `pf_idst` gives the binned sample convolved with the ODD kernel `u K(u)`.
        !!
        !! `lam(j)` belongs to `pf_dct` coefficient `j`, which carries frequency `j - 1`, so the
        !! caller forms `sc(1:L-1) = co(2:L) lam(2:L)` with `sc(L) = 0` -- the shift the guide page
        !! writes out, the sine coefficient at index `k` carrying frequency `k + 1`. `lam(1)` is
        !! zero: an odd kernel has nothing at zero frequency.
        !!
        !! Built by the same impulse route as `kde_dct_filter`, and divided by the SAME number --
        !! the even sampled filter's own mass -- so that the two convolutions carry one scale and
        !! their ratio is the correction the exact deposit applies.
        module subroutine kde_dst_filter(code, hj, dx, lam)
            implicit none
            integer, intent(in)       :: code   !! the kernel
            real(real64), intent(in)  :: hj     !! the bandwidth
            real(real64), intent(in)  :: dx     !! the cell width
            real(real64), intent(out) :: lam(:) !! the multiplier, one per cosine coefficient
        end subroutine kde_dst_filter

        !> The local-polynomial boundary kernel `bcode` names, at the query `t` inside the support,
        !! for a point whose reciprocal bandwidth is `r`: the kernel `K` corrected so that its
        !! zeroth moment over the part of its support inside the bounds is one -- and, under
        !! `KDE_BOUNDARY_LINEAR`, its first moment zero as well.
        !!
        !! The moments are taken over `[max(-R, -q), min(R, p)]`, `p = (t - lo) r` and
        !! `q = (hi - t) r` the distances to the bounds in bandwidths; a bound a reach or more away
        !! leaves the kernel plain. Where the interval is `KDE_CORR_NARROW` standard deviations
        !! wide or more they are differences of the closed forms (`kde_kernel_cdf`, `kde_kernel_m1`,
        !! `kde_kernel_m2`); below it they are formed centred on the interval's midpoint by
        !! `KDE_GL8_X`'s rule, per piece between the kernel's knots, scaled by the width so that
        !! nothing cancels and nothing underflows -- which is also where `a_0` alone would lose
        !! digits, two distribution functions of order one differencing to the width.
        !!
        !! `bcode` other than the two corrected codes is a caller's mistake, not a data condition,
        !! and answers the plain kernel.
        pure module function kde_corr_factor(bcode, code, has_lower, lo, has_upper, hi, t, r) result(cf)
            implicit none
            integer, intent(in)      :: bcode     !! the correction: renormalise (`l = 0`) or linear (`l = 1`)
            integer, intent(in)      :: code      !! the kernel
            logical, intent(in)      :: has_lower !! a lower bound was given
            real(real64), intent(in) :: lo        !! the lower bound
            logical, intent(in)      :: has_upper !! an upper bound was given
            real(real64), intent(in) :: hi        !! the upper bound
            real(real64), intent(in) :: t         !! the query, inside the support
            real(real64), intent(in) :: r         !! one over the point's bandwidth
            type(kde_corr_kernel)    :: cf        !! the kernel's correction there
        end function kde_corr_factor

        !> The corrected kernel's value at the offset `u`, in bandwidths, from the plain kernel's
        !! value there, `k = K(u)`: `k` itself where `cf` is plain.
        pure module function kde_corr_value(cf, u, k) result(v)
            implicit none
            type(kde_corr_kernel), intent(in) :: cf !! the correction at the query
            real(real64), intent(in)          :: u  !! the offset, in bandwidths
            real(real64), intent(in)          :: k  !! the plain kernel's value there
            real(real64)                      :: v  !! the corrected kernel's value
        end function kde_corr_value

        !> The integral of one point's corrected term `fn` between `s` and `t`, both inside the
        !! support: closed form by the kernel's own distribution function where the term is plain,
        !! a fixed Gauss-Legendre rule per analytic piece where the correction is acting.
        !! `fn%absolute` integrates the magnitude instead, whose sign change the caller supplies.
        !!
        !! The one home of the per-point integral: `pf_kde`'s zone tables, stretches and envelope
        !! read it, and `pf_kde_grid`'s deposit reads it for the corrected weight it puts beyond its
        !! own range. Only the part the kernel reaches counts; the piece between the two correction
        !! edges is the plain kernel's, and each corrected piece is integrated with the point's own
        !! breakpoints -- the kernel's knots, its moments' knots, the two correction edges and, for
        !! the sampler, the sign change.
        module function kde_term_integral(fn, s, t, sigma, has_sigma) result(v)
            implicit none
            type(kde_point_fn), intent(inout) :: fn        !! the point's scalars; never updated
            real(real64), intent(in)          :: s         !! the lower end
            real(real64), intent(in)          :: t         !! the upper end
            real(real64), intent(in)          :: sigma     !! the term's sign change
            logical, intent(in)               :: has_sigma !! the sign change is known
            real(real64)                      :: v         !! the integral
        end function kde_term_integral

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

        !> The effective sample size the rules use: the population's size unweighted, `sum(w)` under
        !! frequency weights and Kish's `sum(w)**2/sum(w**2)` under reliability weights.
        pure module function kde_n_eff(m, freq, weights) result(n_eff)
            implicit none
            integer(int64), intent(in)         :: m          !! the population's size
            logical, intent(in)                :: freq       !! frequency weights
            real(real64), intent(in), optional :: weights(:) !! its weights, when weighted
            real(real64)                       :: n_eff      !! the effective sample size
        end function kde_n_eff

        !> The bandwidth the Improved Sheather-Jones rule gives the population `x`, ascending, with
        !! its `weights`: `h = sqrt(t) * R`, `t` the smallest root of its fixed point at or above
        !! one cell of the binning grid and `R` the grid's span. Not multiplied by `adjust`.
        !!
        !! The survivors are binned by `pf_bin_linear` onto the centres of `KDE_ISJ_CELLS` cells (or
        !! the count `parquet_debug_set_kde_isj_cells` forces) spanning the sample's range widened
        !! by `KDE_ISJ_WIDEN` of it on each side and clipped to the support; a point in the half cell
        !! beyond the first or last centre lands whole on it, which is where the reflecting
        !! boundary of the cosine basis puts it. The binned mass, normalised to one, is transformed
        !! by `pf_dct`, and `pf_find_root` grows a bracket from `t` of one cell until the fixed
        !! point changes sign. `found` is `.false.` -- and `h` NaN -- for a sample with no spread or
        !! too extreme a range to bin, one whose fixed point is not negative at one cell (its root,
        !! if any, lies below what the grid resolves), and one with no sign change up to
        !! `KDE_ISJ_T_MAX`.
        !!
        !! A bandwidth below the smallest positive gap between neighbouring values of `x` is refused
        !! the same way: a kernel narrower than the closest pair of distinct observations resolves
        !! structure the sample cannot express, which is what values rounded to a step the grid
        !! resolves ask for.
        !> The bandwidth minimising the least-squares cross-validation criterion over the
        !! population `x`, ascending:
        !!
        !! `LSCV(h) = integral of fhat_h**2  -  (2/n) * sum_i fhat_{h,-i}(x_i)`,
        !!
        !! which estimates the integrated squared error less the part that does not depend on `h`.
        !! Unlike a plug-in rule it is defined for whatever estimator is in force, so asked for with
        !! `adaptive = .true.` it scores the ADAPTIVE estimate and returns that estimator's own
        !! bandwidth rather than the fixed one's.
        !!
        !! **Evaluated for the Gaussian kernel**, as every other rule here is: a bandwidth is the
        !! kernel's standard deviation, so one number serves all four and the criterion has a
        !! closed form in this one. Both terms are exact -- no quadrature -- through
        !! `(K*K)(z) = phi_sqrt2(z)` for the Gaussian.
        !!
        !! **Evaluated over at most `KDE_LSCV_MAX` points**, drawn without replacement at a fixed
        !! seed when the population is larger, because the criterion is a double sum. That makes the
        !! cost bounded whatever `n` is, and the answer reproducible but not a function of every
        !! point.
        !!
        !! `h0` seeds the bracket, which spans `KDE_LSCV_BRACKET` either side of it. `found` is
        !! `.false.` -- and `h` NaN -- where the population is too small to leave anything out of,
        !! or `h0` is not a usable bandwidth to search around.
        module subroutine kde_lscv_bandwidth(x, weights, adaptive, alpha, kernel_code, has_lower, lo, &
                has_upper, hi, boundary_code, w_total, h0, h, found)
            implicit none
            real(real64), intent(in)           :: x(:)       !! the population, ascending
            real(real64), intent(in), optional :: weights(:) !! its weights, when weighted
            logical, intent(in)                :: adaptive   !! score the adaptive estimate
            real(real64), intent(in)           :: alpha      !! the adaptive rule's sensitivity
            integer, intent(in)                :: kernel_code !! the kernel the pilot is built with
            logical, intent(in)                :: has_lower  !! a lower bound was given
            real(real64), intent(in)           :: lo         !! the lower bound
            logical, intent(in)                :: has_upper  !! an upper bound was given
            real(real64), intent(in)           :: hi         !! the upper bound
            integer, intent(in)                :: boundary_code !! the boundary correction
            real(real64), intent(in)           :: w_total    !! the population's total weight
            real(real64), intent(in)           :: h0         !! the bandwidth the search starts from
            real(real64), intent(out)          :: h          !! the bandwidth; NaN when none was found
            logical, intent(out)               :: found      !! the rule found a bandwidth
        end subroutine kde_lscv_bandwidth

        module subroutine kde_isj_bandwidth(x, freq, weights, has_lower, lo, has_upper, hi, h, found)
            implicit none
            real(real64), intent(in)           :: x(:)       !! the population, ascending, inside the support
            logical, intent(in)                :: freq       !! frequency weights
            real(real64), intent(in), optional :: weights(:) !! its weights, when weighted
            logical, intent(in)                :: has_lower  !! a lower bound was given
            real(real64), intent(in)           :: lo         !! the lower bound
            logical, intent(in)                :: has_upper  !! an upper bound was given
            real(real64), intent(in)           :: hi         !! the upper bound
            real(real64), intent(out)          :: h          !! the bandwidth; NaN when none was found
            logical, intent(out)               :: found      !! the rule found a bandwidth
        end subroutine kde_isj_bandwidth

        !> The ISJ rule's fixed-point function at `x`, the squared bandwidth in units of the grid's
        !! span: `x - xi*gamma^[l](x)`. A stage whose norm is zero or too small to divide by makes
        !! the answer `-huge`, the limit the function falls to there. Never NaN and never overflows.
        module function kde_isj_eval(self, x) result(f)
            implicit none
            class(kde_isj_point), intent(inout) :: self !! the sample's binned transform
            real(real64), intent(in)            :: x    !! `t`
            real(real64)                        :: f    !! `F(t)`
        end function kde_isj_eval

        !> Widens a numeric `parquet_column` for a column form: `int32`, `int64`, `float32` and
        !! `float64` are accepted, and any other kind aborts with `entry`, naming it. `wide` is the
        !! column as `real64` and `mask` its validity, unallocated when it has no null, so that it
        !! passes as an absent `is_valid`; `is_valid` itself must be absent, or the family's widener
        !! aborts.
        module subroutine kde_widen_column(entry, x, is_valid, wide, mask)
            implicit none
            character(len=*), intent(in)           :: entry    !! the binding, for the message
            type(parquet_column), intent(in)       :: x        !! the column
            logical, intent(in), optional          :: is_valid(:) !! the caller's `is_valid`, refused
            real(real64), allocatable, intent(out) :: wide(:)  !! the values, widened
            logical, allocatable, intent(out)      :: mask(:)  !! their validity, or unallocated
        end subroutine kde_widen_column

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

        !> Forces the number of cells the ISJ rule bins the sample into; `n <= 0` restores
        !! `2**14`. Otherwise `n` must be a power of two from `2**4` to `2**20`, or the call aborts.
        !!
        !! Test-only, and public for that reason alone: an independent oracle can reproduce the
        !! rule's transform at `2**10` cells to fifty digits, and not at `2**14` in the time a test
        !! may take, so this is how a test reaches the rule's own arithmetic. Process-global and
        !! unsynchronised; the suite that calls it runs serially.
        module subroutine parquet_debug_set_kde_isj_cells(n)
            implicit none
            integer, intent(in) :: n !! the cell count, a power of two; `<= 0` for the default
        end subroutine parquet_debug_set_kde_isj_cells

        !> Forces the number of bandwidth classes a binned adaptive grid convolves at; `n <= 0`
        !! restores the count the rule's own spread gives. Otherwise `n` is used as it stands, up
        !! to `KDE_BINNED_CLASSES_MAX`.
        !!
        !! Test-only, and public for that reason alone: the class count follows the bandwidth
        !! spread by design, so there is no argument a caller could vary it with, and this is how a
        !! test reaches the bucketing law -- that splitting each point's weight between its two
        !! neighbouring classes makes the error fall as the SQUARE of the class count, where
        !! rounding to the nearest class would leave it falling as the count. Process-global and
        !! unsynchronised; the suite that calls it runs serially.
        module subroutine parquet_debug_set_kde_binned_classes(n)
            implicit none
            integer, intent(in) :: n !! the class count; `<= 0` for the rule's own
        end subroutine parquet_debug_set_kde_binned_classes

        !> Caps `%sample`'s attempts before a draw falls back to an inversion: the redraw loop that
        !! places a kernel's variate inside the support, and, under `"linear"`, the zone sampler's
        !! rejection. `n = 0` sends every draw straight to its fallback; a NEGATIVE `n` restores
        !! `KDE_SAMPLE_TRIES`. (`parquet_debug_set_kde_pilot_cells` restores on `0` instead, because
        !! a cell count of zero means nothing, where a cap of zero is the case this hook exists for.)
        !!
        !! Test-only, and public for that reason alone: no fixture a test may take the time to build
        !! makes thirty-two rejections in a row certain, so this is how a test reaches the fallback
        !! and proves it draws from the same distribution. Process-global and unsynchronised; the
        !! suite that calls it runs serially, and restores it before it returns.
        module subroutine parquet_debug_set_kde_sample_tries(n)
            implicit none
            integer, intent(in) :: n !! the cap; `0` for the fallback alone, negative for the default
        end subroutine parquet_debug_set_kde_sample_tries

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

        !> How many samples the most recent `pf_kde%fit`'s corrected boundary scan took, over both
        !! zones, and how many of those it read from the exact estimator rather than from its
        !! binned grid. Both zero for a fit with no corrected boundary.
        !!
        !! Test-and-bench only, and public for that reason: `bench/benchmark_kde.sh`'s `scan` mode
        !! reports `exact/steps` as the share of the scan the grid could not decide, which is what
        !! says whether `KDE_SCAN_GRID_BAND` is where it should be -- a band that almost never
        !! falls back needs no refining, and one that falls back everywhere has removed nothing.
        !! Process-global and unsynchronised: read it from a serial context straight after the fit.
        module subroutine parquet_debug_kde_scan_counts(steps, exact)
            implicit none
            integer(int64), intent(out) :: steps !! samples the scan took, over both zones
            integer(int64), intent(out) :: exact !! those of them the exact estimator answered
        end subroutine parquet_debug_kde_scan_counts

        !> Turns the corrected boundary scan's binned grid off, so that every sample is read from
        !! the exact estimator, and on again.
        !!
        !! Test-only, and public for that reason: it is the control arm for the grid. A test fits
        !! with it on, fits again with it off and compares, which is the only way from inside one
        !! process to show that the grid changed WHERE the exact estimator is asked and not WHAT
        !! the fit answers. Process-global and unsynchronised; turn it back on.
        module subroutine parquet_debug_set_kde_scan_grid(on)
            implicit none
            logical, intent(in) :: on !! `.false.` scans every sample exactly
        end subroutine parquet_debug_set_kde_scan_grid

        !> Turns the fixed arm's LSCV transform route off, so that the criterion is summed over
        !! every pair, and on again.
        !!
        !! Test-only, and public for that reason: it is the control arm for the transform. A test
        !! resolves a bandwidth with it on, resolves it again with it off and compares, which is
        !! the only way from inside one process to show that the two routes score the same
        !! criterion. Process-global and unsynchronised; turn it back on.
        module subroutine parquet_debug_set_kde_lscv_grid(on)
            implicit none
            logical, intent(in) :: on !! `.false.` sums the criterion over every pair
        end subroutine parquet_debug_set_kde_lscv_grid

        !> The least-squares cross-validation criterion of the FIXED estimator, at one bandwidth,
        !! over the points given.
        !!
        !! `integral of fhat**2 - (2/n) sum_i fhat_{-i}(x_i)`, with the normal density
        !! `exp(-z**2/(2 s**2))/(s sqrt(2 pi))` taken as exactly zero beyond `KDE_NORM_CUT`
        !! standard deviations, summed over every pair of the points as given: **no subsample is
        !! drawn and nothing is minimised**, so the answer is a closed form over its arguments and
        !! nothing else. `rule="lscv"` minimises this same function; this reads it at a point.
        !!
        !! Test-only, and public for that reason: it is what lets the golden oracle
        !! (`tools/generate_kde_vectors.py`) certify the criterion itself at fifty digits. The
        !! MINIMISER cannot be certified that way -- an oracle for it would have to reproduce the
        !! golden section, the bracket and `KDE_LSCV_TOL`, pinning the implementation rather than
        !! the mathematics -- so the criterion is pinned here and the bandwidth is pinned by
        !! `test_lscv_rule` and `test_lscv_transform_agrees` instead.
        !!
        !! Honours `parquet_debug_set_kde_lscv_grid`, so a test can read the same criterion by the
        !! transform route and by the pair sum and compare them. The points need not be ordered.
        module subroutine parquet_debug_kde_lscv_at(x, h, crit, ok, weights)
            implicit none
            real(real64), intent(in)           :: x(:)       !! the points, in any order
            real(real64), intent(in)           :: h          !! the bandwidth to read the criterion at
            real(real64), intent(out)          :: crit       !! the criterion's value there
            logical, intent(out)               :: ok         !! the criterion could be formed
            real(real64), intent(in), optional :: weights(:) !! per-point weights; all one when absent
        end subroutine parquet_debug_kde_lscv_at

    end interface

end module parquet_kde ! GCOVR_EXCL_LINE
