!> Tests for `parquet_kde`: `pf_kde`'s kernels, bandwidth rules, population rules, boundary
!> corrections, exact queries and curve, `pf_kde_grid`'s deposit, merge and interpolated queries,
!> the adaptive kernel in both forms, and `%sample` in both forms.
!!
!! **The grid is tested against the exact estimate**, which the golden vectors pin: its density
!! converges to `pf_kde%pdf` as the square of the cell width, its deposit conserves every point's
!! weight to rounding, and the weight it counts beyond its range is the exact estimate's own
!! distribution function there.
!!
!! **The adaptive kernel is tested against an exact pilot.** The oracle sums the fixed estimate
!! at every point for the pilot and integrates its entropy for `g`, so the library, reading a
!! pilot grid, agrees to that grid's discretisation: to about `2e-3` at the rule's quarter
!! bandwidth, and -- in `kde_serial`, where `parquet_debug_set_kde_pilot_cells` makes the pilot
!! fine -- to `1e-9`, falling as the square of the pilot's cell width. `alpha = 0` is the fixed
!! estimate bit for bit, in both forms, which is what keeps the adaptive loop honest.
!!
!! **`%sample` is tested by its recipe and its moments**, never by a goodness-of-fit statistic. The
!! recipe -- which stream and which draw each element reads -- is reproduced here from the generator
!! itself, beside a control arm without the family's label that must match nowhere; the moments of a
!! sample from each kernel, corrected at a bound or not, are asserted against the kernel's own at a
!! tolerance of five standard errors for the sample's size. The seeds are fixed, so each assertion is
!! deterministic.
!!
!! **Every expectation is derived, never read off a run.** The golden vectors come from
!! `tools/generate_kde_vectors.py`, a 50-digit oracle that sums every kernel of every point;
!! the kernel identities (unit mass, unit variance, a CDF that is the density's integral, unit
!! mass inside a bounded support) are checked against `pf_integrate`, which shares nothing with
!! the estimator; and the rules are re-formed here from `pf_stddev` and `pf_iqr`'s own answers.
!!
!! **Every test whose answer depends on the bandwidth rule names the rule**, or passes the
!! bandwidth as a number, except `test_default_rule_is_silverman`, which is the one assertion of
!! the default and which the change making ISJ the default replaces (feature_kde.md, 12.1).
!!
!! Two suites. `kde` is pure in-memory work and runs concurrently. `kde_serial` holds the tests
!! that write process-global state -- they silence `%print` through the `verbosity` setting, or
!! force the pilot's cells through `parquet_debug_set_kde_pilot_cells` -- and is on
!! `suite_is_safe_to_parallelize`'s exclusion list. Both are registered in `run_tester_pf.f90`,
!! the runner that executes no `bind(C)` call.
module test_kde

    use testdrive, only : new_unittest, unittest_type, error_type, check
    use parquet_kde
    use parquet_stats, only : pf_stddev, pf_iqr, pf_count_valid
    use parquet_integrate, only : pf_integrand, pf_integrate
    use parquet_random, only : pf_random_at, pf_random_int_at, pf_random_normal_at, pf_random_key
    use test_kde_golden
    use iso_fortran_env, only : int64, real32, real64
    use, intrinsic :: ieee_arithmetic, only : ieee_value, ieee_quiet_nan, ieee_positive_inf, &
        ieee_is_nan, ieee_is_finite

    implicit none
    private

    public :: collect_tests_kde, collect_tests_kde_serial

    !> The four kernels' tokens, in the order the module's kernel table lists them.
    character(len=12), parameter :: KERNELS(4) = [character(len=12) :: &
        "gaussian", "epanechnikov", "bspline", "box"]

    !> Each kernel's support radius in standard deviations: the Gaussian's cut at five, and
    !> `sqrt(5)`, `2*sqrt(3)` and `sqrt(3)` for the three compact ones.
    real(real64), parameter :: RADIUS(4) = [5.0_real64, sqrt(5.0_real64), 2.0_real64*sqrt(3.0_real64), &
        sqrt(3.0_real64)]

    !> Where each kernel's density has a kink or a jump inside its support, in standard deviations:
    !> the B-spline's pieces meet at `+-sqrt(3)`; the others are smooth inside.
    real(real64), parameter :: BSPLINE_KNOT = sqrt(3.0_real64)

    !> The estimate as a function, `x**power * f(x)`, for `pf_integrate`: the extension the guide
    !> shows. A module-level type, because a callback in this library is never internal.
    type, extends(pf_integrand) :: kde_density
        type(pf_kde) :: k              !! the fitted estimate
        integer      :: power = 0      !! 0 for the density, 2 for its second moment
    contains
        procedure :: eval => kde_density_eval !! `x**power` times the density at `x`
    end type kde_density

    !> A grid's interpolated density as a function, `x**power * f(x)`, for `pf_integrate`.
    type, extends(pf_integrand) :: grid_density
        type(pf_kde_grid) :: g         !! the grid
        integer           :: power = 0 !! 0 for the density, 1 and 2 for its moments
    contains
        procedure :: eval => grid_density_eval !! `x**power` times the grid's `%pdf` at `x`
    end type grid_density

    !> The label `pf_kde%sample` derives its key with, copied from `src/parquet_kde.f90`'s
    !> `KDE_FAMILY_LABEL`: the recipe test reproduces every draw from it, so a changed label -- which
    !> changes every sample a program has ever drawn -- fails that test rather than passing unseen.
    integer(int64), parameter :: KDE_LABEL = 7089359947230782746_int64

    !> `pf_kde_grid%sample`'s label, copied from `KDE_GRID_FAMILY_LABEL` for the same reason.
    integer(int64), parameter :: KDE_GRID_LABEL = 8280789554566260118_int64

contains

    !> Registers the concurrent suite.
    subroutine collect_tests_kde(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:) !! Receives the suite's tests.

        testsuite = [ &
            new_unittest("the fixture recipe matches the generator's", test_fixture_matches_generator), &
            new_unittest("every kernel has unit mass, unit variance and a matching CDF", &
                test_kernel_identities), &
            new_unittest("the fixed estimate reproduces the golden vectors", test_golden_vectors), &
            new_unittest("the default rule is silverman", test_default_rule_is_silverman), &
            new_unittest("silverman and scott reproduce their formulas", test_rules_match_formulas), &
            new_unittest("adjust= multiplies whichever bandwidth was chosen", test_adjust_multiplies), &
            new_unittest("n_eff follows the weight type", test_n_eff_follows_weight_type), &
            new_unittest("frequency weights are replication", test_frequency_weights_replicate), &
            new_unittest("the population rules are the family's", test_population_rules), &
            new_unittest("a NaN under skipnan=.false. poisons the estimate", test_skipnan_false_poisons), &
            new_unittest("an empty and a degenerate population are quiet", test_quiet_returns), &
            new_unittest("lower/upper conserve mass under both corrections", test_bounds_conserve_mass), &
            new_unittest("a point outside the support is excluded and counted", test_outside_excluded), &
            new_unittest("%quantile inverts %cdf", test_quantile_inverts_cdf), &
            new_unittest("%curve spans min - cut*h to max + cut*h, clipped to the support", &
                test_curve_default_range), &
            new_unittest("single precision input widens", test_real32_widens), &
            new_unittest("the array forms equal the scalar forms", test_array_forms_match_scalar), &
            new_unittest("the tokens and the accessors report the fit", test_tokens_and_accessors), &
            new_unittest("a refit replaces everything and %clear unfits", test_refit_and_clear), &
            new_unittest("%print writes the summary and says when there is none", test_print_writes), &
            new_unittest("the guide's rising-density table is what the estimator answers", &
                test_guide_boundary_table), &
            new_unittest("the grid converges to the exact estimate as step**2", test_grid_converges), &
            new_unittest("the grid deposits exactly w/step per point", test_grid_deposits_exact_mass), &
            new_unittest("the grid counts the weight beyond its range at each end", &
                test_grid_counts_weight_beyond_range), &
            new_unittest("a narrow kernel lands whole in one or two cells", test_grid_narrow_kernel), &
            new_unittest("%pdf on a grid interpolates and is exact at the centres", &
                test_grid_pdf_interpolates), &
            new_unittest("the grid's %cdf integrates its %pdf and %quantile inverts it", &
                test_grid_cdf_and_quantile), &
            new_unittest("%merge equals one grid over the concatenation", test_grid_merge), &
            new_unittest("the grid applies the population rules on every %add", &
                test_grid_population_rules), &
            new_unittest("an empty grid answers zeros and a poisoned one NaN", &
                test_grid_empty_and_poisoned), &
            new_unittest("the grid's accessors report its set-up and %clear keeps it", &
                test_grid_accessors_and_clear), &
            new_unittest("the grid's %print writes the summary and says when there is none", &
                test_grid_print_writes), &
            new_unittest("alpha=0 is the fixed estimate, alpha=0.5 is not", test_alpha_zero_is_fixed), &
            new_unittest("the adaptive bandwidths follow the pilot", test_adaptive_bandwidths_follow_pilot), &
            new_unittest("the adaptive golden case", test_adaptive_golden), &
            new_unittest("bandwidth_max caps every h_j", test_bandwidth_max_caps), &
            new_unittest("%pilot returns the grid the fit used", test_pilot_is_the_fits), &
            new_unittest("the pilot's cells follow the rule and its clamps", test_pilot_cells_rule), &
            new_unittest("the two forms agree under one pilot", test_two_forms_agree_under_one_pilot), &
            new_unittest("the adaptive kernel conserves mass under both corrections", &
                test_adaptive_conserves_mass), &
            new_unittest("%bandwidth_at is the rule %fit applied", test_bandwidth_at), &
            new_unittest("the adaptive grid copies its pilot, merges and is poisoned by a poisoned one", &
                test_adaptive_grid), &
            new_unittest("a grid on a pilot with nothing in its cells answers NaN", &
                test_empty_pilot_is_quiet), &
            new_unittest("the adaptive fit's accessors, printer, real32 form and refit", &
                test_adaptive_accessors), &
            new_unittest("a bandwidth too large to use leaves the estimate undefined", &
                test_unusable_bandwidth), &
            new_unittest("%sample is addressed by (seed, stream, k)", test_sample_addressing), &
            new_unittest("%sample respects the support and the kernel", test_sample_support_and_kernel), &
            new_unittest("%sample draws each point's own weight and bandwidth", test_sample_weights_and_bandwidths), &
            new_unittest("the grid's %sample follows its %pdf inside its range", test_grid_sample) &
            ]

    end subroutine collect_tests_kde

    !> Registers the suite that writes process-global state and so runs serially.
    subroutine collect_tests_kde_serial(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:) !! Receives the suite's tests.

        testsuite = [ &
            new_unittest("verbosity silences %print", test_print_silenced), &
            new_unittest("verbosity silences the grid's %print", test_grid_print_silenced), &
            new_unittest("the adaptive golden case with a fine pilot", test_adaptive_golden_fine), &
            new_unittest("parquet_debug_set_kde_pilot_cells forces the pilot's cells", &
                test_pilot_cells_forced) &
            ]

    end subroutine collect_tests_kde_serial

    ! ==========================================================================================
    ! Fixtures and helpers
    ! ==========================================================================================

    !> The recipe every golden case is taken over: `test_stats.f90`'s `golden_fixture`, which
    !> `tools/generate_kde_vectors.py` imports from `tools/generate_stats_vectors.py`. Every value
    !> is an integer over a power of two, so both languages hold it exactly.
    subroutine kde_fixture(n, x)
        integer(int64), intent(in)             :: n    !! how many values
        real(real64), allocatable, intent(out) :: x(:) !! the values
        integer(int64) :: i, a

        allocate(x(n))
        do i = 1_int64, n
            a = mod(i*i*7919_int64 + 12345_int64, 1000003_int64)
            x(i) = real(a - 500001_int64, real64)/1024.0_real64
        end do

    end subroutine kde_fixture

    !> The adaptive cases' recipe, `two_component` in `tools/generate_kde_vectors.py`: two points in
    !> three in a narrow cluster about -300 (within 32 of it) and the third in a cluster five times
    !> as wide about +300, `v` a spread integer from the same recipe. Every value is an integer over
    !> 16, exact in both languages.
    subroutine kde_two_component(n, x)
        integer(int64), intent(in)             :: n    !! how many values
        real(real64), allocatable, intent(out) :: x(:) !! the values
        integer(int64) :: i, a, v

        allocate(x(n))
        do i = 1_int64, n
            a = mod(i*i*7919_int64 + 12345_int64, 1000003_int64)
            v = mod(a, 1024_int64) - 512_int64
            if (mod(i, 3_int64) == 0_int64) then
                x(i) = 300.0_real64 + 5.0_real64*real(v, real64)/16.0_real64
            else
                x(i) = -300.0_real64 + real(v, real64)/16.0_real64
            end if
        end do

    end subroutine kde_two_component

    !> `w(i) = mod(i, 5)`: every fifth weight is zero and removes its element.
    subroutine kde_weights_mod5(n, w)
        integer(int64), intent(in)             :: n    !! how many weights
        real(real64), allocatable, intent(out) :: w(:) !! the weights
        integer(int64) :: i

        allocate(w(n))
        do i = 1_int64, n
            w(i) = real(mod(i, 5_int64), real64)
        end do

    end subroutine kde_weights_mod5

    !> `x**power` times the density at `x`.
    function kde_density_eval(this, x) result(f)
        class(kde_density), intent(inout) :: this !! the estimate as a function
        real(real64), intent(in)          :: x    !! where to evaluate
        real(real64)                      :: f    !! the value

        call this%k%pdf(x, f)
        if (this%power /= 0) f = f*x**this%power

    end function kde_density_eval

    !> `x**power` times the grid's interpolated density at `x`.
    function grid_density_eval(this, x) result(f)
        class(grid_density), intent(inout) :: this !! the grid as a function
        real(real64), intent(in)            :: x    !! where to evaluate
        real(real64)                        :: f    !! the value

        call this%g%pdf(x, f)
        if (this%power /= 0) f = f*x**this%power

    end function grid_density_eval

    !> The mean, variance and kurtosis of `v`, over its own mean.
    pure subroutine moments(v, mean, var, kurt)
        real(real64), intent(in)  :: v(:) !! the values
        real(real64), intent(out) :: mean !! their mean
        real(real64), intent(out) :: var  !! their variance, over `size(v)`
        real(real64), intent(out) :: kurt !! their fourth central moment over the variance squared
        integer(int64) :: i, n
        real(real64) :: d, m2, m4

        n = size(v, kind=int64)
        mean = sum(v)/real(n, real64)
        m2 = 0.0_real64
        m4 = 0.0_real64
        do i = 1_int64, n
            d = v(i) - mean
            m2 = m2 + d*d
            m4 = m4 + (d*d)*(d*d)
        end do
        var = m2/real(n, real64)
        kurt = (m4/real(n, real64))/(var*var)

    end subroutine moments

    !> How many pairs of elements of `v` are equal.
    pure function equal_pairs(v) result(c)
        real(real64), intent(in) :: v(:) !! the values
        integer(int64)           :: c    !! the pairs
        integer(int64) :: i, j

        c = 0_int64
        do i = 1_int64, size(v, kind=int64)
            do j = i + 1_int64, size(v, kind=int64)
                if (v(i) == v(j)) c = c + 1_int64
            end do
        end do

    end function equal_pairs

    !> `.true.` when `a` and `b` agree to `rel` of `scale`, or are both NaN.
    pure function close_to(a, b, rel, scale) result(res)
        real(real64), intent(in) :: a     !! the value
        real(real64), intent(in) :: b     !! the expectation
        real(real64), intent(in) :: rel   !! the tolerance, relative to `scale`
        real(real64), intent(in) :: scale !! the magnitude the tolerance is relative to
        logical                  :: res   !! they agree

        if (ieee_is_nan(a) .or. ieee_is_nan(b)) then
            res = ieee_is_nan(a) .and. ieee_is_nan(b)
            return
        end if
        res = abs(a - b) <= rel*scale

    end function close_to

    !> Makes the `pf_kde%fit` call a golden case names; the arguments mirror the generator's CASES.
    subroutine fit_golden_case(name, k, ok)
        character(len=*), intent(in) :: name !! the case
        type(pf_kde), intent(inout)  :: k    !! fitted here
        logical, intent(out)         :: ok   !! `%fit`'s `ok`
        real(real64), allocatable :: x(:), w(:)
        integer(int64) :: n

        n = 32_int64
        if (name == "N1000") n = 1000_int64
        if (name == "ONE_H" .or. name == "ONE_RULE") n = 1_int64
        if (name(1:min(5, len(name))) == "ADAPT") n = 60_int64
        if (n == 60_int64) then
            call kde_two_component(n, x)
        else
            call kde_fixture(n, x)
        end if
        call kde_weights_mod5(n, w)
        select case (name)
        case ("DEFAULT")
            call k%fit(x, rule="silverman", ok=ok)
        case ("SCOTT")
            call k%fit(x, rule="scott", ok=ok)
        case ("ADJUST")
            call k%fit(x, rule="silverman", adjust=1.5_real64, ok=ok)
        case ("N1000")
            call k%fit(x, rule="silverman", ok=ok)
        case ("GAUSS")
            call k%fit(x, bandwidth=60.0_real64, ok=ok)
        case ("EPAN")
            call k%fit(x, bandwidth=60.0_real64, kernel="epanechnikov", ok=ok)
        case ("BSPL")
            call k%fit(x, bandwidth=60.0_real64, kernel="bspline", ok=ok)
        case ("BOX")
            call k%fit(x, bandwidth=60.0_real64, kernel="box", ok=ok)
        case ("WREL")
            call k%fit(x, rule="silverman", weights=w, ok=ok)
        case ("WFREQ")
            call k%fit(x, rule="silverman", weights=w, weight_type="frequency", ok=ok)
        case ("REN_LO")
            call k%fit(x, bandwidth=60.0_real64, lower=-470.0_real64, ok=ok)
        case ("REN_BOTH")
            call k%fit(x, bandwidth=60.0_real64, kernel="epanechnikov", lower=-470.0_real64, &
                upper=460.0_real64, ok=ok)
        case ("REF_LO")
            call k%fit(x, bandwidth=60.0_real64, kernel="bspline", lower=-470.0_real64, &
                boundary="reflect", ok=ok)
        case ("REF_HI")
            call k%fit(x, bandwidth=60.0_real64, kernel="box", upper=460.0_real64, &
                boundary="reflect", ok=ok)
        case ("REF_WIDE")
            call k%fit(x, bandwidth=400.0_real64, lower=-470.0_real64, upper=460.0_real64, &
                boundary="reflect", ok=ok)
        case ("REN_WIDE")
            call k%fit(x, bandwidth=600.0_real64, kernel="box", lower=-470.0_real64, &
                upper=460.0_real64, ok=ok)
        case ("RULE_BOUNDED")
            call k%fit(x, rule="silverman", lower=-400.0_real64, upper=400.0_real64, ok=ok)
        case ("ONE_H")
            call k%fit(x, bandwidth=10.0_real64, ok=ok)
        case ("ONE_RULE")
            call k%fit(x, rule="silverman", ok=ok)
        case ("ADAPT")
            call k%fit(x, rule="silverman", adaptive=.true., ok=ok)
        case ("ADAPT_CAP")
            call k%fit(x, bandwidth=80.0_real64, kernel="bspline", adaptive=.true., alpha=1.0_real64, &
                bandwidth_max=120.0_real64, lower=-340.0_real64, ok=ok)
        case ("ADAPT_REF")
            call k%fit(x, bandwidth=60.0_real64, kernel="epanechnikov", adaptive=.true., lower=-340.0_real64, &
                upper=470.0_real64, boundary="reflect", ok=ok)
        case ("ADAPT_W")
            call k%fit(x, rule="silverman", adaptive=.true., weights=w, ok=ok)
        case default
            error stop "fit_golden_case: unknown case " // name
        end select

    end subroutine fit_golden_case

    !> Compares one golden case: `ok`, the bandwidth, and the density and CDF at every probe.
    subroutine check_golden_case(error, name, def, h, pdf, cdf)
        type(error_type), allocatable, intent(out) :: error    !! set on the first failed check
        character(len=*), intent(in)               :: name     !! the case
        logical, intent(in)                        :: def      !! the case is defined
        real(real64), intent(in)                   :: h        !! the bandwidth
        real(real64), intent(in)                   :: pdf(NKX) !! the density at `KG_X`
        real(real64), intent(in)                   :: cdf(NKX) !! the CDF at `KG_X`
        type(pf_kde) :: k
        logical :: ok
        real(real64) :: got_pdf(NKX), got_cdf(NKX), peak
        integer :: i
        character(len=200) :: msg

        call fit_golden_case(name, k, ok)
        call check(error, ok .eqv. def, name // ": ok must be " // merge("T", "F", def))
        if (allocated(error)) return
        call k%pdf(KG_X, got_pdf)
        call k%cdf(KG_X, got_cdf)
        if (.not. def) then
            call check(error, ieee_is_nan(k%bandwidth()) .and. all(ieee_is_nan(got_pdf)) .and. &
                all(ieee_is_nan(got_cdf)), name // ": an undefined estimate must answer NaN everywhere")
            return
        end if
        call check(error, close_to(k%bandwidth(), h, 1.0e-13_real64, h), &
            name // ": the bandwidth must match the oracle")
        if (allocated(error)) return
        peak = maxval(pdf)
        do i = 1, NKX
            write(msg, '(a,a,es24.16,a,es24.16,a,es24.16)') name, ": pdf at ", KG_X(i), " is ", &
                got_pdf(i), ", oracle ", pdf(i)
            call check(error, close_to(got_pdf(i), pdf(i), 1.0e-12_real64, peak), trim(msg))
            if (allocated(error)) return
            write(msg, '(a,a,es24.16,a,es24.16,a,es24.16)') name, ": cdf at ", KG_X(i), " is ", &
                got_cdf(i), ", oracle ", cdf(i)
            call check(error, close_to(got_cdf(i), cdf(i), 1.0e-13_real64, 1.0_real64), trim(msg))
            if (allocated(error)) return
        end do

    end subroutine check_golden_case

    !> Reads a unit's lines back, counting them and noting whether one contains `needle`.
    subroutine read_back(path, nlines, needle, seen)
        character(len=*), intent(in) :: path   !! the file
        integer, intent(out)         :: nlines !! how many lines it holds
        character(len=*), intent(in) :: needle !! text to look for
        logical, intent(out)         :: seen   !! some line contains it
        integer :: u, ios
        character(len=256) :: line

        nlines = 0
        seen = .false.
        open(newunit=u, file=path, status="old", action="read")
        do
            read(u, '(a)', iostat=ios) line
            if (ios /= 0) exit
            nlines = nlines + 1
            if (index(line, needle) > 0) seen = .true.
        end do
        close(u, status="delete")

    end subroutine read_back

    ! ==========================================================================================
    ! The tests
    ! ==========================================================================================

    !> The Fortran recipe produces the generator's first values exactly.
    subroutine test_fixture_matches_generator(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        real(real64), allocatable :: x(:)

        call kde_fixture(int(size(KG_PROBE), int64), x)
        call check(error, all(x == KG_PROBE), &
            "kde_fixture has drifted from generate_stats_vectors.py's fixture(): every golden " // &
            "case below would fail for that reason")
        if (allocated(error)) return
        call kde_two_component(int(size(KG_PROBE2), int64), x)
        call check(error, all(x == KG_PROBE2), &
            "kde_two_component has drifted from generate_kde_vectors.py's two_component(): every " // &
            "adaptive golden case would fail for that reason")

    end subroutine test_fixture_matches_generator

    !> One point at zero with bandwidth one: the estimate IS the kernel in standard-deviation
    !> units. Its integral is one, its second moment is one (the cut Gaussian's is
    !> `1 - 10 phi(5)/erf(5/sqrt 2)`, the variance the cut removes), and `%cdf` differences equal
    !> integrals of `%pdf` -- every one of them computed by `pf_integrate`, which shares nothing with
    !> the kernels. A wrong scale factor moves the variance; a wrong CDF polynomial or support
    !> radius moves the differences.
    subroutine test_kernel_identities(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(kde_density) :: fn
        real(real64) :: r, mass, var, want_var, c1, c2, integral
        real(real64), parameter :: PHI5 = 1.4867195147342977e-6_real64
        real(real64), parameter :: EDGES(5) = [-2.9_real64, -1.2_real64, 0.3_real64, 1.9_real64, &
            3.1_real64]
        integer :: kk, i
        real(real64), allocatable :: cuts(:)

        do kk = 1, 4
            r = RADIUS(kk)
            call fn%k%fit([0.0_real64], bandwidth=1.0_real64, kernel=trim(KERNELS(kk)))
            if (trim(KERNELS(kk)) == "bspline") then
                cuts = [-BSPLINE_KNOT, 0.0_real64, BSPLINE_KNOT]
            else
                cuts = [0.0_real64]
            end if
            fn%power = 0
            mass = pf_integrate(fn, -r, r, 1.0e-13_real64, breakpoints=cuts)
            call check(error, abs(mass - 1.0_real64) <= 1.0e-12_real64, &
                trim(KERNELS(kk)) // ": the kernel must integrate to one")
            if (allocated(error)) return
            fn%power = 2
            var = pf_integrate(fn, -r, r, 1.0e-13_real64, breakpoints=cuts)
            want_var = 1.0_real64
            if (kk == 1) want_var = 1.0_real64 - 10.0_real64*PHI5/(1.0_real64 - 5.7330314375838782e-7_real64)
            call check(error, abs(var - want_var) <= 1.0e-12_real64, &
                trim(KERNELS(kk)) // ": the kernel's variance must be one (the bandwidth is its sd)")
            if (allocated(error)) return
            fn%power = 0
            do i = 1, size(EDGES) - 1
                if (.not. (max(-r, EDGES(i)) < min(r, EDGES(i + 1)))) cycle
                call fn%k%cdf(max(-r, EDGES(i)), c1)
                call fn%k%cdf(min(r, EDGES(i + 1)), c2)
                integral = pf_integrate(fn, max(-r, EDGES(i)), min(r, EDGES(i + 1)), 1.0e-13_real64)
                if (trim(KERNELS(kk)) == "bspline") then
                    ! The B-spline's knots at +-sqrt(3) fall inside two of these intervals.
                    integral = integrate_split(fn, max(-r, EDGES(i)), min(r, EDGES(i + 1)))
                end if
                call check(error, abs((c2 - c1) - integral) <= 1.0e-12_real64, &
                    trim(KERNELS(kk)) // ": %cdf differences must equal the integral of %pdf")
                if (allocated(error)) return
            end do
            call fn%k%cdf(-r, c1)
            call fn%k%cdf(r, c2)
            call check(error, c1 == 0.0_real64 .and. c2 == 1.0_real64, &
                trim(KERNELS(kk)) // ": the CDF must be exactly 0 and 1 at the support radius")
            if (allocated(error)) return
        end do

    contains

        !> The integral over `[a, b]` with the B-spline's two knots cut out where they fall inside.
        function integrate_split(g, a, b) result(res)
            type(kde_density), intent(inout) :: g   !! the estimate as a function
            real(real64), intent(in)         :: a   !! lower bound
            real(real64), intent(in)         :: b   !! upper bound
            real(real64)                     :: res !! the integral
            real(real64), allocatable :: inner(:)

            inner = pack([-BSPLINE_KNOT, BSPLINE_KNOT], [-BSPLINE_KNOT, BSPLINE_KNOT] > a .and. &
                [-BSPLINE_KNOT, BSPLINE_KNOT] < b)
            if (size(inner) == 0) then
                res = pf_integrate(g, a, b, 1.0e-13_real64)
            else
                res = pf_integrate(g, a, b, 1.0e-13_real64, breakpoints=inner)
            end if

        end function integrate_split

    end subroutine test_kernel_identities

    !> Every golden case: the bandwidth, and the density and CDF at every probe, against the
    !> 50-digit oracle.
    subroutine test_golden_vectors(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check

        call check_golden_case(error, "DEFAULT", KG_DEFAULT_DEF, KG_DEFAULT_H, KG_DEFAULT_PDF, KG_DEFAULT_CDF)
        if (allocated(error)) return
        call check_golden_case(error, "SCOTT", KG_SCOTT_DEF, KG_SCOTT_H, KG_SCOTT_PDF, KG_SCOTT_CDF)
        if (allocated(error)) return
        call check_golden_case(error, "ADJUST", KG_ADJUST_DEF, KG_ADJUST_H, KG_ADJUST_PDF, KG_ADJUST_CDF)
        if (allocated(error)) return
        call check_golden_case(error, "N1000", KG_N1000_DEF, KG_N1000_H, KG_N1000_PDF, KG_N1000_CDF)
        if (allocated(error)) return
        call check_golden_case(error, "GAUSS", KG_GAUSS_DEF, KG_GAUSS_H, KG_GAUSS_PDF, KG_GAUSS_CDF)
        if (allocated(error)) return
        call check_golden_case(error, "EPAN", KG_EPAN_DEF, KG_EPAN_H, KG_EPAN_PDF, KG_EPAN_CDF)
        if (allocated(error)) return
        call check_golden_case(error, "BSPL", KG_BSPL_DEF, KG_BSPL_H, KG_BSPL_PDF, KG_BSPL_CDF)
        if (allocated(error)) return
        call check_golden_case(error, "BOX", KG_BOX_DEF, KG_BOX_H, KG_BOX_PDF, KG_BOX_CDF)
        if (allocated(error)) return
        call check_golden_case(error, "WREL", KG_WREL_DEF, KG_WREL_H, KG_WREL_PDF, KG_WREL_CDF)
        if (allocated(error)) return
        call check_golden_case(error, "WFREQ", KG_WFREQ_DEF, KG_WFREQ_H, KG_WFREQ_PDF, KG_WFREQ_CDF)
        if (allocated(error)) return
        call check_golden_case(error, "REN_LO", KG_REN_LO_DEF, KG_REN_LO_H, KG_REN_LO_PDF, KG_REN_LO_CDF)
        if (allocated(error)) return
        call check_golden_case(error, "REN_BOTH", KG_REN_BOTH_DEF, KG_REN_BOTH_H, KG_REN_BOTH_PDF, &
            KG_REN_BOTH_CDF)
        if (allocated(error)) return
        call check_golden_case(error, "REF_LO", KG_REF_LO_DEF, KG_REF_LO_H, KG_REF_LO_PDF, KG_REF_LO_CDF)
        if (allocated(error)) return
        call check_golden_case(error, "REF_HI", KG_REF_HI_DEF, KG_REF_HI_H, KG_REF_HI_PDF, KG_REF_HI_CDF)
        if (allocated(error)) return
        call check_golden_case(error, "REF_WIDE", KG_REF_WIDE_DEF, KG_REF_WIDE_H, KG_REF_WIDE_PDF, &
            KG_REF_WIDE_CDF)
        if (allocated(error)) return
        call check_golden_case(error, "REN_WIDE", KG_REN_WIDE_DEF, KG_REN_WIDE_H, KG_REN_WIDE_PDF, &
            KG_REN_WIDE_CDF)
        if (allocated(error)) return
        call check_golden_case(error, "RULE_BOUNDED", KG_RULE_BOUNDED_DEF, KG_RULE_BOUNDED_H, &
            KG_RULE_BOUNDED_PDF, KG_RULE_BOUNDED_CDF)
        if (allocated(error)) return
        call check_golden_case(error, "ONE_H", KG_ONE_H_DEF, KG_ONE_H_H, KG_ONE_H_PDF, KG_ONE_H_CDF)
        if (allocated(error)) return
        call check_golden_case(error, "ONE_RULE", KG_ONE_RULE_DEF, KG_ONE_RULE_H, KG_ONE_RULE_PDF, &
            KG_ONE_RULE_CDF)

    end subroutine test_golden_vectors

    !> With no `rule=` and no `bandwidth=`, the rule is Silverman's. The one test that relies on
    !> the default; P5 replaces it with the assertion that the default is ISJ.
    subroutine test_default_rule_is_silverman(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde) :: a, b
        real(real64), allocatable :: x(:)
        character(len=:), allocatable :: name

        call kde_fixture(32_int64, x)
        call a%fit(x)
        call b%fit(x, rule="silverman")
        call a%rule(name)
        call check(error, name == "silverman" .and. a%bandwidth() == b%bandwidth(), &
            "the default rule must be silverman")

    end subroutine test_default_rule_is_silverman

    !> Both rules, re-formed here from `pf_stddev` and `pf_iqr`'s own answers over the same
    !> population, on a weighted fixture whose reliability `n_eff` is well below `n` -- so a rule
    !> that used `n`, or `s` where the robust scale binds, is caught.
    subroutine test_rules_match_formulas(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde) :: k
        real(real64), allocatable :: x(:), w(:)
        real(real64) :: s, r, a, n_eff, want
        integer(int64) :: i

        ! A long tail on a tight core: the interquartile range, not the standard deviation, binds.
        call kde_fixture(64_int64, x)
        x = x/100.0_real64
        x(1:4) = [40.0_real64, -55.0_real64, 70.0_real64, 90.0_real64]
        allocate(w(64))
        do i = 1_int64, 64_int64
            w(i) = merge(8.0_real64, 1.0_real64, mod(i, 8_int64) == 0_int64)
        end do
        call pf_stddev(x, s, weights=w)
        call pf_iqr(x, r, weights=w)
        call check(error, r/1.349_real64 < s, "the fixture must make the robust scale bind")
        if (allocated(error)) return
        a = r/1.349_real64
        n_eff = sum(w)**2/sum(w*w)
        call check(error, n_eff < 0.5_real64*64.0_real64, "the fixture's n_eff must be well below n")
        if (allocated(error)) return

        call k%fit(x, rule="silverman", weights=w)
        want = 0.9_real64*a*n_eff**(-0.2_real64)
        call check(error, abs(k%bandwidth() - want) <= 1.0e-14_real64*want, &
            "silverman must be 0.9 * min(s, IQR/1.349) * n_eff**(-1/5)")
        if (allocated(error)) return
        call k%fit(x, rule="scott", weights=w)
        want = 1.06_real64*a*n_eff**(-0.2_real64)
        call check(error, abs(k%bandwidth() - want) <= 1.0e-14_real64*want, &
            "scott must be 1.06 * min(s, IQR/1.349) * n_eff**(-1/5)")
        if (allocated(error)) return

        ! Unweighted and near-uniform, the standard deviation binds instead.
        call kde_fixture(64_int64, x)
        call pf_stddev(x, s)
        call pf_iqr(x, r)
        call check(error, s < r/1.349_real64, "a near-uniform sample must make s bind")
        if (allocated(error)) return
        call k%fit(x, rule="silverman")
        want = 0.9_real64*s*64.0_real64**(-0.2_real64)
        call check(error, abs(k%bandwidth() - want) <= 1.0e-14_real64*want, &
            "silverman must use s where it is below IQR/1.349")
        if (allocated(error)) return

        ! More than half the sample tied: the interquartile range is zero, and the rule falls back
        ! to `s` rather than to a bandwidth of zero.
        x(1:40) = 1.0_real64
        call pf_stddev(x, s)
        call pf_iqr(x, r)
        call check(error, r == 0.0_real64 .and. s > 0.0_real64, "the fixture must have a zero IQR")
        if (allocated(error)) return
        call k%fit(x, rule="silverman")
        want = 0.9_real64*s*64.0_real64**(-0.2_real64)
        call check(error, abs(k%bandwidth() - want) <= 1.0e-14_real64*want, &
            "a zero IQR must fall back to s, not to a zero bandwidth")

    end subroutine test_rules_match_formulas

    !> `adjust=` scales a rule's bandwidth and an explicit one alike.
    subroutine test_adjust_multiplies(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde) :: k
        real(real64), allocatable :: x(:)
        real(real64) :: h1

        call kde_fixture(32_int64, x)
        call k%fit(x, rule="scott")
        h1 = k%bandwidth()
        call k%fit(x, rule="scott", adjust=0.25_real64)
        call check(error, k%bandwidth() == 0.25_real64*h1, "adjust must multiply a rule's bandwidth")
        if (allocated(error)) return
        call k%fit(x, bandwidth=3.0_real64, adjust=2.5_real64)
        call check(error, k%bandwidth() == 7.5_real64, "adjust must multiply an explicit bandwidth")

    end subroutine test_adjust_multiplies

    !> Unequal weights: reliability counts Kish's `sum(w)**2/sum(w**2)`, frequency counts `sum(w)`.
    !> On a near-uniform fixture `s` binds under both, so the two bandwidths differ by the counts'
    !> ratio and by the two standard deviations' own `ddof` charges, each re-formed here.
    subroutine test_n_eff_follows_weight_type(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde) :: rel, frq
        real(real64), allocatable :: x(:), w(:)
        real(real64) :: s_rel, s_frq, want_rel, want_frq

        call kde_fixture(40_int64, x)
        call kde_weights_mod5(40_int64, w)
        w = 3.0_real64*w
        call rel%fit(x, rule="silverman", weights=w)
        call frq%fit(x, rule="silverman", weights=w, weight_type="frequency")
        call pf_stddev(x, s_rel, weights=w)
        call pf_stddev(x, s_frq, weights=w, weight_type="frequency")
        want_rel = 0.9_real64*s_rel*(sum(w)**2/sum(w*w))**(-0.2_real64)
        want_frq = 0.9_real64*s_frq*sum(w)**(-0.2_real64)
        call check(error, abs(rel%bandwidth() - want_rel) <= 1.0e-14_real64*want_rel, &
            "reliability weights must use Kish's effective size")
        if (allocated(error)) return
        call check(error, abs(frq%bandwidth() - want_frq) <= 1.0e-14_real64*want_frq, &
            "frequency weights must use sum(w)")
        if (allocated(error)) return
        call check(error, abs(rel%bandwidth() - frq%bandwidth()) > 1.0e-3_real64*want_rel, &
            "the two weight types must give different bandwidths on unequal weights")

    end subroutine test_n_eff_follows_weight_type

    !> A frequency weight of three is three observations. At an explicit bandwidth the estimate
    !> over the weighted sample equals the estimate over the replicated one to rounding; under a
    !> rule the bandwidths agree too while the standard deviation binds, which it does here.
    !>
    !> Where the interquartile range binds instead they need not agree: `pf_iqr`'s frequency
    !> default is the inverted-CDF quartile, the family's own rule for frequency weights, while the
    !> replicated sample takes the interpolated one.
    subroutine test_frequency_weights_replicate(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde) :: wk, rk
        real(real64), allocatable :: x(:), w(:), rep(:)
        real(real64) :: fw(NKX), fr(NKX), cw(NKX), cr(NKX)
        integer(int64) :: i, j, m

        call kde_fixture(30_int64, x)
        allocate(w(30))
        do i = 1_int64, 30_int64
            w(i) = real(mod(i, 3_int64) + 1_int64, real64)
        end do
        allocate(rep(int(sum(w), int64)))
        m = 0_int64
        do i = 1_int64, 30_int64
            do j = 1_int64, int(w(i), int64)
                m = m + 1_int64
                rep(m) = x(i)
            end do
        end do
        call wk%fit(x, bandwidth=45.0_real64, weights=w, weight_type="frequency")
        call rk%fit(rep, bandwidth=45.0_real64)
        call wk%pdf(KG_X, fw)
        call rk%pdf(KG_X, fr)
        call wk%cdf(KG_X, cw)
        call rk%cdf(KG_X, cr)
        call check(error, all(abs(fw - fr) <= 1.0e-13_real64*maxval(fr)) .and. &
            all(abs(cw - cr) <= 1.0e-14_real64), &
            "a frequency-weighted fit must equal the fit over the replicated sample")
        if (allocated(error)) return
        call wk%fit(x, rule="silverman", weights=w, weight_type="frequency")
        call rk%fit(rep, rule="silverman")
        call check(error, abs(wk%bandwidth() - rk%bandwidth()) <= 1.0e-13_real64*rk%bandwidth(), &
            "frequency weights must give the replicated sample's bandwidth where s binds")

    end subroutine test_frequency_weights_replicate

    !> The family's exclusion ORDER: a null element with a NaN weight is null (its weight is never
    !> examined, so no abort), a NaN element with a zero weight is a NaN, and a zero weight on an
    !> ordinary element removes it without a count. The counts must equal `pf_count_valid`'s.
    subroutine test_population_rules(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde) :: k
        real(real64) :: x(7), w(7), nan
        logical :: valid(7), ok
        integer(int64) :: n_null, n_nan, n_out, want_n, want_null, want_nan

        nan = ieee_value(1.0_real64, ieee_quiet_nan)
        x = [1.0_real64, 2.0_real64, nan, 4.0_real64, 5.0_real64, 6.0_real64, 7.5_real64]
        w = [1.0_real64, nan, 0.0_real64, 2.0_real64, 0.0_real64, 1.0_real64, 3.0_real64]
        valid = [.true., .false., .true., .true., .true., .true., .true.]
        call k%fit(x, bandwidth=1.0_real64, is_valid=valid, weights=w, n_null=n_null, n_nan=n_nan, &
            n_outside=n_out, ok=ok)
        call pf_count_valid(x, want_n, is_valid=valid, weights=w, n_null=want_null, n_nan=want_nan)
        call check(error, ok .and. n_null == want_null .and. n_nan == want_nan .and. n_out == 0_int64 &
            .and. k%n_valid() == want_n .and. k%n() == 7_int64, &
            "the exclusion counts must be pf_count_valid's")
        if (allocated(error)) return
        call check(error, want_n == 4_int64 .and. k%sum_weights() == 7.0_real64, &
            "four elements must survive, carrying weight 1 + 2 + 1 + 3")

    end subroutine test_population_rules

    !> `skipnan=.false.` keeps a NaN, and a NaN in the population makes every answer NaN.
    subroutine test_skipnan_false_poisons(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde) :: k
        real(real64) :: x(4), f, c, q
        logical :: ok

        x = [1.0_real64, 2.0_real64, ieee_value(1.0_real64, ieee_quiet_nan), 4.0_real64]
        call k%fit(x, bandwidth=1.0_real64, ok=ok)
        call check(error, ok .and. k%n_nan() == 1_int64, "by default a NaN leaves the population")
        if (allocated(error)) return
        call k%fit(x, bandwidth=1.0_real64, skipnan=.false., ok=ok)
        call k%pdf(2.0_real64, f)
        call k%cdf(2.0_real64, c)
        call k%quantile(0.5_real64, q)
        call check(error, (.not. ok) .and. ieee_is_nan(f) .and. ieee_is_nan(c) .and. ieee_is_nan(q), &
            "skipnan=.false. with a NaN present must give ok=.false. and NaN answers")

    end subroutine test_skipnan_false_poisons

    !> Section 5.5's quiet returns: no abort from a data condition, a NaN from every query.
    subroutine test_quiet_returns(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde) :: k
        real(real64) :: f, c, empty(0), xg(5), fg(5), vs(3)
        logical :: ok

        call k%fit(empty, ok=ok)
        call k%pdf(0.0_real64, f)
        call k%curve(xg, fg)
        call k%sample(vs, 1_int64)
        call check(error, (.not. ok) .and. k%is_fitted() .and. ieee_is_nan(f) .and. &
            all(ieee_is_nan(fg)) .and. all(ieee_is_nan(xg)) .and. all(ieee_is_nan(vs)) .and. &
            k%n_valid() == 0_int64, "an empty sample must leave a fitted, undefined estimate")
        if (allocated(error)) return
        call k%fit([1.0_real64, 2.0_real64], is_valid=[.false., .false.], ok=ok)
        call check(error, .not. ok .and. k%n_null() == 2_int64, "an all-null sample is undefined")
        if (allocated(error)) return
        call k%fit([3.0_real64, 3.0_real64, 3.0_real64], rule="silverman", ok=ok)
        call k%cdf(3.0_real64, c)
        call check(error, (.not. ok) .and. ieee_is_nan(c) .and. ieee_is_nan(k%bandwidth()), &
            "a constant sample under a rule has no bandwidth and is undefined")
        if (allocated(error)) return
        call k%curve(xg, fg, xmin=0.0_real64, xmax=4.0_real64)
        call check(error, all(ieee_is_nan(fg)) .and. xg(1) == 0.0_real64 .and. xg(5) == 4.0_real64, &
            "an undefined estimate's curve keeps the points it was given and answers NaN")
        if (allocated(error)) return
        call k%fit([3.0_real64, 3.0_real64, 3.0_real64], bandwidth=0.5_real64, ok=ok)
        call k%pdf(3.0_real64, f)
        call check(error, ok .and. abs(f - 2.0_real64*0.3989422804014327_real64/(1.0_real64 - &
            5.7330314375838782e-7_real64)) <= 1.0e-15_real64, &
            "a constant sample with an explicit bandwidth is one bump, phi(0)/h at its centre")

    end subroutine test_quiet_returns

    !> Every kernel, both corrections, one bound, the other and both, with the bandwidth larger
    !> than the range so that every point is corrected: the density integrates to one over the
    !> support, and `%cdf` runs from 0 to 1 across it.
    subroutine test_bounds_conserve_mass(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(kde_density) :: fn
        real(real64), allocatable :: x(:), cuts(:)
        real(real64) :: mass, c_lo, c_hi, a, b, h, lo, hi
        character(len=11), parameter :: METHODS(2) = [character(len=11) :: "renormalise", "reflect"]
        integer :: kk, mm, side
        character(len=120) :: what

        call kde_fixture(8_int64, x)
        x = x/500.0_real64
        lo = -1.0_real64
        hi = 1.0_real64
        do kk = 1, 4
            ! Wide enough that every kernel reaches past the far bound from anywhere inside.
            h = 3.0_real64/RADIUS(kk)*2.0_real64
            do mm = 1, 2
                do side = 1, 3
                    select case (side)
                    case (1)
                        call fn%k%fit(x, bandwidth=h, kernel=trim(KERNELS(kk)), lower=lo, &
                            boundary=trim(METHODS(mm)))
                        a = lo
                        b = maxval(x) + RADIUS(kk)*h
                    case (2)
                        call fn%k%fit(x, bandwidth=h, kernel=trim(KERNELS(kk)), upper=hi, &
                            boundary=trim(METHODS(mm)))
                        a = minval(x) - RADIUS(kk)*h
                        b = hi
                    case default
                        call fn%k%fit(x, bandwidth=h, kernel=trim(KERNELS(kk)), lower=lo, upper=hi, &
                            boundary=trim(METHODS(mm)))
                        a = lo
                        b = hi
                    end select
                    write(what, '(a,1x,a,a,i0)') trim(KERNELS(kk)), trim(METHODS(mm)), " side ", side
                    call kinks(fn%k, x, h*RADIUS(kk), a, b, side, cuts)
                    fn%power = 0
                    if (size(cuts) > 0) then
                        mass = pf_integrate(fn, a, b, 1.0e-12_real64, breakpoints=cuts)
                    else
                        mass = pf_integrate(fn, a, b, 1.0e-12_real64)
                    end if
                    call check(error, abs(mass - 1.0_real64) <= 1.0e-10_real64, &
                        trim(what) // ": the density must integrate to one over the support")
                    if (allocated(error)) return
                    call fn%k%cdf(a + 1.0e-12_real64, c_lo)
                    call fn%k%cdf(b - 1.0e-12_real64, c_hi)
                    call check(error, c_lo <= 1.0e-10_real64 .and. c_hi >= 1.0_real64 - 1.0e-10_real64, &
                        trim(what) // ": %cdf must run from 0 to 1 across the support")
                    if (allocated(error)) return
                end do
            end do
        end do

    contains

        !> Every point inside `(a, b)` where some image's kernel starts or stops: the integrand's
        !> jumps (the box) and kinks, which `pf_integrate` is told about rather than left to find.
        subroutine kinks(k, xs, reach, a, b, side, cuts)
            type(pf_kde), intent(in)               :: k       !! the estimate (unused; its settings)
            real(real64), intent(in)               :: xs(:)   !! the sample
            real(real64), intent(in)               :: reach   !! the support radius times `h`
            real(real64), intent(in)               :: a       !! the range's lower end
            real(real64), intent(in)               :: b       !! the range's upper end
            integer, intent(in)                    :: side    !! which bounds are set
            real(real64), allocatable, intent(out) :: cuts(:) !! sorted, distinct, inside
            real(real64), allocatable :: c(:)
            real(real64) :: v, t
            integer :: i, j, n

            if (.not. k%is_fitted()) return
            allocate(c(0))
            do i = 1, size(xs)
                do j = 1, 3
                    select case (j)
                    case (1)
                        v = xs(i)
                    case (2)
                        v = 2.0_real64*lo - xs(i)
                    case default
                        v = 2.0_real64*hi - xs(i)
                    end select
                    if (j == 2 .and. side == 2) cycle
                    if (j == 3 .and. side == 1) cycle
                    ! The support's ends, and the B-spline's inner knots halfway out.
                    c = [c, v - reach, v + reach, v - 0.5_real64*reach, v + 0.5_real64*reach]
                end do
            end do
            ! Sort, and keep the distinct values strictly inside.
            n = size(c)
            do i = 2, n
                t = c(i)
                j = i - 1
                do while (j >= 1)
                    if (c(j) <= t) exit
                    c(j + 1) = c(j)
                    j = j - 1
                end do
                c(j + 1) = t
            end do
            allocate(cuts(0))
            do i = 1, n
                if (.not. (c(i) > a + 1.0e-9_real64 .and. c(i) < b - 1.0e-9_real64)) cycle
                if (size(cuts) > 0) then
                    if (c(i) - cuts(size(cuts)) <= 1.0e-9_real64) cycle
                end if
                cuts = [cuts, c(i)]
            end do

        end subroutine kinks

    end subroutine test_bounds_conserve_mass

    !> A point outside the support is excluded and counted, and the density there is zero.
    subroutine test_outside_excluded(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde) :: k
        real(real64) :: x(6), f, c
        integer(int64) :: n_out

        x = [-1.0_real64, 0.5_real64, 1.0_real64, 2.0_real64, 3.5_real64, &
            ieee_value(1.0_real64, ieee_quiet_nan)]
        x(6) = ieee_value(1.0_real64, ieee_positive_inf)
        call k%fit(x, bandwidth=0.5_real64, lower=0.0_real64, upper=3.0_real64, n_outside=n_out)
        call check(error, n_out == 3_int64 .and. k%n_valid() == 3_int64 .and. k%n_outside() == 3_int64, &
            "-1, 3.5 and +Infinity must be outside [0, 3]")
        if (allocated(error)) return
        call k%pdf(-0.1_real64, f)
        call k%cdf(-0.1_real64, c)
        call check(error, f == 0.0_real64 .and. c == 0.0_real64, "below the support: density 0, CDF 0")
        if (allocated(error)) return
        call k%pdf(3.1_real64, f)
        call k%cdf(3.1_real64, c)
        call check(error, f == 0.0_real64 .and. c == 1.0_real64, "above the support: density 0, CDF 1")
        if (allocated(error)) return
        call k%fit(x(1:5), bandwidth=0.5_real64, n_outside=n_out)
        call check(error, n_out == 0_int64, "with no bound, no finite point is outside")
        if (allocated(error)) return
        call k%fit(x, bandwidth=0.5_real64, n_outside=n_out)
        call check(error, n_out == 1_int64, "an infinite point is outside every support")

    end subroutine test_outside_excluded

    !> `%cdf(%quantile(p)) == p` at seven probabilities, unbounded and bounded, for every kernel;
    !> the two ends of the support at `p = 0` and `p = 1`.
    subroutine test_quantile_inverts_cdf(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde) :: k
        real(real64), allocatable :: x(:)
        real(real64), parameter :: PS(7) = [0.001_real64, 0.05_real64, 0.25_real64, 0.5_real64, &
            0.75_real64, 0.95_real64, 0.999_real64]
        real(real64) :: q(7), c(7), q0, q1
        integer :: kk, bounded

        call kde_fixture(50_int64, x)
        do kk = 1, 4
            do bounded = 0, 1
                if (bounded == 0) then
                    call k%fit(x, bandwidth=40.0_real64, kernel=trim(KERNELS(kk)))
                else
                    call k%fit(x, bandwidth=40.0_real64, kernel=trim(KERNELS(kk)), lower=-480.0_real64, &
                        upper=490.0_real64, boundary=merge("reflect    ", "renormalise", kk > 2))
                end if
                call k%quantile(PS, q)
                call k%cdf(q, c)
                call check(error, all(abs(c - PS) <= 1.0e-12_real64), &
                    trim(KERNELS(kk)) // ": %cdf(%quantile(p)) must be p")
                if (allocated(error)) return
                call k%quantile(0.0_real64, q0)
                call k%quantile(1.0_real64, q1)
                if (bounded == 0) then
                    call check(error, q0 == minval(x) - 40.0_real64*RADIUS(kk) .and. &
                        q1 == maxval(x) + 40.0_real64*RADIUS(kk), &
                        trim(KERNELS(kk)) // ": p = 0 and 1 must be the ends of the estimate's support")
                else
                    call check(error, q0 == -480.0_real64 .and. q1 == 490.0_real64, &
                        trim(KERNELS(kk)) // ": p = 0 and 1 must be the bounds")
                end if
                if (allocated(error)) return
            end do
        end do

    end subroutine test_quantile_inverts_cdf

    !> The default range is `cut` bandwidths beyond the data, clipped to the support, with the
    !> points equally spaced and both ends exact.
    subroutine test_curve_default_range(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde) :: k
        real(real64), allocatable :: x(:)
        real(real64) :: xg(11), fg(11), f
        real(real64) :: h

        call kde_fixture(32_int64, x)
        call k%fit(x, rule="scott")
        h = k%bandwidth()
        call k%curve(xg, fg)
        call check(error, xg(1) == minval(x) - 3.0_real64*h .and. xg(11) == maxval(x) + 3.0_real64*h, &
            "the default range must be min - 3h to max + 3h")
        if (allocated(error)) return
        call k%pdf(xg(6), f)
        call check(error, fg(6) == f, "the curve's values must be %pdf's")
        if (allocated(error)) return
        call k%curve(xg, fg, cut=1.0_real64)
        call check(error, xg(1) == minval(x) - h .and. xg(11) == maxval(x) + h, "cut= must set the reach")
        if (allocated(error)) return
        call k%fit(x, rule="scott", lower=-470.0_real64, upper=460.0_real64)
        call k%curve(xg, fg)
        call check(error, xg(1) == -470.0_real64 .and. xg(11) == 460.0_real64, &
            "the default range must be clipped to the support")
        if (allocated(error)) return
        call k%curve(xg, fg, xmin=-100.0_real64)
        call check(error, xg(1) == -100.0_real64 .and. xg(11) == 460.0_real64 .and. &
            abs(xg(2) - (-100.0_real64 + 56.0_real64)) <= 1.0e-12_real64, &
            "xmin= must replace one end only, and the points must be equally spaced")

    end subroutine test_curve_default_range

    !> A `real32` sample is widened: the fit equals the fit over its `real64` copy exactly.
    subroutine test_real32_widens(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde) :: a, b
        real(real64), allocatable :: x(:)
        real(real32), allocatable :: x32(:)
        real(real64) :: fa(NKX), fb(NKX)

        call kde_fixture(40_int64, x)
        x32 = real(x, real32)
        call a%fit(x32, rule="silverman")
        call b%fit(real(x32, real64), rule="silverman")
        call a%pdf(KG_X, fa)
        call b%pdf(KG_X, fb)
        call check(error, a%bandwidth() == b%bandwidth() .and. all(fa == fb), &
            "a real32 sample must be widened before anything else happens to it")

    end subroutine test_real32_widens

    !> The rank-1 forms answer exactly what the scalar forms answer, element by element.
    subroutine test_array_forms_match_scalar(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde) :: k
        real(real64), allocatable :: x(:)
        real(real64) :: f(NKX), c(NKX), q(3), v
        real(real64), parameter :: PS(3) = [0.1_real64, 0.5_real64, 0.9_real64]
        integer :: i

        call kde_fixture(32_int64, x)
        call k%fit(x, bandwidth=50.0_real64, kernel="bspline", lower=-470.0_real64, boundary="reflect")
        call k%pdf(KG_X, f)
        call k%cdf(KG_X, c)
        call k%quantile(PS, q)
        do i = 1, NKX
            call k%pdf(KG_X(i), v)
            call check(error, v == f(i), "%pdf's array form must equal its scalar form")
            if (allocated(error)) return
            call k%cdf(KG_X(i), v)
            call check(error, v == c(i), "%cdf's array form must equal its scalar form")
            if (allocated(error)) return
        end do
        do i = 1, 3
            call k%quantile(PS(i), v)
            call check(error, v == q(i), "%quantile's array form must equal its scalar form")
            if (allocated(error)) return
        end do

    end subroutine test_array_forms_match_scalar

    !> The tokens come back as given (matched without regard to case), and the bounds as set.
    subroutine test_tokens_and_accessors(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde) :: k
        real(real64), allocatable :: x(:)
        character(len=:), allocatable :: kname, rname
        real(real64) :: lo, hi

        call kde_fixture(20_int64, x)
        call k%fit(x, rule="  Scott", kernel="EPANECHNIKOV")
        call k%kernel(kname)
        call k%rule(rname)
        call k%bounds(lo, hi)
        call check(error, kname == "epanechnikov" .and. rname == "scott" .and. &
            (.not. ieee_is_finite(lo)) .and. lo < 0.0_real64 .and. (.not. ieee_is_finite(hi)) &
            .and. hi > 0.0_real64 .and. .not. k%is_adaptive(), &
            "tokens must fold case and an unbounded support must read -Infinity to +Infinity")
        if (allocated(error)) return
        call k%fit(x, bandwidth=2.0_real64, kernel="box", upper=500.0_real64)
        call k%kernel(kname)
        call k%rule(rname)
        call k%bounds(lo, hi)
        call check(error, kname == "box" .and. rname == "explicit" .and. hi == 500.0_real64 .and. &
            .not. ieee_is_finite(lo), 'an explicit bandwidth''s rule is "explicit"')

    end subroutine test_tokens_and_accessors

    !> A refit forgets the previous fit's settings, and `%clear` leaves the object unfitted.
    subroutine test_refit_and_clear(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde) :: k
        real(real64), allocatable :: x(:)
        real(real64) :: lo, hi, f

        call kde_fixture(20_int64, x)
        call k%fit(x, bandwidth=10.0_real64, lower=-500.0_real64, weights=spread(1.0_real64, 1, 20))
        call k%fit(x, bandwidth=10.0_real64)
        call k%bounds(lo, hi)
        call check(error, .not. ieee_is_finite(lo) .and. k%sum_weights() == 20.0_real64, &
            "a refit must not inherit the previous fit's bound or weights")
        if (allocated(error)) return
        call k%pdf(0.0_real64, f)
        call k%clear()
        call check(error, .not. k%is_fitted(), "%clear must leave the object unfitted")

    end subroutine test_refit_and_clear

    !> `%print` writes a heading and its rows, including the counts and the support, and an
    !> unfitted object prints one line saying so rather than aborting.
    subroutine test_print_writes(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde) :: k, empty
        real(real64), allocatable :: x(:)
        integer :: u, nlines
        logical :: seen
        character(len=*), parameter :: PATH = "test_run/kde_print_writes.txt"

        call kde_fixture(20_int64, x)
        call k%fit(x, rule="silverman", lower=-480.0_real64)
        open(newunit=u, file=PATH, status="replace", action="write")
        call k%print(unit=u)
        close(u)
        call read_back(PATH, nlines, "n_outside", seen)
        ! The heading, eight rows, the lower bound and the correction. The count is asserted, not
        ! the text, so that a reworded label is not a failure while a dropped row is.
        call check(error, nlines == 11 .and. seen, "%print must write a heading and ten rows")
        if (allocated(error)) return
        open(newunit=u, file=PATH, status="replace", action="write")
        call empty%print(unit=u)
        close(u)
        call read_back(PATH, nlines, "not fitted", seen)
        call check(error, nlines == 1 .and. seen, "an unfitted object must print one line saying so")

    end subroutine test_print_writes

    !> The table in doc/pages/utilities/kernel-density.md, "Bounded support": two hundred points at
    !> the quantiles `sqrt((i - 1/2)/200)` of the density `2x` on `[0, 1]`, Silverman's rule, read at
    !> four points unbounded and under each correction. Asserted to the three decimals the page prints,
    !> so a change to either correction that moves the page's numbers fails here first.
    subroutine test_guide_boundary_table(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde) :: k
        real(real64) :: x(200), got(4)
        real(real64), parameter :: AT(4) = [0.0_real64, 0.05_real64, 0.1_real64, 0.2_real64]
        real(real64), parameter :: NONE(4) = [0.058_real64, 0.122_real64, 0.207_real64, 0.400_real64]
        real(real64), parameter :: REN(4) = [0.068_real64, 0.137_real64, 0.221_real64, 0.405_real64]
        real(real64), parameter :: REF(4) = [0.117_real64, 0.143_real64, 0.212_real64, 0.400_real64]
        integer :: i

        do i = 1, 200
            x(i) = sqrt((real(i, real64) - 0.5_real64)/200.0_real64)
        end do
        call k%fit(x, rule="silverman")
        call check(error, abs(k%bandwidth() - 0.0737_real64) <= 5.0e-5_real64, &
            "the page prints h = 0.0737")
        if (allocated(error)) return
        call k%pdf(AT, got)
        call check(error, all(abs(got - NONE) <= 5.0e-4_real64), "the page's unbounded column")
        if (allocated(error)) return
        call k%fit(x, rule="silverman", lower=0.0_real64)
        call k%pdf(AT, got)
        call check(error, all(abs(got - REN) <= 5.0e-4_real64), "the page's renormalise column")
        if (allocated(error)) return
        call k%fit(x, rule="silverman", lower=0.0_real64, boundary="reflect")
        call k%pdf(AT, got)
        call check(error, all(abs(got - REF) <= 5.0e-4_real64), "the page's reflect column")

    end subroutine test_guide_boundary_table

    ! ==========================================================================================
    ! pf_kde_grid
    ! ==========================================================================================

    !> `size(t)` points spread irregularly over `[a, b]`: the fractional parts of `i` times the
    !> golden ratio, which land at every position within the cells of any grid, as a convergence
    !> rate measured over positions needs.
    subroutine spread_points(a, b, t)
        real(real64), intent(in)  :: a    !! the first end
        real(real64), intent(in)  :: b    !! the other end
        real(real64), intent(out) :: t(:) !! the points
        integer :: i

        do i = 1, size(t)
            t(i) = a + (b - a)*modulo(real(i, real64)*0.6180339887498949_real64, 1.0_real64)
        end do

    end subroutine spread_points

    !> `kde_fixture`'s values folded onto `[0, 1)` and squared: a density rising towards zero, so
    !> that a lower bound at zero matters.
    subroutine bounded_fixture(n, z)
        integer(int64), intent(in)             :: n    !! how many values
        real(real64), allocatable, intent(out) :: z(:) !! the values, in `[0, 1)`

        call kde_fixture(n, z)
        z = (abs(z)/489.0_real64)**2

    end subroutine bounded_fixture

    !> The root mean square of `d`.
    pure function rms(d) result(r)
        real(real64), intent(in) :: d(:) !! the differences
        real(real64)             :: r    !! their RMS

        r = sqrt(sum(d*d)/real(size(d), real64))

    end function rms

    !> The raw accumulation of a grid, `%density(normalise=.false.)`, into a new array.
    subroutine raw_cells(g, f)
        type(pf_kde_grid), intent(in)          :: g    !! the grid
        real(real64), allocatable, intent(out) :: f(:) !! its accumulation per cell

        allocate(f(g%ncells()))
        call g%density(f, normalise=.false.)

    end subroutine raw_cells

    !> The grid converges to the exact estimate as the square of its cell width. Linear
    !> interpolation's error at a point a fraction `s` into a segment is `s(1 - s)/2 * step**2 * f''`,
    !> so its RMS over points spread irregularly across the cells falls by four when the cells
    !> halve. A cell index off by one, or a centre off by half a cell, leaves an error of order
    !> `step` and a ratio of two. The two smooth kernels, unbounded and under both corrections
    !> (read away from the outer half-cells, where the interpolant is constant); the other two
    !> have kinks or jumps in their estimate and converge more slowly, by design.
    subroutine test_grid_converges(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde) :: k
        type(pf_kde_grid) :: g
        real(real64), allocatable :: x(:), z(:), c(:), f(:), fe(:)
        real(real64) :: t(997), fx(997), fg(997), e(2), h, lo, hi
        character(len=11), parameter :: METHODS(2) = [character(len=11) :: "renormalise", "reflect"]
        integer :: kk, r, mm
        character(len=200) :: msg

        call kde_fixture(400_int64, x)
        h = 40.0_real64
        lo = minval(x) - 6.0_real64*h
        hi = maxval(x) + 6.0_real64*h
        call spread_points(minval(x), maxval(x), t)
        do kk = 1, 3, 2
            call k%fit(x, bandwidth=h, kernel=KERNELS(kk))
            call k%pdf(t, fx)
            do r = 1, 2
                call g%init(100*r, lo, hi, h, kernel=KERNELS(kk))
                call g%add(x)
                call g%pdf(t, fg)
                e(r) = rms(fg - fx)
            end do
            write(msg, '(3a,f8.4)') "unbounded ", trim(KERNELS(kk)), &
                ": halving the cells must divide the RMS error by four; the ratio is ", e(1)/e(2)
            call check(error, e(1)/e(2) > 3.6_real64 .and. e(1)/e(2) < 4.4_real64, trim(msg))
            if (allocated(error)) return
        end do

        ! At its own centres the Gaussian grid is the exact estimate to the residual of the
        ! discrete normalisation, which is far below the interpolation error.
        call k%fit(x, bandwidth=h)
        call g%init(100, lo, hi, h)
        call g%add(x)
        allocate(c(100), f(100), fe(100))
        call g%density(f, x=c)
        call k%pdf(c, fe)
        write(msg, '(a,es10.3)') "at its centres the grid must be the exact estimate; the largest " // &
            "relative gap is ", maxval(abs(f - fe))/maxval(fe)
        call check(error, maxval(abs(f - fe)) <= 1.0e-6_real64*maxval(fe), trim(msg))
        if (allocated(error)) return

        call bounded_fixture(400_int64, z)
        call spread_points(0.02_real64, 0.98_real64, t)
        do mm = 1, 2
            do kk = 1, 3, 2
                call k%fit(z, bandwidth=0.05_real64, kernel=KERNELS(kk), lower=0.0_real64, &
                    upper=1.0_real64, boundary=METHODS(mm))
                call k%pdf(t, fx)
                do r = 1, 2
                    call g%init(100*r, 0.0_real64, 1.0_real64, 0.05_real64, kernel=KERNELS(kk), &
                        lower=0.0_real64, upper=1.0_real64, boundary=METHODS(mm))
                    call g%add(z)
                    call g%pdf(t, fg)
                    e(r) = rms(fg - fx)
                end do
                write(msg, '(4a,f8.4)') trim(METHODS(mm)), " ", trim(KERNELS(kk)), &
                    ": halving the cells must divide the RMS error by four; the ratio is ", e(1)/e(2)
                call check(error, e(1)/e(2) > 3.6_real64 .and. e(1)/e(2) < 4.4_real64, trim(msg))
                if (allocated(error)) return
            end do
        end do

    end subroutine test_grid_converges

    !> Every accepted point deposits exactly its weight, at every resolution: `sum(f)*step` is one
    !> to rounding for every kernel, unbounded, and under both corrections with the bandwidth
    !> twice the width of the support (every point corrected, F3's case), and under one bound.
    !> `normalise = .false.` sums to the total weight. A deposit normalised by the midpoint rule
    !> instead of by its own discrete sum misses by that rule's residual, `1e-3` and more here.
    subroutine test_grid_deposits_exact_mass(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde_grid) :: g
        real(real64), allocatable :: x(:), w(:), f(:)
        character(len=11), parameter :: METHODS(2) = [character(len=11) :: "renormalise", "reflect"]
        integer, parameter :: CELLS(3) = [7, 64, 501]
        integer :: kk, r, mm
        character(len=160) :: msg

        call kde_fixture(64_int64, x)
        x = x/1000.0_real64 + 0.5_real64
        call kde_weights_mod5(64_int64, w)
        do kk = 1, 4
            do r = 1, 3
                ! Unbounded, on a range every kernel's reach fits inside.
                call g%init(CELLS(r), -0.2_real64, 1.2_real64, 0.03_real64, kernel=KERNELS(kk))
                call g%add(x)
                call raw_cells(g, f)
                write(msg, '(2a,i0,a,es10.3)') trim(KERNELS(kk)), " unbounded at ", CELLS(r), &
                    " cells: the mass is one plus ", sum(f)*g%step()/g%sum_weights() - 1.0_real64
                call check(error, abs(sum(f)*g%step()/g%sum_weights() - 1.0_real64) <= 1.0e-13_real64, trim(msg))
                if (allocated(error)) return
                do mm = 1, 2
                    ! Both bounds, the bandwidth twice the support: every point is corrected.
                    call g%init(CELLS(r), 0.0_real64, 1.0_real64, 2.0_real64, kernel=KERNELS(kk), &
                        lower=0.0_real64, upper=1.0_real64, boundary=METHODS(mm))
                    call g%add(x, weights=w)
                    call raw_cells(g, f)
                    write(msg, '(4a,i0,a,es10.3)') trim(KERNELS(kk)), " ", trim(METHODS(mm)), &
                        " (wide) at ", CELLS(r), " cells: the mass is one plus ", &
                        sum(f)*g%step()/g%sum_weights() - 1.0_real64
                    call check(error, abs(sum(f)*g%step()/g%sum_weights() - 1.0_real64) <= 1.0e-13_real64, &
                        trim(msg))
                    if (allocated(error)) return
                    ! One bound, the range reaching past every kernel on the open side.
                    call g%init(CELLS(r), 0.0_real64, 1.5_real64, 0.08_real64, kernel=KERNELS(kk), &
                        lower=0.0_real64, boundary=METHODS(mm))
                    call g%add(x)
                    call raw_cells(g, f)
                    write(msg, '(4a,i0,a,es10.3)') trim(KERNELS(kk)), " ", trim(METHODS(mm)), &
                        " (lower) at ", CELLS(r), " cells: the mass is one plus ", &
                        sum(f)*g%step()/g%sum_weights() - 1.0_real64
                    call check(error, abs(sum(f)*g%step()/g%sum_weights() - 1.0_real64) <= 1.0e-13_real64, &
                        trim(msg))
                    if (allocated(error)) return
                end do
            end do
        end do
        call check(error, g%sum_weights() == 64.0_real64, &
            "the last grid, unweighted, must carry one unit of weight per point")

    end subroutine test_grid_deposits_exact_mass

    !> The weight a grid narrower than the data counts below `xmin` and above `xmax` is the exact
    !> estimate's own distribution function at those two points, to rounding: each point's share
    !> beyond an end is formed from its kernel's distribution function, as `pf_kde%cdf` forms it.
    !> Beyond the range `%pdf` is zero, `%cdf` is flat, and a quantile there answers the range's end.
    subroutine test_grid_counts_weight_beyond_range(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde) :: k
        type(pf_kde_grid) :: g
        real(real64), allocatable :: x(:), z(:)
        real(real64) :: p0, p1, e0, e1, pb, fb, q
        character(len=11), parameter :: METHODS(2) = [character(len=11) :: "renormalise", "reflect"]
        integer :: mm

        call kde_fixture(400_int64, x)
        call k%fit(x, bandwidth=40.0_real64)
        call g%init(100, -100.0_real64, 100.0_real64, 40.0_real64)
        call g%add(x)
        call g%cdf(-100.0_real64, p0)
        call g%cdf(100.0_real64, p1)
        call k%cdf(-100.0_real64, e0)
        call k%cdf(100.0_real64, e1)
        call check(error, p0 > 0.3_real64 .and. p1 < 0.7_real64, &
            "the fixture must put a good part of its weight beyond each end, or this test is vacuous")
        if (allocated(error)) return
        call check(error, abs(p0 - e0) <= 1.0e-12_real64 .and. abs(p1 - e1) <= 1.0e-12_real64, &
            "the weight counted beyond each end must be the exact estimate's distribution function there")
        if (allocated(error)) return
        call g%cdf(-300.0_real64, pb)
        call g%pdf(-300.0_real64, fb)
        call check(error, pb == p0 .and. fb == 0.0_real64, &
            "below the range the density is zero and the distribution function flat")
        if (allocated(error)) return
        call g%quantile(0.5_real64*p0, q)
        call check(error, q == -100.0_real64, "a quantile the weight below xmin covers answers xmin")
        if (allocated(error)) return
        call g%quantile(0.5_real64*(1.0_real64 + p1), q)
        call check(error, q == 100.0_real64, "a quantile only the weight above xmax reaches answers xmax")
        if (allocated(error)) return

        ! Under both corrections, with the range inside the support.
        call bounded_fixture(400_int64, z)
        do mm = 1, 2
            call k%fit(z, bandwidth=0.05_real64, lower=0.0_real64, upper=1.0_real64, boundary=METHODS(mm))
            call g%init(60, 0.2_real64, 0.8_real64, 0.05_real64, lower=0.0_real64, upper=1.0_real64, &
                boundary=METHODS(mm))
            call g%add(z)
            call g%cdf(0.2_real64, p0)
            call g%cdf(0.8_real64, p1)
            call k%cdf(0.2_real64, e0)
            call k%cdf(0.8_real64, e1)
            call check(error, abs(p0 - e0) <= 1.0e-12_real64 .and. abs(p1 - e1) <= 1.0e-12_real64, &
                trim(METHODS(mm)) // ": the weight counted beyond each end must be the exact " // &
                "estimate's distribution function there")
            if (allocated(error)) return
        end do

    end subroutine test_grid_counts_weight_beyond_range

    !> A kernel narrower than a cell: on a centre it lands whole in that cell; between two centres
    !> it reaches neither and lands whole in the cell holding the point; a kernel reaching the two
    !> centres either side of it splits evenly between them. Near `xmin` the part beyond is counted
    !> there and the rest still lands in the first cell.
    subroutine test_grid_narrow_kernel(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde_grid) :: g
        real(real64) :: f(10), p

        call g%init(10, 0.0_real64, 10.0_real64, 0.01_real64)
        call g%add(2.5_real64)
        call g%density(f)
        call check(error, count(f /= 0.0_real64) == 1 .and. abs(f(3) - 1.0_real64) <= 1.0e-15_real64, &
            "a kernel on cell 3's centre must land whole in cell 3")
        if (allocated(error)) return
        call g%clear()
        call g%add(7.0_real64)
        call g%density(f)
        call check(error, count(f /= 0.0_real64) == 1 .and. abs(f(8) - 1.0_real64) <= 1.0e-15_real64, &
            "a kernel between two centres must land whole in the cell holding the point")
        if (allocated(error)) return
        call g%init(10, 0.0_real64, 10.0_real64, 0.3_real64, kernel="box")
        call g%add(5.0_real64)
        call g%density(f)
        call check(error, count(f /= 0.0_real64) == 2 .and. abs(f(5) - f(6)) <= 1.0e-15_real64 .and. &
            abs(f(5) + f(6) - 1.0_real64) <= 1.0e-15_real64, &
            "a box reaching the centres either side of it must split evenly between them")
        if (allocated(error)) return
        call g%init(10, 0.0_real64, 10.0_real64, 0.01_real64)
        call g%add(0.001_real64)
        call g%density(f)
        call g%cdf(0.0_real64, p)
        call check(error, count(f /= 0.0_real64) == 1 .and. p > 0.4_real64 .and. &
            abs(f(1) + p - 1.0_real64) <= 1.0e-15_real64, &
            "a narrow kernel crossing xmin must put the part beyond it below and the rest in cell 1")

    end subroutine test_grid_narrow_kernel

    !> `%pdf` is exact at every centre (the same bits as `%density`), the mean of its neighbours at
    !> a midpoint, constant over the outer half-cells, and zero outside the range.
    subroutine test_grid_pdf_interpolates(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde_grid) :: g
        real(real64), allocatable :: x(:)
        real(real64) :: c(40), f(40), fc(40), m(39), fm(39), a, b, v
        integer :: j

        call kde_fixture(50_int64, x)
        call g%init(40, -700.0_real64, 700.0_real64, 60.0_real64)
        call g%add(x)
        call g%density(f, x=c)
        call g%pdf(c, fc)
        call check(error, all(fc == f), "%pdf at a centre must be %density there, to the bit")
        if (allocated(error)) return
        do j = 1, 39
            m(j) = 0.5_real64*(c(j) + c(j + 1))
        end do
        call g%pdf(m, fm)
        call check(error, all(abs(fm - 0.5_real64*(f(1:39) + f(2:40))) <= 1.0e-14_real64*maxval(f)), &
            "%pdf midway between two centres must be their mean")
        if (allocated(error)) return
        call g%pdf(-700.0_real64, a)
        call g%pdf(-700.0_real64 + 0.25_real64*g%step(), b)
        call g%pdf(700.0_real64, v)
        call check(error, a == f(1) .and. b == f(1) .and. v == f(40), &
            "%pdf must be constant over the outer half of the first and the last cell")
        if (allocated(error)) return
        call g%pdf(-701.0_real64, a)
        call g%pdf(701.0_real64, b)
        call g%pdf(ieee_value(1.0_real64, ieee_quiet_nan), v)
        call check(error, a == 0.0_real64 .and. b == 0.0_real64 .and. ieee_is_nan(v), &
            "%pdf must be zero outside the range and NaN at a NaN")

    end subroutine test_grid_pdf_interpolates

    !> `%cdf` at each centre is the running trapezoid of `%density` from `xmin`, a difference inside
    !> one segment is the trapezoid of the linear `%pdf`, and `%quantile` inverts `%cdf` both ways;
    !> `p = 0` and `p = 1` answer where the accumulated density starts and ends inside the range.
    subroutine test_grid_cdf_and_quantile(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde_grid) :: g
        real(real64), allocatable :: x(:)
        real(real64) :: c(64), f(64), p(64), cum, a, b, pa, pb, fa, fb, s
        real(real64) :: pp(99), q(99), back(99), t(50), pt(50), qt(50), q0, q1, p0, p1, f0
        integer :: j

        call kde_fixture(200_int64, x)
        x = x/1000.0_real64
        ! A range cutting through the data, so that the first cell holds weight and the weight
        ! counted below `xmin` starts the running sum.
        call g%init(64, -0.3_real64, 0.3_real64, 0.05_real64)
        call g%add(x)
        call g%density(f, x=c)
        s = g%step()
        call g%cdf(c, p)
        call g%cdf(-0.3_real64, cum)
        call check(error, cum > 0.05_real64 .and. f(1) > 0.0_real64, &
            "the fixture must put weight below xmin and in the first cell, or the sum below is vacuous")
        if (allocated(error)) return
        cum = cum + 0.5_real64*f(1)*s
        do j = 1, 64
            if (j > 1) cum = cum + 0.5_real64*(f(j - 1) + f(j))*s
            call check(error, abs(p(j) - cum) <= 1.0e-14_real64, &
                "%cdf at a centre must be the weight below xmin plus the running trapezoid of %density")
            if (allocated(error)) return
        end do
        call g%init(64, -1.0_real64, 1.0_real64, 0.05_real64)
        call g%add(x)
        call g%density(f, x=c)
        s = g%step()
        a = c(30) + 0.2_real64*s
        b = c(30) + 0.7_real64*s
        call g%cdf(a, pa)
        call g%cdf(b, pb)
        call g%pdf(a, fa)
        call g%pdf(b, fb)
        call check(error, abs((pb - pa) - (b - a)*0.5_real64*(fa + fb)) <= 1.0e-15_real64, &
            "inside a segment %cdf must integrate the linear %pdf exactly")
        if (allocated(error)) return
        do j = 1, 99
            pp(j) = real(j, real64)/100.0_real64
        end do
        call g%quantile(pp, q)
        call g%cdf(q, back)
        call check(error, all(abs(back - pp) <= 1.0e-14_real64), "%cdf(%quantile(p)) must be p")
        if (allocated(error)) return
        call check(error, all(q(2:99) >= q(1:98)), "%quantile must be monotone in p")
        if (allocated(error)) return
        call spread_points(-0.45_real64, 0.45_real64, t)
        call g%cdf(t, pt)
        call g%quantile(pt, qt)
        call check(error, all(abs(qt - t) <= 1.0e-12_real64), "%quantile(%cdf(x)) must be x where the density is positive")
        if (allocated(error)) return

        ! A grid whose outer cells hold nothing: `p = 0` and `p = 1` are where the density starts and
        ! ends, and `%cdf` is 0 and 1 there.
        call g%init(64, -1.0_real64, 1.0_real64, 0.01_real64, kernel="box")
        call g%add(x)
        call g%quantile(0.0_real64, q0)
        call g%quantile(1.0_real64, q1)
        call g%cdf(q0, p0)
        call g%cdf(q1, p1)
        call g%pdf(q0, f0)
        call check(error, q0 > -0.6_real64 .and. q1 < 0.6_real64 .and. p0 == 0.0_real64 .and. f0 == 0.0_real64 &
            .and. abs(p1 - 1.0_real64) <= 1.0e-14_real64, &
            "p = 0 and p = 1 must answer where the density starts and ends inside the range")

    end subroutine test_grid_cdf_and_quantile

    !> `%merge`: a grid over all but the last point merged with a grid over that point is the grid
    !> over them all, to the bit, as is a merge into an empty grid; two halves merged agree with the
    !> whole to rounding (the additions group differently), with every count exact; and a poisoned
    !> grid poisons the merge.
    subroutine test_grid_merge(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde_grid) :: g_all, g1, g2, ge, gp
        real(real64), allocatable :: x(:), w(:), r_all(:), r(:)
        real(real64) :: f(30), ends(2), ends_all(2), q_before, q_after, p_low

        call kde_fixture(101_int64, x)
        call kde_weights_mod5(101_int64, w)
        call g_all%init(30, -700.0_real64, 700.0_real64, 50.0_real64)
        call g_all%add(x)
        call g1%init(30, -700.0_real64, 700.0_real64, 50.0_real64)
        call g1%add(x(1:100))
        call g2%init(30, -700.0_real64, 700.0_real64, 50.0_real64)
        call g2%add(x(101:101))
        call g1%merge(g2)
        call raw_cells(g_all, r_all)
        call raw_cells(g1, r)
        call check(error, all(r == r_all) .and. g1%n() == 101_int64 .and. g1%n_valid() == 101_int64 &
            .and. g1%sum_weights() == g_all%sum_weights(), &
            "a grid over 100 points merged with one over the 101st must be the grid over all 101, to the bit")
        if (allocated(error)) return
        ! The kernels reach past both ends of this range, so the weight counted there merges too.
        call g1%cdf([-700.0_real64, 700.0_real64], ends)
        call g_all%cdf([-700.0_real64, 700.0_real64], ends_all)
        call check(error, ends_all(1) > 0.0_real64 .and. ends_all(2) < 1.0_real64 .and. all(ends == ends_all), &
            "the weight counted beyond each end must merge, to the bit")
        if (allocated(error)) return
        call ge%init(30, -700.0_real64, 700.0_real64, 50.0_real64)
        call ge%merge(g_all)
        call raw_cells(ge, r)
        call check(error, all(r == r_all), "a merge into an empty grid must copy the other, to the bit")
        if (allocated(error)) return

        call g_all%init(30, -700.0_real64, 700.0_real64, 50.0_real64, kernel="bspline")
        call g_all%add(x, weights=w)
        call g1%init(30, -700.0_real64, 700.0_real64, 50.0_real64, kernel="bspline")
        call g1%add(x(1:50), weights=w(1:50))
        call g2%init(30, -700.0_real64, 700.0_real64, 50.0_real64, kernel="bspline")
        call g2%add(x(51:101), weights=w(51:101))
        call g1%merge(g2)
        call raw_cells(g_all, r_all)
        call raw_cells(g1, r)
        call check(error, maxval(abs(r - r_all)) <= 1.0e-14_real64*maxval(r_all), &
            "two weighted halves merged must equal the whole to rounding")
        if (allocated(error)) return
        call check(error, g1%n() == g_all%n() .and. g1%n_valid() == g_all%n_valid() .and. &
            g1%n_null() == g_all%n_null() .and. g1%n_nan() == g_all%n_nan() .and. &
            g1%n_outside() == g_all%n_outside() .and. g1%sum_weights() == g_all%sum_weights(), &
            "every count must merge exactly")
        if (allocated(error)) return

        ! Only the grid merged in has weight above its range, which moves `p = 1` from where the
        ! density ends inside the range to `xmax`.
        call g1%init(10, 0.0_real64, 1.0_real64, 0.02_real64)
        call g1%add(0.5_real64)
        call g2%init(10, 0.0_real64, 1.0_real64, 0.02_real64)
        call g2%add(1.5_real64)
        call g1%quantile(1.0_real64, q_before)
        call g1%merge(g2)
        call g1%quantile(1.0_real64, q_after)
        call check(error, q_before < 1.0_real64 .and. q_after == 1.0_real64, &
            "weight above the range must merge: p = 1 then answers xmax")
        if (allocated(error)) return
        ! And only the grid merged in has weight below it: one point of two, so %cdf(xmin) is 1/2.
        call g1%init(10, 0.0_real64, 1.0_real64, 0.02_real64)
        call g1%add(0.5_real64)
        call g2%init(10, 0.0_real64, 1.0_real64, 0.02_real64)
        call g2%add(-0.5_real64)
        call g1%merge(g2)
        call g1%cdf(0.0_real64, p_low)
        call check(error, p_low == 0.5_real64, "weight below the range must merge: %cdf(xmin) is then 1/2")
        if (allocated(error)) return

        call g1%init(30, -700.0_real64, 700.0_real64, 50.0_real64, kernel="bspline")
        call g1%add(x(1:50), weights=w(1:50))
        call gp%init(30, -700.0_real64, 700.0_real64, 50.0_real64, kernel="bspline")
        call gp%add([1.0_real64, ieee_value(1.0_real64, ieee_quiet_nan)], skipnan=.false.)
        call g1%merge(gp)
        call g1%density(f)
        call check(error, all(ieee_is_nan(f)), "merging a poisoned grid must poison the result")

    end subroutine test_grid_merge

    !> The family's exclusion order on every `%add`, counted per call through the optional outputs
    !> and in total through the accessors; the support's exclusion added. The scalar form equals
    !> the array form, and the `real32` form the `real64` form of the widened values, to the bit.
    subroutine test_grid_population_rules(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde_grid) :: g, g1, g2
        real(real64) :: x(7), w(7), nan, inf
        real(real32) :: x32(5)
        logical :: valid(7)
        integer(int64) :: n_null, n_nan, n_out, want_n, want_null, want_nan, i
        real(real64), allocatable :: r1(:), r2(:)

        nan = ieee_value(1.0_real64, ieee_quiet_nan)
        inf = ieee_value(1.0_real64, ieee_positive_inf)
        x = [1.0_real64, 2.0_real64, nan, 4.0_real64, 5.0_real64, 6.0_real64, 7.5_real64]
        w = [1.0_real64, nan, 0.0_real64, 2.0_real64, 0.0_real64, 1.0_real64, 3.0_real64]
        valid = [.true., .false., .true., .true., .true., .true., .true.]
        call g%init(16, 0.0_real64, 8.0_real64, 0.5_real64, lower=0.0_real64, upper=8.0_real64)
        call g%add(x, is_valid=valid, weights=w, n_null=n_null, n_nan=n_nan, n_outside=n_out)
        call pf_count_valid(x, want_n, is_valid=valid, weights=w, n_null=want_null, n_nan=want_nan)
        call check(error, n_null == want_null .and. n_nan == want_nan .and. n_out == 0_int64 .and. &
            g%n_valid() == want_n .and. g%n() == 7_int64 .and. g%sum_weights() == 7.0_real64, &
            "the exclusion counts must be pf_count_valid's")
        if (allocated(error)) return
        call g%add([-1.0_real64, 3.0_real64, inf], n_null=n_null, n_nan=n_nan, n_outside=n_out)
        call check(error, n_null == 0_int64 .and. n_nan == 0_int64 .and. n_out == 2_int64 .and. &
            g%n() == 10_int64 .and. g%n_valid() == want_n + 1_int64 .and. g%n_outside() == 2_int64 .and. &
            g%n_null() == want_null .and. g%sum_weights() == 8.0_real64, &
            "a second %add must report its own counts and add them to the grid's totals")
        if (allocated(error)) return

        call g1%init(16, 0.0_real64, 8.0_real64, 0.5_real64, kernel="epanechnikov")
        call g2%init(16, 0.0_real64, 8.0_real64, 0.5_real64, kernel="epanechnikov")
        call g1%add([1.0_real64, 2.5_real64, 4.0_real64, 6.25_real64], weights=[1.0_real64, 2.0_real64, &
            0.5_real64, 3.0_real64], is_valid=[.true., .true., .false., .true.])
        call g2%add(1.0_real64, weights=1.0_real64)
        call g2%add(2.5_real64, weights=2.0_real64)
        call g2%add(4.0_real64, weights=0.5_real64, is_valid=.false.)
        call g2%add(6.25_real64, weights=3.0_real64)
        call raw_cells(g1, r1)
        call raw_cells(g2, r2)
        call check(error, all(r1 == r2) .and. g2%n_null() == 1_int64 .and. g2%n() == 4_int64, &
            "adding the points one at a time must equal adding the array, to the bit")
        if (allocated(error)) return

        do i = 1_int64, 5_int64
            x32(i) = real(i, real32)*1.3_real32
        end do
        call g1%init(16, 0.0_real64, 8.0_real64, 0.5_real64)
        call g2%init(16, 0.0_real64, 8.0_real64, 0.5_real64)
        call g1%add(x32)
        call g2%add(real(x32, real64))
        call raw_cells(g1, r1)
        call raw_cells(g2, r2)
        call check(error, all(r1 == r2), "a real32 array must deposit as its widened values do")
        if (allocated(error)) return
        call g1%add(x32(1))
        call g2%add(real(x32(1), real64))
        call raw_cells(g1, r1)
        call raw_cells(g2, r2)
        call check(error, all(r1 == r2) .and. g1%n() == 6_int64, &
            "a real32 point must deposit as its widened value does")

    end subroutine test_grid_population_rules

    !> An initialised grid with nothing in it answers zeros from `%density` and NaN from the
    !> interpolated queries; `skipnan=.false.` with a NaN poisons every later answer, until
    !> `%clear`; an all-null `%add` leaves the grid empty and counted.
    subroutine test_grid_empty_and_poisoned(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde_grid) :: g
        real(real64) :: f(8), pdf, cdf, q

        call g%init(8, 0.0_real64, 8.0_real64, 0.5_real64)
        call g%density(f)
        call g%pdf(4.0_real64, pdf)
        call g%cdf(4.0_real64, cdf)
        call g%quantile(0.5_real64, q)
        call check(error, all(f == 0.0_real64) .and. ieee_is_nan(pdf) .and. ieee_is_nan(cdf) .and. &
            ieee_is_nan(q) .and. g%n() == 0_int64, &
            "an empty grid must answer zeros from %density and NaN from its queries")
        if (allocated(error)) return
        call g%add([1.0_real64, 2.0_real64], is_valid=[.false., .false.])
        call g%density(f)
        call check(error, all(f == 0.0_real64) .and. g%n_null() == 2_int64 .and. g%n_valid() == 0_int64, &
            "an all-null %add must leave the grid empty and count the nulls")
        if (allocated(error)) return
        call g%add([1.0_real64, ieee_value(1.0_real64, ieee_quiet_nan), 2.0_real64], skipnan=.false.)
        call g%add([3.0_real64])
        call g%density(f)
        call g%pdf(2.0_real64, pdf)
        call check(error, all(ieee_is_nan(f)) .and. ieee_is_nan(pdf) .and. g%n_valid() == 4_int64, &
            "a NaN kept by skipnan=.false. must make every later answer NaN")
        if (allocated(error)) return
        call g%clear()
        call g%add([3.0_real64])
        call g%density(f)
        call check(error, .not. any(ieee_is_nan(f)) .and. abs(sum(f) - 1.0_real64) <= 1.0e-15_real64, &
            "%clear must lift the poison")

    end subroutine test_grid_empty_and_poisoned

    !> The accessors report what `%init` was given, tokens folded to lower case; `%clear` keeps the
    !> geometry and empties the grid; a second `%init` replaces everything.
    subroutine test_grid_accessors_and_clear(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde_grid) :: g
        real(real64) :: c(50), lo, hi, f(50)
        character(len=:), allocatable :: name

        call check(error, .not. g%is_initialised(), "a new grid must not be initialised")
        if (allocated(error)) return
        call g%init(50, -1.0_real64, 4.0_real64, 0.2_real64, kernel="EPANECHNIKOV", lower=-1.0_real64, &
            upper=4.0_real64, boundary="Reflect")
        call g%kernel(name)
        call g%bounds(lo, hi)
        call g%grid(c)
        call check(error, g%is_initialised() .and. g%ncells() == 50 .and. g%step() == 0.1_real64 .and. &
            g%bandwidth() == 0.2_real64 .and. name == "epanechnikov" .and. lo == -1.0_real64 .and. &
            hi == 4.0_real64 .and. (.not. g%is_adaptive()) .and. abs(c(1) + 0.95_real64) <= 1.0e-15_real64 .and. &
            abs(c(50) - 3.95_real64) <= 1.0e-15_real64, "the accessors must report the set-up")
        if (allocated(error)) return
        call g%add([0.0_real64, 1.0_real64, 2.0_real64])
        call g%clear()
        call g%density(f)
        call check(error, g%is_initialised() .and. g%ncells() == 50 .and. g%bandwidth() == 0.2_real64 .and. &
            g%n() == 0_int64 .and. all(f == 0.0_real64), "%clear must empty the grid and keep its geometry")
        if (allocated(error)) return
        call g%init(5, 0.0_real64, 1.0_real64, 0.5_real64)
        call g%bounds(lo, hi)
        call check(error, g%ncells() == 5 .and. .not. ieee_is_finite(lo) .and. lo < 0.0_real64 .and. &
            .not. ieee_is_finite(hi) .and. hi > 0.0_real64, &
            "a second %init must replace the geometry, and an unbounded support reads as infinite")

    end subroutine test_grid_accessors_and_clear

    !> `%print` writes a heading and its rows; an empty grid adds a line saying so, and an
    !> uninitialised one prints one line rather than aborting.
    subroutine test_grid_print_writes(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde_grid) :: g, fresh
        integer :: u, nlines
        logical :: seen
        character(len=*), parameter :: PATH = "test_run/kde_grid_print_writes.txt"

        call g%init(10, 0.0_real64, 1.0_real64, 0.1_real64, lower=0.0_real64)
        call g%add([0.2_real64, 0.5_real64])
        open(newunit=u, file=PATH, status="replace", action="write")
        call g%print(unit=u)
        close(u)
        call read_back(PATH, nlines, "sum_weights", seen)
        ! The heading, twelve rows, the lower bound and the correction.
        call check(error, nlines == 15 .and. seen, "%print must write a heading and fourteen rows")
        if (allocated(error)) return
        call g%clear()
        open(newunit=u, file=PATH, status="replace", action="write")
        call g%print(unit=u)
        close(u)
        call read_back(PATH, nlines, "nothing accumulated", seen)
        call check(error, nlines == 16 .and. seen, "an empty grid must say so")
        if (allocated(error)) return
        open(newunit=u, file=PATH, status="replace", action="write")
        call fresh%print(unit=u)
        close(u)
        call read_back(PATH, nlines, "not initialised", seen)
        call check(error, nlines == 1 .and. seen, "an uninitialised grid must print one line saying so")

    end subroutine test_grid_print_writes

    !> `verbosity = "silent"` silences the grid's `%print` too; the negative control is the same
    !> call writing its rows first. Writes the process-global setting, hence the serial suite.
    subroutine test_grid_print_silenced(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde_grid) :: g
        integer :: u, nlines
        logical :: seen
        character(len=:), allocatable :: saved
        character(len=*), parameter :: PATH = "test_run/kde_grid_print_silenced.txt"

        call g%init(10, 0.0_real64, 1.0_real64, 0.1_real64)
        call g%add([0.2_real64, 0.5_real64])
        open(newunit=u, file=PATH, status="replace", action="write")
        call g%print(unit=u)
        close(u)
        call read_back(PATH, nlines, "bandwidth", seen)
        call check(error, nlines > 0 .and. seen, "the control: %print must write when not silenced")
        if (allocated(error)) return
        call parquet_get_verbosity(saved)
        call parquet_set_verbosity("silent")
        open(newunit=u, file=PATH, status="replace", action="write")
        call g%print(unit=u)
        close(u)
        call parquet_set_verbosity(saved)
        call read_back(PATH, nlines, "bandwidth", seen)
        call check(error, nlines == 0, 'verbosity="silent" must silence the grid''s %print entirely')

    end subroutine test_grid_print_silenced

    !> `verbosity = "silent"` silences `%print` entirely; the negative control is the same call
    !> writing its rows first. Writes the process-global setting, hence the serial suite.
    subroutine test_print_silenced(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde) :: k
        real(real64), allocatable :: x(:)
        integer :: u, nlines
        logical :: seen
        character(len=:), allocatable :: saved
        character(len=*), parameter :: PATH = "test_run/kde_print_silenced.txt"

        call kde_fixture(20_int64, x)
        call k%fit(x, bandwidth=5.0_real64)
        open(newunit=u, file=PATH, status="replace", action="write")
        call k%print(unit=u)
        close(u)
        call read_back(PATH, nlines, "bandwidth", seen)
        call check(error, nlines > 0 .and. seen, "the control: %print must write when not silenced")
        if (allocated(error)) return
        call parquet_get_verbosity(saved)
        call parquet_set_verbosity("silent")
        open(newunit=u, file=PATH, status="replace", action="write")
        call k%print(unit=u)
        close(u)
        call parquet_set_verbosity(saved)
        call read_back(PATH, nlines, "bandwidth", seen)
        call check(error, nlines == 0, 'verbosity="silent" must silence %print entirely')

    end subroutine test_print_silenced

    ! ==========================================================================================
    ! The adaptive kernel
    ! ==========================================================================================

    !> Compares one adaptive golden case: `ok`, the global bandwidth, and the density and CDF at
    !> every probe, the density to `rel_pdf` of its peak and the CDF to `abs_cdf`.
    subroutine check_adaptive_case(error, name, h, pdf, cdf, rel_pdf, abs_cdf)
        type(error_type), allocatable, intent(out) :: error    !! set on the first failed check
        character(len=*), intent(in)               :: name     !! the case
        real(real64), intent(in)                   :: h        !! the global bandwidth
        real(real64), intent(in)                   :: pdf(NKX) !! the density at `KG_X`
        real(real64), intent(in)                   :: cdf(NKX) !! the CDF at `KG_X`
        real(real64), intent(in)                   :: rel_pdf  !! the density's tolerance, over its peak
        real(real64), intent(in)                   :: abs_cdf  !! the CDF's tolerance
        type(pf_kde) :: k
        logical :: ok
        real(real64) :: got_pdf(NKX), got_cdf(NKX)
        character(len=200) :: msg

        call fit_golden_case(name, k, ok)
        call check(error, ok .and. k%is_adaptive(), name // ": the adaptive fit must be defined")
        if (allocated(error)) return
        call check(error, close_to(k%bandwidth(), h, 1.0e-13_real64, h), &
            name // ": the global bandwidth must match the oracle")
        if (allocated(error)) return
        call k%pdf(KG_X, got_pdf)
        call k%cdf(KG_X, got_cdf)
        write(msg, '(2a,es10.3,a,es10.3)') name, ": the density misses the oracle by ", &
            maxval(abs(got_pdf - pdf))/maxval(pdf), " of its peak; allowed ", rel_pdf
        call check(error, maxval(abs(got_pdf - pdf)) <= rel_pdf*maxval(pdf), trim(msg))
        if (allocated(error)) return
        write(msg, '(2a,es10.3,a,es10.3)') name, ": the CDF misses the oracle by ", &
            maxval(abs(got_cdf - cdf)), "; allowed ", abs_cdf
        call check(error, maxval(abs(got_cdf - cdf)) <= abs_cdf, trim(msg))

    end subroutine check_adaptive_case

    !> `log g` recomputed from a pilot grid's own cells: the mean of `log p` over its density, as
    !> the adaptive rule defines it, formed here by this test and not by the library.
    function pilot_log_g(g) result(lg)
        type(pf_kde_grid), intent(in) :: g  !! the pilot
        real(real64)                  :: lg !! `log g`
        real(real64), allocatable :: f(:)
        real(real64) :: s1, s2
        integer :: i

        allocate(f(g%ncells()))
        call g%density(f)
        s1 = 0.0_real64
        s2 = 0.0_real64
        do i = 1, size(f)
            if (f(i) > 0.0_real64) then
                s1 = s1 + f(i)
                s2 = s2 + f(i)*log(f(i))
            end if
        end do
        lg = s2/s1

    end function pilot_log_g

    !> `alpha = 0` makes every point's bandwidth the global one, and the adaptive estimate is then
    !> the fixed one BIT FOR BIT -- density, distribution function and quantiles -- unbounded, under
    !> "renormalise", and under "reflect" with a kernel wider than the support, where each point's
    !> mass is divided out; the streaming form's accumulation likewise. `alpha = 0.5` moves the
    !> estimate, so an adaptive path that silently fell back on the fixed one is caught.
    subroutine test_alpha_zero_is_fixed(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde) :: kf, k0, k5
        type(pf_kde_grid) :: pilot, gf, g0
        real(real64), allocatable :: x(:), hb(:), rf(:), r0(:)
        real(real64) :: t(301), ff(301), f0(301), f5(301), cf(301), c0(301), qf(9), q0(9), pp(9)
        integer :: cfg, j
        character(len=12) :: kern
        character(len=100) :: tag

        call kde_two_component(60_int64, x)
        call spread_points(-700.0_real64, 800.0_real64, t)
        do j = 1, 9
            pp(j) = real(j, real64)/10.0_real64
        end do
        do cfg = 1, 4
            select case (cfg)
            case (1)
                tag = "unbounded, Silverman's rule, Gaussian"
                call kf%fit(x, rule="silverman")
                call k0%fit(x, rule="silverman", adaptive=.true., alpha=0.0_real64)
                call k5%fit(x, rule="silverman", adaptive=.true., alpha=0.5_real64)
            case (2)
                tag = "renormalised at a lower bound, B-spline"
                call kf%fit(x, bandwidth=80.0_real64, kernel="bspline", lower=-340.0_real64)
                call k0%fit(x, bandwidth=80.0_real64, kernel="bspline", adaptive=.true., alpha=0.0_real64, &
                    lower=-340.0_real64)
                call k5%fit(x, bandwidth=80.0_real64, kernel="bspline", adaptive=.true., lower=-340.0_real64)
            case (3)
                tag = "reflected at both bounds, Epanechnikov"
                call kf%fit(x, bandwidth=60.0_real64, kernel="epanechnikov", lower=-340.0_real64, &
                    upper=470.0_real64, boundary="reflect")
                call k0%fit(x, bandwidth=60.0_real64, kernel="epanechnikov", adaptive=.true., alpha=0.0_real64, &
                    lower=-340.0_real64, upper=470.0_real64, boundary="reflect")
                call k5%fit(x, bandwidth=60.0_real64, kernel="epanechnikov", adaptive=.true., &
                    lower=-340.0_real64, upper=470.0_real64, boundary="reflect")
            case (4)
                tag = "reflected with the kernel wider than the support"
                call kf%fit(x, bandwidth=400.0_real64, lower=-340.0_real64, upper=470.0_real64, boundary="reflect")
                call k0%fit(x, bandwidth=400.0_real64, adaptive=.true., alpha=0.0_real64, lower=-340.0_real64, &
                    upper=470.0_real64, boundary="reflect")
                call k5%fit(x, bandwidth=400.0_real64, adaptive=.true., lower=-340.0_real64, &
                    upper=470.0_real64, boundary="reflect")
            end select
            call kf%pdf(t, ff)
            call k0%pdf(t, f0)
            call k5%pdf(t, f5)
            call kf%cdf(t, cf)
            call k0%cdf(t, c0)
            call kf%quantile(pp, qf)
            call k0%quantile(pp, q0)
            call check(error, all(f0 == ff) .and. all(c0 == cf) .and. all(q0 == qf), &
                trim(tag) // ": alpha = 0 must answer the fixed estimate bit for bit")
            if (allocated(error)) return
            if (allocated(hb)) deallocate(hb)
            allocate(hb(k0%n_valid()))
            call k0%bandwidths(hb)
            call check(error, all(hb == kf%bandwidth()), trim(tag) // ": at alpha = 0 every bandwidth is the global one")
            if (allocated(error)) return
            call check(error, maxval(abs(f5 - ff)) > 1.0e-2_real64*maxval(ff), &
                trim(tag) // ": alpha = 0.5 must move the estimate; the adaptive path has fallen back on the fixed")
            if (allocated(error)) return
        end do

        ! The streaming form: a pilot at alpha = 0 deposits every point as the fixed grid does.
        do cfg = 1, 2
            kern = "gaussian"
            if (cfg == 2) kern = "bspline"
            call pilot%init(64, -700.0_real64, 800.0_real64, 60.0_real64, kernel=kern)
            call pilot%add(x)
            call gf%init(150, -650.0_real64, 750.0_real64, 50.0_real64, kernel=kern)
            call g0%init(150, -650.0_real64, 750.0_real64, 50.0_real64, kernel=kern, pilot=pilot, &
                alpha=0.0_real64)
            call gf%add(x)
            call g0%add(x)
            call raw_cells(gf, rf)
            call raw_cells(g0, r0)
            call gf%cdf(t, cf)
            call g0%cdf(t, c0)
            call check(error, g0%is_adaptive() .and. all(r0 == rf) .and. all(c0 == cf), &
                trim(kern) // ": a grid whose pilot rule has alpha = 0 must accumulate the fixed grid's bits")
            if (allocated(error)) return
        end do

    end subroutine test_alpha_zero_is_fixed

    !> On the two-component recipe the rule narrows the kernels in the dense cluster and widens
    !> them in the sparse one: every bandwidth in the narrow cluster is below the global one and
    !> every one in the wide cluster above it, and the gap between the clusters, where the pilot is
    !> thinnest, gets a wider bandwidth still. A sign error on `alpha`, or a `g` off by much, turns
    !> one of the three around.
    subroutine test_adaptive_bandwidths_follow_pilot(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde) :: k
        real(real64), allocatable :: x(:), hb(:), xs(:)
        real(real64) :: h, h_gap
        character(len=200) :: msg

        call kde_two_component(60_int64, x)
        call k%fit(x, rule="silverman", adaptive=.true.)
        h = k%bandwidth()
        allocate(hb(k%n_valid()), xs(k%n_valid()))
        call k%bandwidths(hb, xs)
        call check(error, count(xs < 0.0_real64) == 40 .and. count(xs > 0.0_real64) == 20, &
            "the fixture must hold forty points in the narrow cluster and twenty in the wide one")
        if (allocated(error)) return
        write(msg, '(a,f8.3,a,f8.3)') "the narrow cluster's widest bandwidth over the global one is ", &
            maxval(hb, mask=xs < 0.0_real64)/h, "; the wide cluster's narrowest ", minval(hb, mask=xs > 0.0_real64)/h
        call check(error, maxval(hb, mask=xs < 0.0_real64) < h .and. minval(hb, mask=xs > 0.0_real64) > h, trim(msg))
        if (allocated(error)) return
        call k%bandwidth_at(0.0_real64, h_gap)
        call check(error, h_gap > maxval(hb), "the gap between the clusters must get the widest bandwidth of all")

    end subroutine test_adaptive_bandwidths_follow_pilot

    !> The four adaptive golden cases, at the rule's own pilot: the library reads a pilot grid a
    !> quarter of a bandwidth to the cell, whose interpolation errs by about `(step/h)**2/8` of the
    !> pilot, a bandwidth by `alpha` times that, so the estimate agrees with the exact pilot's to
    !> about `2e-3` of its peak -- asserted at `5e-3`, and the CDF at `5e-4`. `kde_serial`'s fine
    !> case shows the gap is that discretisation and nothing else.
    subroutine test_adaptive_golden(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check

        call check_adaptive_case(error, "ADAPT", KG_ADAPT_H, KG_ADAPT_PDF, KG_ADAPT_CDF, 5.0e-3_real64, &
            5.0e-4_real64)
        if (allocated(error)) return
        call check_adaptive_case(error, "ADAPT_CAP", KG_ADAPT_CAP_H, KG_ADAPT_CAP_PDF, KG_ADAPT_CAP_CDF, &
            5.0e-3_real64, 5.0e-4_real64)
        if (allocated(error)) return
        call check_adaptive_case(error, "ADAPT_REF", KG_ADAPT_REF_H, KG_ADAPT_REF_PDF, KG_ADAPT_REF_CDF, &
            5.0e-3_real64, 5.0e-4_real64)
        if (allocated(error)) return
        call check_adaptive_case(error, "ADAPT_W", KG_ADAPT_W_H, KG_ADAPT_W_PDF, KG_ADAPT_W_CDF, 5.0e-3_real64, &
            5.0e-4_real64)

    end subroutine test_adaptive_golden

    !> `bandwidth_max` caps every point's bandwidth and touches no other: where the uncapped rule
    !> stays below the cap the two fits agree bit for bit, everywhere else the capped one is the cap
    !> exactly, and the estimates then differ. A point the pilot reads as zero takes the cap itself.
    subroutine test_bandwidth_max_caps(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde) :: k, kc
        real(real64), allocatable :: x(:), h0(:), hc(:)
        real(real64) :: cap, f0(50), fc(50), t(50), far
        integer :: i

        call kde_two_component(60_int64, x)
        call k%fit(x, rule="silverman", adaptive=.true.)
        cap = 1.2_real64*k%bandwidth()
        call kc%fit(x, rule="silverman", adaptive=.true., bandwidth_max=cap)
        allocate(h0(k%n_valid()), hc(kc%n_valid()))
        call k%bandwidths(h0)
        call kc%bandwidths(hc)
        call check(error, count(h0 > cap) >= 3 .and. count(h0 < cap) >= 3, &
            "the cap must bind on some points and not on others, or this test is vacuous")
        if (allocated(error)) return
        do i = 1, size(h0)
            if (h0(i) < cap) then
                call check(error, hc(i) == h0(i), "a bandwidth below the cap must be left as it is")
            else
                call check(error, hc(i) == cap, "a bandwidth above the cap must be the cap exactly")
            end if
            if (allocated(error)) return
        end do
        call spread_points(-400.0_real64, 500.0_real64, t)
        call k%pdf(t, f0)
        call kc%pdf(t, fc)
        call check(error, maxval(abs(fc - f0)) > 1.0e-3_real64*maxval(f0), "the cap must change the estimate")
        if (allocated(error)) return
        call kc%bandwidth_at(1.0e4_real64, far)
        call check(error, far == cap, "a point the pilot reads as zero must take the cap")

    end subroutine test_bandwidth_max_caps

    !> `%pilot` hands back the grid the fit read: at the global bandwidth, over the survivors, from
    !> four bandwidths below the lowest point to four above the highest, with a quarter of a
    !> bandwidth to the cell. Every bandwidth `%bandwidths` reports is recomputed from it by this
    !> test -- the pilot interpolated at the point, `log g` its mean `log` density -- and agrees;
    !> so does the stand-in for a point the pilot reads as zero, its smallest positive density.
    subroutine test_pilot_is_the_fits(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde) :: k, kempty
        type(pf_kde_grid) :: g
        real(real64), allocatable :: x(:), hb(:), xs(:), c(:), f(:)
        real(real64) :: h, lg, p, want, far, pmin, a, b, empty(0)
        integer :: j, nc

        call kde_two_component(60_int64, x)
        call k%fit(x, bandwidth=40.0_real64, adaptive=.true.)
        h = k%bandwidth()
        call k%pilot(g)
        allocate(hb(k%n_valid()), xs(k%n_valid()))
        call k%bandwidths(hb, xs)
        ! Its own check first: every accessor below aborts on a grid never initialised.
        call check(error, g%is_initialised(), "an adaptive fit's %pilot must hand back an initialised grid")
        if (allocated(error)) return
        call check(error, .not. g%is_adaptive() .and. g%bandwidth() == h .and. &
            g%n_valid() == k%n_valid() .and. g%sum_weights() == k%sum_weights(), &
            "the pilot must be a fixed grid at the global bandwidth over the survivors")
        if (allocated(error)) return
        a = xs(1) - 4.0_real64*h
        b = xs(size(xs)) + 4.0_real64*h
        nc = ceiling(4.0_real64*((b - a)/h))
        allocate(c(g%ncells()), f(g%ncells()))
        call g%grid(c)
        call check(error, g%ncells() == nc .and. abs(c(1) - 0.5_real64*g%step() - a) <= 1.0e-12_real64*abs(a) &
            .and. abs(g%step() - (b - a)/nc) <= 1.0e-12_real64*g%step(), &
            "the pilot must span four bandwidths beyond the data, a quarter of a bandwidth to the cell")
        if (allocated(error)) return

        lg = pilot_log_g(g)
        do j = 1, size(xs)
            call g%pdf(xs(j), p)
            want = h*exp(-0.5_real64*(log(p) - lg))
            call check(error, abs(hb(j) - want) <= 1.0e-12_real64*want, &
                "every bandwidth must be h*(p(x)/g)**(-1/2) read from the pilot %pilot returns")
            if (allocated(error)) return
        end do
        call g%density(f)
        pmin = minval(f, mask=f > 0.0_real64)
        call k%bandwidth_at(1.0e4_real64, far)
        want = h*exp(-0.5_real64*(log(pmin) - lg))
        call check(error, abs(far - want) <= 1.0e-12_real64*want, &
            "a point the pilot reads as zero must take its smallest positive density instead")
        if (allocated(error)) return

        call kempty%fit(empty, adaptive=.true.)
        call kempty%pilot(g)
        call check(error, kempty%is_adaptive() .and. .not. g%is_initialised(), &
            "an adaptive fit with nothing to build a pilot from must hand back an uninitialised grid")

    end subroutine test_pilot_is_the_fits

    !> The pilot's cells: a quarter of a bandwidth over its range, never fewer than 64 -- a sample
    !> narrower than sixteen bandwidths -- and never more than 65536 -- a far outlier -- with the
    !> range clipped to the support.
    subroutine test_pilot_cells_rule(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde) :: k
        type(pf_kde_grid) :: g
        real(real64) :: c(64)

        call k%fit([0.0_real64, 1.0_real64, 2.0_real64], bandwidth=10.0_real64, adaptive=.true.)
        call k%pilot(g)
        call check(error, g%ncells() == 64, "a range of about eight bandwidths must get the 64-cell floor")
        if (allocated(error)) return
        call k%fit([0.0_real64, 0.5_real64, 1.0_real64, 1000.0_real64], bandwidth=0.01_real64, adaptive=.true.)
        call k%pilot(g)
        call check(error, g%ncells() == 65536, "a far outlier must meet the 65536-cell ceiling")
        if (allocated(error)) return
        call k%fit([0.1_real64, 0.2_real64, 0.9_real64], bandwidth=0.1_real64, adaptive=.true., lower=0.0_real64, &
            upper=1.0_real64)
        call k%pilot(g)
        call g%grid(c)
        call check(error, g%ncells() == 64 .and. abs(c(1) - 0.5_real64/64.0_real64) <= 1.0e-15_real64 .and. &
            abs(c(64) - 127.0_real64/128.0_real64) <= 1.0e-15_real64, &
            "a pilot reaching past the support must be clipped to it")

    end subroutine test_pilot_cells_rule

    !> A `pf_kde_grid` given the fit's own pilot, with the fit's bandwidth and `alpha`, gives every
    !> point the bandwidth `%fit` gave it, so the grid converges to the fit's adaptive estimate as
    !> the square of its cell width -- the fixed form's convergence test, adaptive. `g` or `lambda`
    !> formed differently in the two forms leaves an error that does not fall.
    subroutine test_two_forms_agree_under_one_pilot(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde) :: k
        type(pf_kde_grid) :: p, g
        real(real64), allocatable :: x(:)
        real(real64) :: t(997), fx(997), fg(997), e(2), h, lo, hi
        integer :: kk, r
        character(len=200) :: msg

        call kde_two_component(60_int64, x)
        h = 30.0_real64
        lo = minval(x) - 3.5_real64*h
        hi = maxval(x) + 3.5_real64*h
        call spread_points(minval(x), maxval(x), t)
        do kk = 1, 3, 2
            call k%fit(x, bandwidth=h, kernel=KERNELS(kk), adaptive=.true.)
            call k%pilot(p)
            call k%pdf(t, fx)
            do r = 1, 2
                call g%init(400*r, lo, hi, h, kernel=KERNELS(kk), pilot=p)
                call g%add(x)
                call g%pdf(t, fg)
                e(r) = rms(fg - fx)
            end do
            write(msg, '(3a,f8.4)') "adaptive ", trim(KERNELS(kk)), &
                ": halving the cells must divide the RMS error by four; the ratio is ", e(1)/e(2)
            call check(error, e(1)/e(2) > 3.6_real64 .and. e(1)/e(2) < 4.4_real64, trim(msg))
            if (allocated(error)) return
        end do

    end subroutine test_two_forms_agree_under_one_pilot

    !> Every point's kernel keeps unit mass inside the support at its own bandwidth: the adaptive
    !> estimate integrates to one under both corrections (`pf_integrate`, which shares nothing with
    !> it), its `%cdf` reaches one just inside the upper bound, and an adaptive grid deposits
    !> exactly its weight. A mass formed at the global bandwidth instead of the point's own loses
    !> or invents mass wherever the two differ.
    subroutine test_adaptive_conserves_mass(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(kde_density) :: fn
        type(pf_kde_grid) :: p, g
        real(real64), allocatable :: z(:), r(:)
        real(real64) :: mass, c_top
        character(len=11), parameter :: METHODS(2) = [character(len=11) :: "renormalise", "reflect"]
        integer :: mm
        character(len=160) :: msg

        call bounded_fixture(300_int64, z)
        do mm = 1, 2
            ! The B-spline, twice continuously differentiable, so that the quadrature needs no
            ! breakpoint at each of six hundred kernel edges.
            call fn%k%fit(z, bandwidth=0.06_real64, kernel="bspline", adaptive=.true., lower=0.0_real64, &
                upper=1.0_real64, boundary=METHODS(mm))
            fn%power = 0
            mass = pf_integrate(fn, 0.0_real64, 1.0_real64, 1.0e-10_real64)
            write(msg, '(2a,es10.3)') trim(METHODS(mm)), ": the adaptive estimate must integrate to one; it misses by ", &
                mass - 1.0_real64
            call check(error, abs(mass - 1.0_real64) <= 1.0e-9_real64, trim(msg))
            if (allocated(error)) return
            call fn%k%cdf(nearest(1.0_real64, -1.0_real64), c_top)
            call check(error, abs(c_top - 1.0_real64) <= 1.0e-12_real64, &
                trim(METHODS(mm)) // ": %cdf must reach one just inside the upper bound")
            if (allocated(error)) return
            call fn%k%pilot(p)
            call g%init(200, 0.0_real64, 1.0_real64, 0.06_real64, kernel="bspline", pilot=p, lower=0.0_real64, &
                upper=1.0_real64, boundary=METHODS(mm))
            call g%add(z)
            call raw_cells(g, r)
            call check(error, abs(sum(r)*g%step()/g%sum_weights() - 1.0_real64) <= 1.0e-13_real64, &
                trim(METHODS(mm)) // ": an adaptive grid must deposit exactly its weight")
            if (allocated(error)) return
        end do

    end subroutine test_adaptive_conserves_mass

    !> `%bandwidth_at` is the look-up `%fit` used: at the retained points it answers `%bandwidths`
    !> bit for bit, the scalar form answers the array form's element, and it is NaN at a NaN point
    !> and on an undefined estimate. A fixed fit answers its one bandwidth everywhere.
    subroutine test_bandwidth_at(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde) :: k, kf, ku
        real(real64), allocatable :: x(:), hb(:), xs(:), ha(:)
        real(real64) :: one, nan_h, fixed(3), w(60)
        logical :: ok
        integer :: i

        call kde_two_component(60_int64, x)
        do i = 1, 60
            w(i) = real(1 + mod(i, 3), real64)
        end do
        call k%fit(x, rule="silverman", adaptive=.true., weights=w)
        allocate(hb(k%n_valid()), xs(k%n_valid()), ha(k%n_valid()))
        call k%bandwidths(hb, xs)
        call k%bandwidth_at(xs, ha)
        call check(error, all(ha == hb), "%bandwidth_at at the retained points must be %bandwidths, bit for bit")
        if (allocated(error)) return
        call k%bandwidth_at(xs(7), one)
        call check(error, one == hb(7), "the scalar form must answer the array form's element")
        if (allocated(error)) return
        call k%bandwidth_at(ieee_value(1.0_real64, ieee_quiet_nan), nan_h)
        call check(error, ieee_is_nan(nan_h), "%bandwidth_at must be NaN at a NaN point")
        if (allocated(error)) return
        call kf%fit(x, bandwidth=25.0_real64)
        call kf%bandwidth_at([-300.0_real64, 0.0_real64, 300.0_real64], fixed)
        call check(error, all(fixed == 25.0_real64), "a fixed fit must answer its one bandwidth everywhere")
        if (allocated(error)) return
        call ku%fit([3.0_real64, 3.0_real64], rule="silverman", adaptive=.true., ok=ok)
        call ku%bandwidth_at(3.0_real64, one)
        call check(error, (.not. ok) .and. ieee_is_nan(one), "an undefined adaptive estimate must answer NaN")

    end subroutine test_bandwidth_at

    !> The streaming form's adaptive rule: `pilot=` copies the pilot, so clearing the caller's
    !> afterwards changes nothing; a grid over all but the last point merged with one over that
    !> point is the grid over them all, to the bit, when both read one pilot; and a poisoned pilot
    !> poisons the grid built on it, quietly and for good.
    subroutine test_adaptive_grid(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde_grid) :: p, pp, g, g_all, g1, g2, gp, gf
        real(real64), allocatable :: x(:), r(:), r_all(:), before(:)
        real(real64) :: f(100)

        call kde_two_component(60_int64, x)
        call p%init(64, -700.0_real64, 800.0_real64, 50.0_real64)
        call p%add(x)
        call g%init(100, -600.0_real64, 700.0_real64, 50.0_real64, pilot=p, alpha=0.7_real64)
        call gf%init(100, -600.0_real64, 700.0_real64, 50.0_real64)
        call check(error, g%is_adaptive() .and. .not. gf%is_adaptive(), "pilot= must make the grid adaptive")
        if (allocated(error)) return
        call g%add(x)
        call raw_cells(g, before)
        call p%clear()
        call g%clear()
        call g%add(x)
        call raw_cells(g, r)
        call check(error, all(r == before), "the pilot must be copied: clearing the caller's must change nothing")
        if (allocated(error)) return

        call p%add(x)
        call g_all%init(100, -600.0_real64, 700.0_real64, 50.0_real64, pilot=p, alpha=0.7_real64)
        call g_all%add(x)
        call g1%init(100, -600.0_real64, 700.0_real64, 50.0_real64, pilot=p, alpha=0.7_real64)
        call g1%add(x(1:59))
        call g2%init(100, -600.0_real64, 700.0_real64, 50.0_real64, pilot=p, alpha=0.7_real64)
        call g2%add(x(60:60))
        call g1%merge(g2)
        call raw_cells(g_all, r_all)
        call raw_cells(g1, r)
        call check(error, all(r == r_all) .and. g1%n() == 60_int64, &
            "two adaptive grids reading one pilot must merge into the grid over all their points, to the bit")
        if (allocated(error)) return

        ! Poisoned after it had accumulated the sample, so that its cells hold weight and every
        ! bandwidth read from them would be finite and wrong: only the poison stops the deposit.
        call pp%init(64, -700.0_real64, 800.0_real64, 50.0_real64)
        call pp%add(x)
        call pp%add([ieee_value(1.0_real64, ieee_quiet_nan)], skipnan=.false.)
        call gp%init(100, -600.0_real64, 700.0_real64, 50.0_real64, pilot=pp)
        call gp%density(f)
        call check(error, all(ieee_is_nan(f)), "a grid on a poisoned pilot must answer NaN before any %add")
        if (allocated(error)) return
        call gp%add(x)
        call gp%density(f)
        call check(error, all(ieee_is_nan(f)), "a poisoned pilot must poison the grid built on it")
        if (allocated(error)) return
        call gp%clear()
        call gp%add(x)
        call gp%density(f)
        call check(error, all(ieee_is_nan(f)), "clearing the grid must not clear its pilot's poison")

    end subroutine test_adaptive_grid

    !> A pilot with no density to read is data, not a mistake, so `%init` accepts it and every answer
    !> of the grid built on it is NaN -- before and after `%add`, `%clear()` or not -- with every
    !> count still kept. Two such pilots: one never given a point, and one whose only point lies
    !> beyond its range, so that it holds weight but none in its cells. The control, a pilot with
    !> points in its cells, gives the same grid finite answers.
    subroutine test_empty_pilot_is_quiet(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde_grid) :: empty, beyond, full, g
        real(real64) :: f(4), fp(8), pdf, cdf, q
        integer :: which

        call empty%init(8, 0.0_real64, 1.0_real64, 0.1_real64)
        call beyond%init(8, 0.0_real64, 1.0_real64, 0.01_real64)
        call beyond%add([5.0_real64])
        call beyond%density(fp)
        call check(error, beyond%sum_weights() == 1.0_real64 .and. all(fp == 0.0_real64), &
            "the fixture: a pilot whose one point lies beyond its range holds its weight and none in its cells")
        if (allocated(error)) return
        call full%init(8, 0.0_real64, 1.0_real64, 0.1_real64)
        call full%add([0.1_real64, 0.2_real64, 0.3_real64, 0.4_real64])

        call g%init(4, 0.0_real64, 1.0_real64, 0.1_real64, pilot=full)
        call g%add([0.25_real64, 0.5_real64])
        call g%density(f)
        call check(error, g%is_adaptive() .and. .not. any(ieee_is_nan(f)), &
            "the control: a grid on a pilot with points in its cells must answer finite densities")
        if (allocated(error)) return

        do which = 1, 2
            if (which == 1) then
                call g%init(4, 0.0_real64, 1.0_real64, 0.1_real64, pilot=empty, bandwidth_max=0.5_real64)
            else
                call g%init(4, 0.0_real64, 1.0_real64, 0.1_real64, pilot=beyond)
            end if
            call g%density(f)
            call check(error, g%is_adaptive() .and. all(ieee_is_nan(f)), &
                "a grid on a pilot with nothing to read must be adaptive and answer NaN before any %add")
            if (allocated(error)) return
            call g%add([0.25_real64, 0.5_real64])
            call g%density(f)
            call g%pdf(0.5_real64, pdf)
            call g%cdf(0.5_real64, cdf)
            call g%quantile(0.5_real64, q)
            call check(error, all(ieee_is_nan(f)) .and. ieee_is_nan(pdf) .and. ieee_is_nan(cdf) .and. &
                ieee_is_nan(q) .and. g%n_valid() == 2_int64 .and. g%sum_weights() == 2.0_real64, &
                "a grid on a pilot with nothing to read must answer NaN and still count what %add accepted")
            if (allocated(error)) return
            call g%clear()
            call g%add([0.5_real64])
            call g%density(f)
            call g%sample(fp, 3_int64)
            call check(error, all(ieee_is_nan(f)) .and. all(ieee_is_nan(fp)), &
                "clearing the grid must not give it a pilot to read, nor its %sample a density to draw")
            if (allocated(error)) return
        end do

    end subroutine test_empty_pilot_is_quiet

    !> The adaptive fit's state through its accessors: `%is_adaptive`, the printer's extra rows,
    !> `%clear`, a fixed refit (every bandwidth the global one again), and the `real32` form, which
    !> forwards `adaptive`, `alpha` and `bandwidth_max` and equals the `real64` form over the
    !> widened values bit for bit.
    subroutine test_adaptive_accessors(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde) :: k, k64
        real(real64), allocatable :: x(:), hb(:), hb64(:), xw(:)
        real(real32), allocatable :: x32(:)
        real(real64) :: t(40), f(40), f64(40)
        integer :: u, nlines
        logical :: seen
        character(len=*), parameter :: PATH = "test_run/kde_adaptive_print.txt"

        call kde_two_component(60_int64, x)
        call k%fit(x, rule="silverman", adaptive=.true., alpha=0.3_real64, bandwidth_max=150.0_real64)
        call check(error, k%is_adaptive(), "an adaptive fit must say so")
        if (allocated(error)) return
        open(newunit=u, file=PATH, status="replace", action="write")
        call k%print(unit=u)
        close(u)
        call read_back(PATH, nlines, "bandwidth_max", seen)
        ! The heading and the fixed summary's nine rows, then alpha, the cap, the pilot and the two
        ! ends of the bandwidths.
        call check(error, nlines == 15 .and. seen, "%print must add the adaptive rule's five rows")
        if (allocated(error)) return

        call k%clear()
        call check(error, .not. k%is_fitted(), "%clear must unfit an adaptive estimate")
        if (allocated(error)) return
        call k%fit(x, bandwidth=30.0_real64)
        allocate(hb(k%n_valid()))
        call k%bandwidths(hb)
        call check(error, (.not. k%is_adaptive()) .and. all(hb == 30.0_real64), &
            "a fixed refit must leave nothing of the adaptive rule behind")
        if (allocated(error)) return

        x32 = real(x, real32)
        xw = real(x32, real64)
        ! At alpha = 0.3 the widest bandwidth is about 1.17 of the global one, 133: a cap of 125 binds.
        call k%fit(x32, rule="silverman", adaptive=.true., alpha=0.3_real64, bandwidth_max=125.0_real64)
        call k64%fit(xw, rule="silverman", adaptive=.true., alpha=0.3_real64, bandwidth_max=125.0_real64)
        deallocate(hb)
        allocate(hb(k%n_valid()), hb64(k64%n_valid()))
        call k%bandwidths(hb)
        call k64%bandwidths(hb64)
        call spread_points(-400.0_real64, 500.0_real64, t)
        call k%pdf(t, f)
        call k64%pdf(t, f64)
        call check(error, maxval(hb) == 125.0_real64, "the real32 form must forward the cap, which binds here")
        if (allocated(error)) return
        call check(error, all(hb == hb64) .and. all(f == f64), &
            "the real32 form must forward the adaptive settings and equal the real64 form, bit for bit")

    end subroutine test_adaptive_accessors

    !> A bandwidth the estimate cannot use is an undefined estimate, never an overflow: one whose
    !> kernel would reach beyond the largest number, and an explicit bandwidth whose product with
    !> `adjust` would overflow, are both `ok = .false.` with every answer NaN.
    subroutine test_unusable_bandwidth(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde) :: k
        real(real64) :: f
        logical :: ok

        call k%fit([1.0_real64, 2.0_real64], bandwidth=0.5_real64*huge(1.0_real64), ok=ok)
        call k%pdf(1.0_real64, f)
        call check(error, (.not. ok) .and. ieee_is_nan(f) .and. ieee_is_nan(k%bandwidth()), &
            "a Gaussian whose reach would pass the largest number must leave the estimate undefined")
        if (allocated(error)) return
        call k%fit([1.0_real64, 2.0_real64], bandwidth=1.0e300_real64, adjust=1.0e10_real64, ok=ok)
        call check(error, (.not. ok) .and. ieee_is_nan(k%bandwidth()), &
            "a bandwidth times adjust past the largest number must leave the estimate undefined")
        if (allocated(error)) return
        call k%fit([1.0_real64, 2.0_real64], bandwidth=0.1_real64*huge(1.0_real64), kernel="box", ok=ok)
        call check(error, ok, "the control: a box whose reach stays finite must be defined")

    end subroutine test_unusable_bandwidth

    !> `%sample` is addressed by `(seed, stream, k)`, and every element follows one recipe: the stream
    !> `pf_random_key(stream, k)` under `pf_random_key(seed, label)`, whose first draw chooses the
    !> point and whose next draws the kernel's variate. Reproduced here from the generator on a
    !> two-point box fit, unweighted (`pf_random_int_at` chooses) and weighted (a uniform share of the
    !> total weight, located in the running weight), beside a coupled control arm that drops the
    !> label and must match no element. Then, on a fit whose draws a bound keeps rejecting: a longer
    !> sample starts with a shorter one, the two stream kinds agree, an absent stream is stream 0,
    !> and another stream or seed changes every element. A redraw reads only its own element's
    !> stream: on a one-point fit whose every other draw a bound rejects, no two elements are alike,
    !> where a redraw reading its neighbour's draws would repeat that neighbour's value; and a
    !> Gaussian variate beyond the cut is replaced by the same stream's next normal, found here by
    !> searching the streams for one whose first variate lies beyond five standard deviations.
    subroutine test_sample_addressing(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        integer(int64), parameter :: SEED = 20260919_int64
        type(pf_kde) :: k
        real(real64), allocatable :: x(:)
        real(real64) :: v(400), a(1000), b(500), c(1000), d(1000), r(2000), u, want, raw, e
        integer(int64) :: i, key, sk, j, jr, st
        integer :: n_recipe, n_raw, pass

        key = pf_random_key(SEED, KDE_LABEL)
        do pass = 1, 2
            if (pass == 1) then
                call k%fit([0.0_real64, 10.0_real64], bandwidth=1.0_real64, kernel="box")
            else
                call k%fit([0.0_real64, 10.0_real64], bandwidth=1.0_real64, kernel="box", &
                    weights=[1.0_real64, 3.0_real64])
            end if
            call k%sample(v, SEED, 3)
            n_recipe = 0
            n_raw = 0
            do i = 1_int64, 400_int64
                sk = pf_random_key(3_int64, i)
                if (pass == 1) then
                    j = pf_random_int_at(key, sk, 1_int64, 2_int64, 1_int64)
                    jr = pf_random_int_at(SEED, sk, 1_int64, 2_int64, 1_int64)
                else
                    ! The running weight is [1, 4]: the second point once the share reaches 1.
                    j = merge(1_int64, 2_int64, 4.0_real64*pf_random_at(key, sk, 1_int64) < 1.0_real64)
                    jr = merge(1_int64, 2_int64, 4.0_real64*pf_random_at(SEED, sk, 1_int64) < 1.0_real64)
                end if
                u = pf_random_at(key, sk, 2_int64)
                want = 10.0_real64*real(j - 1_int64, real64) + sqrt(3.0_real64)*(2.0_real64*u - 1.0_real64)
                u = pf_random_at(SEED, sk, 2_int64)
                raw = 10.0_real64*real(jr - 1_int64, real64) + sqrt(3.0_real64)*(2.0_real64*u - 1.0_real64)
                if (abs(v(i) - want) <= 1.0e-13_real64) n_recipe = n_recipe + 1
                if (abs(v(i) - raw) <= 1.0e-13_real64) n_raw = n_raw + 1
            end do
            call check(error, n_recipe == 400, merge("unweighted", "weighted  ", pass == 1) // &
                ": every element must be the recipe's draw, from its own stream under the family's key")
            if (allocated(error)) return
            call check(error, n_raw == 0, merge("unweighted", "weighted  ", pass == 1) // &
                ": no element may be the draw at the caller's own coordinates: the label must separate them")
            if (allocated(error)) return
        end do

        call kde_two_component(60_int64, x)
        call k%fit(x, rule="silverman", lower=-330.0_real64)
        call k%sample(a, SEED, 7)
        call k%sample(b, SEED, 7)
        call k%sample(c, SEED, 7_int64)
        call check(error, all(b == a(1:500)) .and. all(c == a), &
            "a shorter sample must be a prefix of a longer one, and the two stream kinds must agree")
        if (allocated(error)) return
        call check(error, minval(a) >= -330.0_real64, "every draw must respect the lower bound")
        if (allocated(error)) return
        call k%sample(c, SEED)
        call k%sample(d, SEED, 0_int64)
        call check(error, all(c == d), "an absent stream must be stream 0")
        if (allocated(error)) return
        call k%sample(c, SEED, 8)
        call k%sample(d, SEED + 1_int64, 7)
        call check(error, all(c /= a) .and. all(d /= a), "another stream, or another seed, must change every element")
        if (allocated(error)) return

        call k%fit([0.0_real64], bandwidth=1.0_real64, lower=0.0_real64)
        call k%sample(r, SEED, 1)
        call check(error, minval(r) >= 0.0_real64 .and. equal_pairs(r) == 0_int64, &
            "a redraw at the bound must read only its own element's stream: no two draws may be alike")
        if (allocated(error)) return

        e = 0.0_real64
        do st = 1_int64, 100000000_int64
            e = pf_random_normal_at(key, pf_random_key(st, 1_int64), 2_int64)
            if (abs(e) > 5.0_real64) exit
        end do
        want = pf_random_normal_at(key, pf_random_key(st, 1_int64), 3_int64)
        call check(error, abs(e) > 5.0_real64 .and. abs(want) <= 5.0_real64, &
            "the fixture: a stream whose first normal lies beyond the cut and whose second does not")
        if (allocated(error)) return
        call k%fit([0.0_real64], bandwidth=1.0_real64)
        call k%sample(r(1:1), SEED, st)
        call check(error, r(1) == want, &
            "a variate beyond the Gaussian's cut must be redrawn from the same stream's next normal")

    end subroutine test_sample_addressing

    !> Each kernel's sampler draws that kernel, and each correction keeps the draws where the
    !> corrected estimate is. A one-point fit at unit bandwidth draws the kernel itself: 200000 draws
    !> have its mean (0), variance (1, the cut Gaussian's `1 - 1.5e-5`) and kurtosis (the cut
    !> Gaussian's 3, the Epanechnikov kernel's 15/7, the cubic B-spline's 2.7, the box's 1.8) to
    !> five standard errors, and none lies beyond its support. At a bound the draws follow the
    !> corrected kernel: the half kernel under `"renormalise"`, whose mean is the fit's own first
    !> moment (`pf_integrate`); under both corrections a box ten times wider than `[0, 1]`, which is
    !> flat there -- mean 1/2, variance 1/12 -- and which rejects so many draws that most fall back
    !> to inverting the point's distribution function; and under both, Gaussians wider than
    !> `[0, 1]`, whose draws have the mean and variance of the fit's own corrected density
    !> (`pf_integrate`), the case where a mirror image reaches the far bound and the doubly
    !> reflected mass the images omit is drawn again rather than kept.
    subroutine test_sample_support_and_kernel(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        integer, parameter :: N = 200000
        real(real64), parameter :: KERNEL_KURT(4) = [3.0_real64, 15.0_real64/7.0_real64, 2.7_real64, &
            1.8_real64]
        character(len=11), parameter :: METHODS(2) = [character(len=11) :: "renormalise", "reflect"]
        type(pf_kde) :: k
        type(kde_density) :: fn
        real(real64), allocatable :: v(:)
        real(real64) :: mean, var, kurt, var_k, want, se
        integer :: kk, mm
        character(len=200) :: msg

        allocate(v(N))
        do kk = 1, 4
            call k%fit([0.0_real64], bandwidth=1.0_real64, kernel=trim(KERNELS(kk)))
            call k%sample(v, 11_int64, kk)
            call moments(v, mean, var, kurt)
            var_k = 1.0_real64
            if (kk == 1) var_k = 1.0_real64 - 1.5e-5_real64
            write(msg, '(a,a,3es12.4)') trim(KERNELS(kk)), ": mean, variance and kurtosis ", mean, var, kurt
            call check(error, abs(mean) <= 5.0_real64*sqrt(var_k/real(N, real64)) .and. &
                abs(var - var_k) <= 5.0_real64*var_k*sqrt((KERNEL_KURT(kk) - 1.0_real64)/real(N, real64)) .and. &
                abs(kurt - KERNEL_KURT(kk)) <= 0.1_real64, trim(msg) // " must be the kernel's")
            if (allocated(error)) return
            call check(error, maxval(abs(v)) <= RADIUS(kk), trim(KERNELS(kk)) // ": no draw may lie beyond the support")
            if (allocated(error)) return
        end do

        ! The half Gaussian at a lower bound: its mean is the corrected estimate's first moment.
        call fn%k%fit([0.0_real64], bandwidth=1.0_real64, lower=0.0_real64)
        fn%power = 1
        want = pf_integrate(fn, 0.0_real64, 5.0_real64, 1.0e-12_real64)
        call fn%k%sample(v, 12_int64)
        call moments(v, mean, var, kurt)
        se = sqrt(var/real(N, real64))
        write(msg, '(a,2es14.6)') "renormalise at a bound: the mean and the fit's first moment ", mean, want
        call check(error, minval(v) >= 0.0_real64 .and. abs(mean - want) <= 5.0_real64*se, trim(msg))
        if (allocated(error)) return

        ! Gaussians wider than [0, 1]: the draws follow the fit's own corrected density.
        do mm = 1, 2
            call fn%k%fit([0.1_real64, 0.35_real64, 0.8_real64], bandwidth=1.0_real64, lower=0.0_real64, &
                upper=1.0_real64, boundary=METHODS(mm))
            fn%power = 1
            want = pf_integrate(fn, 0.0_real64, 1.0_real64, 1.0e-12_real64)
            fn%power = 2
            var_k = pf_integrate(fn, 0.0_real64, 1.0_real64, 1.0e-12_real64) - want*want
            call fn%k%sample(v, 14_int64, mm)
            call moments(v, mean, var, kurt)
            write(msg, '(a,a,4es13.5)') trim(METHODS(mm)), ": wide Gaussians; the sample's mean and variance, and the fit's ", &
                mean, var, want, var_k
            call check(error, minval(v) >= 0.0_real64 .and. maxval(v) <= 1.0_real64 .and. &
                abs(mean - want) <= 5.0_real64*sqrt(var_k/real(N, real64)) .and. &
                abs(var - var_k) <= 5.0_real64*var_k*sqrt(1.0_real64/real(N, real64)), trim(msg))
            if (allocated(error)) return
        end do

        ! A box ten bandwidths wide on [0, 1] is flat there under either correction.
        do mm = 1, 2
            call k%fit([0.5_real64], bandwidth=10.0_real64, kernel="box", lower=0.0_real64, upper=1.0_real64, &
                boundary=METHODS(mm))
            call k%sample(v, 13_int64, mm)
            call moments(v, mean, var, kurt)
            write(msg, '(a,a,2es14.6)') trim(METHODS(mm)), ": a box far wider than [0, 1] draws it flat; mean, variance ", &
                mean, var
            call check(error, minval(v) >= 0.0_real64 .and. maxval(v) <= 1.0_real64 .and. &
                abs(mean - 0.5_real64) <= 5.0_real64*sqrt(1.0_real64/(12.0_real64*real(N, real64))) .and. &
                abs(var - 1.0_real64/12.0_real64) <= 5.0_real64*sqrt(0.8_real64/real(N, real64))/12.0_real64, trim(msg))
            if (allocated(error)) return
        end do

    end subroutine test_sample_support_and_kernel

    !> The point a draw starts from is chosen in proportion to its weight, and its kernel has the
    !> point's own bandwidth: three quarters of the draws from a fit weighted 1 to 3 lie about the
    !> heavier point, and the draws from an adaptive fit have the variance of the points plus the
    !> mean of `h_j**2` (each Gaussian kernel adding its own, cut, variance), which a sampler using
    !> the global bandwidth would miss by the spread of the bandwidths.
    subroutine test_sample_weights_and_bandwidths(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        integer, parameter :: N = 200000
        type(pf_kde) :: k
        real(real64), allocatable :: v(:), x(:), hb(:), xs(:)
        real(real64) :: share, mean, var, kurt, mu, want, want_global
        character(len=200) :: msg

        allocate(v(N))
        call k%fit([-10.0_real64, 10.0_real64], bandwidth=1.0_real64, kernel="box", weights=[1.0_real64, 3.0_real64])
        call k%sample(v, 21_int64)
        share = real(count(v > 0.0_real64), real64)/real(N, real64)
        write(msg, '(a,f9.6)') "weights 1 and 3: the share of draws about the heavier point is ", share
        call check(error, abs(share - 0.75_real64) <= 5.0_real64*sqrt(0.75_real64*0.25_real64/real(N, real64)), &
            trim(msg) // ", not 0.75")
        if (allocated(error)) return

        call kde_two_component(60_int64, x)
        call k%fit(x, rule="silverman", adaptive=.true., alpha=1.0_real64)
        allocate(hb(k%n_valid()), xs(k%n_valid()))
        call k%bandwidths(hb, xs)
        mu = sum(xs)/real(size(xs), real64)
        want = sum((xs - mu)**2)/real(size(xs), real64) + (1.0_real64 - 1.5e-5_real64)*sum(hb**2)/real(size(hb), real64)
        want_global = sum((xs - mu)**2)/real(size(xs), real64) + (1.0_real64 - 1.5e-5_real64)*k%bandwidth()**2
        call k%sample(v, 22_int64)
        call moments(v, mean, var, kurt)
        write(msg, '(a,3es14.6)') "adaptive: the variance, and that of the points plus mean(h_j**2) and plus h**2 ", &
            var, want, want_global
        call check(error, abs(mean - mu) <= 5.0_real64*sqrt(want/real(N, real64)) .and. &
            abs(var - want) <= 5.0_real64*want*sqrt(2.0_real64/real(N, real64)) .and. &
            abs(want - want_global) > 10.0_real64*want*sqrt(2.0_real64/real(N, real64)), trim(msg))

    end subroutine test_sample_weights_and_bandwidths

    !> The grid's `%sample` inverts the integral of its `%pdf` over its cells: each draw is the
    !> point where that integral reaches its uniform's share -- the recipe, reproduced here through
    !> the grid's own `%cdf` on a grid holding every point's weight, which at a draw is the draw's
    !> uniform, beside the control arm without the label -- and a sample's mean and variance are those
    !> of `%pdf` over the range (`pf_integrate`), on a grid narrower than its data, whose weight
    !> beyond the range no draw may land on. A longer sample starts with a shorter one; a grid with
    !> nothing in its cells, or a poisoned one, draws NaN.
    subroutine test_grid_sample(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        integer, parameter :: N = 200000
        integer(int64), parameter :: SEED = 77_int64
        type(pf_kde_grid) :: g
        type(grid_density) :: fn
        real(real64), allocatable :: x(:), v(:)
        real(real64) :: w(300), p(300), b(100), mass, m1, m2, mean, var, kurt
        integer(int64) :: i, key, sk
        integer :: n_recipe, n_raw
        character(len=200) :: msg

        call kde_fixture(1000_int64, x)
        call g%init(400, -700.0_real64, 700.0_real64, 30.0_real64)
        call g%add(x)
        call g%sample(w, SEED, 5)
        call g%cdf(w, p)
        key = pf_random_key(SEED, KDE_GRID_LABEL)
        n_recipe = 0
        n_raw = 0
        do i = 1_int64, 300_int64
            sk = pf_random_key(5_int64, i)
            if (abs(p(i) - pf_random_at(key, sk, 1_int64)) <= 1.0e-12_real64) n_recipe = n_recipe + 1
            if (abs(p(i) - pf_random_at(SEED, sk, 1_int64)) <= 1.0e-12_real64) n_raw = n_raw + 1
        end do
        call check(error, n_recipe == 300, &
            "every draw must be where the grid's integral reaches its uniform's share, from its own stream")
        if (allocated(error)) return
        call check(error, n_raw == 0, "no draw may come from the caller's own coordinates: the label must separate them")
        if (allocated(error)) return
        call g%sample(b, SEED, 5_int64)
        call check(error, all(b == w(1:100)), "a shorter sample must be a prefix, and the two stream kinds must agree")
        if (allocated(error)) return

        call g%init(300, -300.0_real64, 300.0_real64, 40.0_real64)
        call g%add(x)
        allocate(v(N))
        call g%sample(v, SEED)
        call moments(v, mean, var, kurt)
        fn%g = g
        ! The interpolant is linear between centres, so the centres are where the integrand bends.
        call g%grid(w)
        fn%power = 0
        mass = pf_integrate(fn, -300.0_real64, 300.0_real64, 1.0e-12_real64, breakpoints=w)
        fn%power = 1
        m1 = pf_integrate(fn, -300.0_real64, 300.0_real64, 1.0e-12_real64, breakpoints=w)/mass
        fn%power = 2
        m2 = pf_integrate(fn, -300.0_real64, 300.0_real64, 1.0e-12_real64, breakpoints=w)/mass - m1*m1
        write(msg, '(a,4es13.5)') "the sample's mean and variance, and %pdf's over the range ", mean, var, m1, m2
        call check(error, minval(v) >= -300.0_real64 .and. maxval(v) <= 300.0_real64 .and. &
            mass < 0.95_real64 .and. abs(mean - m1) <= 5.0_real64*sqrt(m2/real(N, real64)) .and. &
            abs(var - m2) <= 5.0_real64*m2*sqrt(2.0_real64/real(N, real64)), trim(msg))
        if (allocated(error)) return

        call g%init(10, 0.0_real64, 1.0_real64, 0.1_real64)
        call g%sample(b, SEED)
        call check(error, all(ieee_is_nan(b)), "an empty grid must draw NaN")
        if (allocated(error)) return
        call g%add([5.0_real64])
        call g%sample(b, SEED)
        call check(error, all(ieee_is_nan(b)), "a grid whose only weight lies beyond its range must draw NaN")
        if (allocated(error)) return
        call g%clear()
        call g%add([0.5_real64, ieee_value(1.0_real64, ieee_quiet_nan)], skipnan=.false.)
        call g%sample(b, SEED)
        call check(error, all(ieee_is_nan(b)), "a poisoned grid must draw NaN")

    end subroutine test_grid_sample

    !> The adaptive golden cases once the pilot is fine: at 262144 cells the library agrees with the
    !> exact pilot's estimate to `1e-9` of its peak, and its gap falls as the square of the pilot's
    !> cell width -- by about sixteen from 65536 cells -- which is what shows the coarse case's gap
    !> to be the pilot's discretisation and not a difference of definition. Writes the process-global
    !> cell count, so it runs serially, and restores the rule.
    subroutine test_adaptive_golden_fine(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check

        call check_fine("ADAPT", KG_ADAPT_PDF)
        if (allocated(error)) return
        call check_fine("ADAPT_CAP", KG_ADAPT_CAP_PDF)
        if (allocated(error)) return
        call check_fine("ADAPT_REF", KG_ADAPT_REF_PDF)
        if (allocated(error)) return
        call check_fine("ADAPT_W", KG_ADAPT_W_PDF)
        if (allocated(error)) return
        call parquet_debug_set_kde_pilot_cells(262144)
        call check_adaptive_case(error, "ADAPT", KG_ADAPT_H, KG_ADAPT_PDF, KG_ADAPT_CDF, 1.0e-9_real64, &
            1.0e-10_real64)
        if (.not. allocated(error)) call check_adaptive_case(error, "ADAPT_CAP", KG_ADAPT_CAP_H, KG_ADAPT_CAP_PDF, &
            KG_ADAPT_CAP_CDF, 1.0e-9_real64, 1.0e-10_real64)
        if (.not. allocated(error)) call check_adaptive_case(error, "ADAPT_REF", KG_ADAPT_REF_H, KG_ADAPT_REF_PDF, &
            KG_ADAPT_REF_CDF, 1.0e-9_real64, 1.0e-10_real64)
        if (.not. allocated(error)) call check_adaptive_case(error, "ADAPT_W", KG_ADAPT_W_H, KG_ADAPT_W_PDF, &
            KG_ADAPT_W_CDF, 1.0e-9_real64, 1.0e-10_real64)
        call parquet_debug_set_kde_pilot_cells(0)

    contains

        !> The gap to the oracle at 65536 cells over the gap at 262144: second order, so about 16.
        subroutine check_fine(name, pdf)
            character(len=*), intent(in) :: name     !! the case
            real(real64), intent(in)     :: pdf(NKX) !! the oracle's density at `KG_X`
            type(pf_kde) :: k
            logical :: ok
            real(real64) :: got(NKX), e(2)
            integer :: r
            character(len=160) :: msg

            do r = 1, 2
                call parquet_debug_set_kde_pilot_cells(65536*4**(r - 1))
                call fit_golden_case(name, k, ok)
                call k%pdf(KG_X, got)
                e(r) = maxval(abs(got - pdf))
            end do
            call parquet_debug_set_kde_pilot_cells(0)
            write(msg, '(2a,f8.3)') name, ": quartering the pilot's cells must divide the gap by about 16; it is ", &
                e(1)/e(2)
            call check(error, e(1)/e(2) > 10.0_real64 .and. e(1)/e(2) < 25.0_real64, trim(msg))

        end subroutine check_fine

    end subroutine test_adaptive_golden_fine

    !> `parquet_debug_set_kde_pilot_cells(n)` gives the fit's pilot `n` cells whatever the rule
    !> says, and `n <= 0` restores the rule: the control first, then the override, then the
    !> restoration.
    subroutine test_pilot_cells_forced(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde) :: k
        type(pf_kde_grid) :: g
        real(real64), allocatable :: x(:)
        integer :: rule_cells

        call kde_two_component(60_int64, x)
        call k%fit(x, bandwidth=40.0_real64, adaptive=.true.)
        call k%pilot(g)
        rule_cells = g%ncells()
        call parquet_debug_set_kde_pilot_cells(100)
        call k%fit(x, bandwidth=40.0_real64, adaptive=.true.)
        call k%pilot(g)
        call parquet_debug_set_kde_pilot_cells(0)
        call check(error, rule_cells /= 100 .and. g%ncells() == 100, "the override must set the pilot's cells")
        if (allocated(error)) return
        call k%fit(x, bandwidth=40.0_real64, adaptive=.true.)
        call k%pilot(g)
        call check(error, g%ncells() == rule_cells, "n <= 0 must restore the rule")

    end subroutine test_pilot_cells_forced

end module test_kde
