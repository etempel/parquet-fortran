!> Tests for `parquet_kde`'s `pf_kde`: the kernels, the bandwidth rules, the population rules,
!> the boundary corrections, the exact queries and the curve.
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
!! Two suites. `kde` is pure in-memory work and runs concurrently. `kde_serial` holds the one test
!! that writes process-global state -- it silences `%print` through the `verbosity` setting -- and
!! is on `suite_is_safe_to_parallelize`'s exclusion list. Both are registered in
!! `run_tester_pf.f90`, the runner that executes no `bind(C)` call.
module test_kde

    use testdrive, only : new_unittest, unittest_type, error_type, check
    use parquet_kde
    use parquet_stats, only : pf_stddev, pf_iqr, pf_count_valid
    use parquet_integrate, only : pf_integrand, pf_integrate
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
                test_guide_boundary_table) &
            ]

    end subroutine collect_tests_kde

    !> Registers the suite that writes process-global state and so runs serially.
    subroutine collect_tests_kde_serial(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:) !! Receives the suite's tests.

        testsuite = [ &
            new_unittest("verbosity silences %print", test_print_silenced) &
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
        call kde_fixture(n, x)
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
        real(real64) :: f, c, empty(0), xg(5), fg(5)
        logical :: ok

        call k%fit(empty, ok=ok)
        call k%pdf(0.0_real64, f)
        call k%curve(xg, fg)
        call check(error, (.not. ok) .and. k%is_fitted() .and. ieee_is_nan(f) .and. &
            all(ieee_is_nan(fg)) .and. all(ieee_is_nan(xg)) .and. k%n_valid() == 0_int64, &
            "an empty sample must leave a fitted, undefined estimate")
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

end module test_kde
