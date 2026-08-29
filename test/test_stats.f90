!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> Unit tests for `parquet_stats`.
!>
!> **What this suite is really asserting is a set of CONVENTIONS, not a computation.**
!> `pf_count_valid` counts; the interesting content is the exclusion policy every later reduction
!> in this module inherits from it, and each of those rules is a silent wrong answer if it breaks:
!>
!> * **the three exclusion classes are independent** -- a null, a NaN and a zero weight each remove
!>   an element on their own, and each is tested on its own so that a rule which happens to be
!>   implemented by another cannot pass for it;
!> * **the ORDER of exclusion is nullness, then NaN, then weight**, which is only observable in one
!>   place: an element that is null must never have its weight examined, so a null element carrying
!>   a NaN weight must count cleanly rather than abort. That is `test_null_element_ignores_its_weight`,
!>   and without it the ordering could be reversed with every other test still passing;
!> * **`skipnan` exists only where a NaN can exist.** The integer and logical specifics do not take
!>   it, so a test passing it to them would not compile -- which is the assertion;
!> * **an empty or fully excluded population answers 0 and does not abort**, because a per-group
!>   loop meets that case on real data.
!>
!> **Abort paths live elsewhere.** `error stop` kills the process, so a mismatched `is_valid`, a
!> mismatched `weights` and a negative/NaN/infinite weight are `stats_*` scenarios in
!> test/error_scenarios.f90, driven from test_errors.f90.
!>
!> Every test allocates its own arrays and shares no state, so nothing here needs a per-test
!> fixture filename.
module test_stats
    use parquet
    use test_stats_golden
    use iso_fortran_env, only : int32, int64, real32, real64
    use ieee_arithmetic, only : ieee_value, ieee_quiet_nan
    use testdrive, only : new_unittest, unittest_type, error_type, check
    implicit none
    private

    public :: collect_tests_parquet_stats

contains

    !> Registers this module's tests with test-drive.
    subroutine collect_tests_parquet_stats(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:) !! the suite to fill.

        testsuite = [ &
            new_unittest("pf_count_valid counts every element of a clean array", test_plain_count), &
            new_unittest("pf_count_valid spans every numeric kind", test_every_kind), &
            new_unittest("pf_count_valid excludes the nulls is_valid marks", test_nulls_excluded), &
            new_unittest("pf_count_valid excludes NaNs by default and counts them under skipnan=.false.", &
                test_nan_policy), &
            new_unittest("pf_count_valid excludes a zero-weighted element", test_zero_weight_excluded), &
            new_unittest("pf_count_valid examines no weight belonging to a null element", &
                test_null_element_ignores_its_weight), &
            new_unittest("pf_count_valid answers 0 for an empty or fully excluded population", &
                test_empty_population_is_not_fatal), &
            new_unittest("the fixture recipe still matches the generator's", test_fixture_recipe), &
            new_unittest("pf_moments matches the 50-digit oracle, unweighted", test_golden_unweighted), &
            new_unittest("variance is shift-invariant at an offset of 1e9", test_shift_invariance), &
            new_unittest("pf_moments matches the oracle under both weight_type conventions", &
                test_golden_weighted), &
            new_unittest("pf_moments matches the oracle with nulls and NaNs excluded", &
                test_golden_exclusions), &
            new_unittest("pf_moments answers NaN, not an abort, on every degenerate population", &
                test_golden_degenerate), &
            new_unittest("the one-shot reductions equal pf_moments bit for bit", test_one_shot_agrees), &
            new_unittest("equal weights give exactly the unweighted answer", test_equal_weights), &
            new_unittest("the mean of a constant array is that constant, exactly", test_mean_of_constant), &
            new_unittest("ok is .true. exactly when the answer is not NaN", test_ok_tracks_the_answer), &
            new_unittest("an empty population sums to 0 while every other answer is NaN", &
                test_empty_sums_to_zero) &
            ]
    end subroutine collect_tests_parquet_stats

    !> With no optional argument, every element is in the population.
    subroutine test_plain_count(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        real(real64) :: v(7)
        integer(int64) :: n
        integer :: i

        do i = 1, size(v)
            v(i) = real(i, real64)
        end do
        call pf_count_valid(v, n)
        call check(error, n == 7_int64, "pf_count_valid over a clean array must count every element")
    end subroutine test_plain_count

    !> Every numeric kind reaches the generic, and each answers for itself.
    !!
    !! A generic that resolved four of five kinds would still pass any test written for one of
    !! them, so all five are exercised rather than a representative.
    subroutine test_every_kind(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        integer(int64) :: n

        call pf_count_valid([1_int32, 2_int32, 3_int32], n)
        call check(error, n == 3_int64, "pf_count_valid must accept an int32 array")
        if (allocated(error)) return
        call pf_count_valid([1_int64, 2_int64, 3_int64, 4_int64], n)
        call check(error, n == 4_int64, "pf_count_valid must accept an int64 array")
        if (allocated(error)) return
        call pf_count_valid([1.0_real32, 2.0_real32], n)
        call check(error, n == 2_int64, "pf_count_valid must accept a real32 array")
        if (allocated(error)) return
        call pf_count_valid([1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64, 5.0_real64], n)
        call check(error, n == 5_int64, "pf_count_valid must accept a real64 array")
        if (allocated(error)) return
        call pf_count_valid([.true., .false., .true.], n)
        call check(error, n == 3_int64, &
            "pf_count_valid must accept a logical array and count .false. as an ordinary value")
    end subroutine test_every_kind

    !> `is_valid(i) == .false.` removes element `i`, and nothing else does.
    subroutine test_nulls_excluded(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        integer(int32) :: v(6)
        logical :: ok(6)
        integer(int64) :: n

        v = [10_int32, 20_int32, 30_int32, 40_int32, 50_int32, 60_int32]
        ok = [.true., .false., .true., .false., .false., .true.]
        call pf_count_valid(v, n, is_valid=ok)
        call check(error, n == 3_int64, "pf_count_valid must exclude every element is_valid marks null")
        if (allocated(error)) return
        call pf_count_valid(v, n, is_valid=[.false., .false., .false., .false., .false., .false.])
        call check(error, n == 0_int64, "an all-null population must count 0, not abort")
    end subroutine test_nulls_excluded

    !> A NaN leaves the population by default and stays in it under `skipnan=.false.`.
    !!
    !! **Both directions are asserted**, because the default is the interesting half and a
    !! one-sided test passes just as happily against an implementation that ignores the argument.
    subroutine test_nan_policy(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        real(real64) :: v(5)
        real(real32) :: v32(4)
        integer(int64) :: n

        v = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64, 5.0_real64]
        v(2) = ieee_value(1.0_real64, ieee_quiet_nan)
        v(4) = ieee_value(1.0_real64, ieee_quiet_nan)

        call pf_count_valid(v, n)
        call check(error, n == 3_int64, "a NaN must leave the population by default, as pf_minmax skips it")
        if (allocated(error)) return
        call pf_count_valid(v, n, skipnan=.true.)
        call check(error, n == 3_int64, "skipnan=.true. must be the default, not a different answer")
        if (allocated(error)) return
        call pf_count_valid(v, n, skipnan=.false.)
        call check(error, n == 5_int64, "skipnan=.false. must count a NaN as an ordinary value, as numpy does")
        if (allocated(error)) return

        v32 = [1.0_real32, 2.0_real32, 3.0_real32, 4.0_real32]
        v32(3) = ieee_value(1.0_real32, ieee_quiet_nan)
        call pf_count_valid(v32, n)
        call check(error, n == 3_int64, "the real32 specific must apply the same NaN rule as real64")
    end subroutine test_nan_policy

    !> Weight zero is how a caller says "drop this row".
    subroutine test_zero_weight_excluded(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        real(real64) :: v(5), w(5)
        integer(int64) :: n

        v = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64, 5.0_real64]
        w = [2.0_real64, 0.0_real64, 1.0_real64, 0.0_real64, 0.5_real64]
        call pf_count_valid(v, n, weights=w)
        call check(error, n == 3_int64, "a zero weight must remove the element from the population")
        if (allocated(error)) return
        call pf_count_valid(v, n)
        call check(error, n == 5_int64, &
            "the same array without weights must count every element -- the two answers differ legitimately")
        if (allocated(error)) return
        call pf_count_valid(v, n, weights=[0.0_real64, 0.0_real64, 0.0_real64, 0.0_real64, 0.0_real64])
        call check(error, n == 0_int64, "an all-zero weight array is the empty population, not an abort")
    end subroutine test_zero_weight_excluded

    !> The exclusion ORDER, which is observable in exactly one place.
    !!
    !! A null element's weight must never be examined, so a NaN or infinite weight sitting on a null
    !! element must not abort. Reverse the order -- validate weights first -- and every other test in
    !! this suite still passes while an ordinary program dies: a weight column computed as
    !! `1/err**2` is routinely NaN or infinite exactly where the value column is null.
    subroutine test_null_element_ignores_its_weight(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        real(real64) :: v(4), w(4)
        integer(int64) :: n

        v = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64]
        w = [1.0_real64, 1.0_real64, 1.0_real64, 1.0_real64]
        w(2) = ieee_value(1.0_real64, ieee_quiet_nan)          ! NaN weight ...
        w(3) = -5.0_real64                                     ! ... and a negative one ...
        call pf_count_valid(v, n, is_valid=[.true., .false., .false., .true.], weights=w)
        call check(error, n == 2_int64, &                      ! ... both on NULL elements: no abort.
            "a weight belonging to a null element must never be validated, let alone abort")
        if (allocated(error)) return

        ! The same for a NaN VALUE: it leaves the population before its weight is looked at.
        v(3) = ieee_value(1.0_real64, ieee_quiet_nan)
        call pf_count_valid(v, n, is_valid=[.true., .false., .true., .true.], weights=w)
        call check(error, n == 2_int64, &
            "a weight belonging to a NaN value must not be validated either -- NaN is excluded first")
    end subroutine test_null_element_ignores_its_weight

    !> A population with nothing in it counts 0. It must never abort: a group loop meets this.
    subroutine test_empty_population_is_not_fatal(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        real(real64), allocatable :: empty(:)
        real(real64) :: v(3)
        integer(int64) :: n

        allocate(empty(0))
        call pf_count_valid(empty, n)
        call check(error, n == 0_int64, "a zero-length array must count 0")
        if (allocated(error)) return

        v = [ieee_value(1.0_real64, ieee_quiet_nan), ieee_value(1.0_real64, ieee_quiet_nan), &
             ieee_value(1.0_real64, ieee_quiet_nan)]
        call pf_count_valid(v, n)
        call check(error, n == 0_int64, "an all-NaN array must count 0 rather than abort")
    end subroutine test_empty_population_is_not_fatal

    ! ==================================================================================
    ! Phase P2: the real64 moment core, against the 50-digit oracle
    !
    ! The expectations live in test_stats_golden, emitted by tools/generate_stats_vectors.py from
    ! an `mpmath` model that shares no arithmetic with the library and none with numpy either.
    ! `--check` in the lint stage is what stops them drifting into a record of whatever the library
    ! did on the day someone regenerated them.
    ! ==================================================================================

    !> The population every non-degenerate golden case is taken over.
    !!
    !! An integer sequence divided by a power of two, so every value is exactly representable and
    !! the generator's Python reproduces it bit for bit -- which is what lets the fixture travel as
    !! a recipe rather than as a thousand-element literal table. Change it here and
    !! `test_fixture_recipe` reports the mismatch against `G_PROBE`.
    subroutine golden_fixture(n, x)
        integer(int64), intent(in) :: n                          !! how many values to build.
        real(real64), allocatable, intent(out) :: x(:)           !! the population.
        integer(int64) :: i, a

        allocate(x(n))
        do i = 1_int64, n
            a = mod(i * i * 7919_int64 + 12345_int64, 1000003_int64)
            x(i) = real(a - 500001_int64, real64) / 1024.0_real64
        end do
    end subroutine golden_fixture

    !> `w(i) = mod(i, 5)`, so every fifth weight is ZERO and removes its element.
    subroutine golden_weights_mod5(n, w)
        integer(int64), intent(in) :: n                          !! how many weights to build.
        real(real64), allocatable, intent(out) :: w(:)           !! the weights.
        integer(int64) :: i

        allocate(w(n))
        do i = 1_int64, n
            w(i) = real(mod(i, 5_int64), real64)
        end do
    end subroutine golden_weights_mod5

    !> Compares one `pf_moments` result against its golden row, quantity by quantity.
    !!
    !! A quantity the oracle marks undefined is asserted to be a quiet NaN rather than compared
    !! against a number -- which is the half that would otherwise pass against a library returning
    !! zero for an empty group.
    subroutine check_case(error, label, got, gold, golddef, tol)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        character(len=*), intent(in) :: label               !! the case name, for the message.
        real(real64), intent(in) :: got(NQ)                 !! what the library answered.
        real(real64), intent(in) :: gold(NQ)                !! what the oracle says.
        logical, intent(in) :: golddef(NQ)                  !! which quantities are defined.
        real(real64), intent(in) :: tol                     !! relative tolerance.
        integer :: q
        character(len=220) :: msg

        do q = 1, NQ
            if (golddef(q)) then
                write(msg, '(a)') label // ": " // trim(QNAME(q)) // " came back NaN but is defined"
                call check(error, got(q) == got(q), trim(msg))
                if (allocated(error)) return
                write(msg, '(a,es24.17,a,es24.17)') label // ": " // trim(QNAME(q)) // " is ", &
                    got(q), " but the oracle says ", gold(q)
                call check(error, abs(got(q) - gold(q)) <= tol * max(1.0_real64, abs(gold(q))), trim(msg))
            else
                write(msg, '(a,es24.17)') label // ": " // trim(QNAME(q)) // &
                    " is undefined for this population and must be NaN, not ", got(q)
                call check(error, got(q) /= got(q), trim(msg))
            end if
            if (allocated(error)) return
        end do
    end subroutine check_case

    !> Runs one case through `pf_moments` and compares it with its golden row.
    subroutine run_case(error, label, gold, golddef, goldn, values, is_valid, weights, &
            weight_type, ddof, bias, excess, skipnan, tol)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        character(len=*), intent(in) :: label               !! the case name, for the message.
        real(real64), intent(in) :: gold(NQ)                !! the oracle's values.
        logical, intent(in) :: golddef(NQ)                  !! which of them are defined.
        integer(int64), intent(in) :: goldn(3)              !! [n_valid, n_null, n_nan].
        real(real64), intent(in) :: values(:)               !! the population.
        logical, intent(in), optional :: is_valid(:)        !! per element: .false. marks a null.
        real(real64), intent(in), optional :: weights(:)    !! per element weight.
        character(len=*), intent(in), optional :: weight_type !! "reliability" or "frequency".
        integer, intent(in), optional :: ddof               !! delta degrees of freedom.
        logical, intent(in), optional :: bias               !! .true. leaves the moments uncorrected.
        logical, intent(in), optional :: excess             !! .false. adds 3 back to the kurtosis.
        logical, intent(in), optional :: skipnan            !! .false. lets a NaN propagate.
        real(real64), intent(in) :: tol                     !! relative tolerance.
        real(real64) :: got(NQ)
        integer(int64) :: nv, nnull, nnan
        character(len=160) :: msg

        call pf_moments(values, n_valid=nv, mean=got(2), variance=got(3), stddev=got(4), &
            sem=got(5), skewness=got(6), kurtosis=got(7), vsum=got(1), vmin=got(8), vmax=got(9), &
            is_valid=is_valid, weights=weights, weight_type=weight_type, ddof=ddof, bias=bias, &
            excess=excess, skipnan=skipnan, n_null=nnull, n_nan=nnan)

        write(msg, '(a,i0,a,i0)') label // ": n_valid is ", nv, " but should be ", goldn(1)
        call check(error, nv == goldn(1), trim(msg))
        if (allocated(error)) return
        write(msg, '(a,i0,a,i0)') label // ": n_null is ", nnull, " but should be ", goldn(2)
        call check(error, nnull == goldn(2), trim(msg))
        if (allocated(error)) return
        write(msg, '(a,i0,a,i0)') label // ": n_nan is ", nnan, " but should be ", goldn(3)
        call check(error, nnan == goldn(3), trim(msg))
        if (allocated(error)) return

        call check_case(error, label, got, gold, golddef, tol)
    end subroutine run_case

    !> The recipe here and the recipe in the generator must be the same recipe.
    !!
    !! Without this, a drifted fixture reports as a tolerance failure in every case at once, which
    !! looks like an accuracy defect in the library rather than a mismatch in the test data.
    subroutine test_fixture_recipe(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        real(real64), allocatable :: x(:)
        integer :: i
        character(len=160) :: msg

        call golden_fixture(int(size(G_PROBE), int64), x)
        do i = 1, size(G_PROBE)
            write(msg, '(a,i0,a,es24.17,a,es24.17)') "golden_fixture element ", i, " is ", x(i), &
                " but the generator emitted ", G_PROBE(i)
            call check(error, x(i) == G_PROBE(i), trim(msg))
            if (allocated(error)) return
        end do
    end subroutine test_fixture_recipe

    !> The unweighted cases: the default configuration, both `ddof` ends, and both `bias` spellings.
    subroutine test_golden_unweighted(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        real(real64), allocatable :: x(:), big(:)
        real(real64), parameter :: TOL = 1.0e-13_real64

        call golden_fixture(32_int64, x)
        call run_case(error, "U32", G_U32, G_U32_DEF, G_U32_N, x, tol=TOL)
        if (allocated(error)) return
        call run_case(error, "U32_D0", G_U32_D0, G_U32_D0_DEF, G_U32_D0_N, x, ddof=0, tol=TOL)
        if (allocated(error)) return
        ! ddof beyond the population is an ordinary data condition -- NaN, never a division by zero
        ! and never an abort. "this group has one member" reaches it constantly.
        call run_case(error, "U32_D99", G_U32_D99, G_U32_D99_DEF, G_U32_D99_N, x, ddof=99, tol=TOL)
        if (allocated(error)) return
        call run_case(error, "U32_BIAS", G_U32_BIAS, G_U32_BIAS_DEF, G_U32_BIAS_N, x, bias=.true., tol=TOL)
        if (allocated(error)) return
        call run_case(error, "U32_RAWK", G_U32_RAWK, G_U32_RAWK_DEF, G_U32_RAWK_N, x, excess=.false., tol=TOL)
        if (allocated(error)) return

        call golden_fixture(1000_int64, big)
        call run_case(error, "U1000", G_U1000, G_U1000_DEF, G_U1000_N, big, tol=TOL)
    end subroutine test_golden_unweighted

    !> The single best regression test in the module: the same population, offset by 1e9.
    !!
    !! The offset is EXACT -- the values are multiples of 2**-10 and `ulp(1e9)` is 2**-23 -- so the
    !! variance of the shifted population is mathematically identical to the variance of the
    !! original, and the two library answers may be compared directly rather than through the
    !! oracle. The textbook `sum(x**2) - sum(x)**2/n` misses this by orders of magnitude; two-pass
    !! misses it by a few ulp, because the derivative of the central moment with respect to the
    !! mean is zero at the mean.
    subroutine test_shift_invariance(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        real(real64), allocatable :: x(:), shifted(:)
        ! NOT `g_shift`: Fortran is case-insensitive, so that local would shadow the golden
        ! parameter `G_SHIFT` and turn it into a scalar at the `run_case` call above.
        real(real64) :: v_plain, v_shift, sd_plain, sd_shift, sk_plain, sk_shift
        character(len=200) :: msg
        integer(int64) :: i

        call golden_fixture(1000_int64, x)
        allocate(shifted(size(x)))
        do i = 1_int64, size(x, kind=int64)
            shifted(i) = x(i) + 1.0e9_real64
        end do

        call run_case(error, "SHIFT", G_SHIFT, G_SHIFT_DEF, G_SHIFT_N, shifted, tol=1.0e-14_real64)
        if (allocated(error)) return

        call pf_variance(x, v_plain)
        call pf_variance(shifted, v_shift)
        call pf_stddev(x, sd_plain)
        call pf_stddev(shifted, sd_shift)
        call pf_skewness(x, sk_plain)
        call pf_skewness(shifted, sk_shift)
        write(msg, '(a,es24.17,a,es24.17)') "variance is not shift-invariant: plain ", v_plain, &
            " vs shifted ", v_shift
        call check(error, abs(v_shift - v_plain) <= 1.0e-13_real64 * v_plain, trim(msg))
        if (allocated(error)) return
        write(msg, '(a,es24.17,a,es24.17)') "stddev is not shift-invariant: plain ", sd_plain, &
            " vs shifted ", sd_shift
        call check(error, abs(sd_shift - sd_plain) <= 1.0e-13_real64 * sd_plain, trim(msg))
        if (allocated(error)) return

        ! The SKEWNESS is the sharp end of this, and the reason `stats_engine` carries the
        ! `sum(w*d)` correction at all. The second moment is immune to a perturbed mean because its
        ! derivative there vanishes; the third picks the error up linearly through `3*delta*m2`,
        ! and without the correction this comparison was wrong in the eighth significant digit
        ! while the variance above was still right in the fifteenth.
        write(msg, '(a,es24.17,a,es24.17)') "skewness is not shift-invariant: plain ", sk_plain, &
            " vs shifted ", sk_shift
        call check(error, abs(sk_shift - sk_plain) <= 1.0e-12_real64 * abs(sk_plain), trim(msg))
    end subroutine test_shift_invariance

    !> The two `weight_type` conventions, which separate only when the weights are unequal.
    subroutine test_golden_weighted(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        real(real64), allocatable :: x(:), w(:)
        real(real64) :: three(32)
        real(real64), parameter :: TOL = 1.0e-13_real64

        call golden_fixture(32_int64, x)
        three = 3.0_real64
        call run_case(error, "W3", G_W3, G_W3_DEF, G_W3_N, x, weights=three, tol=TOL)
        if (allocated(error)) return
        call run_case(error, "W3F", G_W3F, G_W3F_DEF, G_W3F_N, x, weights=three, &
            weight_type="frequency", tol=TOL)
        if (allocated(error)) return

        ! mod(i,5) puts a ZERO weight on every fifth element, so n_valid is 26 rather than 32 --
        ! a zero weight removes the element exactly as a null does.
        call golden_weights_mod5(32_int64, w)
        call run_case(error, "WVAR", G_WVAR, G_WVAR_DEF, G_WVAR_N, x, weights=w, tol=TOL)
        if (allocated(error)) return
        call run_case(error, "WVARF", G_WVARF, G_WVARF_DEF, G_WVARF_N, x, weights=w, &
            weight_type="frequency", tol=TOL)
    end subroutine test_golden_weighted

    !> Nulls and NaNs, including the case that pins the exclusion ORDER.
    subroutine test_golden_exclusions(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        real(real64), allocatable :: x(:), nanx(:), nanw(:)
        logical :: mask(32)
        real(real64), parameter :: TOL = 1.0e-13_real64
        integer(int64) :: i

        call golden_fixture(32_int64, x)
        do i = 1_int64, 32_int64
            mask(i) = mod(i, 3_int64) /= 0_int64
        end do
        call run_case(error, "NULLS", G_NULLS, G_NULLS_DEF, G_NULLS_N, x, is_valid=mask, tol=TOL)
        if (allocated(error)) return

        ! Every tenth value is NaN AND carries a NaN weight. Under the family's exclusion order the
        ! NaN value is out of the population before its weight is looked at, so this must compute
        ! cleanly; reverse the order and it aborts on the weight instead.
        allocate(nanx(32), nanw(32))
        do i = 1_int64, 32_int64
            if (mod(i, 10_int64) == 0_int64) then
                nanx(i) = ieee_value(1.0_real64, ieee_quiet_nan)
                nanw(i) = ieee_value(1.0_real64, ieee_quiet_nan)
            else
                nanx(i) = x(i)
                nanw(i) = 1.0_real64
            end if
        end do
        call run_case(error, "NANS", G_NANS, G_NANS_DEF, G_NANS_N, nanx, weights=nanw, tol=TOL)
        if (allocated(error)) return

        ! skipnan=.false. is numpy's propagating behaviour: the NaN stays in the population, so
        ! every answer is NaN -- including min and max, which are not IEEE minNum/maxNum here.
        call run_case(error, "NANS_KEEP", G_NANS_KEEP, G_NANS_KEEP_DEF, G_NANS_KEEP_N, nanx, &
            skipnan=.false., tol=TOL)
    end subroutine test_golden_exclusions

    !> Every degenerate population a per-group loop meets on real data. None of them may abort.
    subroutine test_golden_degenerate(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        real(real64) :: constv(17), one(1)
        real(real64), allocatable :: nothing(:), x8(:), zerow(:)
        logical :: allnull(8)
        real(real64), parameter :: TOL = 1.0e-13_real64

        constv = 2.5_real64
        call run_case(error, "CONST", G_CONST, G_CONST_DEF, G_CONST_N, constv, tol=TOL)
        if (allocated(error)) return

        one = [3.75_real64]
        call run_case(error, "SINGLE", G_SINGLE, G_SINGLE_DEF, G_SINGLE_N, one, tol=TOL)
        if (allocated(error)) return
        call run_case(error, "SINGLE_D0", G_SINGLE_D0, G_SINGLE_D0_DEF, G_SINGLE_D0_N, one, &
            ddof=0, tol=TOL)
        if (allocated(error)) return

        allocate(nothing(0))
        call run_case(error, "EMPTY", G_EMPTY, G_EMPTY_DEF, G_EMPTY_N, nothing, tol=TOL)
        if (allocated(error)) return

        call golden_fixture(8_int64, x8)
        allnull = .false.
        call run_case(error, "ALLNULL", G_ALLNULL, G_ALLNULL_DEF, G_ALLNULL_N, x8, &
            is_valid=allnull, tol=TOL)
        if (allocated(error)) return

        ! numpy RAISES on an all-zero weight array; this module treats it as the empty case, which
        ! is the only answer consistent with "a zero weight removes the element".
        allocate(zerow(8))
        zerow = 0.0_real64
        call run_case(error, "ALLZEROW", G_ALLZEROW, G_ALLZEROW_DEF, G_ALLZEROW_N, x8, &
            weights=zerow, tol=TOL)
    end subroutine test_golden_degenerate

    !> Every one-shot reduction must be the corresponding `pf_moments` output, bit for bit.
    !!
    !! Compared with `==`, not a tolerance: they go through the same engine, so anything else means
    !! one of them has grown a second code path -- which is how two reductions in one program come
    !! to disagree about the same array.
    subroutine test_one_shot_agrees(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        real(real64), allocatable :: x(:), w(:)
        real(real64) :: mm(NQ), s, m, v, sd, se, g, k

        call golden_fixture(500_int64, x)
        call golden_weights_mod5(500_int64, w)
        call pf_moments(x, mean=mm(2), variance=mm(3), stddev=mm(4), sem=mm(5), skewness=mm(6), &
            kurtosis=mm(7), vsum=mm(1), weights=w)
        call pf_sum(x, s, weights=w)
        call pf_mean(x, m, weights=w)
        call pf_variance(x, v, weights=w)
        call pf_stddev(x, sd, weights=w)
        call pf_sem(x, se, weights=w)
        call pf_skewness(x, g, weights=w)
        call pf_kurtosis(x, k, weights=w)

        call check(error, s == mm(1), "pf_sum disagrees with pf_moments' vsum")
        if (allocated(error)) return
        call check(error, m == mm(2), "pf_mean disagrees with pf_moments' mean")
        if (allocated(error)) return
        call check(error, v == mm(3), "pf_variance disagrees with pf_moments' variance")
        if (allocated(error)) return
        call check(error, sd == mm(4), "pf_stddev disagrees with pf_moments' stddev")
        if (allocated(error)) return
        call check(error, se == mm(5), "pf_sem disagrees with pf_moments' sem")
        if (allocated(error)) return
        call check(error, g == mm(6), "pf_skewness disagrees with pf_moments' skewness")
        if (allocated(error)) return
        call check(error, k == mm(7), "pf_kurtosis disagrees with pf_moments' kurtosis")
    end subroutine test_one_shot_agrees

    !> Equal weights must give EXACTLY the unweighted answer, under the default convention.
    !!
    !! Exactly, not nearly: with every weight equal the weighted formulas reduce to the unweighted
    !! ones algebraically, and any difference means a weight is being folded in somewhere it should
    !! have cancelled. The frequency convention is asserted to DIFFER, which is the negative
    !! control -- without it this test passes against an implementation ignoring `weight_type`.
    subroutine test_equal_weights(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        real(real64), allocatable :: x(:)
        real(real64) :: w(64), v_plain, v_w, v_freq, g_plain, g_w

        call golden_fixture(64_int64, x)
        w = 3.0_real64
        call pf_variance(x, v_plain)
        call pf_variance(x, v_w, weights=w)
        call pf_variance(x, v_freq, weights=w, weight_type="frequency")
        call pf_skewness(x, g_plain)
        call pf_skewness(x, g_w, weights=w)

        ! Agreement to a few ulp rather than bit-for-bit, and the reason is worth stating: the
        ! weighted path forms `w*x` and `sum(w)` where the unweighted one forms `x` and `n`, so
        ! every product rounds once more even though the algebra cancels the weight out exactly.
        ! A bit-for-bit assertion here would be asserting an accident of the multiplier.
        call check(error, abs(v_w - v_plain) <= 1.0e-14_real64 * abs(v_plain), &
            "reliability weights all equal must give the unweighted variance")
        if (allocated(error)) return
        call check(error, abs(g_w - g_plain) <= 1.0e-14_real64 * abs(g_plain), &
            "reliability weights all equal must give the unweighted skewness")
        if (allocated(error)) return
        call check(error, v_freq /= v_plain, &
            "frequency weights of 3 must NOT give the unweighted variance -- ddof is charged against sum(w)")
    end subroutine test_equal_weights

    !> The mean of a constant array is that constant, and its variance is exactly zero.
    !!
    !! **Exactly, but only for a constant the format can hold.** With `2.5` every partial sum is
    !! exact and the division is exact, so `==` is the right assertion and it is a real one: it
    !! fails the moment the mean acquires a spurious correction term. With `0.1` the summation
    !! itself rounds -- 0.1 is not a binary fraction, and the in-block sum is sequential -- so the
    !! same property holds only to a few ulp, and asserting `==` there would be asserting an
    !! accident of the summation order rather than anything about the mean.
    subroutine test_mean_of_constant(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        real(real64) :: c(300), m, v

        c = 2.5_real64
        call pf_mean(c, m)
        call check(error, m == 2.5_real64, "the mean of a constant array must be that constant exactly")
        if (allocated(error)) return
        call pf_variance(c, v)
        call check(error, v == 0.0_real64, "the variance of a constant array must be exactly 0")
        if (allocated(error)) return

        c = 0.1_real64
        call pf_mean(c, m)
        call check(error, abs(m - 0.1_real64) <= 4.0_real64 * spacing(0.1_real64), &
            "the mean of a non-representable constant must still be that constant to a few ulp")
        if (allocated(error)) return
        call pf_variance(c, v)
        call check(error, v <= 1.0e-30_real64, "the variance of a constant array must be negligible")
    end subroutine test_mean_of_constant

    !> `ok` is set from the answer, so the two can never disagree.
    subroutine test_ok_tracks_the_answer(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        real(real64) :: one(1), v, s
        real(real64), allocatable :: nothing(:)
        logical :: ok

        one = [3.75_real64]
        call pf_variance(one, v, ok=ok)
        call check(error, .not. ok, "a one-element population has no ddof=1 variance, so ok must be .false.")
        if (allocated(error)) return
        call check(error, v /= v, "the value beside ok=.false. must be a quiet NaN")
        if (allocated(error)) return

        call pf_variance(one, v, ddof=0, ok=ok)
        call check(error, ok, "at ddof=0 a one-element population does have a variance")
        if (allocated(error)) return
        call check(error, v == 0.0_real64, "the ddof=0 variance of one element is exactly 0")
        if (allocated(error)) return

        allocate(nothing(0))
        call pf_sum(nothing, s, ok=ok)
        call check(error, ok, "an empty sum is defined, so ok must be .true.")
    end subroutine test_ok_tracks_the_answer

    !> The one quantity an empty population still defines, and the reason it is the only one.
    subroutine test_empty_sums_to_zero(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        real(real64), allocatable :: nothing(:)
        real(real64) :: s, m
        integer(int64) :: nv

        allocate(nothing(0))
        call pf_moments(nothing, n_valid=nv, vsum=s, mean=m)
        call check(error, s == 0.0_real64, "an empty population sums to exactly 0, as numpy and pandas do")
        if (allocated(error)) return
        call check(error, m /= m, "an empty population has no mean, so it must come back NaN")
        if (allocated(error)) return
        call check(error, nv == 0_int64, "an empty population has n_valid 0")
    end subroutine test_empty_sums_to_zero

end module test_stats
