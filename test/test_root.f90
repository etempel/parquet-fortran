!> Tests for `parquet_root`: Brent's method on a bracket, the bracket expansion under each growth
!> policy, the three status codes, the evaluation record, and the absence of any floating-point
!> exception on a legal call.
!!
!! **Every expected root is stated in `test_root_support.f90` beside its function**, or re-derived
!! here by an in-test bisection that shares nothing with the solver, never read off a run. An
!! evaluation COUNT is asserted as an equality only where it is structural -- the expansion tries
!! from a bracket to its first sign change are a power-of-two fact -- and as a bound elsewhere,
!! because the interpolation's last bits differ between compilers.
!!
!! **The accuracy bound is the stopping rule's own.** Under the defaults the search stops when the
!! bracket around the root is at most `2*t` wide, `t = 4*epsilon*|x|`, so `x` is within
!! `8*epsilon*|x|` of the root. `DEFAULT_BOUND` is that, and a hardwired absolute tolerance misses
!! it by orders of magnitude at a small root.
!!
!! **Two tests were written from KDEpy's bracket search for the Improved Sheather-Jones bandwidth**,
!! the motivating caller, and must not be weakened: `test_root_expansion_up_finds_the_bracket`
!! grows `[0, tol]` by doubling until the ends differ in sign, and
!! `test_root_expansion_exhausted_reports_no_bracket` reports a missing sign change as a status a
!! retry can act on, at a limit and after the tries run out.
!!
!! This suite is pure computation with no fixture files and no process-global state, so it runs
!! concurrently. Its only library import is `use parquet_root`: it is registered in
!! `run_tester_pf.f90`, the runner that executes no `bind(C)` call, and
!! `check_test_runner_partition` requires that the files feeding that runner never reach
!! `parquet_bindings`.
module test_root

    use testdrive, only : new_unittest, unittest_type, error_type, check
    use parquet_root
    use test_root_support
    use iso_fortran_env, only : real64, int64
    use, intrinsic :: ieee_arithmetic, only : ieee_get_flag, ieee_set_flag, ieee_usual, &
                                             ieee_is_finite, ieee_support_flag, ieee_underflow
#ifndef __flang__
    ! The halting-mode pair lowers to `feenableexcept`/`fedisableexcept`, which Apple's libc
    ! lacks, so flang on macOS cannot LINK a reference to either (`fortran-gotchas.md`).
    use, intrinsic :: ieee_arithmetic, only : ieee_support_halting, ieee_get_halting_mode, &
                                             ieee_set_halting_mode, ieee_overflow, ieee_invalid, &
                                             ieee_divide_by_zero
#endif

    implicit none
    private

    public :: collect_tests_root

    !> Where the default stopping rule leaves `x`, relative to `|x|`: within `8*epsilon`.
    real(real64), parameter :: DEFAULT_BOUND = 8.0_real64*epsilon(1.0_real64)
    !> Cases the seeded stress run in `test_root_extreme_calls_raise_no_flag` draws.
    integer, parameter :: STRESS_CASES = 4000

contains

    !> Registers this module's tests.
    subroutine collect_tests_root(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:) !! Receives the suite's tests.

        testsuite = [ &
            new_unittest("x*x - 2 on [0, 2] gives sqrt(2) to the stopping rule's bound", &
                         test_root_finds_a_simple_root), &
            new_unittest("five functions agree with an in-test bisection oracle", &
                         test_root_matches_a_bisection_oracle), &
            new_unittest("the interpolation beats bisection on evaluations", &
                         test_root_beats_bisection_on_evaluations), &
            new_unittest("an exact zero at an end or a probe is returned at once", &
                         test_root_endpoint_that_is_a_root_returns_at_once), &
            new_unittest("doubling [0, 1e-11] upward finds a root at 0.3 in 35 tries", &
                         test_root_expansion_up_finds_the_bracket), &
            new_unittest("growing from below stops at the first sign change and its root", &
                         test_root_expansion_stops_at_the_first_sign_change), &
            new_unittest("an expansion that runs out reports PF_ROOT_NO_BRACKET, not an abort", &
                         test_root_expansion_exhausted_reports_no_bracket), &
            new_unittest("no evaluation lies outside the expansion limits", &
                         test_root_expansion_respects_the_limit), &
            new_unittest("the DOWN and BOTH policies each find a bracket UP cannot", &
                         test_root_expansion_down_and_both), &
            new_unittest("without an expansion the bracket must already change sign", &
                         test_root_no_expansion_requires_a_bracket), &
            new_unittest("an infinite value at an end is a sign, not an error", &
                         test_root_infinite_endpoint_is_a_sign), &
            new_unittest("values whose product underflows or overflows still bracket", &
                         test_root_tiny_values_still_bracket), &
            new_unittest("a spent budget reports PF_ROOT_LIMIT with x defined, while expanding too", &
                         test_root_budget_is_spent), &
            new_unittest("a root near 1e-9 comes back to the relative bound under the defaults", &
                         test_root_relative_tolerance_at_a_tiny_root), &
            new_unittest("a root at zero converges with atol and spends the budget without it", &
                         test_root_at_zero_needs_atol), &
            new_unittest("a looser atol or rtol stops sooner and still meets itself", &
                         test_root_tolerances_stop_early_when_asked), &
            new_unittest("the object and plain-function forms agree to the bit", &
                         test_root_both_forms_agree), &
            new_unittest("the history records every evaluation, in order, trimmed to n", &
                         test_root_history_records_every_evaluation), &
            new_unittest("a pole is converged on and exposed through info%froot", &
                         test_root_pole_is_reported_through_froot), &
            new_unittest("extreme but legal calls raise no overflow, invalid or divide-by-zero", &
                         test_root_extreme_calls_raise_no_flag), &
            new_unittest("the guide's overshoot bound holds, and max_neval binds", &
                         test_guide_budget_overshoot_bound), &
            new_unittest("a tie on |f| at the two ends hands back the lower one", &
                         test_root_no_bracket_tie_takes_the_lower_end) &
            ]

    end subroutine collect_tests_root


    !> A tie on `|f|` at the two ends of an unbracketed interval hands back the LOWER end.
    !!
    !! `doc/pages/utilities/root-finding.md` says `x` is "the end of the last bracket with the
    !! smaller `|f|` -- the lower end where the two are equal". The first half is already covered
    !! by `test_root_expansion_exhausted_reports_no_bracket`; the tie is not, and `take_the_better_end`
    !! (`src/parquet_root_solve.f90`) settles it by falling through a strict `<`.
    !!
    !! The NEGATIVE CONTROL is the second call: on a bracket whose UPPER end has the strictly
    !! smaller `|f|`, the answer must be the upper end. Without it an implementation that always
    !! returned the lower end would pass the first assertion, which is the whole claim here.
    subroutine test_root_no_bracket_tie_takes_the_lower_end(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        type(pf_root_info) :: info
        real(real64)       :: x

        tie: block
            ! `x*x + 1` is even, so |f(-1)| == |f(1)| == 2 exactly.
            call pf_find_root(root_even_no_root, -1.0_real64, 1.0_real64, x, info=info)
            call check(error, info%status == PF_ROOT_NO_BRACKET, &
                       "an even function on a centred bracket must report PF_ROOT_NO_BRACKET")
            if (allocated(error)) exit tie
            call check(error, abs(root_even_no_root(-1.0_real64)) == abs(root_even_no_root(1.0_real64)), &
                       "the fixture must really tie: |f| equal at the two ends")
            if (allocated(error)) exit tie
            call check(error, x == -1.0_real64 .and. info%froot == 2.0_real64, &
                       "a tie on |f| must hand back the LOWER end, with froot its value")
            if (allocated(error)) exit tie

            ! Negative control: the upper end is strictly better, so it must come back instead.
            call pf_find_root(root_even_no_root, -2.0_real64, 1.0_real64, x, info=info)
            call check(error, abs(root_even_no_root(1.0_real64)) < abs(root_even_no_root(-2.0_real64)), &
                       "the control's upper end must really be the smaller |f|")
            if (allocated(error)) exit tie
            call check(error, info%status == PF_ROOT_NO_BRACKET .and. x == 1.0_real64 &
                       .and. info%froot == 2.0_real64, &
                       "where the upper end has the smaller |f| it is the one handed back")
        end block tie

    end subroutine test_root_no_bracket_tie_takes_the_lower_end

    !> `x*x - 2` on `[0, 2]`: `sqrt(2)` within the default bound, and the record of what happened.
    subroutine test_root_finds_a_simple_root(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        type(pf_root_info) :: info
        real(real64)       :: x, want
        logical            :: conv

        want = sqrt(2.0_real64)
        call pf_find_root(root_sq2, 0.0_real64, 2.0_real64, x, converged=conv, info=info)
        call check(error, info%status == PF_ROOT_OK .and. info%converged .and. conv, &
                   "x*x - 2 on [0, 2] must converge, and converged= must say so")
        if (allocated(error)) return
        call check(error, abs(x - want) <= DEFAULT_BOUND*want, &
                   "x*x - 2 on [0, 2] must give sqrt(2) to within 8*epsilon relative")
        if (allocated(error)) return
        call check(error, info%froot == root_sq2(x), "info%froot must be f at the returned point")
        if (allocated(error)) return
        call check(error, info%bracket_lo == 0.0_real64 .and. info%bracket_hi == 2.0_real64 &
                   .and. info%nexpand == 0, "a bracket that already changes sign is solved as given")
        if (allocated(error)) return
        call check(error, info%neval == info%niter + 2, &
                   "every evaluation beyond the two ends must be a Brent iteration")

    end subroutine test_root_finds_a_simple_root

    !> Five functions against a slow bisection written here, which shares nothing with the solver,
    !! and against each root's stated value.
    subroutine test_root_matches_a_bisection_oracle(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        call compare_with_oracle(error, root_cubic, 2.0_real64, 3.0_real64, CUBIC_ROOT, "x**3 - 2x - 5")
        if (allocated(error)) return
        call compare_with_oracle(error, root_cos_minus_x, 0.0_real64, 1.0_real64, DOTTIE, "cos(x) - x")
        if (allocated(error)) return
        call compare_with_oracle(error, root_exp_minus_2, 0.0_real64, 1.0_real64, log(2.0_real64), &
                                 "exp(x) - 2")
        if (allocated(error)) return
        call compare_with_oracle(error, root_log_plus_x, 0.1_real64, 1.0_real64, OMEGA, "log(x) + x")
        if (allocated(error)) return
        call compare_with_oracle(error, root_x_minus_tanh, 0.5_real64, 2.0_real64, TANH_ROOT, &
                                 "x - tanh(2x)")

    end subroutine test_root_matches_a_bisection_oracle

    !> Asserts the solver, the oracle and the stated root agree for one function.
    subroutine compare_with_oracle(error, f, a, b, want, label)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.
        procedure(pf_rootfun_func)                    :: f     !! the function
        real(real64), intent(in)                   :: a     !! bracket, lower end
        real(real64), intent(in)                   :: b     !! bracket, upper end
        real(real64), intent(in)                   :: want  !! the stated root
        character(len=*), intent(in)               :: label !! names the function in a message

        type(pf_root_info) :: info
        real(real64)       :: x, oracle
        integer            :: n_oracle

        call pf_find_root(f, a, b, x, info=info)
        call bisection_oracle(f, a, b, oracle, n_oracle)
        call check(error, info%status == PF_ROOT_OK, label//" must converge")
        if (allocated(error)) return
        call check(error, abs(x - oracle) <= 2.0_real64*DEFAULT_BOUND*abs(oracle), &
                   label//": the solver and the bisection oracle must agree")
        if (allocated(error)) return
        call check(error, abs(oracle - want) <= 4.0_real64*epsilon(1.0_real64)*abs(want), &
                   label//": the oracle must reproduce the stated root")

    end subroutine compare_with_oracle

    !> Bisection to adjacent doubles, counting its evaluations: the slow, obviously correct way.
    subroutine bisection_oracle(f, a, b, x, neval)
        procedure(pf_rootfun_func)   :: f     !! the function
        real(real64), intent(in)  :: a     !! bracket, lower end
        real(real64), intent(in)  :: b     !! bracket, upper end; f(a) and f(b) differ in sign
        real(real64), intent(out) :: x     !! an end of the final, adjacent-double bracket
        integer, intent(out)      :: neval !! evaluations made

        real(real64) :: lo, hi, mid, flo, fmid

        lo = a
        hi = b
        flo = f(lo)
        neval = 2
        do
            mid = lo + 0.5_real64*(hi - lo)
            if (mid <= lo .or. mid >= hi) exit
            fmid = f(mid)
            neval = neval + 1
            if (fmid == 0.0_real64) then
                lo = mid
                exit
            end if
            if ((fmid > 0.0_real64) .eqv. (flo > 0.0_real64)) then
                lo = mid
                flo = fmid
            else
                hi = mid
            end if
        end do
        x = lo

    end subroutine bisection_oracle

    !> Brent's step must pay for itself: under half the evaluations bisection spends reaching the
    !! same root, on each of the five oracle functions.
    subroutine test_root_beats_bisection_on_evaluations(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        call beat_bisection(error, root_cubic, 2.0_real64, 3.0_real64, "x**3 - 2x - 5")
        if (allocated(error)) return
        call beat_bisection(error, root_cos_minus_x, 0.0_real64, 1.0_real64, "cos(x) - x")
        if (allocated(error)) return
        call beat_bisection(error, root_exp_minus_2, 0.0_real64, 1.0_real64, "exp(x) - 2")
        if (allocated(error)) return
        call beat_bisection(error, root_log_plus_x, 0.1_real64, 1.0_real64, "log(x) + x")
        if (allocated(error)) return
        call beat_bisection(error, root_x_minus_tanh, 0.5_real64, 2.0_real64, "x - tanh(2x)")

    end subroutine test_root_beats_bisection_on_evaluations

    !> Asserts the solver spends under half of the oracle's evaluations on one function.
    subroutine beat_bisection(error, f, a, b, label)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.
        procedure(pf_rootfun_func)                    :: f     !! the function
        real(real64), intent(in)                   :: a     !! bracket, lower end
        real(real64), intent(in)                   :: b     !! bracket, upper end
        character(len=*), intent(in)               :: label !! names the function in a message

        type(pf_root_info) :: info
        real(real64)       :: x, oracle
        integer            :: n_oracle

        call pf_find_root(f, a, b, x, info=info)
        call bisection_oracle(f, a, b, oracle, n_oracle)
        call check(error, 2*info%neval < n_oracle, &
                   label//": Brent's method must need under half of bisection's evaluations")

    end subroutine beat_bisection

    !> An exact zero is the answer the moment it is seen: at the lower end, at the upper end, and
    !! at an expansion probe.
    subroutine test_root_endpoint_that_is_a_root_returns_at_once(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        type(shifted_line)         :: line
        type(pf_root_info)         :: info
        type(pf_bracket_expansion) :: grow
        real(real64)               :: x

        line%root = 0.5_real64
        call pf_find_root(line, 0.5_real64, 1.0_real64, x, info=info)
        call check(error, x == 0.5_real64 .and. info%status == PF_ROOT_OK .and. info%neval == 1 &
                   .and. info%niter == 0 .and. line%calls == 1, &
                   "f(a) == 0 must return a after one evaluation, with no iteration")
        if (allocated(error)) return

        call pf_find_root(line, 0.0_real64, 0.5_real64, x, info=info)
        call check(error, x == 0.5_real64 .and. info%status == PF_ROOT_OK .and. info%neval == 2 &
                   .and. info%niter == 0, "f(b) == 0 must return b after two evaluations")
        if (allocated(error)) return

        ! 0.1 doubled twice is 0.4 exactly -- the same significand -- so the second probe lands on
        ! the root of x - 0.4 and must end the search there.
        line%root = 0.4_real64
        grow%mode = PF_EXPAND_UP
        call pf_find_root(line, 0.0_real64, 0.1_real64, x, expand=grow, info=info)
        call check(error, x == 0.4_real64 .and. info%status == PF_ROOT_OK .and. info%nexpand == 2 &
                   .and. info%niter == 0 .and. info%neval == 4, &
                   "an expansion probe landing on the root must return it at once")

    end subroutine test_root_endpoint_that_is_a_root_returns_at_once

    !> **The Improved Sheather-Jones shape**, as KDEpy's bracket search runs it: `[0, 1e-11]`
    !! doubled upward, limited at 1, until the ends differ in sign. `1e-11*2**35` is the first
    !! power-of-two multiple past 0.3, so the count is a fact, not a measurement.
    subroutine test_root_expansion_up_finds_the_bracket(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        type(pf_bracket_expansion) :: grow
        type(pf_root_info)         :: info
        real(real64)               :: x

        grow%mode = PF_EXPAND_UP
        grow%factor = 2.0_real64
        grow%upper_limit = 1.0_real64
        call pf_find_root(root_line_03, 0.0_real64, 1.0e-11_real64, x, expand=grow, info=info)
        call check(error, info%status == PF_ROOT_OK, "doubling [0, 1e-11] must find the root at 0.3")
        if (allocated(error)) return
        call check(error, info%nexpand, 35, "reaching past 0.3 from 1e-11 takes 35 doublings")
        if (allocated(error)) return
        call check(error, info%bracket_lo == 0.0_real64 .and. &
                   info%bracket_hi == 1.0e-11_real64*2.0_real64**35, &
                   "the lower end is held and the bracket solved on is [0, 1e-11*2**35]")
        if (allocated(error)) return
        call check(error, abs(x - 0.3_real64) <= DEFAULT_BOUND*0.3_real64, &
                   "the root must be 0.3 to the default bound")

    end subroutine test_root_expansion_up_finds_the_bracket

    !> Two roots, at 1 and 3, above a negative start: growing from below must stop at the first
    !! bracket that changes sign and solve for the smaller root, never probe past it for the
    !! second.
    subroutine test_root_expansion_stops_at_the_first_sign_change(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        type(pf_bracket_expansion) :: grow
        type(pf_root_info)         :: info
        real(real64)               :: x

        grow%mode = PF_EXPAND_UP
        call pf_find_root(root_two_roots, 0.0_real64, 0.1_real64, x, expand=grow, info=info)
        call check(error, info%status == PF_ROOT_OK .and. abs(x - 1.0_real64) <= DEFAULT_BOUND, &
                   "growing [0, 0.1] upward must return the smaller root, 1")
        if (allocated(error)) return
        ! 0.2, 0.4, 0.8 are below 1; 1.6 is the first probe between the roots.
        call check(error, info%nexpand, 4, "the first sign change is at the fourth doubling")
        if (allocated(error)) return
        call check(error, info%bracket_hi == 16.0_real64*0.1_real64, &
                   "the bracket solved on must end at the first probe past the root")

    end subroutine test_root_expansion_stops_at_the_first_sign_change

    !> **A missing sign change is a status**: a function with no root anywhere, grown until the
    !! tries run out, and grown to a limit, each return `PF_ROOT_NO_BRACKET` and the end with the
    !! smaller `|f|` -- the answer a caller retrying with a wider policy needs instead of an abort.
    subroutine test_root_expansion_exhausted_reports_no_bracket(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        type(pf_bracket_expansion) :: grow
        type(pf_root_info)         :: info
        real(real64)               :: x
        logical                    :: conv
        logical :: uf_ok, uf_was

        ! Extreme but legal inputs underflow inside the library; the flag is put back rather than
        ! left for nagfor to report at exit, unattributed (`.claude/rules/fortran-gotchas.md`).
        uf_ok = ieee_support_flag(ieee_underflow, 0.0_real64)
        if (uf_ok) call ieee_get_flag(ieee_underflow, uf_was)

        run: block
            grow%mode = PF_EXPAND_UP
            call pf_find_root(root_gentle, 0.0_real64, 1.0e-3_real64, x, expand=grow, converged=conv, &
                              info=info)
            call check(error, info%status == PF_ROOT_NO_BRACKET .and. .not. info%converged &
                       .and. .not. conv, "running out of tries must report PF_ROOT_NO_BRACKET")
            if (allocated(error)) exit run
            call check(error, info%nexpand == 64 .and. info%neval == 66, &
                       "the default policy makes 64 tries, one evaluation each")
            if (allocated(error)) exit run
            call check(error, x == 0.0_real64 .and. info%froot == 2.0_real64, &
                       "x must be the end with the smaller |f|, and froot its value")
            if (allocated(error)) exit run

            ! KDEpy's own stop: the upper end reaching 1. 1e-3*2**10 passes it, so the tenth try is
            ! the last, at the limit exactly.
            grow%upper_limit = 1.0_real64
            call pf_find_root(root_gentle, 0.0_real64, 1.0e-3_real64, x, expand=grow, info=info)
            call check(error, info%status == PF_ROOT_NO_BRACKET .and. info%nexpand == 10 &
                       .and. info%bracket_hi == 1.0_real64, &
                       "an expansion stopped by its limit must report PF_ROOT_NO_BRACKET at the limit")
            if (allocated(error)) exit run

            grow = pf_bracket_expansion(mode=PF_EXPAND_UP, max_tries=5)
            call pf_find_root(root_gentle, 0.0_real64, 1.0e-3_real64, x, expand=grow, info=info)
            call check(error, info%status == PF_ROOT_NO_BRACKET .and. info%nexpand == 5, &
                       "max_tries must cap the tries")

        end block run

        if (uf_ok) call ieee_set_flag(ieee_underflow, uf_was)
    end subroutine test_root_expansion_exhausted_reports_no_bracket

    !> No evaluation outside `[lower_limit, upper_limit]`, read from the record, in all three
    !! policies; an end reaching its limit is evaluated there exactly once.
    subroutine test_root_expansion_respects_the_limit(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        type(pf_bracket_expansion) :: grow
        type(pf_root_info)         :: info
        type(pf_root_history)      :: hist
        type(shifted_line)         :: line
        real(real64)               :: x
        integer                    :: i
        logical :: uf_ok, uf_was

        ! UP toward a root at 0.9 beyond a limit at 0.7: 0.2, 0.4, then 0.8 cut to 0.7.
        ! Extreme but legal inputs underflow inside the library; the flag is put back rather than
        ! left for nagfor to report at exit, unattributed (`.claude/rules/fortran-gotchas.md`).
        uf_ok = ieee_support_flag(ieee_underflow, 0.0_real64)
        if (uf_ok) call ieee_get_flag(ieee_underflow, uf_was)

        run: block
            line%root = 0.9_real64
            grow = pf_bracket_expansion(mode=PF_EXPAND_UP, upper_limit=0.7_real64)
            call pf_find_root(line, 0.0_real64, 0.1_real64, x, expand=grow, info=info, history=hist)
            call check(error, info%status == PF_ROOT_NO_BRACKET .and. info%nexpand == 3, &
                       "a root beyond the limit must not be found")
            if (allocated(error)) exit run
            call check(error, maxval(hist%x(1:hist%n)) == 0.7_real64, &
                       "the last probe must be at the limit exactly, and nothing beyond it")
            if (allocated(error)) exit run

            ! DOWN toward a root at -5 below a limit at -3.
            line%root = -5.0_real64
            grow = pf_bracket_expansion(mode=PF_EXPAND_DOWN, lower_limit=-3.0_real64)
            call pf_find_root(line, -1.0_real64, 0.0_real64, x, expand=grow, info=info, history=hist)
            call check(error, info%status == PF_ROOT_NO_BRACKET .and. &
                       minval(hist%x(1:hist%n)) == -3.0_real64, &
                       "DOWN must stop at its lower limit exactly")
            if (allocated(error)) exit run

            ! The limit that IS the root. `step_down` clamps a step that would overshoot onto the
            ! limit exactly, so the second try lands on -3 and the value there is an exact zero --
            ! which the widening has to take as the answer rather than press on and report
            ! NO_BRACKET for having reached its limit. The pair with the case above it is the point:
            ! a root BELOW the limit is unreachable, a root AT it is not.
            line%root = -3.0_real64
            grow = pf_bracket_expansion(mode=PF_EXPAND_DOWN, lower_limit=-3.0_real64)
            call pf_find_root(line, -1.0_real64, 0.0_real64, x, expand=grow, info=info)
            call check(error, info%status == PF_ROOT_OK .and. x == -3.0_real64 .and. info%froot == 0.0_real64, &
                       "an expansion landing exactly on a root at its own limit must report it")
            if (allocated(error)) exit run

            ! BOTH inside [-3, 5] from [0, 1]: each end reaches its limit, and an end at its limit is
            ! never evaluated again, so no point appears twice.
            grow = pf_bracket_expansion(mode=PF_EXPAND_BOTH, lower_limit=-3.0_real64, &
                                        upper_limit=5.0_real64)
            call pf_find_root(root_gentle, 0.0_real64, 1.0_real64, x, expand=grow, info=info, &
                              history=hist)
            call check(error, info%status == PF_ROOT_NO_BRACKET .and. info%bracket_lo == -3.0_real64 &
                       .and. info%bracket_hi == 5.0_real64, "BOTH must stop with both ends at their limits")
            if (allocated(error)) exit run
            call check(error, minval(hist%x(1:hist%n)) >= -3.0_real64 .and. &
                       maxval(hist%x(1:hist%n)) <= 5.0_real64, "BOTH must evaluate nothing outside its limits")
            if (allocated(error)) exit run
            do i = 2, hist%n
                call check(error, all(hist%x(1:i - 1) /= hist%x(i)), &
                           "an end already at its limit must not be evaluated again")
                if (allocated(error)) exit run
            end do

        end block run

        if (uf_ok) call ieee_set_flag(ieee_underflow, uf_was)
    end subroutine test_root_expansion_respects_the_limit

    !> DOWN finds a root below the bracket and BOTH one on either side, each by the structural
    !! count its policy implies -- roots an UP arm, the likeliest fall-through, can never reach.
    subroutine test_root_expansion_down_and_both(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        type(pf_bracket_expansion) :: grow
        type(pf_root_info)         :: info
        type(shifted_line)         :: line
        real(real64)               :: x

        ! DOWN from [-1, 0]: the lower end goes to -2, -4, -8, and -8 is past the root at -5.
        line%root = -5.0_real64
        grow%mode = PF_EXPAND_DOWN
        call pf_find_root(line, -1.0_real64, 0.0_real64, x, expand=grow, info=info)
        call check(error, info%status == PF_ROOT_OK .and. abs(x + 5.0_real64) <= 5.0_real64*DEFAULT_BOUND &
                   .and. info%nexpand == 3 .and. info%bracket_lo == -8.0_real64 &
                   .and. info%bracket_hi == 0.0_real64, "DOWN must find the root at -5 below [-1, 0]")
        if (allocated(error)) return

        ! BOTH from [-1, 1] toward a root at -10: the ends go out to +/-2, +/-4, +/-8, and the
        ! fourth try's LOWER end, -16, is past the root; the upper end stays at 8, unmoved.
        line%root = -10.0_real64
        grow%mode = PF_EXPAND_BOTH
        call pf_find_root(line, -1.0_real64, 1.0_real64, x, expand=grow, info=info)
        call check(error, info%status == PF_ROOT_OK .and. abs(x + 10.0_real64) <= 10.0_real64*DEFAULT_BOUND &
                   .and. info%nexpand == 4 .and. info%bracket_lo == -16.0_real64 &
                   .and. info%bracket_hi == 8.0_real64, &
                   "BOTH must move the lower end first and stop at the sign change it finds")
        if (allocated(error)) return

        ! And toward a root at +10: the fourth try's lower end changes nothing, its upper end does.
        line%root = 10.0_real64
        call pf_find_root(line, -1.0_real64, 1.0_real64, x, expand=grow, info=info)
        call check(error, info%status == PF_ROOT_OK .and. abs(x - 10.0_real64) <= 10.0_real64*DEFAULT_BOUND &
                   .and. info%nexpand == 4 .and. info%bracket_lo == -16.0_real64 &
                   .and. info%bracket_hi == 16.0_real64, "BOTH must find the root at +10 above [-1, 1]")

    end subroutine test_root_expansion_down_and_both

    !> The strict contract: no `expand=`, a `PF_EXPAND_NONE` policy, or a policy with no tries, on
    !! a bracket whose ends have the same sign, is `PF_ROOT_NO_BRACKET` after the two ends alone.
    subroutine test_root_no_expansion_requires_a_bracket(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        type(pf_bracket_expansion) :: grow
        type(pf_root_info)         :: info
        real(real64)               :: x
        logical :: uf_ok, uf_was

        ! Extreme but legal inputs underflow inside the library; the flag is put back rather than
        ! left for nagfor to report at exit, unattributed (`.claude/rules/fortran-gotchas.md`).
        uf_ok = ieee_support_flag(ieee_underflow, 0.0_real64)
        if (uf_ok) call ieee_get_flag(ieee_underflow, uf_was)

        run: block
            call pf_find_root(root_gentle, 0.0_real64, 1.0_real64, x, info=info)
            call check(error, info%status == PF_ROOT_NO_BRACKET .and. info%neval == 2 &
                       .and. info%nexpand == 0, "no expand= must mean no expansion")
            if (allocated(error)) exit run

            grow = pf_bracket_expansion(mode=PF_EXPAND_NONE, factor=10.0_real64, max_tries=100)
            call pf_find_root(root_gentle, 0.0_real64, 1.0_real64, x, expand=grow, info=info)
            call check(error, info%status == PF_ROOT_NO_BRACKET .and. info%neval == 2, &
                       "PF_EXPAND_NONE must not expand whatever its other components say")
            if (allocated(error)) exit run

            grow = pf_bracket_expansion(mode=PF_EXPAND_UP, max_tries=0)
            call pf_find_root(root_gentle, 0.0_real64, 1.0_real64, x, expand=grow, info=info)
            call check(error, info%status == PF_ROOT_NO_BRACKET .and. info%neval == 2, &
                       "max_tries = 0 must not expand")

        end block run

        if (uf_ok) call ieee_set_flag(ieee_underflow, uf_was)
    end subroutine test_root_no_expansion_requires_a_bracket

    !> `-Infinity` at an end is a sign: an infinite tail above the root, a sign change with no
    !! finite value at all, an expansion probe landing in the infinite tail, and infinite tails on
    !! both sides of a finite root. None may abort, and none may raise a flag -- read here with
    !! halting held off, so a regression is a failed assertion rather than the whole runner
    !! stopping under nagfor.
    subroutine test_root_infinite_endpoint_is_a_sign(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        type(pf_bracket_expansion) :: grow
        type(pf_root_info)         :: info(4)
        real(real64)               :: x(4)
        logical :: halting(size(ieee_usual)), saved(size(ieee_usual)), raised(size(ieee_usual))

        ! Held off, read and restored in this body, never in a helper: see `traps_can_be_held`.
        halting = .false.
        call ieee_get_flag(ieee_usual, saved)
#ifndef __flang__
        if (traps_can_be_held()) then
            call ieee_get_halting_mode(ieee_usual, halting)
            call ieee_set_halting_mode(ieee_usual, .false.)
        end if
#endif
        call ieee_set_flag(ieee_usual, .false.)

        call pf_find_root(root_neg_inf_tail, 0.0_real64, 5.0_real64, x(1), info=info(1))
        call pf_find_root(root_inf_step, 0.0_real64, 3.0_real64, x(2), info=info(2))
        grow = pf_bracket_expansion(mode=PF_EXPAND_UP, factor=64.0_real64)
        call pf_find_root(root_neg_inf_tail, 0.0_real64, 0.1_real64, x(3), expand=grow, info=info(3))
        call pf_find_root(root_inf_both_tails, -1.0_real64, 3.0_real64, x(4), info=info(4))

        call ieee_get_flag(ieee_usual, raised)
        call ieee_set_flag(ieee_usual, saved .or. raised)
#ifndef __flang__
        if (traps_can_be_held()) call ieee_set_halting_mode(ieee_usual, halting)
#endif

        call check(error, .not. any(raised), &
                   "a bracket with an infinite end must raise no overflow, invalid or divide-by-zero")
        if (allocated(error)) return
        call check(error, info(1)%status == PF_ROOT_OK .and. abs(x(1) - 1.0_real64) <= DEFAULT_BOUND, &
                   "1 - x with a -Infinity tail must give the root at 1")
        if (allocated(error)) return
        call check(error, info(2)%status == PF_ROOT_OK .and. abs(x(2) - 1.0_real64) <= DEFAULT_BOUND &
                   .and. .not. ieee_is_finite(info(2)%froot), &
                   "a sign change from -Infinity to +Infinity must be bisected down to the jump")
        if (allocated(error)) return
        call check(error, info(3)%status == PF_ROOT_OK .and. abs(x(3) - 1.0_real64) <= DEFAULT_BOUND &
                   .and. info(3)%nexpand == 1, "an expansion probe landing on -Infinity is a sign change")
        if (allocated(error)) return
        call check(error, info(4)%status == PF_ROOT_OK .and. &
                   abs(x(4) - 0.3_real64) <= DEFAULT_BOUND*0.3_real64, &
                   "a finite root between two infinite tails must be found")

    end subroutine test_root_infinite_endpoint_is_a_sign

    !> Values of order `1e-200` either side of the root, whose product underflows to zero, and of
    !! order `1e200`, whose product overflows: both are a sign change.
    subroutine test_root_tiny_values_still_bracket(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        type(pf_root_info) :: info
        real(real64)       :: x

        call pf_find_root(root_tiny_line, 0.0_real64, 1.0_real64, x, info=info)
        call check(error, info%status == PF_ROOT_OK .and. abs(x - 0.3_real64) <= DEFAULT_BOUND, &
                   "values of order 1e-200 either side of the root must bracket it")
        if (allocated(error)) return
        call pf_find_root(root_huge_line, 0.0_real64, 1.0_real64, x, info=info)
        call check(error, info%status == PF_ROOT_OK .and. abs(x - 0.3_real64) <= DEFAULT_BOUND, &
                   "values of order 1e200 either side of the root must bracket it")

    end subroutine test_root_tiny_values_still_bracket

    !> Running out of evaluations is `PF_ROOT_LIMIT` with a defined `x`: during Brent's method,
    !! after the first end, and during an expansion -- the last before the policy was used up.
    subroutine test_root_budget_is_spent(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        type(pf_bracket_expansion) :: grow
        type(pf_root_info)         :: info
        type(pf_root_history)      :: hist
        real(real64)               :: x
        logical                    :: conv
        logical :: uf_ok, uf_was

        ! Extreme but legal inputs underflow inside the library; the flag is put back rather than
        ! left for nagfor to report at exit, unattributed (`.claude/rules/fortran-gotchas.md`).
        uf_ok = ieee_support_flag(ieee_underflow, 0.0_real64)
        if (uf_ok) call ieee_get_flag(ieee_underflow, uf_was)

        run: block
            call pf_find_root(root_cos_minus_x, 0.0_real64, 1.0_real64, x, max_neval=3, converged=conv, &
                              info=info, history=hist)
            call check(error, info%status == PF_ROOT_LIMIT .and. .not. info%converged .and. .not. conv, &
                       "max_neval = 3 must stop the search with PF_ROOT_LIMIT")
            if (allocated(error)) exit run
            call check(error, info%neval == 3 .and. hist%n == 3, "max_neval must never be exceeded")
            if (allocated(error)) exit run
            call check(error, any(hist%x(1:3) == x) .and. info%froot == root_cos_minus_x(x), &
                       "x must be an evaluated point, and info%froot the value there")
            if (allocated(error)) exit run

            call pf_find_root(root_cos_minus_x, 0.0_real64, 1.0_real64, x, max_neval=1, info=info)
            call check(error, info%status == PF_ROOT_LIMIT .and. info%neval == 1 .and. x == 0.0_real64, &
                       "max_neval = 1 must evaluate a alone and return it")
            if (allocated(error)) exit run

            ! Two ends and three probes spend five evaluations before the policy's 64 tries are used.
            grow%mode = PF_EXPAND_UP
            call pf_find_root(root_gentle, 0.0_real64, 1.0e-3_real64, x, expand=grow, max_neval=5, info=info)
            call check(error, info%status == PF_ROOT_LIMIT .and. info%nexpand == 3 .and. info%neval == 5 &
                       .and. x == 0.0_real64, &
                       "a budget spent while expanding must report PF_ROOT_LIMIT, not PF_ROOT_NO_BRACKET")
            if (allocated(error)) exit run

            ! **The budget running out BETWEEN the two ends of one try**, which only BOTH can do: it
            ! moves the lower end and evaluates it before the upper end, so the budget can be gone by
            ! the time the upper one comes round. UP and DOWN move one end, so for them the check at
            ! the top of the try is the only one there is -- and that is the one the case above
            ! reaches. Three evaluations: the two given ends, then the lower end of the first try.
            grow%mode = PF_EXPAND_BOTH
            call pf_find_root(root_gentle, 0.0_real64, 1.0e-3_real64, x, expand=grow, max_neval=3, info=info)
            call check(error, info%status == PF_ROOT_LIMIT .and. info%nexpand == 1 .and. info%neval == 3, &
                       "a budget spent between one try's two ends must report PF_ROOT_LIMIT")

        end block run

        if (uf_ok) call ieee_set_flag(ieee_underflow, uf_was)
    end subroutine test_root_budget_is_spent

    !> A root of order `1e-9` -- a squared bandwidth on unit-scaled data -- comes back to the
    !! relative bound under the defaults, from a wide bracket and from the one an expansion builds.
    !! An `rtol` below the floor is the floor: the same run, bit for bit.
    subroutine test_root_relative_tolerance_at_a_tiny_root(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        type(pf_bracket_expansion) :: grow
        type(pf_root_info)         :: info, floored
        real(real64)               :: x, x_floored

        call pf_find_root(root_tiny_root, 0.0_real64, 1.0_real64, x, info=info)
        call check(error, info%status == PF_ROOT_OK .and. &
                   abs(x - TINY_ROOT) <= 2.0_real64*DEFAULT_BOUND*TINY_ROOT, &
                   "a root at 1.2e-9 must come back to about 8*epsilon relative, not to an absolute floor")
        if (allocated(error)) return

        call pf_find_root(root_tiny_root, 0.0_real64, 1.0_real64, x_floored, rtol=0.0_real64, &
                          info=floored)
        call check(error, x_floored == x .and. floored%neval == info%neval, &
                   "rtol = 0 must be raised to the 4*epsilon floor, the default")
        if (allocated(error)) return

        grow = pf_bracket_expansion(mode=PF_EXPAND_UP, upper_limit=1.0_real64)
        call pf_find_root(root_tiny_root, 0.0_real64, 1.0e-11_real64, x, expand=grow, info=info)
        call check(error, info%status == PF_ROOT_OK .and. &
                   abs(x - TINY_ROOT) <= 2.0_real64*DEFAULT_BOUND*TINY_ROOT, &
                   "the same root from the bracket an expansion builds")

    end subroutine test_root_relative_tolerance_at_a_tiny_root

    !> A root at exactly zero is the one place a relative tolerance cannot help, so `atol` decides.
    !!
    !! **`x**3` has a triple root at zero.** The bracket around it cannot become narrow RELATIVE to
    !! `|x|`, because `|x|` is going to zero with it, so `rtol*|x|` vanishes as fast as the bracket
    !! and the stopping test `2*eps*|x| + max(atol, rtol*|x|)/2` is met only through `atol`. Both
    !! halves are asserted, because either alone would pass with the two tolerances exchanged: the
    !! call WITH `atol` must converge, and the call without it must spend the budget and report
    !! `PF_ROOT_LIMIT`.
    !!
    !! Mutation proved: exchange `atol` and `rtol` in the stopping rule -- swap `tol_abs` and
    !! `tol_rel` where `stop_tolerance` forms `max(tol_abs, tol_rel*ax)`
    !! (`src/parquet_root_solve.f90`) -- and this test fails on `a root at zero must NOT converge on
    !! the relative tolerance alone`. It is the SECOND half that catches it, not the first: the
    !! exchange puts `rtol`, whose default is `4*epsilon`, into the ABSOLUTE slot, which is a
    !! positive floor the bracket at zero can meet, so the call with no `atol` converges when it
    !! must not. The first half still passes, because a mutant that converges too readily converges
    !! in that arm too.
    subroutine test_root_at_zero_needs_atol(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        type(pf_root_info) :: with_atol, without
        real(real64)       :: x

        call pf_find_root(root_cube, -1.0_real64, 2.0_real64, x, atol=1.0e-8_real64, info=with_atol)
        call check(error, with_atol%converged, "a root at zero must converge when atol is given")
        if (allocated(error)) return
        call check(error, abs(x) <= 1.0e-8_real64, "the root found must be zero to within atol")
        if (allocated(error)) return

        call pf_find_root(root_cube, -1.0_real64, 2.0_real64, x, info=without)
        call check(error, .not. without%converged, &
                   "a root at zero must NOT converge on the relative tolerance alone")
        if (allocated(error)) return
        call check(error, without%status == PF_ROOT_LIMIT, &
                   "the run without atol must end on PF_ROOT_LIMIT, having spent the budget")
        if (allocated(error)) return
        call check(error, without%neval > with_atol%neval, &
                   "the run without atol must cost more than the one with it")

    end subroutine test_root_at_zero_needs_atol

    !> A looser `atol` or `rtol` stops sooner, and the answer still meets what was asked for.
    subroutine test_root_tolerances_stop_early_when_asked(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        type(pf_root_info) :: tight, loose
        real(real64)       :: x

        call pf_find_root(root_cos_minus_x, 0.0_real64, 1.0_real64, x, info=tight)
        call pf_find_root(root_cos_minus_x, 0.0_real64, 1.0_real64, x, atol=1.0e-3_real64, info=loose)
        call check(error, loose%neval < tight%neval .and. abs(x - DOTTIE) <= 1.0e-3_real64 + DEFAULT_BOUND, &
                   "atol = 1e-3 must stop sooner and still be within 1e-3 of the root")
        if (allocated(error)) return
        call pf_find_root(root_cos_minus_x, 0.0_real64, 1.0_real64, x, rtol=1.0e-4_real64, info=loose)
        call check(error, loose%neval < tight%neval .and. &
                   abs(x - DOTTIE) <= 1.0e-4_real64*DOTTIE + DEFAULT_BOUND, &
                   "rtol = 1e-4 must stop sooner and still be within 1e-4 relative of the root")

    end subroutine test_root_tolerances_stop_early_when_asked

    !> The object form and the plain-function form of the same arithmetic agree to the bit, in
    !! every output, through an expansion and the solve after it.
    subroutine test_root_both_forms_agree(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        type(counted_sq2)          :: obj
        type(pf_bracket_expansion) :: grow
        type(pf_root_info)         :: io, ib
        type(pf_root_history)      :: ho, hb
        real(real64)               :: xo, xb

        grow%mode = PF_EXPAND_UP
        call pf_find_root(obj, 0.0_real64, 0.01_real64, xo, expand=grow, info=io, history=ho)
        call pf_find_root(root_sq2, 0.0_real64, 0.01_real64, xb, expand=grow, info=ib, history=hb)
        call check(error, xo == xb .and. io%froot == ib%froot .and. io%status == ib%status, &
                   "the two forms must return the same root and value, bit for bit")
        if (allocated(error)) return
        call check(error, io%neval == ib%neval .and. io%niter == ib%niter .and. io%nexpand == ib%nexpand &
                   .and. io%bracket_lo == ib%bracket_lo .and. io%bracket_hi == ib%bracket_hi, &
                   "the two forms must report the same counts and bracket")
        if (allocated(error)) return
        call check(error, ho%n == hb%n, "the two forms must record the same number of evaluations")
        if (allocated(error)) return
        call check(error, all(ho%x(1:ho%n) == hb%x(1:hb%n)) .and. all(ho%f(1:ho%n) == hb%f(1:hb%n)), &
                   "the two forms must evaluate the same points to the same values")
        if (allocated(error)) return
        call check(error, obj%calls, io%neval, "the object must have been called once per evaluation")

    end subroutine test_root_both_forms_agree

    !> The record is every evaluation in order -- the ends, each probe, each step -- trimmed to its
    !! length, and asking for it changes neither the answer nor the count.
    subroutine test_root_history_records_every_evaluation(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        type(pf_bracket_expansion) :: grow
        type(pf_root_info)         :: info, plain
        type(pf_root_history)      :: hist
        real(real64)               :: x, x_plain
        integer                    :: i

        ! Two ends, 35 probes and the solve: long enough to grow the record past its first block.
        grow = pf_bracket_expansion(mode=PF_EXPAND_UP, upper_limit=1.0_real64)
        call pf_find_root(root_line_03, 0.0_real64, 1.0e-11_real64, x, expand=grow, info=info, &
                          history=hist)
        call check(error, hist%n == info%neval .and. size(hist%x) == hist%n .and. &
                   size(hist%f) == hist%n, "the record must hold every evaluation and be trimmed to n")
        if (allocated(error)) return
        call check(error, hist%x(1) == 0.0_real64 .and. hist%x(2) == 1.0e-11_real64 .and. &
                   hist%x(3) == 2.0e-11_real64, "the record must start with the two ends, then the probes")
        if (allocated(error)) return
        do i = 1, hist%n
            call check(error, hist%f(i) == root_line_03(hist%x(i)), &
                       "each recorded value must be the function at its recorded point")
            if (allocated(error)) return
        end do

        call pf_find_root(root_line_03, 0.0_real64, 1.0e-11_real64, x_plain, expand=grow, info=plain)
        call check(error, x_plain == x .and. plain%neval == info%neval, &
                   "asking for the record must change neither the answer nor the count")

    end subroutine test_root_history_records_every_evaluation

    !> `1/(x - 0.5)` changes sign across a POLE: the solver converges on it exactly as on a root
    !! and reports success, and `info%froot` is what tells the two apart. Nothing rewrites the
    !! answer.
    subroutine test_root_pole_is_reported_through_froot(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        type(pf_root_info) :: info
        real(real64)       :: x

        call pf_find_root(root_pole, 0.0_real64, 1.0_real64, x, info=info)
        call check(error, info%status == PF_ROOT_OK .and. abs(x - 0.5_real64) <= DEFAULT_BOUND, &
                   "a sign change across a pole is converged on like a root")
        if (allocated(error)) return
        call check(error, abs(info%froot) > 1.0e10_real64, &
                   "info%froot must expose the pole: huge or infinite, where a root's is near zero")

    end subroutine test_root_pole_is_reported_through_froot

    !> **No legal call raises overflow, invalid or divide-by-zero.** Brackets at the ends of the
    !! range, an expansion that runs into `huge`, the widest bracket there is, values spanning the
    !! whole exponent range, huge tolerances, and a seeded stress run over every policy and every
    !! shape. Read here with halting held off, so a regression is a failed assertion under every
    !! compiler; under nagfor's default `-ieee=stop` it would otherwise end the process, and under
    !! the others pass silently.
    subroutine test_root_extreme_calls_raise_no_flag(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        real(real64), parameter :: FACTORS(7) = [1.0001_real64, 1.5_real64, 2.0_real64, 3.0_real64, &
                                                 10.0_real64, 1.0e10_real64, 1.0e300_real64]
        real(real64), parameter :: TOLS(5) = [0.0_real64, 1.0e-300_real64, 1.0e-10_real64, &
                                              1.0_real64, 1.0e300_real64]
        real(real64), parameter :: RTOLS(7) = [0.0_real64, 4.0e-16_real64, 1.0e-8_real64, &
                                               0.5_real64, 2.0_real64, 1.0e10_real64, 1.0e300_real64]
        real(real64), parameter :: SCALES(3) = [1.0e-300_real64, 1.0e-10_real64, 1.0_real64]
        type(pf_bracket_expansion) :: grow
        type(pf_root_info)         :: info, widest
        type(shifted_line)         :: line
        type(shaped_root)          :: shape
        real(real64)               :: x, a, b, u(12), x_widest
        integer(int64)             :: state
        integer                    :: i, j, nbad
        logical :: halting(size(ieee_usual)), saved(size(ieee_usual)), raised(size(ieee_usual))
        logical :: uf_ok, uf_was

        ! Held off, read and restored in this body, never in a helper: see `traps_can_be_held`.
        ! UNDERFLOW is saved and put back beside them but deliberately NOT asserted on: these
        ! calls are extreme by construction and underflow harmlessly, which is why the assertion
        ! below names the other three (`.claude/rules/fortran-gotchas.md`).
        uf_ok = ieee_support_flag(ieee_underflow, 0.0_real64)
        if (uf_ok) call ieee_get_flag(ieee_underflow, uf_was)
        halting = .false.
        call ieee_get_flag(ieee_usual, saved)
#ifndef __flang__
        if (traps_can_be_held()) then
            call ieee_get_halting_mode(ieee_usual, halting)
            call ieee_set_halting_mode(ieee_usual, .false.)
        end if
#endif
        call ieee_set_flag(ieee_usual, .false.)

        ! Expansions running into huge from both sides, including the one whose room, taken as a
        ! plain difference, rounds past huge (3*2**970 below it).
        call pf_find_root(root_gentle, 1.6e308_real64, 1.7e308_real64, x, &
                          expand=pf_bracket_expansion(mode=PF_EXPAND_UP), info=info)
        call pf_find_root(root_gentle, 1.6e308_real64, 1.7e308_real64, x, &
                          expand=pf_bracket_expansion(mode=PF_EXPAND_BOTH), info=info)
        call pf_find_root(root_gentle, -1.0e308_real64, -0.9e308_real64, x, &
                          expand=pf_bracket_expansion(mode=PF_EXPAND_UP), info=info)
        call pf_find_root(root_gentle, 3.0_real64*2.0_real64**970, 4.0_real64*2.0_real64**970, x, &
                          expand=pf_bracket_expansion(mode=PF_EXPAND_DOWN), info=info)
        ! A lower end put `huge` below this upper end rounds so far down that the width itself
        ! overflows: the case the width cap sits a few units of epsilon short of huge for.
        call pf_find_root(root_gentle, 5.6624424771458028e149_real64, 7.1107572039746096e307_real64, x, &
                          expand=pf_bracket_expansion(mode=PF_EXPAND_DOWN, factor=3.0_real64), info=info)
        call pf_find_root(root_gentle, -1.0_real64, 1.0_real64, x, &
                          expand=pf_bracket_expansion(mode=PF_EXPAND_BOTH, factor=1.0e300_real64), info=info)
        ! The widest bracket there is, values spanning the exponent range, and huge tolerances.
        line%root = 1.0e307_real64
        call pf_find_root(line, -0.8e308_real64, 0.8e308_real64, x_widest, info=widest)
        call pf_find_root(root_steep, 0.0_real64, 1.0_real64, x, info=info)
        line%root = 1.0e300_real64
        call pf_find_root(line, 0.5e300_real64, 2.0e300_real64, x, rtol=1.0e10_real64, info=info)
        call pf_find_root(line, 0.5e300_real64, 2.0e300_real64, x, atol=1.0e308_real64, info=info)

        ! The seeded stress run: a 31-bit linear congruential generator, formed in int64 so that no
        ! product overflows, gives the same cases under every compiler.
        state = 20260918_int64
        nbad = 0
        do i = 1, STRESS_CASES
            do j = 1, size(u)
                state = mod(1103515245_int64*state + 12345_int64, 2147483648_int64)
                u(j) = real(state, real64)/2147483648.0_real64
            end do
            a = spread_magnitude(u(1), u(2))
            b = spread_magnitude(u(3), u(4))
            if (a == b) cycle
            if (a > b) then
                x = a
                a = b
                b = x
            end if
            if ((0.5_real64*b) - (0.5_real64*a) > 0.5_real64*huge(1.0_real64)) cycle
            shape%kind = 1 + int(6.0_real64*u(5))
            shape%scale = SCALES(1 + int(3.0_real64*u(6)))
            shape%root = spread_magnitude(u(7), u(8))
            grow = pf_bracket_expansion(mode=int(4.0_real64*u(9)), &
                                        factor=FACTORS(1 + int(7.0_real64*u(10))), &
                                        max_tries=int(80.0_real64*u(11)))
            if (u(12) < 0.3_real64) then
                grow%lower_limit = a - abs(a)*u(12)
                grow%upper_limit = b + abs(b)*u(12)
            end if
            call pf_find_root(shape, a, b, x, expand=grow, atol=TOLS(1 + int(5.0_real64*u(11))), &
                              rtol=RTOLS(1 + int(7.0_real64*u(12))), info=info)
            if (.not. ieee_is_finite(x)) then
                nbad = nbad + 1
            else if (x < min(a, grow%lower_limit) .or. x > max(b, grow%upper_limit)) then
                nbad = nbad + 1
            end if
        end do

        call ieee_get_flag(ieee_usual, raised)
        call ieee_set_flag(ieee_usual, saved .or. raised)
        if (uf_ok) call ieee_set_flag(ieee_underflow, uf_was)
#ifndef __flang__
        if (traps_can_be_held()) call ieee_set_halting_mode(ieee_usual, halting)
#endif

        call check(error, .not. any(raised), &
                   "a legal call raised overflow, invalid or divide-by-zero inside the solver")
        if (allocated(error)) return
        call check(error, nbad, 0, "every stress case must return a finite x inside its reach")
        if (allocated(error)) return
        call check(error, widest%status == PF_ROOT_OK .and. &
                   abs(x_widest - 1.0e307_real64) <= DEFAULT_BOUND*1.0e307_real64, &
                   "the widest bracket there is must still be solved")

    end subroutine test_root_extreme_calls_raise_no_flag

    !> A value of any magnitude from `1e-308` to `9e307`, either sign, or zero: the stress run's
    !! bracket ends and roots.
    pure function spread_magnitude(p, q) result(v)
        real(real64), intent(in) :: p !! picks the decade, and zero for its lowest twentieth
        real(real64), intent(in) :: q !! picks the significand and the sign
        real(real64)             :: v !! the value

        integer :: decade

        decade = int(616.0_real64*p) - 308
        v = (1.0_real64 + 8.0_real64*q)*10.0_real64**decade
        if (q < 0.5_real64) v = -v
        if (p < 0.05_real64) v = 0.0_real64

    end function spread_magnitude

#ifndef __flang__
    !> Can overflow, invalid and divide-by-zero all be held off around a call?
    !!
    !! nagfor halts on the three by default, so a regression that raised one would take the whole
    !! runner down before the test could assert anything; holding halting off keeps it a test
    !! result. **That bracket is written out in each test's own body, and this inquiry is the only
    !! part a helper may carry**: F2018 17.3 restores the halting modes on return from any
    !! procedure other than `ieee_set_halting_mode`, and quietens a flag signalling on entry to a
    !! procedure until it returns, so a helper that set the modes or read the flags would change
    !! and see nothing (`fortran-gotchas.md`). `ieee_set_halting_mode` does not link under flang
    !! on macOS, which is what the preprocessor guard is for.
    function traps_can_be_held() result(can)
        logical :: can !! `ieee_support_halting` holds for overflow, invalid and divide-by-zero

        can = ieee_support_halting(ieee_overflow) .and. ieee_support_halting(ieee_invalid) &
            .and. ieee_support_halting(ieee_divide_by_zero)

    end function traps_can_be_held
#endif

    !> The `pf_find_root` row of the overshoot column in doc/pages/utilities/solvers.md,
    !! "The budget: max_neval": the page says NEVER, and this is what holds it there.
    !!
    !! **Why never is the right word.** Every evaluation goes through one site
    !! (`src/parquet_root_solve.f90`, the single `outcome%neval = outcome%neval + 1`), and each of
    !! the four places that can reach it tests `outcome%neval >= budget` first -- the expansion
    !! probe, the two bracket ends and the Brent step. A test that PRECEDES the work it guards is
    !! the whole of the claim, so `neval` reaches `max_neval` and stops there.
    !!
    !! **Two negative controls.** The bound alone is met by a call that does nothing, so each
    !! iteration also asserts the run evaluated at all; and the sweep must somewhere actually spend
    !! its budget -- `PF_ROOT_LIMIT`, which in this module means exactly "`max_neval` was spent
    !! first" -- counted and required non-zero. Without that count the bound is asserted over runs
    !! that converged long before the cap and never tested it.
    subroutine test_guide_budget_overshoot_bound(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        type(pf_root_info) :: info
        real(real64) :: x
        integer :: budget, seen_limit
        character(len=190) :: msg

        seen_limit = 0
        do budget = 1, 200
            ! A root that needs real work to find: `cos(x) - x` on a wide bracket, at the
            ! tightest tolerance the module will take, so a small budget genuinely runs out.
            call pf_find_root(root_cos_minus_x, -5.0_real64, 5.0_real64, x, &
                              rtol=4.0_real64*epsilon(1.0_real64), max_neval=budget, info=info)
            write(msg, '(a, i0, a, i0)') "pf_find_root must never exceed max_neval ", budget, &
                "; neval is ", info%neval
            call check(error, info%neval <= budget, trim(msg))
            if (allocated(error)) return
            write(msg, '(a, i0, a)') "at max_neval ", budget, &
                " the run must have evaluated something"
            call check(error, info%neval >= 1, trim(msg))
            if (allocated(error)) return
            if (info%status == PF_ROOT_LIMIT) then
                seen_limit = seen_limit + 1
                write(msg, '(a, i0, a, i0)') "PF_ROOT_LIMIT at max_neval ", budget, &
                    " means the budget was spent, so neval must equal it; it is ", info%neval
                call check(error, info%neval == budget, trim(msg))
                if (allocated(error)) return
            end if
        end do

        write(msg, '(a, i0, a)') "the budget must bind somewhere in the sweep; it bound ", &
            seen_limit, " times"
        call check(error, seen_limit > 0, trim(msg))

    end subroutine test_guide_budget_overshoot_bound

end module test_root
