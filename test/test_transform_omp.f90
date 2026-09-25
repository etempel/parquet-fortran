!> Threading tests for `parquet_transform`: that many threads transforming at once get the same
!> answers, bit for bit, as one thread does.
!!
!! **The claim under test is the guide page's own** -- `doc/pages/utilities/transforms.md`'s
!! "Thread safety" section tells the reader to "transform from as many threads as you like, each on
!! its own arrays", and `doc/pages/operating/thread-safety.md` lists this tier among those that
!! "share no state between calls", which is why that page carries no rule for it. Both give the
!! same mechanism: the module has no variable that is not a `parameter`, and each call allocates
!! its own workspace.
!!
!! **That is a claim about ABSENCE, so nothing in the source can be pointed at to prove it.** The
!! static half is `check_parquet_transform_holds_no_state`
!! (tools/check_source_conventions.py), which fails the moment a module-level variable appears --
!! deterministically, where a data race is a flaky detector. This suite is the other half: it
!! proves the promise the reader actually acts on, by running it. The regression both are aimed at
!! is the same one, and it is plausible rather than hypothetical: the engine rebuilds a table of
!! `n/2` twiddle factors on every call, and caching it in a module variable is the obvious
!! optimisation. The page forbids it in as many words -- "nothing is cached between calls".
!!
!! **This suite is registered SERIALLY** (`suite_is_safe_to_parallelize` in
!! `test/test_runner_support.f90` excludes `transform_omp`), for the nested-team reason
!! `test_integrate_omp.f90`'s header states: test-drive dispatches a suite's tests inside its own
!! `!$omp parallel do`, so a test opening a team of its own would open a nested one, which libgomp
!! deadlocks on intermittently. A serial suite runs at level 0, where the team this asserts
!! actually opens.
!!
!! **Every test here carries the skip guard**, because without OpenMP -- or on one thread -- its
!! assertions are not merely untestable but VACUOUS: the concurrent arm becomes a second serial
!! arm and "the answers agree" holds for the wrong reason. Such a build must report a skip here,
!! never a pass.
!!
!! Its only library import is `use parquet_transform`: it is registered in `run_tester_pf.f90`, the
!! runner that executes no `bind(C)` call, and `check_test_runner_partition` requires that the
!! files feeding that runner never reach `parquet_bindings`.
module test_transform_omp

    use testdrive, only : new_unittest, unittest_type, error_type, check, skip_test
    use parquet_transform
    use iso_fortran_env, only : real64, int64
#ifdef _OPENMP
    use omp_lib, only : omp_get_max_threads, omp_get_num_threads
#endif

    implicit none
    private

    public :: collect_tests_transform_omp

    !> Sequences transformed in each concurrent loop: enough that every thread runs many of them,
    !! and that a workspace shared where it should not be would be seen by more than one at once.
    integer, parameter :: CASES = 400
    !> Length of every sequence here: a power of two, large enough that the engine runs several
    !! butterfly stages and builds a twiddle table worth caching.
    integer, parameter :: N = 128

contains

    !> Registers every test in this suite.
    subroutine collect_tests_transform_omp(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:) !! Receives the suite's tests.

        testsuite = [ &
            new_unittest("all four transforms answer the serial bits from a team of threads", &
                         test_transforms_are_reentrant), &
            new_unittest("the team really opens and every sequence is really transformed", &
                         test_the_team_is_really_opened) &
            ]

    end subroutine collect_tests_transform_omp

    !> 400 different sequences through all four entry points, concurrently, against the answers the
    !> same calls give one at a time.
    !!
    !! **Bit for bit, not to a tolerance.** Every arm transforms the identical input by the
    !! identical code, so the only thing that can move a bit is state crossing between two calls --
    !! which is exactly the defect. A tolerance would hide a small one and prove nothing about a
    !! large one that happened to land close.
    !!
    !! Each sequence is different (its stream is its case number), so a workspace leaking between
    !! two threads produces a wrong ANSWER rather than merely a wrong call count.
    !!
    !! Both norms run, because `"ortho"` takes a second pass over the coefficients and scales one
    !! index differently from the rest -- a place a shared buffer would show up that `"none"` does
    !! not reach.
    subroutine test_transforms_are_reentrant(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        real(real64), allocatable :: x(:, :), wc(:, :), ws(:, :), wo(:, :)
        real(real64), allocatable :: gc(:, :), gs(:, :), go(:, :), rt(:, :)
        integer :: i

#ifdef _OPENMP
        if (omp_get_max_threads() < 2) then
            call skip_test(error, "needs at least two threads: on one the concurrent arm below is " // &
                "a second serial arm, and the bit-equality it asserts holds for the wrong reason")
            return
        end if
#else
        call skip_test(error, "needs OpenMP: without it the concurrent arm below is a second " // &
            "serial arm, and the bit-equality it asserts holds for the wrong reason")
        return
#endif

        allocate (x(N, CASES), wc(N, CASES), ws(N, CASES), wo(N, CASES))
        allocate (gc(N, CASES), gs(N, CASES), go(N, CASES), rt(N, CASES))
        do i = 1, CASES
            call fill_uniform(x(:, i), i)
        end do

        ! One at a time: the answers every concurrent call must reproduce exactly.
        do i = 1, CASES
            call pf_dct(x(:, i), wc(:, i))
            call pf_dst(x(:, i), ws(:, i))
            call pf_dct(x(:, i), wo(:, i), norm="ortho")
        end do

        gc = 0.0_real64
        gs = 0.0_real64
        go = 0.0_real64
        rt = 0.0_real64
        !$omp parallel do default(shared) private(i) schedule(dynamic, 8)
        do i = 1, CASES
            call pf_dct(x(:, i), gc(:, i))
            call pf_dst(x(:, i), gs(:, i))
            call pf_dct(x(:, i), go(:, i), norm="ortho")
            call pf_idct(gc(:, i), rt(:, i))
        end do
        !$omp end parallel do

        call check(error, all(gc == wc), "pf_dct from a team must answer the serial bits exactly")
        if (allocated(error)) return
        call check(error, all(gs == ws), "pf_dst from a team must answer the serial bits exactly")
        if (allocated(error)) return
        call check(error, all(go == wo), &
                   "pf_dct(norm='ortho') from a team must answer the serial bits exactly")
        if (allocated(error)) return

        ! The round trip is asserted separately: it is the one arm whose input was produced by
        ! another call in the same iteration, so a workspace outliving a call lands in it first.
        do i = 1, CASES
            call check(error, maxval(abs(rt(:, i) - x(:, i))) <= 64.0_real64*epsilon(1.0_real64), &
                       "pf_idct from a team must undo pf_dct")
            if (allocated(error)) return
        end do

    end subroutine test_transforms_are_reentrant

    !> The vacuity guard for the test above: that the region really ran wide, and really ran
    !> everything.
    !!
    !! Without this, a runtime handing back a team of one -- or a `schedule` that somehow visited
    !! no iteration -- would let every bit-equality above pass while proving nothing at all, since
    !! a serial arm compared against a serial arm always agrees. `seen` is written from inside the
    !! region by each iteration, and `team` by each thread, so both report what actually happened
    !! rather than what was asked for.
    subroutine test_the_team_is_really_opened(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        real(real64) :: x(N), y(N)
        integer, allocatable :: seen(:)
        integer :: i, team

#ifdef _OPENMP
        if (omp_get_max_threads() < 2) then
            call skip_test(error, "needs at least two threads: the team this asserts cannot open")
            return
        end if
#else
        call skip_test(error, "needs OpenMP: there is no team to assert")
        return
#endif

        allocate (seen(CASES))
        seen = 0
        team = 1
        !$omp parallel do default(shared) private(i, x, y) schedule(dynamic, 8)
        do i = 1, CASES
            call fill_uniform(x, i)
            call pf_dct(x, y)
#ifdef _OPENMP
            !$omp critical (transform_omp_team)
            team = max(team, omp_get_num_threads())
            !$omp end critical (transform_omp_team)
#endif
            seen(i) = 1
        end do
        !$omp end parallel do

        call check(error, team > 1, &
                   "the region ran on one thread, so every agreement this suite asserts is vacuous")
        if (allocated(error)) return
        call check(error, sum(seen), CASES, "every sequence must really have been transformed")

    end subroutine test_the_team_is_really_opened

    !> Fills `x` with values on `(-1, 1)`, exact in binary, from MINSTD (Park and Miller's
    !! `s = 48271*s mod (2**31 - 1)`): the same values under every compiler, and a different
    !! sequence for every `seed`, so no two cases here transform the same numbers.
    subroutine fill_uniform(x, seed)
        real(real64), intent(out) :: x(:)  !! receives the sequence
        integer, intent(in)       :: seed  !! selects the stream; from 1 to 2**31 - 2

        integer(int64) :: s
        integer :: i

        s = int(seed, int64)
        do i = 1, size(x)
            s = modulo(48271_int64*s, 2147483647_int64)
            x(i) = real(2_int64*s - 2147483647_int64, real64)/2147483647.0_real64
        end do

    end subroutine fill_uniform

end module test_transform_omp
