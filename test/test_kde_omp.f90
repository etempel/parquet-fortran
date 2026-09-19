!> Threading tests for `parquet_kde`: that `pf_kde_grid%add(threads=)` opens the team it resolves,
!> answers the same bits every time at one team size and within rounding of the serial deposit at
!> another, and that its two limits -- a floor of survivors per thread, and a cap from the partial
!> grids' own cost -- keep a team shut where one would not pay; and that the pilot an adaptive
!> `pf_kde%fit` builds answers the same bits at every thread count, its deposit cut by the sample
!> and not by the team.
!!
!! **Every threaded assertion reads `parquet_debug_kde_threads_used()` beside the answer**: two
!! team sizes differ only by rounding, and not at all against a deposit that never opened its
!! team, so an answer comparison alone would pass against a serial implementation. `threads=1` is
!! the negative control. **The serial answer is checked against the exact estimate too**, so that a
!! defect shared by both arms, above the point where they divide, cannot pass as agreement.
!!
!! **This suite is registered SERIALLY** (`suite_is_safe_to_parallelize` in
!! `test/test_runner_support.f90` excludes `kde_omp`): test-drive dispatches a suite's tests inside
!! its own `!$omp parallel do`, where a team opened by the library would be a nested one, which
!! libgomp deadlocks on intermittently (`feature_risks.md` Risk-104), and the counter is
!! process-global. A serial suite runs at level 0, where the team actually opens.
!!
!! **Every test carries the skip guard**: without OpenMP no team can open and every comparison
!! below would hold for the wrong reason. The thread count is clamped to the processors available,
!! so a process bound to one processor skips too.
!!
!! Its only library import is `use parquet_kde`: it is registered in `run_tester_pf.f90`, the
!! runner that executes no `bind(C)` call.
module test_kde_omp

    use testdrive, only : new_unittest, unittest_type, error_type, check, skip_test
    use parquet_kde
    use iso_fortran_env, only : int64, real64
#ifdef _OPENMP
    use omp_lib, only : omp_get_num_procs
#endif

    implicit none
    private

    public :: collect_tests_kde_omp

contains

    !> Registers every test in this suite.
    subroutine collect_tests_kde_omp(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:) !! Receives the suite's tests.

        testsuite = [ &
            new_unittest("%add(threads=) opens its team and changes the answer only by rounding", &
                test_add_team_changes_only_rounding), &
            new_unittest("%add's floor per thread and its grid cap keep a team shut", &
                test_add_limits_decide_the_team), &
            new_unittest("an adaptive %fit answers the same bits at every thread count", &
                test_adaptive_fit_ignores_the_team) &
            ]

    end subroutine collect_tests_kde_omp

    !> `n` values spread over about `[-500, 500]`, a pure function of the index, and their
    !> weights `1, 2, 3, 1, 2, 3, ...`.
    subroutine team_fixture(n, x, w)
        integer(int64), intent(in)             :: n    !! how many
        real(real64), allocatable, intent(out) :: x(:) !! the values
        real(real64), allocatable, intent(out) :: w(:) !! their weights
        integer(int64) :: i

        allocate(x(n), w(n))
        do i = 1_int64, n
            x(i) = 400.0_real64*sin(real(i, real64)*0.37_real64) + 100.0_real64*cos(real(i, real64)*1.1_real64)
            w(i) = real(1_int64 + modulo(i, 3_int64), real64)
        end do

    end subroutine team_fixture

    !> The same weighted deposit at one thread and at four, on a grid narrower than the data so
    !> that the weight counted beyond each end is summed across the team too.
    !!
    !! 1. `threads=1` runs serially and `threads=4` opens four: the negative control first, or the
    !!    comparisons below prove nothing about threading;
    !! 2. four threads twice give the same bits: the partition and the summation order are fixed
    !!    by the survivor count and the team size;
    !! 3. four threads and one agree to rounding in every cell and at both ends, and every count
    !!    and the total weight exactly;
    !! 4. the serial grid is the exact estimate at its centres, to the discrete normalisation's
    !!    residual -- the independent oracle beside the A/B.
    subroutine test_add_team_changes_only_rounding(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde_grid) :: g1, g4, g4b
        type(pf_kde) :: k
        real(real64), allocatable :: x(:), w(:)
        real(real64) :: r1(400), r4(400), r4b(400), c(400), f(400), fe(400), p1(2), p4(2)
        integer :: team1, team4

#ifndef _OPENMP
        call skip_test(error, "needs OpenMP: without it no team can open at any thread count, so " // &
            "the one-thread and four-thread deposits below would be the same serial code and agree " // &
            "for the wrong reason")
        return
#endif
#ifdef _OPENMP
        if (omp_get_num_procs() < 2) then
            call skip_test(error, "needs two processors: the thread count is clamped to the " // &
                "processors available, so a process bound to one opens no team")
            return
        end if
#endif
        call team_fixture(50000_int64, x, w)
        call g1%init(400, -200.0_real64, 200.0_real64, 5.0_real64)
        call g1%add(x, weights=w, threads=1)
        team1 = parquet_debug_kde_threads_used()
        call g4%init(400, -200.0_real64, 200.0_real64, 5.0_real64)
        call g4%add(x, weights=w, threads=4)
        team4 = parquet_debug_kde_threads_used()
        call g4b%init(400, -200.0_real64, 200.0_real64, 5.0_real64)
        call g4b%add(x, weights=w, threads=4)

        call check(error, team1 == 1, "threads=1 must deposit serially; the team observable says it did not")
        if (allocated(error)) return
        call check(error, team4 == 4, &
            "threads=4 must open a team of four, or every comparison below compares the serial " // &
            "deposit with itself")
        if (allocated(error)) return

        call g1%density(r1, normalise=.false.)
        call g4%density(r4, normalise=.false.)
        call g4b%density(r4b, normalise=.false.)
        call check(error, all(r4 == r4b), "one team size must give the same bits every time")
        if (allocated(error)) return
        call check(error, maxval(abs(r4 - r1)) <= 1.0e-13_real64*maxval(r1), &
            "four threads and one must agree to rounding in every cell")
        if (allocated(error)) return
        call g1%cdf([-200.0_real64, 200.0_real64], p1)
        call g4%cdf([-200.0_real64, 200.0_real64], p4)
        call check(error, p1(1) > 0.1_real64 .and. p1(2) < 0.9_real64 .and. &
            all(abs(p4 - p1) <= 1.0e-14_real64), &
            "the weight counted beyond each end must be summed across the team")
        if (allocated(error)) return
        call check(error, g4%n() == g1%n() .and. g4%n_valid() == g1%n_valid() .and. &
            g4%sum_weights() == g1%sum_weights(), "every count and the total weight must not depend on the team")
        if (allocated(error)) return

        ! Away from the ends: a kernel crossing an end has its in-range share spread over the
        ! centres to the midpoint rule's accuracy, which is the grid's own O(step**2), while a kernel
        ! wholly inside is exact to the far smaller residual of the Gaussian's cut.
        call k%fit(x, bandwidth=5.0_real64, weights=w)
        call g1%density(f, x=c)
        call k%pdf(c, fe)
        call check(error, maxval(abs(f - fe), mask=abs(c) <= 150.0_real64) <= 1.0e-6_real64*maxval(fe), &
            "the serial grid must be the exact estimate at its centres")

    end subroutine test_add_team_changes_only_rounding

    !> The two limits on `%add`'s team, each asserted against a control that does open one:
    !!
    !! * 1500 survivors at `threads=4` stay serial, since each thread would get fewer than the
    !!   floor;
    !! * 50000 survivors on a million cells, each kernel reaching about ten of them, stay serial,
    !!   since four partial grids of a million cells cost more than the deposit they would share;
    !! * the same 50000 survivors on 400 cells open the four.
    subroutine test_add_limits_decide_the_team(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde_grid) :: g
        real(real64), allocatable :: x(:), w(:)
        integer :: team_floor, team_cap, team_open

#ifndef _OPENMP
        call skip_test(error, "needs OpenMP: without it every team is one thread, so a limit that " // &
            "keeps the team shut cannot be told from one that does nothing")
        return
#endif
#ifdef _OPENMP
        if (omp_get_num_procs() < 2) then
            call skip_test(error, "needs two processors: the thread count is clamped to the " // &
                "processors available, so a process bound to one opens no team")
            return
        end if
#endif
        call team_fixture(50000_int64, x, w)
        call g%init(400, -600.0_real64, 600.0_real64, 5.0_real64)
        call g%add(x(1:1500), threads=4)
        team_floor = parquet_debug_kde_threads_used()
        call g%init(1000000, -600.0_real64, 600.0_real64, 0.0012_real64)
        call g%add(x, threads=4)
        team_cap = parquet_debug_kde_threads_used()
        call g%init(400, -600.0_real64, 600.0_real64, 5.0_real64)
        call g%add(x, threads=4)
        team_open = parquet_debug_kde_threads_used()
        call check(error, team_open == 4, "the control: 50000 survivors on 400 cells must open four threads")
        if (allocated(error)) return
        call check(error, team_floor == 1, "1500 survivors must stay below the floor of a team of two")
        if (allocated(error)) return
        call check(error, team_cap == 1, &
            "a million cells, each kernel reaching ten, must keep the team shut: the partial grids " // &
            "would cost more than the deposit")

    end subroutine test_add_limits_decide_the_team

    !> The pilot an adaptive `%fit` builds is deposited in pieces the sample and the pilot decide,
    !> which the team takes in turn, so `threads=1` and `threads=4` give every point the same
    !> bandwidth and the estimate the same bits, while the team counter shows that four threads
    !> took the pieces. The pilot, a grid of 50000 points in dozens of pieces, is the exact fixed
    !> estimate at its centres -- the oracle beside the A/B, which a piece deposited twice or not at
    !> all would pass. An adaptive grid's `%add(threads=)` is the fixed grid's weaker promise: four
    !> threads agree with one to rounding.
    subroutine test_adaptive_fit_ignores_the_team(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde) :: k1, k4, kf
        type(pf_kde_grid) :: p, g1, g4
        real(real64), allocatable :: x(:), w(:), h1(:), h4(:), c(:), fc(:), fe(:)
        real(real64) :: t(200), f1(200), f4(200), r1(400), r4(400)
        integer :: team1, team4, team_add, i

#ifndef _OPENMP
        call skip_test(error, "needs OpenMP: without it the pilot's pieces are always deposited on one " // &
            "thread, so the one-thread and four-thread fits below would agree for the wrong reason")
        return
#endif
#ifdef _OPENMP
        if (omp_get_num_procs() < 2) then
            call skip_test(error, "needs two processors: the thread count is clamped to the " // &
                "processors available, so a process bound to one opens no team")
            return
        end if
#endif
        call team_fixture(50000_int64, x, w)
        call k1%fit(x, bandwidth=5.0_real64, weights=w, adaptive=.true., threads=1)
        team1 = parquet_debug_kde_threads_used()
        call k4%fit(x, bandwidth=5.0_real64, weights=w, adaptive=.true., threads=4)
        team4 = parquet_debug_kde_threads_used()
        call check(error, team1 == 1, "threads=1 must build the pilot serially; the team observable says it did not")
        if (allocated(error)) return
        call check(error, team4 == 4, &
            "threads=4 must build the pilot on a team of four, or the comparison below compares the " // &
            "serial fit with itself")
        if (allocated(error)) return
        allocate(h1(k1%n_valid()), h4(k4%n_valid()))
        call k1%bandwidths(h1)
        call k4%bandwidths(h4)
        do i = 1, 200
            t(i) = -500.0_real64 + 5.0_real64*real(i, real64)
        end do
        call k1%pdf(t, f1)
        call k4%pdf(t, f4)
        call check(error, all(h4 == h1) .and. all(f4 == f1) .and. maxval(h1) > 1.5_real64*minval(h1), &
            "an adaptive fit must give the same bits at every thread count")
        if (allocated(error)) return

        call k1%pilot(p)
        allocate(c(p%ncells()), fc(p%ncells()), fe(p%ncells()))
        call p%density(fc, x=c)
        call kf%fit(x, bandwidth=5.0_real64, weights=w)
        call kf%pdf(c, fe)
        call check(error, maxval(abs(fc - fe)) <= 1.0e-6_real64*maxval(fe), &
            "the pilot, deposited in pieces, must be the exact fixed estimate at its centres")
        if (allocated(error)) return

        call k4%pilot(p)
        call g1%init(400, -200.0_real64, 200.0_real64, 5.0_real64, pilot=p)
        call g1%add(x, weights=w, threads=1)
        call g4%init(400, -200.0_real64, 200.0_real64, 5.0_real64, pilot=p)
        call g4%add(x, weights=w, threads=4)
        team_add = parquet_debug_kde_threads_used()
        call g1%density(r1, normalise=.false.)
        call g4%density(r4, normalise=.false.)
        call check(error, team_add == 4 .and. maxval(abs(r4 - r1)) <= 1.0e-13_real64*maxval(r1), &
            "an adaptive grid's four-thread deposit must open its team and agree with one thread's to rounding")

    end subroutine test_adaptive_fit_ignores_the_team

end module test_kde_omp
