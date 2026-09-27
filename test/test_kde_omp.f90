!> Threading tests for `parquet_kde`: that `pf_kde_grid%add(threads=)`, exact or binned, opens the
!> team it resolves, answers the same bits every time at one team size and within rounding of the
!> serial deposit at another, and that its two limits -- a floor of survivors per thread, and a cap
!> from the partial grids' own cost -- keep a team shut where one would not pay; that the pilot an
!> adaptive `pf_kde%fit` builds answers the same bits at every thread count, its deposit cut by the
!> sample and not by the team; that the bulk queries and `%sample` of both forms answer the same
!> bits at every thread count and open a team only where the work pays for it; and that objects
!> fitted and queried inside a caller's own region, one per iteration or one shared by every
!> thread, answer what they answer serially.
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
!! libgomp deadlocks on intermittently, and the counter is process-global. A serial suite runs at
!! level 0, where the team actually opens.
!!
!! **Every test carries the skip guard**: without OpenMP no team can open and every comparison
!! below would hold for the wrong reason. The thread count is clamped to the processors available,
!! so a process bound to fewer processors than the team a test asserts skips too.
!!
!! Its only library import is `use parquet_kde`: it is registered in `run_tester_pf.f90`, the
!! runner that executes no `bind(C)` call.
module test_kde_omp

    use testdrive, only : new_unittest, unittest_type, error_type, check, skip_test
    use parquet_kde
    use iso_fortran_env, only : int64, real64
#ifdef _OPENMP
    use omp_lib, only : omp_get_num_procs, omp_get_thread_num
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
            new_unittest("a binned grid's %add(threads=) opens its team and changes the answer only by rounding", &
                test_binned_add_team_changes_only_rounding), &
            new_unittest("%add's floor per thread and its grid cap keep a team shut", &
                test_add_limits_decide_the_team), &
            new_unittest("an adaptive %fit answers the same bits at every thread count", &
                test_adaptive_fit_ignores_the_team), &
            new_unittest("the bulk queries and %sample answer the same bits at every thread count", &
                test_queries_ignore_the_team), &
            new_unittest("a bulk query opens a team only where the work pays for it", &
                test_query_team_floor), &
            new_unittest("the grid's bulk queries answer the same bits at every thread count", &
                test_grid_queries_ignore_the_team), &
            new_unittest("one object per iteration inside a caller's region answers the serial bits", &
                test_one_object_per_iteration), &
            new_unittest("one fitted object shared by every thread answers the serial bits", &
                test_one_object_shared) &
            ]

    end subroutine collect_tests_kde_omp

    !> `pf_kde_grid`'s three bulk queries take `threads=` and answer the same bits at every count.
    !>
    !> Each element is computed by one thread alone, so the team can only change the time; the team
    !> observable is read beside every answer, with `threads=1` as the negative control, because an
    !> answer comparison cannot tell a team that ran from one that never opened.
    subroutine test_grid_queries_ignore_the_team(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde_grid) :: g
        real(real64), allocatable :: x(:), w(:), t(:), f1(:), f4(:), c1(:), c4(:), pr(:), q1(:), q4(:)
        integer(int64) :: i, n
        integer :: team1(3), team4(3)

#ifndef _OPENMP
        call skip_test(error, "needs OpenMP: without it no team can open at any thread count, so " // &
            "the one-thread and four-thread queries below would be the same serial code and agree " // &
            "for the wrong reason")
        return
#endif
#ifdef _OPENMP
        if (omp_get_num_procs() < 4) then
            call skip_test(error, "needs four processors: the thread count is clamped to the " // &
                "processors available, so threads=4 cannot open the team of four asserted below")
            return
        end if
#endif
        call team_fixture(20000_int64, x, w)
        call g%init(400, -500.0_real64, 500.0_real64, 5.0_real64)
        call g%add(x, weights=w, finish=.true.)
        ! Long enough that the team's own floor of work per thread is passed.
        n = 400000_int64
        allocate(t(n), pr(n), f1(n), f4(n), c1(n), c4(n), q1(n), q4(n))
        do i = 1_int64, n
            t(i) = -500.0_real64 + 1000.0_real64*real(i - 1_int64, real64)/real(n - 1_int64, real64)
            pr(i) = real(i, real64)/real(n + 1_int64, real64)
        end do
        call g%pdf(t, f1, threads=1)
        team1(1) = parquet_debug_kde_threads_used()
        call g%pdf(t, f4, threads=4)
        team4(1) = parquet_debug_kde_threads_used()
        call g%cdf(t, c1, threads=1)
        team1(2) = parquet_debug_kde_threads_used()
        call g%cdf(t, c4, threads=4)
        team4(2) = parquet_debug_kde_threads_used()
        call g%quantile(pr, q1, threads=1)
        team1(3) = parquet_debug_kde_threads_used()
        call g%quantile(pr, q4, threads=4)
        team4(3) = parquet_debug_kde_threads_used()

        call check(error, all(team1 == 1), &
            "threads=1 must run every grid query serially; the team observable says not")
        if (allocated(error)) return
        call check(error, all(team4 == 4), &
            "threads=4 must open a team of four for every grid query, or the comparisons below " // &
            "compare the serial answers with themselves")
        if (allocated(error)) return
        call check(error, all(f4 == f1) .and. all(c4 == c1) .and. all(q4 == q1), &
            "every grid bulk query must give the same bits at every thread count")
        if (allocated(error)) return
        ! And the serial answers are the scalar forms', which nothing about threading can reach.
        call g%pdf(t(7), f1(1))
        call g%cdf(t(7), c1(1))
        call g%quantile(pr(7), q1(1))
        call check(error, f1(1) == f4(7) .and. c1(1) == c4(7) .and. q1(1) == q4(7), &
            "a grid's bulk query must equal its scalar form at every element")

    end subroutine test_grid_queries_ignore_the_team

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
        if (omp_get_num_procs() < 4) then
            call skip_test(error, "needs four processors: the thread count is clamped to the " // &
                "processors available, so threads=4 cannot open the team of four asserted below")
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

        call g1%finish()
        call g1%density(r1, normalise=.false.)
        call g4%finish()
        call g4%density(r4, normalise=.false.)
        call g4b%finish()
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

    !> The same four assertions for `method="binned"`, whose `%add(threads=)` makes the exact
    !> method's promise: each thread bins a static share of the points into a private bin array,
    !> the arrays are added in thread order, so one team size gives the same bits every time and
    !> two team sizes group the additions differently and agree to rounding -- not to the bit.
    !!
    !! The oracle is the exact estimate again, to the binning's own error rather than the exact
    !! grid's -- about `1.4e-4` of the peak away from the ends at this cell width, asserted at
    !! `1e-3` -- so that a defect both arms share, above the point where they divide, cannot pass as
    !! agreement. A thread's share lost or binned twice is the agreement assertion's to catch.
    subroutine test_binned_add_team_changes_only_rounding(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde_grid) :: g1, g4, g4b
        type(pf_kde) :: k
        real(real64), allocatable :: x(:), w(:)
        real(real64) :: r1(400), r4(400), r4b(400), c(400), f(400), fe(400), p1(2), p4(2)
        integer :: team1, team4

#ifndef _OPENMP
        call skip_test(error, "needs OpenMP: without it no team can open at any thread count, so " // &
            "the one-thread and four-thread binnings below would be the same serial code and agree " // &
            "for the wrong reason")
        return
#endif
#ifdef _OPENMP
        if (omp_get_num_procs() < 4) then
            call skip_test(error, "needs four processors: the thread count is clamped to the " // &
                "processors available, so threads=4 cannot open the team of four asserted below")
            return
        end if
#endif
        call team_fixture(50000_int64, x, w)
        call g1%init(400, -200.0_real64, 200.0_real64, 5.0_real64, method="binned")
        call g1%add(x, weights=w, threads=1)
        team1 = parquet_debug_kde_threads_used()
        call g4%init(400, -200.0_real64, 200.0_real64, 5.0_real64, method="binned")
        call g4%add(x, weights=w, threads=4)
        team4 = parquet_debug_kde_threads_used()
        call g4b%init(400, -200.0_real64, 200.0_real64, 5.0_real64, method="binned")
        call g4b%add(x, weights=w, threads=4)

        call check(error, team1 == 1, "threads=1 must bin serially; the team observable says it did not")
        if (allocated(error)) return
        call check(error, team4 == 4, &
            "threads=4 must open a team of four, or every comparison below compares the serial " // &
            "binning with itself")
        if (allocated(error)) return

        call g1%finish()
        call g1%density(r1, normalise=.false.)
        call g4%finish()
        call g4%density(r4, normalise=.false.)
        call g4b%finish()
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

        call k%fit(x, bandwidth=5.0_real64, weights=w)
        call g1%density(f, x=c)
        call k%pdf(c, fe)
        call check(error, maxval(abs(f - fe), mask=abs(c) <= 150.0_real64) <= 1.0e-3_real64*maxval(fe), &
            "the serial binned grid must be the exact estimate at its centres, to the binning's error")

    end subroutine test_binned_add_team_changes_only_rounding

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
        if (omp_get_num_procs() < 4) then
            call skip_test(error, "needs four processors: the thread count is clamped to the " // &
                "processors available, so threads=4 cannot open the team of four asserted below")
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
        if (omp_get_num_procs() < 4) then
            call skip_test(error, "needs four processors: the thread count is clamped to the " // &
                "processors available, so threads=4 cannot open the team of four asserted below")
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
        call g1%finish()
        call g1%density(r1, normalise=.false.)
        call g4%finish()
        call g4%density(r4, normalise=.false.)
        call check(error, team_add == 4 .and. maxval(abs(r4 - r1)) <= 1.0e-13_real64*maxval(r1), &
            "an adaptive grid's four-thread deposit must open its team and agree with one thread's to rounding")

    end subroutine test_adaptive_fit_ignores_the_team

    !> The cut Gaussian's density at `z` standard deviations, written here from the intrinsic `exp`
    !> rather than read from the library: `phi(z)/(1 - 2*Phi(-5))` inside the cut, 0 beyond.
    pure function cut_pdf(z) result(k)
        real(real64), intent(in) :: z !! the offset, in standard deviations
        real(real64)             :: k !! the density

        k = 0.0_real64
        if (abs(z) <= 5.0_real64) k = exp(-0.5_real64*z*z)/(sqrt(2.0_real64*acos(-1.0_real64))*(1.0_real64 - &
            erfc(5.0_real64/sqrt(2.0_real64))))

    end function cut_pdf

    !> The cut Gaussian's distribution function at `z`, from the intrinsic `erfc`: 0 below the cut, 1
    !> above it, `(Phi(z) - Phi(-5))/(1 - 2*Phi(-5))` between.
    pure function cut_cdf(z) result(c)
        real(real64), intent(in) :: z !! the offset, in standard deviations
        real(real64)             :: c !! the mass at or below it

        real(real64) :: tail

        tail = 0.5_real64*erfc(5.0_real64/sqrt(2.0_real64))
        c = 0.0_real64
        if (z >= 5.0_real64) then
            c = 1.0_real64
        else if (z > -5.0_real64) then
            c = (0.5_real64*erfc(-z/sqrt(2.0_real64)) - tail)/(1.0_real64 - 2.0_real64*tail)
        end if

    end function cut_cdf

    !> Every bulk query of a weighted, bounded fit, and both forms' `%sample`, at `threads=1` and at
    !> `threads=4`: the counter reads 1 and then 4 -- the negative control first -- and the answers
    !> agree bit for bit, each element being computed by one thread alone. Beside the A/B, the
    !> serial answers against an independent oracle: `%pdf` and `%cdf` against the kernel sums
    !> written out here from `exp` and `erfc`, each point's kernel renormalised at the bound;
    !> `%quantile` inverting `%cdf`; `%curve` being `%pdf` at its points; and the sample's mean the
    !> population's to five standard errors. An adaptive fit's `%pdf` and `%sample` agree across
    !> the two counts as well.
    !>
    !> **`NP` is a hundred rather than four hundred, and `NQ` a thousand rather than two.** What is
    !> split across the team is the QUERY vector, one element per thread, so the number of query
    !> points is what the bitwise comparison needs -- a thousand is two hundred and fifty per
    !> thread -- and not how long each one takes. A `%quantile` point costs `KDE_QUANTILE_STEPS`
    !> `%cdf` evaluations over the whole sample, which made four hundred of them 43% of this
    !> suite's entire running time (1.05 s of 2.43 s, measured) while adding nothing the first
    !> hundred had not already asserted: the inversion is checked point by point against `%cdf`,
    !> so the grid's JOB is to span `(0, 1)`, which a hundred points do at 1% resolution. The two
    !> counts are still far above the team floor, and `team4` is asserted rather than assumed, so a
    !> reduction that took a query below `KDE_QUERY_MIN_WORK` would fail here rather than quietly
    !> compare the serial answer with itself.
    subroutine test_queries_ignore_the_team(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        real(real64), parameter :: H = 5.0_real64, LO = -450.0_real64
        integer, parameter :: NQ = 1000, NP = 100, NS = 100000, NL = 200
        type(pf_kde) :: k, ka, kl
        type(pf_kde_grid) :: g
        real(real64), allocatable :: x(:), w(:), t(:), f1(:), f4(:), c1(:), c4(:), s1(:), s4(:)
        real(real64) :: p(NP), q1(NP), q4(NP), qc(NP), xc1(NQ), xc4(NQ), fc1(NQ), fc4(NQ), fe, ce, img, wsum
        real(real64) :: gap_f, gap_c, mean, xbar
        real(real64) :: tl(NL), pl(NL), cl1(NL), cl4(NL), ql1(NL), ql4(NL), sl1(NL), sl4(NL)
        integer :: team1(7), team4(7), i
        integer(int64) :: j

#ifndef _OPENMP
        call skip_test(error, "needs OpenMP: without it no query can open a team, so the one-thread and " // &
            "four-thread answers below would be the same serial code and agree for the wrong reason")
        return
#endif
#ifdef _OPENMP
        if (omp_get_num_procs() < 4) then
            call skip_test(error, "needs four processors: the thread count is clamped to the " // &
                "processors available, so threads=4 cannot open the team of four asserted below")
            return
        end if
#endif
        call team_fixture(50000_int64, x, w)
        call k%fit(x, kernel="gaussian", bandwidth=H, weights=w, lower=LO)
        allocate(t(NQ), f1(NQ), f4(NQ), c1(NQ), c4(NQ), s1(NS), s4(NS))
        do i = 1, NQ
            t(i) = LO + 950.0_real64*modulo(real(i, real64)*0.6180339887498949_real64, 1.0_real64)
        end do
        do i = 1, NP
            p(i) = (real(i, real64) - 0.5_real64)/real(NP, real64)
        end do

        call k%pdf(t, f1, threads=1)
        team1(1) = parquet_debug_kde_threads_used()
        call k%pdf(t, f4, threads=4)
        team4(1) = parquet_debug_kde_threads_used()
        call k%cdf(t, c1, threads=1)
        team1(2) = parquet_debug_kde_threads_used()
        call k%cdf(t, c4, threads=4)
        team4(2) = parquet_debug_kde_threads_used()
        call k%quantile(p, q1, threads=1)
        team1(3) = parquet_debug_kde_threads_used()
        call k%quantile(p, q4, threads=4)
        team4(3) = parquet_debug_kde_threads_used()
        call k%curve(xc1, fc1, threads=1)
        team1(4) = parquet_debug_kde_threads_used()
        call k%curve(xc4, fc4, threads=4)
        team4(4) = parquet_debug_kde_threads_used()
        call k%sample(s1, 99_int64, 3, threads=1)
        team1(5) = parquet_debug_kde_threads_used()
        call k%sample(s4, 99_int64, 3, threads=4)
        team4(5) = parquet_debug_kde_threads_used()
        call check(error, all(team1(1:5) == 1), "threads=1 must run every bulk query serially; the team observable says not")
        if (allocated(error)) return
        call check(error, all(team4(1:5) == 4), &
            "threads=4 must open a team of four for every bulk query, or the comparisons below compare the " // &
            "serial answers with themselves")
        if (allocated(error)) return
        call check(error, all(f4 == f1) .and. all(c4 == c1) .and. all(q4 == q1) .and. all(xc4 == xc1) .and. &
            all(fc4 == fc1) .and. all(s4 == s1), "every bulk query and %sample must give the same bits at every thread count")
        if (allocated(error)) return

        ! The oracle: every point's cut kernel plus its mirror image about the bound, which is what
        ! `"reflect"` sums -- the correction a bound without `boundary=` resolves to. With one bound
        ! the images keep the whole mass inside the support, so nothing is divided by a mass; the
        ! two terms subtracted at the bound are a point's mass below it and its mirror's, which sum
        ! to one.
        wsum = sum(w, mask=x >= LO)
        gap_f = 0.0_real64
        gap_c = 0.0_real64
        ! Every tenth query point: a hundred of them, as before, each an O(sample) kernel sum.
        do i = 1, NQ, 10
            fe = 0.0_real64
            ce = 0.0_real64
            do j = 1_int64, size(x, kind=int64)
                if (x(j) < LO) cycle
                img = 2.0_real64*LO - x(j)
                fe = fe + w(j)*(cut_pdf((t(i) - x(j))/H) + cut_pdf((t(i) - img)/H))
                ce = ce + w(j)*((cut_cdf((t(i) - x(j))/H) + cut_cdf((t(i) - img)/H)) &
                    - (cut_cdf((LO - x(j))/H) + cut_cdf((LO - img)/H)))
            end do
            gap_f = max(gap_f, abs(f1(i) - fe/(H*wsum)))
            gap_c = max(gap_c, abs(c1(i) - ce/wsum))
        end do
        call check(error, gap_f <= 1.0e-12_real64*maxval(f1) .and. gap_c <= 1.0e-12_real64, &
            "the serial %pdf and %cdf must be the kernel sums written out independently")
        if (allocated(error)) return
        call k%cdf(q1, qc)
        call k%pdf(xc1, f4)
        call check(error, maxval(abs(qc - p)) <= 1.0e-12_real64 .and. all(f4(1:NQ) == fc1), &
            "the serial %quantile must invert %cdf and %curve must be %pdf at its points")
        if (allocated(error)) return
        xbar = sum(w*x, mask=x >= LO)/wsum
        mean = sum(s1)/real(NS, real64)
        call check(error, abs(mean - xbar) <= 5.0_real64*300.0_real64/sqrt(real(NS, real64)), &
            "the serial sample's mean must be the population's, to five standard errors")
        if (allocated(error)) return

        call ka%fit(x, bandwidth=H, weights=w, adaptive=.true., lower=LO)
        call ka%pdf(t, f1, threads=1)
        call ka%pdf(t, f4, threads=4)
        team4(5) = parquet_debug_kde_threads_used()
        call ka%sample(s1, 5_int64, threads=1)
        call ka%sample(s4, 5_int64, threads=4)
        call check(error, team4(5) == 4 .and. all(f4 == f1) .and. all(s4 == s1), &
            "an adaptive fit's %pdf and %sample must give the same bits at every thread count")
        if (allocated(error)) return

        ! Under `"linear"` a query inside a zone integrates its window point by point, which is
        ! what has to answer the same bits at every thread count: a smaller fixture and fewer
        ! points, one such query costing many times a plain one.
        call kl%fit(x(1:400), bandwidth=H, lower=minval(x(1:400)), boundary="linear")
        do i = 1, NL
            tl(i) = minval(x(1:400)) + 60.0_real64*modulo(real(i, real64)*0.6180339887498949_real64, 1.0_real64)
            pl(i) = (real(i, real64) - 0.5_real64)/real(NL, real64)
        end do
        call kl%cdf(tl, cl1, threads=1)
        team1(6) = parquet_debug_kde_threads_used()
        call kl%cdf(tl, cl4, threads=4)
        team4(6) = parquet_debug_kde_threads_used()
        call kl%quantile(pl, ql1, threads=1)
        call kl%quantile(pl, ql4, threads=4)
        call kl%sample(sl1, 9_int64, threads=1)
        call kl%sample(sl4, 9_int64, threads=4)
        call check(error, team1(6) == 1 .and. team4(6) == 4 .and. all(cl4 == cl1) .and. &
            all(ql4 == ql1) .and. all(sl4 == sl1), &
            "a linear fit's %cdf, %quantile and %sample must open their team and give the same bits at every count")
        if (allocated(error)) return

        call g%init(500, -500.0_real64, 500.0_real64, H)
        call g%add(x, weights=w)
        call g%finish()
        call g%sample(s1, 7_int64, 2_int64, threads=1)
        team1(7) = parquet_debug_kde_threads_used()
        call g%sample(s4, 7_int64, 2_int64, threads=4)
        team4(7) = parquet_debug_kde_threads_used()
        mean = sum(s1)/real(NS, real64)
        call check(error, team1(7) == 1 .and. team4(7) == 4 .and. all(s4 == s1) .and. &
            abs(mean - sum(w*x)/sum(w)) <= 5.0_real64*300.0_real64/sqrt(real(NS, real64)), &
            "the grid's %sample must open its team, give the same bits at every thread count, and have the mean")

    end subroutine test_queries_ignore_the_team

    !> A bulk query's team is sized by its work: twenty density queries over two hundred points stay
    !> serial at `threads=4`, as do a thousand draws, while the controls -- two thousand queries over
    !> fifty thousand points, and a hundred thousand draws -- open the four. Each limit asserted
    !> against a control that does open a team, so that a floor that did nothing would be seen.
    subroutine test_query_team_floor(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde) :: small, big
        real(real64), allocatable :: x(:), w(:), v(:)
        real(real64) :: t(2000), f(2000)
        integer :: team_small, team_big, team_few, team_many, i

#ifndef _OPENMP
        call skip_test(error, "needs OpenMP: without it every team is one thread, so a floor that keeps " // &
            "the team shut cannot be told from one that does nothing")
        return
#endif
#ifdef _OPENMP
        if (omp_get_num_procs() < 4) then
            call skip_test(error, "needs four processors: the thread count is clamped to the " // &
                "processors available, so threads=4 cannot open the team of four asserted below")
            return
        end if
#endif
        call team_fixture(50000_int64, x, w)
        do i = 1, 2000
            t(i) = -500.0_real64 + real(i, real64)*0.5_real64
        end do
        call small%fit(x(1:200), bandwidth=5.0_real64)
        call big%fit(x, bandwidth=5.0_real64)
        call big%pdf(t, f, threads=4)
        team_big = parquet_debug_kde_threads_used()
        call small%pdf(t(1:20), f(1:20), threads=4)
        team_small = parquet_debug_kde_threads_used()
        allocate(v(100000))
        call big%sample(v, 1_int64, threads=4)
        team_many = parquet_debug_kde_threads_used()
        call big%sample(v(1:1000), 1_int64, threads=4)
        team_few = parquet_debug_kde_threads_used()
        call check(error, team_big == 4 .and. team_many == 4, &
            "the controls: two thousand queries over fifty thousand points, and a hundred thousand draws, must open four")
        if (allocated(error)) return
        call check(error, team_small == 1, "twenty queries over two hundred points must stay serial")
        if (allocated(error)) return
        call check(error, team_few == 1, "a thousand draws must stay serial")

    end subroutine test_query_team_floor

    !> Two thousand estimates fitted and queried inside the caller's own `!$omp parallel do`, one
    !> object per iteration held in an array made before the region, each over its own sample: every
    !> density and every draw equals what a fresh object answers serially afterwards, bit for bit,
    !> and every query inside the region stood down to one thread (the counter, read after each
    !> iteration's calls, never exceeds 1).
    subroutine test_one_object_per_iteration(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        integer, parameter :: NOBJ = 2000, M = 40
        type(pf_kde), allocatable :: ks(:)
        type(pf_kde) :: k
        real(real64) :: xs(M, NOBJ), t(5), f(5, NOBJ), v(5, NOBJ), fs(5), vs(5)
        integer :: seen(NOBJ), i, j, bad

#ifndef _OPENMP
        call skip_test(error, "needs OpenMP: the point is the caller's own region, which cannot open without it")
        return
#endif
#ifdef _OPENMP
        if (omp_get_num_procs() < 2) then
            call skip_test(error, "needs two processors: a region of one thread shares nothing")
            return
        end if
#endif
        do i = 1, NOBJ
            do j = 1, M
                xs(j, i) = 10.0_real64*sin(real(i*M + j, real64)*0.37_real64) + 0.01_real64*real(i, real64)
            end do
        end do
        do j = 1, 5
            t(j) = -12.0_real64 + 5.0_real64*real(j, real64)
        end do
        allocate(ks(NOBJ))
        !$omp parallel do num_threads(4) schedule(dynamic, 16) default(shared) private(i)
        do i = 1, NOBJ
            call ks(i)%fit(xs(:, i), rule="silverman", adaptive=(mod(i, 2) == 0))
            call ks(i)%pdf(t, f(:, i))
            call ks(i)%sample(v(:, i), int(i, int64))
            seen(i) = parquet_debug_kde_threads_used()
        end do
        !$omp end parallel do

        call check(error, maxval(seen) == 1, "every query inside the caller's region must stand down to one thread")
        if (allocated(error)) return
        bad = 0
        do i = 1, NOBJ
            call k%fit(xs(:, i), rule="silverman", adaptive=(mod(i, 2) == 0))
            call k%pdf(t, fs)
            call k%sample(vs, int(i, int64))
            if (any(fs /= f(:, i)) .or. any(vs /= v(:, i))) bad = bad + 1
        end do
        call check(error, bad == 0, "every object fitted inside the region must answer the serial bits")

    end subroutine test_one_object_per_iteration

    !> One fitted object and one grid, shared by every thread of the caller's region: each thread's
    !> densities, probabilities, quantiles and draws -- at its own points and on its own stream --
    !> equal the serial answers bit for bit, since every query takes the object `intent(in)` and
    !> writes nothing.
    subroutine test_one_object_shared(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        integer, parameter :: NT = 4, NQ = 300, NL = 60
        type(pf_kde) :: k, kl
        type(pf_kde_grid) :: g, p
        real(real64), allocatable :: x(:), w(:)
        real(real64) :: t(NQ, NT), f(NQ, NT), c(NQ, NT), q(NQ, NT), v(NQ, NT), gf(NQ, NT), gv(NQ, NT)
        real(real64) :: fs(NQ), cs(NQ), qs(NQ), vs(NQ), gfs(NQ), gvs(NQ), pr(NQ)
        real(real64) :: cl(NL, NT), vl(NL, NT), cls(NL), vls(NL)
        integer :: i, j, tid, bad

#ifndef _OPENMP
        call skip_test(error, "needs OpenMP: the point is several threads sharing one object")
        return
#endif
#ifdef _OPENMP
        if (omp_get_num_procs() < 2) then
            call skip_test(error, "needs two processors: a region of one thread shares nothing")
            return
        end if
#endif
        call team_fixture(20000_int64, x, w)
        call k%fit(x, bandwidth=5.0_real64, weights=w, adaptive=.true., lower=-450.0_real64)
        call k%pilot(p)
        call g%init(400, -450.0_real64, 500.0_real64, 5.0_real64, pilot=p, lower=-450.0_real64)
        call g%add(x, weights=w, finish=.true.)
        do j = 1, NT
            do i = 1, NQ
                t(i, j) = -440.0_real64 + 930.0_real64*modulo(real(i + NQ*j, real64)*0.6180339887498949_real64, &
                    1.0_real64)
            end do
        end do
        do i = 1, NQ
            pr(i) = (real(i, real64) - 0.5_real64)/real(NQ, real64)
        end do
        !$omp parallel num_threads(NT) default(shared) private(tid)
        tid = 1
#ifdef _OPENMP
        tid = omp_get_thread_num() + 1
#endif
        call k%pdf(t(:, tid), f(:, tid))
        call k%cdf(t(:, tid), c(:, tid))
        call k%quantile(pr, q(:, tid))
        call k%sample(v(:, tid), 3_int64, tid)
        ! The grid was finished BEFORE the region: `%finish` writes, and a write shared by four
        ! threads is a race whatever it writes. Only the queries are shared here.
        call g%pdf(t(:, tid), gf(:, tid))
        call g%sample(gv(:, tid), 3_int64, tid)
        !$omp end parallel

        bad = 0
        do j = 1, NT
            call k%pdf(t(:, j), fs)
            call k%cdf(t(:, j), cs)
            call k%quantile(pr, qs)
            call k%sample(vs, 3_int64, j)
            call g%pdf(t(:, j), gfs)
            call g%sample(gvs, 3_int64, j)
            if (any(fs /= f(:, j)) .or. any(cs /= c(:, j)) .or. any(qs /= q(:, j)) .or. any(vs /= v(:, j)) .or. &
                any(gfs /= gf(:, j)) .or. any(gvs /= gv(:, j))) bad = bad + 1
        end do
        call check(error, bad == 0, "every thread sharing one fitted object and one grid must get the serial bits")
        if (allocated(error)) return

        ! Under `"linear"` every query inside a zone integrates its window point by point, so this
        ! is the arm where `pf_integrate` runs from every thread of the caller's own region at once.
        ! A smaller fixture and fewer points: one such query costs many times a plain one.
        call kl%fit(x(1:400), bandwidth=5.0_real64, lower=minval(x(1:400)), boundary="linear")
        !$omp parallel num_threads(NT) default(shared) private(tid)
        tid = 1
#ifdef _OPENMP
        tid = omp_get_thread_num() + 1
#endif
        call kl%cdf(t(1:NL, tid), cl(:, tid))
        call kl%sample(vl(:, tid), 5_int64, tid)
        !$omp end parallel
        bad = 0
        do j = 1, NT
            call kl%cdf(t(1:NL, j), cls)
            call kl%sample(vls, 5_int64, j)
            if (any(cls /= cl(:, j)) .or. any(vls /= vl(:, j))) bad = bad + 1
        end do
        call check(error, bad == 0, &
            "every thread sharing one linear fit must get the serial bits of %cdf and %sample")

    end subroutine test_one_object_shared

end module test_kde_omp
