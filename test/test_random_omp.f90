!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!> The claim `parquet_random` exists to make: the same values under every OpenMP schedule.
!>
!> Everything else in the suite checks that the module computes the right numbers. This checks the
!> only property that motivated writing it at all -- that a parallel loop gets the same numbers as
!> a serial one, whatever the schedule and whatever the thread count. A stateful generator cannot
!> do this at any speed, and no amount of locking fixes it: with shared state, which value an
!> iteration receives depends on how many draws happened first.
!>
!> **This suite is excluded from test-drive's per-test parallelism** (see
!> `suite_is_safe_to_parallelize` in `run_tester.f90`), and the reason is specific rather than
!> precautionary. test-drive dispatches a suite's tests inside its own `!$omp parallel do`, so a
!> parallel region opened by a test is NESTED -- and with nesting disabled by default it gets a
!> team of one, which would make every schedule comparison below pass without ever running two
!> threads. Excluding the suite gives these regions a real team. Enabling nested parallelism
!> instead would mutate process-global OpenMP state underneath concurrently running sibling
!> suites, which is exactly the hazard the exclusion list exists for.
module test_random_omp

    use parquet
    use iso_fortran_env, only: int32, int64, real64
    use testdrive, only: new_unittest, unittest_type, error_type, check
#ifdef _OPENMP
    use omp_lib, only: omp_get_max_threads, omp_get_num_threads
#endif

    implicit none
    private
    public :: collect_tests_parquet_random_omp

    !> How many draws each arm of the schedule comparison produces.
    integer, parameter :: n = 20000
    !> The seed every arm uses. Any value would do; a fixed one keeps the test deterministic.
    integer(int64), parameter :: seed = 20260816_int64

contains

    !> Registers every test in the `random_omp` suite.
    subroutine collect_tests_parquet_random_omp(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)   !! the suite's tests

        testsuite = [ &
            new_unittest("draws are identical under every schedule and thread count", test_schedule_independence), &
            new_unittest("pf_random_seed differs across concurrent threads", test_seed_across_threads), &
            new_unittest("a per-iteration stream reproduces under every schedule", test_stream_schedule), &
            new_unittest("a bulk permutation is bit-identical at every thread count", test_perm_threads), &
            new_unittest("a resample is bit-identical at every thread count", test_resample_threads) &
            ]
    end subroutine collect_tests_parquet_random_omp

    !> Fills the same array five ways and requires every value to be bit-identical.
    !!
    !! Three things here are load-bearing and each is easy to leave out:
    !!
    !!  * **Variable per-iteration work.** Without it a single thread can claim the whole loop
    !!    before the others start, and the test passes without ever having tested anything. Each
    !!    iteration therefore does an amount of extra work that depends on its index -- extra draws
    !!    that are stored, so they cannot be optimised away, and that provably do not disturb the
    !!    value being checked, since the module has no state for them to disturb.
    !!  * **A vacuity guard on the team size.** If a region gets a team of one, every arm is really
    !!    the serial arm and the comparison is empty. That must fail loudly rather than pass quietly.
    !!    The guard asserts what the region ACHIEVED, which is the only thing that answers the
    !!    question -- an ambient thread count says what was available, not what OpenMP handed out.
    !!  * **Two different thread counts, one of them not a divisor of `n`.** A schedule that
    !!    happens to partition the loop the same way twice would agree for the wrong reason.
    !!
    !! **The first two arms ASK for their team rather than accepting the ambient one**, via
    !! `num_threads(want)` where `want` is `max(2, omp_get_max_threads())`. On any ordinary machine
    !! that is the ambient count and nothing changes. On a one-core machine, or under
    !! `OMP_NUM_THREADS=1`, or in a container that pins the count, it is 2 -- and that is the
    !! difference between a correct library failing its own suite and being tested properly. The
    !! `num_threads` clause overrides the ambient count for that region, which is exactly what
    !! `test_seed_across_threads` below has always relied on. The vacuity guard is unchanged and
    !! still fires if a team of one is somehow handed out anyway; it is now a real assertion about
    !! OpenMP rather than a report that the environment was small.
    subroutine test_schedule_independence(error)
        type(error_type), allocatable, intent(out) :: error   !! set on the first failed assertion
        real(real64) :: serial(n), stat(n), dyn(n), few(n), many(n)
        integer(int64) :: burn(n)
        integer :: i, team_static, team_dynamic, want

        team_static = 1
        team_dynamic = 1
        want = 2
#ifdef _OPENMP
        want = max(2, omp_get_max_threads())
#endif

        do i = 1, n
            serial(i) = draw(i, burn(i))
        end do

#ifdef _OPENMP
        !$omp parallel do schedule(static) num_threads(want) default(shared) private(i)
        do i = 1, n
            if (i == 1) team_static = omp_get_num_threads()
            stat(i) = draw(i, burn(i))
        end do
        !$omp end parallel do

        !$omp parallel do schedule(dynamic, 1) num_threads(want) default(shared) private(i)
        do i = 1, n
            ! Captured from whichever thread runs iteration 1 -- under schedule(dynamic,1) that
            ! is not thread 0, and keying the capture on thread 0 would leave this reading 1
            ! even with a full team, i.e. a false alarm from the guard rather than a real one.
            if (i == 1) team_dynamic = omp_get_num_threads()
            dyn(i) = draw(i, burn(i))
        end do
        !$omp end parallel do

        !$omp parallel do schedule(static) num_threads(2) default(shared) private(i)
        do i = 1, n
            few(i) = draw(i, burn(i))
        end do
        !$omp end parallel do

        ! 7 does not divide n, so this schedule's blocks land differently from every arm above.
        !$omp parallel do schedule(dynamic, 3) num_threads(7) default(shared) private(i)
        do i = 1, n
            many(i) = draw(i, burn(i))
        end do
        !$omp end parallel do

        call check(error, team_static > 1, &
            "vacuity guard: the schedule(static) region ran with a team of one, so nothing was varied -- " // &
            "check that this suite is still excluded from test-drive's own per-test parallelism")
        if (allocated(error)) return
        call check(error, team_dynamic > 1, &
            "vacuity guard: the schedule(dynamic,1) region ran with a team of one, so nothing was varied")
        if (allocated(error)) return
#else
        ! Without OpenMP there is no schedule to vary, so there is nothing this test can assert.
        ! It is genuinely vacuous here rather than falsely green: the arms are copies of the serial
        ! loop, and saying so is better than skipping, because a build that silently lost OpenMP
        ! would otherwise look the same as one that never had it.
        stat = serial
        dyn = serial
        few = serial
        many = serial
#endif

        call check(error, all(stat == serial), "schedule(static) produced different values from a serial loop")
        if (allocated(error)) return
        call check(error, all(dyn == serial), "schedule(dynamic,1) produced different values from a serial loop")
        if (allocated(error)) return
        call check(error, all(few == serial), "a 2-thread run produced different values from a serial loop")
        if (allocated(error)) return
        call check(error, all(many == serial), "a 7-thread run produced different values from a serial loop")
        if (allocated(error)) return
        call check(error, all(serial >= 0.0_real64 .and. serial < 1.0_real64), &
            "a draw escaped [0, 1) -- the arms agree but on the wrong values")
    end subroutine test_schedule_independence

    !> A `pf_random_stream` seeded per iteration reproduces under every schedule and thread count.
    !!
    !! This is the tier-1 analogue of the test above, and it is the case tier 1 exists for: each
    !! iteration draws a **data-dependent** number of values, which is exactly what a coordinate
    !! cannot express in advance. What makes it reproducible is the discipline the guide states --
    !! seed from a run-invariant label at the top of the iteration -- not anything about the type.
    !!
    !! **The stream is declared in a `block`, never in a `private()` clause**, and that is
    !! load-bearing rather than stylistic: it is the library-wide rule for a per-thread derived type
    !! (`feature_risks.md` Risk-45). `pf_random_stream` is deliberately plain scalars with no
    !! allocatable components and no `FINAL`, which is what keeps *both* shapes safe here -- but the
    !! example a user copies must be the one that stays safe if the type ever gains either.
    subroutine test_stream_schedule(error)
        type(error_type), allocatable, intent(out) :: error   !! set on the first failed assertion
        real(real64) :: serial(n), stat(n), dyn(n), many(n)
        integer :: i, team_dynamic, want

        team_dynamic = 1
        want = 2
#ifdef _OPENMP
        want = max(2, omp_get_max_threads())
#endif

        do i = 1, n
            serial(i) = stream_walk(i)
        end do

#ifdef _OPENMP
        !$omp parallel do schedule(static) num_threads(want) default(shared) private(i)
        do i = 1, n
            stat(i) = stream_walk(i)
        end do
        !$omp end parallel do

        !$omp parallel do schedule(dynamic, 1) num_threads(want) default(shared) private(i)
        do i = 1, n
            if (i == 1) team_dynamic = omp_get_num_threads()
            dyn(i) = stream_walk(i)
        end do
        !$omp end parallel do

        !$omp parallel do schedule(dynamic, 3) num_threads(7) default(shared) private(i)
        do i = 1, n
            many(i) = stream_walk(i)
        end do
        !$omp end parallel do

        call check(error, team_dynamic > 1, &
            "vacuity guard: the schedule(dynamic,1) region ran with a team of one, so nothing was varied")
        if (allocated(error)) return
#else
        stat = serial
        dyn = serial
        many = serial
#endif

        call check(error, all(stat == serial), "a streamed schedule(static) run differed from the serial one")
        if (allocated(error)) return
        call check(error, all(dyn == serial), "a streamed schedule(dynamic,1) run differed from the serial one")
        if (allocated(error)) return
        call check(error, all(many == serial), "a streamed 7-thread run differed from the serial one")
        if (allocated(error)) return
        call check(error, all(serial >= 0.0_real64 .and. serial < 1.0_real64), &
            "a streamed draw escaped [0, 1) -- the arms agree but on the wrong values")
    end subroutine test_stream_schedule

    !> One iteration's streamed work: seed from the iteration's own label, then draw until a
    !! condition on the values themselves is met, and return the last value drawn.
    !!
    !! The loop count depends on the draws, so no coordinate names the result in advance -- which is
    !! what makes this a tier-1 test rather than a tier-0 one wearing a different hat. The bound
    !! keeps a pathological stream from running long; it is not the usual exit.
    function stream_walk(i) result(last)
        integer, intent(in) :: i                    !! the iteration's run-invariant label
        real(real64) :: last                        !! the last value this iteration drew
        block
            type(pf_random_stream) :: rng           ! in a block, NOT in private() -- see Risk-45
            real(real64) :: x
            integer :: taken
            call rng%seed(20260816_int64, i)
            last = 0.0_real64
            taken = 0
            do
                call rng%uniform(x)
                last = x
                taken = taken + 1
                if (x < 0.2_real64 .or. taken >= 64) exit
            end do
        end block
    end function stream_walk

    !> One draw plus an index-dependent amount of extra, stored work.
    !!
    !! The extra draws exist only to make iterations cost different amounts, so that no schedule
    !! can degenerate into "one thread does everything". They are returned rather than discarded so
    !! no compiler can delete them, and they cannot affect `r`: every value this module produces is
    !! a function of its coordinates alone.
    function draw(i, burnt) result(r)
        integer, intent(in) :: i                    !! the loop index, used as the stream
        integer(int64), intent(out) :: burnt        !! an accumulator over the extra draws
        real(real64) :: r                           !! the value under test
        integer :: k
        r = pf_random_at(seed, int(i, int64))
        burnt = 0_int64
        do k = 1, modulo(i, 23) + 1
            burnt = ieor(burnt, pf_random_bits_at(seed, int(i, int64), int(k, int64) + 1_int64))
        end do
    end function draw

    !> Concurrent calls to `pf_random_seed` must return different values.
    !!
    !! This belongs here rather than in the `random` suite: test-drive's parallelism runs whole
    !! TESTS concurrently, so a test there would compare only its own thread's values against each
    !! other. The property that matters is that two threads calling at the same moment -- possibly
    !! within the same clock tick -- come away with different seeds, which is what the critical
    !! region around the process-wide counter is for.
    !!
    !! **The vacuity guard here is not optional decoration, and its absence would be invisible.**
    !! `team` says how many threads were ASKED for; `got` says how many the region received. With a
    !! team of one there is no concurrency, and yet every assertion below still passes -- the
    !! process-wide counter guarantees distinct values when the calls are serial, so the duplicate
    !! count is zero for the wrong reason and the race this test exists to catch goes unexercised
    !! while the test stays green. `test_schedule_independence` above has always guarded this;
    !! this one did not, and read as passing on a machine where it proved nothing.
    subroutine test_seed_across_threads(error)
        type(error_type), allocatable, intent(out) :: error   !! set on the first failed assertion
        integer, parameter :: per_thread = 50
        integer :: nt, i, j, duplicates, team, got
        integer(int64), allocatable :: s(:)

        team = 1
#ifdef _OPENMP
        team = max(2, min(8, omp_get_max_threads()))
#endif
        nt = team * per_thread
        allocate(s(nt))
        s = 0_int64
        got = 1

#ifdef _OPENMP
        !$omp parallel do schedule(static) num_threads(team) default(shared) private(i)
#endif
        do i = 1, nt
            ! Captured from whichever thread runs iteration 1, matching the sibling test's reasoning:
            ! keying this on thread 0 would read 1 even with a full team under some schedules.
            if (i == 1) got = team_size()
            s(i) = pf_random_seed()
        end do
#ifdef _OPENMP
        !$omp end parallel do
#endif

#ifdef _OPENMP
        call check(error, got > 1, &
            "vacuity guard: the seed region ran with a team of one, so no two calls were ever concurrent and " // &
            "the duplicate check below proves nothing -- check that this suite is still excluded from " // &
            "test-drive's own per-test parallelism")
        if (allocated(error)) return
#endif
        call check(error, all(s >= 1_int64), "pf_random_seed returned a value below 1 from a thread")
        if (allocated(error)) return
        duplicates = 0
        do i = 1, nt
            do j = i + 1, nt
                if (s(i) == s(j)) duplicates = duplicates + 1
            end do
        end do
        call check(error, duplicates == 0, &
            "two pf_random_seed calls returned the same value -- the process-wide counter is not being incremented " // &
            "atomically, so two threads read it before either wrote it back")
    end subroutine test_seed_across_threads

    !> The current team's size: the real thing under OpenMP, 1 without it.
    !!
    !! A helper rather than a bare `omp_get_num_threads()` because its one call site sits in a loop
    !! body that is compiled on BOTH sides of `#ifdef _OPENMP`. The directives around that loop
    !! vanish without OpenMP but the loop itself does not, and the runtime routine is not declared
    !! there -- which is the same portability trap `materialize_marked_parallel` documents in
    !! `src/parquet_tables_read.f90`, met here in a test rather than in the library.
    function team_size() result(t)
        integer :: t                                !! threads in the current team, or 1 without OpenMP
#ifdef _OPENMP
        t = omp_get_num_threads()
#else
        t = 1
#endif
    end function team_size

    !> **The claim `threads=` rests on: the thread count changes the time and nothing else.**
    !!
    !! `pf_random_perm_at(seed, m, k)` is a pure function of its coordinates, so a bulk fill split
    !! into contiguous chunks cannot produce a different array however the chunks are assigned. That
    !! is what makes `threads=` admissible as an argument at all -- CLAUDE.md's settings rule is
    !! that a knob may change how fast, how large or how loud, never what the library answers -- and
    !! a user switching thread counts while debugging is entitled to the same permutation.
    !!
    !! Asserted against the SCALAR form rather than against a 1-thread bulk run, so this is also the
    !! outermost binding of the whole construction: every thread count, both result kinds, the
    !! subset form, and the elemental entry point all have to agree on one array.
    !!
    !! **The work floor is disabled first, or this test would be vacuous** -- at the factory default
    !! a 5000-element permutation feeds at most five threads, and a `threads=64` arm would silently
    !! resolve to five, leaving the high thread counts untested while still passing. The negative
    !! control for that is `test_random_parallel_min_effect` in the settings suite, which asserts
    !! the floor really does bite.
    subroutine test_perm_threads(error)
        type(error_type), allocatable, intent(out) :: error   !! set on the first failed assertion
        integer(int64), parameter :: SD = 555_int64
        integer(int64), parameter :: M = 5000_int64
        integer, parameter :: teams(*) = [1, 2, 3, 5, 8, 16, 64]
        integer(int64) :: ref(M), got(M), k
        integer(int32) :: got32(M)
        integer(int64) :: sub(M / 4_int64)
        integer :: ti
        character(len=72) :: msg

        do k = 1_int64, M
            ref(k) = pf_random_perm_at(SD, M, k)
        end do
        call parquet_set_random_parallel_min_elements(0)
        do ti = 1, size(teams)
            got = -1_int64
            call pf_random_permutation(got, SD, threads=teams(ti))
            if (any(got /= ref)) then
                write (msg, '(a,i0,a)') "a bulk permutation at threads=", teams(ti), &
                    " differs from the scalar form"
                call check(error, .false., trim(msg))
                call parquet_reset_settings()
                return
            end if
            got32 = -1_int32
            call pf_random_permutation(got32, SD, threads=teams(ti))
            if (any(int(got32, int64) /= ref)) then
                write (msg, '(a,i0,a)') "the int32 bulk permutation at threads=", teams(ti), &
                    " differs from the scalar form"
                call check(error, .false., trim(msg))
                call parquet_reset_settings()
                return
            end if
            sub = -1_int64
            call pf_random_subset(sub, M, SD, threads=teams(ti))
            if (any(sub /= ref(1:size(sub, kind=int64)))) then
                write (msg, '(a,i0,a)') "a bulk subset at threads=", teams(ti), &
                    " differs from the scalar form"
                call check(error, .false., trim(msg))
                call parquet_reset_settings()
                return
            end if
        end do
        call parquet_reset_settings()
        !
        ! The automatic form agrees too -- it is the one nobody passes an argument to.
        got = -1_int64
        call pf_random_permutation(got, SD)
        call check(error, all(got == ref), "the automatic bulk permutation differs from the scalar form")
    end subroutine test_perm_threads

    !> **The same claim for `pf_random_resample`: the thread count changes the time and nothing
    !! else** -- asserted against the SCALAR draw, not against a 1-thread bulk run.
    !!
    !! A resample splits the DRAW axis, which is a different construction from the permutation's and
    !! deserves its own assertion rather than an appeal to the sibling: a chunk covering elements
    !! `lo .. hi` is the serial fill restarted at draw `lo`, so a chunk boundary can land in the
    !! middle of a Philox block and the fill's alignment head is what makes that come out right.
    !! **The thread counts are chosen so that some boundaries are odd**: with `n = 5000`, `threads=3`
    !! gives a chunk of 1667 and `threads=7` one of 715, both odd, so the second and later chunks
    !! start on a block's second pair. An all-even set of chunk sizes would exercise only the aligned
    !! path and would pass against a fill that mishandled the other one.
    !!
    !! Three things are asserted, and the last two are negative controls without which the first is
    !! weak:
    !!
    !!  * every thread count, both result kinds, reproduces the scalar draw exactly;
    !!  * the **work floor is honoured against an explicit request** -- restored to its factory value,
    !!    a 5000-element resample must resolve `threads=64` to fewer than 64. Without this the first
    !!    assertion could pass against an implementation that silently ignored `threads` entirely;
    !!  * what a **parallel region** does to the count, which is two different rules and not one:
    !!    the *automatic* form goes serial inside any region, while an *explicit* request is honoured
    !!    in full inside an active one and clamped to a single worker only when the enclosing team has
    !!    one thread (Risk-104). And the values must still be right when called from in there.
    subroutine test_resample_threads(error)
        type(error_type), allocatable, intent(out) :: error   !! set on the first failed assertion
        integer(int64), parameter :: SD = 4242_int64
        integer(int64), parameter :: M = 900_int64
        integer(int64), parameter :: N = 5000_int64
        integer(int64), parameter :: ST = 11_int64
        integer, parameter :: teams(*) = [1, 2, 3, 5, 7, 8, 16, 64]
        integer(int64) :: ref(N), got(N), k
        integer(int32) :: got32(N)
        integer :: ti, floored, inside
        character(len=80) :: msg

        do k = 1_int64, N
            ref(k) = pf_random_int_at(SD, ST, 1_int64, M, k)
        end do

        call parquet_set_random_parallel_min_elements(0)
        do ti = 1, size(teams)
            got = -1_int64
            call pf_random_resample(got, M, SD, ST, threads=teams(ti))
            if (any(got /= ref)) then
                write (msg, '(a,i0,a)') "a resample at threads=", teams(ti), &
                    " differs from the scalar integer draw"
                call check(error, .false., trim(msg))
                call parquet_reset_settings()
                return
            end if
            got32 = -1_int32
            call pf_random_resample(got32, int(M, int32), SD, ST, threads=teams(ti))
            if (any(int(got32, int64) /= ref)) then
                write (msg, '(a,i0,a)') "the int32 resample at threads=", teams(ti), &
                    " differs from the scalar integer draw"
                call check(error, .false., trim(msg))
                call parquet_reset_settings()
                return
            end if
        end do
        call parquet_reset_settings()

        ! Negative control 1: with the floor back at its factory value, an explicit request above
        ! what the work can feed must be cut down. If this reported 64 the sweep above would have
        ! been comparing the serial path against itself eight times over.
        floored = parquet_debug_random_bulk_threads(N, 64)
        call check(error, floored < 64, &
            "the work floor did not bite on an explicit threads=64 for a 5000-element resample, so the " // &
            "thread sweep above may never have run more than one worker")
        if (allocated(error)) return

        ! Negative control 2: what a parallel region does to the thread count, and the two cases
        ! are deliberately DIFFERENT -- an early version of this test asserted one rule for both and
        ! failed against correct code. The AUTOMATIC form goes serial inside any region, because
        ! `parquet_auto_thread_count` will not nest by default. An EXPLICIT request is honoured in
        ! full inside an ACTIVE region -- CLAUDE.md's auto-threading note is precisely that
        ! `omp_in_parallel()` picks a default and does not veto a request -- and is clamped to 1 only
        ! when the enclosing team has ONE thread, which is the shape libgomp deadlocks on
        ! (`parquet_nested_team_unsafe`, Risk-104).
        inside = 0
#ifdef _OPENMP
        !$omp parallel num_threads(2) default(shared)
        !$omp single
        inside = parquet_debug_random_bulk_threads(N)          ! automatic, active region
        !$omp end single
        !$omp end parallel
        call check(error, inside == 1, &
            "an automatic resample inside a parallel region did not resolve to a single worker")
        if (allocated(error)) return

        inside = 0
        !$omp parallel num_threads(1) default(shared)
        !$omp single
        inside = parquet_debug_random_bulk_threads(N, 8)       ! explicit, INACTIVE region
        !$omp end single
        !$omp end parallel
        call check(error, inside == 1, &
            "an explicit threads= inside an inactive region was not clamped to one worker, which is " // &
            "the nested-team shape libgomp deadlocks on -- see feature_risks.md Risk-104")
        if (allocated(error)) return
#endif

        ! And it still answers correctly from in there -- with a real team, so this exercises the
        ! honoured-request path rather than the clamped one, which the count checks alone do not say.
        got = -1_int64
#ifdef _OPENMP
        !$omp parallel num_threads(2) default(shared)
        !$omp single
        call pf_random_resample(got, M, SD, ST, threads=8)
        !$omp end single
        !$omp end parallel
#else
        call pf_random_resample(got, M, SD, ST, threads=8)
#endif
        call check(error, all(got == ref), &
            "a resample called from inside a parallel region returned different values")
        if (allocated(error)) return

        ! The automatic form, which is the one nobody passes an argument to.
        got = -1_int64
        call pf_random_resample(got, M, SD, ST)
        call check(error, all(got == ref), "the automatic resample differs from the scalar integer draw")
    end subroutine test_resample_threads

end module test_random_omp
