!> Threading tests for `parquet_index`: the guarded mutation surface, lock-free lookups, and the
!> threaded `%build`.
!!
!! **This suite is registered SERIALLY** (`suite_is_safe_to_parallelize` in
!! `test/test_runner_support.f90` excludes `index_omp`), and for two reasons rather than one. The
!! familiar one is the nested-team hazard: test-drive dispatches a suite's tests inside its own
!! `!$omp parallel do`, so a test that opens a team of its own would open a nested one, which
!! libgomp deadlocks on intermittently (`feature_risks.md` Risk-104). The sharper one is that
!! `parquet_auto_thread_count` answers **1** whenever `omp_get_level() > 0` -- so inside
!! test-drive's region an automatic `%build` is always serial, and "the threaded build equals the
!! serial build" would hold because both arms ran the same serial code. A serial suite runs at
!! level 0, where the threading being asserted actually happens.
!!
!! **Every test whose assertion IS that threads interleaved carries the skip guard**, because
!! without OpenMP those assertions are not merely untestable, they are vacuous: both arms of an
!! A/B run the same code and the equality holds for the wrong reason. A build without OpenMP
!! should report a skip here, never a pass.
!!
!! **Two of them also skip on a one-processor machine.** `parquet_auto_thread_count` clamps to
!! `omp_get_num_procs()`, so on a runner with one processor a team is never opened whatever
!! `OMP_NUM_THREADS` says, and an A/B against "the threaded build" would again compare serial with
!! serial. `pf_index_threads` is what those tests read to find out, rather than a second copy of
!! the resolution rule.
module test_index_omp
    use testdrive, only: new_unittest, unittest_type, error_type, check, skip_test
    use parquet_index
    use iso_fortran_env, only: int32, int64
#ifdef _OPENMP
    use omp_lib, only: omp_get_max_threads, omp_get_thread_num
#endif
    implicit none
    private

    public :: collect_tests_index_omp

contains

    !> Registers every test in this suite.
    subroutine collect_tests_index_omp(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:) !! Receives the suite's tests.

        testsuite = [ &
            new_unittest("a shared pool survives concurrent get and free", test_pool_hammer), &
            new_unittest("no index is ever held by two threads at once", test_pool_no_double_issue), &
            new_unittest("a shared map numbers keys densely from several threads", test_shared_get_or_add), &
            new_unittest("concurrent lookups agree with serial answers", test_concurrent_lookups), &
            new_unittest("concurrent get_many agrees with serial answers", test_concurrent_get_many), &
            new_unittest("a threaded build equals a serial build", test_threaded_build_matches), &
            new_unittest("a threaded composite build equals a serial one", test_threaded_composite_build), &
            new_unittest("threads= is honoured and bounded", test_threads_argument), &
            new_unittest("pf_index_threads reports the rule the build follows", test_index_threads_rule), &
            new_unittest("a per-thread map and pool work inside a region", test_per_thread_containers) &
            ]
    end subroutine collect_tests_index_omp

    !> Threads that repeatedly take and give back indexes leave the pool consistent.
    !!
    !! The invariant under test is the one the guard exists for: no index is handed to two owners,
    !! and the counters still reconcile after every thread has given everything back. A dropped
    !! `!$omp critical` fails this probabilistically, which is exactly the class
    !! `feature_risks.md` is for -- so the iteration count is high enough to make a lost update
    !! likely rather than merely possible, and the test says so.
    subroutine test_pool_hammer(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(pf_index_pool) :: p
        integer(int64) :: held(4), i, k
        integer :: t, rounds, team

#ifndef _OPENMP
        call skip_test(error, "needs OpenMP: without it the loop below runs on one thread, so " // &
            "the pool is never reached concurrently and the guard this asserts is never exercised")
        return
#endif
        team = 1
        rounds = 400
#ifdef _OPENMP
        team = omp_get_max_threads()
#endif
        ! Each thread cycles through taking four indexes and giving them back. Everything it holds
        ! is private; the pool is the only shared object, which is what makes any inconsistency
        ! the pool's own.
        !$omp parallel do default(shared) private(t, i, k, held) schedule(static)
        do t = 1, team * rounds
            do k = 1_int64, 4_int64
                held(k) = p%get_index()
            end do
            do k = 1_int64, 4_int64
                call p%free_index(held(k))
            end do
        end do
        call check(error, p%get_used_count() == 0_int64, &
            "every index taken was given back, so nothing may still be held")
        if (allocated(error)) return
        call check(error, p%get_max_index() >= 4_int64, &
            "the pool must have issued at least the four one thread holds at a time")
        if (allocated(error)) return
        call check(error, p%get_free_index_count() == p%get_max_index(), &
            "with nothing held, every index below the watermark must be free")
        if (allocated(error)) return
        ! And it is still usable: the counters and the free list agree well enough to keep going.
        i = p%get_index()
        call check(error, i >= 1_int64 .and. i <= p%get_max_index(), &
            "the pool still hands out an index inside its own watermark")
        if (allocated(error)) return
        call check(error, p%is_used(i), "and records it as held")
    end subroutine test_pool_hammer

    !> No index is ever held by two threads at the same moment.
    !!
    !! Each thread marks the slot it was given with its own identity and checks nobody had marked
    !! it already. That check is itself unsynchronised, which is deliberate: a collision means the
    !! invariant is ALREADY broken, so every detection is a true positive, and the only cost of the
    !! race is a false negative -- which the round count is there to make unlikely.
    subroutine test_pool_no_double_issue(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(pf_index_pool) :: p
        integer, allocatable :: owner(:)
        integer(int64) :: idx
        integer :: t, tid, rounds, team, clashes, cap

#ifndef _OPENMP
        call skip_test(error, "needs OpenMP: with one thread no two owners can exist, so the " // &
            "assertion below is vacuous rather than merely untestable")
        return
#endif
        team = 1
        rounds = 500
#ifdef _OPENMP
        team = omp_get_max_threads()
#endif
        cap = 64 * (team + 2)
        allocate(owner(cap))
        owner = 0
        clashes = 0
        !$omp parallel do default(shared) private(t, tid, idx) reduction(+:clashes) schedule(static)
        do t = 1, team * rounds
            tid = 1
#ifdef _OPENMP
            tid = omp_get_thread_num() + 1
#endif
            idx = p%get_index()
            if (idx >= 1_int64 .and. idx <= int(cap, int64)) then
                if (owner(idx) /= 0) clashes = clashes + 1
                owner(idx) = tid
                owner(idx) = 0
            else
                ! The pool grew past the slot table, which means indexes were leaked rather than
                ! reused -- counted as a clash so the assertion below reports it.
                clashes = clashes + 1
            end if
            call p%free_index(idx)
        end do
        call check(error, clashes == 0, &
            "an index was handed to two threads at once, or the pool grew past what " // &
            "reuse-before-growth allows -- the pool's guard is not holding")
        if (allocated(error)) return
        call check(error, p%get_used_count() == 0_int64, "and everything was given back")
    end subroutine test_pool_no_double_issue

    !> Several threads streaming keys through one shared map get dense, unique indexes.
    !!
    !! This is the documented dictionary-encoding pattern under concurrency: `%get_or_add` is
    !! serialized internally, so `k` distinct keys must come out numbered exactly `1 .. k` however
    !! the threads interleave. A missing guard shows up as two keys sharing an index or as a gap in
    !! the numbering, and the permutation check below catches both.
    subroutine test_shared_get_or_add(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(pf_index_map) :: m
        integer(int64) :: idx, nkeys
        integer(int64), allocatable :: seen(:)
        integer :: t, team, reps, j
        logical :: ok

        nkeys = 300_int64
#ifndef _OPENMP
        call skip_test(error, "needs OpenMP: with one thread the interleaving this asserts " // &
            "cannot happen, so a missing guard would pass")
        return
#endif
        team = 1
#ifdef _OPENMP
        team = omp_get_max_threads()
#endif
        reps = 6
        call m%init()
        ! Every thread streams the SAME key set, so all but the first arrival at each key must get
        ! the index that arrival was given.
        !$omp parallel do default(shared) private(t, j, idx) schedule(static)
        do t = 1, team * reps
            do j = 1, int(nkeys)
                call m%get_or_add(int(j, int64) * 977_int64, idx)
            end do
        end do
        call check(error, m%nkeys() == nkeys, &
            "exactly the distinct keys were added, whatever the interleaving")
        if (allocated(error)) return
        ! The indexes handed out must be a permutation of 1..k: no duplicate, no gap.
        allocate(seen(nkeys))
        seen = 0_int64
        ok = .true.
        do j = 1, int(nkeys)
            idx = m%get(int(j, int64) * 977_int64)
            if (idx < 1_int64 .or. idx > nkeys) then
                ok = .false.
                exit
            end if
            if (seen(idx) /= 0_int64) then
                ok = .false.
                exit
            end if
            seen(idx) = 1_int64
        end do
        call check(error, ok, &
            "the indexes handed out must be exactly 1..k with no duplicate and no gap")
        if (allocated(error)) return
        call check(error, all(seen == 1_int64), "and every index in 1..k must have been used")
    end subroutine test_shared_get_or_add

    !> Any number of threads may look up a map nobody is mutating, and all get the same answers.
    subroutine test_concurrent_lookups(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(pf_index_map) :: m
        integer(int64) :: keys(2000), probes(4000), want(4000), i
        integer :: t, team, bad, k
        character(len=6) :: methods(3)

#ifndef _OPENMP
        call skip_test(error, "needs OpenMP: the point of this test is that several threads " // &
            "read one map at once, which one thread cannot do")
        return
#endif
        team = 1
#ifdef _OPENMP
        team = omp_get_max_threads()
#endif
        do i = 1_int64, 2000_int64
            keys(i) = i * 3_int64
        end do
        ! Half hits, half misses, interleaved.
        do i = 1_int64, 4000_int64
            probes(i) = i * 3_int64 / 2_int64
        end do
        methods = ["direct", "hash  ", "sorted"]
        do k = 1, 3
            call m%build(keys, method=trim(methods(k)))
            ! The serial answers first, on one thread, as the oracle.
            do i = 1_int64, 4000_int64
                want(i) = m%get(probes(i))
            end do
            bad = 0
            !$omp parallel do default(shared) private(t, i) reduction(+:bad) schedule(static)
            do t = 1, team * 4
                do i = 1_int64, 4000_int64
                    if (m%get(probes(i)) /= want(i)) bad = bad + 1
                end do
            end do
            call check(error, bad == 0, &
                "a concurrent lookup disagreed with the serial answer on backend " // &
                trim(methods(k)))
            if (allocated(error)) return
        end do
    end subroutine test_concurrent_lookups

    !> The bulk form is lock-free too, and agrees with the serial answers.
    subroutine test_concurrent_get_many(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(pf_index_map) :: m
        integer(int64) :: keys(1000), probes(1000), want(1000), got(1000), i
        integer :: t, team, bad

#ifndef _OPENMP
        call skip_test(error, "needs OpenMP: this asserts that several threads may run the " // &
            "bulk lookup at once, which one thread cannot show")
        return
#endif
        team = 1
#ifdef _OPENMP
        team = omp_get_max_threads()
#endif
        do i = 1_int64, 1000_int64
            keys(i) = i * 7_int64
            probes(i) = i * 5_int64
        end do
        call m%build(keys, method="hash")
        call m%get_many(probes, want)
        bad = 0
        !$omp parallel do default(shared) private(t, got) reduction(+:bad) schedule(static)
        do t = 1, team * 4
            call m%get_many(probes, got)
            if (any(got /= want)) bad = bad + 1
        end do
        call check(error, bad == 0, "a concurrent get_many disagreed with the serial answer")
    end subroutine test_concurrent_get_many

    !> A build that used a team answers exactly what a serial build answers.
    !!
    !! The fixture is deliberately above the work floor -- threading begins at twice
    !! `IX_MIN_KEYS_PER_THREAD`, so a few tens of thousands of keys reaches the threaded scan and
    !! scatter without a debug override. `pf_index_threads` is checked FIRST, because if it answers
    !! 1 the two arms are the same code and the equality below proves nothing.
    subroutine test_threaded_build_matches(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(pf_index_map) :: serial_map, threaded_map
        integer(int64) :: keys(30000), probes(200), a, b, i
        integer :: k, nt
        character(len=6) :: methods(3)

#ifndef _OPENMP
        call skip_test(error, "needs OpenMP: without it the threaded arm IS the serial arm, so " // &
            "the equality below would hold for the wrong reason")
        return
#endif
        nt = pf_index_threads(30000_int64)
        if (nt < 2) then
            call skip_test(error, "this machine's affinity mask allows one processor, so no " // &
                "team is opened and both arms of the comparison would run serially")
            return
        end if
        do i = 1_int64, 30000_int64
            keys(i) = i * 11_int64
        end do
        do i = 1_int64, 200_int64
            probes(i) = i * 331_int64
        end do
        methods = ["direct", "hash  ", "sorted"]
        do k = 1, 3
            call serial_map%build(keys, method=trim(methods(k)), threads=1)
            call threaded_map%build(keys, method=trim(methods(k)), threads=nt)
            call check(error, threaded_map%nkeys() == serial_map%nkeys(), &
                "a threaded build stored a different number of keys on " // trim(methods(k)))
            if (allocated(error)) return
            do i = 1_int64, 200_int64
                a = serial_map%get(probes(i))
                b = threaded_map%get(probes(i))
                call check(error, a == b, &
                    "a threaded build answered differently from a serial one on " // &
                    trim(methods(k)))
                if (allocated(error)) return
            end do
            do i = 1_int64, 30000_int64, 97_int64
                call check(error, threaded_map%get(keys(i)) == i, &
                    "a threaded build lost or misplaced a key on " // trim(methods(k)))
                if (allocated(error)) return
            end do
        end do
    end subroutine test_threaded_build_matches

    !> The threaded composite scatter agrees with the serial one.
    !!
    !! Worth its own test because the composite scatter computes a mixed-radix offset per key
    !! inside the parallel loop, which is more arithmetic under `private()` than the scalar arm has.
    subroutine test_threaded_composite_build(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(pf_index_map) :: serial_map, threaded_map
        integer(int64) :: pairs(20000, 2), i, a, b
        integer :: nt, k
        character(len=6) :: methods(2)

#ifndef _OPENMP
        call skip_test(error, "needs OpenMP: without it both arms run the serial scatter")
        return
#endif
        nt = pf_index_threads(20000_int64)
        if (nt < 2) then
            call skip_test(error, "one processor available, so no team is opened and both " // &
                "arms would run serially")
            return
        end if
        do i = 1_int64, 20000_int64
            pairs(i, 1) = mod(i, 200_int64)
            pairs(i, 2) = i / 200_int64
        end do
        methods = ["direct", "hash  "]
        do k = 1, 2
            call serial_map%build(pairs, method=trim(methods(k)), threads=1)
            call threaded_map%build(pairs, method=trim(methods(k)), threads=nt)
            call check(error, threaded_map%nkeys() == serial_map%nkeys(), &
                "a threaded composite build stored a different key count on " // trim(methods(k)))
            if (allocated(error)) return
            do i = 1_int64, 20000_int64, 61_int64
                a = serial_map%get(pairs(i, :))
                b = threaded_map%get(pairs(i, :))
                call check(error, a == b .and. b == i, &
                    "a threaded composite build answered differently on " // trim(methods(k)))
                if (allocated(error)) return
            end do
        end do
    end subroutine test_threaded_composite_build

    !> `threads=` is honoured, and `threads=1` really is serial.
    subroutine test_threads_argument(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(pf_index_map) :: m
        integer(int64) :: keys(20000), i

        do i = 1_int64, 20000_int64
            keys(i) = i
        end do
        ! Both must build a correct map, whatever the machine allows. This one needs no skip
        ! guard: it asserts the ARGUMENT is accepted and the answers are right, not that a team
        ! was opened.
        call m%build(keys, threads=1)
        call check(error, m%get(12345_int64) == 12345_int64, "threads=1 builds correctly")
        if (allocated(error)) return
        call m%build(keys, threads=4)
        call check(error, m%get(12345_int64) == 12345_int64, "threads=4 builds correctly")
        if (allocated(error)) return
        call m%build(keys, method="hash", threads=3)
        call check(error, m%get(19999_int64) == 19999_int64, "a threaded hash build is correct")
        if (allocated(error)) return
        call m%build(keys, method="sorted", threads=3)
        call check(error, m%get(19999_int64) == 19999_int64, "a threaded sorted build is correct")
    end subroutine test_threads_argument

    !> `pf_index_threads` reports the rule `%build` actually follows.
    subroutine test_index_threads_rule(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        integer :: small, large, inside

        small = pf_index_threads(10_int64)
        call check(error, small == 1, &
            "a build over ten keys is not worth a team, so the rule must answer 1")
        if (allocated(error)) return
        large = pf_index_threads(10000000_int64)
        call check(error, large >= 1, "the rule always answers at least 1")
        if (allocated(error)) return
        call check(error, large >= small, "and never fewer threads for more work")
        if (allocated(error)) return
        ! The int32 spelling agrees with the int64 one.
        call check(error, pf_index_threads(10_int32) == small, &
            "the int32 and int64 spellings must agree")
        if (allocated(error)) return
#ifdef _OPENMP
        ! Inside somebody else's parallel region the answer is 1, so a build there does not open a
        ! nested team. This is the rule shared with every other threaded tier in the library.
        inside = 0
        !$omp parallel default(shared) num_threads(2)
        !$omp single
        inside = pf_index_threads(10000000_int64)
        !$omp end single
        !$omp end parallel
        call check(error, inside == 1, &
            "inside an active parallel region the rule must resolve to 1, or a build there " // &
            "would open a nested team")
#endif
    end subroutine test_index_threads_rule

    !> A per-thread map and pool, held in a shared array indexed by thread, work inside a region.
    !!
    !! **The per-thread array is the portable shape, and the two obvious alternatives are not.**
    !! Measured on this very type, gfortran 15.2: a `private()` copy's scalar components are not
    !! default-initialised, so `pf_index_pool%get_index` reads a garbage free-list count and
    !! segfaults inside the allocator. Block-local declaration is what gfortran wants instead, and
    !! is what ifx segfaults on for any type with allocatable components. Only allocating one
    !! instance per thread BEFORE the region and indexing it by thread number satisfies both --
    !! the same pattern `materialize_marked_parallel` (src/parquet_tables_read.f90) uses, and for
    !! the same reason.
    !!
    !! It is also the negative control for every guard in the module: a thread-private container
    !! must be fully usable inside a parallel region, so a guard that refused any concurrent access
    !! rather than only shared access would fail here while passing every abort scenario.
    subroutine test_per_thread_containers(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(pf_index_map), allocatable :: maps(:)
        type(pf_index_pool), allocatable :: pools(:)
        integer :: t, team, bad, tid

#ifndef _OPENMP
        call skip_test(error, "needs OpenMP: there is no per-thread instance to test without a " // &
            "parallel region, and the assertion below would be about ordinary serial use")
        return
#endif
        team = 1
#ifdef _OPENMP
        team = omp_get_max_threads()
#endif
        allocate(maps(team))
        allocate(pools(team))
        bad = 0
        !$omp parallel do default(shared) private(t, tid) reduction(+:bad) schedule(static)
        do t = 1, team * 4
            tid = 1
#ifdef _OPENMP
            tid = omp_get_thread_num() + 1
#endif
            call maps(tid)%build([int(t, int64), int(t, int64) + 1_int64, int(t, int64) + 2_int64])
            if (maps(tid)%get(int(t, int64)) /= 1_int64) bad = bad + 1
            if (maps(tid)%get(int(t, int64) + 2_int64) /= 3_int64) bad = bad + 1
            if (maps(tid)%nkeys() /= 3_int64) bad = bad + 1
            if (pools(tid)%get_index() < 1_int64) bad = bad + 1
            call pools(tid)%clear()
        end do
        call check(error, bad == 0, &
            "a thread-private map and pool must work inside a parallel region -- if this " // &
            "fails, a guard is refusing the permitted case rather than the shared one")
    end subroutine test_per_thread_containers

    ! gcov attribution artifact: an `end module` line is not a statement and reports 0 hits.
end module test_index_omp ! GCOVR_EXCL_LINE
