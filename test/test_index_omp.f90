!> Threading tests for `parquet_index`: the guarded mutation surface, lock-free lookups, the
!> threaded `%build` and `%get_many`, and the multimap's bulk lookups.
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
    use omp_lib, only: omp_get_max_threads, omp_get_thread_num, omp_get_num_procs
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
            new_unittest("a threaded get_many equals a serial one and resolves its team", &
                test_threaded_get_many), &
            new_unittest("get_many's team follows the build's rule", test_get_many_threads_rule), &
            new_unittest("the multimap's bulk lookups equal serial and resolve their team", &
                test_mm_threaded_bulk), &
            new_unittest("the multimap's bulk team follows the build's rule", test_mm_bulk_threads_rule), &
            new_unittest("a threaded build equals a serial build", test_threaded_build_matches), &
            new_unittest("a threaded composite build equals a serial one", test_threaded_composite_build), &
            new_unittest("threads= is honoured and bounded", test_threads_argument), &
            new_unittest("pf_index_threads reports the rule the build follows", test_index_threads_rule), &
            new_unittest("a per-thread map and pool work inside a region", test_per_thread_containers), &
            new_unittest("two builds on two threads run at the same time", test_concurrent_builds_overlap), &
            new_unittest("a partitioned hash build answers every key and every miss, and spills", &
                test_partitioned_build_answers), &
            new_unittest("a threaded get_or_add_many codes densely and consistently", &
                test_partitioned_get_or_add_many), &
            new_unittest("a multimap grouped on a team answers as the serial one", test_mm_threaded_grouping) &
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
    !!
    !! The calls below are made from INSIDE a parallel region, where the bulk form's automatic
    !! team resolves to 1 (the rule shared with every threaded tier here), so this is the
    !! contract that any number of threads may run their own serial `%get_many` on one map at
    !! once. `test_threaded_get_many` is the one that opens a team inside the call.
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

    !> A `%get_many` that used a team answers exactly what a serial one answers, and the team it
    !! resolved is the one the rule reports.
    !!
    !! Both halves matter and neither is enough alone. The answers are identical at every thread
    !! count by design, so the equality passes against a worker that never threads; the counter
    !! is what tells a team that was opened from one that silently collapsed (CLAUDE.md's nested-
    !! OpenMP note: a collapsed team is undetectable without a team assertion). The probe count is
    !! above the work floor, so the automatic rule opens a team on any machine that can thread;
    !! `pf_index_threads` is checked first so that a one-processor runner skips rather than
    !! comparing serial with serial.
    subroutine test_threaded_get_many(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(pf_index_map) :: m
        integer(int64), allocatable :: keys(:), probes(:), want(:), got(:), pairs(:,:), pprobe(:,:)
        integer(int32), allocatable :: got32(:)
        logical, allocatable :: mask(:)
        integer(int64) :: i
        integer :: k, nt, want_four
        character(len=6) :: methods(3)

#ifndef _OPENMP
        call skip_test(error, "needs OpenMP: without it the threaded arm IS the serial arm, so " // &
            "the equality below would hold for the wrong reason and the team assertion is moot")
        return
#endif
        nt = pf_index_threads(50000_int64)
        if (nt < 2) then
            call skip_test(error, "this machine's affinity mask allows one processor, so no " // &
                "team is opened and both arms of the comparison would run serially")
            return
        end if
        want_four = 4
#ifdef _OPENMP
        want_four = min(4, omp_get_num_procs())
#endif
        allocate(keys(30000), probes(50000), want(50000), got(50000), got32(50000), mask(50000))
        do i = 1_int64, 30000_int64
            keys(i) = i * 11_int64
        end do
        ! About half hits and half misses, interleaved, wrapping around the key set: an even
        ! residue lands on a stored key, an odd one between two of them.
        do i = 1_int64, 50000_int64
            probes(i) = (mod(i * 7_int64, 66000_int64) * 11_int64) / 2_int64
            mask(i) = mod(i, 3_int64) /= 0_int64
        end do
        methods = ["direct", "hash  ", "sorted"]
        do k = 1, 3
            call m%build(keys, method=trim(methods(k)), threads=1)
            call m%get_many(probes, want, threads=1)
            call check(error, parquet_debug_index_get_many_threads_used() == 1, &
                "threads=1 must force a serial lookup on " // trim(methods(k)))
            if (allocated(error)) return
            call m%get_many(probes, got)
            call check(error, parquet_debug_index_get_many_threads_used() == nt, &
                "an automatic get_many over 50000 rows must resolve the team pf_index_threads " // &
                "reports, on " // trim(methods(k)))
            if (allocated(error)) return
            call check(error, all(got == want), &
                "a threaded get_many answered differently from a serial one on " // trim(methods(k)))
            if (allocated(error)) return
            call m%get_many(probes, got, threads=4)
            call check(error, parquet_debug_index_get_many_threads_used() == want_four, &
                "threads=4 must be honoured, clamped only by this process's CPU affinity, on " // &
                trim(methods(k)))
            if (allocated(error)) return
            call check(error, all(got == want), &
                "a get_many with threads=4 answered differently from a serial one on " // &
                trim(methods(k)))
            if (allocated(error)) return
            ! The mask under a team: masked rows 0, the rest as serial.
            call m%get_many(probes, got, valid=mask)
            call check(error, all(merge(want, 0_int64, mask) == got), &
                "a masked threaded get_many must answer 0 on masked rows and the serial " // &
                "answer elsewhere, on " // trim(methods(k)))
            if (allocated(error)) return
            ! The int32 answer form threads too.
            call m%get_many(probes, got32)
            call check(error, all(int(got32, int64) == want), &
                "the int32 answer form of a threaded get_many must agree on " // trim(methods(k)))
            if (allocated(error)) return
        end do
        ! Vacuity guard: the probe set must contain both hits and misses, or the equalities above
        ! compare arrays of zeros.
        call check(error, count(want /= 0_int64) > 1000 .and. count(want == 0_int64) > 1000, &
            "fixture: the probes must mix hits and misses in the thousands")
        if (allocated(error)) return
        ! Composite keys under a team.
        allocate(pairs(30000, 2), pprobe(50000, 2))
        pairs(:, 1) = keys
        pairs(:, 2) = mod(keys, 5_int64)
        pprobe(:, 1) = probes
        pprobe(:, 2) = mod(probes, 5_int64)
        do k = 1, 2
            call m%build(pairs, method=trim(methods(k)), threads=1)
            call m%get_many(pprobe, want, threads=1)
            call m%get_many(pprobe, got)
            call check(error, parquet_debug_index_get_many_threads_used() == nt, &
                "an automatic composite get_many must resolve the same team on " // trim(methods(k)))
            if (allocated(error)) return
            call check(error, all(got == want), &
                "a threaded composite get_many answered differently from a serial one on " // &
                trim(methods(k)))
            if (allocated(error)) return
        end do
        call check(error, count(want /= 0_int64) > 1000, &
            "fixture: the composite probes must hit in the thousands")
    end subroutine test_threaded_get_many

    !> The bulk lookup's team follows the build's rule: 1 below the work floor, 1 inside a
    !! parallel region, and `pf_index_threads(n)` otherwise.
    subroutine test_get_many_threads_rule(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(pf_index_map) :: m
        integer(int64) :: keys(100), probes(100), got(100), i
        integer(int64), allocatable :: big(:), bigout(:)
        integer :: inside, nt

#ifndef _OPENMP
        call skip_test(error, "needs OpenMP: the assertions below are about a team being " // &
            "opened, or deliberately not, and without OpenMP no arm can differ from serial")
        return
#endif
        do i = 1_int64, 100_int64
            keys(i) = i
            probes(i) = i + 50_int64
        end do
        call m%build(keys)
        call m%get_many(probes, got)
        call check(error, parquet_debug_index_get_many_threads_used() == 1, &
            "a lookup over a hundred rows is below the work floor and must run serially")
        if (allocated(error)) return
        call check(error, count(got /= 0_int64) == 50, "and it still answers the fifty hits")
        if (allocated(error)) return
        nt = pf_index_threads(100000_int64)
        if (nt < 2) then
            call skip_test(error, "one processor available, so no team is ever opened and " // &
                "the automatic arm below could not differ from the serial control")
            return
        end if
        allocate(big(100000), bigout(100000))
        do i = 1_int64, 100000_int64
            big(i) = i
        end do
        call m%get_many(big, bigout)
        call check(error, parquet_debug_index_get_many_threads_used() == nt, &
            "above the floor the automatic team must be what pf_index_threads reports, or the " // &
            "query is a second copy of the rule that has drifted")
        if (allocated(error)) return
        call check(error, count(bigout /= 0_int64) == 100, "and the hundred stored keys are found")
        if (allocated(error)) return
        ! Inside somebody else's parallel region the automatic answer is 1: a lookup there must
        ! not open a nested team. Read on the same thread that made the call, before the region
        ! closes, so that nothing else can have written the counter in between.
        inside = 0
        !$omp parallel default(shared) num_threads(2)
        !$omp single
        call m%get_many(big, bigout)
        inside = parquet_debug_index_get_many_threads_used()
        !$omp end single
        !$omp end parallel
        call check(error, inside == 1, &
            "inside an active parallel region an automatic get_many must resolve to 1")
        if (allocated(error)) return
        call check(error, count(bigout /= 0_int64) == 100, "and still answers correctly there")
    end subroutine test_get_many_threads_rule

    !> `pf_index_multimap%get_first_many`, `%get_many` and `%probe_many` answer what their serial
    !! forms answer and resolve the team `pf_index_map%get_many` would, read through the same
    !! counter -- which is what proves the multimap's drivers thread at all, since every answer is
    !! identical at every team size.
    !!
    !! The fixture repeats every key five times and the probes are half hits, so the CSR arm has
    !! ranges of several rows to copy in parallel and empty ranges between them.
    subroutine test_mm_threaded_bulk(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(pf_index_multimap) :: mm
        integer(int64), allocatable :: keys(:), probes(:), want(:), got(:), woff(:), wm(:), off(:), m(:), &
            pairs(:,:), pprobe(:,:)
        integer(int32), allocatable :: got32(:)
        logical, allocatable :: mask(:)
        integer(int64) :: i, nm1, nm2
        integer :: k, nt, want_four
        character(len=6) :: methods(3)

#ifndef _OPENMP
        call skip_test(error, "needs OpenMP: without it the threaded arm IS the serial arm, so " // &
            "the equalities below would hold for the wrong reason and the team assertions are moot")
        return
#endif
        nt = pf_index_threads(50000_int64)
        if (nt < 2) then
            call skip_test(error, "this machine's affinity mask allows one processor, so no " // &
                "team is opened and both arms of the comparison would run serially")
            return
        end if
        want_four = 4
#ifdef _OPENMP
        want_four = min(4, omp_get_num_procs())
#endif
        allocate(keys(30000), probes(50000), want(50000), got(50000), got32(50000), mask(50000))
        ! 6000 distinct keys, each five times (11 is coprime with 6000, so every residue appears).
        do i = 1_int64, 30000_int64
            keys(i) = mod(i * 11_int64, 6000_int64)
        end do
        ! Probes over twice the key range: those below 6000 hit, the rest miss.
        do i = 1_int64, 50000_int64
            probes(i) = mod(i * 7_int64, 12000_int64)
            mask(i) = mod(i, 3_int64) /= 0_int64
        end do
        methods = ["direct", "hash  ", "sorted"]
        do k = 1, 3
            call mm%build(keys, method=trim(methods(k)), threads=1)
            call mm%get_first_many(probes, want, threads=1)
            call check(error, parquet_debug_index_get_many_threads_used() == 1, &
                "threads=1 must force a serial get_first_many on " // trim(methods(k)))
            if (allocated(error)) return
            call mm%get_first_many(probes, got)
            call check(error, parquet_debug_index_get_many_threads_used() == nt, &
                "an automatic get_first_many over 50000 rows must resolve the team " // &
                "pf_index_threads reports, on " // trim(methods(k)))
            if (allocated(error)) return
            call check(error, all(got == want), &
                "a threaded get_first_many answered differently from a serial one on " // &
                trim(methods(k)))
            if (allocated(error)) return
            call mm%get_first_many(probes, got, threads=4)
            call check(error, parquet_debug_index_get_many_threads_used() == want_four, &
                "threads=4 must be honoured on get_first_many, clamped only by affinity, on " // &
                trim(methods(k)))
            if (allocated(error)) return
            call check(error, all(got == want), &
                "get_first_many with threads=4 answered differently on " // trim(methods(k)))
            if (allocated(error)) return
            call mm%get_first_many(probes, got, valid=mask)
            call check(error, all(merge(want, 0_int64, mask) == got), &
                "a masked threaded get_first_many must answer 0 on masked rows and the serial " // &
                "answer elsewhere, on " // trim(methods(k)))
            if (allocated(error)) return
            call mm%get_first_many(probes, got32)
            call check(error, all(int(got32, int64) == want), &
                "the int32 answer form of a threaded get_first_many must agree on " // trim(methods(k)))
            if (allocated(error)) return
            ! The group-id form.
            call mm%get_many(probes, want, threads=1)
            call mm%get_many(probes, got)
            call check(error, parquet_debug_index_get_many_threads_used() == nt .and. all(got == want), &
                "a threaded get_many must resolve the team and equal the serial one on " // &
                trim(methods(k)))
            if (allocated(error)) return
            ! The CSR form.
            call mm%probe_many(probes, woff, wm, threads=1, n_matched=nm1)
            call mm%probe_many(probes, off, m, n_matched=nm2)
            call check(error, parquet_debug_index_get_many_threads_used() == nt, &
                "an automatic probe_many over 50000 rows must resolve the team on " // trim(methods(k)))
            if (allocated(error)) return
            call check(error, size(off) == size(woff) .and. size(m) == size(wm), &
                "a threaded probe_many must produce the serial CSR shape on " // trim(methods(k)))
            if (allocated(error)) return
            call check(error, all(off == woff) .and. all(m == wm) .and. nm1 == nm2, &
                "a threaded probe_many must equal the serial one, offsets, matches and n_matched, " // &
                "on " // trim(methods(k)))
            if (allocated(error)) return
            call check(error, size(wm, kind=int64) > 100000_int64, &
                "fixture: the probes match with repeats, in the hundred thousands of pairs")
            if (allocated(error)) return
        end do
        call mm%get_first_many(probes, want, threads=1)
        call check(error, count(want /= 0_int64) > 10000 .and. count(want == 0_int64) > 10000, &
            "fixture: the probes must mix hits and misses in the tens of thousands")
        if (allocated(error)) return
        ! Composite keys under a team.
        allocate(pairs(30000, 2), pprobe(50000, 2))
        pairs(:, 1) = keys
        pairs(:, 2) = mod(keys, 3_int64)
        pprobe(:, 1) = probes
        pprobe(:, 2) = mod(probes, 3_int64)
        do k = 1, 2
            call mm%build(pairs, method=trim(methods(k)), threads=1)
            call mm%get_first_many(pprobe, want, threads=1)
            call mm%get_first_many(pprobe, got)
            call check(error, parquet_debug_index_get_many_threads_used() == nt .and. all(got == want), &
                "a threaded composite get_first_many must resolve the team and equal serial on " // &
                trim(methods(k)))
            if (allocated(error)) return
            call mm%probe_many(pprobe, woff, wm, threads=1)
            call mm%probe_many(pprobe, off, m)
            call check(error, size(m) == size(wm) .and. all(off == woff) .and. all(m == wm), &
                "a threaded composite probe_many must equal the serial one on " // trim(methods(k)))
            if (allocated(error)) return
        end do
        call check(error, count(want /= 0_int64) > 10000, "fixture: the composite probes hit in the thousands")
    end subroutine test_mm_threaded_bulk

    !> The multimap's bulk team is the map's rule: 1 below the work floor, 1 inside a parallel
    !! region, and `pf_index_threads(n)` otherwise, on every one of the three bulk forms.
    subroutine test_mm_bulk_threads_rule(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(pf_index_multimap) :: mm
        integer(int64) :: keys(100), probes(100), got(100), i
        integer(int64), allocatable :: big(:), bigout(:), off(:), m(:)
        integer :: inside, inside2, nt

#ifndef _OPENMP
        call skip_test(error, "needs OpenMP: the assertions below are about a team being " // &
            "opened, or deliberately not, and without OpenMP no arm can differ from serial")
        return
#endif
        do i = 1_int64, 100_int64
            keys(i) = 1_int64 + mod(i, 40_int64)
            probes(i) = i
        end do
        call mm%build(keys)
        call mm%get_first_many(probes, got)
        call check(error, parquet_debug_index_get_many_threads_used() == 1, &
            "a get_first_many over a hundred rows is below the work floor and must run serially")
        if (allocated(error)) return
        call check(error, count(got /= 0_int64) == 40, "and it still answers the forty hits")
        if (allocated(error)) return
        call mm%probe_many(probes, off, m)
        call check(error, parquet_debug_index_get_many_threads_used() == 1 .and. size(m) == 100, &
            "a probe_many over a hundred rows runs serially and finds every row")
        if (allocated(error)) return
        nt = pf_index_threads(100000_int64)
        if (nt < 2) then
            call skip_test(error, "one processor available, so no team is ever opened and " // &
                "the automatic arms below could not differ from the serial control")
            return
        end if
        allocate(big(100000), bigout(100000))
        do i = 1_int64, 100000_int64
            big(i) = i
        end do
        call mm%get_first_many(big, bigout)
        call check(error, parquet_debug_index_get_many_threads_used() == nt, &
            "above the floor the automatic get_first_many team must be what pf_index_threads reports")
        if (allocated(error)) return
        call check(error, count(bigout /= 0_int64) == 40, "and the forty stored keys are found")
        if (allocated(error)) return
        call mm%probe_many(big, off, m)
        call check(error, parquet_debug_index_get_many_threads_used() == nt .and. size(m) == 100, &
            "above the floor the automatic probe_many team must be what pf_index_threads reports")
        if (allocated(error)) return
        ! Inside somebody else's parallel region the automatic answer is 1, on both forms.
        inside = 0
        inside2 = 0
        !$omp parallel default(shared) num_threads(2)
        !$omp single
        call mm%get_first_many(big, bigout)
        inside = parquet_debug_index_get_many_threads_used()
        call mm%probe_many(big, off, m)
        inside2 = parquet_debug_index_get_many_threads_used()
        !$omp end single
        !$omp end parallel
        call check(error, inside == 1 .and. inside2 == 1, &
            "inside an active parallel region an automatic multimap bulk lookup must resolve to 1")
        if (allocated(error)) return
        call check(error, count(bigout /= 0_int64) == 40 .and. size(m) == 100, &
            "and still answers correctly there")
    end subroutine test_mm_bulk_threads_rule

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
            ! The hash arm is vacuous unless the threaded build really took the partitioned pass:
            ! a serial insert loop under a threaded scan answers identically. The spill count is
            ! -1 for the serial loop and 0 or more for the partitioned pass.
            if (k == 2) then
                call check(error, parquet_debug_index_spills() >= 0_int64, &
                    "the threaded hash build must have run the partitioned insert, not the serial loop")
                if (allocated(error)) return
            end if
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
            if (k == 2) then
                call check(error, parquet_debug_index_spills() >= 0_int64, &
                    "the threaded composite hash build must have run the partitioned insert")
                if (allocated(error)) return
            end if
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
        integer :: seen_many, seen_one, want_many, want_three

        do i = 1_int64, 20000_int64
            keys(i) = i
        end do
#ifndef _OPENMP
        call skip_test(error, "needs OpenMP: without it no team is ever opened, so the two arms " // &
            "below would differ only in a resolved number that reaches no parallel region, and " // &
            "the assertion would hold for the wrong reason")
        return
#endif
#ifdef _OPENMP
        if (omp_get_num_procs() < 2) then
            call skip_test(error, "needs 2+ processors: threads= is clamped to " // &
                "omp_get_num_procs(), so on a one-processor runner the threads=4 arm and the " // &
                "threads=1 control both resolve to 1 and the comparison is vacuous")
            return
        end if
        ! The clamp is part of the contract, so the expectation carries it rather than the test
        ! demanding a machine wide enough to avoid it.
        want_many = min(4, omp_get_num_procs())
        want_three = min(3, omp_get_num_procs())
#endif
        ! Each arm asserts BOTH halves: the map is right, and the team the rule resolved is the one
        ! that was asked for. The correctness assertions alone pass against a %build that discards
        ! threads= entirely -- the answer is identical at every thread count by design, which is
        ! exactly why parquet_debug_index_threads_used has to exist.
        call m%build(keys, threads=4)
        seen_many = parquet_debug_index_threads_used()
        call check(error, m%get(12345_int64) == 12345_int64, "threads=4 builds correctly")
        if (allocated(error)) return
        call check(error, seen_many == want_many, &
            "threads=4 must be honoured, clamped only by this process's CPU affinity")
        if (allocated(error)) return
        ! The negative control. Without it the assertion above passes just as happily against a
        ! build that always opens the full machine and never reads threads= at all.
        call m%build(keys, threads=1)
        seen_one = parquet_debug_index_threads_used()
        call check(error, m%get(12345_int64) == 12345_int64, "threads=1 builds correctly")
        if (allocated(error)) return
        call check(error, seen_one == 1, "threads=1 must force serial")
        if (allocated(error)) return
        call check(error, seen_many > seen_one, &
            "the two requests must resolve differently, or neither assertion is discriminating")
        if (allocated(error)) return
        ! Both non-default backends reach the same rule for their own scan.
        call m%build(keys, method="hash", threads=3)
        call check(error, m%get(19999_int64) == 19999_int64, "a threaded hash build is correct")
        if (allocated(error)) return
        call check(error, parquet_debug_index_threads_used() == want_three, &
            "a hash build honours threads= for its key scan")
        if (allocated(error)) return
        ! A sorted build's SORT answers to pf_argsort and the sorting knobs; what this counter
        ! reports is this module's own scan, which follows the index rule like every other backend.
        call m%build(keys, method="sorted", threads=3)
        call check(error, m%get(19999_int64) == 19999_int64, "a threaded sorted build is correct")
        if (allocated(error)) return
        call check(error, parquet_debug_index_threads_used() == want_three, &
            "a sorted build honours threads= for its own scan, whatever pf_argsort then does")
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
#ifdef _OPENMP
        ! `large >= small` is vacuous on its own -- `small` is asserted to be 1 just above, so it
        ! reduces to `large >= 1`, which the line before it already checked. Where the machine can
        ! actually thread, the work bound must be seen to lift.
        if (omp_get_num_procs() >= 2 .and. omp_get_max_threads() >= 2) then
            call check(error, large > small, &
                "on a machine that can thread, ten million keys must resolve to more than the " // &
                "one thread ten keys get, or the work bound is not being applied at all")
            if (allocated(error)) return
        end if
#endif
        ! The test's name promises this and only this line delivers it: that the number the query
        ! reports is the number a BUILD actually resolves, not a second copy of the rule that has
        ! drifted. `pf_index_threads` deliberately does not record, so reading the counter after
        ! the query cannot be what makes this pass.
        call build_and_compare(error, 20000_int64)
        if (allocated(error)) return
        call build_and_compare(error, 100_int64)
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

    !> Builds a map of `n` keys automatically and checks the build resolved what the query reports.
    !!
    !! Two sizes are worth passing: one above the work floor and one below it. Below the floor both
    !! sides are 1, which proves nothing on its own but would catch a query that answered 1 where
    !! the build threaded; above it, the two agreeing is the real assertion.
    subroutine build_and_compare(error, n)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        integer(int64), intent(in) :: n                     !! keys to build over.
        type(pf_index_map) :: m
        integer(int64), allocatable :: keys(:)
        integer(int64) :: i
        character(len=32) :: t

        allocate(keys(n))
        do i = 1_int64, n
            keys(i) = i
        end do
        call m%build(keys)
        write (t, '(i0)') n
        call check(error, parquet_debug_index_threads_used() == pf_index_threads(n), &
            "over " // trim(t) // " keys the team the build resolved must equal what " // &
            "pf_index_threads reports, or the query is a second copy of the rule that has drifted")
    end subroutine build_and_compare

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

    !> Two builds on two threads run at the same time: a `%build` scans and fills a LOCAL map on
    !! the calling thread and takes the map's lock only to swap the result in, so builds of
    !! different maps no longer take turns.
    !!
    !! A build answers identically whether it ran beside another or waited for it, so the maps
    !! themselves cannot show which happened; `parquet_debug_index_concurrent_builds` -- the most
    !! builds ever in flight at once -- is the one observable that can, and it is read after its
    !! own reset so that an earlier test's builds are not what is measured. Each thread builds
    !! twenty 30000-key hash maps in a row, so that the builds overlap in practice however the
    !! threads are scheduled. The negative control -- the builds back under the guard for their
    !! whole duration -- holds the mark at 1.
    subroutine test_concurrent_builds_overlap(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(pf_index_map), allocatable :: maps(:)
        integer(int64), allocatable :: keys(:)
        integer :: t, team, bad, r, mark
        integer(int64) :: i

#ifndef _OPENMP
        call skip_test(error, "needs OpenMP: without a parallel region no two builds can " // &
            "overlap, and the high-water mark below would be 1 for the wrong reason")
        return
#endif
        team = 1
#ifdef _OPENMP
        team = min(4, omp_get_max_threads(), omp_get_num_procs())
#endif
        if (team < 2) then
            call skip_test(error, "needs 2+ processors: a one-processor team cannot overlap two builds")
            return
        end if
        allocate(maps(team), keys(30000))
        do i = 1_int64, 30000_int64
            keys(i) = i * 7_int64
        end do
        mark = parquet_debug_index_concurrent_builds(reset=.true.)
        bad = 0
        !$omp parallel do default(shared) private(t, r, i) reduction(+:bad) num_threads(team) &
        !$omp     schedule(static)
        do t = 1, team
            do r = 1, 20
                call maps(t)%build(keys, method="hash", threads=1)
                do i = 1_int64, 30000_int64, 997_int64
                    if (maps(t)%get(keys(i)) /= i) bad = bad + 1
                end do
            end do
        end do
        call check(error, bad == 0, "every map built beside another answers every key")
        if (allocated(error)) return
        mark = parquet_debug_index_concurrent_builds()
        call check(error, mark >= 2, &
            "at least two builds must have been in flight at once -- a build that holds the " // &
            "map's lock for its whole duration keeps the high-water mark at 1")
    end subroutine test_concurrent_builds_overlap

    !> Two keys above `above` whose home slots are the last slot of their partition, for a hash
    !! build of `ntot` keys on `nt` threads: what makes a deferral to the spill pass certain when
    !! they are inserted last. Both come from the library's own hash and partition rule through
    !! `parquet_debug_index_partition`, never from a copy of either. `a` and `k` are 0 when such
    !! a build would not partition, and `k` stays 0 when no pair was found among 2**20 candidates.
    subroutine craft_boundary_pair(ntot, nt, above, a, k)
        integer(int64), intent(in) :: ntot   !! keys the build stores.
        integer, intent(in) :: nt            !! the build's `threads=`.
        integer(int64), intent(in) :: above  !! the largest key the fixture already holds.
        integer(int64), intent(out) :: a     !! the first crafted key.
        integer(int64), intent(out) :: k     !! the second.
        integer(int64) :: c, home, part, cap

        a = 0_int64
        k = 0_int64
        do c = above + 1_int64, above + 1048576_int64
            call parquet_debug_index_partition(c, ntot, home, part, cap, threads=nt)
            if (part == 0_int64) return
            if (mod(home, part) /= part - 1_int64) cycle
            if (a == 0_int64) then
                a = c
            else
                k = c
                return
            end if
        end do
    end subroutine craft_boundary_pair

    !> Whether two codings of one stream agree on which rows share a key -- `a(i) == a(j)` exactly
    !! when `b(i) == b(j)` -- with both dense in `1 .. k`; a row coded 0 in both is a masked row
    !! and is skipped, a row coded 0 in one only is a disagreement.
    pure function codes_consistent(a, b, k) result(ok)
        integer(int64), intent(in) :: a(:) !! one coding.
        integer(int64), intent(in) :: b(:) !! the other.
        integer(int64), intent(in) :: k    !! the distinct-key count both must be dense over.
        logical :: ok                      !! `.true.` when the two are relabellings of each other.
        integer(int64), allocatable :: ab(:), ba(:)
        integer(int64) :: i, hi_a, hi_b

        ok = .false.
        if (size(a) /= size(b)) return
        if (minval(a) < 0_int64 .or. minval(b) < 0_int64) return
        hi_a = maxval(a)
        hi_b = maxval(b)
        if (hi_a /= k .or. hi_b /= k) return
        allocate(ab(k), ba(k))
        ab = 0_int64
        ba = 0_int64
        do i = 1_int64, size(a, kind=int64)
            if (a(i) == 0_int64 .and. b(i) == 0_int64) cycle
            if (a(i) == 0_int64 .or. b(i) == 0_int64) return
            if (ab(a(i)) == 0_int64) ab(a(i)) = b(i)
            if (ab(a(i)) /= b(i)) return
            if (ba(b(i)) == 0_int64) ba(b(i)) = a(i)
            if (ba(b(i)) /= a(i)) return
        end do
        ok = all(ab /= 0_int64) .and. all(ba /= 0_int64)
    end function codes_consistent

    !> The partitioned hash build answers every key and every miss -- for dense, strided and
    !! sparse keys, with values, under a mask, and for tuples -- with the hash backend forced
    !! and the team `pf_index_threads` reports; and it did partition and did defer at least one
    !! key, which the spill count says.
    !!
    !! Asserts on ANSWERS only: the partitioned pass places keys in different slots than the
    !! serial loop, so `%keys()` order and `%probe_stats` may legitimately differ, and neither
    !! is compared. Two keys crafted through `parquet_debug_index_partition` to sit on the last
    !! slot of one partition, and placed LAST in the key array, make the spill pass certain to
    !! run: by the time the second is inserted that slot is taken, so its walk meets the
    !! partition boundary at once and is deferred.
    subroutine test_partitioned_build_answers(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(pf_index_map) :: m
        integer(int64), allocatable :: keys(:), vals(:), pairs(:,:)
        logical, allocatable :: mask(:)
        integer(int64), parameter :: n = 200000_int64
        integer(int64) :: i, a, k, cmax, bad, kept
        integer :: p, nt
        character(len=7) :: patterns(3)

#ifndef _OPENMP
        call skip_test(error, "needs OpenMP: without it the hash build never partitions, and the " // &
            "spill assertion below would have nothing to see")
        return
#endif
        nt = pf_index_threads(n + 2_int64)
        if (nt < 2) then
            call skip_test(error, "this machine's affinity mask allows one processor, so no build " // &
                "ever partitions")
            return
        end if
        patterns = ["dense  ", "strided", "sparse "]
        allocate(keys(n + 2_int64), vals(n + 2_int64), mask(n + 2_int64), pairs(n + 2_int64, 2))
        do p = 1, 3
            do i = 1_int64, n
                select case (p)
                case (1)
                    keys(i) = i
                case (2)
                    keys(i) = i * 11_int64
                case default
                    ! An odd multiplier modulo 2**48 is a bijection, so these are distinct.
                    keys(i) = iand(i * 2654435761_int64, 281474976710655_int64)
                end select
            end do
            cmax = maxval(keys(1:n))
            call craft_boundary_pair(n + 2_int64, nt, cmax, a, k)
            call check(error, a > cmax .and. k > a, &
                "fixture: two boundary keys were crafted above the pattern's own keys on " // trim(patterns(p)))
            if (allocated(error)) return
            keys(n + 1_int64) = a
            keys(n + 2_int64) = k
            call m%build(keys, method="hash", threads=nt)
            call check(error, parquet_debug_index_threads_used() == nt, &
                "the build resolved the requested team on " // trim(patterns(p)))
            if (allocated(error)) return
            call check(error, parquet_debug_index_spills() >= 1_int64, &
                "the crafted key must reach the spill pass -- the build partitioned and deferred it -- on " // &
                trim(patterns(p)))
            if (allocated(error)) return
            call check(error, m%nkeys() == n + 2_int64, "every key is stored on " // trim(patterns(p)))
            if (allocated(error)) return
            bad = 0_int64
            do i = 1_int64, n + 2_int64
                if (m%get(keys(i)) /= i) bad = bad + 1_int64
            end do
            call check(error, bad == 0_int64, "every key answers its row on " // trim(patterns(p)))
            if (allocated(error)) return
            bad = 0_int64
            do i = 1_int64, n
                if (m%get(-keys(i)) /= 0_int64) bad = bad + 1_int64
            end do
            call check(error, bad == 0_int64, "every absent key answers 0 on " // trim(patterns(p)))
            if (allocated(error)) return
        end do
        ! Values, and a mask, over the sparse pattern the loop left in `keys`.
        do i = 1_int64, n + 2_int64
            vals(i) = 2_int64 * i + 5_int64
            mask(i) = mod(i, 3_int64) /= 0_int64
        end do
        call m%build(keys, vals, method="hash", threads=nt)
        bad = 0_int64
        do i = 1_int64, n + 2_int64
            if (m%get(keys(i)) /= vals(i)) bad = bad + 1_int64
        end do
        call check(error, bad == 0_int64, "every key answers its value under the partitioned build")
        if (allocated(error)) return
        call m%build(keys, method="hash", threads=nt, valid=mask)
        kept = count(mask, kind=int64)
        call check(error, m%nkeys() == kept, "a masked partitioned build stores the unmasked keys only")
        if (allocated(error)) return
        bad = 0_int64
        do i = 1_int64, n + 2_int64
            if (mask(i)) then
                if (m%get(keys(i)) /= i) bad = bad + 1_int64
            else
                if (m%get(keys(i)) /= 0_int64) bad = bad + 1_int64
            end if
        end do
        call check(error, bad == 0_int64, &
            "a masked partitioned build answers the row for an unmasked key and 0 for a masked one")
        if (allocated(error)) return
        ! Tuples: first every pair sharing its first component, so a walk comparing the first
        ! component alone would accept a neighbour; then pairs distinct in the first.
        do i = 1_int64, n + 2_int64
            pairs(i, 1) = 7_int64
            pairs(i, 2) = keys(i)
        end do
        call m%build(pairs, method="hash", threads=nt)
        call check(error, parquet_debug_index_spills() >= 0_int64, "the composite build ran the partitioned pass")
        if (allocated(error)) return
        bad = 0_int64
        do i = 1_int64, n + 2_int64
            if (m%get(pairs(i, :)) /= i) bad = bad + 1_int64
            if (m%get([7_int64, -keys(i)]) /= 0_int64) bad = bad + 1_int64
        end do
        call check(error, bad == 0_int64, "every shared-first tuple answers its row and every absent one 0")
        if (allocated(error)) return
        do i = 1_int64, n + 2_int64
            pairs(i, 1) = keys(i)
            pairs(i, 2) = mod(i, 5_int64)
        end do
        call m%build(pairs, method="hash", threads=nt)
        bad = 0_int64
        do i = 1_int64, n + 2_int64
            if (m%get(pairs(i, :)) /= i) bad = bad + 1_int64
            if (m%get([keys(i), pairs(i, 2) + 5_int64]) /= 0_int64) bad = bad + 1_int64
        end do
        call check(error, bad == 0_int64, "every distinct-first tuple answers its row and every absent one 0")
    end subroutine test_partitioned_build_answers

    !> A threaded `%get_or_add_many` numbers the distinct keys densely and consistently: equal
    !! keys share a code, distinct keys do not, and the codes are exactly `1 .. k` -- on a fresh
    !! map, on a map already holding keys (old codes kept, new ones continuing above them, and a
    !! handful of new keys taking the serial path the pass keeps for them), under a mask, as
    !! `int32`, and for tuples. `threads=1` is the serial pass and numbers by first appearance;
    !! the team record and the spill count say the other calls did not take it.
    subroutine test_partitioned_get_or_add_many(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(pf_index_map) :: s, p, q
        integer(int64), allocatable :: distinct(:), stream(:), c1(:), c2(:), c3(:), old(:), more(:), pairs(:,:)
        integer(int64), allocatable :: cm1(:), cm2(:)
        integer(int32), allocatable :: c32(:)
        logical, allocatable :: mask(:)
        integer(int64), parameter :: n = 120000_int64, k = 40000_int64, nnew = 12500_int64
        integer(int64) :: i, j, bad, mx, kt
        integer :: nt

#ifndef _OPENMP
        call skip_test(error, "needs OpenMP: without it every get_or_add_many is the serial pass, " // &
            "and the comparison below would compare that pass with itself")
        return
#endif
        nt = pf_index_threads(n)
        if (nt < 2) then
            call skip_test(error, "this machine's affinity mask allows one processor, so no call " // &
                "ever partitions")
            return
        end if
        allocate(distinct(k), stream(n), c1(n), c2(n), c3(n), old(k), more(2_int64 * nnew), c32(n), mask(n))
        allocate(cm1(n), cm2(n), pairs(n, 2))
        do j = 1_int64, k
            distinct(j) = j * 1000003_int64 + 17_int64
        end do
        do i = 1_int64, n
            stream(i) = distinct(1_int64 + mod(i * 7919_int64, k))
            mask(i) = mod(i, 4_int64) /= 0_int64
            ! Squares modulo 3 are 0 or 1, and the three rows sharing a stream key (i, i + k,
            ! i + 2k) take exactly two distinct residues, so every key yields two tuples.
            pairs(i, 1) = stream(i)
            pairs(i, 2) = mod(i * i, 3_int64)
        end do
        call s%init()
        call s%get_or_add_many(stream, c1, threads=1)
        call check(error, parquet_debug_index_threads_used() == 1, "threads=1 forces the serial pass")
        if (allocated(error)) return
        mx = 0_int64
        bad = 0_int64
        do i = 1_int64, n
            if (c1(i) > mx) then
                if (c1(i) /= mx + 1_int64) bad = bad + 1_int64
                mx = c1(i)
            end if
        end do
        call check(error, bad == 0_int64 .and. mx == k, "the serial pass numbers by first appearance")
        if (allocated(error)) return
        call p%init()
        call p%get_or_add_many(stream, c2)
        call check(error, parquet_debug_index_threads_used() == nt, &
            "an automatic get_or_add_many over 120000 rows resolves the team pf_index_threads reports")
        if (allocated(error)) return
        call check(error, parquet_debug_index_spills() >= 0_int64, &
            "the new keys were inserted by the partitioned pass (a spill count of -1 is the serial pass)")
        if (allocated(error)) return
        call check(error, p%nkeys() == k .and. s%nkeys() == k, "both passes store exactly the distinct keys")
        if (allocated(error)) return
        call check(error, codes_consistent(c1, c2, k), &
            "the threaded codes are a relabelling of the serial ones: dense in 1..k, equal keys equal codes")
        if (allocated(error)) return
        bad = 0_int64
        do i = 1_int64, n
            if (p%get(stream(i)) /= c2(i)) bad = bad + 1_int64
        end do
        call check(error, bad == 0_int64, "every row's code is what %get answers for its key afterwards")
        if (allocated(error)) return
        ! A second call on the filled map: the first half old keys, the second half new ones,
        ! each new key twice. Old codes are kept; the new keys continue densely above k.
        do j = 1_int64, k
            old(j) = p%get(distinct(j))
        end do
        do j = 1_int64, nnew
            more(j) = distinct(j)
            more(nnew + j) = distinct(j) + 1_int64
        end do
        call p%get_or_add_many(more, c3(1:2_int64 * nnew))
        call check(error, parquet_debug_index_spills() >= 0_int64, "the second call's 12500 new keys partitioned")
        if (allocated(error)) return
        call check(error, all(c3(1:nnew) == old(1:nnew)), "a key already in the map keeps its code")
        if (allocated(error)) return
        call check(error, p%nkeys() == k + nnew, "the new keys were added once each")
        if (allocated(error)) return
        call check(error, minval(c3(nnew + 1:2_int64 * nnew)) == k + 1_int64 .and. &
            maxval(c3(nnew + 1:2_int64 * nnew)) == k + nnew, "the new codes continue densely above the watermark")
        if (allocated(error)) return
        ! Fewer new keys than the threading floor take the serial path inside the threaded call.
        do j = 1_int64, 100_int64
            more(j) = distinct(j) + 2_int64
        end do
        call p%get_or_add_many(more(1:100), c3(1:100))
        call check(error, parquet_debug_index_spills() == -1_int64 .and. p%nkeys() == k + nnew + 100_int64, &
            "a hundred new keys are added serially inside the threaded call")
        if (allocated(error)) return
        call check(error, all(c3(1:100) == [(k + nnew + i, i = 1_int64, 100_int64)]), &
            "a hundred new keys take the next hundred codes in the caller's order")
        if (allocated(error)) return
        ! A mask: masked rows answer 0 and add nothing; the rest are consistent with the serial pass.
        call s%clear()
        call s%init()
        call s%get_or_add_many(stream, cm1, valid=mask, threads=1)
        call q%init()
        call q%get_or_add_many(stream, cm2, valid=mask)
        call check(error, q%nkeys() == s%nkeys() .and. all(pack(cm2, .not. mask) == 0_int64), &
            "a masked row gets 0 and adds nothing under the threaded pass")
        if (allocated(error)) return
        call check(error, codes_consistent(cm1, cm2, s%nkeys()), &
            "the unmasked rows are coded consistently with the serial pass")
        if (allocated(error)) return
        ! The int32 answer form.
        call q%clear()
        call q%init()
        call q%get_or_add_many(stream, c32)
        call check(error, codes_consistent(c1, int(c32, int64), k), "the int32 form codes consistently")
        if (allocated(error)) return
        ! Tuples.
        call s%clear()
        call s%init(ncomp=2)
        call s%get_or_add_many(pairs, c1, threads=1)
        kt = s%nkeys()
        call q%clear()
        call q%init(ncomp=2)
        call q%get_or_add_many(pairs, c2)
        call check(error, parquet_debug_index_threads_used() == nt .and. parquet_debug_index_spills() >= 0_int64, &
            "a composite get_or_add_many resolves the team and partitions")
        if (allocated(error)) return
        call check(error, q%nkeys() == kt .and. kt > k .and. kt < n, &
            "fixture: the tuples repeat and are more numerous than the scalar keys")
        if (allocated(error)) return
        call check(error, codes_consistent(c1, c2, kt), "the composite codes are a relabelling of the serial ones")
    end subroutine test_partitioned_get_or_add_many

    !> A multimap built on a team -- its grouping pass is the map's threaded `%get_or_add_many`
    !! -- answers every count, every first row and every `%probe_many` range as the serial build
    !! does; only the group ids may differ, and nothing here reads them.
    subroutine test_mm_threaded_grouping(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(pf_index_multimap) :: ms, mp
        integer(int64), allocatable :: distinct(:), keys(:), probes(:), off1(:), m1(:), off2(:), m2(:)
        integer(int64), parameter :: n = 100000_int64, k = 25000_int64, np = 30000_int64
        integer(int64) :: i, j, bad
        integer :: nt

#ifndef _OPENMP
        call skip_test(error, "needs OpenMP: without it the grouping pass is serial on both arms")
        return
#endif
        nt = pf_index_threads(n)
        if (nt < 2) then
            call skip_test(error, "this machine's affinity mask allows one processor, so the " // &
                "grouping pass never threads")
            return
        end if
        allocate(distinct(k), keys(n), probes(np))
        do j = 1_int64, k
            distinct(j) = j * 999983_int64 + 5_int64
        end do
        do i = 1_int64, n
            keys(i) = distinct(1_int64 + mod(i * 7919_int64, k))
        end do
        do i = 1_int64, np
            probes(i) = distinct(1_int64 + mod(i * 104729_int64, k)) + mod(i, 2_int64)
        end do
        call ms%build(keys, method="hash", threads=1)
        call mp%build(keys, method="hash")
        call check(error, parquet_debug_index_threads_used() == nt, "the multimap's grouping pass resolved the team")
        if (allocated(error)) return
        call check(error, mp%ngroups() == k .and. ms%ngroups() == k, "both builds found every distinct key")
        if (allocated(error)) return
        bad = 0_int64
        do j = 1_int64, k
            if (mp%count(distinct(j)) /= ms%count(distinct(j))) bad = bad + 1_int64
            if (mp%get_first(distinct(j)) /= ms%get_first(distinct(j))) bad = bad + 1_int64
        end do
        call check(error, bad == 0_int64, "every key's count and first row agree between the two builds")
        if (allocated(error)) return
        call ms%probe_many(probes, off1, m1, threads=1)
        call mp%probe_many(probes, off2, m2, threads=1)
        call check(error, size(off1) == size(off2) .and. size(m1) == size(m2), "probe_many answers the same shape")
        if (allocated(error)) return
        call check(error, all(off1 == off2) .and. all(m1 == m2), "probe_many answers the same ranges and rows")
        if (allocated(error)) return
        call check(error, size(m1, kind=int64) > np .and. count(off1(2:np + 1) == off1(1:np)) > 1000, &
            "fixture: the probes both repeat and miss in the thousands")
    end subroutine test_mm_threaded_grouping

    ! gcov attribution artifact: an `end module` line is not a statement and reports 0 hits.
end module test_index_omp ! GCOVR_EXCL_LINE
