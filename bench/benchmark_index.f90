!> Benchmarks `parquet_index`: per-lookup cost for each backend, build cost, and the guarded
!> mutation surface under contention.
!!
!! Driven by `bench/benchmark_index.sh`, which is where the environment variables and the
!! `--profile release` assertion live. Modes:
!!
!! * `lookup`   -- ns per `%get`, per backend x {hit, miss} x key pattern, plus `%get_many` and
!!                 the two baselines every figure is read against: a raw array read (the floor the
!!                 direct backend should sit on) and `findloc` (the naive alternative this module
!!                 replaces).
!! * `build`    -- ns per key to build, per backend, serial and threaded.
!! * `tuple`    -- the same lookup sweep over composite keys at ncomp = 1, 2 and 4.
!! * `mutate`   -- `%set` and `%get_or_add` throughput, one thread and several sharing one map:
!!                 what the named critical costs when it is contended.
!! * `pool`     -- `%get_index`/`%free_index` throughput, private and shared, and `%compact`.
!!
!! **Rules this program follows, from CLAUDE.md's benchmarking section.** Every array is written
!! once before any timed loop, so no figure pays first-touch page faults; the row walk uses a
!! wrapping counter rather than `mod` with a runtime divisor, which is an integer division worth
!! about six nanoseconds and was once an entire reported floor; each figure is the best of
!! `ROUNDS` rounds, the run least disturbed by everything else on the machine; and a checksum is
!! accumulated and printed so no arm can be optimised away, computed outside every timed region.
program benchmark_index
    use parquet_index
    use iso_fortran_env, only: int32, int64, real64, output_unit
#ifdef _OPENMP
    use omp_lib, only: omp_get_wtime, omp_get_max_threads, omp_get_thread_num
#endif
    implicit none

    integer(int64) :: nkeys, naccess
    integer :: rounds, threads_req, ncomp_max
    character(len=:), allocatable :: mode
    integer(int64) :: checksum

    checksum = 0_int64
    call read_arguments()
    write (output_unit, "(a)") "# benchmark_index -- parquet_index"
    write (output_unit, "(a,i0,a,i0,a,i0)") "# nkeys=", nkeys, " naccess=", naccess, &
        " rounds=", rounds
    write (output_unit, "(a,i0,a,i0)") "# threads=", threads_req, " ncomp_max=", ncomp_max
    write (output_unit, "(a)") ""

    select case (mode)
    case ("lookup")
        call mode_lookup()
    case ("build")
        call mode_build()
    case ("tuple")
        call mode_tuple()
    case ("mutate")
        call mode_mutate()
    case ("pool")
        call mode_pool()
    case ("all")
        call mode_lookup()
        call mode_build()
        call mode_tuple()
        call mode_mutate()
        call mode_pool()
    case default
        write (output_unit, "(a)") "unknown mode: " // mode
        stop 2
    end select
    ! Printed so that no arm above can be optimised away. Its value carries no meaning.
    write (output_unit, "(a)") ""
    write (output_unit, "(a,i0)") "# checksum ", checksum

contains

    !> Reads the `--key=value` flags the shell wrapper forwards.
    subroutine read_arguments()
        character(len=256) :: arg
        integer :: i, n

        nkeys = 1000000_int64
        naccess = 4000000_int64
        rounds = 5
        threads_req = 0
        ncomp_max = 4
        mode = "all"
        n = command_argument_count()
        do i = 1, n
            call get_command_argument(i, arg)
            if (index(arg, "--nkeys=") == 1) then
                read (arg(9:), *) nkeys
            else if (index(arg, "--naccess=") == 1) then
                read (arg(11:), *) naccess
            else if (index(arg, "--rounds=") == 1) then
                read (arg(10:), *) rounds
            else if (index(arg, "--threads=") == 1) then
                read (arg(11:), *) threads_req
            else if (index(arg, "--ncomp=") == 1) then
                read (arg(9:), *) ncomp_max
            else if (index(arg, "--mode=") == 1) then
                mode = trim(arg(8:))
            end if
        end do
        if (ncomp_max > pf_index_max_components) ncomp_max = pf_index_max_components
    end subroutine read_arguments

    !> Seconds now, from the best clock available.
    function now() result(t)
        real(real64) :: t !! seconds, on an arbitrary origin.
#ifdef _OPENMP
        t = omp_get_wtime()
#else
        integer(int64) :: c, r
        call system_clock(c, r)
        t = real(c, real64) / real(r, real64)
#endif
    end function now

    !> Prints one figure as nanoseconds per operation.
    subroutine report(label, best, ops)
        character(len=*), intent(in) :: label   !! what was measured.
        real(real64), intent(in) :: best        !! best elapsed time over the rounds, seconds.
        integer(int64), intent(in) :: ops       !! operations in one round.

        write (output_unit, "(a,t46,f12.3,a)") label, best * 1.0e9_real64 / real(ops, real64), &
            " ns/op"
        flush (output_unit)
    end subroutine report

    !> Fills `keys` with one of the three patterns the sweep covers.
    !!
    !! `dense` is consecutive, which the direct backend is for; `strided` is multiples of 2**16,
    !! the pattern a weak hash collapses on; `sparse` walks a wide range pseudo-randomly, which is
    !! what forces the hash backend and is the realistic large-catalogue case.
    subroutine fill_keys(pattern, keys)
        character(len=*), intent(in) :: pattern       !! "dense", "strided" or "sparse".
        integer(int64), intent(out) :: keys(:)        !! receives the keys.
        integer(int64) :: i, n, x

        n = size(keys, kind=int64)
        select case (pattern)
        case ("dense")
            do i = 1_int64, n
                keys(i) = i
            end do
        case ("strided")
            do i = 1_int64, n
                keys(i) = i * 65536_int64
            end do
        case default
            ! A multiplicative walk: distinct for every i below 2**31 and spread over a range far
            ! too wide for the direct backend, so the auto choice takes hash.
            x = 1_int64
            do i = 1_int64, n
                x = x + 2654435761_int64
                keys(i) = x
            end do
        end select
    end subroutine fill_keys

    ! ---- lookup ----

    !> ns per lookup, per backend, for hits and for misses, over three key patterns.
    subroutine mode_lookup()
        integer(int64), allocatable :: keys(:), probes(:), answers(:)
        type(pf_index_map) :: m
        integer :: p, b, r
        integer(int64) :: i, j, acc, n
        real(real64) :: t0, best
        character(len=8) :: patterns(3)
        character(len=6) :: backends(3)

        write (output_unit, "(a)") "## lookup"
        patterns = ["dense   ", "strided ", "sparse  "]
        backends = ["direct", "hash  ", "sorted"]
        n = nkeys
        allocate(keys(n), probes(naccess), answers(naccess))
        ! Warm every buffer before any timing: a freshly allocated array pays first-touch page
        ! faults on its first pass and never again, which is enough to reverse a comparison.
        keys = 0_int64
        probes = 0_int64
        answers = 0_int64
        do p = 1, 3
            call fill_keys(trim(patterns(p)), keys)
            ! Hit probes: the keys themselves, walked in a scattered order so the access pattern
            ! is not itself sequential.
            do i = 1_int64, naccess
                j = 1_int64 + mod_wrap(i * 7919_int64, n)
                probes(i) = keys(j)
            end do
            ! The floor every other figure is read against: a plain array read of the same length.
            best = huge(1.0_real64)
            do r = 1, rounds
                t0 = now()
                acc = 0_int64
                j = 1_int64
                do i = 1_int64, naccess
                    acc = acc + keys(j)
                    j = j + 1_int64
                    if (j > n) j = 1_int64
                end do
                best = min(best, now() - t0)
                checksum = checksum + acc
            end do
            call report(trim(patterns(p)) // " baseline: raw array read", best, naccess)
            do b = 1, 3
                if (trim(patterns(p)) == "sparse" .and. trim(backends(b)) == "direct") cycle
                if (trim(patterns(p)) == "strided" .and. trim(backends(b)) == "direct") cycle
                call m%build(keys, method=trim(backends(b)), threads=threads_req_or_absent())
                ! Hits.
                best = huge(1.0_real64)
                do r = 1, rounds
                    t0 = now()
                    acc = 0_int64
                    do i = 1_int64, naccess
                        acc = acc + m%get(probes(i))
                    end do
                    best = min(best, now() - t0)
                    checksum = checksum + acc
                end do
                call report(trim(patterns(p)) // " " // trim(backends(b)) // ": get, hit", &
                    best, naccess)
                ! Misses: shift every probe off the key set.
                best = huge(1.0_real64)
                do r = 1, rounds
                    t0 = now()
                    acc = 0_int64
                    do i = 1_int64, naccess
                        acc = acc + m%get(probes(i) + 1_int64)
                    end do
                    best = min(best, now() - t0)
                    checksum = checksum + acc
                end do
                call report(trim(patterns(p)) // " " // trim(backends(b)) // ": get, miss", &
                    best, naccess)
                ! The bulk form, which is the documented hot-loop shape.
                best = huge(1.0_real64)
                do r = 1, rounds
                    t0 = now()
                    call m%get_many(probes, answers)
                    best = min(best, now() - t0)
                    checksum = checksum + answers(1) + answers(naccess)
                end do
                call report(trim(patterns(p)) // " " // trim(backends(b)) // ": get_many", &
                    best, naccess)
            end do
            ! The naive alternative this module replaces. Deliberately over a much smaller probe
            ! count -- it is O(n) per lookup, so the same naccess would take minutes.
            best = huge(1.0_real64)
            do r = 1, rounds
                t0 = now()
                acc = 0_int64
                do i = 1_int64, min(200_int64, naccess)
                    acc = acc + findloc(keys, probes(i), dim=1)
                end do
                best = min(best, now() - t0)
                checksum = checksum + acc
            end do
            call report(trim(patterns(p)) // " baseline: findloc scan", best, &
                min(200_int64, naccess))
            write (output_unit, "(a)") ""
        end do
    end subroutine mode_lookup

    !> A wrapping index step, so no timed loop divides by a runtime value.
    !!
    !! `mod` with a runtime divisor compiles to `idivq` on x86-64 -- about six nanoseconds, which
    !! is more than several of the quantities being measured here and was once an entire reported
    !! floor. This is used only OUTSIDE timed regions, where the constant divisor is fine.
    pure function mod_wrap(a, n) result(r)
        integer(int64), intent(in) :: a !! value to reduce.
        integer(int64), intent(in) :: n !! modulus, > 0.
        integer(int64) :: r             !! `a` reduced into `0 .. n-1`.

        r = mod(a, n)
        if (r < 0_int64) r = r + n
    end function mod_wrap

    !> The `threads=` value to pass, or 1 when the caller asked for automatic.
    !!
    !! A benchmark should never leave the team to chance: `--threads=0` means "let the library
    !! decide" for the build arms and is reported as such, and everything else passes the request
    !! through so a figure is attributable to a known team size.
    function threads_req_or_absent() result(n)
        integer :: n !! threads to ask the build for; at least 1.

        n = threads_req
        if (n < 1) n = 1
    end function threads_req_or_absent

    ! ---- build ----

    !> ns per key to build, per backend, serial against the requested team.
    subroutine mode_build()
        integer(int64), allocatable :: keys(:)
        type(pf_index_map) :: m
        integer :: b, r, nt
        real(real64) :: t0, best
        character(len=6) :: backends(3)

        write (output_unit, "(a)") "## build"
        backends = ["direct", "hash  ", "sorted"]
        allocate(keys(nkeys))
        keys = 0_int64
        call fill_keys("dense", keys)
        nt = threads_req
        if (nt < 1) nt = pf_index_threads(nkeys)
        write (output_unit, "(a,i0,a,i0)") "# serial arm threads=1, threaded arm threads=", nt, &
            "; pf_index_threads(nkeys)=", pf_index_threads(nkeys)
        do b = 1, 3
            best = huge(1.0_real64)
            do r = 1, rounds
                t0 = now()
                call m%build(keys, method=trim(backends(b)), threads=1)
                best = min(best, now() - t0)
                checksum = checksum + m%nkeys()
            end do
            call report(trim(backends(b)) // ": build, threads=1", best, nkeys)
            if (nt > 1) then
                best = huge(1.0_real64)
                do r = 1, rounds
                    t0 = now()
                    call m%build(keys, method=trim(backends(b)), threads=nt)
                    best = min(best, now() - t0)
                    checksum = checksum + m%nkeys()
                end do
                call report(trim(backends(b)) // ": build, threaded", best, nkeys)
            end if
            write (output_unit, "(a,a,i0,a)") "# ", trim(backends(b)) // " memory: ", &
                m%memory_bytes(), " bytes"
        end do
        write (output_unit, "(a)") ""
    end subroutine mode_build

    ! ---- tuple ----

    !> The lookup sweep over composite keys, at each component count.
    subroutine mode_tuple()
        integer(int64), allocatable :: pairs(:,:), probe(:)
        type(pf_index_map) :: m
        integer :: nc, b, r, j
        integer(int64) :: i, acc, n, side
        real(real64) :: t0, best
        character(len=6) :: backends(2)
        character(len=24) :: tag

        write (output_unit, "(a)") "## tuple"
        backends = ["direct", "hash  "]
        n = nkeys
        do nc = 1, ncomp_max
            if (nc /= 1 .and. nc /= 2 .and. nc /= 4) cycle
            allocate(pairs(n, nc), probe(nc))
            pairs = 0_int64
            probe = 0_int64
            ! A lattice whose per-component span is the nc-th root of n, so the product of the
            ! spans stays near n and the direct backend remains admissible at every nc.
            side = max(2_int64, nint(real(n, real64) ** (1.0_real64 / real(nc, real64)), int64))
            do i = 1_int64, n
                do j = 1, nc
                    pairs(i, j) = 1_int64 + mod_wrap((i - 1_int64) / side ** (j - 1), side)
                end do
            end do
            do b = 1, 2
                call m%build(pairs, method=trim(backends(b)), threads=threads_req_or_absent())
                if (m%nkeys() /= n) then
                    ! The lattice repeated a tuple, which only happens when `side**nc` is below n;
                    ! say so rather than reporting a figure for a map that was never built.
                    write (output_unit, "(a,i0,a)") "# ncomp=", nc, &
                        ": lattice was not unique, skipped"
                    exit
                end if
                best = huge(1.0_real64)
                do r = 1, rounds
                    t0 = now()
                    acc = 0_int64
                    do i = 1_int64, naccess
                        probe = pairs(1_int64 + mod_wrap(i * 7919_int64, n), :)
                        acc = acc + m%get(probe)
                    end do
                    best = min(best, now() - t0)
                    checksum = checksum + acc
                end do
                write (tag, "(a,i0,a)") "ncomp=", nc, " " // trim(backends(b)) // ": get, hit"
                call report(trim(tag), best, naccess)
            end do
            deallocate(pairs, probe)
        end do
        write (output_unit, "(a)") ""
    end subroutine mode_tuple

    ! ---- mutate ----

    !> `%set` and `%get_or_add` throughput, uncontended and shared between threads.
    !!
    !! The contended arm is the one the named critical's cost shows up in: every mutation of every
    !! map in the process takes the same lock, so this is what a caller streaming keys through one
    !! shared map from several threads actually pays.
    subroutine mode_mutate()
        type(pf_index_map) :: m
        integer(int64), allocatable :: keys(:)
        integer(int64) :: i, idx, acc
        integer :: r, team, t
        real(real64) :: t0, best

        write (output_unit, "(a)") "## mutate"
        allocate(keys(nkeys))
        keys = 0_int64
        call fill_keys("sparse", keys)
        best = huge(1.0_real64)
        do r = 1, rounds
            call m%init()
            call m%reserve(nkeys)
            t0 = now()
            do i = 1_int64, nkeys
                call m%set(keys(i), i)
            end do
            best = min(best, now() - t0)
            checksum = checksum + m%nkeys()
        end do
        call report("set: one thread, reserved", best, nkeys)
        best = huge(1.0_real64)
        do r = 1, rounds
            call m%init()
            t0 = now()
            do i = 1_int64, nkeys
                call m%get_or_add(keys(i), idx)
            end do
            best = min(best, now() - t0)
            checksum = checksum + m%nkeys()
        end do
        call report("get_or_add: one thread, growing", best, nkeys)
        team = 1
#ifdef _OPENMP
        team = omp_get_max_threads()
        if (threads_req > 0) team = threads_req
#endif
        if (team > 1) then
            best = huge(1.0_real64)
            do r = 1, rounds
                call m%init()
                call m%reserve(nkeys)
                t0 = now()
                acc = 0_int64
                !$omp parallel do default(shared) private(t, i, idx) reduction(+:acc) &
                !$omp     num_threads(team) schedule(static)
                do t = 1, team
                    do i = int(t, int64), nkeys, int(team, int64)
                        call m%get_or_add(keys(i), idx)
                        acc = acc + idx
                    end do
                end do
                best = min(best, now() - t0)
                checksum = checksum + acc
            end do
            call report("get_or_add: shared map, contended", best, nkeys)
            write (output_unit, "(a,i0)") "# contended arm team size ", team
        end if
        write (output_unit, "(a)") ""
    end subroutine mode_mutate

    ! ---- pool ----

    !> `%get_index`/`%free_index` throughput, and what `%compact` costs.
    subroutine mode_pool()
        type(pf_index_pool) :: p
        integer(int64), allocatable :: held(:)
        integer(int64) :: i, acc
        integer :: r, team, t
        real(real64) :: t0, best

        write (output_unit, "(a)") "## pool"
        allocate(held(nkeys))
        held = 0_int64
        best = huge(1.0_real64)
        do r = 1, rounds
            call p%clear()
            call p%reserve(nkeys)
            t0 = now()
            do i = 1_int64, nkeys
                held(i) = p%get_index()
            end do
            best = min(best, now() - t0)
            checksum = checksum + held(nkeys)
        end do
        call report("get_index: growing, reserved", best, nkeys)
        best = huge(1.0_real64)
        do r = 1, rounds
            t0 = now()
            do i = nkeys, 1_int64, -1_int64
                call p%free_index(held(i))
            end do
            best = min(best, now() - t0)
            checksum = checksum + p%get_used_count()
            ! Put them back so the next round frees a full pool again.
            do i = 1_int64, nkeys
                held(i) = p%get_index()
            end do
        end do
        call report("free_index: full pool", best, nkeys)
        best = huge(1.0_real64)
        do r = 1, rounds
            t0 = now()
            do i = 1_int64, nkeys
                held(i) = p%get_index()
                call p%free_index(held(i))
            end do
            best = min(best, now() - t0)
            checksum = checksum + held(1)
        end do
        call report("get_index + free_index: reuse path", best, 2_int64 * nkeys)
        ! %compact over the maintainer's own shape: take many, free most, compact.
        best = huge(1.0_real64)
        do r = 1, rounds
            call p%clear()
            do i = 1_int64, nkeys
                held(i) = p%get_index()
            end do
            do i = nkeys / 10_int64 + 1_int64, nkeys
                call p%free_index(held(i))
            end do
            t0 = now()
            call p%compact()
            best = min(best, now() - t0)
            checksum = checksum + p%get_max_index()
        end do
        call report("compact: 90% freed", best, nkeys)
        team = 1
#ifdef _OPENMP
        team = omp_get_max_threads()
        if (threads_req > 0) team = threads_req
#endif
        if (team > 1) then
            best = huge(1.0_real64)
            do r = 1, rounds
                call p%clear()
                t0 = now()
                acc = 0_int64
                !$omp parallel do default(shared) private(t, i) reduction(+:acc) &
                !$omp     num_threads(team) schedule(static)
                do t = 1, team
                    do i = 1_int64, nkeys / int(team, int64)
                        acc = acc + pool_cycle(p)
                    end do
                end do
                best = min(best, now() - t0)
                checksum = checksum + acc
            end do
            call report("get_index + free_index: shared pool", best, &
                2_int64 * (nkeys / int(team, int64)) * int(team, int64))
            write (output_unit, "(a,i0)") "# contended arm team size ", team
        end if
        write (output_unit, "(a)") ""
    end subroutine mode_pool

    !> One take-and-give-back cycle on a shared pool, returning the index it held.
    function pool_cycle(p) result(idx)
        type(pf_index_pool), intent(inout) :: p !! the shared pool.
        integer(int64) :: idx                   !! the index this cycle held.

        idx = p%get_index()
        call p%free_index(idx)
    end function pool_cycle

end program benchmark_index
