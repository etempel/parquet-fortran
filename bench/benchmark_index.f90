!> Benchmarks `parquet_index`: per-lookup cost for each backend, build cost, and the guarded
!> mutation surface under contention.
!!
!! Driven by `bench/benchmark_index.sh`, which is where the environment variables and the
!! `--profile release` assertion live. Modes:
!!
!! * `lookup`   -- ns per `%get`, per backend x {hit, miss} x key pattern, plus `%get_many` serial
!!                 and on a team (the arm the filter's per-row-group probe and the join's probe
!!                 pay; the pair reads as the speed-up), and the two baselines every figure is
!!                 read against: a raw array read (the floor the direct backend should sit on)
!!                 and `findloc` (the naive alternative this module replaces).
!! * `build`    -- ns per key to build, per backend, serial and threaded; the hash backend's
!!                 threaded arm is the partitioned insert, with its spill count printed.
!! * `tuple`    -- the same lookup sweep over composite keys at ncomp = 1, 2 and 4: `%get` per
!!                 probe, and the serial `%get_many` over the whole probe array.
!! * `mutate`   -- `%set`, `%get_or_add` and `%get_or_add_many` throughput, one thread and several
!!                 sharing one map: what the named critical costs when it is contended, what the
!!                 bulk form saves by taking it once per call, and what its team saves on top.
!! * `pool`     -- `%get_index`/`%free_index` throughput, private and shared, and `%compact`.
!! * `multimap` -- `pf_index_multimap` on the join's shapes: the build over keys that repeat
!!                 `REPEAT` times on average, `%get_first_many` and `%probe_many` serial and on a
!!                 team, and `pf_match_all` over the same arrays as the sort-engine baseline the
!!                 hash engine is read against.
!! * `strings`  -- string keys: the build of a string map from a `parquet_string_column` of
!!                 `NKEYS` distinct identifiers, serial and on a team, `%get` per key, `%get_many` over a probe column
!!                 (half hits, half misses) serial and on a team, and `pf_in` over the same two
!!                 columns -- the sort-merge the filter's string leaf used to run once per row
!!                 group, and the baseline the map's figure is read against.
!!
!! **Rules this program follows, from CLAUDE.md's benchmarking section.** Every array is written
!! once before any timed loop, so no figure pays first-touch page faults; the row walk uses a
!! wrapping counter rather than `mod` with a runtime divisor, which is an integer division worth
!! about six nanoseconds and was once an entire reported floor; each figure is the best of
!! `ROUNDS` rounds, the run least disturbed by everything else on the machine; and a checksum is
!! accumulated and printed so no arm can be optimised away, computed outside every timed region.
program benchmark_index
    use parquet_index
    use parquet_sorting, only: pf_match_all, pf_in
    use parquet_strings, only: parquet_string_column
    use iso_fortran_env, only: int32, int64, real64, output_unit
#ifdef _OPENMP
    use omp_lib, only: omp_get_wtime, omp_get_max_threads, omp_get_thread_num
#endif
    implicit none

    integer(int64) :: nkeys, naccess, repeat_factor
    integer :: rounds, threads_req, ncomp_max
    character(len=:), allocatable :: mode
    integer(int64) :: checksum

    checksum = 0_int64
    call read_arguments()
    write (output_unit, "(a)") "# benchmark_index -- parquet_index"
    write (output_unit, "(a,i0,a,i0,a,i0)") "# nkeys=", nkeys, " naccess=", naccess, &
        " rounds=", rounds
    write (output_unit, "(a,i0,a,i0,a,i0)") "# threads=", threads_req, " ncomp_max=", ncomp_max, &
        " repeat=", repeat_factor
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
    case ("multimap")
        call mode_multimap()
    case ("strings")
        call mode_strings()
    case ("all")
        call mode_lookup()
        call mode_build()
        call mode_tuple()
        call mode_mutate()
        call mode_pool()
        call mode_multimap()
        call mode_strings()
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
        repeat_factor = 1_int64
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
            else if (index(arg, "--repeat=") == 1) then
                read (arg(10:), *) repeat_factor
            end if
        end do
        if (ncomp_max > pf_index_max_components) ncomp_max = pf_index_max_components
        if (repeat_factor < 1_int64) repeat_factor = 1_int64
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
        integer :: p, b, r, nt
        integer(int64) :: i, j, acc, n
        real(real64) :: t0, best
        character(len=8) :: patterns(3)
        character(len=6) :: backends(3)
        character(len=64) :: tag

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
                ! The bulk form, which is the documented hot-loop shape -- pinned to one thread,
                ! because an unqualified call over this many rows would open a team and the
                ! serial figure is the one the threaded arm below is read against.
                best = huge(1.0_real64)
                do r = 1, rounds
                    t0 = now()
                    call m%get_many(probes, answers, threads=1)
                    best = min(best, now() - t0)
                    checksum = checksum + answers(1) + answers(naccess)
                end do
                call report(trim(patterns(p)) // " " // trim(backends(b)) // ": get_many, threads=1", &
                    best, naccess)
                ! The same call on a team: the shape the filter's per-row-group probe and the
                ! join's probe pay, at the team the library resolves for this many rows or at
                ! the one asked for. Read the speed-up off the pair.
                nt = threads_req
                if (nt < 1) nt = pf_index_threads(naccess)
                best = huge(1.0_real64)
                do r = 1, rounds
                    t0 = now()
                    call m%get_many(probes, answers, threads=nt)
                    best = min(best, now() - t0)
                    checksum = checksum + answers(1) + answers(naccess)
                end do
                write (tag, "(a,i0)") trim(patterns(p)) // " " // trim(backends(b)) // &
                    ": get_many, threads=", nt
                call report(trim(tag), best, naccess)
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
        ! The hash backend's threaded arm is the partitioned insert: the serial arm beside it is
        ! the insert loop it replaces, and the spill count printed after it is how many keys the
        ! pass deferred to its serial tail.
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
                if (trim(backends(b)) == "hash") write (output_unit, "(a,i0)") &
                    "# hash: keys the partitioned build deferred to its spill pass: ", &
                    parquet_debug_index_spills()
            end if
            write (output_unit, "(a,a,i0,a)") "# ", trim(backends(b)) // " memory: ", &
                m%memory_bytes(), " bytes"
        end do
        write (output_unit, "(a)") ""
    end subroutine mode_build

    ! ---- tuple ----

    !> The lookup sweep over composite keys, at each component count.
    subroutine mode_tuple()
        integer(int64), allocatable :: pairs(:,:), probe(:), probes(:,:), probes_t(:,:), answers(:)
        type(pf_index_map) :: m
        integer :: nc, b, r, j
        integer(int64) :: i, acc, n, side, maxp
        real(real64) :: t0, best, meanp
        character(len=6) :: backends(2)
        character(len=48) :: tag

        write (output_unit, "(a)") "## tuple"
        backends = ["direct", "hash  "]
        n = nkeys
        do nc = 1, ncomp_max
            if (nc /= 1 .and. nc /= 2 .and. nc /= 4) cycle
            allocate(pairs(n, nc), probe(nc), probes(naccess, nc), probes_t(nc, naccess), answers(naccess))
            pairs = 0_int64
            probe = 0_int64
            answers = 0_int64
            ! A lattice whose per-component span is the nc-th root of n, rounded UP and then
            ! checked in exact integer arithmetic, so that `side**nc >= n` and every tuple is
            ! distinct at any NKEYS: the product of the spans stays within a factor of about
            ! `(1 + 1/side)**nc` of n, so the direct backend remains admissible at every nc.
            side = max(2_int64, ceiling(real(n, real64) ** (1.0_real64 / real(nc, real64)), int64))
            do while (side ** nc < n)
                side = side + 1_int64
            end do
            do i = 1_int64, n
                do j = 1, nc
                    pairs(i, j) = 1_int64 + mod_wrap((i - 1_int64) / side ** (j - 1), side)
                end do
            end do
            ! The probes are laid out once, OUTSIDE the timed loops, in both shapes the two arms
            ! read: `(nc, naccess)` for the scalar arm, so its per-probe gather is one contiguous
            ! read whatever nc is -- a `(naccess, nc)` row is nc strided loads, each a DRAM miss
            ! at ten million keys, which would charge the composite arms one miss per component
            ! that the probe itself never pays -- and `(naccess, nc)` for the bulk arm, which is
            ! the shape `%get_many` takes.
            do i = 1_int64, naccess
                probes(i, :) = pairs(1_int64 + mod_wrap(i * 7919_int64, n), :)
                probes_t(:, i) = probes(i, :)
            end do
            do b = 1, 2
                call m%build(pairs, method=trim(backends(b)), threads=threads_req_or_absent())
                if (m%nkeys() /= n) then
                    ! Unreachable now that `side**nc >= n` is checked above; kept so that a lattice
                    ! that somehow repeats is reported rather than measured.
                    write (output_unit, "(a,i0,a)") "# ncomp=", nc, &
                        ": lattice was not unique, skipped"
                    exit
                end if
                ! The table's own clustering, so a composite figure is read against it: a mean
                ! probe well above one says the hash is at fault, not the layout.
                if (trim(backends(b)) == "hash") then
                    call m%probe_stats(maxp, meanp)
                    write (output_unit, "(a,i0,a,i0,a,f0.3)") "# ncomp=", nc, &
                        " hash: probe_stats max=", maxp, " mean=", meanp
                end if
                best = huge(1.0_real64)
                do r = 1, rounds
                    t0 = now()
                    acc = 0_int64
                    do i = 1_int64, naccess
                        probe = probes_t(:, i)
                        acc = acc + m%get(probe)
                    end do
                    best = min(best, now() - t0)
                    checksum = checksum + acc
                end do
                write (tag, "(a,i0,a)") "ncomp=", nc, " " // trim(backends(b)) // ": get, hit"
                call report(trim(tag), best, naccess)
                ! The bulk form over the same probes, serial: what the filter's temporal leaf and
                ! the join's tuple path actually pay per key, without the harness's own gather.
                best = huge(1.0_real64)
                do r = 1, rounds
                    t0 = now()
                    call m%get_many(probes, answers, threads=1)
                    best = min(best, now() - t0)
                    checksum = checksum + answers(1) + answers(naccess)
                end do
                write (tag, "(a,i0,a)") "ncomp=", nc, " " // trim(backends(b)) // &
                    ": get_many, threads=1"
                call report(trim(tag), best, naccess)
            end do
            deallocate(pairs, probe, probes, probes_t, answers)
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
        integer(int64), allocatable :: keys(:), codes(:)
        integer(int64) :: i, idx, acc
        integer :: r, team, t
        real(real64) :: t0, best
        character(len=48) :: tag

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
        ! The bulk form of the same encoding: one guard acquisition per call rather than per
        ! key, over the identical key stream, so the pair reads as what the guard costs per key.
        allocate(codes(nkeys))
        codes = 0_int64
        best = huge(1.0_real64)
        do r = 1, rounds
            call m%init()
            t0 = now()
            call m%get_or_add_many(keys, codes, threads=1)
            best = min(best, now() - t0)
            checksum = checksum + m%nkeys() + codes(nkeys)
        end do
        call report("get_or_add_many: threads=1, growing", best, nkeys)
        team = 1
#ifdef _OPENMP
        team = omp_get_max_threads()
        if (threads_req > 0) team = threads_req
#endif
        ! The same call on a team: every key looked up lock-free on the team, the ones not found
        ! inserted by the partitioned pass the hash build uses. Read against the arm above; the
        ! codes differ in order only.
        if (team > 1) then
            best = huge(1.0_real64)
            do r = 1, rounds
                call m%init()
                t0 = now()
                call m%get_or_add_many(keys, codes, threads=team)
                best = min(best, now() - t0)
                checksum = checksum + m%nkeys() + codes(nkeys)
            end do
            write (tag, "(a,i0,a)") "get_or_add_many: threads=", team, ", growing"
            call report(trim(tag), best, nkeys)
        end if
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

    ! ---- multimap ----

    !> `pf_index_multimap` on the join's shapes: the build over keys with repeats, the m:1 and
    !! m:m bulk lookups serial and on a team, and the sort engine over the same arrays.
    !!
    !! The build side is `NKEYS` rows drawn from `NKEYS / REPEAT` distinct sparse keys, so every
    !! key repeats `REPEAT` times on average; the probe side is `NACCESS` keys of which every
    !! second one is a stored key and the rest sit between two of them. `pf_match_all` over the
    !! same two arrays is the baseline: it is what the join uses today, its contract is what
    !! `%probe_many` reproduces, and the two are compared row for row before either is timed.
    subroutine mode_multimap()
        type(pf_index_multimap) :: mm
        integer(int64), allocatable :: distinct(:), keys(:), probes(:), first(:), off(:), m(:), &
            off2(:), m2(:)
        integer(int64) :: i, ndistinct, nm
        integer :: r, nt
        real(real64) :: t0, best
        character(len=64) :: tag

        write (output_unit, "(a)") "## multimap"
        ndistinct = max(1_int64, nkeys / repeat_factor)
        allocate(distinct(ndistinct), keys(nkeys), probes(naccess), first(naccess))
        distinct = 0_int64
        keys = 0_int64
        probes = 0_int64
        first = 0_int64
        call fill_keys("sparse", distinct)
        do i = 1_int64, nkeys
            keys(i) = distinct(1_int64 + mod_wrap(i * 7919_int64, ndistinct))
        end do
        do i = 1_int64, naccess
            probes(i) = distinct(1_int64 + mod_wrap(i * 104729_int64, ndistinct))
            if (mod(i, 2_int64) == 0_int64) probes(i) = probes(i) + 1_int64
        end do
        nt = threads_req
        if (nt < 1) nt = pf_index_threads(naccess)
        write (output_unit, "(a,i0,a,i0,a,i0)") "# rows=", nkeys, " distinct=", ndistinct, &
            " probes=", naccess
        ! The build: serial grouping in this version, so one arm; `threads=` reaches the map.
        best = huge(1.0_real64)
        do r = 1, rounds
            t0 = now()
            call mm%build(keys, threads=1)
            best = min(best, now() - t0)
            checksum = checksum + mm%ngroups()
        end do
        call report("multimap: build, threads=1", best, nkeys)
        if (nt > 1) then
            best = huge(1.0_real64)
            do r = 1, rounds
                t0 = now()
                call mm%build(keys, threads=nt)
                best = min(best, now() - t0)
                checksum = checksum + mm%ngroups()
            end do
            write (tag, "(a,i0)") "multimap: build, threads=", nt
            call report(trim(tag), best, nkeys)
        end if
        write (output_unit, "(a,i0,a,i0,a)") "# multimap memory: ", mm%memory_bytes(), &
            " bytes; max_multiplicity ", mm%max_multiplicity()
        ! The m:1 bulk lookup, serial and on the team.
        best = huge(1.0_real64)
        do r = 1, rounds
            t0 = now()
            call mm%get_first_many(probes, first, threads=1)
            best = min(best, now() - t0)
            checksum = checksum + first(1) + first(naccess)
        end do
        call report("multimap: get_first_many, threads=1", best, naccess)
        if (nt > 1) then
            best = huge(1.0_real64)
            do r = 1, rounds
                t0 = now()
                call mm%get_first_many(probes, first, threads=nt)
                best = min(best, now() - t0)
                checksum = checksum + first(1) + first(naccess)
            end do
            write (tag, "(a,i0)") "multimap: get_first_many, threads=", nt
            call report(trim(tag), best, naccess)
        end if
        ! The m:m CSR probe, serial and on the team; the pair count is printed because it is
        ! what the copy pass is proportional to.
        best = huge(1.0_real64)
        do r = 1, rounds
            t0 = now()
            call mm%probe_many(probes, off, m, threads=1, n_matched=nm)
            best = min(best, now() - t0)
            checksum = checksum + size(m, kind=int64) + nm
        end do
        call report("multimap: probe_many, threads=1", best, naccess)
        ! `kind=int64`: the pair count of a heavy-repeat shape passes 2**31 (ten million probes
        ! over a thousand-row group is five billion pairs), and a default-kind `size` wraps.
        write (output_unit, "(a,i0,a,i0)") "# probe_many pairs=", size(m, kind=int64), &
            " matched probes=", nm
        if (nt > 1) then
            best = huge(1.0_real64)
            do r = 1, rounds
                t0 = now()
                call mm%probe_many(probes, off, m, threads=nt)
                best = min(best, now() - t0)
                checksum = checksum + size(m, kind=int64)
            end do
            write (tag, "(a,i0)") "multimap: probe_many, threads=", nt
            call report(trim(tag), best, naccess)
        end if
        ! The sort engine over the same arrays: what the join runs today. Compared row for row
        ! first, so the figure below is for an answer known to be the same one.
        call pf_match_all(probes, keys, off2, m2, threads=nt)
        if (size(off2) /= size(off) .or. size(m2) /= size(m)) then
            write (output_unit, "(a)") "# ERROR: pf_match_all and probe_many disagree on shape"
            stop 3
        end if
        if (any(off2 /= off) .or. any(m2 /= m)) then
            write (output_unit, "(a)") "# ERROR: pf_match_all and probe_many disagree on content"
            stop 3
        end if
        best = huge(1.0_real64)
        do r = 1, rounds
            t0 = now()
            call pf_match_all(probes, keys, off2, m2, threads=nt)
            best = min(best, now() - t0)
            checksum = checksum + size(m2, kind=int64)
        end do
        write (tag, "(a,i0)") "baseline: pf_match_all, threads=", nt
        call report(trim(tag), best, naccess)
        write (output_unit, "(a)") ""
    end subroutine mode_multimap

    !> One take-and-give-back cycle on a shared pool, returning the index it held.
    function pool_cycle(p) result(idx)
        type(pf_index_pool), intent(inout) :: p !! the shared pool.
        integer(int64) :: idx                   !! the index this cycle held.

        idx = p%get_index()
        call p%free_index(idx)
    end function pool_cycle


    ! ---- strings ----

    !> String keys against the sort-merge they replace in the filter's string leaf.
    !!
    !! The keys are `NKEYS` distinct identifiers of the shape `obj_<n>` over a sparse walk, so
    !! no two share a prefix pattern the hash could exploit; the probes are `NACCESS` of them,
    !! every second one altered so that half miss. `pf_in` over the same two columns is the
    !! baseline: it sorts the concatenation of both, which is what the filter's string leaf
    !! paid once per row group before the map gained string keys, and what a join's string
    !! path pays today.
    subroutine mode_strings()
        type(pf_index_map) :: m
        type(parquet_string_column) :: keys, probes
        integer(int64), allocatable :: answers(:), ids(:)
        logical, allocatable :: hit(:)
        integer(int64) :: i, j, v, nchar
        integer :: r, nt
        real(real64) :: t0, best
        character(len=64) :: tag
        character(len=24) :: txt

        write (output_unit, "(a)") "## strings"
        allocate(ids(nkeys), answers(naccess))
        ids = 0_int64
        answers = 0_int64
        call fill_keys("sparse", ids)
        call keys%clear()
        call keys%reserve(nkeys, nkeys * 16_int64)
        do i = 1_int64, nkeys
            write (txt, "(a,i0)") "obj_", ids(i)
            call keys%append_string(trim(txt))
        end do
        call probes%clear()
        call probes%reserve(naccess, naccess * 16_int64)
        do i = 1_int64, naccess
            j = 1_int64 + mod_wrap(i * 104729_int64, nkeys)
            if (mod(i, 2_int64) == 0_int64) then
                write (txt, "(a,i0,a)") "obj_", ids(j), "x"
            else
                write (txt, "(a,i0)") "obj_", ids(j)
            end if
            call probes%append_string(trim(txt))
        end do
        nchar = keys%character_size()
        nt = threads_req
        if (nt < 1) nt = pf_index_threads(naccess)
        write (output_unit, "(a,i0,a,i0,a,f6.2)") "# keys=", nkeys, " probes=", naccess, &
            " mean key bytes=", real(nchar, real64) / real(nkeys, real64)
        ! The build: the hash of every key plus one copy of its bytes, serial; then on the
        ! team, where the hashing and the copy are threaded and the tuples take the partitioned
        ! insert (the pair reads as the speed-up).
        best = huge(1.0_real64)
        do r = 1, rounds
            t0 = now()
            call m%build(keys, threads=1)
            best = min(best, now() - t0)
            checksum = checksum + m%nkeys()
        end do
        call report("strings: build from a string column, threads=1", best, nkeys)
        if (nt > 1) then
            best = huge(1.0_real64)
            do r = 1, rounds
                t0 = now()
                call m%build(keys, threads=nt)
                best = min(best, now() - t0)
                checksum = checksum + m%nkeys()
            end do
            write (tag, "(a,i0)") "strings: build from a string column, threads=", nt
            call report(trim(tag), best, nkeys)
        end if
        write (output_unit, "(a,i0,a)") "# string map memory: ", m%memory_bytes(), " bytes"
        ! The scalar lookup, one key at a time, through the map's own stored keys.
        best = huge(1.0_real64)
        do r = 1, rounds
            t0 = now()
            v = 0_int64
            do i = 1_int64, naccess
                j = 1_int64 + mod_wrap(i * 104729_int64, nkeys)
                write (txt, "(a,i0)") "obj_", ids(j)
                v = v + m%get(trim(txt))
            end do
            best = min(best, now() - t0)
            checksum = checksum + v
        end do
        call report("strings: get, one key at a time (incl. formatting)", best, naccess)
        ! The bulk lookup over the probe column, in place, serial and on the team.
        best = huge(1.0_real64)
        do r = 1, rounds
            t0 = now()
            call m%get_many(probes, answers, threads=1)
            best = min(best, now() - t0)
            checksum = checksum + answers(1) + answers(naccess)
        end do
        call report("strings: get_many over a string column, threads=1", best, naccess)
        if (nt > 1) then
            best = huge(1.0_real64)
            do r = 1, rounds
                t0 = now()
                call m%get_many(probes, answers, threads=nt)
                best = min(best, now() - t0)
                checksum = checksum + answers(1) + answers(naccess)
            end do
            write (tag, "(a,i0)") "strings: get_many over a string column, threads=", nt
            call report(trim(tag), best, naccess)
        end if
        write (output_unit, "(a,i0)") "# probes found=", count(answers > 0_int64)
        ! The baseline: pf_in over the same two columns, which sorts both.
        call pf_in(probes, keys, hit, threads=nt)
        if (count(hit) /= count(answers > 0_int64)) then
            write (output_unit, "(a)") "# ERROR: pf_in and get_many disagree on the hit count"
            stop 3
        end if
        do i = 1_int64, naccess
            if (hit(i) .neqv. (answers(i) > 0_int64)) then
                write (output_unit, "(a)") "# ERROR: pf_in and get_many disagree on a probe"
                stop 3
            end if
        end do
        best = huge(1.0_real64)
        do r = 1, rounds
            t0 = now()
            call pf_in(probes, keys, hit, threads=nt)
            best = min(best, now() - t0)
            checksum = checksum + count(hit, kind=int64)
        end do
        write (tag, "(a,i0)") "baseline: pf_in over the same columns, threads=", nt
        call report(trim(tag), best, naccess)
        write (output_unit, "(a)") ""
    end subroutine mode_strings

end program benchmark_index
