!> The hash backend's mutation side: insertion, growth, backward-shift deletion, the
!> partitioned insert that threads a build and `%get_or_add_many`, and the two diagnostics that
!> walk the whole table.
!!
!! **The mixer and the lookup probe are NOT here.** `ix_mix32`, `ix_hash_bits`, `ix_hash_one`,
!! `ix_hash_tuple`, `ix_hash_str` and the two probes (`ix_probe_1`, `ix_probe_n`) live in
!! `parquet_index_map.f90`, in the same translation unit as `%get` and the chunk loops of
!! `%get_many`, so that the compiler inlines them into the lookup loops: a probe that has to call
!! across a submodule boundary pays about three nanoseconds per key, a third of a small-map probe
!! This file is a DESCENDANT of that submodule and reaches the same functions by host association,
!! so the two sides hash identically by construction -- there is one mixer, not a copy of it on each
!! side.
!!
!! Open addressing with linear probing, a power-of-two capacity and a maximum load factor of 0.6.
!! Three properties of the layout are worth stating because each one removes something a hash
!! table usually needs:
!!
!! * **`val == 0` marks a slot empty**, which the values->=1 contract buys. So no key value is
!!   reserved, the whole int64 key domain is available, and there is no occupancy bitmap to keep in
!!   step with the table. It also keeps the module clear of the most-negative-int64 constant that
!!   a reserved-key design would want, which nagfor 7.2 miscompiles (CLAUDE.md's nagfor gotchas).
!! * **Deletion shifts backwards rather than leaving a tombstone.** About twenty lines, and every
!!   later lookup stays tombstone-free -- where a tombstone design pays on every probe and needs a
!!   rehash policy of its own once a delete-heavy workload has filled the table with them.
!! * **The slot index is `iand(h, cap - 1)`, never `mod`.** A `mod` by a runtime divisor is an
!!   integer division, around six nanoseconds on x86-64, which is more than the rest of a lookup.
!!
!! **Two slot layouts, each one record per slot.** A single-component map keeps `(key, val)` as an
!! `ix_slot` in `slots(cap)`; a composite map keeps `(key_1 .. key_ncomp, val)` as one column of
!! `hrec(ncomp + 1, cap)`, so that a probe touches one cache line for `ncomp <= 7` rather than one
!! line of keys and a second of values -- which cost 2.3x at ten million keys when the two were
!! separate arrays. A 1-tuple never reaches the composite layout: every entry below routes `ncomp
!! <= 1` to the scalar table, which is what makes a scalar key and a 1-tuple the same key without
!! the two hashes having to agree.
!!
!! **Slot numbers are 0-based inside this file** and converted at the point of array access
!! (`slots(s + 1)`, `hrec(:, s + 1)`). That is what makes the cyclic arithmetic ordinary: `iand(h,
!! m)` lands in range, `iand(s + 1, m)` advances with wraparound, and the deletion test below can
!! compare slot numbers directly instead of correcting for the base at every step.
!!
!! **The partitioned insert (`ix_hash_build_part_1`/`_n`, `ix_hash_goam_part_1`/`_n`)** is the
!! threaded arm of the hash build and of `%get_or_add_many`: the keys are scattered into the
!! order of their home slots' partitions and each thread fills its own slot ranges, a chain that
!! reaches a range boundary being deferred to a serial spill pass rather than followed across
!! it. The section below says why that is correct.
!!
!! **Every abort goes through `ix_abort`**, the module's serialised reporter, because a build runs
!! OUTSIDE `pf_index_map_guard` now (`ix_adopt` in `parquet_index_map.f90`) and two builds may
!! therefore fail at the same moment; see the reporter's doc-comment for the property it keeps.
submodule (parquet_index:parquet_index_map) parquet_index_hash
    implicit none

contains

    !> Grows the hash table when the load factor demands it. See the interface in
    !! parquet_index.f90. Implemented FIRST in this file because nagfor rejects a separate
    !! module procedure whose body follows a call to it in the same submodule.
    module procedure ix_hash_reserve
        integer(int64) :: need

        ! **The early return is what keeps a bulk build linear**, not a micro-optimisation. Every
        ! insert calls this with `nk + 1`, and `ix_hash_cap_for` walks the capacity up by doubling
        ! from `IX_MIN_HASH_CAP` -- about seventeen iterations plus a division at a million keys,
        ! computed and discarded on every single key. Testing the load factor against the table
        ! that is already there answers the common case in two arithmetic operations.
        !
        ! The multiplication is guarded rather than assumed safe: `hcap * IX_MAX_LOAD_PCT`
        ! overflows for a capacity above `huge / 60`, which no real table reaches but which the
        ! reader should not have to verify from the call sites.
        if (self%hcap > 0_int64 .and. self%hcap <= huge(0_int64) / IX_MAX_LOAD_PCT) then
            if (want <= (self%hcap * IX_MAX_LOAD_PCT) / 100_int64) return
        end if
        need = ix_hash_cap_for(want)
        if (need > self%hcap) call ix_hash_rehash(self, need)
    end procedure ix_hash_reserve

    ! ---- Insertion ----

    !> Aborts when a placement scan has walked the whole table without finding an empty slot.
    !!
    !! **Unreachable while the load factor holds, which is exactly why it is here.** A table kept
    !! at 0.6 always has an empty slot, so this cannot fire in a correct build -- but if `nk` ever
    !! disagreed with the real occupancy the table would stop growing, and the placement scan below
    !! would then spin FOREVER rather than fail. A hang in a library is far harder to diagnose than
    !! an abort naming the invariant that broke, and this costs one compare per probe on the
    !! guarded mutation path only. Confirmed reachable by mutation: deleting the `nk` increment
    !! from an insert hangs the test suite without it.
    !!
    !! The LOOKUP path is deliberately left unbounded and pays nothing. It does not need a bound of
    !! its own: a bounded, correct insert is what stops the table ever filling, so protecting the
    !! one place that could create the condition protects every reader of it too.
    ! GCOVR_EXCL_START
    subroutine ix_hash_table_full()

        call ix_abort("pf_index_map: the hash table is full, which cannot happen while the " // &
            "load factor holds -- the key count and the table's real occupancy have diverged")
    end subroutine ix_hash_table_full
    ! GCOVR_EXCL_STOP

    module procedure ix_hash_insert_scalar
        integer(int64) :: s, m, walked

        call ix_hash_reserve(self, self%nk + 1_int64)
        m = self%hcap - 1_int64
        s = iand(ix_hash_one(key), m)
        walked = 0_int64
        do
            if (self%slots(s + 1_int64)%val == 0_int64) exit
            if (self%slots(s + 1_int64)%key == key) then
                self%slots(s + 1_int64)%val = value
                is_new = .false.
                return
            end if
            s = iand(s + 1_int64, m)
            walked = walked + 1_int64
            if (walked > self%hcap) call ix_hash_table_full()
        end do
        self%slots(s + 1_int64)%key = key
        self%slots(s + 1_int64)%val = value
        self%nk = self%nk + 1_int64
        is_new = .true.
    end procedure ix_hash_insert_scalar

    module procedure ix_hash_insert
        integer(int64) :: s, m, walked
        integer :: j, nc, nv
        logical :: hit

        if (self%ncomp <= 1) then
            call ix_hash_insert_scalar(self, key(1), value, is_new)
            return
        end if
        nc = self%ncomp
        nv = nc + 1
        call ix_hash_reserve(self, self%nk + 1_int64)
        m = self%hcap - 1_int64
        s = iand(ix_hash_tuple(key, nc), m)
        walked = 0_int64
        do
            if (self%hrec(nv, s + 1_int64) == 0_int64) exit
            hit = .true.
            do j = 1, nc
                if (self%hrec(j, s + 1_int64) /= key(j)) then
                    hit = .false.
                    exit
                end if
            end do
            if (hit) then
                self%hrec(nv, s + 1_int64) = value
                is_new = .false.
                return
            end if
            s = iand(s + 1_int64, m)
            walked = walked + 1_int64
            if (walked > self%hcap) call ix_hash_table_full()
        end do
        do j = 1, nc
            self%hrec(j, s + 1_int64) = key(j)
        end do
        self%hrec(nv, s + 1_int64) = value
        self%nk = self%nk + 1_int64
        is_new = .true.
    end procedure ix_hash_insert

    ! ---- Removal: backward-shift deletion ----

    !> Whether slot number `k` lies in the cyclic interval `(i, j]`.
    !!
    !! The test backward-shift deletion turns on. An entry whose home slot is cyclically within
    !! `(i, j]` is still reachable from its home once slot `i` has been emptied, so it stays put;
    !! anything else has to move into `i` or the probe chain leading to it is broken and the entry
    !! becomes invisible. Both arms are needed because the interval may wrap the end of the table.
    pure function ix_cyclic_in(i, k, j) result(inside)
        integer(int64), intent(in) :: i !! the slot being emptied.
        integer(int64), intent(in) :: k !! the home slot of the entry under consideration.
        integer(int64), intent(in) :: j !! the slot that entry currently occupies.
        logical :: inside               !! `.true.` when `k` is in `(i, j]` going forwards from `i`.

        if (i < j) then
            inside = (k > i .and. k <= j)
        else
            inside = (k > i .or. k <= j)
        end if
    end function ix_cyclic_in

    module procedure ix_hash_remove_scalar
        integer(int64) :: s, m, i, j, k

        found = .false.
        if (self%hcap <= 0_int64) return
        m = self%hcap - 1_int64
        s = iand(ix_hash_one(key), m)
        do
            if (self%slots(s + 1_int64)%val == 0_int64) return
            if (self%slots(s + 1_int64)%key == key) exit
            s = iand(s + 1_int64, m)
        end do
        found = .true.
        i = s
        self%slots(i + 1_int64)%val = 0_int64
        j = i
        do
            j = iand(j + 1_int64, m)
            if (self%slots(j + 1_int64)%val == 0_int64) exit
            k = iand(ix_hash_one(self%slots(j + 1_int64)%key), m)
            if (.not. ix_cyclic_in(i, k, j)) then
                self%slots(i + 1_int64) = self%slots(j + 1_int64)
                self%slots(j + 1_int64)%val = 0_int64
                i = j
            end if
        end do
        self%nk = self%nk - 1_int64
    end procedure ix_hash_remove_scalar

    module procedure ix_hash_remove
        integer(int64) :: s, m, i, j, k
        integer :: c, nc, nv
        logical :: hit

        found = .false.
        if (self%ncomp <= 1) then
            call ix_hash_remove_scalar(self, key(1), found)
            return
        end if
        if (self%hcap <= 0_int64) return
        nc = self%ncomp
        nv = nc + 1
        m = self%hcap - 1_int64
        s = iand(ix_hash_tuple(key, nc), m)
        do
            if (self%hrec(nv, s + 1_int64) == 0_int64) return
            hit = .true.
            do c = 1, nc
                if (self%hrec(c, s + 1_int64) /= key(c)) then
                    hit = .false.
                    exit
                end if
            end do
            if (hit) exit
            s = iand(s + 1_int64, m)
        end do
        found = .true.
        i = s
        self%hrec(nv, i + 1_int64) = 0_int64
        j = i
        do
            j = iand(j + 1_int64, m)
            if (self%hrec(nv, j + 1_int64) == 0_int64) exit
            k = iand(ix_hash_tuple(self%hrec(1:nc, j + 1_int64), nc), m)
            if (.not. ix_cyclic_in(i, k, j)) then
                do c = 1, nv
                    self%hrec(c, i + 1_int64) = self%hrec(c, j + 1_int64)
                end do
                self%hrec(nv, j + 1_int64) = 0_int64
                i = j
            end if
        end do
        self%nk = self%nk - 1_int64
    end procedure ix_hash_remove

    ! ---- Sizing and growth ----

    !> The smallest power-of-two capacity at which `want` keys sit under the load factor.
    !!
    !! `100 * want <= IX_MAX_LOAD_PCT * cap` in exact integer arithmetic, so the doubling point
    !! does not depend on how a real comparison rounds. The refusal above `huge / 100` is
    !! unreachable with any real key set -- it would need 9e16 keys -- and is here so that the
    !! multiplication below can be read as safe without tracing the caller. `pure`, so the abort
    !! is a bare `error stop` rather than the reporter (an OpenMP directive may not appear in a
    !! pure procedure); unreachable, so nothing is lost.
    pure function ix_hash_cap_for(want) result(cap)
        integer(int64), intent(in) :: want !! keys the table must hold.
        integer(int64) :: cap              !! a power of two, at least `IX_MIN_HASH_CAP`.
        integer(int64) :: need

        if (want > huge(0_int64) / 100_int64) &
            error stop "pf_index_map: key count is too large for a hash table" ! GCOVR_EXCL_LINE
        need = (want * 100_int64 + IX_MAX_LOAD_PCT - 1_int64) / IX_MAX_LOAD_PCT
        cap = IX_MIN_HASH_CAP
        do while (cap < need)
            cap = cap * 2_int64
        end do
    end function ix_hash_cap_for

    !> Rebuilds the table at `newcap` slots, reinserting every stored key.
    !!
    !! The reinsertion loop needs no duplicate check and no growth check: the keys already in the
    !! table are distinct by construction and the new table is large enough by construction, so
    !! this is a straight probe-to-the-first-empty-slot placement.
    subroutine ix_hash_rehash(self, newcap)
        type(pf_index_map), intent(inout) :: self !! a map whose backend is `IX_HASH`.
        integer(int64), intent(in) :: newcap      !! the new capacity; a power of two.
        type(ix_slot), allocatable :: fresh(:)
        integer(int64), allocatable :: fr(:,:)
        integer(int64) :: s, m, t, old
        integer :: c, nc, nv

        m = newcap - 1_int64
        old = self%hcap
        if (self%ncomp <= 1) then
            allocate(fresh(newcap))
            fresh(:)%key = 0_int64
            fresh(:)%val = 0_int64
            do t = 1_int64, old
                if (self%slots(t)%val == 0_int64) cycle
                s = iand(ix_hash_one(self%slots(t)%key), m)
                do while (fresh(s + 1_int64)%val /= 0_int64)
                    s = iand(s + 1_int64, m)
                end do
                fresh(s + 1_int64) = self%slots(t)
            end do
            call move_alloc(fresh, self%slots)
        else
            nc = self%ncomp
            nv = nc + 1
            allocate(fr(nv, newcap))
            fr = 0_int64
            do t = 1_int64, old
                if (self%hrec(nv, t) == 0_int64) cycle
                s = iand(ix_hash_tuple(self%hrec(1:nc, t), nc), m)
                do while (fr(nv, s + 1_int64) /= 0_int64)
                    s = iand(s + 1_int64, m)
                end do
                do c = 1, nv
                    fr(c, s + 1_int64) = self%hrec(c, t)
                end do
            end do
            call move_alloc(fr, self%hrec)
        end if
        self%hcap = newcap
    end subroutine ix_hash_rehash

    ! ---- Diagnostics ----

    module procedure ix_hash_probe_stats
        integer(int64) :: t, s, m, d, total
        integer :: nc, nv

        max_probe = 0_int64
        mean_probe = 0.0_real64
        if (self%hcap <= 0_int64 .or. self%nk == 0_int64) return
        m = self%hcap - 1_int64
        nc = self%ncomp
        nv = nc + 1
        total = 0_int64
        do t = 1_int64, self%hcap
            if (nc <= 1) then
                if (self%slots(t)%val == 0_int64) cycle
                s = iand(ix_hash_one(self%slots(t)%key), m)
            else
                if (self%hrec(nv, t) == 0_int64) cycle
                s = iand(ix_hash_tuple(self%hrec(1:nc, t), nc), m)
            end if
            ! Cyclic distance from the home slot to where the key actually sits, counting the home
            ! slot itself as one probe.
            d = iand(t - 1_int64 - s, m) + 1_int64
            total = total + d
            if (d > max_probe) max_probe = d
        end do
        mean_probe = real(total, real64) / real(self%nk, real64)
    end procedure ix_hash_probe_stats

    module procedure ix_hash_collect
        integer(int64) :: t, k
        integer :: c, nc, nv

        k = 0_int64
        nc = self%ncomp
        nv = nc + 1
        do t = 1_int64, self%hcap
            if (nc <= 1) then
                if (self%slots(t)%val == 0_int64) cycle
                k = k + 1_int64
                out(k, 1) = self%slots(t)%key
            else
                if (self%hrec(nv, t) == 0_int64) cycle
                k = k + 1_int64
                do c = 1, nc
                    out(k, c) = self%hrec(c, t)
                end do
            end if
        end do
    end procedure ix_hash_collect

    ! ---- The partitioned insert: the threaded arm of the hash build and of %get_or_add_many ----
    !
    ! Five passes, each of which either runs on the team with no two threads writing the same
    ! element, or runs serially:
    !
    !   1. the table -- allocated for the keys and zeroed on the team (`ix_hash_alloc_team`):
    !      zeroing a 268 MB table serially was two thirds of a ten-million-key build;
    !   2. the plan -- every counted row's partition from its hash, one histogram per chunk on
    !      the team, then the write cursors (`ix_partition_plan`);
    !   3. the scatter -- key and value into partition order, on the team;
    !   4. the insert -- one partition at a time on the team (`schedule(dynamic)`), each thread
    !      hashing its keys again and walking only inside its partition's slot range, DEFERRING
    !      a chain that reaches the range's last slot (`ix_part_insert_1`/`_n`);
    !   5. the spill pass -- the deferred keys, serially, walking the whole table as an ordinary
    !      insert does (`ix_part_spill_1`/`_n`).
    !
    ! The hash is computed twice, in the plan and in the insert, rather than stored: a stored
    ! home slot is another array of the key count written once and read twice, and at ten
    ! million keys it cost more than the hashing it saved. The scratch is one `integer(int32)`
    ! partition id per row plus the keys and values in partition order.
    !
    ! Why the result is a correct table: a lookup walks from a key's home slot to the first empty
    ! slot, so it finds a key wherever any order of insertion put it, PROVIDED no slot between the
    ! key's home and its position is empty -- and a slot, once filled, is never emptied during a
    ! pass, so a key placed at the end of a full run of slots stays reachable whatever is placed
    ! later. Why no thread races another: a key's home slot is in exactly one partition, a thread
    ! writes only inside its partitions' ranges, and the spill pass starts after the team has
    ! finished. Why duplicates are still found: two copies of a key have one home, so they are in
    ! one partition, walked by one thread in the caller's order -- the second meets the first, or
    ! both are deferred and the second meets the first on the serial walk.
    !
    ! `%get_or_add_many` adds two passes: the entries a partition places take PROVISIONAL values
    ! counted from the map's watermark, and once every partition's count is known a renumbering
    ! pass (`ix_part_renumber_1`/`_n`, on the team, each thread over its own range) adds the
    ! partitions before it, so the codes are dense; the spill pass then numbers its placements
    ! after all of those. A second lookup on the team reads every new row's code back.

    !> Allocates the table for `want` keys, releasing any table there was, and zeroes it on the
    !! team. Serially, the zeroing of a table larger than the caches -- 268 MB at ten million
    !! keys -- costs more than every other pass of the partitioned insert together, and it is
    !! also the first touch of those pages, which spreads them across the team's memory nodes.
    !! For a table holding keys the serial `ix_hash_reserve` (a rehash) stays the way to grow.
    subroutine ix_hash_alloc_team(self, want, nt)
        type(pf_index_map), intent(inout) :: self !! a map whose backend is `IX_HASH`; any table released.
        integer(int64), intent(in) :: want        !! keys the table must hold under the load factor.
        integer, intent(in) :: nt                 !! the team.
        integer(int64) :: cap, lo, hi
        integer :: c, nv

        cap = ix_hash_cap_for(want)
        if (self%ncomp <= 1) then
            if (allocated(self%slots)) deallocate(self%slots)
            allocate(self%slots(cap))
            !$omp parallel do default(shared) private(c, lo, hi) schedule(static) num_threads(nt)
            do c = 1, nt
                call ix_chunk_bounds(cap, nt, c, lo, hi)
                self%slots(lo:hi)%key = 0_int64
                self%slots(lo:hi)%val = 0_int64
            end do
        else
            nv = self%ncomp + 1
            if (allocated(self%hrec)) deallocate(self%hrec)
            allocate(self%hrec(nv, cap))
            !$omp parallel do default(shared) private(c, lo, hi) schedule(static) num_threads(nt)
            do c = 1, nt
                call ix_chunk_bounds(cap, nt, c, lo, hi)
                self%hrec(:, lo:hi) = 0_int64
            end do
        end if
        self%hcap = cap
    end subroutine ix_hash_alloc_team

    !> Inserts the keys of one partition into that partition's own slot range: pass 4 of the
    !! partitioned insert, for the single-component table.
    !!
    !! `pk(k0:k1)` are the partition's keys in the caller's order, every one homing inside the
    !! range ending at `pu`. A walk that reaches `pu` without an empty slot or a match is
    !! deferred: the entry is swapped to the front of the partition's own span of the arrays
    !! (`pk(k0 : k0 + nsp - 1)` on return), which costs no memory and keeps the deferred entries
    !! in the caller's order; the entry swapped back has already been processed. In build mode a
    !! match is a duplicate and is counted, and a placed key takes its value from `pv`; in
    !! `%get_or_add_many` mode a match is an earlier copy of a new key and needs nothing, and a
    !! placed key takes the PROVISIONAL value `base + j` for the partition's j-th placement.
    subroutine ix_part_insert_1(slots, m, pu, pk, k0, k1, goam, base, nsp, ndup, nplaced, pv)
        type(ix_slot), intent(inout) :: slots(0:)        !! the table, 0-based.
        integer(int64), intent(in) :: m                  !! `hcap - 1`, the slot mask.
        integer(int64), intent(in) :: pu                 !! the partition's last slot.
        integer(int64), intent(inout) :: pk(:)           !! keys in partition order; deferred ones moved to the front.
        integer(int64), intent(in) :: k0                 !! the partition's first entry.
        integer(int64), intent(in) :: k1                 !! its last entry.
        logical, intent(in) :: goam                      !! `.true.` for `%get_or_add_many`'s numbering.
        integer(int64), intent(in) :: base               !! the watermark provisional values count from.
        integer(int64), intent(out) :: nsp               !! entries deferred; `pk(k0 : k0 + nsp - 1)` now.
        integer(int64), intent(out) :: ndup              !! duplicates met (build mode).
        integer(int64), intent(out) :: nplaced           !! entries placed.
        integer(int64), intent(inout), optional :: pv(:) !! values in partition order (build mode).
        integer(int64) :: k, key, s, j, t

        nsp = 0_int64
        ndup = 0_int64
        nplaced = 0_int64
        do k = k0, k1
            key = pk(k)
            s = iand(ix_hash_one(key), m)
            do
                if (slots(s)%val == 0_int64) then
                    nplaced = nplaced + 1_int64
                    slots(s)%key = key
                    if (goam) then
                        slots(s)%val = base + nplaced
                    else
                        slots(s)%val = pv(k)
                    end if
                    exit
                end if
                if (slots(s)%key == key) then
                    if (.not. goam) ndup = ndup + 1_int64
                    exit
                end if
                if (s == pu) then
                    nsp = nsp + 1_int64
                    j = k0 + nsp - 1_int64
                    t = pk(j)
                    pk(j) = pk(k)
                    pk(k) = t
                    if (present(pv)) then
                        t = pv(j)
                        pv(j) = pv(k)
                        pv(k) = t
                    end if
                    exit
                end if
                s = s + 1_int64
            end do
        end do
    end subroutine ix_part_insert_1

    !> `ix_part_insert_1` for the composite table: `pk(1:nc, k)` is entry `k`'s tuple.
    subroutine ix_part_insert_n(hrec, nc, m, pu, pk, k0, k1, goam, base, nsp, ndup, nplaced, pv)
        integer(int64), intent(inout) :: hrec(:, 0:)     !! the records, 0-based: the tuple, then its value.
        integer, intent(in) :: nc                        !! components.
        integer(int64), intent(in) :: m                  !! `hcap - 1`, the slot mask.
        integer(int64), intent(in) :: pu                 !! the partition's last slot.
        integer(int64), intent(inout) :: pk(:, :)        !! tuples in partition order, `(nc, *)`.
        integer(int64), intent(in) :: k0                 !! the partition's first entry.
        integer(int64), intent(in) :: k1                 !! its last entry.
        logical, intent(in) :: goam                      !! `.true.` for `%get_or_add_many`'s numbering.
        integer(int64), intent(in) :: base               !! the watermark provisional values count from.
        integer(int64), intent(out) :: nsp               !! entries deferred; `pk(:, k0 : k0 + nsp - 1)` now.
        integer(int64), intent(out) :: ndup              !! duplicates met (build mode).
        integer(int64), intent(out) :: nplaced           !! entries placed.
        integer(int64), intent(inout), optional :: pv(:) !! values in partition order (build mode).
        integer(int64) :: tb(pf_index_max_components)
        integer(int64) :: k, s, j, t
        integer :: c, nv
        logical :: hit

        nv = nc + 1
        nsp = 0_int64
        ndup = 0_int64
        nplaced = 0_int64
        do k = k0, k1
            s = iand(ix_hash_tuple(pk(1:nc, k), nc), m)
            do
                if (hrec(nv, s) == 0_int64) then
                    nplaced = nplaced + 1_int64
                    do c = 1, nc
                        hrec(c, s) = pk(c, k)
                    end do
                    if (goam) then
                        hrec(nv, s) = base + nplaced
                    else
                        hrec(nv, s) = pv(k)
                    end if
                    exit
                end if
                hit = .true.
                do c = 1, nc
                    if (hrec(c, s) /= pk(c, k)) then
                        hit = .false.
                        exit
                    end if
                end do
                if (hit) then
                    if (.not. goam) ndup = ndup + 1_int64
                    exit
                end if
                if (s == pu) then
                    nsp = nsp + 1_int64
                    j = k0 + nsp - 1_int64
                    tb(1:nc) = pk(1:nc, j)
                    pk(1:nc, j) = pk(1:nc, k)
                    pk(1:nc, k) = tb(1:nc)
                    if (present(pv)) then
                        t = pv(j)
                        pv(j) = pv(k)
                        pv(k) = t
                    end if
                    exit
                end if
                s = s + 1_int64
            end do
        end do
    end subroutine ix_part_insert_n

    !> Inserts every deferred entry of every partition, serially, walking the whole table with
    !! wraparound as an ordinary insert does: pass 5 of the partitioned insert, single-component
    !! table. In `%get_or_add_many` mode a placement takes the next value above `base`.
    subroutine ix_part_spill_1(slots, m, pk, pstart, nsp, np, goam, base, ndup, nplaced, pv)
        type(ix_slot), intent(inout) :: slots(0:)     !! the table, 0-based.
        integer(int64), intent(in) :: m               !! `hcap - 1`, the slot mask.
        integer(int64), intent(in) :: pk(:)           !! keys in partition order, deferred ones first.
        integer(int64), intent(in) :: pstart(0:)      !! the partition starts, `(0:np)`.
        integer(int64), intent(in) :: nsp(0:)         !! deferred entries per partition.
        integer, intent(in) :: np                     !! partitions.
        logical, intent(in) :: goam                   !! `.true.` for `%get_or_add_many`'s numbering.
        integer(int64), intent(in) :: base            !! the value the first placement follows.
        integer(int64), intent(out) :: ndup           !! duplicates met (build mode).
        integer(int64), intent(out) :: nplaced        !! entries placed.
        integer(int64), intent(in), optional :: pv(:) !! values in partition order (build mode).
        integer(int64) :: k, key, s, walked
        integer :: p

        ndup = 0_int64
        nplaced = 0_int64
        do p = 0, np - 1
            do k = pstart(p) + 1_int64, pstart(p) + nsp(p)
                key = pk(k)
                s = iand(ix_hash_one(key), m)
                walked = 0_int64
                do
                    if (slots(s)%val == 0_int64) then
                        nplaced = nplaced + 1_int64
                        slots(s)%key = key
                        if (goam) then
                            slots(s)%val = base + nplaced
                        else
                            slots(s)%val = pv(k)
                        end if
                        exit
                    end if
                    if (slots(s)%key == key) then
                        if (.not. goam) ndup = ndup + 1_int64
                        exit
                    end if
                    s = iand(s + 1_int64, m)
                    walked = walked + 1_int64
                    if (walked > m) call ix_hash_table_full()
                end do
            end do
        end do
    end subroutine ix_part_spill_1

    !> `ix_part_spill_1` for the composite table.
    subroutine ix_part_spill_n(hrec, nc, m, pk, pstart, nsp, np, goam, base, ndup, nplaced, pv)
        integer(int64), intent(inout) :: hrec(:, 0:)  !! the records, 0-based.
        integer, intent(in) :: nc                     !! components.
        integer(int64), intent(in) :: m               !! `hcap - 1`, the slot mask.
        integer(int64), intent(in) :: pk(:, :)        !! tuples in partition order, deferred ones first.
        integer(int64), intent(in) :: pstart(0:)      !! the partition starts, `(0:np)`.
        integer(int64), intent(in) :: nsp(0:)         !! deferred entries per partition.
        integer, intent(in) :: np                     !! partitions.
        logical, intent(in) :: goam                   !! `.true.` for `%get_or_add_many`'s numbering.
        integer(int64), intent(in) :: base            !! the value the first placement follows.
        integer(int64), intent(out) :: ndup           !! duplicates met (build mode).
        integer(int64), intent(out) :: nplaced        !! entries placed.
        integer(int64), intent(in), optional :: pv(:) !! values in partition order (build mode).
        integer(int64) :: k, s, walked
        integer :: p, c, nv
        logical :: hit

        nv = nc + 1
        ndup = 0_int64
        nplaced = 0_int64
        do p = 0, np - 1
            do k = pstart(p) + 1_int64, pstart(p) + nsp(p)
                s = iand(ix_hash_tuple(pk(1:nc, k), nc), m)
                walked = 0_int64
                do
                    if (hrec(nv, s) == 0_int64) then
                        nplaced = nplaced + 1_int64
                        do c = 1, nc
                            hrec(c, s) = pk(c, k)
                        end do
                        if (goam) then
                            hrec(nv, s) = base + nplaced
                        else
                            hrec(nv, s) = pv(k)
                        end if
                        exit
                    end if
                    hit = .true.
                    do c = 1, nc
                        if (hrec(c, s) /= pk(c, k)) then
                            hit = .false.
                            exit
                        end if
                    end do
                    if (hit) then
                        if (.not. goam) ndup = ndup + 1_int64
                        exit
                    end if
                    s = iand(s + 1_int64, m)
                    walked = walked + 1_int64
                    if (walked > m) call ix_hash_table_full()
                end do
            end do
        end do
    end subroutine ix_part_spill_n

    !> Makes one partition's provisional `%get_or_add_many` values final by adding the number
    !! of keys the partitions before it placed. Every entry in the range whose value is above
    !! the watermark was placed by this pass, by the thread that owned the range.
    subroutine ix_part_renumber_1(slots, pl, pu, watermark, add)
        type(ix_slot), intent(inout) :: slots(0:) !! the table, 0-based.
        integer(int64), intent(in) :: pl          !! the partition's first slot.
        integer(int64), intent(in) :: pu          !! its last slot.
        integer(int64), intent(in) :: watermark   !! the map's `next_auto` before the pass.
        integer(int64), intent(in) :: add         !! keys placed by the partitions before this one.
        integer(int64) :: s

        do s = pl, pu
            if (slots(s)%val > watermark) slots(s)%val = slots(s)%val + add
        end do
    end subroutine ix_part_renumber_1

    !> `ix_part_renumber_1` for the composite table.
    subroutine ix_part_renumber_n(hrec, nv, pl, pu, watermark, add)
        integer(int64), intent(inout) :: hrec(:, 0:) !! the records, 0-based.
        integer, intent(in) :: nv                    !! `ncomp + 1`, the value's row.
        integer(int64), intent(in) :: pl             !! the partition's first slot.
        integer(int64), intent(in) :: pu             !! its last slot.
        integer(int64), intent(in) :: watermark      !! the map's `next_auto` before the pass.
        integer(int64), intent(in) :: add            !! keys placed by the partitions before this one.
        integer(int64) :: s

        do s = pl, pu
            if (hrec(nv, s) > watermark) hrec(nv, s) = hrec(nv, s) + add
        end do
    end subroutine ix_part_renumber_n

    !> Passes 2 and 3 for single-component keys: the partition of every counted row from its
    !! hash, the plan, and the scatter of key and value into partition order.
    !!
    !! A row is counted when it is unmasked and, for `%get_or_add_many`, was not found by the
    !! lookup pass (`codes` present and 0 there); the value scattered is `values(i)`, or the row
    !! number when `values` is absent, or nothing when `pv` is absent.
    subroutine ix_part_scatter_1(keys, valid, hv, codes, values, n, m, lg, np, nt, pstart, pk, pv)
        integer(int64), intent(in) :: keys(:)                 !! the keys.
        logical, intent(in), optional :: valid(:)             !! the mask, or absent.
        logical, intent(in) :: hv                             !! `present(valid)`, hoisted.
        integer(int64), intent(in), optional :: codes(:)      !! the lookup pass's answers; a row with 0 is counted.
        integer(int64), intent(in), optional :: values(:)     !! the values, or absent for the row numbers.
        integer(int64), intent(in) :: n                       !! rows.
        integer(int64), intent(in) :: m                       !! `hcap - 1`, the slot mask.
        integer, intent(in) :: lg                             !! `ix_part_shift`'s answer.
        integer, intent(in) :: np                             !! partitions.
        integer, intent(in) :: nt                             !! the team.
        integer(int64), allocatable, intent(out) :: pstart(:) !! receives the partition starts, `(0:np)`.
        integer(int64), allocatable, intent(out) :: pk(:)     !! receives the keys in partition order.
        integer(int64), allocatable, intent(out), optional :: pv(:) !! receives their values.
        integer(int32), allocatable :: pid(:)
        integer(int64), allocatable :: cursor(:,:)
        integer(int64) :: i, lo, hi, k
        integer :: c, p
        logical :: hc, has_v

        hc = present(codes)
        has_v = present(values)
        allocate(pid(n))
        !$omp parallel do default(shared) private(c, lo, hi, i) schedule(static) num_threads(nt)
        do c = 1, nt
            call ix_chunk_bounds(n, nt, c, lo, hi)
            do i = lo, hi
                pid(i) = -1_int32
                if (hc) then
                    if (codes(i) /= 0_int64) cycle
                end if
                if (hv) then
                    if (.not. valid(i)) cycle
                end if
                pid(i) = int(shiftr(iand(ix_hash_one(keys(i)), m), lg), int32)
            end do
        end do
        allocate(pstart(0:np), cursor(0:np - 1, nt))
        call ix_partition_plan(pid, n, np, nt, pstart, cursor)
        allocate(pk(pstart(np)))
        if (present(pv)) allocate(pv(pstart(np)))
        !$omp parallel do default(shared) private(c, lo, hi, i, p, k) schedule(static) num_threads(nt)
        do c = 1, nt
            call ix_chunk_bounds(n, nt, c, lo, hi)
            do i = lo, hi
                if (pid(i) < 0_int32) cycle
                p = int(pid(i))
                k = cursor(p, c) + 1_int64
                cursor(p, c) = k
                pk(k) = keys(i)
                if (present(pv)) then
                    if (has_v) then
                        pv(k) = values(i)
                    else
                        pv(k) = i
                    end if
                end if
            end do
        end do
    end subroutine ix_part_scatter_1

    !> `ix_part_scatter_1` for key tuples: `keys(i, 1:nc)` becomes `pk(1:nc, k)`.
    subroutine ix_part_scatter_n(keys, nc, valid, hv, codes, values, n, m, lg, np, nt, pstart, pk, pv)
        integer(int64), intent(in) :: keys(:, :)              !! the tuples, `(n, ncomp)`.
        integer, intent(in) :: nc                             !! components.
        logical, intent(in), optional :: valid(:)             !! the mask, or absent.
        logical, intent(in) :: hv                             !! `present(valid)`, hoisted.
        integer(int64), intent(in), optional :: codes(:)      !! the lookup pass's answers; a row with 0 is counted.
        integer(int64), intent(in), optional :: values(:)     !! the values, or absent for the row numbers.
        integer(int64), intent(in) :: n                       !! rows.
        integer(int64), intent(in) :: m                       !! `hcap - 1`, the slot mask.
        integer, intent(in) :: lg                             !! `ix_part_shift`'s answer.
        integer, intent(in) :: np                             !! partitions.
        integer, intent(in) :: nt                             !! the team.
        integer(int64), allocatable, intent(out) :: pstart(:) !! receives the partition starts, `(0:np)`.
        integer(int64), allocatable, intent(out) :: pk(:, :)  !! receives the tuples in partition order, `(nc, *)`.
        integer(int64), allocatable, intent(out), optional :: pv(:) !! receives their values.
        integer(int32), allocatable :: pid(:)
        integer(int64), allocatable :: cursor(:,:)
        integer(int64) :: kb(pf_index_max_components)
        integer(int64) :: i, lo, hi, k
        integer :: c, p, j
        logical :: hc, has_v

        hc = present(codes)
        has_v = present(values)
        allocate(pid(n))
        !$omp parallel do default(shared) private(c, lo, hi, i, j, kb) schedule(static) num_threads(nt)
        do c = 1, nt
            call ix_chunk_bounds(n, nt, c, lo, hi)
            do i = lo, hi
                pid(i) = -1_int32
                if (hc) then
                    if (codes(i) /= 0_int64) cycle
                end if
                if (hv) then
                    if (.not. valid(i)) cycle
                end if
                do j = 1, nc
                    kb(j) = keys(i, j)
                end do
                pid(i) = int(shiftr(iand(ix_hash_tuple(kb, nc), m), lg), int32)
            end do
        end do
        allocate(pstart(0:np), cursor(0:np - 1, nt))
        call ix_partition_plan(pid, n, np, nt, pstart, cursor)
        allocate(pk(nc, pstart(np)))
        if (present(pv)) allocate(pv(pstart(np)))
        !$omp parallel do default(shared) private(c, lo, hi, i, p, k, j) schedule(static) num_threads(nt)
        do c = 1, nt
            call ix_chunk_bounds(n, nt, c, lo, hi)
            do i = lo, hi
                if (pid(i) < 0_int32) cycle
                p = int(pid(i))
                k = cursor(p, c) + 1_int64
                cursor(p, c) = k
                do j = 1, nc
                    pk(j, k) = keys(i, j)
                end do
                if (present(pv)) then
                    if (has_v) then
                        pv(k) = values(i)
                    else
                        pv(k) = i
                    end if
                end if
            end do
        end do
    end subroutine ix_part_scatter_n

    module procedure ix_hash_build_part_1
        integer(int64), allocatable :: pk(:), pv(:), pstart(:), nsp(:)
        integer(int64) :: m, pl, pu, pdup, pplaced, tdup, tplaced, sdup, splaced, tsp
        integer :: lg, np, p
        logical :: hv

        hv = present(valid)
        call ix_hash_alloc_team(self, nv, nt)
        m = self%hcap - 1_int64
        lg = ix_part_shift(self%hcap, nt)
        np = int(shiftr(self%hcap, lg))
        call ix_part_scatter_1(keys, valid, hv, values=values, n=size(keys, kind=int64), m=m, lg=lg, np=np, &
            nt=nt, pstart=pstart, pk=pk, pv=pv)
        allocate(nsp(0:np - 1))
        tdup = 0_int64
        tplaced = 0_int64
        !$omp parallel do default(shared) private(p, pl, pu, pdup, pplaced) reduction(+:tdup, tplaced) &
        !$omp     schedule(dynamic) num_threads(nt)
        do p = 0, np - 1
            pl = shiftl(int(p, int64), lg)
            pu = pl + shiftl(1_int64, lg) - 1_int64
            call ix_part_insert_1(self%slots, m, pu, pk, pstart(p) + 1_int64, pstart(p + 1), &
                .false., 0_int64, nsp(p), pdup, pplaced, pv)
            tdup = tdup + pdup
            tplaced = tplaced + pplaced
        end do
        dup = tdup > 0_int64
        if (dup) return
        call ix_part_spill_1(self%slots, m, pk, pstart, nsp, np, .false., 0_int64, sdup, splaced, pv)
        tsp = sum(nsp)
        !$omp atomic write
        dbg_index_spills = tsp
        dup = sdup > 0_int64
        if (dup) return
        self%nk = tplaced + splaced
    end procedure ix_hash_build_part_1

    module procedure ix_hash_build_part_n
        integer(int64), allocatable :: pk(:,:), pv(:), pstart(:), nsp(:)
        integer(int64) :: m, pl, pu, pdup, pplaced, tdup, tplaced, sdup, splaced, tsp
        integer :: lg, np, p, nc
        logical :: hv

        nc = self%ncomp
        hv = present(valid)
        call ix_hash_alloc_team(self, nv, nt)
        m = self%hcap - 1_int64
        lg = ix_part_shift(self%hcap, nt)
        np = int(shiftr(self%hcap, lg))
        call ix_part_scatter_n(keys, nc, valid, hv, values=values, n=size(keys, 1, kind=int64), m=m, lg=lg, &
            np=np, nt=nt, pstart=pstart, pk=pk, pv=pv)
        allocate(nsp(0:np - 1))
        tdup = 0_int64
        tplaced = 0_int64
        !$omp parallel do default(shared) private(p, pl, pu, pdup, pplaced) reduction(+:tdup, tplaced) &
        !$omp     schedule(dynamic) num_threads(nt)
        do p = 0, np - 1
            pl = shiftl(int(p, int64), lg)
            pu = pl + shiftl(1_int64, lg) - 1_int64
            call ix_part_insert_n(self%hrec, nc, m, pu, pk, pstart(p) + 1_int64, pstart(p + 1), &
                .false., 0_int64, nsp(p), pdup, pplaced, pv)
            tdup = tdup + pdup
            tplaced = tplaced + pplaced
        end do
        dup = tdup > 0_int64
        if (dup) return
        call ix_part_spill_n(self%hrec, nc, m, pk, pstart, nsp, np, .false., 0_int64, sdup, splaced, pv)
        tsp = sum(nsp)
        !$omp atomic write
        dbg_index_spills = tsp
        dup = sdup > 0_int64
        if (dup) return
        self%nk = tplaced + splaced
    end procedure ix_hash_build_part_n

    !> Room for `nmiss` new keys ahead of a threaded `%get_or_add_many`: a fresh table on the
    !! team when the map is empty and the one it has is too small, the serial rehash otherwise.
    subroutine ix_hash_room_team(self, nmiss, nt)
        type(pf_index_map), intent(inout) :: self !! a map whose backend is `IX_HASH`.
        integer(int64), intent(in) :: nmiss       !! keys about to be added.
        integer, intent(in) :: nt                 !! the team.

        if (self%nk == 0_int64 .and. ix_hash_cap_for(nmiss) > self%hcap) then
            call ix_hash_alloc_team(self, nmiss, nt)
        else
            call ix_hash_reserve(self, self%nk + nmiss)
        end if
    end subroutine ix_hash_room_team

    module procedure ix_hash_goam_part_1
        integer(int64), allocatable :: pk(:), pstart(:), nsp(:), placed(:), base(:)
        integer(int64) :: n, m, i, lo, hi, pl, pu, nmiss, pdup, watermark, total, tsp, sdup, splaced, idx
        integer :: lg, np, c, p
        logical :: hv, is_new

        n = size(keys, kind=int64)
        hv = present(valid)
        !$omp parallel do default(shared) private(c, lo, hi) schedule(static) num_threads(nt)
        do c = 1, nt
            call ix_chunk_bounds(n, nt, c, lo, hi)
            call ix_many_1_k64_i64(self, keys, codes, valid, hv, lo, hi)
        end do
        nmiss = 0_int64
        !$omp parallel do default(shared) private(i) reduction(+:nmiss) schedule(static) num_threads(nt)
        do i = 1_int64, n
            if (codes(i) /= 0_int64) cycle
            if (hv) then
                if (.not. valid(i)) cycle
            end if
            nmiss = nmiss + 1_int64
        end do
        if (nmiss == 0_int64) then
            !$omp atomic write
            dbg_index_spills = -1_int64
            return
        end if
        call ix_hash_room_team(self, nmiss, nt)
        m = self%hcap - 1_int64
        if (nmiss < IX_MIN_KEYS_PER_THREAD) then
            ! Too few new keys to be worth the pass: a direct probe and insert per row, in the
            ! caller's order. Not the `%get_or_add` worker, whose shape checks are the public
            ! entry's business and would refuse the string map the tuple pass serves.
            !$omp atomic write
            dbg_index_spills = -1_int64
            do i = 1_int64, n
                if (codes(i) /= 0_int64) cycle
                if (hv) then
                    if (.not. valid(i)) cycle
                end if
                idx = ix_probe_1(self%slots, m, keys(i))
                if (idx == 0_int64) then
                    idx = self%next_auto + 1_int64
                    call ix_hash_insert_scalar(self, keys(i), idx, is_new)
                    self%next_auto = idx
                    m = self%hcap - 1_int64
                end if
                codes(i) = idx
            end do
            return
        end if
        lg = ix_part_shift(self%hcap, nt)
        np = int(shiftr(self%hcap, lg))
        call ix_part_scatter_1(keys, valid, hv, codes=codes, n=n, m=m, lg=lg, np=np, nt=nt, pstart=pstart, pk=pk)
        allocate(nsp(0:np - 1), placed(0:np - 1), base(0:np))
        watermark = self%next_auto
        !$omp parallel do default(shared) private(p, pl, pu, pdup) schedule(dynamic) num_threads(nt)
        do p = 0, np - 1
            pl = shiftl(int(p, int64), lg)
            pu = pl + shiftl(1_int64, lg) - 1_int64
            call ix_part_insert_1(self%slots, m, pu, pk, pstart(p) + 1_int64, pstart(p + 1), &
                .true., watermark, nsp(p), pdup, placed(p))
        end do
        base(0) = 0_int64
        do p = 0, np - 1
            base(p + 1) = base(p) + placed(p)
        end do
        !$omp parallel do default(shared) private(p, pl, pu) schedule(dynamic) num_threads(nt)
        do p = 0, np - 1
            if (placed(p) == 0_int64 .or. base(p) == 0_int64) cycle
            pl = shiftl(int(p, int64), lg)
            pu = pl + shiftl(1_int64, lg) - 1_int64
            call ix_part_renumber_1(self%slots, pl, pu, watermark, base(p))
        end do
        total = base(np)
        call ix_part_spill_1(self%slots, m, pk, pstart, nsp, np, .true., watermark + total, sdup, splaced)
        total = total + splaced
        self%nk = self%nk + total
        self%next_auto = watermark + total
        tsp = sum(nsp)
        !$omp atomic write
        dbg_index_spills = tsp
        !$omp parallel do default(shared) private(i) schedule(static) num_threads(nt)
        do i = 1_int64, n
            if (codes(i) /= 0_int64) cycle
            if (hv) then
                if (.not. valid(i)) cycle
            end if
            codes(i) = ix_probe_1(self%slots, m, keys(i))
        end do
    end procedure ix_hash_goam_part_1

    module procedure ix_hash_goam_part_n
        integer(int64), allocatable :: pk(:,:), pstart(:), nsp(:), placed(:), base(:)
        integer(int64) :: kb(pf_index_max_components)
        integer(int64) :: n, m, i, lo, hi, pl, pu, nmiss, pdup, watermark, total, tsp, sdup, splaced, idx
        integer :: lg, np, c, p, nc, j
        logical :: hv, is_new

        n = size(keys, 1, kind=int64)
        nc = self%ncomp
        hv = present(valid)
        !$omp parallel do default(shared) private(c, lo, hi) schedule(static) num_threads(nt)
        do c = 1, nt
            call ix_chunk_bounds(n, nt, c, lo, hi)
            call ix_many_n_k64_i64(self, keys, codes, valid, hv, lo, hi)
        end do
        nmiss = 0_int64
        !$omp parallel do default(shared) private(i) reduction(+:nmiss) schedule(static) num_threads(nt)
        do i = 1_int64, n
            if (codes(i) /= 0_int64) cycle
            if (hv) then
                if (.not. valid(i)) cycle
            end if
            nmiss = nmiss + 1_int64
        end do
        if (nmiss == 0_int64) then
            !$omp atomic write
            dbg_index_spills = -1_int64
            return
        end if
        call ix_hash_room_team(self, nmiss, nt)
        m = self%hcap - 1_int64
        if (nmiss < IX_MIN_KEYS_PER_THREAD) then
            ! As in `ix_hash_goam_part_1`: a direct probe and insert per row, so that the string
            ! map's `(hash, 0)` tuples take this path as any tuple does.
            !$omp atomic write
            dbg_index_spills = -1_int64
            do i = 1_int64, n
                if (codes(i) /= 0_int64) cycle
                if (hv) then
                    if (.not. valid(i)) cycle
                end if
                do j = 1, nc
                    kb(j) = keys(i, j)
                end do
                idx = ix_probe_n(self%hrec, nc, m, kb)
                if (idx == 0_int64) then
                    idx = self%next_auto + 1_int64
                    call ix_hash_insert(self, kb(1:nc), idx, is_new)
                    self%next_auto = idx
                    m = self%hcap - 1_int64
                end if
                codes(i) = idx
            end do
            return
        end if
        lg = ix_part_shift(self%hcap, nt)
        np = int(shiftr(self%hcap, lg))
        call ix_part_scatter_n(keys, nc, valid, hv, codes=codes, n=n, m=m, lg=lg, np=np, nt=nt, pstart=pstart, pk=pk)
        allocate(nsp(0:np - 1), placed(0:np - 1), base(0:np))
        watermark = self%next_auto
        !$omp parallel do default(shared) private(p, pl, pu, pdup) schedule(dynamic) num_threads(nt)
        do p = 0, np - 1
            pl = shiftl(int(p, int64), lg)
            pu = pl + shiftl(1_int64, lg) - 1_int64
            call ix_part_insert_n(self%hrec, nc, m, pu, pk, pstart(p) + 1_int64, pstart(p + 1), &
                .true., watermark, nsp(p), pdup, placed(p))
        end do
        base(0) = 0_int64
        do p = 0, np - 1
            base(p + 1) = base(p) + placed(p)
        end do
        !$omp parallel do default(shared) private(p, pl, pu) schedule(dynamic) num_threads(nt)
        do p = 0, np - 1
            if (placed(p) == 0_int64 .or. base(p) == 0_int64) cycle
            pl = shiftl(int(p, int64), lg)
            pu = pl + shiftl(1_int64, lg) - 1_int64
            call ix_part_renumber_n(self%hrec, nc + 1, pl, pu, watermark, base(p))
        end do
        total = base(np)
        call ix_part_spill_n(self%hrec, nc, m, pk, pstart, nsp, np, .true., watermark + total, sdup, splaced)
        total = total + splaced
        self%nk = self%nk + total
        self%next_auto = watermark + total
        tsp = sum(nsp)
        !$omp atomic write
        dbg_index_spills = tsp
        !$omp parallel do default(shared) private(i, j, kb) schedule(static) num_threads(nt)
        do i = 1_int64, n
            if (codes(i) /= 0_int64) cycle
            if (hv) then
                if (.not. valid(i)) cycle
            end if
            do j = 1, nc
                kb(j) = keys(i, j)
            end do
            codes(i) = ix_probe_n(self%hrec, nc, m, kb)
        end do
    end procedure ix_hash_goam_part_n

    ! ---- The two test-only hooks of the partitioned insert ----

    module procedure parquet_debug_index_partition
        integer :: nt, lg

        cap = ix_hash_cap_for(n)
        home = iand(ix_hash_one(key), cap - 1_int64)
        nt = ix_threads_rule(n, threads, "build")
        part = 0_int64
        if (nt > 1) then
            lg = ix_part_shift(cap, nt)
            part = shiftl(1_int64, lg)
        end if
    end procedure parquet_debug_index_partition

    module procedure parquet_debug_index_spills
        !$omp atomic read
        n = dbg_index_spills
    end procedure parquet_debug_index_spills

end submodule parquet_index_hash
