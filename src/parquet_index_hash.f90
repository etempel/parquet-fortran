!> The hash backend's mutation side: insertion, growth, backward-shift deletion, and the two
!> diagnostics that walk the whole table.
!!
!! **The mixer and the lookup probe are NOT here.** `ix_mix32`, `ix_hash_bits`, `ix_hash_one`,
!! `ix_hash_tuple`, `ix_hash_str` and the two probes (`ix_probe_1`, `ix_probe_n`) live in
!! `parquet_index_map.f90`, in the same translation unit as `%get` and the chunk loops of
!! `%get_many`, so that the compiler inlines them into the lookup loops: a probe that has to call
!! across a submodule boundary pays about three nanoseconds per key, a third of a small-map probe
!! (feature_pf_index.md, section 4.6). This file is a DESCENDANT of that submodule and reaches the
!! same functions by host association, so the two sides hash identically by construction -- there
!! is one mixer, not a copy of it on each side.
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
!! separate arrays (feature_pf_index.md, section 4.3). A 1-tuple never reaches the composite
!! layout: every entry below routes `ncomp <= 1` to the scalar table, which is what makes a scalar
!! key and a 1-tuple the same key without the two hashes having to agree.
!!
!! **Slot numbers are 0-based inside this file** and converted at the point of array access
!! (`slots(s + 1)`, `hrec(:, s + 1)`). That is what makes the cyclic arithmetic ordinary: `iand(h,
!! m)` lands in range, `iand(s + 1, m)` advances with wraparound, and the deletion test below can
!! compare slot numbers directly instead of correcting for the base at every step.
!!
!! **Every abort goes through `ix_abort`**, the module's serialised reporter, because a build runs
!! OUTSIDE `pf_index_map_guard` now (`ix_adopt` in `parquet_index_map.f90`) and two builds may
!! therefore fail at the same moment; see the reporter's doc-comment for the property it keeps.
submodule (parquet_index:parquet_index_map) parquet_index_hash
    implicit none

contains

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
    subroutine ix_hash_table_full()

        call ix_abort("pf_index_map: the hash table is full, which cannot happen while the " // &
            "load factor holds -- the key count and the table's real occupancy have diverged")
    end subroutine ix_hash_table_full

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
            error stop "pf_index_map: key count is too large for a hash table"
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

end submodule parquet_index_hash
