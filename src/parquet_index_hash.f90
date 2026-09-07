!> The hash backend: the mixer, the position-sensitive tuple combine, probing, growth and
!> backward-shift deletion.
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
!! **Slot numbers are 0-based inside this file** and converted at the point of array access
!! (`slots(s + 1)`). That is what makes the cyclic arithmetic ordinary: `iand(h, m)` lands in
!! range, `iand(s + 1, m)` advances with wraparound, and the deletion test below can compare slot
!! numbers directly instead of correcting for the base at every step.
!!
!! **The mixer is overflow-free by construction, and that is a deliberate design constraint rather
!! than an accident of the constants.** A conventional 64-bit multiplicative mixer wraps a signed
!! multiply, which this project treats as the hazard it is: a compiler may wrap the arithmetic and
!! *still* use the overflow's undefinedness to delete a branch somewhere else -- the confirmed ifx
!! incident behind `feature_risks.md` Risk-94, which cost `parquet_random` a three-arm
!! preprocessor fork and two standalone check scripts to own exactly one such site. This module
!! avoids the entire class instead: the key is split into 32-bit halves and mixed with 32-bit x
!! 31-bit multiplies, so every product is provably below 2**63 and no wrapping site exists at all.
!! There is therefore nothing here to fork, nothing to check with a separate script, and the
!! module is clean under `-ftrapv`, nagfor's `-C=intovf` and UBSan on every compiler in the fleet.
!! A lookup is memory-bound; the few extra ALU operations disappear next to one cache miss.
submodule (parquet_index) parquet_index_hash
    implicit none

    !> First multiplicative constant of the 32-bit mixing step: murmur3's `0x85ebca6b` with its
    !! top bit cleared, so it is below 2**31 and every product stays below 2**63.
    !!
    !! **Odd, which is what matters about it.** Multiplication by an odd constant modulo 2**32 is a
    !! bijection, so the step loses no information; the exact value only has to avalanche well, and
    !! the clustering tests in `test/test_index.f90` are what hold it to that.
    integer(int64), parameter :: IX_C1 = 99244651_int64
    !> Second multiplicative constant: murmur3's `0xc2b2ae35` with its top bit cleared. Odd, for
    !! the reason given on `IX_C1`.
    integer(int64), parameter :: IX_C2 = 1119548469_int64
    !> Seed the key and tuple hashes start from, so that a hash of zero is not zero. The golden
    !! ratio constant `0x9E3779B9`; only ever XOR-ed, never multiplied, so its width is free.
    integer(int64), parameter :: IX_SEED = 2654435769_int64
    !> Seed the key and tuple hashes start their chain from, distinct from `IX_SEED` so that the
    !! per-half mixing and the per-component chaining are not the same transformation applied
    !! twice. `0x9E3779B97F4A7C15`'s low 62 bits; XOR-ed only, never multiplied.
    integer(int64), parameter :: IX_SEED_T = 2135587861_int64
    !> Seed the STRING hash starts its chain from, distinct from both above so that a string's
    !! word chain and a tuple's component chain are not one transformation. `0x61C88647`, the
    !! negated golden-ratio constant; XOR-ed only, never multiplied.
    integer(int64), parameter :: IX_SEED_S = 1640531527_int64

contains

    ! ---- The mixer ----

    !> Avalanches the low 32 bits of `v` into a value in `0 .. 2**32 - 1`.
    !!
    !! Murmur3's 32-bit finalizer shape: shift-xor, multiply, shift-xor, multiply, shift-xor. Each
    !! multiplicand is masked to 32 bits and each constant is below 2**31, so **every product is
    !! below 2**63** and this function cannot overflow for any input. Read the masks as part of the
    !! algorithm rather than as tidying: removing one reintroduces exactly the undefined-behaviour
    !! class the file header is about.
    pure function ix_mix32(v) result(r)
        integer(int64), intent(in) :: v !! any value; only its low 32 bits are read.
        integer(int64) :: r             !! mixed value in `0 .. 2**32 - 1`.

        r = iand(v, IX_MASK32)
        r = ieor(r, ishft(r, -16))
        r = iand(r * IX_C1, IX_MASK32)
        r = ieor(r, ishft(r, -13))
        r = iand(r * IX_C2, IX_MASK32)
        r = ieor(r, ishft(r, -16))
    end function ix_mix32

    !> Hashes one 64-bit key into a full-width bit pattern.
    !!
    !! Both 32-bit halves are mixed and then cross-fed, so the LOW half of the result -- which is
    !! all the slot index reads -- depends on every bit of the key. That is what stops a key set
    !! that varies only in its high word (a stride of 2**32, say) from collapsing onto one slot:
    !! the final `ix_mix32(ieor(lo, hi))` is what carries the high word down.
    !!
    !! `ishft` is a bit-model intrinsic, not arithmetic, so shifting a value into the sign bit is
    !! defined and raises nothing. A negative result is an ordinary bit pattern here and `iand`
    !! with the capacity mask still yields a slot in range.
    pure function ix_hash_bits(k) result(h)
        integer(int64), intent(in) :: k !! the key to hash.
        integer(int64) :: h             !! a mixed bit pattern; only its low bits are used.
        integer(int64) :: lo, hi

        lo = iand(k, IX_MASK32)
        hi = iand(ishft(k, -32), IX_MASK32)
        lo = ix_mix32(ieor(lo, IX_SEED))
        hi = ix_mix32(ieor(hi, lo))
        lo = ix_mix32(ieor(lo, hi))
        h = ior(ishft(hi, 32), lo)
    end function ix_hash_bits

    !> The hash of a single-component key.
    !!
    !! **Identical to `ix_hash_chain` over a one-element tuple, and that is a contract**: a
    !! single-component map may be looked up with a scalar key or with a 1-tuple, and both spellings
    !! have to find the same slot. Keeping this a one-line wrapper over the same seeded step is
    !! what makes that true by construction rather than by two constants agreeing.
    pure function ix_hash_one(k) result(h)
        integer(int64), intent(in) :: k !! the key.
        integer(int64) :: h             !! its hash.

        h = ix_hash_bits(ieor(IX_SEED_T, k))
    end function ix_hash_one

    !> The hash of a key tuple: a chain, seeded, one step per component.
    !!
    !! **Position-sensitive, which the feature requires.** Each step's input is the accumulated
    !! hash, so `[1, 2]` and `[2, 1]` take different paths and land in different slots. A symmetric
    !! combine -- XOR-ing or summing per-component hashes -- would collide them, and
    !! `test_composite_order_matters` (`test/test_index.f90`) is what pins the asymmetry.
    pure function ix_hash_chain(key) result(h)
        integer(int64), intent(in) :: key(:) !! the tuple.
        integer(int64) :: h                  !! its hash.
        integer :: j

        h = IX_SEED_T
        do j = 1, size(key)
            h = ix_hash_bits(ieor(h, key(j)))
        end do
    end function ix_hash_chain

    ! The string hash: the same chain as a tuple's, over the string's 8-byte words, with the
    ! length mixed in as the first component so that a string and its own zero-padded extension
    ! never share a hash. Each word is assembled little-endian from the bytes, read as unsigned,
    ! so the result is the same on every platform. The chain is what makes the string hash
    ! position-sensitive for free; the clustering test over near-identical strings holds it to
    ! the integer mixer's standard.
    module procedure ix_hash_str
        integer(int64) :: w, i, nb
        integer :: k

        h = ix_hash_bits(ieor(IX_SEED_S, n))
        do i = 0_int64, n - 1_int64, 8_int64
            nb = min(8_int64, n - i)
            w = 0_int64
            do k = 1, int(nb)
                w = ior(w, ishft(int(iand(iachar(b(i + k)), 255), int64), 8 * (k - 1)))
            end do
            h = ix_hash_bits(ieor(h, w))
        end do
        ! The hook narrows the RESULT rather than the mixer's input, so the narrowed hashes are
        ! as evenly spread over their few bits as the full ones are over 64 -- which is what
        ! makes a test at 2 bits a test of the collision chain and not of a degenerate mixer.
        if (dbg_index_string_hash_bits > 0) then
            h = iand(h, ishft(1_int64, dbg_index_string_hash_bits) - 1_int64)
        end if
    end procedure ix_hash_str

    module procedure parquet_debug_set_index_string_hash_bits
        if (nbits < 1) then
            dbg_index_string_hash_bits = 0
        else
            dbg_index_string_hash_bits = min(nbits, 62)
        end if
    end procedure parquet_debug_set_index_string_hash_bits

    ! ---- Lookup ----

    module procedure ix_hash_find_scalar
        integer(int64) :: s, m

        v = 0_int64
        if (self%hcap <= 0_int64) return
        m = self%hcap - 1_int64
        s = iand(ix_hash_one(key), m)
        do
            if (self%slots(s + 1_int64)%val == 0_int64) return
            if (self%slots(s + 1_int64)%key == key) then
                v = self%slots(s + 1_int64)%val
                return
            end if
            s = iand(s + 1_int64, m)
        end do
    end procedure ix_hash_find_scalar

    module procedure ix_hash_find_tuple
        integer(int64) :: s, m
        integer :: j
        logical :: hit

        v = 0_int64
        if (self%hcap <= 0_int64) return
        ! A single-component map keeps its table in `slots`, whichever spelling the caller used to
        ! reach it, so a 1-tuple lookup is the scalar lookup.
        if (self%ncomp <= 1) then
            v = ix_hash_find_scalar(self, key(1))
            return
        end if
        m = self%hcap - 1_int64
        s = iand(ix_hash_chain(key), m)
        do
            if (self%hvals(s + 1_int64) == 0_int64) return
            hit = .true.
            do j = 1, self%ncomp
                if (self%hkeys(j, s + 1_int64) /= key(j)) then
                    hit = .false.
                    exit
                end if
            end do
            if (hit) then
                v = self%hvals(s + 1_int64)
                return
            end if
            s = iand(s + 1_int64, m)
        end do
    end procedure ix_hash_find_tuple

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

        error stop "pf_index_map: the hash table is full, which cannot happen while the load " // &
            "factor holds -- the key count and the table's real occupancy have diverged"
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
        integer :: j
        logical :: hit

        if (self%ncomp <= 1) then
            call ix_hash_insert_scalar(self, key(1), value, is_new)
            return
        end if
        call ix_hash_reserve(self, self%nk + 1_int64)
        m = self%hcap - 1_int64
        s = iand(ix_hash_chain(key), m)
        walked = 0_int64
        do
            if (self%hvals(s + 1_int64) == 0_int64) exit
            hit = .true.
            do j = 1, self%ncomp
                if (self%hkeys(j, s + 1_int64) /= key(j)) then
                    hit = .false.
                    exit
                end if
            end do
            if (hit) then
                self%hvals(s + 1_int64) = value
                is_new = .false.
                return
            end if
            s = iand(s + 1_int64, m)
            walked = walked + 1_int64
            if (walked > self%hcap) call ix_hash_table_full()
        end do
        do j = 1, self%ncomp
            self%hkeys(j, s + 1_int64) = key(j)
        end do
        self%hvals(s + 1_int64) = value
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
        integer :: c
        logical :: hit

        found = .false.
        if (self%ncomp <= 1) then
            call ix_hash_remove_scalar(self, key(1), found)
            return
        end if
        if (self%hcap <= 0_int64) return
        m = self%hcap - 1_int64
        s = iand(ix_hash_chain(key), m)
        do
            if (self%hvals(s + 1_int64) == 0_int64) return
            hit = .true.
            do c = 1, self%ncomp
                if (self%hkeys(c, s + 1_int64) /= key(c)) then
                    hit = .false.
                    exit
                end if
            end do
            if (hit) exit
            s = iand(s + 1_int64, m)
        end do
        found = .true.
        i = s
        self%hvals(i + 1_int64) = 0_int64
        j = i
        do
            j = iand(j + 1_int64, m)
            if (self%hvals(j + 1_int64) == 0_int64) exit
            k = iand(ix_hash_chain(self%hkeys(:, j + 1_int64)), m)
            if (.not. ix_cyclic_in(i, k, j)) then
                do c = 1, self%ncomp
                    self%hkeys(c, i + 1_int64) = self%hkeys(c, j + 1_int64)
                end do
                self%hvals(i + 1_int64) = self%hvals(j + 1_int64)
                self%hvals(j + 1_int64) = 0_int64
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
    !! multiplication below can be read as safe without tracing the caller.
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
        integer(int64), allocatable :: fk(:,:), fv(:)
        integer(int64) :: s, m, t, old
        integer :: c

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
            allocate(fk(self%ncomp, newcap))
            allocate(fv(newcap))
            fk = 0_int64
            fv = 0_int64
            do t = 1_int64, old
                if (self%hvals(t) == 0_int64) cycle
                s = iand(ix_hash_chain(self%hkeys(:, t)), m)
                do while (fv(s + 1_int64) /= 0_int64)
                    s = iand(s + 1_int64, m)
                end do
                do c = 1, self%ncomp
                    fk(c, s + 1_int64) = self%hkeys(c, t)
                end do
                fv(s + 1_int64) = self%hvals(t)
            end do
            call move_alloc(fk, self%hkeys)
            call move_alloc(fv, self%hvals)
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

        max_probe = 0_int64
        mean_probe = 0.0_real64
        if (self%hcap <= 0_int64 .or. self%nk == 0_int64) return
        m = self%hcap - 1_int64
        total = 0_int64
        do t = 1_int64, self%hcap
            if (self%ncomp <= 1) then
                if (self%slots(t)%val == 0_int64) cycle
                s = iand(ix_hash_one(self%slots(t)%key), m)
            else
                if (self%hvals(t) == 0_int64) cycle
                s = iand(ix_hash_chain(self%hkeys(:, t)), m)
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
        integer :: c

        k = 0_int64
        do t = 1_int64, self%hcap
            if (self%ncomp <= 1) then
                if (self%slots(t)%val == 0_int64) cycle
                k = k + 1_int64
                out(k, 1) = self%slots(t)%key
            else
                if (self%hvals(t) == 0_int64) cycle
                k = k + 1_int64
                do c = 1, self%ncomp
                    out(k, c) = self%hkeys(c, t)
                end do
            end if
        end do
    end procedure ix_hash_collect

end submodule parquet_index_hash
