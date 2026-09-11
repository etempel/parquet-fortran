!> `pf_index_pool`: hands out and recycles unique index values, and its OpenMP guard.
!!
!! Three pieces of state, and every operation is O(1) in terms of them:
!!
!! * a **bitmap**, one bit per index, set while that index is held. This is what lets
!!   `%free_index` reject a double free and `%is_used` answer at all, and it is what `%compact`
!!   walks to rebuild the free list.
!! * a **free list**, a stack of released indexes. Push on free, pop on get -- so ordinary
!!   operation hands back the most recently freed index, which is the cheapest thing to do and
!!   keeps the recently-used indexes hot in cache.
!! * two **counters**, `max_used` and `n_used`, which is what makes `%get_free_index_count` O(1)
!!   rather than a bitmap scan.
!!
!! **Every public entry takes one process-wide named critical**, `pf_index_pool_guard`, including
!! the queries -- they read counters a concurrent mutation is writing. The shape is always the
!! same: the public procedure's body is the guard and a single call to an unguarded worker, and no
!! worker ever calls a public entry. Two rules make that mandatory rather than stylistic. A named
!! critical is not recursive, so a public entry reached from inside another would deadlock rather
!! than fail to build. And branching out of a `critical` region is not conforming, so a worker
!! that wants to `return` early cannot be inlined into the guarded body -- which is exactly why
!! the workers are separate procedures.
!!
!! A consequence worth having: because at most one thread is ever inside the region, at most one
!! thread can reach an abort from this type. Several threads terminating at once leaves the
!! process exit status undefined under ifx (CLAUDE.md's ifx gotchas), and this shape rules it out
!! for the pool on its own. The module's serialised reporter `ix_abort` (declared in the spec)
!! holds the same property for every index type -- the map's builds included, which run outside
!! the map's guard -- so the pool's aborts go through it too: one rule across the module rather
!! than two, and the one `tools/check_source_conventions.py` can hold.
!!
!! Every worker takes a `type(pf_index_pool)` dummy, never `class`: a `class` actual passed to a
!! `type` dummy is free, while the reverse builds a runtime class descriptor in the caller's
!! prologue on every call.
submodule (parquet_index) parquet_index_pool
    implicit none

contains

    ! ---- Guarded public entries ----

    module procedure pool_get_index
        !$omp critical (pf_index_pool_guard)
        idx = pool_take(self)
        !$omp end critical (pf_index_pool_guard)
    end procedure pool_get_index

    module procedure pool_free_i32
        !$omp critical (pf_index_pool_guard)
        call pool_release(self, int(idx, int64))
        !$omp end critical (pf_index_pool_guard)
    end procedure pool_free_i32

    module procedure pool_free_i64
        !$omp critical (pf_index_pool_guard)
        call pool_release(self, idx)
        !$omp end critical (pf_index_pool_guard)
    end procedure pool_free_i64

    module procedure pool_get_max
        !$omp critical (pf_index_pool_guard)
        n = self%max_used
        !$omp end critical (pf_index_pool_guard)
    end procedure pool_get_max

    module procedure pool_get_free_count
        !$omp critical (pf_index_pool_guard)
        n = self%max_used - self%n_used
        !$omp end critical (pf_index_pool_guard)
    end procedure pool_get_free_count

    module procedure pool_get_used
        !$omp critical (pf_index_pool_guard)
        n = self%n_used
        !$omp end critical (pf_index_pool_guard)
    end procedure pool_get_used

    module procedure pool_is_used_i32
        !$omp critical (pf_index_pool_guard)
        ok = pool_held(self, int(idx, int64))
        !$omp end critical (pf_index_pool_guard)
    end procedure pool_is_used_i32

    module procedure pool_is_used_i64
        !$omp critical (pf_index_pool_guard)
        ok = pool_held(self, idx)
        !$omp end critical (pf_index_pool_guard)
    end procedure pool_is_used_i64

    module procedure pool_used_indexes_i64
        !$omp critical (pf_index_pool_guard)
        call pool_used_list(self, list)
        !$omp end critical (pf_index_pool_guard)
    end procedure pool_used_indexes_i64

    module procedure pool_used_indexes_i32
        integer(int64), allocatable :: wide(:)

        ! The whole read happens under the guard, watermark check included: the pool is documented
        ! safe to mutate from several threads, so a check taken outside it could be answered about
        ! a different pool than the list.
        !$omp critical (pf_index_pool_guard)
        call pool_used_list(self, wide)
        if (self%max_used > int(huge(0_int32), int64)) call ix_abort("pf_index_pool%used_indexes" // &
            ": an index is too large for an int32 answer; take the list as int64")
        allocate(list(size(wide, kind=int64)))
        list = int(wide, int32)
        !$omp end critical (pf_index_pool_guard)
    end procedure pool_used_indexes_i32

    module procedure pool_compact
        !$omp critical (pf_index_pool_guard)
        call pool_do_compact(self)
        !$omp end critical (pf_index_pool_guard)
    end procedure pool_compact

    module procedure pool_reserve_i32
        !$omp critical (pf_index_pool_guard)
        call pool_do_reserve(self, idx_reserve_check(int(n, int64), "reserve"))
        !$omp end critical (pf_index_pool_guard)
    end procedure pool_reserve_i32

    module procedure pool_reserve_i64
        !$omp critical (pf_index_pool_guard)
        call pool_do_reserve(self, idx_reserve_check(n, "reserve"))
        !$omp end critical (pf_index_pool_guard)
    end procedure pool_reserve_i64

    module procedure pool_memory_bytes
        !$omp critical (pf_index_pool_guard)
        b = pool_bytes(self)
        !$omp end critical (pf_index_pool_guard)
    end procedure pool_memory_bytes

    module procedure pool_clear
        !$omp critical (pf_index_pool_guard)
        call pool_do_clear(self)
        !$omp end critical (pf_index_pool_guard)
    end procedure pool_clear

    ! ---- Unguarded workers. None of these may call a public entry above. ----

    !> Validates a `reserve` count and returns it, so both kind specifics share one message.
    !!
    !! A function rather than a bare check so that the guarded body stays a single statement: the
    !! abort happens while the caller holds the guard, which is what keeps at most one thread on
    !! the fatal path.
    pure function idx_reserve_check(n, what) result(out)
        integer(int64), intent(in) :: n     !! the requested count.
        character(len=*), intent(in) :: what !! procedure name, for the message.
        integer(int64) :: out                !! `n`, unchanged.

        if (n < 0_int64) error stop "pf_index_pool%" // what // ": n must be >= 0"
        out = n
    end function idx_reserve_check

    !> The block of `bits` holding index `idx`, 1-based.
    !!
    !! `ishft(idx - 1, -6)` rather than `(idx - 1) / 64`: a shift is unconditionally a shift, where
    !! a division by a literal is only a shift if the optimiser says so, and this is called on
    !! every `%get_index` and every `%free_index`.
    pure function pool_blk(idx) result(b)
        integer(int64), intent(in) :: idx !! a 1-based index.
        integer(int64) :: b               !! the block holding it.

        b = ishft(idx - 1_int64, -6) + 1_int64
    end function pool_blk

    !> The bit position of index `idx` within its block, 0-based.
    pure function pool_pos(idx) result(p)
        integer(int64), intent(in) :: idx !! a 1-based index.
        integer :: p                      !! bit position, 0 .. 63.

        p = int(iand(idx - 1_int64, 63_int64))
    end function pool_pos

    !> Whether `idx` is currently held. `.false.` for anything outside `1 .. max_used`.
    pure function pool_held(self, idx) result(ok)
        type(pf_index_pool), intent(in) :: self !! the pool.
        integer(int64), intent(in) :: idx       !! the index to test; any value accepted.
        logical :: ok                           !! `.true.` when held.

        ok = .false.
        if (idx < 1_int64 .or. idx > self%max_used) return
        if (.not. allocated(self%bits)) return
        ok = btest(self%bits(pool_blk(idx)), pool_pos(idx))
    end function pool_held

    !> Grows the bitmap so that it can address at least `n` bits, zero-filling what is new.
    !!
    !! Geometric at 1.5x, matching the column-storage convention, so a run of `%get_index` calls
    !! is amortised O(1) each. The new blocks are zeroed explicitly rather than left to chance:
    !! an unset bit must mean "not held", so undefined bytes here would be a wrong answer from
    !! `%is_used` and a spurious abort from `%free_index`.
    subroutine pool_ensure_bits(self, n)
        type(pf_index_pool), intent(inout) :: self !! the pool.
        integer(int64), intent(in) :: n            !! bits that must be addressable.
        integer(int64), allocatable :: grown(:)
        integer(int64) :: need, have, want

        if (n <= self%nbits) return
        need = ishft(n + 63_int64, -6)
        have = 0_int64
        if (allocated(self%bits)) have = size(self%bits, kind=int64)
        want = have + have / 2_int64
        if (want < need) want = need
        allocate(grown(want))
        grown = 0_int64
        if (have > 0_int64) grown(1:have) = self%bits(1:have)
        call move_alloc(grown, self%bits)
        self%nbits = ishft(size(self%bits, kind=int64), 6)
    end subroutine pool_ensure_bits

    !> Pushes one released index onto the free list, growing it geometrically.
    subroutine pool_push_free(self, idx)
        type(pf_index_pool), intent(inout) :: self !! the pool.
        integer(int64), intent(in) :: idx          !! the index being released.
        integer(int64), allocatable :: grown(:)
        integer(int64) :: have, want

        have = 0_int64
        if (allocated(self%flist)) have = size(self%flist, kind=int64)
        if (self%nfree >= have) then
            want = have + have / 2_int64
            if (want < self%nfree + 1_int64) want = self%nfree + 1_int64
            if (want < 8_int64) want = 8_int64
            allocate(grown(want))
            grown = 0_int64
            if (self%nfree > 0_int64) grown(1:self%nfree) = self%flist(1:self%nfree)
            call move_alloc(grown, self%flist)
        end if
        self%nfree = self%nfree + 1_int64
        self%flist(self%nfree) = idx
    end subroutine pool_push_free

    !> Hands out an index: the top of the free list if there is one, else `max_used + 1`.
    !!
    !! Reuse before growth is the original specification's rule, and it is what keeps the issued
    !! set dense: the internal arrays only ever grow when every index up to the watermark is out.
    function pool_take(self) result(idx)
        type(pf_index_pool), intent(inout) :: self !! the pool.
        integer(int64) :: idx                      !! the index handed out; >= 1.

        if (self%nfree > 0_int64) then
            idx = self%flist(self%nfree)
            self%nfree = self%nfree - 1_int64
        else
            if (self%max_used == huge(0_int64)) &
                call ix_abort("pf_index_pool%get_index: the index space is exhausted")
            idx = self%max_used + 1_int64
            self%max_used = idx
            call pool_ensure_bits(self, idx)
        end if
        self%bits(pool_blk(idx)) = ibset(self%bits(pool_blk(idx)), pool_pos(idx))
        self%n_used = self%n_used + 1_int64
    end function pool_take

    !> Releases one index back to the pool, rejecting anything that is not currently held.
    !!
    !! The two rejections carry different messages on purpose: an index above the watermark was
    !! never handed out at all, which is usually a different bug from freeing something twice.
    subroutine pool_release(self, idx)
        type(pf_index_pool), intent(inout) :: self !! the pool.
        integer(int64), intent(in) :: idx          !! the index to release; must be held.
        character(len=32) :: t

        if (idx < 1_int64 .or. idx > self%max_used) then
            write (t, "(i0)") idx
            call ix_abort("pf_index_pool%free_index: index " // trim(t) // &
                " was never handed out by this pool")
        end if
        if (.not. btest(self%bits(pool_blk(idx)), pool_pos(idx))) then
            write (t, "(i0)") idx
            call ix_abort("pf_index_pool%free_index: index " // trim(t) // " is already free")
        end if
        self%bits(pool_blk(idx)) = ibclr(self%bits(pool_blk(idx)), pool_pos(idx))
        self%n_used = self%n_used - 1_int64
        call pool_push_free(self, idx)
    end subroutine pool_release

    !> Pre-sizes the bitmap for `n` indexes.
    subroutine pool_do_reserve(self, n)
        type(pf_index_pool), intent(inout) :: self !! the pool.
        integer(int64), intent(in) :: n            !! indexes to make room for.

        if (n > 0_int64) call pool_ensure_bits(self, n)
    end subroutine pool_do_reserve

    !> Every held index, ascending.
    !!
    !! Iterates the SET bits of each nonzero block with `trailz`/`ibclr` and skips a zero block
    !! whole, the same walk the column validity code uses -- so a pool holding few indexes over a
    !! wide watermark costs what its content is rather than what its range is.
    subroutine pool_used_list(self, list)
        type(pf_index_pool), intent(in) :: self                !! the pool.
        integer(int64), allocatable, intent(out) :: list(:)    !! the held indexes, ascending.
        integer(int64) :: b, w, k, nb
        integer :: j

        allocate(list(self%n_used))
        if (self%n_used == 0_int64) return
        list = 0_int64
        k = 0_int64
        nb = size(self%bits, kind=int64)
        do b = 1_int64, nb
            w = self%bits(b)
            do while (w /= 0_int64)
                j = trailz(w)
                w = ibclr(w, j)
                k = k + 1_int64
                list(k) = ishft(b - 1_int64, 6) + int(j, int64) + 1_int64
            end do
        end do
    end subroutine pool_used_list

    !> Lowers the watermark to the highest index actually held, rebuilds the free list so the
    !! smallest free index comes out next, and gives back storage the pool grew.
    !!
    !! The rebuild walks the bitmap ASCENDING and fills the free-list stack from its top downward,
    !! so popping (which takes the last entry) yields the free indexes smallest first. That is why
    !! there is no sort call here at all: the walk order and the stack discipline do it between
    !! them.
    !!
    !! Bits above the new watermark read as free in the complement and are filtered by the
    !! `i <= newmax` test rather than by masking the top word, which keeps the loop identical for
    !! every block.
    !!
    !! **Both bit walks here use `trailz`, and neither may be rewritten with `leadz`** -- however
    !! natural `63 - leadz(w)` is for finding a highest set bit. nagfor 7.2 miscompiles `LEADZ` on
    !! an `integer(int64)` at `-O1` and above: the result is exactly 2 too small whenever the true
    !! answer is 2 or more (62 of the 64 single-bit values are wrong), while `leadz` on `int32`,
    !! `trailz` and `popcnt` are all correct. Under `--profile release` that turned the free-list
    !! walk into an infinite loop -- `j` came back 63 instead of 61, so `ibclr` cleared a bit that
    !! was already clear and `w` stopped shrinking -- inside the `pf_index_pool_guard` critical
    !! region, hanging every other caller behind it. The watermark walk above has the same defect
    !! with a quieter failure: a `max_used` up to 2 too high, and nothing to report it. See
    !! `feature_risks.md` Risk-186.
    subroutine pool_do_compact(self)
        type(pf_index_pool), intent(inout) :: self !! the pool.
        integer(int64), allocatable :: grown(:)
        integer(int64) :: b, w, i, k, newmax, nb, topblk, need
        integer :: j

        if (.not. allocated(self%bits)) then
            self%max_used = 0_int64
            self%nfree = 0_int64
            return
        end if
        nb = size(self%bits, kind=int64)
        ! The highest set bit anywhere is the highest index still held.
        newmax = 0_int64
        do b = nb, 1_int64, -1_int64
            if (self%bits(b) /= 0_int64) then
                ! The LAST index this ascending walk reaches is the block's highest set bit.
                w = self%bits(b)
                do while (w /= 0_int64)
                    j = trailz(w)
                    w = ibclr(w, j)
                    newmax = ishft(b - 1_int64, 6) + int(j, int64) + 1_int64
                end do
                exit
            end if
        end do
        self%max_used = newmax
        ! Rebuild the free list over 1 .. newmax, filling the stack top-down, so popping ascends.
        k = 0_int64
        if (newmax > 0_int64) then
            need = newmax - self%n_used
            if (need > 0_int64) then
                if (allocated(self%flist)) deallocate(self%flist)
                allocate(self%flist(need))
                self%flist = 0_int64
                topblk = pool_blk(newmax)
                ! `need` is exactly the number of clear bits in 1 .. newmax, so this walk fills
                ! flist(need) down to flist(1) and ends with k == need. Both halves matter: the
                ! descending SLOT is what makes the ascending walk pop smallest-first, and the
                ! count is what makes `self%nfree = k` below name the top of a full stack.
                do b = 1_int64, topblk
                    w = not(self%bits(b))
                    do while (w /= 0_int64)
                        j = trailz(w)
                        w = ibclr(w, j)
                        i = ishft(b - 1_int64, 6) + int(j, int64) + 1_int64
                        if (i <= newmax) then
                            k = k + 1_int64
                            self%flist(need - k + 1_int64) = i
                        end if
                    end do
                end do
            else if (allocated(self%flist)) then
                deallocate(self%flist)
            end if
        else if (allocated(self%flist)) then
            deallocate(self%flist)
        end if
        self%nfree = k
        ! Give back the bitmap storage the watermark has fallen behind, on the specification's own
        ! condition: shrink once the allocation exceeds the watermark by more than the growth
        ! factor, so an ordinary run of growth is not undone by every compact.
        if (self%max_used == 0_int64) then
            deallocate(self%bits)
            self%nbits = 0_int64
        else if (self%max_used + self%max_used / 2_int64 < self%nbits) then
            need = ishft(self%max_used + 63_int64, -6)
            allocate(grown(need))
            grown = 0_int64
            grown(1:need) = self%bits(1:need)
            call move_alloc(grown, self%bits)
            self%nbits = ishft(size(self%bits, kind=int64), 6)
        end if
    end subroutine pool_do_compact

    !> Bytes of heap this pool holds.
    pure function pool_bytes(self) result(b)
        type(pf_index_pool), intent(in) :: self !! the pool.
        integer(int64) :: b                     !! bytes held by the bitmap and the free list.

        b = 0_int64
        if (allocated(self%bits)) b = b + 8_int64 * size(self%bits, kind=int64)
        if (allocated(self%flist)) b = b + 8_int64 * size(self%flist, kind=int64)
    end function pool_bytes

    !> Resets the pool to its as-new state.
    subroutine pool_do_clear(self)
        type(pf_index_pool), intent(inout) :: self !! the pool.

        if (allocated(self%bits)) deallocate(self%bits)
        if (allocated(self%flist)) deallocate(self%flist)
        self%max_used = 0_int64
        self%n_used = 0_int64
        self%nfree = 0_int64
        self%nbits = 0_int64
    end subroutine pool_do_clear

end submodule parquet_index_pool
