!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> Kind-dispatched validity for `parquet_column` (RF8).
!!
!! Validity is deliberately NOT uniform across kinds, and this submodule is the single place
!! that knows the difference:
!!
!! ```
!! kind                          null state lives in            any_null() answers via
!! int32/int64/f32/f64/logical   the column's own bitmap        bitmap scan (O(blocks))
!!   and their *_VEC forms       one bit per ELEMENT            (nrows*width bits)
!! string (+ PK_STRING_VEC)      the embedded string column     its own null_count() > 0
!! date/time/timestamp (+ _VEC)  inside each element            cached flag, rescanned when dirty
!! list/map/struct (reserved)    the container column           delegated (not yet implemented)
!! ```
!!
!! Hoisting everything into the column bitmap was rejected: it would mean two sources of truth
!! kept in sync by every mutation path, and for the temporal kinds it would fight
!! `parquet_temporal`'s deliberate decision that those three types carry their own null state.
!!
!! The bitmap is **sparse** (R2): a null-free column allocates nothing at all. It appears on the
!! first `set_null`/`append_nulls` and is dropped again by a whole-column `set_all` or an
!! explicit `compact_validity`.
submodule (parquet_columns) parquet_columns_validity
    implicit none
contains
    !
    !> Whether any element of the column is null.
    !!
    !! Bitmap kinds scan the blocks (cheap: one machine word per 64 elements, and the common
    !! null-free case is a single `allocated()` test). String kinds ask the embedded column.
    !! Temporal kinds would need an O(n) element scan, so the answer is cached and only
    !! recomputed after a mutation has marked it dirty -- otherwise `any_null` would be an O(n)
    !! query on types whose whole point is cheap access.
    module procedure any_null
        integer(int64) :: k, nbits, nblk
        res = .false.
        if (self%nrows <= 0_int64) return
        if (is_string_kind(self%kind)) then
            if (allocated(self%str)) res = self%str%null_count() > 0_int64
            return
        end if
        if (is_temporal_kind(self%kind)) then
            if (self%nulls_dirty) then
                call rescan_temporal_nulls(self)
            end if
            res = self%nulls_cached
            return
        end if
        if (.not. self%has_nulls) return
        if (.not. allocated(self%validity)) return
        nbits = bits_needed(self)
        nblk = min(blocks_for(nbits), size(self%validity, kind=int64))
        do k = 1_int64, nblk
            if (self%validity(k) /= 0_int64) then
                res = .true.
                return
            end if
        end do
    end procedure any_null
    !
    !> Whether row `i` is null.
    !!
    !! For a vector kind a row is reported null when its FIRST element is null, which is the
    !! convention the whole-row operations (`set_null`, `append_nulls`, the `modify_nulls=`
    !! guard) maintain: they mark every element of the row together. Per-element nulls within a
    !! row remain representable in the bitmap for the reader path, but row-level queries answer
    !! about the row.
    module procedure is_null
        integer(int64) :: bit
        call check_index(self, i, "is_null")
        res = .false.
        select case (self%kind)
        case (PK_STRING, PK_STRING_VEC)
            if (.not. allocated(self%str)) return
            if (self%kind == PK_STRING) then
                res = self%str%is_null(i)
            else
                res = self%str%is_null((i - 1_int64)*int(self%width, int64) + 1_int64)
            end if
        case (PK_DATE)
            res = self%dt(i)%is_null()
        case (PK_TIME)
            res = self%tm(i)%is_null()
        case (PK_TIMESTAMP)
            res = self%ts(i)%is_null()
        case (PK_DATE_VEC)
            res = self%dtv(1, i)%is_null()
        case (PK_TIME_VEC)
            res = self%tmv(1, i)%is_null()
        case (PK_TIMESTAMP_VEC)
            res = self%tsv(1, i)%is_null()
        case (PK_NONE)
            ! Unreachable through the public API: check_index above rejects every index on a
            ! kindless column (its row count is 0), so this arm is defensive only.
            error stop EP//"is_null: column has no kind assigned" ! GCOVR_EXCL_LINE
        case default
            if (.not. self%has_nulls) return
            bit = (i - 1_int64)*int(self%width, int64) + 1_int64
            res = bit_test(self%validity, bit)
        end select
    end procedure is_null
    !
    !> Builds the whole per-row validity mask in one pass.
    !!
    !! Three cases, in decreasing order of how well they can be done:
    !!
    !!  1. **No nulls at all** -- `valid` is left unallocated and nothing is scanned. This is the
    !!     case worth optimising for: it is what every column of an ordinary file looks like, and
    !!     an unallocated result passed to an `optional` dummy is an absent argument, so the
    !!     caller's writer never has to build a null bitmap either.
    !!  2. **A bitmap kind with nulls** -- the bitmap is walked one 64-bit block at a time, and a
    !!     block that is entirely zero (64 valid elements, the overwhelmingly common block even in
    !!     a column that does have nulls) is skipped without touching a single row. Only a nonzero
    !!     block costs per-row work.
    !!  3. **A string or temporal kind** -- their null state does not live in this bitmap at all
    !!     (see the table in the module doc), so there is nothing to walk and the per-row `is_null`
    !!     loop is the honest implementation.
    module procedure row_validity
        integer(int64) :: i, n, nblk, blk, base, lo, hi, w, nbits
        integer(int64) :: word
        integer :: p
        !
        n = self%nrows
        if (n <= 0_int64) return
        ! Case 1. any_null is O(1) for a column that never had a null set, so this costs nothing
        ! on the common path -- and refreshes the temporal kinds' cache, which is why `self` is
        ! intent(inout) here just as it is on any_null itself.
        if (.not. self%any_null()) return
        allocate(valid(n))
        valid = .true.
        ! Case 3.
        if (is_string_kind(self%kind) .or. is_temporal_kind(self%kind)) then
            do i = 1_int64, n
                valid(i) = .not. self%is_null(i)
            end do
            return
        end if
        ! Case 2. any_null already established there is a bitmap; the guard keeps a future caller
        ! from turning a missing one into an out-of-bounds read.
        if (.not. allocated(self%validity)) return
        w = int(self%width, int64)
        nbits = bits_needed(self)
        nblk = min(blocks_for(nbits), size(self%validity, kind=int64))
        do blk = 1_int64, nblk
            word = self%validity(blk)
            if (word == 0_int64) cycle
            base = (blk - 1_int64)*BITS_PER_BLOCK
            if (w == 1_int64) then
                ! Scalar kinds: one bit per row, so a block covers 64 consecutive rows and each
                ! set bit names its row directly.
                do p = 0, int(BITS_PER_BLOCK) - 1
                    if (.not. btest(word, p)) cycle
                    i = base + int(p, int64) + 1_int64
                    if (i <= n) valid(i) = .false.
                end do
            else
                ! Vector kinds: a row's validity is its FIRST element's bit (the same convention
                ! is_null uses), so consecutive rows sit `width` bits apart and a block spans only
                ! part of a row range. Derive that range and test the one bit each row owns.
                lo = base/w + 1_int64
                hi = (base + BITS_PER_BLOCK - 1_int64)/w + 1_int64
                if (lo < 1_int64) lo = 1_int64
                if (hi > n) hi = n
                do i = lo, hi
                    if (bit_test(self%validity, (i - 1_int64)*w + 1_int64)) valid(i) = .false.
                end do
            end if
        end do
    end procedure row_validity
    !
    !> Marks row `i` null.
    !!
    !! For a bitmap kind this is where the bitmap is lazily allocated (R2 ii) -- the first null
    !! in a column is what makes it exist at all. For a vector kind every element of the row is
    !! marked. Temporal kinds write the element's own null state; string kinds delegate.
    module procedure set_null
        integer(int64) :: e, base, w
        call check_index(self, i, "set_null")
        w = int(self%width, int64)
        select case (self%kind)
        case (PK_STRING)
            call self%str%set_null(i)
        case (PK_STRING_VEC)
            base = (i - 1_int64)*w
            do e = 1_int64, w
                call self%str%set_null(base + e)
            end do
        case (PK_DATE)
            call self%dt(i)%set_null()
            self%nulls_dirty = .true.
        case (PK_TIME)
            call self%tm(i)%set_null()
            self%nulls_dirty = .true.
        case (PK_TIMESTAMP)
            call self%ts(i)%set_null()
            self%nulls_dirty = .true.
        case (PK_DATE_VEC)
            do e = 1_int64, w
                call self%dtv(e, i)%set_null()
            end do
            self%nulls_dirty = .true.
        case (PK_TIME_VEC)
            do e = 1_int64, w
                call self%tmv(e, i)%set_null()
            end do
            self%nulls_dirty = .true.
        case (PK_TIMESTAMP_VEC)
            do e = 1_int64, w
                call self%tsv(e, i)%set_null()
            end do
            self%nulls_dirty = .true.
        case (PK_NONE)
            ! Unreachable through the public API: check_index above rejects every index on a
            ! kindless column (its row count is 0), so this arm is defensive only.
            error stop EP//"set_null: column has no kind assigned" ! GCOVR_EXCL_LINE
        case default
            call ensure_bitmap(self)
            base = (i - 1_int64)*w
            do e = 1_int64, w
                call bit_set(self%validity, base + e)
            end do
        end select
    end procedure set_null
    !
    !> Marks row `i` valid without writing a value.
    !!
    !! The value behind a previously-null row is unspecified until it is written, so this is
    !! normally used by the value-writing paths rather than called directly. It never drops the
    !! bitmap (a single-cell edit does not pay for a scan, R2 iii) -- use `compact_validity` for
    !! that. On a column that has no bitmap and no nulls it is a no-op.
    module procedure clear_null
        integer(int64) :: e, base, w
        call check_index(self, i, "clear_null")
        w = int(self%width, int64)
        select case (self%kind)
        case (PK_STRING)
            call self%str%set(i, "")
        case (PK_STRING_VEC)
            base = (i - 1_int64)*w
            do e = 1_int64, w
                call self%str%set(base + e, "")
            end do
        case (PK_DATE, PK_TIME, PK_TIMESTAMP, PK_DATE_VEC, PK_TIME_VEC, PK_TIMESTAMP_VEC)
            error stop EP//"clear_null: a temporal element becomes valid by writing a value to it"
        case (PK_NONE)
            ! Unreachable through the public API: check_index above rejects every index on a
            ! kindless column (its row count is 0), so this arm is defensive only.
            error stop EP//"clear_null: column has no kind assigned" ! GCOVR_EXCL_LINE
        case default
            if (.not. self%has_nulls) return
            base = (i - 1_int64)*w
            do e = 1_int64, w
                call bit_clear(self%validity, base + e)
            end do
        end select
    end procedure clear_null
    !
    !> Scans for remaining nulls and releases the bitmap when there are none (R2 iv).
    !!
    !! This is the explicit, public counterpart to the automatic O(1) drop a whole-column
    !! `set_all` performs: single-cell edits deliberately skip the scan, so a column that has had
    !! its last null overwritten one cell at a time keeps its bitmap until this is called.
    !! Harmless (and cheap) on kinds that carry no bitmap.
    module procedure compact_validity
        if (is_string_kind(self%kind)) return
        if (is_temporal_kind(self%kind)) then
            call rescan_temporal_nulls(self)
            return
        end if
        if (.not. self%has_nulls) return
        if (self%any_null()) return
        call drop_bitmap(self)
    end procedure compact_validity
    !
    !> Recomputes a temporal column's cached "has at least one null" flag (the O(n) scan the
    !! cache exists to avoid repeating).
    subroutine rescan_temporal_nulls(self)
        class(parquet_column), intent(inout) :: self !! the temporal column.
        integer(int64) :: k
        self%nulls_cached = .false.
        do k = 1_int64, self%nrows
            if (self%is_null(k)) then
                self%nulls_cached = .true.
                exit
            end if
        end do
        self%nulls_dirty = .false.
    end subroutine rescan_temporal_nulls
    !
end submodule parquet_columns_validity
