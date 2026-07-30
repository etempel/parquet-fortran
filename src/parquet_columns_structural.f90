!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> Lifecycle and structural mutation for `parquet_column`: `init`, `clear`, `deep_copy`, the
!! unit accessors, and the four row-set operations `append`, `append_nulls`, `delete_by_mask`
!! and `reindex`.
!!
!! Every procedure here is written **once, kind-agnostically**, on top of the four generated
!! per-kind storage helpers in `parquet_columns_mutate` (`grow_storage`, `gather_storage`,
!! `append_storage`, `copy_storage`). What is left in this file is the part that is genuinely
!! about the column as a whole rather than about one kind: geometry bookkeeping, the sparse
!! bitmap (R2), and the string kinds' delegation to `parquet_string_column` (DD1) — the one
!! storage that does not live in a Fortran array of its own.
!!
!! Note the string kinds index their store by ELEMENT, not by row: a `PK_STRING_VEC` column of
!! `nrows` rows and width `w` holds `nrows*w` strings, with row `i`'s vector at flat positions
!! `(i-1)*w + 1 .. i*w` (RF6). Every row-level operation below expands the row index range
!! accordingly before delegating.
submodule (parquet_columns) parquet_columns_structural
    implicit none
contains
    !
    !> Sets the column's kind and geometry, releasing whatever it held before.
    !!
    !! Rows are created in the natural "empty" state for the kind: unspecified values for the
    !! numeric and logical kinds (they carry no nulls until one is set — R2), and genuinely null
    !! elements for the temporal and string kinds, whose null state lives in the element itself.
    module procedure init
        integer(int32) :: w
        call self%clear()
        if (kind == PK_NONE) error stop EP//"init: PK_NONE is not a storable kind"
        if (kind == PK_LIST .or. kind == PK_MAP .or. kind == PK_STRUCT) then
            error stop EP//"init: container kinds are reserved and not implemented yet"
        end if
        if (nrows < 0_int64) error stop EP//"init: negative row count"
        w = 1_int32
        if (present(width)) w = width
        if (w < 1_int32) error stop EP//"init: column width must be at least 1"
        if (is_vector_kind(kind)) then
            if (w < 2_int32) error stop EP//"init: a vector kind requires width > 1"
        else if (w /= 1_int32) then
            error stop EP//"init: width > 1 requires a vector (*_VEC) kind"
        end if
        self%kind = kind
        self%width = w
        self%nrows = 0_int64
        if (present(unit)) then
            if (len_trim(unit) > 0) self%unit = trim(unit)
        end if
        if (is_string_kind(kind)) allocate(self%str)
        if (nrows > 0_int64) call grow_rows(self, nrows)
        if (is_temporal_kind(kind)) then
            self%nulls_dirty = .true.
        end if
    end procedure init
    !
    !> Releases every storage array and resets the column to an empty PK_NONE state.
    module procedure clear
        self%kind = PK_NONE
        self%nrows = 0_int64
        self%width = 1_int32
        self%has_nulls = .false.
        self%nulls_dirty = .true.
        self%nulls_cached = .false.
        if (allocated(self%validity)) deallocate(self%validity)
        if (allocated(self%unit)) deallocate(self%unit)
        if (allocated(self%str)) deallocate(self%str)
        if (allocated(self%i32)) deallocate(self%i32)
        if (allocated(self%i64)) deallocate(self%i64)
        if (allocated(self%f32)) deallocate(self%f32)
        if (allocated(self%f64)) deallocate(self%f64)
        if (allocated(self%bool)) deallocate(self%bool)
        if (allocated(self%dt)) deallocate(self%dt)
        if (allocated(self%tm)) deallocate(self%tm)
        if (allocated(self%ts)) deallocate(self%ts)
        if (allocated(self%i32v)) deallocate(self%i32v)
        if (allocated(self%i64v)) deallocate(self%i64v)
        if (allocated(self%f32v)) deallocate(self%f32v)
        if (allocated(self%f64v)) deallocate(self%f64v)
        if (allocated(self%boolv)) deallocate(self%boolv)
        if (allocated(self%dtv)) deallocate(self%dtv)
        if (allocated(self%tmv)) deallocate(self%tmv)
        if (allocated(self%tsv)) deallocate(self%tsv)
        if (allocated(self%container)) deallocate(self%container)
    end procedure clear
    !
    !> Produces a fully independent copy (F-mut-6): values, validity, unit and geometry.
    !!
    !! This is the library's only rollback mechanism under the in-place mutation model (D6's
    !! "copy before mutating"), so it is a first-class operation rather than an afterthought.
    module procedure deep_copy
        call out%clear()
        if (self%kind == PK_NONE) return
        if (allocated(self%unit)) then
            call out%init(self%kind, self%nrows, self%width, self%unit)
        else
            call out%init(self%kind, self%nrows, self%width)
        end if
        call copy_storage(self, out)
        if (allocated(self%validity)) then
            allocate(out%validity(size(self%validity, kind=int64)))
            out%validity = self%validity
        end if
        out%has_nulls = self%has_nulls
        out%nulls_dirty = self%nulls_dirty
        out%nulls_cached = self%nulls_cached
    end procedure deep_copy
    !
    !> Copies the unit string out, yielding "" when the column carries no unit.
    module procedure unit_string
        if (allocated(self%unit)) then
            u = self%unit
        else
            u = ""
        end if
    end procedure unit_string
    !
    !> Sets the column's unit string; an empty string clears it.
    module procedure set_unit
        if (allocated(self%unit)) deallocate(self%unit)
        if (len_trim(u) > 0) self%unit = trim(u)
    end procedure set_unit
    !
    !> Appends every row of `other`, which must have identical kind and width.
    !!
    !! Validity is carried across: if either column has nulls the destination ends up
    !! bitmap-backed, with the appended rows' bits taken from the source. A null-free append
    !! into a null-free column allocates no bitmap at all.
    module procedure append
        integer(int64) :: old, n, k, w, e, src_base, dst_base
        if (self%kind /= other%kind) then
            error stop EP//"append: column kinds differ"
        end if
        if (self%width /= other%width) then
            error stop EP//"append: column widths differ"
        end if
        n = other%nrows
        if (n == 0_int64) return
        old = self%nrows
        w = int(self%width, int64)
        call append_storage(self, other)
        if (is_string_kind(self%kind)) return
        if (is_temporal_kind(self%kind)) then
            self%nulls_dirty = .true.
            return
        end if
        if (other%has_nulls) then
            call ensure_bitmap(self)
            do k = 1_int64, n
                src_base = (k - 1_int64)*w
                dst_base = (old + k - 1_int64)*w
                do e = 1_int64, w
                    if (bit_test(other%validity, src_base + e)) then
                        call bit_set(self%validity, dst_base + e)
                    end if
                end do
            end do
        end if
    end procedure append
    !
    !> Appends `n` all-null rows (R5).
    !!
    !! This is what makes the "grow a seed table by N blank rows, fill them during the analysis,
    !! then append the source" workflow cheap. For bitmap-backed kinds it is also the operation
    !! that materializes the bitmap, since those rows are the column's first nulls.
    module procedure append_nulls
        integer(int64) :: old, k
        if (n < 0_int64) error stop EP//"append_nulls: negative row count"
        if (n == 0_int64) return
        if (self%kind == PK_NONE) error stop EP//"append_nulls: column has no kind assigned"
        old = self%nrows
        call grow_rows(self, n)
        if (is_string_kind(self%kind)) return
        if (is_temporal_kind(self%kind)) then
            self%nulls_dirty = .true.
            return
        end if
        call ensure_bitmap(self)
        do k = old + 1_int64, self%nrows
            call self%set_null(k)
        end do
    end procedure append_nulls
    !
    !> Overwrites the existing row range `at .. at+count-1` with rows of `src` (see the full
    !! contract on the interface in `parquet_columns.f90`).
    !!
    !! Not a row-structural operation: `nrows` is unchanged, nothing is reallocated, and no
    !! outstanding `data_ptr`/row handle is invalidated -- which is the whole point, since the
    !! caller has already sized the column and is filling it in pieces.
    !!
    !! Validity is *replaced*, not merged: an element pasted from a valid source element becomes
    !! valid even if the destination row was null before. Overwriting a row range and leaving a
    !! stale null behind would be a silent wrong answer, so the all-valid source path still has
    !! to clear bits -- but only when this column actually has a bitmap to clear.
    module procedure paste
        integer(int64) :: n, f, k, w, e, src_base, dst_base
        if (self%kind /= src%kind) error stop EP//"paste: column kinds differ"
        if (self%width /= src%width) error stop EP//"paste: column widths differ"
        if (is_string_kind(self%kind)) then
            error stop EP//"paste: the string kinds cannot be overwritten in place; use append"
        end if
        f = 1_int64
        if (present(from)) f = from
        n = src%nrows - f + 1_int64
        if (present(count)) n = count
        if (f < 1_int64) error stop EP//"paste: source row index is below 1"
        if (n < 0_int64) error stop EP//"paste: negative row count"
        if (n == 0_int64) return
        if (f + n - 1_int64 > src%nrows) then
            error stop EP//"paste: source row range extends past the end of the source column"
        end if
        if (at < 1_int64) error stop EP//"paste: destination row index is below 1"
        if (at + n - 1_int64 > self%nrows) then
            error stop EP//"paste: destination row range extends past the end of the column"
        end if
        call paste_storage(self, src, at, f, n)
        if (is_temporal_kind(self%kind)) then
            self%nulls_dirty = .true.
            return
        end if
        w = int(self%width, int64)
        if (src%has_nulls) then
            call ensure_bitmap(self)
            do k = 1_int64, n
                src_base = (f + k - 2_int64)*w
                dst_base = (at + k - 2_int64)*w
                do e = 1_int64, w
                    if (bit_test(src%validity, src_base + e)) then
                        call bit_set(self%validity, dst_base + e)
                    else
                        call bit_clear(self%validity, dst_base + e)
                    end if
                end do
            end do
        else if (self%has_nulls) then
            ! Every source element is valid, so the destination range must end up all-valid too.
            ! Skipped entirely when this column has no bitmap: there is then nothing to clear,
            ! and materializing one just to write zeros into it would defeat the sparse design.
            do k = 1_int64, n
                dst_base = (at + k - 2_int64)*w
                do e = 1_int64, w
                    call bit_clear(self%validity, dst_base + e)
                end do
            end do
        end if
    end procedure paste
    !
    !> Keeps only the rows whose `keep` entry is .true., preserving their order.
    !!
    !! A row-structural operation: it changes the row set, so at the table layer it is one of the
    !! operations that detaches the table from its file (D6) and invalidates every outstanding
    !! pointer and row handle.
    module procedure delete_by_mask
        integer(int64) :: n, k, kept
        integer(int64), allocatable :: idx(:)
        logical, allocatable :: elem_keep(:)
        n = self%nrows
        if (size(keep, kind=int64) /= n) then
            error stop EP//"delete_by_mask: mask length does not match the column's row count"
        end if
        if (n == 0_int64) return
        kept = count(keep, kind=int64)
        allocate(idx(max(kept, 1_int64)))
        kept = 0_int64
        do k = 1_int64, n
            if (.not. keep(k)) cycle
            kept = kept + 1_int64
            idx(kept) = k
        end do
        if (is_string_kind(self%kind)) then
            call expand_row_mask(keep, int(self%width, int64), elem_keep)
            call self%str%delete_by_mask(elem_keep)
        else
            call gather_storage(self, idx(1:kept))
        end if
        call gather_validity(self, idx(1:kept))
        self%nrows = kept
        if (is_temporal_kind(self%kind)) self%nulls_dirty = .true.
    end procedure delete_by_mask
    !
    !> Reorders rows so that row `k` afterwards is the row that was at `perm(k)`.
    !!
    !! `perm` is validated in full (length, range, no duplicates) **before** any storage is
    !! touched, so a bad permutation aborts with the column still intact rather than half
    !! rebuilt. This is the primitive the table's in-memory `sort_by` is built on: the sort
    !! engine produces the permutation, every column then replays it.
    module procedure reindex
        integer(int64) :: n, k, p
        integer(int64), allocatable :: elem_perm(:)
        logical, allocatable :: seen(:)
        n = self%nrows
        if (size(perm, kind=int64) /= n) then
            error stop EP//"reindex: permutation length does not match the column's row count"
        end if
        if (n == 0_int64) return
        allocate(seen(n))
        seen = .false.
        do k = 1_int64, n
            p = perm(k)
            if (p < 1_int64 .or. p > n) error stop EP//"reindex: permutation entry out of range"
            if (seen(p)) error stop EP//"reindex: permutation contains a duplicate index"
            seen(p) = .true.
        end do
        deallocate(seen)
        if (is_string_kind(self%kind)) then
            call expand_row_perm(perm, int(self%width, int64), elem_perm)
            call self%str%reindex(elem_perm)
        else
            call gather_storage(self, perm)
        end if
        call gather_validity(self, perm)
        if (is_temporal_kind(self%kind)) self%nulls_dirty = .true.
    end procedure reindex
    !
    ! ==================================================================================
    ! Private helpers
    ! ==================================================================================
    !
    !> Whether a PK_* kind stores several values per row (a `*_VEC` kind).
    pure function is_vector_kind(kind) result(res)
        integer, intent(in) :: kind !! a PK_* discriminator.
        logical :: res              !! .true. for the vector kinds.
        select case (kind)
        case (PK_INT32_VEC, PK_INT64_VEC, PK_FLOAT32_VEC, PK_FLOAT64_VEC, PK_LOGICAL_VEC, &
              PK_STRING_VEC, PK_DATE_VEC, PK_TIME_VEC, PK_TIMESTAMP_VEC)
            res = .true.
        case default
            res = .false.
        end select
    end function is_vector_kind
    !
    !> Grows the column by `n` rows, routing the string kinds to their own store.
    !!
    !! New rows start out null for the string kinds (`append_nulls` on the embedded store) and
    !! default-initialized for the array kinds -- which is already null for the temporal kinds
    !! and unspecified-but-valid for the numeric ones, matching `init`'s documented contract.
    subroutine grow_rows(self, n)
        class(parquet_column), intent(inout) :: self !! the column.
        integer(int64), intent(in) :: n              !! number of rows to add.
        if (n <= 0_int64) return
        if (is_string_kind(self%kind)) then
            call self%str%append_nulls(n*int(self%width, int64))
            self%nrows = self%nrows + n
        else
            call grow_storage(self, n)
        end if
    end subroutine grow_rows
    !
    !> Rebuilds the validity bitmap so that destination row `k` takes the bits of source row
    !! `idx(k)`, element by element. A no-op for the kinds that carry no bitmap.
    subroutine gather_validity(self, idx)
        class(parquet_column), intent(inout) :: self !! the column.
        integer(int64), intent(in) :: idx(:)         !! source row index per destination row.
        integer(int64) :: n, k, e, w, src_base, dst_base, nbits
        integer(int64), allocatable :: new_map(:)
        if (.not. self%has_nulls) return
        if (.not. allocated(self%validity)) return
        if (is_string_kind(self%kind) .or. is_temporal_kind(self%kind)) return
        n = size(idx, kind=int64)
        w = int(self%width, int64)
        nbits = n*w
        allocate(new_map(max(blocks_for(nbits), 1_int64)))
        new_map = 0_int64
        do k = 1_int64, n
            src_base = (idx(k) - 1_int64)*w
            dst_base = (k - 1_int64)*w
            do e = 1_int64, w
                if (bit_test(self%validity, src_base + e)) call bit_set(new_map, dst_base + e)
            end do
        end do
        call move_alloc(new_map, self%validity)
    end subroutine gather_validity
    !
    !> Expands a per-row keep mask into the per-element mask a string store needs (RF6's flat
    !! `(i-1)*width + e` layout).
    subroutine expand_row_mask(keep, w, elem_keep)
        logical, intent(in) :: keep(:)                       !! per-row keep mask.
        integer(int64), intent(in) :: w                      !! column width.
        logical, allocatable, intent(out) :: elem_keep(:)    !! per-element keep mask.
        integer(int64) :: n, k, e
        n = size(keep, kind=int64)
        allocate(elem_keep(max(n*w, 1_int64)))
        elem_keep = .false.
        do k = 1_int64, n
            do e = 1_int64, w
                elem_keep((k - 1_int64)*w + e) = keep(k)
            end do
        end do
    end subroutine expand_row_mask
    !
    !> Expands a per-row permutation into the per-element permutation a string store needs.
    subroutine expand_row_perm(perm, w, elem_perm)
        integer(int64), intent(in) :: perm(:)                 !! per-row permutation.
        integer(int64), intent(in) :: w                       !! column width.
        integer(int64), allocatable, intent(out) :: elem_perm(:) !! per-element permutation.
        integer(int64) :: n, k, e
        n = size(perm, kind=int64)
        allocate(elem_perm(max(n*w, 1_int64)))
        do k = 1_int64, n
            do e = 1_int64, w
                elem_perm((k - 1_int64)*w + e) = (perm(k) - 1_int64)*w + e
            end do
        end do
    end subroutine expand_row_perm
    !
end submodule parquet_columns_structural
