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
        ! Reset alongside nrows, never left behind: a stale capacity on a column whose storage has
        ! just been deallocated would make the next ensure_capacity believe there is room and skip
        ! the allocation entirely.
        self%cap = 0_int64
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
    module procedure move_from
        ! Handed over one component at a time rather than by intrinsic assignment, which would
        ! deep-copy every one of them -- the whole point of this procedure. THE COMPONENT LIST
        ! HERE AND `clear`'s ABOVE MUST STAY IN STEP: a new kind's storage array missing from this
        ! list is not a compile error, it is a column that silently loses that array on every
        ! move. Both lists are checked by test_column_move_from, which moves a column of every
        ! kind and asserts the source came back empty and the destination intact.
        call self%clear()
        self%kind = src%kind
        self%nrows = src%nrows
        ! Moved with the arrays, not recomputed: the destination inherits the source's ALLOCATION,
        ! so it inherits its capacity too. Deriving it from nrows instead would understate it and
        ! the next append would reallocate storage that already had room.
        self%cap = src%cap
        self%width = src%width
        self%has_nulls = src%has_nulls
        self%nulls_dirty = src%nulls_dirty
        self%nulls_cached = src%nulls_cached
        if (allocated(src%validity)) call move_alloc(src%validity, self%validity)
        if (allocated(src%unit)) call move_alloc(src%unit, self%unit)
        if (allocated(src%str)) call move_alloc(src%str, self%str)
        if (allocated(src%i32)) call move_alloc(src%i32, self%i32)
        if (allocated(src%i64)) call move_alloc(src%i64, self%i64)
        if (allocated(src%f32)) call move_alloc(src%f32, self%f32)
        if (allocated(src%f64)) call move_alloc(src%f64, self%f64)
        if (allocated(src%bool)) call move_alloc(src%bool, self%bool)
        if (allocated(src%dt)) call move_alloc(src%dt, self%dt)
        if (allocated(src%tm)) call move_alloc(src%tm, self%tm)
        if (allocated(src%ts)) call move_alloc(src%ts, self%ts)
        if (allocated(src%i32v)) call move_alloc(src%i32v, self%i32v)
        if (allocated(src%i64v)) call move_alloc(src%i64v, self%i64v)
        if (allocated(src%f32v)) call move_alloc(src%f32v, self%f32v)
        if (allocated(src%f64v)) call move_alloc(src%f64v, self%f64v)
        if (allocated(src%boolv)) call move_alloc(src%boolv, self%boolv)
        if (allocated(src%dtv)) call move_alloc(src%dtv, self%dtv)
        if (allocated(src%tmv)) call move_alloc(src%tmv, self%tmv)
        if (allocated(src%tsv)) call move_alloc(src%tsv, self%tsv)
        if (allocated(src%container)) call move_alloc(src%container, self%container)
        ! `src` gave up its arrays but still claims a kind and a row count, which would describe
        ! storage that is no longer there. Reset it to exactly what a fresh column looks like.
        call src%clear()
    end procedure move_from
    !
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
    !> int32 form of `reserve`; converts and delegates.
    module procedure reserve_i32
        call self%reserve_i64(int(n, int64))
    end procedure reserve_i32
    !
    !> Grows capacity to hold at least `n` rows without changing the row count (see the interface
    !! in `parquet_columns.f90` for the full contract).
    module procedure reserve_i64
        if (n < 0_int64) error stop EP//"reserve: negative row count"
        if (self%kind == PK_NONE) error stop EP//"reserve: column has no kind assigned"
        if (is_string_kind(self%kind)) then
            ! The string store indexes by ELEMENT, not by row (RF6), so a width-w column of n rows
            ! needs n*w elements reserved. Characters are left to grow on their own: how many bytes
            ! n rows will occupy is not knowable from a row count.
            call parquet_string_column_reserve(self%str, n*int(self%width, int64), 0_int64)
            return
        end if
        call ensure_capacity(self, n)
        ! Only when the bitmap already exists. Reserving must not materialize one -- that would
        ! defeat the sparse representation (R2) for a column that may never see a null.
        if (self%has_nulls) call ensure_bitmap(self)
    end procedure reserve_i64
    !
    !> Releases capacity beyond the rows stored (see the interface in `parquet_columns.f90`).
    module procedure shrink_to_fit
        integer(int64) :: need
        if (present(released)) released = .false.
        if (is_string_kind(self%kind)) then
            if (.not. allocated(self%str)) return
            ! Two independent buffers carry slack in a string store -- the offsets array and the
            ! byte payload -- and either alone means there is something to release.
            if (present(released)) then
                released = parquet_string_column_capacity(self%str) > parquet_string_column_size(self%str) .or. &
                    parquet_string_column_character_capacity(self%str) > parquet_string_column_character_size(self%str)
            end if
            call parquet_string_column_shrink_to_fit(self%str)
            return
        end if
        if (present(released)) released = self%cap > self%nrows
        call shrink_storage(self)
        ! The bitmap is sized from `cap`, so shrinking the storage leaves it oversized too. Trim it
        ! to what the rows actually need -- `ensure_bitmap` only ever grows, so this is the one
        ! place it comes back down.
        if (allocated(self%validity)) then
            need = max(blocks_for(bits_needed(self)), 1_int64)
            if (size(self%validity, kind=int64) > need) then
                block
                    integer(int64), allocatable :: tmp(:)
                    allocate(tmp(need))
                    tmp(1:need) = self%validity(1:need)
                    call move_alloc(tmp, self%validity)
                end block
            end if
        end if
    end procedure shrink_to_fit
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
            ! Both runs are contiguous -- `n` whole rows from the front of `other` onto `n` whole
            ! rows at the end of `self` -- so this is one bit-run copy rather than `n*w` calls that
            ! each redo the divide and the `mod`. MERGE, not replace: the destination rows were
            ! just grown and are all-valid, and merging is `%append`'s documented rule.
            call bits_copy_range(self%validity, old*w + 1_int64, other%validity, 1_int64, n*w, .true.)
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
        ! The appended rows are one contiguous run of n*width bits, so they are filled a word at a
        ! time. The loop this replaces was a type-bound `%set_null` per row, each one redoing
        ! `check_index` and the kind `select case` before touching a single bit.
        call bits_set_range(self%validity, old*int(self%width, int64) + 1_int64, &
                            self%nrows*int(self%width, int64))
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
            ! REPLACE, not merge -- a valid source element clears a null the destination already
            ! had. That is `%paste`'s documented rule and the one place it differs from `%append`,
            ! so the two calls below differ only in this final argument.
            call bits_copy_range(self%validity, (at - 1_int64)*w + 1_int64, &
                                 src%validity, (f - 1_int64)*w + 1_int64, n*w, .false.)
        else if (self%has_nulls) then
            ! Every source element is valid, so the destination range must end up all-valid too.
            ! Skipped entirely when this column has no bitmap: there is then nothing to clear,
            ! and materializing one just to write zeros into it would defeat the sparse design.
            call bits_clear_range(self%validity, (at - 1_int64)*w + 1_int64, (at - 1_int64 + n)*w)
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
            call parquet_string_column_delete_by_mask(self%str, elem_keep)
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
        call check_row_permutation(self, perm)
        call apply_row_permutation(self, perm, trusted=.false.)
    end procedure reindex
    !
    !> `reindex` without the O(n) range/duplicate scan, for a permutation the caller has already
    !! established is one. **The O(1) length check still runs**, because it guards a different
    !! invariant -- a column whose row count disagrees with the permutation -- and costs nothing.
    !!
    !! **This is public only because Fortran has no narrower visibility.** `parquet_column`'s
    !! components are private to this module, so `parquet_tables` -- a different module -- cannot
    !! reach them, and `%sort_by` needs exactly this to stop re-validating one permutation once per
    !! column (measured at a third of its total time on a wide table). It is internal plumbing, is
    !! deliberately absent from README.md's API overview, and a caller who passes a non-permutation
    !! gets silently duplicated and dropped rows. A library can refuse accidents; it cannot refuse
    !! deliberate misuse of a procedure documented as internal.
    !!
    !! `pf_permute(..., assume_valid=.true.)` routes here for the two container types, which is the
    !! only other supported way in.
    module procedure reindex_trusted
        if (size(perm, kind=int64) /= self%nrows) then
            error stop EP//"reindex_trusted: permutation length does not match the column's row count"
        end if
        call apply_row_permutation(self, perm, trusted=.true.)
    end procedure reindex_trusted
    !
    !> int32 form of `gather`; converts and delegates.
    module procedure gather_i32
        call self%gather_i64(int(idx, int64))
    end procedure gather_i32
    !
    !> Keeps the rows `idx` lists, in the order it lists them, changing the row count to match.
    !!
    !! Everything this needs already existed for `reindex` and `delete_by_mask`: `gather_storage`
    !! and `gather_validity` are both written against an index list of arbitrary length (that is how
    !! `delete_by_mask` uses them), and `expand_row_perm` turns a row-level list into the
    !! element-level one the string store takes without caring about its length either. So the only
    !! genuinely new thing here is the range check and setting `nrows`.
    !!
    !! **Repeats are permitted** -- see the interface's doc-comment for why the duplicate scan is
    !! deliberately absent rather than merely omitted.
    module procedure gather_i64
        integer(int64) :: m, k
        integer(int64), allocatable :: elem_idx(:)
        character(len=32) :: got, want
        m = size(idx, kind=int64)
        do k = 1_int64, m
            if (idx(k) < 1_int64 .or. idx(k) > self%nrows) then
                write(got, "(I0)") idx(k)
                write(want, "(I0)") self%nrows
                error stop EP//"gather: row index "//trim(got)//" is outside this column's 1.."// &
                    trim(want)//" rows"
            end if
        end do
        if (is_string_kind(self%kind)) then
            call expand_row_perm(idx, int(self%width, int64), elem_idx)
            ! Sliced, not passed whole: expand_row_perm allocates max(m*width, 1) -- the padded
            ! shape delete_by_mask also uses -- so for an EMPTY selection it hands back one
            ! uninitialized entry. reindex never sees that (a zero-row column returns before the
            ! call), but a gather legitimately can: %top_n(keys, 0) empties a table that has rows.
            call parquet_string_column_gather(self%str, elem_idx(1:m*int(self%width, int64)))
        else
            call gather_storage(self, idx)
        end if
        call gather_validity(self, idx)
        self%nrows = m
        if (is_temporal_kind(self%kind)) self%nulls_dirty = .true.
    end procedure gather_i64
    !
    !> The full length/range/duplicate check `reindex` runs before touching any storage, so that a
    !! bad permutation aborts with the column still intact rather than half rebuilt.
    subroutine check_row_permutation(self, perm)
        class(parquet_column), intent(in) :: self !! the column being reindexed.
        integer(int64), intent(in) :: perm(:)     !! the permutation to check.
        integer(int64) :: n, k, p, word
        integer(int8), allocatable :: seen(:)
        n = self%nrows
        if (size(perm, kind=int64) /= n) then
            error stop EP//"reindex: permutation length does not match the column's row count"
        end if
        if (n == 0_int64) return
        ! A BIT-PACKED seen-set, not a `logical` array: gfortran's default LOGICAL is 32 bits, so a
        ! plain seen(n) would spend 4 bytes per row to record one bit -- 59 MiB at 15.6M rows, and
        ! %sort_by used to pay it once per COLUMN. Measured at 3.9-4.4x faster per validation on a
        ! 15-20M-row column, purely from the scratch fitting in cache. The same shape appears in
        ! parquet_string_column%reindex (src/parquet_strings.f90) and in check_permutation
        ! (src/parquet_sorting_keys.f90); keep the three in step rather than adding a fourth.
        allocate(seen((n + 7_int64)/8_int64))
        seen = 0_int8
        do k = 1_int64, n
            p = perm(k)
            if (p < 1_int64 .or. p > n) error stop EP//"reindex: permutation entry out of range"
            word = (p - 1_int64)/8_int64 + 1_int64
            if (btest(seen(word), int(mod(p - 1_int64, 8_int64)))) then
                error stop EP//"reindex: permutation contains a duplicate index"
            end if
            seen(word) = ibset(seen(word), int(mod(p - 1_int64, 8_int64)))
        end do
    end subroutine check_row_permutation
    !
    !> Applies an already-checked row permutation to storage and validity. `trusted` selects which
    !! of the string store's two entry points is used, so the trust decision is made once here
    !! rather than being re-derived one level down.
    subroutine apply_row_permutation(self, perm, trusted)
        class(parquet_column), intent(inout) :: self !! the column.
        integer(int64), intent(in) :: perm(:)        !! source row index per destination row.
        logical, intent(in) :: trusted               !! .true. skips the string store's own scan too.
        integer(int64), allocatable :: elem_perm(:)
        if (self%nrows == 0_int64) return
        if (is_string_kind(self%kind)) then
            call expand_row_perm(perm, int(self%width, int64), elem_perm)
            ! The string store validates its own (expanded, element-level) permutation, so a trusted
            ! reindex has to say so here as well -- otherwise a string column keeps paying the very
            ! scan this path exists to skip, over width*nrows elements rather than nrows.
            if (trusted) then
                call parquet_string_column_reindex_trusted(self%str, elem_perm)
            else
                call parquet_string_column_reindex(self%str, elem_perm)
            end if
        else
            call gather_storage(self, perm)
        end if
        call gather_validity(self, perm)
        if (is_temporal_kind(self%kind)) self%nulls_dirty = .true.
    end subroutine apply_row_permutation
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
            call parquet_string_column_append_nulls(self%str, n*int(self%width, int64))
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
        integer(int64) :: n, k, e, w, nbits, dst_word, blk, nblk, base, nb, b, row
        integer(int64), allocatable :: new_map(:)
        if (.not. self%has_nulls) return
        if (.not. allocated(self%validity)) return
        if (is_string_kind(self%kind) .or. is_temporal_kind(self%kind)) return
        n = size(idx, kind=int64)
        w = int(self%width, int64)
        nbits = n*w
        allocate(new_map(max(blocks_for(nbits), 1_int64)))
        new_map = 0_int64
        ! A permutation, so the SOURCE rows are scattered and only the destination is sequential.
        ! That rules out a run copy -- but each destination WORD can still be built whole in a
        ! register and stored once, instead of a `bit_set` call per set bit. Walking by destination
        ! word rather than by row is what makes the bit position `b` immediately available, with no
        ! divide in the inner loop.
        nblk = blocks_for(nbits)
        if (w == 1_int64) then
            ! The overwhelmingly common case, and worth its own loop: with one element per row the
            ! destination element index IS the row index, so the row/element split below -- two
            ! integer divisions per element -- disappears entirely.
            do blk = 1_int64, nblk
                dst_word = 0_int64
                base = (blk - 1_int64)*BITS_PER_BLOCK
                nb = min(BITS_PER_BLOCK, nbits - base)
                do b = 0_int64, nb - 1_int64
                    if (bit_test(self%validity, idx(base + b + 1_int64))) then
                        dst_word = ibset(dst_word, int(b))
                    end if
                end do
                new_map(blk) = dst_word
            end do
        else
            do blk = 1_int64, nblk
                dst_word = 0_int64
                base = (blk - 1_int64)*BITS_PER_BLOCK
                nb = min(BITS_PER_BLOCK, nbits - base)
                do b = 0_int64, nb - 1_int64
                    k = base + b                          ! 0-based destination element
                    row = k/w + 1_int64
                    e = k - (row - 1_int64)*w + 1_int64
                    if (bit_test(self%validity, (idx(row) - 1_int64)*w + e)) then
                        dst_word = ibset(dst_word, int(b))
                    end if
                end do
                new_map(blk) = dst_word
            end do
        end if
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
