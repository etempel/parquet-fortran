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
        ! `init` STILL REFUSES the container kinds, and this refusal is permanent rather than a
        ! placeholder -- it allocates a kind's storage from a row count, and `init(PK_LIST, 100)`
        ! would have to mean "a hundred rows of what?", which has no answer. A container column's
        ! contents come from an object the caller has already built, so the entry point is
        ! `adopt_container` (feature_map_list_struct.md, Phase 1).
        !
        ! `self%kind` therefore has exactly FIVE writers: this one (never a container kind),
        ! `clear` (writes PK_NONE), `move_from` (copies whatever the source held), the sixteen
        ! `adopt_*` array specifics (each hard-codes one non-container kind), and
        ! `adopt_container` -- the only one that can write PK_LIST/PK_MAP/PK_STRUCT.
        if (parquet_kind_is_container(kind)) then
            error stop EP//"init: a container column is built with adopt_container, not init"
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
        ! BEFORE the rows, and unconditionally: a zero-row column is otherwise live -- a real kind,
        ! a real width, answering %length() -- with no storage array allocated at all, because
        ! `ensure_capacity` returns early on `0 <= 0`. Everything that then names the storage
        ! references an unallocated allocatable, which is non-conforming however empty the section
        ! is; see allocate_empty_storage's own doc-comment for what does and what reports it.
        ! Costs nothing when `nrows > 0` -- grow_rows reallocates over the zero-sized array the
        ! same way it would over an unallocated one.
        call allocate_empty_storage(self)
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
    !> Takes ownership of a container column, making this a PK_LIST/PK_MAP/PK_STRUCT column.
    !!
    !! The seventeenth `adopt_*` sibling and, while `init` keeps refusing the container kinds, the
    !! ONLY writer of a container kind into `self%kind` besides `move_from` (which can only copy a
    !! kind some earlier adopt already wrote). See `init`'s own comment for the full list.
    module procedure adopt_container
        if (.not. allocated(container)) error stop EP//"adopt_container: the container is not allocated"
        if (.not. parquet_kind_is_container(container%kindof())) then
            ! GCOVR_EXCL_START -- not reachable from inside the library: `kindof` is deferred, and
            ! each of the three concrete containers answers its own PK_LIST/PK_MAP/PK_STRUCT and
            ! nothing else. The guard is for an extension of the abstract type written OUTSIDE it,
            ! where the alternative is a column whose kind and whose contents disagree for good.
            error stop EP//"adopt_container: the container reports a kind that is not a container kind"
            ! GCOVR_EXCL_STOP
        end if
        call self%clear()
        ! Taken from the container itself rather than from an argument, so the column cannot end up
        ! claiming a kind or a row count the thing it holds disagrees with.
        self%kind = container%kindof()
        self%nrows = container%nrows()
        ! A container row holds a variable number of elements, which is exactly what `width` cannot
        ! express -- so it stays 1 and no *_VEC guard anywhere can be fooled into striding.
        self%width = 1_int32
        ! The adopted object IS the capacity, as for the array kinds: leaving `cap` at 0 would
        ! break the cap >= nrows invariant that %capacity and ensure_capacity both read.
        self%cap = self%nrows
        ! Row nullness lives INSIDE the container, exactly as it lives inside the element for the
        ! temporal kinds -- so this column's own bitmap stays unallocated and `has_nulls` .false.
        ! Materializing one here would create a second, silently divergent answer to "is row i
        ! null?".
        call move_alloc(container, self%container)
    end procedure adopt_container
    !
    module procedure deep_copy
        class(parquet_container_column), allocatable :: cc
        call out%clear()
        if (self%kind == PK_NONE) return
        ! The container kinds fork BEFORE `init`, not inside it: `init` refuses them (and must keep
        ! doing so -- see its own comment), so routing a container through the ordinary
        ! init + copy_storage path would abort on the DESTINATION. The container copies itself
        ! whole instead, because only its dynamic type knows what to allocate.
        if (parquet_kind_is_container(self%kind)) then
            call self%container%clone_into(cc)
            call out%adopt_container(cc)
            if (allocated(self%unit)) out%unit = self%unit
            return
        end if
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
        integer(int64) :: old, n, w
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
        ! Row nullness travelled inside the container, which append_from has already merged. This
        ! column's own bitmap is never allocated for a container kind, so falling through would
        ! ask `other%has_nulls` -- permanently .false. here -- and answer "nothing to carry" for
        ! the right reason by accident. Saying so explicitly keeps that from becoming load-bearing.
        if (parquet_kind_is_container(self%kind)) return
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
        integer(int64) :: old
        if (n < 0_int64) error stop EP//"append_nulls: negative row count"
        if (n == 0_int64) return
        if (self%kind == PK_NONE) error stop EP//"append_nulls: column has no kind assigned"
        old = self%nrows
        call grow_rows(self, n)
        if (is_string_kind(self%kind)) return
        ! Row nullness lives inside the container, which `grow_rows` has just appended null rows
        ! to. Falling through would set bits in THIS column's bitmap as well, giving two answers to
        ! "is row i null?" with nothing to keep them in step.
        if (parquet_kind_is_container(self%kind)) return
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
        integer(int64) :: n, f, w
        if (self%kind /= src%kind) error stop EP//"paste: column kinds differ"
        if (self%width /= src%width) error stop EP//"paste: column widths differ"
        if (is_string_kind(self%kind)) then
            error stop EP//"paste: the string kinds cannot be overwritten in place; use append"
        end if
        ! Refused HERE rather than in paste_storage, so the message names the operation the caller
        ! actually asked for. A container column has no fixed-width row slots to overwrite: row i's
        ! element count is data, so replacing it moves every following row's payload. Presizing
        ! plus %paste is therefore unavailable for containers and `reserve` + append is the shape
        ! to use -- which is why `reserve_rows` is a deferred binding at all.
        if (parquet_kind_is_container(self%kind)) then
            error stop EP//"paste: a container column cannot be overwritten in place; use append"
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
        call self%gather_i64(int(idx, int64), valid=valid, threads=threads)
    end procedure gather_i32
    !
    !> Keeps the rows `idx` lists, in the order it lists them, changing the row count to match.
    !!
    !! Since stage 4 of `feature_join.md` this is `gather_build` -- the one implementation of the
    !! copy, the bitmap rebuild and the row split that `gather_from` also runs -- applied to the
    !! column's own rows into a fresh column, which `move_from` then hands back over the original
    !! in O(1). What stays here is what differs between the two entry points: the range check,
    !! whose message names this binding, and the container kinds, which rebuild themselves in
    !! place and cannot be copied from outside -- nor null-filled by a mask, since their row
    !! nullness lives inside the container, which is why `parquet_table%join` refuses the two
    !! `how=` values that could ask for it before anything reaches here (`feature_risks.md`
    !! Risk-188: a container column is carried across a join by exactly this route).
    !!
    !! **Repeats are permitted** -- see the interface's doc-comment for why the duplicate scan is
    !! deliberately absent rather than merely omitted.
    module procedure gather_i64
        type(parquet_column) :: fresh
        integer(int64) :: m, k
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
        call check_gather_mask(valid, m, "gather")
        if (parquet_kind_is_container(self%kind)) then
            if (present(valid)) then
                ! Not reachable through the table layer (see the doc-comment above); kept because
                ! a mask silently ignored would leave a container whose scalar siblings were
                ! null-filled and it was not -- a misaligned table with every row count right.
                if (.not. all(valid)) then ! GCOVR_EXCL_START
                    error stop EP//"gather: a container column cannot be null-filled by a mask"
                end if ! GCOVR_EXCL_STOP
            end if
            call self%container%gather_rows(idx)
            self%nrows = m
            self%cap = m
            return
        end if
        call gather_build(fresh, self, idx, valid, threads)
        call self%move_from(fresh)
    end procedure gather_i64
    !
    !> int32 form of `gather_from`; converts and delegates.
    module procedure gather_from_i32
        call self%gather_from_i64(src, int(idx, int64), valid=valid, threads=threads)
    end procedure gather_from_i32
    !
    !> Builds this column from the rows of `src` that `idx` lists -- see the interface for the
    !! contract. The checks live here, in the order that leaves the destination untouched by a
    !! refusal; the work is `gather_build`'s, which `gather` shares.
    module procedure gather_from_i64
        integer(int64) :: m, k
        character(len=32) :: got, want
        m = size(idx, kind=int64)
        if (parquet_kind_is_container(src%kind)) then
            error stop EP//"gather_from: a container column is gathered in place with %gather, "// &
                "not from another column"
        end if
        call check_gather_mask(valid, m, "gather_from")
        ! Serial deliberately: which index is out of range must not depend on which thread noticed.
        do k = 1_int64, m
            if (idx(k) < 1_int64 .or. idx(k) > src%nrows) then
                write(got, "(I0)") idx(k)
                write(want, "(I0)") src%nrows
                error stop EP//"gather_from: row index "//trim(got)//" is outside the source "// &
                    "column's 1.."//trim(want)//" rows"
            end if
        end do
        call gather_build(self, src, idx, valid, threads)
    end procedure gather_from_i64
    !
    !> Aborts unless `valid`, when present, has one entry per destination row.
    subroutine check_gather_mask(valid, m, proc)
        logical, intent(in), optional :: valid(:) !! the mask, or absent.
        integer(int64), intent(in) :: m           !! destination rows, i.e. the index list's length.
        character(len=*), intent(in) :: proc      !! calling binding, for the message.
        character(len=32) :: got, want
        if (.not. present(valid)) return
        if (size(valid, kind=int64) == m) return
        write(got, "(I0)") size(valid, kind=int64)
        write(want, "(I0)") m
        error stop EP//proc//": mask has "//trim(got)//" entries but the index list names "// &
            trim(want)//" rows"
    end subroutine check_gather_mask
    !
    !> The one gather: `dst` becomes `src`'s rows `idx` lists, with `src`'s nulls carried and the
    !! mask's nulls added, on one thread or on `threads`. Every check has been made by the caller,
    !! and `src` is not a container kind. `dst` is cleared first, so it may be a fresh column or a
    !! column being reused; it must be a different object from `src`.
    !!
    !! **The string kinds go to their store whole**: `parquet_string_column_gather_from` is the
    !! same one-pass rebuild at the element level (a `PK_STRING_VEC` row is `width` consecutive
    !! elements, expanded by `expand_row_perm`/`expand_row_mask` exactly as `gather` always did),
    !! and it threads by its own rule. Every other kind is copied here, one range of destination
    !! rows per thread, and its validity bitmap is written a whole word at a time from the
    !! source's bits at `idx` OR-ed with the mask -- so the add-only rule holds by construction
    !! (`feature_risks.md` Risk-182).
    !!
    !! **Two threads never write one bitmap word.** `gather_ranges` cuts the rows so that every
    !! range boundary is a multiple of `BITS_PER_BLOCK / gcd(width, BITS_PER_BLOCK)` rows -- the
    !! period after which `(row-1)*width` is again a multiple of the word size -- so each range
    !! owns whole words and the last range owns the ragged tail alone. That is the reasoning
    !! `paste_row_group_safely` (`src/parquet_tables_read.f90`) applies to a row-group boundary it
    !! cannot move, done the cheaper way round here, where the boundary is free to choose
    !! (`feature_risks.md` Risk-64: disjoint rows are not disjoint bits).
    subroutine gather_build(dst, src, idx, valid, threads)
        type(parquet_column), intent(inout) :: dst  !! the destination; cleared first.
        type(parquet_column), intent(in) :: src     !! the source, never written.
        integer(int64), intent(in) :: idx(:)        !! 1-based source row per destination row, range-checked.
        logical, intent(in), optional :: valid(:)   !! per destination row, `size(idx)` entries when present.
        integer, intent(in), optional :: threads    !! team to split the rows across; absent or 1 is serial.
        integer(int64) :: m, w
        integer(int64), allocatable :: elem_idx(:), lo(:), hi(:)
        logical, allocatable :: elem_valid(:)
        logical :: masked, bitmap
        integer :: nt, tix
        !
        m = size(idx, kind=int64)
        w = int(src%width, int64)
        call dst%clear()
        if (src%kind == PK_NONE) return
        ! Computed once, serially: an all-true mask changes nothing and must cost no bitmap -- the
        ! common case for a join whose every row matched (`set_validity` returns early on the same
        ! test).
        masked = .false.
        if (present(valid)) masked = .not. all(valid)
        if (is_string_kind(src%kind)) then
            call init_like(dst, src, 0_int64)
            call expand_row_perm(idx, w, elem_idx)
            ! Sliced, not passed whole: expand_row_perm allocates max(m*width, 1) -- the padded
            ! shape delete_by_mask also uses -- so for an EMPTY selection it hands back one
            ! uninitialized entry, and %top_n(keys, 0) legitimately empties a table that has rows.
            if (masked) then
                call expand_row_mask(valid, w, elem_valid)
                call parquet_string_column_gather_from(dst%str, src%str, elem_idx(1:m*w), &
                    valid=elem_valid(1:m*w), threads=threads)
            else
                call parquet_string_column_gather_from(dst%str, src%str, elem_idx(1:m*w), threads=threads)
            end if
            dst%nrows = m
            dst%cap = m
            return
        end if
        ! `init` at the final row count IS the exact-fit allocation: the capacity starts at zero,
        ! so the one growth it performs allocates exactly `m` rows (`ensure_capacity`).
        call init_like(dst, src, m)
        nt = 1
        if (present(threads)) nt = gather_team(m, w, threads)
        call gather_ranges(m, w, nt, lo, hi)
        ! A bitmap only when there is something to put in it: the source's own map (a null-free
        ! source has none, R2) or a mask that nulls at least one row. The temporal kinds carry the
        ! null inside the element and take the mask in gather_storage_from instead.
        bitmap = .not. is_temporal_kind(src%kind) .and. &
            ((src%has_nulls .and. allocated(src%validity)) .or. masked)
        if (bitmap) then
            allocate(dst%validity(max(blocks_for(m*w), 1_int64)))
            dst%validity = 0_int64
            dst%has_nulls = .true.
        end if
        ! Each range writes its own rows of the storage and its own whole words of the bitmap;
        ! `src`, `idx` and `valid` are only read.
        !$omp parallel do default(shared) private(tix) schedule(static) num_threads(nt) if (nt > 1)
        do tix = 1, nt
            call gather_storage_from(dst, src, idx, lo(tix), hi(tix), valid)
            if (bitmap) call gather_bits_from(dst, src, idx, valid, masked, lo(tix), hi(tix))
        end do
        !$omp end parallel do
        if (is_temporal_kind(src%kind)) dst%nulls_dirty = .true.
    end subroutine gather_build
    !
    !> The team a gather of `m` rows, `w` wide, opens when asked for `threads`: the request
    !! clamped like every other thread count this library resolves -- an explicit one included, as
    !! the sort engine's is (api-conventions.md); a count arriving from `table_colwork` is already
    !! clamped, so that is a no-op there -- and lowered by `gather_ranges` to what the rows can
    !! occupy in whole validity words. The one rule, which `parquet_debug_column_gather_threads`
    !! reports for the tests.
    integer function gather_team(m, w, threads) result(nt)
        use parquet_settings_base, only : parquet_clamp_to_affinity
        integer(int64), intent(in) :: m  !! destination rows.
        integer(int64), intent(in) :: w  !! elements per row.
        integer, intent(in) :: threads   !! the count asked for.
        integer(int64), allocatable :: lo(:), hi(:)
        nt = 1
        if (threads > 1) nt = parquet_clamp_to_affinity(threads, "column gathering")
        call gather_ranges(m, w, nt, lo, hi)
    end function gather_team
    !
    module procedure parquet_debug_column_gather_threads
        n = gather_team(nrows, int(max(width, 1_int32), int64), threads)
    end procedure parquet_debug_column_gather_threads
    !
    !> `dst%init` with `src`'s kind, width and unit, at `nrows` rows.
    subroutine init_like(dst, src, nrows)
        type(parquet_column), intent(inout) :: dst !! the column to initialise.
        type(parquet_column), intent(in) :: src    !! whose kind, width and unit to take.
        integer(int64), intent(in) :: nrows        !! rows to allocate.
        if (allocated(src%unit)) then
            call dst%init(src%kind, nrows, src%width, src%unit)
        else
            call dst%init(src%kind, nrows, src%width)
        end if
    end subroutine init_like
    !
    !> Cuts destination rows `1 .. m` into `nt` contiguous ranges whose boundaries fall on
    !! validity-WORD boundaries, lowering `nt` when there are not enough whole words to go round.
    !!
    !! With `w` elements per row, `(row-1)*w` is a multiple of `BITS_PER_BLOCK` every
    !! `BITS_PER_BLOCK/gcd(w, BITS_PER_BLOCK)` rows, and that is the period every boundary is a
    !! multiple of -- so each range owns whole words of the bitmap and the last one owns the tail.
    !! The whole periods are dealt out as evenly as integer division allows, the remainder one
    !! extra to the first ranges, exactly as `parquet_strings`' `thread_row_ranges` deals out its
    !! validity bytes. A range is never empty: `nt` is lowered to the number of whole periods
    !! first, and to 1 when there is not even one.
    subroutine gather_ranges(m, w, nt, lo, hi)
        integer(int64), intent(in) :: m                   !! destination rows.
        integer(int64), intent(in) :: w                   !! elements per row.
        integer, intent(inout) :: nt                      !! ranges wanted, in; ranges cut, out.
        integer(int64), allocatable, intent(out) :: lo(:) !! first row of each range.
        integer(int64), allocatable, intent(out) :: hi(:) !! last row of each range.
        integer(int64) :: period, nper, per, extra, cur, take
        integer :: k
        period = BITS_PER_BLOCK/gcd_int64(w, BITS_PER_BLOCK)
        nper = m/period
        if (nt > 1 .and. int(nt, int64) > nper) nt = int(max(nper, 1_int64))
        if (nt < 1) nt = 1
        allocate(lo(nt), hi(nt))
        if (nt == 1) then
            lo(1) = 1_int64
            hi(1) = m
            return
        end if
        per = nper/int(nt, int64)
        extra = mod(nper, int(nt, int64))
        cur = 1_int64
        do k = 1, nt
            take = per
            if (int(k, int64) <= extra) take = take + 1_int64
            lo(k) = cur
            hi(k) = cur + take*period - 1_int64
            cur = hi(k) + 1_int64
        end do
        ! The last range runs to m, whole periods or not: the rows past the last whole period are
        ! its own, and the partial word they end in is touched by no other range.
        hi(nt) = m
    end subroutine gather_ranges
    !
    !> Greatest common divisor, for `gather_ranges`' period. Both arguments positive.
    pure function gcd_int64(a, b) result(g)
        integer(int64), intent(in) :: a !! first value.
        integer(int64), intent(in) :: b !! second value.
        integer(int64) :: g             !! their greatest common divisor.
        integer(int64) :: x, y, t
        x = a
        y = b
        do while (y /= 0_int64)
            t = mod(x, y)
            x = y
            y = t
        end do
        g = x
    end function gcd_int64
    !
    !> Writes the validity words of destination rows `lo .. hi`: bit `e` of row `k` is set (null)
    !! when the source's bit for element `e` of row `idx(k)` is set, or when the mask marks row
    !! `k` `.false.`. Every word the range touches is built whole in a register and stored once,
    !! by plain assignment -- the destination map was zeroed at allocation and no other range
    !! writes these words (`gather_ranges`). The same walk as `gather_validity`, over a range.
    subroutine gather_bits_from(dst, src, idx, valid, masked, lo, hi)
        type(parquet_column), intent(inout) :: dst !! the destination, its bitmap allocated.
        type(parquet_column), intent(in) :: src    !! the source.
        integer(int64), intent(in) :: idx(:)       !! source row per destination row.
        logical, intent(in), optional :: valid(:)  !! the mask; read only when `masked`.
        logical, intent(in) :: masked              !! `valid` is present and holds a .false.
        integer(int64), intent(in) :: lo           !! first destination row of the range.
        integer(int64), intent(in) :: hi           !! last destination row of the range.
        integer(int64) :: w, first, last, blk, base, nb, b, k, row, e, word
        logical :: src_map
        if (hi < lo) return
        w = int(dst%width, int64)
        src_map = src%has_nulls .and. allocated(src%validity)
        first = ((lo - 1_int64)*w)/BITS_PER_BLOCK + 1_int64
        last = (hi*w - 1_int64)/BITS_PER_BLOCK + 1_int64
        do blk = first, last
            word = 0_int64
            base = (blk - 1_int64)*BITS_PER_BLOCK          ! 0-based element index of this word's bit 0
            nb = min(BITS_PER_BLOCK, hi*w - base)           ! bits of this word inside the range
            if (w == 1_int64) then
                ! The common case, and worth its own loop: the element index IS the row index, so
                ! the row/element split below -- two integer divisions per element -- disappears.
                do b = 0_int64, nb - 1_int64
                    row = base + b + 1_int64
                    if (src_map) then
                        if (bit_test(src%validity, idx(row))) word = ibset(word, int(b))
                    end if
                    if (masked) then
                        if (.not. valid(row)) word = ibset(word, int(b))
                    end if
                end do
            else
                do b = 0_int64, nb - 1_int64
                    k = base + b
                    row = k/w + 1_int64
                    e = k - (row - 1_int64)*w + 1_int64
                    if (src_map) then
                        if (bit_test(src%validity, (idx(row) - 1_int64)*w + e)) word = ibset(word, int(b))
                    end if
                    if (masked) then
                        if (.not. valid(row)) word = ibset(word, int(b))
                    end if
                end do
            end if
            dst%validity(blk) = word
        end do
    end subroutine gather_bits_from
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
