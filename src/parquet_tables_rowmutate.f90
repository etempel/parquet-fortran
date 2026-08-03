!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> Mutation that changes a `parquet_table`'s ROW SET: `%filter_rows`, `%sort_by`,
!! `%delete_rows`, `%truncate`, `%append` and `%append_null_rows`.
!!
!! **Everything in this file detaches the table when it actually changes the row set** (F-mut-7),
!! and that is why it is a separate file from `parquet_tables_mutate.f90`. Once the row set has
!! changed, a column still sitting in the file can never be lined up with the columns already in
!! memory again -- so rather than hand back silently misaligned data later, the table records that
!! it has left its file behind and every later read from that file is a named error.
!!
!! **A call that changes nothing changes nothing at all** -- it does not touch a column, does not
!! invalidate a `%col` pointer, and does not detach. Detaching costs the caller their file, so it
!! is only ever paid for when something actually required it. Five calls reach this file with
!! nothing to do, all decided by the caller's own arguments (`%truncate(n)` with `n >= %nrows()`,
!! `%filter_rows` with an all-`.true.` mask, `%delete_rows` with no indices, `%append` of a
!! zero-row table, `%append_null_rows(0)`), plus one decided by the data (`%sort_by` whose
!! permutation moves no row, which includes every table of fewer than two rows). Each returns
!! early, AFTER its own validation -- a no-op still rejects a bad argument.
!!
!! **Two rules hold this together, and a new operation added here must follow both:**
!!
!! 1. **Validate everything, THEN mutate everything.** A mutation that aborts halfway leaves some
!!    columns changed and some not -- a state with no diagnostic and no way back. So every check
!!    (mask length, index bounds, key names and kinds, the appended table's column set) runs
!!    before the first column is touched.
!! 2. **Detach LAST.** If a mutation does fail, the table should still be attached and
!!    diagnosable rather than detached and half-changed.
!!
!! **A column that has not been read is skipped, not an obstacle** (`table_mutable_column`). That
!! is the approved policy: requiring every column to be resident first would force a lazy table to
!! read everything it has before it could drop a single row, which is exactly the memory cost
!! laziness and the slice regime exist to avoid. The skipped column is then unreadable for good --
!! detaching sees to that -- so a caller who wants it must `%prefetch` it BEFORE mutating, and the
!! detach guard is what says so if they did not.
!!
!! `%append` is deliberately split into a public wrapper and a private worker even though there
!! is no lock yet: the lock arrives in the next milestone and goes in the wrapper alone, and an
!! OpenMP simple lock is not recursive, so the internal callers must already bypass it.
submodule (parquet_tables) parquet_tables_rowmutate
    implicit none
    !
contains
    !
    ! ---- the shared machinery -------------------------------------------------------------
    !
    module procedure table_detach
        ! "Detached" means "had a file and can no longer read from it". A table built in memory
        ! never had one, so growing or reordering it detaches nothing and %is_detached must keep
        ! answering .false. -- otherwise every from-scratch table would report itself detached the
        ! moment it was filled. The flag is only ever SET here, never cleared, so a table that has
        ! already detached stays detached through any later mutation.
        if (self%cache%file_backed) then
            ! The reader can never be read from again, so let it go now rather than at
            ! finalization: it holds an open file and whatever Arrow state it cached, and
            ! releasing it here also turns any missed read-after-detach into a clean guard failure
            ! instead of a read that quietly succeeds through a still-live reader.
            if (allocated(self%cache%reader)) then
                call parquet_close_reader(self%cache%reader)
                deallocate(self%cache%reader)
            end if
            self%cache%file_backed = .false.
            self%detached = .true.
        end if
        ! The surviving rows are no longer a contiguous range of FILE rows, so the slice scope
        ! stops meaning anything. A detached table is simply an in-memory table whose rows are its
        ! own, which is what every path that tests file_backed already assumes.
        self%regime = REGIME_FULL
        self%row_lo = 1_int64
        self%row_hi = self%row_count
        if (allocated(self%cache%rg_bounds)) deallocate(self%cache%rg_bounds)
    end procedure table_detach
    !
    module procedure table_mutable_column
        ok = self%cache%cols(idx)%supported .and. self%cache%cols(idx)%residency == RES_FULL
    end procedure table_mutable_column
    !
    module procedure table_apply_keep
        integer :: i
        !
        ! Keeping every row removes none, so there is nothing to rewrite and nothing to detach
        ! for: `delete_by_mask` would reallocate every column's storage to the same contents,
        ! invalidating every %col pointer, and the table would lose its file for a call that did
        ! not change a single row. This covers %filter_rows with an all-.true. mask,
        ! %delete_rows() with no indices, and a filter of a zero-row table.
        if (all(keep)) return
        do i = 1, self%cache%ncols
            if (.not. table_mutable_column(self, i)) cycle
            call self%cache%cols(i)%values%delete_by_mask(keep)
        end do
        self%row_count = count(keep, kind=int64)
        call table_detach(self)
    end procedure table_apply_keep
    !
    ! ---- filter / delete / truncate ---------------------------------------------------------
    !
    module procedure table_filter_rows
        character(len=32) :: got, want
        !
        call table_check_open(self, "filter_rows")
        if (size(keep, kind=int64) /= self%row_count) then
            write(got, "(I0)") size(keep, kind=int64)
            write(want, "(I0)") self%row_count
            error stop EP // "filter_rows: the mask has " // trim(got) // " entries but the " // &
                "table has " // trim(want) // " rows"
        end if
        call table_apply_keep(self, keep, "filter_rows")
    end procedure table_filter_rows
    !
    module procedure table_delete_rows_i32
        call self%delete_rows(int(indices, int64))
    end procedure table_delete_rows_i32
    !
    module procedure table_delete_rows_i64
        logical, allocatable :: keep(:)
        integer(int64) :: k
        character(len=32) :: got, want
        !
        call table_check_open(self, "delete_rows")
        ! Every index is checked before any is used, so a bad one aborts with the table intact.
        do k = 1_int64, size(indices, kind=int64)
            if (indices(k) < 1_int64 .or. indices(k) > self%row_count) then
                write(got, "(I0)") indices(k)
                write(want, "(I0)") self%row_count
                error stop EP // "delete_rows: row index " // trim(got) // " is outside this " // &
                    "table's 1.." // trim(want) // " rows"
            end if
        end do
        allocate(keep(max(self%row_count, 1_int64)))
        keep = .true.
        ! A repeat is harmless by construction: clearing an already-cleared entry changes nothing,
        ! so naming a row twice removes it once.
        do k = 1_int64, size(indices, kind=int64)
            keep(indices(k)) = .false.
        end do
        call table_apply_keep(self, keep(1:self%row_count), "delete_rows")
    end procedure table_delete_rows_i64
    !
    module procedure table_truncate_i32
        call self%truncate(int(n, int64))
    end procedure table_truncate_i32
    !
    module procedure table_truncate_i64
        logical, allocatable :: keep(:)
        character(len=32) :: got
        !
        call table_check_open(self, "truncate")
        if (n < 0_int64) then
            write(got, "(I0)") n
            error stop EP // "truncate: cannot keep " // trim(got) // " rows"
        end if
        ! Asking to keep more rows than there are is a no-op rather than an error: "give me the
        ! first 1000" is a reasonable thing to say about a table that may have fewer.
        if (n >= self%row_count) return
        allocate(keep(max(self%row_count, 1_int64)))
        keep = .false.
        if (n > 0_int64) keep(1_int64:n) = .true.
        call table_apply_keep(self, keep(1:self%row_count), "truncate")
    end procedure table_truncate_i64
    !
    ! ---- sort -------------------------------------------------------------------------------
    !
    module procedure table_sort_by
        integer(int64), allocatable :: perm(:)
        integer :: i
        character(len=32) :: got, want
        !
        call table_check_open(self, "sort_by")
        if (size(keys) < 1) error stop EP // "sort_by: no sort key was given"
        if (present(descending)) then
            if (size(descending) /= size(keys)) then
                write(got, "(I0)") size(descending)
                write(want, "(I0)") size(keys)
                error stop EP // "sort_by: descending= has " // trim(got) // " entries but " // &
                    trim(want) // " keys were given; it takes one entry per key"
            end if
        end if
        if (present(nulls_first)) then
            if (size(nulls_first) /= size(keys)) then
                write(got, "(I0)") size(nulls_first)
                write(want, "(I0)") size(keys)
                error stop EP // "sort_by: nulls_first= has " // trim(got) // " entries but " // &
                    trim(want) // " keys were given; it takes one entry per key"
            end if
        end if
        ! Builds (and so validates every key) before a single column is touched.
        call table_build_sort_permutation(self, keys, descending, nulls_first, perm)
        ! A permutation that moves no row leaves the table exactly as it was, so it costs neither
        ! a reindex (which reallocates every column) nor the file. Unlike the other early returns
        ! in this file this one depends on the DATA, not on the arguments: sorting an
        ! already-ordered column keeps the table attached, sorting the same column after an edit
        ! may not. Fewer than two rows always lands here.
        if (permutation_moves_nothing(perm(1:self%row_count))) return
        do i = 1, self%cache%ncols
            if (.not. table_mutable_column(self, i)) cycle
            call self%cache%cols(i)%values%reindex(perm(1:self%row_count))
        end do
        call table_detach(self)
    end procedure table_sort_by
    !
    !> .true. when a sort permutation sends every row to its own position, i.e. the sort is a
    !! no-op. A zero- or one-row table always answers .true.
    logical function permutation_moves_nothing(perm) result(still)
        integer(int64), intent(in) :: perm(:) !! the permutation, one destination row per source row.
        integer(int64) :: k
        !
        still = .true.
        do k = 1_int64, size(perm, kind=int64)
            if (perm(k) /= k) then
                still = .false.
                return
            end if
        end do
    end function permutation_moves_nothing
    !
    ! ---- append -----------------------------------------------------------------------------
    !
    module procedure table_append_table
        call append_table_worker(self, other)
    end procedure table_append_table
    !
    !> The unlocked body of `%append`.
    !!
    !! Split out from the binding even though nothing takes a lock yet: the next milestone adds
    !! the table's lock to `table_append_table` alone, and an OpenMP simple lock is not recursive,
    !! so every INTERNAL caller must already bypass the public entry point. `%append(row)` is that
    !! caller today. Getting this shape wrong shows up as a self-deadlock rather than a compile
    !! error, so it is worth having before the lock exists rather than after.
    subroutine append_table_worker(self, other)
        class(parquet_table), intent(inout) :: self !! the table to grow.
        class(parquet_table), intent(in) :: other   !! the table whose rows are appended.
        integer :: i, j
        integer(int64) :: added
        !
        call table_check_open(self, "append")
        call table_check_open(other, "append")
        ! Two validation passes over `other`'s columns, both before anything is appended: every
        ! column it has must exist here and be compatible.
        do j = 1, other%cache%ncols
            if (.not. table_mutable_column(other, j)) cycle
            i = table_find(self, other%cache%cols(j)%name)
            if (i == 0) call append_unknown_column(self, other%cache%cols(j)%name)
            if (.not. table_mutable_column(self, i)) cycle
            call append_check_compatible(self, i, other, j)
        end do
        added = other%row_count
        ! Appending no rows adds nothing, so the table keeps its columns' storage and its file --
        ! but only after the compatibility checks above have run, so an incompatible zero-row
        ! table is still refused rather than quietly accepted.
        if (added == 0_int64) return
        do i = 1, self%cache%ncols
            if (.not. table_mutable_column(self, i)) cycle
            j = cache_find(other%cache, self%cache%cols(i)%name)
            if (j == 0) then
                ! M1's default fill: a column this table has and `other` does not gets nulls for
                ! the appended rows, rather than the append being refused.
                call self%cache%cols(i)%values%append_nulls(added)
            else
                call self%cache%cols(i)%values%append(other%cache%cols(j)%values)
            end if
        end do
        self%row_count = self%row_count + added
        call table_detach(self)
    end subroutine append_table_worker
    !
    module procedure table_append_row
        type(parquet_table) :: one
        !
        call table_check_open(self, "append")
        call append_row_as_table(self, r, one)
        ! The worker, not self%append: see append_table_worker's own note on why an internal
        ! caller must not re-enter the public entry point.
        call append_table_worker(self, one)
    end procedure table_append_row
    !
    module procedure table_append_null_rows_i32
        call self%append_null_rows(int(n, int64))
    end procedure table_append_null_rows_i32
    !
    module procedure table_append_null_rows_i64
        integer :: i
        character(len=32) :: got
        !
        call table_check_open(self, "append_null_rows")
        if (n < 0_int64) then
            write(got, "(I0)") n
            error stop EP // "append_null_rows: cannot append " // trim(got) // " rows"
        end if
        ! Appending no rows leaves the row set exactly as it was, so it does not detach: the
        ! table keeps its file, and a later touch can still read a column it has not read yet.
        if (n == 0_int64) return
        do i = 1, self%cache%ncols
            if (.not. table_mutable_column(self, i)) cycle
            call self%cache%cols(i)%values%append_nulls(n)
        end do
        self%row_count = self%row_count + n
        call table_detach(self)
    end procedure table_append_null_rows_i64
    !
    !> Reports a column `other` has and this table does not. Never silently dropped: losing a
    !! column's values because the destination happened not to have that column is the kind of
    !! quiet data loss an append should refuse to perform.
    subroutine append_unknown_column(self, name)
        class(parquet_table), intent(in) :: self !! the destination table.
        character(len=*), intent(in) :: name     !! the offending column name.
        character(len=:), allocatable :: sfx
        !
        call table_context_suffix(self%cache, name, sfx)
        error stop EP // "append: the appended table has a column this table does not; add it " // &
            "first (%add_column) or drop it there, but it will not be silently dropped" // sfx
    end subroutine append_unknown_column
    !
    !> error stops unless two columns can be concatenated: same kind, same width, same unit.
    !!
    !! A kind mismatch is a hard error rather than a silent widen. The widening rule elsewhere in
    !! this type is a READ rule -- it chooses the type a caller receives -- whereas an append
    !! would have to change the destination column's stored kind behind the caller's back.
    !! `%cast` is the explicit way round it.
    !!
    !! A unit mismatch is an error for a sharper reason: there is no unit conversion in this
    !! library, so concatenating "m/s" rows with "km/h" rows would produce a column whose rows
    !! mean different things with nothing recording it. A column with NO declared unit on either
    !! side is accepted, keeping the destination's -- an in-memory table built without `unit=` is
    !! ordinary and should not be unappendable.
    subroutine append_check_compatible(self, i, other, j)
        class(parquet_table), intent(in) :: self  !! the destination table.
        integer, intent(in) :: i                  !! destination slot.
        class(parquet_table), intent(in) :: other !! the source table.
        integer, intent(in) :: j                  !! source slot.
        character(len=:), allocatable :: sfx, mine, theirs
        character(len=32) :: got, want
        !
        if (self%cache%cols(i)%values%kindof() /= other%cache%cols(j)%values%kindof()) then
            call parquet_kind_name(self%cache%cols(i)%values%kindof(), mine)
            call parquet_kind_name(other%cache%cols(j)%values%kindof(), theirs)
            call table_context_suffix(self%cache, self%cache%cols(i)%name, sfx)
            error stop EP // "append: this column is " // mine // " here but " // theirs // &
                " in the appended table; convert it first (%cast)" // sfx
        end if
        if (self%cache%cols(i)%values%colwidth() /= other%cache%cols(j)%values%colwidth()) then
            write(want, "(I0)") self%cache%cols(i)%values%colwidth()
            write(got, "(I0)") other%cache%cols(j)%values%colwidth()
            call table_context_suffix(self%cache, self%cache%cols(i)%name, sfx)
            error stop EP // "append: this column holds " // trim(want) // " values per row " // &
                "here but " // trim(got) // " in the appended table" // sfx
        end if
        call self%cache%cols(i)%values%unit_string(mine)
        call other%cache%cols(j)%values%unit_string(theirs)
        if (len_trim(mine) > 0 .and. len_trim(theirs) > 0 .and. trim(mine) /= trim(theirs)) then
            call table_context_suffix(self%cache, self%cache%cols(i)%name, sfx)
            error stop EP // "append: this column is in '" // trim(mine) // "' here but '" // &
                trim(theirs) // "' in the appended table, and this library does not convert " // &
                "units" // sfx
        end if
    end subroutine append_check_compatible
    !
    !> Builds a one-row table from a row handle, so `%append(row)` can reuse `%append(table)`'s
    !! compatibility rules rather than growing a second, subtly different set of them.
    !!
    !! This is what makes appending a row at a time slow in bulk: it costs a whole table's
    !! machinery per row. The documented bulk idiom is `%clone_structure` -> fill -> `%append`.
    subroutine append_row_as_table(self, r, one)
        class(parquet_table), intent(in) :: self  !! the destination, for its column list.
        type(parquet_table_row), intent(in) :: r  !! the row to copy.
        type(parquet_table), intent(out) :: one   !! receives the one-row table.
        integer :: i, src, matched
        type(parquet_column) :: piece
        !
        matched = 0
        do i = 1, self%cache%ncols
            if (cache_find(r%cache, self%cache%cols(i)%name) > 0) matched = matched + 1
        end do
        ! With nothing in common there is no row to append -- every column would be null-filled,
        ! which is %append_null_rows(1) said in a confusing way, and far more likely a mistake.
        if (matched == 0) error stop EP // "append: the row's table has no column in common " // &
            "with this table, so there is nothing to append"
        call parquet_new_table(one)
        do i = 1, self%cache%ncols
            src = cache_find(r%cache, self%cache%cols(i)%name)
            ! A column the row's own table does not have is left out entirely, so %append's
            ! null-fill rule covers it -- one rule for a missing column, not two.
            if (src == 0) cycle
            call row_slice_one(r%cache%cols(src)%values, r%irow, piece)
            call table_put_column(one, self%cache%cols(i)%name, piece)
            call piece%clear()
        end do
    end subroutine append_row_as_table
    !
    !> Copies row `irow` of a column into a fresh one-row column of the same kind, width and unit.
    subroutine row_slice_one(src, irow, out)
        type(parquet_column), intent(in) :: src    !! the source column.
        integer(int64), intent(in) :: irow         !! the row to take.
        type(parquet_column), intent(out) :: out   !! receives the one-row column.
        logical, allocatable :: keep(:)
        !
        call src%deep_copy(out)
        allocate(keep(src%length()))
        keep = .false.
        keep(irow) = .true.
        call out%delete_by_mask(keep)
    end subroutine row_slice_one
    !
end submodule parquet_tables_rowmutate
