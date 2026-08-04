!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> Turns a `parquet_table`'s key columns into a row permutation, using the library's own C++
!! `std::sort` engine -- the SAME engine the read-time `sort_by=` runs on.
!!
!! **That sharing is the whole point of this file.** A table can be ordered two ways: by reading
!! a file with `parquet_open_reader(..., sort_by=...)`, or by `%sort_by` on an already-assembled
!! table. If those two used different comparators they could disagree about null placement, NaN
!! placement or tie order, and nothing would report it -- the rows would simply come back in a
!! different order depending on which path a program happened to take.
!!
!! **The sharing now goes through `parquet_sorting`'s public API rather than the raw C bindings.**
!! `parquet_column` is one of the eleven element types `pf_sort_keys%add` accepts, so a table's
!! key columns are handed over as ordinary sort keys and `pf_argsort` does the rest -- which is
!! how the value extraction, the (seconds, nanoseconds) split every timestamp key needs, and the
!! per-row validity handling all came to live in exactly one place. Before `parquet_sorting`
!! existed this file carried its own copy of all three.
!!
!! What this file therefore does NOT do: decide anything about order, or know anything about how
!! a value reaches the engine. It resolves key names to columns, refuses the columns that cannot
!! be sort keys, and passes the caller's `descending`/`nulls_first` flags through. **The refusals
!! stay here** rather than being left to `pf_argsort`'s own equivalents, because a table can say
!! which column and which file the problem is in and `pf_argsort`, handed a bare column, cannot.
submodule (parquet_tables) parquet_tables_sort
    use parquet_sorting, only : pf_sort_keys, pf_argsort
    implicit none
    !
contains
    !
    module procedure table_build_sort_permutation
        type(pf_sort_keys) :: skeys
        integer :: ik, idx
        logical :: desc, nulls_lo
        !
        do ik = 1, size(keys)
            call sort_lookup_key(self, keys(ik), idx)
            desc = .false.
            if (present(descending)) desc = descending(ik)
            nulls_lo = .false.
            if (present(nulls_first)) nulls_lo = nulls_first(ik)
            call skeys%add(self%cache%cols(idx)%values, descending=desc, nulls_first=nulls_lo)
        end do
        call pf_argsort(skeys, perm)
    end procedure table_build_sort_permutation
    !
    !> Resolves one key name to its slot, refusing every column that cannot be a sort key.
    !!
    !! An unmaterialized key column is refused rather than read (Q3c-6). Sorting already forces
    !! whole-column reads, and letting it ALSO pull columns off disk would make the memory a
    !! `%sort_by` call costs depend on which columns happen to be resident -- worst of all on a
    !! slice-regime table over a large file. `%prefetch` is one line and says what it does.
    subroutine sort_lookup_key(self, name, idx)
        class(parquet_table), intent(in) :: self !! the table.
        character(len=*), intent(in) :: name     !! the key column's name.
        integer, intent(out) :: idx              !! its slot index.
        character(len=:), allocatable :: sfx, kname
        !
        call table_lookup_or_fail(self, name, "sort_by", idx)
        if (.not. self%cache%cols(idx)%supported) then
            call table_context_suffix(self%cache, name, sfx)
            error stop EP // "sort_by: this column's type is not supported by parquet_table, " // &
                "so it cannot be a sort key" // sfx
        end if
        if (self%cache%cols(idx)%residency /= RES_FULL) then
            call table_context_suffix(self%cache, name, sfx)
            error stop EP // "sort_by: this column has not been read yet, and sorting will not " // &
                "read it implicitly; call table%prefetch(...) on every key column first" // sfx
        end if
        if (.not. sort_kind_is_orderable(self%cache%cols(idx)%values%kindof())) then
            call parquet_kind_name(self%cache%cols(idx)%values%kindof(), kname)
            call table_context_suffix(self%cache, name, sfx)
            error stop EP // "sort_by: a " // kname // " column cannot be a sort key; there is " // &
                "no defined order on a whole vector row" // sfx
        end if
    end subroutine sort_lookup_key
    !
    !> Whether a PK_* kind can be a sort key: every SCALAR kind can, no *_VEC kind can.
    !!
    !! Kept here, rather than deferred to `pf_argsort`'s own vector-column refusal, so the message
    !! can name the offending column and its file -- see this file's header.
    pure logical function sort_kind_is_orderable(kind) result(ok)
        integer, intent(in) :: kind !! the PK_* discriminator to test.
        ok = kind == PK_INT32 .or. kind == PK_INT64 .or. kind == PK_FLOAT32 .or. &
            kind == PK_FLOAT64 .or. kind == PK_LOGICAL .or. kind == PK_STRING .or. &
            kind == PK_DATE .or. kind == PK_TIME .or. kind == PK_TIMESTAMP
    end function sort_kind_is_orderable
    !
end submodule parquet_tables_sort
