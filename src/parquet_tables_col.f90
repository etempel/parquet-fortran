!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> `parquet_table_col` — a resolved handle on one column of a `parquet_table`.
!!
!! The column-major twin of `parquet_tables_row.f90`: where a `parquet_table_row` fixes the ROW and
!! leaves the column to be named per access, this fixes the COLUMN and leaves the row to be given
!! per access. A per-element loop over one column is the shape that pays, because the name lookup
!! it removes is 52–77% of what a `%get_element` costs.
!!
!! Three rules run through the file:
!!
!! * **Resolve once, at creation.** `column_by_name`/`column_by_index` do the whole of
!!   `table_resolve`'s work — the lookup, the unsupported-type refusal, the lazy first touch — and
!!   store the slot, the kind and the row scope. Nothing here repeats any of it.
!! * **A handle does not survive a structural change.** Every access runs `col_resolve`, which is
!!   an `associated` test and one `integer(int64)` comparison against the stamp taken at creation.
!!   That is deliberately conservative — `%append` does not move existing slots, so a handle
!!   *could* survive one — because a single total rule beats a list of exceptions the next
!!   mutation would silently fall outside.
!! * **The handle points at `cache`, never at the table.** Same rule as the row handle, and it is
!!   what lets a caller hold a handle without the table needing the `target` attribute.
submodule (parquet_tables) parquet_tables_col
    implicit none
    !
contains
    !
    !> Fills a handle from a slot the caller has already resolved. The one place a handle is
    !! constructed, so the stamp and the scope cannot be taken from different moments.
    subroutine col_attach(c, cache, idx, scope)
        type(parquet_table_col), intent(out) :: c   !! the handle to fill.
        type(parquet_table_cache), pointer, intent(in) :: cache !! the table's column store.
        integer, intent(in) :: idx                  !! validated slot index.
        type(table_scope), intent(in) :: scope      !! the table's row scope, copied by value.
        !
        c%cache => cache
        c%slot = idx
        c%colkind = cache%cols(idx)%declared_kind
        c%gen = cache%generation
        c%scope = scope
    end subroutine col_attach
    !
    module procedure table_resolve_to_handle
        integer :: idx
        !
        ! The handle is `intent(out)`, so a reported miss leaves it at its default initializers --
        ! detached, generation -1 -- and %is_valid() answers .false. without anything else to do.
        call table_resolve(self, name, proc, idx, found)
        if (idx == 0) return
        call col_attach(c, self%cache, idx, table_scope_of(self))
    end procedure table_resolve_to_handle
    !
    module procedure column_by_name
        call table_resolve_to_handle(self, name, "column", c, found)
    end procedure column_by_name
    !
    module procedure column_by_index
        integer :: idx
        !
        call table_check_open(self, "column")
        call table_check_no_append(self%cache, "column")
        call table_slot_or_fail(self, j, "column", idx, found)
        if (idx == 0) return
        ! The same tail every name-resolved value access runs, so a handle made by position is
        ! subject to exactly the unsupported-type and residency rules one made by name is.
        call table_resolve_slot(self, idx, "column", found)
        if (idx == 0) return
        call col_attach(c, self%cache, idx, table_scope_of(self))
    end procedure column_by_index
    !
    module procedure col_is_valid
        ok = .false.
        if (.not. associated(self%cache)) return
        ok = self%cache%generation == self%gen
    end procedure col_is_valid
    !
    module procedure col_index
        call col_resolve(self, "index")
        j = self%slot
    end procedure col_index
    !
    module procedure col_kind
        call col_resolve(self, "kind")
        k = self%colkind
    end procedure col_kind
    !
    module procedure col_resolve
        character(len=:), allocatable :: sfx
        character(len=32) :: g_now, g_then
        !
        if (.not. associated(self%cache)) then
            error stop EP // "column handle: " // trim(proc) // ": this handle is not attached " // &
                "to a table"
        end if
        if (self%cache%generation == self%gen) return
        ! The remedy is named because the cause is usually several statements away: a %drop_column,
        ! a %sort_by or an %append somewhere between making the handle and using it.
        write(g_now, "(I0)") self%cache%generation
        write(g_then, "(I0)") self%gen
        call table_context_suffix(self%cache, self%cache%cols(self%slot)%name, sfx)
        error stop EP // "column handle: " // trim(proc) // ": this table has changed " // &
            "structurally since the handle was made (generation " // trim(g_now) // &
            ", handle " // trim(g_then) // "); re-fetch it with %column(...)" // sfx
    end procedure col_resolve
    !
    module procedure col_require_row
        character(len=32) :: got, want
        !
        if (i >= 1_int64 .and. i <= self%scope%nrows) return
        write(got, "(I0)") i
        write(want, "(I0)") self%scope%nrows
        error stop EP // "column handle: " // trim(proc) // ": row index " // trim(got) // &
            " is outside this table's 1.." // trim(want) // " rows"
    end procedure col_require_row
    !
    module procedure col_kind_error
        character(len=:), allocatable :: sfx, got, wanted
        !
        ! `parquet_kind_name` is a SUBROUTINE with an allocatable-character out-argument, not a
        ! function -- this project bans character-returning functions outright (a confirmed
        ! gfortran thread-safety bug). See CLAUDE.md.
        call parquet_kind_name(self%colkind, got)
        call parquet_kind_name(want, wanted)
        call table_context_suffix(self%cache, self%cache%cols(self%slot)%name, sfx)
        error stop EP // "column handle: " // trim(proc) // ": this column holds " // &
            trim(got) // ", which cannot be read as " // trim(wanted) // sfx
    end procedure col_kind_error
    !
    ! ---- the null trio ------------------------------------------------------------------------
    !
    ! These are the one part of the handle's surface that is NOT per-kind: they take no value
    ! argument, so one body serves every column type, including the ten kinds stage 3a has not
    ! given %get/%set yet. That is deliberate -- a handle whose %is_null answered only for scalars
    ! would be a runtime surprise decided by the file's schema, which is exactly what the
    ! compile-time-only gap in %get/%set is not. See feature_colindex.md's stage 3.
    !
    module procedure col_is_null_i32
        isnull = self%is_null(int(i, int64))
    end procedure col_is_null_i32
    !
    module procedure col_is_null_i64
        call col_resolve(self, "is_null")
        call col_require_row(self, i, "is_null")
        isnull = self%cache%cols(self%slot)%values%is_null(i)
    end procedure col_is_null_i64
    !
    module procedure col_is_null_e32
        isnull = self%is_null(int(i, int64), int(e, int64))
    end procedure col_is_null_e32
    !
    module procedure col_is_null_e64
        call col_resolve(self, "is_null")
        call col_require_row(self, i, "is_null")
        ! The element index is bounds-checked by parquet_column itself, which names the element
        ! axis in its message -- the mistake this guards is passing a FLAT element position.
        isnull = self%cache%cols(self%slot)%values%is_null(i, e)
    end procedure col_is_null_e64
    !
    module procedure col_set_null_i32
        call self%set_null(int(i, int64))
    end procedure col_set_null_i32
    !
    module procedure col_set_null_i64
        call col_resolve(self, "set_null")
        call col_require_row(self, i, "set_null")
        ! nulling=.true.: the FIRST null on a column allocates its validity storage, and two
        ! threads doing that race with no diagnostic. Same rule the table's own %set_null obeys.
        call cache_check_shared_write(self%cache, self%slot, "set_null", nulling=.true.)
        call self%cache%cols(self%slot)%values%set_null(i)
    end procedure col_set_null_i64
    !
    module procedure col_set_null_e32
        call self%set_null(int(i, int64), int(e, int64))
    end procedure col_set_null_e32
    !
    module procedure col_set_null_e64
        call col_resolve(self, "set_null")
        call col_require_row(self, i, "set_null")
        call cache_check_shared_write(self%cache, self%slot, "set_null", nulling=.true.)
        call self%cache%cols(self%slot)%values%set_null(i, e)
    end procedure col_set_null_e64
    !
    module procedure col_clear_null_i32
        call self%clear_null(int(i, int64))
    end procedure col_clear_null_i32
    !
    module procedure col_clear_null_i64
        call col_resolve(self, "clear_null")
        call col_require_row(self, i, "clear_null")
        ! nulling=.false.: clearing a null cannot be the write that first allocates validity
        ! storage, because there is nothing to clear until something allocated it.
        call cache_check_shared_write(self%cache, self%slot, "clear_null", nulling=.false.)
        call self%cache%cols(self%slot)%values%clear_null(i)
    end procedure col_clear_null_i64
    !
    module procedure col_clear_null_e32
        call self%clear_null(int(i, int64), int(e, int64))
    end procedure col_clear_null_e32
    !
    module procedure col_clear_null_e64
        call col_resolve(self, "clear_null")
        call col_require_row(self, i, "clear_null")
        call cache_check_shared_write(self%cache, self%slot, "clear_null", nulling=.false.)
        call self%cache%cols(self%slot)%values%clear_null(i, e)
    end procedure col_clear_null_e64
    !
end submodule parquet_tables_col ! GCOVR_EXCL_LINE
