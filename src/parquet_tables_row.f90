!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> The row handle `parquet_table_row`: making one, resolving a column through it, and the two
!! queries that do not depend on a column's kind. The per-kind `%get` specifics are generated
!! into `parquet_tables_access` alongside the table's own accessors.
!!
!! The structural rule this file exists to protect, and the reason the handle looks the way it
!! does: **it points at the table's cache, never at the table**. `t%row(i)` returns by value, and
!! a pointer associated with a `target` DUMMY becomes undefined the moment the function returns
!! (F2018 15.5.2.4) -- so `r%cache => self` would be valid only when the caller happened to
!! declare the table `target`, and silent memory corruption otherwise, with no diagnostic either
!! way. Pointing at `self%cache`, a pointer COMPONENT whose target is heap the table owns,
!! escapes that rule entirely and keeps `target` out of the public API. Do not "simplify" this.
!!
!! Everything else the handle needs (the row index, the row scope, the detach flag) is copied by
!! value, so a handle carries exactly what a lazy first touch requires and nothing that could go
!! stale without also invalidating the handle.
submodule (parquet_tables) parquet_tables_row
    implicit none
    !
contains
    !
    module procedure row_at_i32
        r = row_at_i64(self, int(i, int64))
    end procedure row_at_i32
    !
    module procedure row_at_i64
        call table_check_open(self, "row")
        ! Checked here rather than at the first %get: a handle that can never work should say so
        ! where the mistake was made, not several calls later.
        call table_require_row(self, i, "row")
        r%cache => self%cache
        r%irow = i
        r%gen = self%cache%generation
        r%scope = table_scope_of(self)
    end procedure row_at_i64
    !
    module procedure row_index
        i = self%irow
    end procedure row_index
    !
    module procedure row_is_valid
        ok = .false.
        if (.not. associated(self%cache)) return
        ok = self%cache%generation == self%gen
    end procedure row_is_valid
    !
    module procedure row_check_current
        character(len=:), allocatable :: sfx
        character(len=32) :: g_now, g_then
        !
        if (.not. associated(self%cache)) then
            error stop EP // "row " // trim(proc) // ": this row handle is not attached to a table"
        end if
        if (self%cache%generation == self%gen) return
        ! The remedy is named because the cause is usually several statements away: a %sort_by, a
        ! %filter_rows, an %append somewhere between making the handle and using it.
        write(g_now, "(I0)") self%cache%generation
        write(g_then, "(I0)") self%gen
        call table_context_suffix(self%cache, "", sfx)
        error stop EP // "row " // trim(proc) // ": this table has changed structurally since the " // &
            "handle was made (generation " // trim(g_now) // ", handle " // trim(g_then) // &
            "); re-fetch it with %row(...)" // sfx
    end procedure row_check_current
    !
    module procedure row_is_null
        integer :: idx
        !
        call row_resolve(self, name, "is_null", idx)
        isnull = parquet_column_is_null(self%cache%cols(idx)%values, self%irow)
    end procedure row_is_null
    !
    module procedure row_is_null_elem
        integer :: idx
        !
        call row_resolve(self, name, "is_null", idx)
        isnull = parquet_column_is_null(self%cache%cols(idx)%values, self%irow, e)
    end procedure row_is_null_elem
    !
    module procedure row_require_col
        call row_check_current(self, proc)
        ! Then `c`, whose message names its own remedy. Only a CURRENT handle's cache pointer and
        ! scope say anything about the table as it is now, which is what the two checks below need.
        call col_resolve(c, proc)
        if (.not. associated(self%cache, c%cache)) then
            error stop EP // "row " // trim(proc) // ": this column handle belongs to a different " // &
                "table than this row handle; a handle carries its own table and cannot be reused " // &
                "across two"
        end if
        ! Against `c`'s scope rather than the row handle's own. Both are current by this point --
        ! `row_check_current` above has already proved the row handle's generation matches -- so the
        ! two agree; using `c`'s keeps the bounds check reading from the handle that was validated
        ! most recently, and costs nothing.
        call col_require_row(c, self%irow, proc)
    end procedure row_require_col
    !
    module procedure row_resolve
        character(len=:), allocatable :: sfx
        !
        ! Attached AND current, from the same body %is_valid answers from -- one rule, and the
        ! same one parquet_table_col obeys.
        call row_check_current(self, proc)
        idx = cache_find(self%cache, name)
        if (idx == 0) then
            call table_context_suffix(self%cache, name, sfx)
            error stop EP // "row " // trim(proc) // ": no column of this name" // sfx
        end if
        if (.not. self%cache%cols(idx)%supported) then
            call table_context_suffix(self%cache, name, sfx)
            error stop EP // "row " // trim(proc) // ": this column's type is not supported by " // &
                "parquet_table, so its values were never read" // sfx
        end if
        ! A row read is a value read, so it triggers the same lazy first touch the table's own
        ! accessors do -- which is the whole reason the handle carries the scope by value.
        if (self%cache%cols(idx)%residency /= RES_FULL) then
            call table_touch(self%cache, self%scope, idx, "row " // trim(proc))
        end if
    end procedure row_resolve
    !
    module procedure row_require_kind
        character(len=:), allocatable :: sfx, got, want
        !
        if (self%cache%cols(idx)%declared_kind == kind) return
        call table_context_suffix(self%cache, name, sfx)
        call parquet_kind_name(self%cache%cols(idx)%declared_kind, got)
        call parquet_kind_name(kind, want)
        error stop EP // "row get: column kind is " // got // ", not " // want // sfx
    end procedure row_require_kind
    !
    module procedure row_kind_error
        character(len=:), allocatable :: sfx, kname
        !
        call table_context_suffix(self%cache, name, sfx)
        call parquet_kind_name(self%cache%cols(idx)%declared_kind, kname)
        error stop EP // "row get: column kind (" // kname // ") cannot be copied into this " // &
            "variable" // sfx
    end procedure row_kind_error
    !
end submodule parquet_tables_row
