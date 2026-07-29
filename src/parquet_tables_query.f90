!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> Introspection and name resolution for `parquet_table`: the row/column counts, the per-column
!! queries (`%kind`, `%width`, `%unit`, `%residency`, `%is_null`), and the shared lookup and
!! guard helpers every value accessor is built on.
!!
!! Two conventions run through the whole file:
!!
!! * **The lookup key is always the column's INTERNAL name**, never the physical name in the
!!   file. They are the same today; keeping every lookup in `table_find` is what will let a
!!   read-time remap change that in one place.
!! * **`found=` decides whether a miss is fatal.** Absent, a missing column is a hard
!!   `error stop`; present, the miss is reported through it and the call returns quietly.
submodule (parquet_tables) parquet_tables_query
    implicit none
    !
contains
    !
    module procedure table_nrows
        call table_check_open(self, "nrows")
        n = self%row_count
    end procedure table_nrows
    !
    module procedure table_ncols
        call table_check_open(self, "ncols")
        n = self%cache%ncols
    end procedure table_ncols
    !
    module procedure table_column_names
        integer :: i, maxlen
        !
        call table_check_open(self, "column_names")
        ! A fixed-length array cannot hold ragged names, so the width is the longest name
        ! present; len=1 keeps a zero-column table's result well-formed rather than len=0.
        maxlen = 1
        do i = 1, self%cache%ncols
            if (len(self%cache%cols(i)%name) > maxlen) maxlen = len(self%cache%cols(i)%name)
        end do
        allocate(character(len=maxlen) :: names(self%cache%ncols))
        do i = 1, self%cache%ncols
            names(i) = self%cache%cols(i)%name
        end do
    end procedure table_column_names
    !
    module procedure table_has_column
        call table_check_open(self, "has_column")
        found = table_find(self, name) > 0
    end procedure table_has_column
    !
    module procedure table_find
        integer :: i
        !
        idx = 0
        if (.not. associated(self%cache)) return
        do i = 1, self%cache%ncols
            if (self%cache%cols(i)%name == trim(name)) then
                idx = i
                return
            end if
        end do
    end procedure table_find
    !
    module procedure table_column_kind
        integer :: idx
        !
        k = PK_NONE
        call table_lookup_or_fail(self, name, "kind", idx, found)
        if (idx == 0) return
        k = self%cache%cols(idx)%values%kindof()
    end procedure table_column_kind
    !
    module procedure table_column_width
        integer :: idx
        !
        wdt = 1
        call table_lookup_or_fail(self, name, "width", idx, found)
        if (idx == 0) return
        wdt = self%cache%cols(idx)%values%colwidth()
    end procedure table_column_width
    !
    module procedure table_column_unit
        integer :: idx
        !
        u = ""
        call table_lookup_or_fail(self, name, "unit", idx, found)
        if (idx == 0) return
        call self%cache%cols(idx)%values%unit_string(u)
    end procedure table_column_unit
    !
    module procedure table_column_residency
        integer :: idx
        !
        r = RES_EMPTY
        call table_lookup_or_fail(self, name, "residency", idx, found)
        if (idx == 0) return
        r = self%cache%cols(idx)%residency
    end procedure table_column_residency
    !
    module procedure table_is_supported
        integer :: idx
        !
        ok = .false.
        call table_lookup_or_fail(self, name, "is_supported", idx, found)
        if (idx == 0) return
        ok = self%cache%cols(idx)%supported
    end procedure table_is_supported
    !
    module procedure table_is_detached
        call table_check_open(self, "is_detached")
        d = self%detached
    end procedure table_is_detached
    !
    module procedure table_filename
        fname = ""
        if (allocated(self%source_file)) fname = self%source_file
    end procedure table_filename
    !
    module procedure table_get_file_metadata
        character(len=:), allocatable :: sfx
        logical :: got
        !
        call table_check_open(self, "get_file_metadata")
        value = ""
        if (.not. self%file_backed) then
            if (present(found)) then
                found = .false.
                return
            end if
            call table_context_suffix(self, "", sfx)
            error stop EP // "get_file_metadata: this table was not opened from a file" // sfx
        end if
        ! warn=.false. keeps a missing key quiet here: whether it is fatal is `found`'s job,
        ! decided one level up, not the reader's.
        call parquet_get_metadata(self%cache%reader, key, value, default="", warn=.false.)
        got = len(value) > 0
        if (present(found)) then
            found = got
            return
        end if
        if (.not. got) then
            call table_context_suffix(self, "", sfx)
            error stop EP // "get_file_metadata: no metadata key '" // trim(key) // "'" // sfx
        end if
    end procedure table_get_file_metadata
    !
    module procedure table_is_null
        integer :: idx
        !
        call table_resolve(self, name, "is_null", idx)
        isnull = self%cache%cols(idx)%values%is_null(i)
    end procedure table_is_null
    !
    module procedure table_resolve
        character(len=:), allocatable :: sfx
        !
        call table_check_open(self, proc)
        idx = table_find(self, name)
        if (idx == 0) then
            if (present(found)) then
                found = .false.
                return
            end if
            call table_context_suffix(self, name, sfx)
            error stop EP // trim(proc) // ": no column of this name" // sfx
        end if
        ! A column whose physical type this library cannot read got a slot at open time so it
        ! would still show up in %column_names -- but there is nothing to hand back, so any
        ! attempt to reach its values stops here rather than returning an empty column.
        if (.not. self%cache%cols(idx)%supported) then
            if (present(found)) then
                found = .false.
                idx = 0
                return
            end if
            call table_context_suffix(self, name, sfx)
            error stop EP // trim(proc) // ": this column's type is not supported by " // &
                "parquet_table, so its values were never read" // sfx
        end if
        if (self%cache%cols(idx)%residency == RES_EMPTY) then
            if (present(found)) then
                found = .false.
                idx = 0
                return
            end if
            call table_context_suffix(self, name, sfx)
            error stop EP // trim(proc) // ": this column holds no values" // sfx
        end if
        if (present(found)) found = .true.
    end procedure table_resolve
    !
    module procedure table_require_kind
        character(len=:), allocatable :: sfx, got, want
        !
        if (self%cache%cols(idx)%values%kindof() == kind) return
        call table_context_suffix(self, self%cache%cols(idx)%name, sfx)
        call parquet_kind_name(self%cache%cols(idx)%values%kindof(), got)
        call parquet_kind_name(kind, want)
        error stop EP // trim(proc) // ": column kind is " // got // ", not " // want // sfx
    end procedure table_require_kind
    !
    module procedure table_require_length
        character(len=:), allocatable :: sfx
        character(len=32) :: gots, wants
        !
        if (self%cache%cols(idx)%values%length() == n) return
        call table_context_suffix(self, self%cache%cols(idx)%name, sfx)
        write(gots, "(I0)") n
        write(wants, "(I0)") self%cache%cols(idx)%values%length()
        error stop EP // trim(proc) // ": array has " // trim(gots) // " rows but the column " // &
            "has " // trim(wants) // "; this replaces values, never the row set" // sfx
    end procedure table_require_length
    !
    module procedure table_check_open
        if (.not. associated(self%cache)) then
            ! Exempt from the file/column context suffix by CLAUDE.md's own rule: there is no
            ! file context to name yet.
            error stop EP // trim(proc) // ": table has not been opened"
        end if
    end procedure table_check_open
    !
    module procedure table_context_suffix
        character(len=:), allocatable :: parts
        !
        parts = ""
        if (allocated(self%source_file)) then
            if (len(self%source_file) > 0) parts = " (file '" // self%source_file // "'"
        end if
        if (len(parts) == 0 .and. len_trim(name) > 0) then
            suffix = " (column '" // trim(name) // "')"
            return
        end if
        if (len(parts) == 0) then
            suffix = ""
            return
        end if
        if (len_trim(name) > 0) then
            suffix = parts // ", column '" // trim(name) // "')"
        else
            suffix = parts // ")"
        end if
    end procedure table_context_suffix
    !
    !> Shared front half of every soft-failing query: resolves `name`, honouring `found=` and
    !! otherwise aborting. Unlike `table_resolve` this does NOT require the column to hold
    !! values -- asking a column's kind or residency must work precisely when it has none.
    subroutine table_lookup_or_fail(self, name, proc, idx, found)
        class(parquet_table), intent(in) :: self !! the table.
        character(len=*), intent(in) :: name     !! column name.
        character(len=*), intent(in) :: proc     !! calling procedure, for the message.
        integer, intent(out) :: idx              !! slot index, or 0 on a reported miss.
        logical, intent(out), optional :: found  !! present: report a miss instead of aborting.
        character(len=:), allocatable :: sfx
        !
        call table_check_open(self, proc)
        idx = table_find(self, name)
        if (idx == 0) then
            if (present(found)) then
                found = .false.
                return
            end if
            call table_context_suffix(self, name, sfx)
            error stop EP // trim(proc) // ": no column of this name" // sfx
        end if
        if (present(found)) found = .true.
    end subroutine table_lookup_or_fail
    !
end submodule parquet_tables_query
