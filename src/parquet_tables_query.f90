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
    module procedure table_scope_of
        sc%regime = self%regime
        sc%row_lo = self%row_lo
        sc%row_hi = self%row_hi
        sc%nrows = self%row_count
        sc%detached = self%detached
    end procedure table_scope_of
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
        idx = 0
        ! Defensive only: every call site (table_has_column, table_resolve, table_lookup_or_fail,
        ! table_new_slot's own callers) runs table_check_open first, which already aborts on an
        ! unassociated cache -- so this branch guards an invariant nothing reachable can violate.
        if (.not. associated(self%cache)) return ! GCOVR_EXCL_LINE
        idx = cache_find(self%cache, name)
    end procedure table_find
    !
    module procedure cache_find
        integer :: i
        !
        idx = 0
        do i = 1, cache%ncols
            if (cache%cols(i)%name == trim(name)) then
                idx = i
                return
            end if
        end do
    end procedure cache_find
    !
    module procedure table_column_kind
        integer :: idx
        !
        ! Answers from the DESCRIPTOR, not from the value store, because a column that has not
        ! been touched yet holds no values -- and %kind is precisely how a caller decides which
        ! %col specific to call, so it has to work before the first read, not after it.
        k = PK_NONE
        call table_lookup_or_fail(self, name, "kind", idx, found)
        if (idx == 0) return
        ! For a plain LIST/LARGE_LIST column the kind is not known until the width is, and a
        ! metadata query must not answer with a guess -- so this resolves it for real (proven),
        ! which does read data for that one column type. Every other column was classified from
        ! the schema at open and this is a no-op. See table_resolve_width.
        call table_resolve_width(self%cache, table_scope_of(self), idx, .true., "kind")
        k = self%cache%cols(idx)%declared_kind
    end procedure table_column_kind
    !
    module procedure table_column_width
        integer :: idx
        !
        wdt = 1
        call table_lookup_or_fail(self, name, "width", idx, found)
        if (idx == 0) return
        ! Same as %kind above: proven, not guessed.
        call table_resolve_width(self%cache, table_scope_of(self), idx, .true., "width")
        wdt = self%cache%cols(idx)%width
    end procedure table_column_width
    !
    module procedure table_column_unit
        integer :: idx
        !
        u = ""
        call table_lookup_or_fail(self, name, "unit", idx, found)
        if (idx == 0) return
        ! The descriptor first, because it answers for a column nothing has read yet -- a
        ! file-backed column's unit comes from the read-in MAML at open, not from its values. The
        ! values are asked only for a column that has no descriptor unit, which is every column
        ! built with %add_column(unit=).
        if (allocated(self%cache%cols(idx)%unit)) then
            u = self%cache%cols(idx)%unit
            return
        end if
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
        ! Deliberately does NOT go through table_check_open: asking an unopened table where it
        ! came from is a fair question with a good answer ("nowhere"), unlike asking it for values.
        fname = ""
        if (.not. associated(self%cache)) return
        if (allocated(self%cache%source_file)) fname = self%cache%source_file
    end procedure table_filename
    !
    module procedure table_get_file_metadata
        character(len=:), allocatable :: sfx
        logical :: got
        integer :: i
        !
        call table_check_open(self, "get_file_metadata")
        value = ""
        ! Answered from the snapshot taken at open, NOT through the reader -- which is what makes
        ! it keep working after a row mutation has detached the table and released that reader.
        ! `meta_keys` is allocated (possibly zero-size) for every file-backed open, so its being
        ! unallocated is exactly "this table never had a file", which is the question here; by
        ! this point `file_backed` cannot answer it, since detaching clears that too.
        if (.not. allocated(self%cache%meta_keys)) then
            if (present(found)) then
                found = .false.
                return
            end if
            call table_context_suffix(self%cache, "", sfx)
            error stop EP // "get_file_metadata: this table was not opened from a file" // sfx
        end if
        got = .false.
        do i = 1, size(self%cache%meta_keys)
            if (trim(self%cache%meta_keys(i)) == trim(key)) then
                value = trim(self%cache%meta_values(i))
                got = .true.
                exit
            end if
        end do
        if (present(found)) then
            found = got
            return
        end if
        if (.not. got) then
            call table_context_suffix(self%cache, "", sfx)
            error stop EP // "get_file_metadata: no metadata key '" // trim(key) // "'" // sfx
        end if
    end procedure table_get_file_metadata
    !
    module procedure table_is_null_i32
        isnull = self%is_null(name, int(i, int64))
    end procedure table_is_null_i32
    !
    module procedure table_is_null_i64
        integer :: idx
        !
        call table_resolve(self, name, "is_null", idx)
        call table_require_row(self, i, "is_null")
        isnull = self%cache%cols(idx)%values%is_null(i)
    end procedure table_is_null_i64
    !
    module procedure table_require_row
        character(len=32) :: got, want
        !
        if (i >= 1_int64 .and. i <= self%row_count) return
        write(got, "(I0)") i
        write(want, "(I0)") self%row_count
        error stop EP // trim(proc) // ": row index " // trim(got) // " is outside this " // &
            "table's 1.." // trim(want) // " rows"
    end procedure table_require_row
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
            call table_context_suffix(self%cache, name, sfx)
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
            call table_context_suffix(self%cache, name, sfx)
            error stop EP // trim(proc) // ": this column's type is not supported by " // &
                "parquet_table, so its values were never read" // sfx
        end if
        ! The lazy first touch, and the ONLY place it happens: every value accessor -- %col,
        ! %get, %set, %is_null, a row handle's %get, %get_slice -- reaches its slot through here,
        ! so residency is settled once, in one place. A column already resident costs the
        ! comparison below and nothing else.
        if (self%cache%cols(idx)%residency /= RES_FULL) then
            call table_touch(self%cache, table_scope_of(self), idx, proc)
        end if
        if (present(found)) found = .true.
    end procedure table_resolve
    !
    module procedure table_check_not_detached
        character(len=:), allocatable :: sfx
        !
        if (.not. sc%detached) return
        call table_context_suffix(cache, name, sfx)
        error stop EP // trim(proc) // ": this table has been detached from its file by " // &
            "a row-structural change; materialize a column before mutating rows" // sfx
    end procedure table_check_not_detached
    !
    module procedure table_require_kind
        character(len=:), allocatable :: sfx, got, want
        !
        if (self%cache%cols(idx)%values%kindof() == kind) return
        call table_context_suffix(self%cache, self%cache%cols(idx)%name, sfx)
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
        call table_context_suffix(self%cache, self%cache%cols(idx)%name, sfx)
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
        if (allocated(cache%source_file)) then
            if (len(cache%source_file) > 0) parts = " (file '" // cache%source_file // "'"
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
    module procedure table_lookup_or_fail
        character(len=:), allocatable :: sfx
        !
        call table_check_open(self, proc)
        idx = table_find(self, name)
        if (idx == 0) then
            if (present(found)) then
                found = .false.
                return
            end if
            call table_context_suffix(self%cache, name, sfx)
            error stop EP // trim(proc) // ": no column of this name" // sfx
        end if
        if (present(found)) found = .true.
    end procedure table_lookup_or_fail
    !
end submodule parquet_tables_query
