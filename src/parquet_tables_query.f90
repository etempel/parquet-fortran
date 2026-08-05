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
    module procedure table_nrows_unfiltered
        call table_check_open(self, "nrows_unfiltered")
        n = self%cache%unfiltered_rows
    end procedure table_nrows_unfiltered
    !
    module procedure table_row_group_extent
        call table_check_open(self, "row_group_extent")
        n = self%cache%rg_extent_rows
    end procedure table_row_group_extent
    !
    module procedure table_ncols
        integer :: i
        !
        call table_check_open(self, "ncols")
        n = self%cache%ncols
        if (.not. present(resident_only)) return
        if (.not. resident_only) return
        n = 0
        do i = 1, self%cache%ncols
            if (self%cache%cols(i)%residency == RES_FULL) n = n + 1
        end do
    end procedure table_ncols
    !
    module procedure table_column_names
        integer :: i, maxlen, n
        logical :: only_res
        !
        call table_check_open(self, "column_names")
        only_res = .false.
        if (present(resident_only)) only_res = resident_only
        ! A fixed-length array cannot hold ragged names, so the width is the longest name
        ! present; len=1 keeps a zero-column table's result well-formed rather than len=0. Sized
        ! over the columns actually being reported, so a filtered list is not padded to the width
        ! of a name it leaves out.
        maxlen = 1
        n = 0
        do i = 1, self%cache%ncols
            if (only_res .and. self%cache%cols(i)%residency /= RES_FULL) cycle
            n = n + 1
            if (len(self%cache%cols(i)%name) > maxlen) maxlen = len(self%cache%cols(i)%name)
        end do
        allocate(character(len=maxlen) :: names(n))
        n = 0
        do i = 1, self%cache%ncols
            if (only_res .and. self%cache%cols(i)%residency /= RES_FULL) cycle
            n = n + 1
            names(n) = self%cache%cols(i)%name
        end do
    end procedure table_column_names
    !
    module procedure table_has_column
        call table_check_open(self, "has_column")
        found = table_find(self, name) > 0
        ! The automatic row-index column exists whether or not it has been asked for yet, so this
        ! answers .true. for it while it is still virtual. That is the ONE place %has_column and
        ! %column_names disagree, and deliberately: %has_column asks "can I use this name?", which
        ! is yes, while %column_names lists what the table is holding, which does not include a
        ! column nobody has asked for.
        if (.not. found .and. name == PARQUET_ROW_INDEX) found = self%cache%file_backed
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
    module procedure table_generation
        call table_check_open(self, "generation")
        g = self%cache%generation
    end procedure table_generation
    !
    module procedure table_has_nulls
        integer :: idx
        integer(int64) :: rg_lo, rg_hi
        !
        any_null = .false.
        call table_lookup_or_fail(self, name, "has_nulls", idx, found)
        if (idx == 0) return
        ! A resident column knows the answer exactly. A file-backed one that has NOT been read is
        ! answered from the file's footer instead of by reading it -- the whole reason this exists
        ! rather than the caller reading the column and scanning it. The footer answer is
        ! conservative (see the interface's own note), and it is not asked for a column that is
        ! not file-backed or a table that has detached, since neither has a footer to ask.
        if (self%cache%cols(idx)%residency == RES_FULL) then
            any_null = self%cache%cols(idx)%values%any_null()
            return
        end if
        if (.not. self%cache%cols(idx)%file_source) return
        if (.not. self%cache%file_backed) return
        ! Scoped to the row groups this table actually covers, so a slice is not told about nulls
        ! in rows it does not hold. 0/0 asks about the whole file, which is right for a whole-file
        ! table; a slice narrows it to its covering row groups.
        rg_lo = 0_int64
        rg_hi = 0_int64
        if (allocated(self%cache%rg_bounds)) then
            call rg_covering_range(self%cache%rg_bounds, self%row_lo, self%row_hi, rg_lo, rg_hi)
        end if
        any_null = parquet_column_has_nulls(self%cache%reader, self%cache%cols(idx)%file_name, rg_lo, rg_hi)
    end procedure table_has_nulls
    !
    module procedure table_get_valid_mask
        integer :: idx
        integer(int64) :: i
        !
        call table_resolve(self, name, "get_valid_mask", idx, found)
        if (idx == 0) then
            allocate(mask(0))
            return
        end if
        allocate(mask(self%cache%cols(idx)%values%length()))
        ! Always filled, never left unallocated for a null-free column: a caller would then have
        ! to test allocated() before every use, and the one thing this procedure exists to give
        ! them is an array they can use directly.
        do i = 1_int64, size(mask, kind=int64)
            mask(i) = .not. self%cache%cols(idx)%values%is_null(i)
        end do
    end procedure table_get_valid_mask
    !
    module procedure table_get_valid_mask_elem
        integer :: idx, wdt
        integer(int64) :: i, e
        !
        call table_resolve(self, name, "get_valid_mask", idx, found)
        if (idx == 0) then
            allocate(mask(0, 0))
            return
        end if
        wdt = self%cache%cols(idx)%values%colwidth()
        allocate(mask(wdt, self%cache%cols(idx)%values%length()))
        ! Always filled, for the same reason the row form is: a caller should not have to test
        ! allocated() before using what this hands back.
        do i = 1_int64, size(mask, 2, kind=int64)
            do e = 1_int64, int(wdt, int64)
                mask(e, i) = .not. self%cache%cols(idx)%values%is_null(i, e)
            end do
        end do
    end procedure table_get_valid_mask_elem
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
        isnull = self%is_null(name, int(i, int64), found)
    end procedure table_is_null_i32
    !
    module procedure table_is_null_i64
        integer :: idx
        !
        ! .false. on a reported miss, not .true.: "there is no such column" is not "that row is
        ! null", and a caller ignoring `found` should not read a missing column as all-null.
        isnull = .false.
        call table_resolve(self, name, "is_null", idx, found)
        if (idx == 0) return
        call table_require_row(self, i, "is_null")
        isnull = self%cache%cols(idx)%values%is_null(i)
    end procedure table_is_null_i64
    !
    module procedure table_is_null_e32
        isnull = self%is_null(name, int(i, int64), int(e, int64), found)
    end procedure table_is_null_e32
    !
    module procedure table_is_null_e64
        integer :: idx
        !
        ! .false. on a reported miss, for the same reason the row form gives .false.
        isnull = .false.
        call table_resolve(self, name, "is_null", idx, found)
        if (idx == 0) return
        call table_require_row(self, i, "is_null")
        ! The element index is bounds-checked by parquet_column itself (check_element), which
        ! names the element axis in its message -- the mistake this guards is passing a FLAT
        ! element position where a row and an element were wanted.
        isnull = self%cache%cols(idx)%values%is_null(i, e)
    end procedure table_is_null_e64
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
        ! Every value accessor -- %col, %get, %set, %is_null, %get_element, a row handle's %get,
        ! %get_slice -- reaches its slot through here, which makes this the one place the cheap
        ! half of the append/read contract has to be written for all of them to have it, including
        ! any accessor added later. One atomic read of a counter, against a call that is about to
        ! copy or point at a whole column.
        call table_check_no_append(self%cache, proc)
        ! The reserved name resolves to the automatic column, materializing it on this first use.
        ! Done here, in the one lookup every value accessor goes through, so every one of them
        ! reaches it without knowing it exists.
        if (name == PARQUET_ROW_INDEX .and. table_find(self, name) == 0) then
            ! `meta_keys` is allocated for every table that was opened from a file and stays so
            ! after a detach, which is exactly the question here: a table that HAD a file gets the
            ! row index (or, once detached, the message explaining why it can no longer have it),
            ! while one built in memory never had a file row to name and falls through to the
            ! ordinary "no column of this name".
            if (allocated(self%cache%meta_keys)) call table_make_row_index(self)
        end if
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
            ! Registered as a read in flight for its duration, so a concurrent %append refuses
            ! rather than reallocating storage underneath it. Only this window is registered, not
            ! the whole accessor: a first touch reads from the file and takes real time, while the
            ! copy the accessor performs afterwards is a memcpy over already-resident memory. The
            ! cheap check above covers that shorter window, and keeping the counter off the
            ! resident path is what keeps a read of a resident column entirely free of atomics --
            ! the property this whole design exists to protect. See parquet_table_cache's
            ! `readers_active` comment.
            call table_read_enter(self%cache, proc)
            call table_touch(self%cache, table_scope_of(self), idx, proc)
            call table_read_exit(self%cache)
        end if
        ! Checked here, after `idx` is known good, so every %set/%set_element specific inherits
        ! the string-column rule from one place rather than each carrying its own copy -- and so
        ! a specific added later gets it by copying its neighbour's table_resolve call.
        if (present(writing)) then
            if (writing) call table_check_shared_write(self, idx, proc, nulling=.false.)
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
    module procedure table_print_stat
        logical :: want_all
        integer :: i, nshown, wname
        integer(int64) :: nulls, k
        character(len=:), allocatable :: kname, min_s, max_s, unit_s, fname
        character(len=32) :: rows_s, nulls_s, wdt_s
        !
        call table_check_open(self, "print_stat")
        ! Solicited output: verbosity="silent" and below turn this into a no-op. The open check
        ! stays ABOVE it, so a print_stat on an unopened table still reports that mistake rather
        ! than silently doing nothing for the wrong reason.
        if (parquet_output_is_suppressed()) return
        want_all = .false.
        if (present(all)) want_all = all
        !
        nshown = 0
        ! At least as wide as the header word, or the header line would be wider than the rows
        ! under it and nothing would line up.
        wname = len("column")
        do i = 1, self%cache%ncols
            if (.not. want_all .and. self%cache%cols(i)%residency /= RES_FULL) cycle
            nshown = nshown + 1
            wname = max(wname, len(self%cache%cols(i)%name))
        end do
        call self%filename(fname)
        write(rows_s, "(I0)") self%row_count
        if (len_trim(fname) > 0) then
            print "(a)", "parquet_table: " // trim(fname)
        else
            print "(a)", "parquet_table: (built in memory)"
        end if
        write(nulls_s, "(I0)") self%cache%ncols
        write(wdt_s, "(I0)") count_resident(self%cache)
        print "(a)", "  rows: " // trim(rows_s) // "   columns: " // trim(nulls_s) // &
            " (" // trim(wdt_s) // " materialized)"
        if (nshown == 0) then
            print "(a)", "  (no materialized columns; pass all=.true. to list every column)"
            return
        end if
        print "(a)", "  " // pad("column", wname) // "  " // pad("kind", 18) // "  " // &
            pad("width", 6) // "  " // pad("nulls", 10) // "  " // pad("min", 22) // "  max"
        do i = 1, self%cache%ncols
            if (.not. want_all .and. self%cache%cols(i)%residency /= RES_FULL) cycle
            ! A deferred plain-LIST column is reported as pending rather than resolved: measuring
            ! its width would read the data, and a report must not change what it reports on.
            if (self%cache%cols(i)%width_pending) then
                print "(a)", "  " // pad(self%cache%cols(i)%name, wname) // "  " // &
                    pad("pending", 18) // "  " // pad("-", 6) // "  " // pad("-", 10) // "  " // &
                    pad("-", 22) // "  -"
                cycle
            end if
            call parquet_kind_name(self%cache%cols(i)%declared_kind, kname)
            write(wdt_s, "(I0)") self%cache%cols(i)%width
            if (self%cache%cols(i)%residency /= RES_FULL) then
                print "(a)", "  " // pad(self%cache%cols(i)%name, wname) // "  " // &
                    pad(kname, 18) // "  " // pad(trim(wdt_s), 6) // "  " // pad("-", 10) // &
                    "  " // pad("-", 22) // "  -"
                cycle
            end if
            nulls = 0_int64
            do k = 1_int64, self%cache%cols(i)%values%length()
                if (self%cache%cols(i)%values%is_null(k)) nulls = nulls + 1_int64
            end do
            write(nulls_s, "(I0)") nulls
            call table_column_stat_text(self%cache%cols(i)%values, min_s, max_s)
            call self%cache%cols(i)%values%unit_string(unit_s)
            if (allocated(self%cache%cols(i)%unit)) unit_s = self%cache%cols(i)%unit
            if (len_trim(unit_s) > 0) kname = kname // " [" // trim(unit_s) // "]"
            print "(a)", "  " // pad(self%cache%cols(i)%name, wname) // "  " // &
                pad(kname, 18) // "  " // pad(trim(wdt_s), 6) // "  " // pad(trim(nulls_s), 10) // &
                "  " // pad(min_s, 22) // "  " // max_s
        end do
    end procedure table_print_stat
    !
    !> How many of a table's columns are resident.
    integer function count_resident(cache) result(n)
        type(parquet_table_cache), intent(in) :: cache !! the column store.
        integer :: i
        !
        n = 0
        do i = 1, cache%ncols
            if (cache%cols(i)%residency == RES_FULL) n = n + 1
        end do
    end function count_resident
    !
    !> `text` in a field `w` wide: blank-padded, or returned whole when it is longer.
    !!
    !! A column whose value overflows its column is left overflowing rather than truncated -- a
    !! ragged line is a nuisance, a silently shortened value is a wrong answer.
    function pad(text, w) result(res)
        character(len=*), intent(in) :: text !! the text to place.
        integer, intent(in) :: w             !! field width.
        character(len=max(len_trim(text), w)) :: res !! the padded field.
        !
        res = trim(text)
    end function pad
    !
    module procedure table_valid_mask_of
        integer(int64) :: i
        !
        allocate(mask(cache%cols(idx)%values%length()))
        do i = 1_int64, size(mask, kind=int64)
            mask(i) = .not. cache%cols(idx)%values%is_null(i)
        end do
    end procedure table_valid_mask_of
    !
    module procedure table_valid_mask_rows
        integer(int64) :: k
        !
        allocate(mask(size(rows)))
        do k = 1_int64, size(rows, kind=int64)
            mask(k) = .not. cache%cols(idx)%values%is_null(rows(k))
        end do
    end procedure table_valid_mask_rows
    !
    module procedure table_apply_valid
        integer(int64) :: i
        character(len=32) :: got, want
        character(len=:), allocatable :: sfx
        !
        if (size(is_valid, kind=int64) /= self%row_count) then
            write(got, "(I0)") size(is_valid, kind=int64)
            write(want, "(I0)") self%row_count
            call table_context_suffix(self%cache, name, sfx)
            error stop EP // trim(proc) // ": is_valid has " // trim(got) // " entries but the " // &
                "table has " // trim(want) // " rows" // sfx
        end if
        do i = 1_int64, self%row_count
            if (.not. is_valid(i)) call self%cache%cols(idx)%values%set_null(i)
        end do
    end procedure table_apply_valid
    !
    module procedure table_apply_valid_rows
        integer(int64) :: k
        character(len=32) :: got, want
        character(len=:), allocatable :: sfx
        !
        if (size(is_valid, kind=int64) /= size(rows, kind=int64)) then
            write(got, "(I0)") size(is_valid, kind=int64)
            write(want, "(I0)") size(rows, kind=int64)
            call table_context_suffix(self%cache, name, sfx)
            error stop EP // trim(proc) // ": is_valid has " // trim(got) // " entries but the " // &
                "selection has " // trim(want) // " rows" // sfx
        end if
        do k = 1_int64, size(rows, kind=int64)
            if (.not. is_valid(k)) call self%cache%cols(idx)%values%set_null(rows(k))
        end do
    end procedure table_apply_valid_rows
    !
    module procedure table_valid_mask_of_elem
        integer(int64) :: i, e
        integer :: wdt
        !
        ! Per-element queries rather than `element_validity`, for the same reason
        ! table_valid_mask_of uses `is_null` rather than `row_validity`: `cache` is intent(in)
        ! here, and the bulk builders take the column as intent(inout) so a temporal kind can
        ! refresh its cached null flag. The contract also differs -- this one always allocates,
        ! where the bulk builders deliberately leave a null-free column's mask unallocated.
        wdt = cache%cols(idx)%values%colwidth()
        allocate(mask(wdt, cache%cols(idx)%values%length()))
        do i = 1_int64, size(mask, 2, kind=int64)
            do e = 1_int64, int(wdt, int64)
                mask(e, i) = .not. cache%cols(idx)%values%is_null(i, e)
            end do
        end do
    end procedure table_valid_mask_of_elem
    !
    module procedure table_valid_mask_rows_elem
        integer(int64) :: k, e
        integer :: wdt
        !
        wdt = cache%cols(idx)%values%colwidth()
        allocate(mask(wdt, size(rows)))
        do k = 1_int64, size(rows, kind=int64)
            do e = 1_int64, int(wdt, int64)
                mask(e, k) = .not. cache%cols(idx)%values%is_null(rows(k), e)
            end do
        end do
    end procedure table_valid_mask_rows_elem
    !
    module procedure table_apply_valid_elem
        integer(int64) :: i, e, w
        character(len=64) :: got, want
        character(len=:), allocatable :: sfx
        !
        w = int(self%cache%cols(idx)%values%colwidth(), int64)
        if (size(is_valid, 1, kind=int64) /= w .or. size(is_valid, 2, kind=int64) /= self%row_count) then
            write(got, "(I0,A,I0)") size(is_valid, 1, kind=int64), " x ", size(is_valid, 2, kind=int64)
            write(want, "(I0,A,I0)") w, " x ", self%row_count
            call table_context_suffix(self%cache, name, sfx)
            error stop EP // trim(proc) // ": is_valid is shaped " // trim(got) // " but the " // &
                "column is " // trim(want) // " (width x rows)" // sfx
        end if
        do i = 1_int64, self%row_count
            do e = 1_int64, w
                if (.not. is_valid(e, i)) call self%cache%cols(idx)%values%set_null(i, e)
            end do
        end do
    end procedure table_apply_valid_elem
    !
    module procedure table_apply_valid_rows_elem
        integer(int64) :: k, e, w
        character(len=64) :: got, want
        character(len=:), allocatable :: sfx
        !
        w = int(self%cache%cols(idx)%values%colwidth(), int64)
        if (size(is_valid, 1, kind=int64) /= w .or. &
            size(is_valid, 2, kind=int64) /= size(rows, kind=int64)) then
            write(got, "(I0,A,I0)") size(is_valid, 1, kind=int64), " x ", size(is_valid, 2, kind=int64)
            write(want, "(I0,A,I0)") w, " x ", size(rows, kind=int64)
            call table_context_suffix(self%cache, name, sfx)
            error stop EP // trim(proc) // ": is_valid is shaped " // trim(got) // " but the " // &
                "selection is " // trim(want) // " (width x rows)" // sfx
        end if
        do k = 1_int64, size(rows, kind=int64)
            do e = 1_int64, w
                if (.not. is_valid(e, k)) call self%cache%cols(idx)%values%set_null(rows(k), e)
            end do
        end do
    end procedure table_apply_valid_rows_elem
    !
    module procedure table_require_slice_size
        character(len=32) :: got, want
        character(len=:), allocatable :: sfx
        !
        if (n_arr == n_rows) return
        write(got, "(I0)") n_arr
        write(want, "(I0)") n_rows
        call table_context_suffix(self%cache, name, sfx)
        error stop EP // trim(proc) // ": the array has " // trim(got) // " values but the " // &
            "selection picks " // trim(want) // " rows" // sfx
    end procedure table_require_slice_size
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
