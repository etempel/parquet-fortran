!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> Read-in (Role-B) MAML support for `parquet_table`: parsing the `extra: remap:` block that
!! relabels a file's columns for reading, and building the descriptor slots from it.
!!
!! A read-in MAML describes the PHYSICAL file and travels with it, so everything it names is a
!! file column name. `remap:` is the one place the two vocabularies meet: its keys are the
!! table-facing (internal) names a program will use, its values the file columns they read.
!! Everything downstream then keeps the split the descriptor already has -- `%name` is the lookup
!! key, `%file_name` is what the reader is asked for -- which is why remapping needs no change to
!! classification, materialization or residency at all.
!!
!! Three rules this file implements, all of them easy to get subtly wrong:
!!
!! * **An internal name may equal a physical column name it does not refer to.** A file often
!!   carries columns a program does not want, and one of them happening to share a name with an
!!   internal name must not make that name unusable. The physical column is then simply
!!   unreachable -- a deliberate, silent shadow, not an error.
!! * **Two internal names may read the SAME physical column.** They become two ordinary,
!!   independent columns from that point on: each materializes on its own first touch (reading the
!!   physical column again) and each may be mutated without the other seeing it.
!! * **File order is preserved.** Slots are emitted by walking the physical columns, not the remap
!!   list, so a struct's leaves stay adjacent -- which the batch release policy in
!!   `materialize_marked` depends on -- and duplicate slots land next to each other, where one
!!   decode serves both.
!!
!! The parser is deliberately independent of the write-side `parquet_parse_col_map`, which is
!! validated against a base schema a generic table has no equivalent of and serves the opposite
!! direction. It scans lines already loaded from disk (via `parquet_load_qc_maml_file`), so the
!! shared reader's line-length cap and CRLF handling apply without this file re-reading anything.
submodule (parquet_tables) parquet_tables_maml
    implicit none
    !
contains
    !
    module procedure parse_read_maml_remap
        integer :: i, j, idx_extra, extra_end, idx_remap, count, width
        character(len=:), allocatable :: tline, key, cvalue
        !
        n = 0
        allocate(character(len=1) :: internal(0))
        allocate(character(len=1) :: physical(0))
        if (.not. allocated(schema%maml%lines)) return
        ! Two passes: one to count and size the deferred-length result, one to fill it. Cheaper
        ! than growing a deferred-length array entry by entry, and a remap: block is tiny.
        call locate_remap_block(schema%maml%lines, idx_extra, extra_end, idx_remap)
        if (idx_remap == 0) return
        count = 0
        width = 1
        do i = idx_remap + 1, extra_end
            if (len_trim(schema%maml%lines(i)) == 0) cycle
            tline = trim(adjustl(schema%maml%lines(i)))
            if (tline(1:1) /= "-") exit
            call split_remap_entry(tline, key, cvalue)
            if (len(key) == 0 .or. len(cvalue) == 0) cycle
            count = count + 1
            width = max(width, len(key), len(cvalue))
        end do
        if (count == 0) return
        deallocate(internal)
        deallocate(physical)
        allocate(character(len=width) :: internal(count))
        allocate(character(len=width) :: physical(count))
        ! Blanked ELEMENT BY ELEMENT, never as `internal = ""`. A whole-array intrinsic assignment
        ! to a deferred-length allocatable array reallocates it to the RHS's length -- here zero --
        ! so the fill loop below would then write `width` bytes into a zero-length allocation: every
        ! name comes back blank AND the heap is corrupted. gfortran happens to keep the length and
        ! hides both; ifx follows the standard and shows them. An array ELEMENT is not itself an
        ! allocatable variable, so assigning to one only blank-pads, which is what is wanted.
        do i = 1, count
            internal(i) = ""
            physical(i) = ""
        end do
        do i = idx_remap + 1, extra_end
            if (len_trim(schema%maml%lines(i)) == 0) cycle
            tline = trim(adjustl(schema%maml%lines(i)))
            if (tline(1:1) /= "-") exit
            call split_remap_entry(tline, key, cvalue)
            if (len(key) == 0 .or. len(cvalue) == 0) cycle
            n = n + 1
            internal(n) = key
            physical(n) = cvalue
        end do
        ! A repeated internal name is ambiguous with no rule that could pick between the entries.
        ! Checked here rather than alongside the "does that column exist?" rule in validate_remap
        ! because it is a property of the MAML alone: catching it at parse time means it aborts
        ! before parquet_open_table has opened the file at all, with no reader (and so no live
        ! Arrow object) in scope when the process stops.
        do i = 2, n
            do j = 1, i - 1
                if (trim(internal(j)) /= trim(internal(i))) cycle
                error stop EP // "parquet_open_table: extra: remap: internal name '" // &
                    trim(internal(i)) // "' is declared more than once (maml '" // &
                    trim(maml_file) // "')"
            end do
        end do
    end procedure parse_read_maml_remap
    !
    !> Finds the `remap:` key nested inside the MAML's own `extra:` section, reporting the block
    !! bounds the caller then walks. `idx_remap` is 0 when there is no `extra:` section at all, or
    !! none with a `remap:` key in it.
    !!
    !! `remap:` is only meaningful nested under `extra:` -- exactly as `col_map:` is on the write
    !! side -- because `extra:` is the one MAML section whose contents the core validator accepts
    !! unexamined. A stray top-level `remap:` is therefore correctly rejected by that validator as
    !! an unknown section, and never reaches here.
    subroutine locate_remap_block(lines, idx_extra, extra_end, idx_remap)
        character(len=*), intent(in) :: lines(:) !! raw MAML source lines.
        integer, intent(out) :: idx_extra        !! line index of "extra:", or 0.
        integer, intent(out) :: extra_end        !! last line belonging to the extra: block.
        integer, intent(out) :: idx_remap        !! line index of "remap:", or 0.
        integer :: i, nlines
        !
        nlines = size(lines)
        idx_extra = 0
        extra_end = nlines
        idx_remap = 0
        do i = 1, nlines
            if (len(lines(i)) == 0) cycle
            if (lines(i)(1:1) /= " " .and. trim(adjustl(lines(i))) == "extra:") then
                idx_extra = i
                exit
            end if
        end do
        if (idx_extra == 0) return
        ! The block runs until the next unindented (top-level) line.
        do i = idx_extra + 1, nlines
            if (len_trim(lines(i)) == 0) cycle
            if (lines(i)(1:1) /= " ") then
                extra_end = i - 1
                exit
            end if
        end do
        do i = idx_extra + 1, extra_end
            if (len_trim(lines(i)) == 0) cycle
            if (trim(adjustl(lines(i))) == "remap:") then
                idx_remap = i
                exit
            end if
        end do
    end subroutine locate_remap_block
    !
    !> Splits one `- internal: physical` list item (leading dash already the first character) into
    !! its two unquoted halves. Both come back empty for a line that is not a well-formed entry,
    !! which the caller skips rather than aborting on -- the same tolerance the write-side
    !! `col_map:` parser applies to its own list.
    subroutine split_remap_entry(tline, key, cvalue)
        character(len=*), intent(in) :: tline                  !! trimmed list-item line, starting with "-".
        character(len=:), allocatable, intent(out) :: key      !! internal name, unquoted.
        character(len=:), allocatable, intent(out) :: cvalue   !! file column name, unquoted.
        character(len=:), allocatable :: body
        integer :: colon
        !
        key = ""
        cvalue = ""
        body = trim(adjustl(tline(2:)))
        if (len(body) == 0) return
        colon = index(body, ":")
        if (colon <= 1) return
        call unquote_trimmed(body(1:colon-1), key)
        call unquote_trimmed(body(colon+1:), cvalue)
    end subroutine split_remap_entry
    !
    !> Trims `raw` and strips one matching pair of surrounding single or double quotes, so a name
    !! that needs quoting in YAML (one containing a colon, say) is stored as the caller means it.
    !!
    !! A local equivalent of the core's own unquote helper rather than a call to it: that one is
    !! private to `parquet`, and making three string utilities public just to reach them would
    !! widen that module's API for no benefit (see this file's own module doc on staying
    !! independent of the write-side parser).
    subroutine unquote_trimmed(raw, res)
        character(len=*), intent(in) :: raw                 !! text to trim and unquote.
        character(len=:), allocatable, intent(out) :: res   !! trimmed, unquoted result.
        character(len=:), allocatable :: t
        !
        t = trim(adjustl(raw))
        if (len(t) >= 2) then
            if ((t(1:1) == '"' .and. t(len(t):len(t)) == '"') .or. &
                (t(1:1) == "'" .and. t(len(t):len(t)) == "'")) then
                res = t(2:len(t)-1)
                return
            end if
        end if
        res = t
    end subroutine unquote_trimmed
    !
    module procedure table_enumerate_columns
        integer :: i, j, nslots, claimed
        character(len=:), allocatable :: sfx
        !
        call validate_remap(names, internal, physical, n_remap, filename)
        !
        ! Slot count: every physical column contributes one slot unless its name is claimed as an
        ! internal name, plus one slot per remap entry. NOT size(names) any more -- two internal
        ! names may target one physical column, so slots can outnumber the file's own columns.
        nslots = n_remap
        do i = 1, size(names)
            if (remap_claims_internal(internal, n_remap, trim(names(i)))) cycle
            if (remap_targets_physical(physical, n_remap, trim(names(i)))) cycle
            nslots = nslots + 1
        end do
        allocate(cache%cols(nslots + COL_HEADROOM))
        cache%ncols = 0
        !
        ! Walk the PHYSICAL columns in file order, expanding each into whatever claims it. File
        ! order is what keeps a struct's leaves adjacent for the batch release policy, and what
        ! puts two internal names reading one column in neighbouring slots.
        do i = 1, size(names)
            claimed = 0
            do j = 1, n_remap
                if (trim(physical(j)) /= trim(names(i))) cycle
                claimed = claimed + 1
                call add_file_slot(cache, trim(internal(j)), trim(names(i)))
            end do
            if (claimed > 0) cycle
            ! Nothing remaps TO this column. It is still shadowed out if its own name was claimed
            ! as an internal name pointing somewhere else -- the deliberate silent shadow.
            if (remap_claims_internal(internal, n_remap, trim(names(i)))) cycle
            call add_file_slot(cache, trim(names(i)), trim(names(i)))
        end do
        !
        ! Every remap entry must have produced a slot; a target that does not exist was already
        ! rejected by validate_remap, so this is an assertion rather than a reachable error.
        if (cache%ncols /= nslots) then
            call table_context_suffix(cache, "", sfx) ! GCOVR_EXCL_LINE
            error stop EP // "parquet_open_table: internal error building the column list " // & ! GCOVR_EXCL_LINE
                "from extra: remap:" // sfx ! GCOVR_EXCL_LINE
        end if
    end procedure table_enumerate_columns
    !
    !> Appends one descriptor slot for a file-backed column: `name` is the lookup key, `file_name`
    !! the column actually read. The two differ only for a remapped column.
    subroutine add_file_slot(cache, name, file_name)
        type(parquet_table_cache), intent(inout) :: cache !! the column store gaining a slot.
        character(len=*), intent(in) :: name              !! internal/logical name.
        character(len=*), intent(in) :: file_name         !! physical name in the file.
        !
        cache%ncols = cache%ncols + 1
        cache%cols(cache%ncols)%name = name
        cache%cols(cache%ncols)%file_name = file_name
        cache%cols(cache%ncols)%file_source = .true.
    end subroutine add_file_slot
    !
    !> Whether any remap entry declares `name` as an INTERNAL name (i.e. claims that name for
    !! itself, whatever it points at).
    logical function remap_claims_internal(internal, n_remap, name) result(res)
        character(len=*), intent(in) :: internal(:) !! remap: table-facing names.
        integer, intent(in) :: n_remap              !! live entries.
        character(len=*), intent(in) :: name        !! name to look for.
        integer :: j
        !
        res = .false.
        do j = 1, n_remap
            if (trim(internal(j)) == name) then
                res = .true.
                return
            end if
        end do
    end function remap_claims_internal
    !
    !> Whether any remap entry TARGETS the physical column `name`.
    logical function remap_targets_physical(physical, n_remap, name) result(res)
        character(len=*), intent(in) :: physical(:) !! remap: file columns targeted.
        integer, intent(in) :: n_remap              !! live entries.
        character(len=*), intent(in) :: name        !! name to look for.
        integer :: j
        !
        res = .false.
        do j = 1, n_remap
            if (trim(physical(j)) == name) then
                res = .true.
                return
            end if
        end do
    end function remap_targets_physical
    !
    !> The one remap rule that needs the file: every entry must target a column it actually has.
    !! Checked before any slot is built, so a bad MAML fails while the table is still obviously
    !! unusable.
    !!
    !! The other rule -- no repeated internal name -- is enforced by `parse_read_maml_remap`
    !! instead, since it needs no file to detect and is better raised before one is opened.
    !!
    !! Deliberately NOT a rule: two entries targeting the same file column. That is supported --
    !! the two internal names become independent columns over one physical source (see this file's
    !! module doc).
    subroutine validate_remap(names, internal, physical, n_remap, filename)
        character(len=*), intent(in) :: names(:)    !! the file's own column names.
        character(len=*), intent(in) :: internal(:) !! remap: table-facing names.
        character(len=*), intent(in) :: physical(:) !! remap: the file column each one reads.
        integer, intent(in) :: n_remap              !! live entries.
        character(len=*), intent(in) :: filename    !! source file, for the message.
        integer :: i, j
        logical :: found
        !
        do i = 1, n_remap
            found = .false.
            do j = 1, size(names)
                if (trim(names(j)) == trim(physical(i))) then
                    found = .true.
                    exit
                end if
            end do
            if (.not. found) then
                error stop EP // "parquet_open_table: extra: remap: column '" // trim(physical(i)) // &
                    "' declared for internal name '" // trim(internal(i)) // "' does not exist in " // &
                    "this file (file '" // trim(filename) // "')"
            end if
        end do
    end subroutine validate_remap
    !
end submodule parquet_tables_maml
