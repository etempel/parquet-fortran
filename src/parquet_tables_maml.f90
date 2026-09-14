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
        call locate_extra_block(schema%maml%lines, "remap:", idx_extra, extra_end, idx_remap)
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
    !> .true. when `line` is exactly the MAML block header `key`, ignoring surrounding blanks and
    !! ASCII case — a MAML key is case-insensitive, so `Extra:` and `extra:` are the same section.
    !!
    !! A local twin of `parquet_core`'s `parquet_maml_key_matches`, which is private to that
    !! module's own subtree. This file already carries its own `split_key_value`/`unquote_trimmed`
    !! for the same reason (see the module doc above): the read-side parser is deliberately
    !! independent of the write-side one. What must not diverge is the RULE, and
    !! `check_maml_keys_case_insensitive` (`tools/check_source_conventions.py`) is what enforces
    !! that — it fails on any new literal `== "<key>:"` comparison in either subtree, which is the
    !! shape that once let a capitalized `Extra:` silently lose its whole block. Pinned by
    !! `test_maml_block_headers_case_insensitive` (`test/test_maml.f90`).
    !!
    !! Indentation is not considered here; a caller needing a top-level header keeps its own
    !! `line(1:1) /= " "` test. Values stay case-sensitive — a `remap:` column name is data.
    logical function maml_key_matches(line, key)
        character(len=*), intent(in) :: line !! raw MAML source line.
        character(len=*), intent(in) :: key  !! block-header name, lowercase, with its colon.
        character(len=len(line)) :: t
        character(len=1) :: c
        integer :: i, n
        !
        t = adjustl(line)
        n = len_trim(t)
        maml_key_matches = .false.
        if (n /= len_trim(key)) return
        do i = 1, n
            c = t(i:i)
            if (c >= "A" .and. c <= "Z") c = achar(iachar(c) + 32)
            if (c /= key(i:i)) return
        end do
        maml_key_matches = .true.
    end function maml_key_matches
    !
    !> Finds `key` nested inside the MAML's own `extra:` section, reporting the block bounds the
    !! caller then walks. `idx_key` is 0 when there is no `extra:` section at all, or none with
    !! that key in it.
    !!
    !! `remap:`/`filter:`/`sort:` are only meaningful nested under `extra:` -- exactly as
    !! `col_map:` is on the write side -- because `extra:` is the one MAML section whose contents
    !! the core validator accepts unexamined. A stray top-level `remap:` is therefore correctly
    !! rejected by that validator as an unknown section, and never reaches here.
    subroutine locate_extra_block(lines, key, idx_extra, extra_end, idx_key)
        character(len=*), intent(in) :: lines(:) !! raw MAML source lines.
        character(len=*), intent(in) :: key      !! nested key to find, with its colon ("remap:").
        integer, intent(out) :: idx_extra        !! line index of "extra:", or 0.
        integer, intent(out) :: extra_end        !! last line belonging to the extra: block.
        integer, intent(out) :: idx_key          !! line index of `key`, or 0.
        integer :: i, nlines
        !
        nlines = size(lines)
        idx_extra = 0
        extra_end = nlines
        idx_key = 0
        do i = 1, nlines
            if (len(lines(i)) == 0) cycle
            if (lines(i)(1:1) /= " " .and. maml_key_matches(lines(i), "extra:")) then
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
            if (maml_key_matches(lines(i), key)) then
                idx_key = i
                exit
            end if
        end do
    end subroutine locate_extra_block
    !
    module procedure parse_read_maml_string_list
        integer :: i, idx_extra, extra_end, idx_key, count, width
        character(len=:), allocatable :: tline, item
        !
        n = 0
        allocate(character(len=1) :: items(0))
        if (.not. allocated(schema%maml%lines)) return
        call locate_extra_block(schema%maml%lines, key, idx_extra, extra_end, idx_key)
        if (idx_key == 0) return
        ! Two passes, as in parse_read_maml_remap: one to size the deferred-length result, one to
        ! fill it.
        count = 0
        width = 1
        do i = idx_key + 1, extra_end
            if (len_trim(schema%maml%lines(i)) == 0) cycle
            tline = trim(adjustl(schema%maml%lines(i)))
            if (tline(1:1) /= "-") exit
            call unquote_trimmed(tline(2:), item)
            if (len(item) == 0) cycle
            count = count + 1
            width = max(width, len(item))
        end do
        if (count == 0) return
        deallocate(items)
        allocate(character(len=width) :: items(count))
        ! Blanked element by element -- see parse_read_maml_remap's own comment for why never
        ! `items = ""`.
        do i = 1, count
            items(i) = ""
        end do
        do i = idx_key + 1, extra_end
            if (len_trim(schema%maml%lines(i)) == 0) cycle
            tline = trim(adjustl(schema%maml%lines(i)))
            if (tline(1:1) /= "-") exit
            call unquote_trimmed(tline(2:), item)
            if (len(item) == 0) cycle
            n = n + 1
            items(n) = item
        end do
    end procedure parse_read_maml_string_list
    !
    module procedure split_sort_nulls_token
        integer :: sep
        character(len=:), allocatable :: tail, lowered
        integer :: i
        !
        nulls_first = .false.
        key_text = trim(adjustl(entry))
        ! The token is the LAST blank-separated word, and only when it is one of the two spellings
        ! -- so an ordinary two-word key ("ra asc") and a bare column name are both left alone, and
        ! anything else is handed to parquet_sortkey%add to reject with its own message.
        sep = index(trim(key_text), " ", back=.true.)
        if (sep == 0) return
        tail = trim(adjustl(key_text(sep + 1:)))
        lowered = tail
        do i = 1, len(lowered)
            if (lowered(i:i) >= "A" .and. lowered(i:i) <= "Z") lowered(i:i) = achar(iachar(lowered(i:i)) + 32)
        end do
        if (lowered == "nulls_first") then
            nulls_first = .true.
        else if (lowered /= "nulls_last") then
            return
        end if
        key_text = trim(key_text(1:sep - 1))
    end procedure split_sort_nulls_token
    !
    module procedure compose_read_transform
        type(parquet_schema), allocatable :: read_maml
        type(parquet_read_qc), allocatable :: qc_local
        type(parquet_sortkey) :: sort_local
        character(len=:), allocatable :: maml_rules(:), maml_keys(:), key_text
        integer :: i, n_rules, n_keys
        logical :: nulls_first
        !
        n_remap = 0
        n_qc = 0
        n_rules = 0
        n_keys = 0
        n_units = 0
        allocate(character(len=1) :: unit_cols(0))
        allocate(character(len=1) :: unit_vals(0))
        allocate(character(len=1) :: internal(0))
        allocate(character(len=1) :: physical(0))
        allocate(character(len=1) :: maml_rules(0))
        allocate(character(len=1) :: maml_keys(0))
        if (present(maml_file)) then
            read_maml = parquet_load_qc_maml_file(trim(maml_file))
            deallocate(internal, physical)
            call parse_read_maml_remap(read_maml, internal, physical, n_remap, trim(maml_file))
            deallocate(unit_cols, unit_vals)
            call collect_maml_units(read_maml, unit_cols, unit_vals, n_units)
            deallocate(maml_rules, maml_keys)
            call parse_read_maml_string_list(read_maml, "filter:", maml_rules, n_rules)
            call parse_read_maml_string_list(read_maml, "sort:", maml_keys, n_keys)
            ! The code-facing half of this rule needs no check at all -- the slice specifics of
            ! parquet_open_table have no `sort` argument, so supplying one does not compile. A
            ! MAML's own list cannot be caught that way, so it is caught here, before the file is
            ! opened: a sort reorders rows across the whole file, which would leave the slice's
            ! [row_lo, row_hi] naming a different set of rows than the caller chose it to.
            if (sliced .and. n_keys > 0) then
                error stop EP // "parquet_open_table: sort is not allowed in the slice regime; " // &
                    "remove extra: sort: from this maml or open the whole file (maml '" // &
                    trim(maml_file) // "')"
            end if
        end if
        !
        ! Filter: the caller's rules, translated out of internal names, then the MAML's own (already
        ! file names) appended. AND is commutative, so the order is a documentation choice only.
        if (present(filter)) then
            out_filter = filter
            call out_filter%remap_column_names(internal(1:n_remap), physical(1:n_remap))
        end if
        do i = 1, n_rules
            call out_filter%add(trim(maml_rules(i)))
        end do
        !
        ! Sort: MAML keys FIRST, because the first key is the primary one -- this half genuinely is
        ! order-sensitive, unlike the filter above.
        do i = 1, n_keys
            call split_sort_nulls_token(maml_keys(i), key_text, nulls_first)
            call out_sort%add(key_text, nulls_first=nulls_first)
        end do
        if (present(sort)) then
            sort_local = sort
            call sort_local%remap_column_names(internal(1:n_remap), physical(1:n_remap))
            do i = 1, sort_local%n
                call out_sort%add(trim(sort_local%keys(i)), nulls_first=sort_local%nulls_first(i))
            end do
        end if
        !
        ! qc: translated the same way, then merged per column by parquet_compose_read_qc. Both of
        ! its sources are passed as ALLOCATABLE locals left unallocated when absent -- an
        ! unallocated allocatable actual makes an optional dummy absent (F2018 15.5.2.12), which is
        ! what lets one call cover all four present/absent combinations without branching.
        if (present(qc)) then
            qc_local = qc
            call qc_local%remap_column_names(internal(1:n_remap), physical(1:n_remap))
        end if
        call parquet_compose_read_qc(read_maml, qc_local, out_qc, n_qc)
    end procedure compose_read_transform
    !
    !> Collects every `fields:` entry of a read-in MAML that declares a `unit:`, as a
    !! file-name/unit pair list.
    !!
    !! Walks the raw MAML lines, exactly as `parse_read_maml_remap` above does, and for the same
    !! reason: `parquet_load_qc_maml_file` deliberately does not parse a read-in MAML into
    !! `%cinfo` (its validation rules are the write side's, and a file-describing MAML need not
    !! satisfy them), so `%get_num_fields` would answer 0 here. The grammar recognized is the one
    !! `fields:` already uses everywhere else -- a list of `- name: X` items, each followed by
    !! indented `key: value` lines until the next item.
    !!
    !! The names are the MAML's own, i.e. the FILE's column names -- a read-in MAML describes the
    !! physical file, so `extra: remap:` renames its columns for the table but does not rename them
    !! here. open_table_impl therefore matches these against each slot's `file_name`, never its
    !! internal name.
    !!
    !! Fields declaring no unit are skipped rather than stored as "", so an absent entry and an
    !! empty one stay distinguishable and a column keeps whatever unit it had.
    subroutine collect_maml_units(read_maml, cols, vals, n)
        type(parquet_schema), intent(in) :: read_maml              !! the loaded read-in MAML.
        character(len=:), allocatable, intent(out) :: cols(:)      !! file column names declaring a unit.
        character(len=:), allocatable, intent(out) :: vals(:)      !! the unit each declares.
        integer, intent(out) :: n                                  !! live entries.
        character(len=:), allocatable :: fname, u
        integer :: i, first, last, count, width, pass
        !
        n = 0
        allocate(character(len=1) :: cols(0))
        allocate(character(len=1) :: vals(0))
        if (.not. allocated(read_maml%maml%lines)) return
        call locate_fields_block(read_maml%maml%lines, first, last)
        if (first == 0) return
        ! Two passes, as everywhere else in this file: one to size the deferred-length result,
        ! one to fill it.
        count = 0
        width = 1
        do pass = 1, 2
            if (pass == 2) then
                if (count == 0) return
                deallocate(cols, vals)
                allocate(character(len=width) :: cols(count))
                allocate(character(len=width) :: vals(count))
                ! Element by element, never `cols = ""` -- see parse_read_maml_remap's own note on
                ! what a whole-array assignment to a deferred-length allocatable array does here.
                do i = 1, count
                    cols(i) = ""
                    vals(i) = ""
                end do
            end if
            n = 0
            fname = ""
            u = ""
            do i = first, last
                if (len_trim(read_maml%maml%lines(i)) == 0) cycle
                call scan_field_line(read_maml%maml%lines(i), fname, u, pass, cols, vals, n, width)
            end do
            call emit_field_unit(fname, u, pass, cols, vals, n, width)
            if (pass == 1) count = n
        end do
    end subroutine collect_maml_units
    !
    !> One line of a `fields:` block: starts a new entry, records its `unit:`, or is ignored.
    subroutine scan_field_line(line, fname, u, pass, cols, vals, n, width)
        character(len=*), intent(in) :: line                  !! the raw MAML line.
        character(len=:), allocatable, intent(inout) :: fname !! name of the entry being scanned.
        character(len=:), allocatable, intent(inout) :: u     !! its unit so far ("" if none).
        integer, intent(in) :: pass                           !! 1 = counting, 2 = filling.
        character(len=*), intent(inout) :: cols(:)            !! result names (pass 2).
        character(len=*), intent(inout) :: vals(:)            !! result units (pass 2).
        integer, intent(inout) :: n                           !! entries emitted so far.
        integer, intent(inout) :: width                       !! widest entry seen (pass 1).
        character(len=:), allocatable :: tline, key, cvalue
        !
        tline = trim(adjustl(line))
        if (tline(1:1) == "-") then
            ! A new list item closes the previous one, whatever it had.
            call emit_field_unit(fname, u, pass, cols, vals, n, width)
            u = ""
            fname = ""
            call split_key_value(tline(2:), key, cvalue)
            if (key == "name") fname = cvalue
            return
        end if
        call split_key_value(tline, key, cvalue)
        if (key == "unit") u = cvalue
    end subroutine scan_field_line
    !
    !> Records one completed `fields:` entry, if it named a column and declared a unit.
    subroutine emit_field_unit(fname, u, pass, cols, vals, n, width)
        character(len=*), intent(in) :: fname      !! the entry's column name ("" if none).
        character(len=*), intent(in) :: u          !! its unit ("" if none).
        integer, intent(in) :: pass                !! 1 = counting, 2 = filling.
        character(len=*), intent(inout) :: cols(:) !! result names (pass 2).
        character(len=*), intent(inout) :: vals(:) !! result units (pass 2).
        integer, intent(inout) :: n                !! entries emitted so far.
        integer, intent(inout) :: width            !! widest entry seen (pass 1).
        !
        if (len_trim(fname) == 0 .or. len_trim(u) == 0) return
        n = n + 1
        if (pass == 1) then
            width = max(width, len_trim(fname), len_trim(u))
        else
            cols(n) = trim(fname)
            vals(n) = trim(u)
        end if
    end subroutine emit_field_unit
    !
    !> Bounds of the MAML's top-level `fields:` block: the lines after it, up to the next
    !! unindented line. `first` is 0 when there is no such section.
    subroutine locate_fields_block(lines, first, last)
        character(len=*), intent(in) :: lines(:) !! raw MAML source lines.
        integer, intent(out) :: first            !! first line inside the block, or 0.
        integer, intent(out) :: last             !! last line inside it.
        integer :: i, nlines
        !
        nlines = size(lines)
        first = 0
        last = nlines
        do i = 1, nlines
            if (len_trim(lines(i)) == 0) cycle
            if (lines(i)(1:1) /= " " .and. maml_key_matches(lines(i), "fields:")) then
                first = i + 1
                exit
            end if
        end do
        if (first == 0) return
        do i = first, nlines
            if (len_trim(lines(i)) == 0) cycle
            if (lines(i)(1:1) /= " " .and. lines(i)(1:1) /= "-") then
                last = i - 1
                return
            end if
        end do
    end subroutine locate_fields_block
    !
    !> Splits a `key: value` line into its two trimmed, unquoted halves. Both come back empty for
    !! a line with no colon, which every caller skips.
    subroutine split_key_value(line, key, cvalue)
        character(len=*), intent(in) :: line                 !! the line, already trimmed.
        character(len=:), allocatable, intent(out) :: key    !! the key, lowercase-as-written.
        character(len=:), allocatable, intent(out) :: cvalue !! the value, unquoted.
        integer :: colon
        !
        key = ""
        cvalue = ""
        colon = index(line, ":")
        if (colon <= 1) return
        key = trim(adjustl(line(1:colon - 1)))
        call unquote_trimmed(line(colon + 1:), cvalue)
    end subroutine split_key_value
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
        call cache_name_index_insert(cache, cache%ncols)
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
