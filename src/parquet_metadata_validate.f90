!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!> Bodies of the MAML validation module procedures declared in parquet.f90's
!> interface block: full-schema structural validation (parquet_validate_maml),
!> cross-checking a user MAML against a base schema (parquet_validate_user_maml),
!> loading MAML/qc-maml files from disk, and qc-maml field parsing/validation
!> for read-time quality control.
submodule (parquet) parquet_metadata_validate
    use ieee_arithmetic, only: ieee_is_nan
    implicit none
contains

    module procedure parquet_validate_user_maml
        type(parquet_column_info) :: base_cinfo, user_cinfo
        type(parquet_table_metadata) :: base_metadata, user_metadata
        character(len=:), allocatable :: bad_names, map_errors
        integer :: i, j
        logical :: found

        call parquet_validate_maml(base_maml)
        call parquet_validate_maml(user_maml)

        call parquet_parse_maml_lines(base_maml%lines, base_cinfo, base_metadata)
        call parquet_parse_maml_lines(user_maml%lines, user_cinfo, user_metadata)

        ! col_map: renames (col_internal -> col_user) are already applied to
        ! user_cinfo%col(:)%name by parquet_parse_maml_lines above -- every
        ! check below that compares names against base_cinfo therefore
        ! already operates on resolved internal names, with no changes
        ! needed. What's checked here, specific to col_map itself: every
        ! mapped internal name actually exists in the base schema, and the
        ! map has no internal-name duplicates or output-name collisions.
        user_maml%col_map = parquet_parse_col_map(user_maml%lines)
        map_errors = ""
        do i = 1, size(user_maml%col_map)
            found = .false.
            if (allocated(base_cinfo%col)) then
                do j = 1, size(base_cinfo%col)
                    if (trim(base_cinfo%col(j)%name) == trim(user_maml%col_map(i)%internal_name)) then
                        found = .true.
                        exit
                    end if
                end do
            end if
            if (.not. found) then
                map_errors = map_errors // "col_map: internal column '" // &
                    trim(user_maml%col_map(i)%internal_name) // "' not present in base MAML; "
            end if

            do j = 1, i - 1
                if (trim(user_maml%col_map(j)%internal_name) == trim(user_maml%col_map(i)%internal_name)) then
                    map_errors = map_errors // "col_map: duplicate internal column '" // &
                        trim(user_maml%col_map(i)%internal_name) // "'; "
                    exit
                end if
            end do

            do j = 1, i - 1
                if (trim(user_maml%col_map(j)%output_name) == trim(user_maml%col_map(i)%output_name)) then
                    map_errors = map_errors // "col_map: output name '" // &
                        trim(user_maml%col_map(i)%output_name) // "' used for more than one internal column; "
                    exit
                end if
            end do

            ! The renamed column must actually be declared in fields: under
            ! its output_name -- parquet_parse_maml_lines only renames a
            ! field it finds already declared as `output_name`; if none
            ! exists, the rename silently has nothing to apply to.
            found = .false.
            if (allocated(user_cinfo%col)) then
                do j = 1, size(user_cinfo%col)
                    if (trim(user_cinfo%col(j)%output_name) == trim(user_maml%col_map(i)%output_name)) then
                        found = .true.
                        exit
                    end if
                end do
            end if
            if (.not. found) then
                map_errors = map_errors // "col_map: renamed column '" // &
                    trim(user_maml%col_map(i)%output_name) // "' is not declared in fields:; "
            end if

            ! A remapped internal column must not also appear directly
            ! (un-renamed) in fields: -- that's ambiguous: was it meant to be
            ! renamed, or used as-is? (A field whose declared name matches
            ! internal_name but never got renamed keeps name == output_name
            ! == internal_name, since only a field declared under
            ! output_name is renamed.)
            if (allocated(user_cinfo%col)) then
                do j = 1, size(user_cinfo%col)
                    if (trim(user_cinfo%col(j)%name) == trim(user_maml%col_map(i)%internal_name) .and. &
                        trim(user_cinfo%col(j)%output_name) == trim(user_maml%col_map(i)%internal_name)) then ! GCOVR_EXCL_START
                        map_errors = map_errors // "col_map: internal column '" // &
                            trim(user_maml%col_map(i)%internal_name) // &
                            "' is remapped but also appears directly (un-renamed) in fields:; "
                        exit
                    end if ! GCOVR_EXCL_STOP
                end do
            end if

            ! The chosen output_name must not coincide with a *different*
            ! existing (base) column's own name: if it did, that other base
            ! column -- whether or not this user MAML mentions it -- would
            ! collide with the renamed one the moment it's ever activated
            ! (e.g. via set_column_available), since both would then share the same
            ! output_name in the written schema.
            if (allocated(base_cinfo%col)) then
                do j = 1, size(base_cinfo%col)
                    if (trim(base_cinfo%col(j)%name) == trim(user_maml%col_map(i)%output_name) .and. &
                        trim(base_cinfo%col(j)%name) /= trim(user_maml%col_map(i)%internal_name)) then
                        map_errors = map_errors // "col_map: output name '" // &
                            trim(user_maml%col_map(i)%output_name) // &
                            "' coincides with the existing base column of that name; "
                        exit
                    end if
                end do
            end if
        end do

        ! Guards against a renamed column's output_name silently colliding
        ! with another, unrelated field's own declared name (or with another
        ! renamed column's output_name): every field ending up in the
        ! schema must have a distinct output_name, since that's what
        ! actually gets registered/written to the parquet file.
        if (allocated(user_cinfo%col)) then
            do i = 1, size(user_cinfo%col)
                do j = 1, i - 1
                    if (trim(user_cinfo%col(j)%output_name) == trim(user_cinfo%col(i)%output_name)) then ! GCOVR_EXCL_START
                        map_errors = map_errors // "duplicate output name '" // &
                            trim(user_cinfo%col(i)%output_name) // "' used by more than one field in fields:; "
                        exit
                    end if ! GCOVR_EXCL_STOP
                end do
            end do
        end if

        if (len_trim(map_errors) > 0) then
            error stop "parquet_validate_user_maml: " // trim(map_errors)
        end if

        bad_names = ""
        if (allocated(user_cinfo%col)) then
            do i = 1, size(user_cinfo%col)
                found = .false.
                if (allocated(base_cinfo%col)) then
                    do j = 1, size(base_cinfo%col)
                        if (trim(base_cinfo%col(j)%name) == trim(user_cinfo%col(i)%name)) then
                            found = .true.
                            exit
                        end if
                    end do
                end if
                if (.not. found) then
                    if (len_trim(bad_names) > 0) bad_names = bad_names // ", "
                    bad_names = bad_names // trim(user_cinfo%col(i)%name)
                end if
            end do
        end if

        if (len_trim(bad_names) > 0) then
            error stop "parquet_validate_user_maml: columns not present in base MAML: " // trim(bad_names)
        end if

        if (allocated(user_maml%missing_columns)) deallocate(user_maml%missing_columns)
        user_maml%user_maml = .false.

        if (allocated(base_cinfo%col)) then
            do i = 1, size(base_cinfo%col)
                found = .false.
                if (allocated(user_cinfo%col)) then
                    do j = 1, size(user_cinfo%col)
                        if (trim(user_cinfo%col(j)%name) == trim(base_cinfo%col(i)%name)) then
                            found = .true.
                            exit
                        end if
                    end do
                end if
                if (.not. found) then
                    user_maml%user_maml = .true.
                    call parquet_append_missing_column(user_maml, base_cinfo%col(i))
                end if
            end do
        end if
    end procedure parquet_validate_user_maml

    !> Appends `col` (a base-schema column absent from `maml`) to
    !> maml%missing_columns, so parquet_merge_missing_columns can later
    !> restore it as a disabled/deactivated column.
    subroutine parquet_append_missing_column(maml, col)
        type(parquet_maml_file), intent(inout) :: maml !! user MAML gaining one missing-column entry.
        type(parquet_column_type), intent(in) :: col !! base-schema column that maml doesn't declare.
        type(parquet_maml_missing_column), allocatable :: tmp(:)
        integer :: n

        ! See g_maml_mutex in parquet_wrapper.cpp.
        call parquet_maml_lock()
        if (.not. allocated(maml%missing_columns)) then
            allocate(maml%missing_columns(1))
            n = 1
        else
            allocate(tmp(size(maml%missing_columns) + 1))
            tmp(1:size(maml%missing_columns)) = maml%missing_columns
            call move_alloc(tmp, maml%missing_columns)
            n = size(maml%missing_columns)
        end if

        maml%missing_columns(n)%name = col%name
        maml%missing_columns(n)%unit = col%unit
        maml%missing_columns(n)%info = col%info
        maml%missing_columns(n)%ucd = col%ucd
        maml%missing_columns(n)%data_type = col%data_type
        maml%missing_columns(n)%array_size = col%array_size
        maml%missing_columns(n)%col_size = col%col_size
        call parquet_maml_unlock()
    end subroutine parquet_append_missing_column

    module procedure parquet_validate_maml_internal
        type(parquet_column_info) :: cinfo
        type(parquet_table_metadata) :: metadata
        character(len=:), allocatable :: errors
        character(len=:), allocatable :: cur_name
        character(len=:), allocatable :: protected_names(:)
        character(len=32) :: idx_buf
        integer :: i, j
        logical :: type_ok, has_table, found
        real(real64) :: qc_bound_value

        call parquet_parse_maml_lines(maml%lines, cinfo, metadata)

        errors = ""

        if (.not. allocated(cinfo%col)) then
            errors = errors // "no fields defined; " ! GCOVR_EXCL_LINE
        else if (size(cinfo%col) == 0) then
            errors = errors // "no fields defined; "
        else
            do i = 1, size(cinfo%col)
                cur_name = trim(cinfo%col(i)%name)

                if (len_trim(cur_name) == 0) then ! GCOVR_EXCL_START
                    write(idx_buf, '(I0)') i
                    errors = errors // "field #" // trim(idx_buf) // " has an empty name; "
                    cycle
                end if ! GCOVR_EXCL_STOP

                type_ok = .false.
                do j = 1, size(valid_maml_data_types)
                    if (trim(cinfo%col(i)%data_type) == trim(valid_maml_data_types(j))) then
                        type_ok = .true.
                        exit
                    end if
                end do
                if (.not. type_ok) then
                    errors = errors // "field '" // cur_name // "' has invalid data_type '" // &
                        trim(cinfo%col(i)%data_type) // "'; "
                end if

                do j = 1, i - 1
                    if (trim(cinfo%col(j)%name) == cur_name) then
                        errors = errors // "duplicate field name '" // cur_name // "'; "
                        exit
                    end if
                end do

                ! qc: min: must use a lower-bound operator (>= or >) and qc:
                ! max: an upper-bound operator (<= or <); the opposite
                ! direction (e.g. min: '< 5') is a nonsensical bound. This is
                ! a purely syntactic check, applied to every enforced type
                ! (numeric and string alike); boolean's qc: is silently
                ! ignored entirely (see the numeric block below), so it's
                ! exempt here too.
                if (trim(cinfo%col(i)%data_type) /= "boolean") then
                    if (cinfo%col(i)%has_qc_min .and. cinfo%col(i)%qc_min_op(1:1) == "<") then
                        errors = errors // "field '" // cur_name // "' has a qc: min value with a '" // &
                            trim(cinfo%col(i)%qc_min_op) // "' operator; min: accepts only >= or > " // &
                            "(use max: for an upper bound); "
                    end if
                    if (cinfo%col(i)%has_qc_max .and. cinfo%col(i)%qc_max_op(1:1) == ">") then
                        errors = errors // "field '" // cur_name // "' has a qc: max value with a '" // &
                            trim(cinfo%col(i)%qc_max_op) // "' operator; max: accepts only <= or < " // &
                            "(use min: for a lower bound); "
                    end if
                end if

                ! qc: min:/max: numeric convertibility only applies to the
                ! numeric types; string uses its bound as a literal (nothing
                ! to convert, so it can't fail), and boolean's qc: is always
                ! silently ignored (never enforced), so it isn't checked here.
                select case (trim(cinfo%col(i)%data_type))
                case ("int32", "int64", "float32", "float64")
                    if (cinfo%col(i)%has_qc_min) then
                        if (.not. parquet_qc_numeric_bound( &
                                cinfo%col(i)%qc_min_raw, cinfo%col(i)%data_type, qc_bound_value)) then
                            errors = errors // "field '" // cur_name // "' has an invalid qc: min value '" // &
                                trim(cinfo%col(i)%qc_min_raw) // "' for data_type " // trim(cinfo%col(i)%data_type) // "; "
                        end if
                    end if
                    if (cinfo%col(i)%has_qc_max) then
                        if (.not. parquet_qc_numeric_bound( &
                                cinfo%col(i)%qc_max_raw, cinfo%col(i)%data_type, qc_bound_value)) then ! GCOVR_EXCL_START
                            errors = errors // "field '" // cur_name // "' has an invalid qc: max value '" // &
                                trim(cinfo%col(i)%qc_max_raw) // "' for data_type " // trim(cinfo%col(i)%data_type) // "; "
                        end if ! GCOVR_EXCL_STOP
                    end if
                end select
            end do
        end if

        has_table = .false.
        if (allocated(metadata%items)) then
            do i = 1, size(metadata%items)
                if (trim(metadata%items(i)%key) == "table") then
                    has_table = len_trim(metadata%items(i)%value) > 0
                    exit
                end if
            end do
        end if
        if (.not. has_table) errors = errors // "missing required non-empty metadata: table; "

        ! extra: protected_cols: may only name columns declared under this
        ! same MAML's own fields: (matched by output_name -- see
        ! parquet_parse_maml_lines); anything else is a typo/dangling reference.
        call parquet_parse_protected_cols(maml%lines, protected_names)
        if (allocated(cinfo%col)) then
            do i = 1, size(protected_names)
                found = .false.
                do j = 1, size(cinfo%col)
                    if (trim(protected_names(i)) == trim(cinfo%col(j)%output_name)) then
                        found = .true.
                        exit
                    end if
                end do
                if (.not. found) then
                    errors = errors // "protected_cols: unknown column '" // trim(protected_names(i)) // "'; "
                end if
            end do
        end if

        call parquet_validate_maml_sections(maml%lines, errors)

        if (len_trim(errors) > 0) then
            error stop "parquet_validate_maml: " // trim(errors)
        end if
    end procedure parquet_validate_maml_internal

    !> Loads maml (a filename) from disk and validates it (parquet_load_maml_file
    !> already validates internally, but this keeps that requirement explicit
    !> and self-contained here rather than depending on that side effect).
    module procedure parquet_validate_maml_file
        type(parquet_maml_file) :: loaded_maml

        loaded_maml = parquet_load_maml_file(maml)
        call parquet_validate_maml_internal(loaded_maml)
    end procedure parquet_validate_maml_file

    module procedure parquet_load_maml_file
        character(len=1024), allocatable :: lines(:)
        character(len=1024) :: line
        integer :: unit, ios, nlines, i, max_len

        nlines = 0
        open(newunit=unit, file=trim(filename), status="old", action="read", iostat=ios)
        if (ios /= 0) error stop "parquet_load_maml_file: cannot open file: " // trim(filename)

        do
            read(unit, '(A)', iostat=ios) line
            if (ios /= 0) exit
            nlines = nlines + 1
            call parquet_append_line(lines, nlines, line)
        end do

        close(unit)

        maml%name = trim(filename)

        max_len = 1
        do i = 1, nlines
            max_len = max(max_len, len_trim(lines(i)))
        end do

        allocate(character(len=max_len) :: maml%lines(nlines))
        do i = 1, nlines
            maml%lines(i) = lines(i)(1:max_len)
        end do

        call parquet_validate_maml(maml)
    end procedure parquet_load_maml_file

    module procedure parquet_load_qc_maml_file
        character(len=1024), allocatable :: lines(:)
        character(len=1024) :: line
        integer :: unit, ios, nlines, i, max_len

        nlines = 0
        open(newunit=unit, file=trim(filename), status="old", action="read", iostat=ios)
        if (ios /= 0) error stop "parquet_load_qc_maml_file: cannot open file: " // trim(filename)

        do
            read(unit, '(A)', iostat=ios) line
            if (ios /= 0) exit
            nlines = nlines + 1
            call parquet_append_line(lines, nlines, line)
        end do

        close(unit)

        schema%maml%name = trim(filename)

        max_len = 1
        do i = 1, nlines
            max_len = max(max_len, len_trim(lines(i)))
        end do

        allocate(character(len=max_len) :: schema%maml%lines(nlines))
        do i = 1, nlines
            schema%maml%lines(i) = lines(i)(1:max_len)
        end do
        ! Deliberately no parquet_validate_maml call here -- a qc-maml has its
        ! own, lighter validation (parquet_parse_qc_maml), run later once
        ! parquet_open_reader actually uses it.
    end procedure parquet_load_qc_maml_file

    !> Grows `rules(:)` by one empty entry and increments `n` -- same
    !> grow-by-one-element pattern as parquet_append_line/parquet_filter_add
    !> elsewhere in this codebase; qc-maml field counts are always small, so
    !> no capacity-doubling scheme is warranted.
    subroutine parquet_qc_append_empty_rule(rules, n)
        type(parquet_qc_rule), allocatable, intent(inout) :: rules(:) !! rule array being grown.
        integer, intent(inout) :: n !! number of rules in use; incremented by 1.
        type(parquet_qc_rule), allocatable :: tmp(:)

        n = n + 1
        if (.not. allocated(rules)) then
            allocate(rules(1))
            return
        end if
        if (size(rules) < n) then
            allocate(tmp(n))
            tmp(1:n-1) = rules
            call move_alloc(tmp, rules)
        end if
    end subroutine parquet_qc_append_empty_rule

    module procedure parquet_parse_qc_maml
        character(len=1024) :: line
        character(len=:), allocatable :: tline, key, cvalue, raw, errors, miss_lower
        character(len=:), allocatable :: qc_maml_suffix
        logical :: in_fields, have_current, in_qc
        integer :: i, j, n
        character(len=32) :: idx_buf
        type(parquet_qc_rule), allocatable :: tmp(:)
        character(len=:), allocatable :: tlo1, tlo3, tlo4 !! scratch (to_lower).
        character(len=:), allocatable :: tuq2, tuq5 !! scratch (unquote).

        ! "" if maml%name was never set (e.g. a qc-maml built in memory via
        ! add_col_qc); otherwise " (maml: X)", appended to every error stop
        ! below so it names which qc-maml file failed validation.
        qc_maml_suffix = ""
        if (allocated(maml%name)) then
            if (len_trim(maml%name) > 0) qc_maml_suffix = " (maml: " // trim(maml%name) // ")"
        end if

        ! Reuses the same top-level-section/sub-key name schema every other
        ! MAML validation path checks against (allowed_maml_sections/
        ! allowed_maml_nested_sections) -- so a typo'd section name or an
        ! unrecognized fields:/qc: sub-key is still caught here, exactly as
        ! it would be for a schema-authoring maml. Everything else
        ! parquet_validate_maml_internal additionally requires (table:, at
        ! least one field, valid data_type, ...) is deliberately NOT applied
        ! to a qc-maml -- see parquet_qc_rule's own doc comment.
        errors = ""
        call parquet_validate_maml_sections(maml%lines, errors)
        if (len_trim(errors) > 0) then
            error stop "parquet_open_reader: invalid qc maml: " // trim(errors) // qc_maml_suffix
        end if

        in_fields = .false.
        have_current = .false.
        in_qc = .false.
        n = 0

        do i = 1, size(maml%lines)
            line = maml%lines(i)
            tline = trim(adjustl(line))
            if (len_trim(tline) == 0) cycle
            if (tline(1:1) == "#") cycle

            if (.not. in_fields) then
                if (tline == "fields:") in_fields = .true.
                cycle
            end if

            ! A new top-level section (unindented, has a ":", not a dash
            ! item) ends the fields: block, same as parquet_parse_maml_lines.
            if (index(tline, "- ") /= 1 .and. index(tline, ":") > 0 .and. line(1:1) /= " ") exit

            if (index(tline, "-") == 1 .and. line(1:1) /= " ") then
                call parquet_qc_append_empty_rule(tmp, n)
                have_current = .true.
                in_qc = .false.
                tline = trim(adjustl(tline(2:)))
                if (len_trim(tline) == 0) cycle
            end if

            if (.not. have_current) cycle

            if (in_qc) then
                call parquet_split_key_value(tline, key, cvalue)
                call parquet_to_lower(key, tlo1)
                select case (tlo1)
                case ("min")
                    call parquet_set_qc_bound(tmp(n)%has_min, tmp(n)%min_op, raw, cvalue, ">=")
                    tmp(n)%min_text = raw
                    cycle
                case ("max")
                    call parquet_set_qc_bound(tmp(n)%has_max, tmp(n)%max_op, raw, cvalue, "<=")
                    tmp(n)%max_text = raw
                    cycle
                case ("miss")
                    call parquet_unquote(cvalue, tuq2)
                    call parquet_to_lower(tuq2, tlo3)
                    miss_lower = trim(tlo3)
                    if (len_trim(miss_lower) == 0) then
                        tmp(n)%null_values_allowed = .false.
                    else if (trim(miss_lower) == "null" .or. trim(miss_lower) == "na") then
                        tmp(n)%null_values_allowed = .true.
                    else
                        error stop "parquet_open_reader: invalid qc maml: qc: miss: value '" // trim(miss_lower) // &
                            "' for field '" // trim(tmp(n)%name) // &
                            "' is not recognized (expected Null/NA or empty)" // qc_maml_suffix
                    end if
                    cycle
                case default
                    in_qc = .false.
                end select
            end if

            call parquet_split_key_value(tline, key, cvalue)
            if (len_trim(key) == 0) cycle

            call parquet_to_lower(key, tlo4)
            select case (tlo4)
            case ("name")
                call parquet_unquote(cvalue, tuq5)
                tmp(n)%name = tuq5
            case ("qc")
                in_qc = .true.
                tmp(n)%has_qc_block = .true.
            end select
        end do

        do i = 1, n
            if (len_trim(tmp(i)%name) == 0) then
                write(idx_buf, '(I0)') i
                error stop "parquet_open_reader: invalid qc maml: field #" // trim(idx_buf) // &
                    " is missing required 'name'" // qc_maml_suffix
            end if
            do j = 1, i - 1
                if (trim(tmp(j)%name) == trim(tmp(i)%name)) then
                    error stop "parquet_open_reader: invalid qc maml: duplicate field name '" // trim(tmp(i)%name) // &
                        "'" // qc_maml_suffix
                end if
            end do
            ! qc: min: must be a lower bound (>= or >), qc: max: an upper
            ! bound (<= or <); the reversed direction is a nonsensical bound.
            ! Unlike the write side this can't (and needn't) consult a
            ! data_type -- a qc-maml has none -- so it applies to every field.
            if (tmp(i)%has_min .and. tmp(i)%min_op(1:1) == "<") then
                error stop "parquet_open_reader: invalid qc maml: qc: min: for field '" // trim(tmp(i)%name) // &
                    "' uses a '" // trim(tmp(i)%min_op) // "' operator; min: accepts only >= or > (use max: for an upper bound)" &
                    // qc_maml_suffix
            end if
            if (tmp(i)%has_max .and. tmp(i)%max_op(1:1) == ">") then
                error stop "parquet_open_reader: invalid qc maml: qc: max: for field '" // trim(tmp(i)%name) // &
                    "' uses a '" // trim(tmp(i)%max_op) // "' operator; max: accepts only <= or < (use min: for a lower bound)" &
                    // qc_maml_suffix
            end if
        end do

        ! Fields with just a name: and no qc: block at all get no rule --
        ! same as a field never mentioned in this maml (see parquet_qc_rule's
        ! has_qc_block doc comment).
        allocate(rules(0))
        do i = 1, n
            if (tmp(i)%has_qc_block) rules = [rules, tmp(i)]
        end do
    end procedure parquet_parse_qc_maml

    module procedure parquet_set_qc_bound
        character(len=:), allocatable :: text
        character(len=:), allocatable :: tuq1 !! scratch (unquote).

        call parquet_unquote(cvalue, tuq1)
        text = trim(adjustl(tuq1))
        if (index(text, ">=") == 1) then
            op = ">="
            text = trim(adjustl(text(3:)))
        else if (index(text, "<=") == 1) then
            op = "<="
            text = trim(adjustl(text(3:)))
        else if (index(text, ">") == 1) then
            op = "> "
            text = trim(adjustl(text(2:)))
        else if (index(text, "<") == 1) then
            op = "< "
            text = trim(adjustl(text(2:)))
        else
            op = default_op
        end if
        raw = text
        has_flag = .true.
    end procedure parquet_set_qc_bound

    module procedure parquet_qc_numeric_bound
        integer :: ios
        real(real64) :: rounded

        value = 0.0_real64
        parquet_qc_numeric_bound = .false.

        read(raw, *, iostat=ios) value
        if (ios /= 0) return
        if (ieee_is_nan(value)) return
        if (.not. (abs(value) <= huge(1.0_real64))) return ! Inf (or a magnitude beyond real64's finite range)

        select case (trim(data_type))
        case ("int32")
            rounded = anint(value)
            if (value /= rounded) return
            if (rounded < -real(huge(0_int32), real64) - 1.0_real64 .or. rounded > real(huge(0_int32), real64)) return
        case ("int64")
            rounded = anint(value)
            if (value /= rounded) return
            if (rounded < -real(huge(0_int64), real64) .or. rounded >= real(huge(0_int64), real64)) return
        end select

        parquet_qc_numeric_bound = .true.
    end procedure parquet_qc_numeric_bound

end submodule parquet_metadata_validate
