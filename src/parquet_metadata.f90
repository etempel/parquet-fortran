!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!> Bodies of the schema-building module procedures declared in parquet.f90's
!> interface block: parsing a MAML into a parquet_schema (parquet_parse_maml),
!> building a schema from scratch (schema%init/%add_field), and the central
!> parquet_parse_maml_lines parser, plus the private MAML-section helpers
!> (keyarray:/DOI/depends/keywords/col_map/protected_cols) it depends on.
submodule (parquet) parquet_metadata
    implicit none

contains

    module procedure parquet_parse_maml_from_file
        schema%maml = parquet_load_maml_file(filename)
        call parquet_parse_maml_lines(schema%maml%lines, schema%cinfo, schema%metadata)
        call parquet_merge_missing_columns(schema%maml, schema%cinfo)
        schema%metadata%source_maml_lines = schema%maml%lines
    end procedure parquet_parse_maml_from_file

    module procedure parquet_parse_maml_from_object
        if (.not. allocated(schema%maml%lines)) &
            error stop "parquet_parse_maml: schema%maml has no loaded content " // &
                "(use the filename form, or populate schema%maml first)" ! GCOVR_EXCL_LINE
        call parquet_validate_maml(schema%maml)
        call parquet_parse_maml_lines(schema%maml%lines, schema%cinfo, schema%metadata)
        call parquet_merge_missing_columns(schema%maml, schema%cinfo)
        schema%metadata%source_maml_lines = schema%maml%lines
    end procedure parquet_parse_maml_from_object

    ! ---- schema%init / schema%add_field: building a MAML from scratch -----
    ! These emit raw MAML text lines into schema%maml%lines, the same
    ! representation parquet_parse_maml_lines below eventually parses --
    ! %add_field validates eagerly (name/data_type/duplicate/qc) but the
    ! result is still just text, so parquet_parse_maml must be called
    ! afterward to populate %cinfo/%metadata, exactly as for a MAML loaded
    ! from disk.

    !> Appends one line to maml%lines, growing the (deferred-length) array and
    !> renormalizing its element length to fit. A smaller, independent copy of
    !> the identically-named helper in parquet_maml_base_add_col_qc.f90 -- that
    !> one is private to a different module (parquet_maml_base sits below this
    !> one in the module stack) and cannot be reused here without a new public
    !> API neither add_col_qc nor add_field need for anything else.
    subroutine maml_push_line(maml, s)
        type(parquet_maml_file), intent(inout) :: maml !! schema being built; gains one more source line.
        character(len=*), intent(in) :: s !! line to append.
        character(len=:), allocatable :: tmp(:)
        integer :: n, newlen, i

        if (allocated(maml%lines)) then
            n = size(maml%lines)
        else
            n = 0
        end if
        newlen = len_trim(s)
        if (n > 0) newlen = max(newlen, len(maml%lines))
        if (newlen < 1) newlen = 1

        allocate(character(len=newlen) :: tmp(n + 1))
        do i = 1, n
            tmp(i) = maml%lines(i)
        end do
        tmp(n + 1) = s
        call move_alloc(tmp, maml%lines)
    end subroutine maml_push_line

    !> True if any line of maml%lines equals `target` after trimming leading
    !> and trailing blanks (used for the fields: header check).
    logical function maml_line_exists(maml, target) result(found)
        type(parquet_maml_file), intent(in) :: maml !! schema being built.
        character(len=*), intent(in) :: target !! line text to look for (matched after trim(adjustl(...))).
        integer :: i

        found = .false.
        if (.not. allocated(maml%lines)) return ! GCOVR_EXCL_LINE
        do i = 1, size(maml%lines)
            if (trim(adjustl(maml%lines(i))) == trim(target)) then
                found = .true.
                return
            end if
        end do
    end function maml_line_exists

    !> True if maml%lines already declares a fields: entry named `name`, i.e.
    !> a "- name: <name>" line.
    logical function maml_field_name_exists(maml, name) result(found)
        type(parquet_maml_file), intent(in) :: maml !! schema being built.
        character(len=*), intent(in) :: name !! field name to look for (case-sensitive).
        integer :: i, colon
        character(len=:), allocatable :: t, key, val

        found = .false.
        if (.not. allocated(maml%lines)) return ! GCOVR_EXCL_LINE
        do i = 1, size(maml%lines)
            t = trim(adjustl(maml%lines(i)))
            if (len(t) == 0) cycle
            if (t(1:1) /= "-") cycle
            t = trim(adjustl(t(2:)))
            colon = index(t, ":")
            if (colon <= 1) cycle
            key = trim(adjustl(t(1:colon-1)))
            if (parquet_to_lower(key) /= "name") cycle
            val = trim(adjustl(t(colon+1:)))
            if (len(val) >= 2) then
                if ((val(1:1) == '"' .and. val(len(val):len(val)) == '"') .or. &
                    (val(1:1) == "'" .and. val(len(val):len(val)) == "'")) then
                    val = val(2:len(val)-1) ! GCOVR_EXCL_LINE
                end if
            end if
            if (trim(val) == trim(name)) then
                found = .true.
                return
            end if
        end do
    end function maml_field_name_exists

    module procedure schema_init
        if (this%is_initialized) then
            error stop "parquet_schema%init: schema is already initialized"
        end if
        if (len_trim(table) == 0) then
            error stop "parquet_schema%init: table must not be empty"
        end if

        ! Gives an in-memory schema (no source .maml file) a name anyway, so
        ! writer_maml_suffix and parquet_close_writer's missing-write error
        ! can still identify which schema they're complaining about.
        this%maml%name = "internal:" // trim(table)

        call maml_push_line(this%maml, "table: " // trim(table))
        if (present(survey))       call maml_push_line(this%maml, "survey: " // trim(survey))
        if (present(dataset))      call maml_push_line(this%maml, "dataset: " // trim(dataset))
        if (present(version))     call maml_push_line(this%maml, "version: " // trim(version))
        if (present(date))         call maml_push_line(this%maml, "date: " // trim(date))
        if (present(author))      call maml_push_line(this%maml, "author: " // trim(author))
        if (present(description)) call maml_push_line(this%maml, "description: " // trim(description))
        if (present(license))     call maml_push_line(this%maml, "license: " // trim(license))
        if (present(maml_version)) call maml_push_line(this%maml, "MAML_version: " // trim(maml_version))

        this%is_initialized = .true.
    end procedure schema_init

    module procedure parquet_schema_new
        call this%init(table=table, survey=survey, dataset=dataset, version=version, date=date, &
                author=author, description=description, license=license, maml_version=maml_version)
    end procedure parquet_schema_new

    !> "" if maml%name was never set; otherwise " (maml: X)". schema%add_field
    !> can only be reached after schema%init (checked below), and %init/
    !> parquet_schema(...) always give an in-memory schema a name
    !> ("internal:<table>"), so this is effectively always populated for
    !> every add_field error below it.
    function maml_name_suffix(maml) result(suffix)
        type(parquet_maml_file), intent(in) :: maml !! schema being built.
        character(len=:), allocatable :: suffix !! " (maml: X)"-style suffix, or "".

        suffix = ""
        if (allocated(maml%name)) then
            if (len_trim(maml%name) > 0) suffix = " (maml: " // trim(maml%name) // ")"
        end if
    end function maml_name_suffix

    module procedure schema_add_field
        logical :: type_ok
        integer :: j
        character(len=:), allocatable :: miss_low
        character(len=32) :: buf
        logical :: have_qc_min, have_qc_max, have_qc_miss

        if (.not. this%is_initialized) then
            error stop "parquet_schema%add_field: call schema%init(...) before adding fields"
        end if

        if (len_trim(name) == 0) then
            error stop "parquet_schema%add_field: field name must not be empty" // maml_name_suffix(this%maml)
        end if

        if (maml_field_name_exists(this%maml, trim(name))) then
            error stop "parquet_schema%add_field: duplicate field name '" // trim(name) // "'" // maml_name_suffix(this%maml)
        end if

        type_ok = .false.
        do j = 1, size(valid_maml_data_types)
            if (trim(data_type) == trim(valid_maml_data_types(j))) then
                type_ok = .true.
                exit
            end if
        end do
        if (.not. type_ok) then
            error stop "parquet_schema%add_field: field '" // trim(name) // "' has invalid data_type '" // &
                trim(data_type) // "'" // maml_name_suffix(this%maml)
        end if

        call validate_qc_bound(qc_min, .true.)
        call validate_qc_bound(qc_max, .false.)

        if (present(qc_miss)) then
            if (len_trim(qc_miss) > 0) then
                miss_low = parquet_to_lower(trim(adjustl(qc_miss)))
                if (.not. (miss_low == "null" .or. miss_low == "na")) then
                    error stop "parquet_schema%add_field: invalid qc_miss value '" // trim(adjustl(qc_miss)) // &
                        "' for field '" // trim(name) // "' (expected Null/NA or empty)" // maml_name_suffix(this%maml)
                end if
            end if
        end if

        if (.not. maml_line_exists(this%maml, "fields:")) call maml_push_line(this%maml, "fields:")

        call maml_push_line(this%maml, "- name: " // trim(name))
        if (present(unit)) call maml_push_line(this%maml, "  unit: " // trim(unit))
        if (present(info)) call maml_push_line(this%maml, "  info: " // trim(info))
        if (present(ucd))  call maml_push_line(this%maml, "  ucd: " // trim(ucd))
        call maml_push_line(this%maml, "  data_type: " // trim(data_type))

        if (present(array_size)) then
            write(buf, '(I0)') array_size
            call maml_push_line(this%maml, "  array_size: " // trim(buf))
        end if
        if (present(col_size)) then
            write(buf, '(I0)') col_size
            call maml_push_line(this%maml, "  col_size: " // trim(buf))
        end if

        have_qc_min = .false.
        if (present(qc_min)) have_qc_min = len_trim(qc_min) > 0
        have_qc_max = .false.
        if (present(qc_max)) have_qc_max = len_trim(qc_max) > 0
        have_qc_miss = .false.
        if (present(qc_miss)) have_qc_miss = len_trim(qc_miss) > 0

        if (have_qc_min .or. have_qc_max .or. have_qc_miss) then
            call maml_push_line(this%maml, "  qc:")
            if (present(qc_min)) then
                if (len_trim(qc_min) > 0) &
                    call maml_push_line(this%maml, "    min: '" // trim(adjustl(qc_min)) // "'")
            end if
            if (present(qc_max)) then
                if (len_trim(qc_max) > 0) &
                    call maml_push_line(this%maml, "    max: '" // trim(adjustl(qc_max)) // "'")
            end if
            if (present(qc_miss)) then
                if (len_trim(qc_miss) > 0) &
                    call maml_push_line(this%maml, "    miss: " // trim(adjustl(qc_miss)))
            end if
        end if

    contains

        !> Validates one qc_min/qc_max bound (absent or empty -- nothing to
        !> check). If an operator prefix is present it must point the right
        !> way (min: >=/>, max: <=/<) and be followed by a non-empty value; a
        !> bare value with no operator is accepted as-is. Mirrors the rule
        !> %add_col_qc enforces for its own min:/max: fields, checked
        !> independently here -- see schema_add_field's doc comment (parquet.f90)
        !> for why these two aren't unified into one implementation.
        subroutine validate_qc_bound(raw, is_min)
            character(len=*), intent(in), optional :: raw !! qc_min/qc_max text, absent or empty means nothing to check.
            logical, intent(in) :: is_min !! .true. when validating qc_min (accepts >=/>); .false. for qc_max (<=/<).
            character(len=:), allocatable :: t, rem
            character(len=2) :: op
            logical :: has_op

            if (.not. present(raw)) return
            t = trim(adjustl(raw))
            if (len_trim(t) == 0) return

            has_op = .true.
            if (index(t, ">=") == 1) then
                op = ">="; rem = trim(adjustl(t(3:)))
            else if (index(t, "<=") == 1) then
                op = "<="; rem = trim(adjustl(t(3:)))
            else if (index(t, ">") == 1) then
                op = "> "; rem = trim(adjustl(t(2:)))
            else if (index(t, "<") == 1) then
                op = "< "; rem = trim(adjustl(t(2:)))
            else
                has_op = .false.; rem = t
            end if

            if (has_op) then
                if (is_min .and. op(1:1) == "<") then
                    error stop "parquet_schema%add_field: qc_min for field '" // trim(name) // "' uses a '" // &
                        trim(op) // "' operator; qc_min accepts only >= or > (use qc_max for an upper bound)" // &
                        maml_name_suffix(this%maml)
                end if
                if (.not. is_min .and. op(1:1) == ">") then
                    error stop "parquet_schema%add_field: qc_max for field '" // trim(name) // "' uses a '" // &
                        trim(op) // "' operator; qc_max accepts only <= or < (use qc_min for a lower bound)" // &
                        maml_name_suffix(this%maml)
                end if
                if (len_trim(rem) == 0) then
                    if (is_min) then
                        error stop "parquet_schema%add_field: bad qc_min value provided for field '" // trim(name) // &
                            "'" // maml_name_suffix(this%maml)
                    else
                        error stop "parquet_schema%add_field: bad qc_max value provided for field '" // trim(name) // &
                            "'" // maml_name_suffix(this%maml)
                    end if
                end if
            end if
        end subroutine validate_qc_bound

    end procedure schema_add_field

    ! ---- parquet_schema flat convenience passthroughs ---------------------
    ! Each simply forwards to the matching procedure on %cinfo, %metadata or
    ! %maml. Absent optional arguments propagate unchanged.

    module procedure set_column_available
        call this%cinfo%set_column_available(name)
    end procedure set_column_available

    module procedure set_column_unavailable
        call this%cinfo%set_column_unavailable(name)
    end procedure set_column_unavailable

    module procedure schema_get_column_index
        schema_get_column_index = this%cinfo%get_column_index(name)
    end procedure schema_get_column_index

    module procedure schema_get_num_fields
        schema_get_num_fields = this%cinfo%get_num_fields()
    end procedure schema_get_num_fields

    module procedure schema_get_field_name
        name = this%cinfo%get_field_name(index)
    end procedure schema_get_field_name

    module procedure schema_add_col_qc
        call this%maml%add_col_qc(qc_input, col_name)
    end procedure schema_add_col_qc

    module procedure schema_get_col_qc
        col_name = this%maml%get_col_qc(qc_input)
    end procedure schema_get_col_qc

    module procedure schema_add_metadata_int32
        call this%metadata%add_metadata(key, value, description)
    end procedure schema_add_metadata_int32

    module procedure schema_add_metadata_int64
        call this%metadata%add_metadata(key, value, description)
    end procedure schema_add_metadata_int64

    module procedure schema_add_metadata_float32
        call this%metadata%add_metadata(key, value, description, fmt)
    end procedure schema_add_metadata_float32

    module procedure schema_add_metadata_float64
        call this%metadata%add_metadata(key, value, description, fmt)
    end procedure schema_add_metadata_float64

    module procedure schema_add_metadata_logical
        call this%metadata%add_metadata(key, value, description)
    end procedure schema_add_metadata_logical

    module procedure schema_add_metadata_string
        call this%metadata%add_metadata(key, value, description)
    end procedure schema_add_metadata_string

    module procedure schema_add_metadata_int32_array
        call this%metadata%add_metadata(key, value, description)
    end procedure schema_add_metadata_int32_array

    module procedure schema_add_metadata_int64_array
        call this%metadata%add_metadata(key, value, description)
    end procedure schema_add_metadata_int64_array

    module procedure schema_add_metadata_float32_array
        call this%metadata%add_metadata(key, value, description, fmt)
    end procedure schema_add_metadata_float32_array

    module procedure schema_add_metadata_float64_array
        call this%metadata%add_metadata(key, value, description, fmt)
    end procedure schema_add_metadata_float64_array

    module procedure schema_add_metadata_logical_array
        call this%metadata%add_metadata(key, value, description)
    end procedure schema_add_metadata_logical_array

    module procedure schema_add_metadata_string_array
        call this%metadata%add_metadata(key, value, description)
    end procedure schema_add_metadata_string_array

    !> Restores every column in maml%missing_columns (populated by
    !> parquet_validate_user_maml for a user MAML that omits base-schema
    !> columns) back into cinfo%col, as disabled/deactivated entries -- so
    !> they exist for lookups but are never written and can't be re-enabled
    !> via set_column_available. No-op unless maml%user_maml is set and it
    !> actually has missing columns recorded.
    subroutine parquet_merge_missing_columns(maml, cinfo)
        type(parquet_maml_file), intent(in) :: maml !! validated user MAML, possibly with missing_columns recorded.
        type(parquet_column_info), intent(inout) :: cinfo !! schema gaining one disabled entry per missing column.
        type(parquet_column_type), allocatable :: merged(:)
        integer :: n_old, n_new, i

        if (.not. maml%user_maml) return
        if (.not. allocated(maml%missing_columns)) return
        if (size(maml%missing_columns) == 0) return

        ! See g_maml_mutex in parquet_wrapper.cpp.
        call parquet_maml_lock()
        n_old = 0
        if (allocated(cinfo%col)) n_old = size(cinfo%col)
        n_new = size(maml%missing_columns)

        allocate(merged(n_old + n_new))
        if (n_old > 0) merged(1:n_old) = cinfo%col

        do i = 1, n_new
            merged(n_old + i)%name = maml%missing_columns(i)%name
            merged(n_old + i)%unit = maml%missing_columns(i)%unit
            merged(n_old + i)%info = maml%missing_columns(i)%info
            merged(n_old + i)%ucd = maml%missing_columns(i)%ucd
            merged(n_old + i)%data_type = maml%missing_columns(i)%data_type
            merged(n_old + i)%array_size = maml%missing_columns(i)%array_size
            merged(n_old + i)%col_size = maml%missing_columns(i)%col_size
            merged(n_old + i)%is_set = .false.
            merged(n_old + i)%is_deactivated = .true.
            merged(n_old + i)%output_name = maml%missing_columns(i)%name
        end do

        call move_alloc(merged, cinfo%col)
        call parquet_maml_unlock()
    end subroutine parquet_merge_missing_columns

    module procedure parquet_parse_maml_lines
        type(parquet_column_type), allocatable :: tmp(:)
        character(len=1024) :: line
        character(len=:), allocatable :: tline, key, cvalue
        logical :: in_fields, have_current, in_list, in_field_list, in_keyarray, in_doiarray, in_dependsarray, in_extra
        logical :: in_qc
        character(len=:), allocatable :: list_key, field_list_key, list_item
        character(len=:), allocatable :: ka_key, ka_value, ka_comment
        character(len=:), allocatable :: doi_value, doi_type
        character(len=:), allocatable :: depends_survey, depends_dataset, depends_table, depends_version
        character(len=:), allocatable :: keywords_value
        type(parquet_maml_col_map_entry), allocatable :: col_map(:)
        integer :: ios, n, i, j, list_item_idx, doi_idx, depends_idx
        character(len=32) :: idx_buf

        ! See g_maml_mutex in parquet_wrapper.cpp: this function's repeated
        ! "grow tmp(:), whole-array-assign the old contents in, move_alloc"
        ! pattern is not safely reentrant under genuine concurrent threads,
        ! even with -frecursive -- this lock keeps concurrent MAML parsing
        ! correct (serialized) rather than racing (recursive: this function
        ! calls other locked helpers, e.g. parquet_metadata_append_entry).
        call parquet_maml_lock()

        in_fields = .false.
        have_current = .false.
        in_list = .false.
        in_field_list = .false.
        in_keyarray = .false.
        in_doiarray = .false.
        in_dependsarray = .false.
        in_extra = .false.
        in_qc = .false.
        list_item_idx = 0
        doi_idx = 0
        depends_idx = 0
        n = 0
        list_key = ""
        field_list_key = ""
        ka_key = ""
        ka_value = ""
        ka_comment = ""
        doi_value = ""
        doi_type = ""
        depends_survey = ""
        depends_dataset = ""
        depends_table = ""
        depends_version = ""
        keywords_value = ""
        if (allocated(metadata%items)) deallocate(metadata%items)

        do i = 1, size(lines)
            line = lines(i)

            tline = trim(adjustl(line))
            if (len_trim(tline) == 0) cycle
            if (tline(1:1) == "#") cycle

            if (.not. in_fields) then
                if (in_keyarray) then
                    if (index(tline, "-") == 1 .and. line(1:1) /= " ") then
                        call parquet_flush_keyarray_item(metadata, ka_key, ka_value, ka_comment)
                        tline = trim(adjustl(tline(2:)))
                        if (len_trim(tline) > 0) then
                            call parquet_split_key_value(tline, key, cvalue)
                            if (parquet_to_lower(key) == "key") ka_key = parquet_unquote(cvalue)
                        end if
                        cycle
                    else if (line(1:1) == " " .and. index(tline, ":") > 0) then
                        call parquet_split_key_value(tline, key, cvalue)
                        select case (parquet_to_lower(key))
                        case ("key")
                            ka_key = parquet_unquote(cvalue)
                        case ("value")
                            ka_value = parquet_unquote(cvalue)
                        case ("comment")
                            ka_comment = parquet_unquote(cvalue)
                        end select
                        cycle
                    else
                        call parquet_flush_keyarray_item(metadata, ka_key, ka_value, ka_comment)
                        in_keyarray = .false.
                    end if
                end if

                if (in_doiarray) then
                    if (index(tline, "-") == 1 .and. line(1:1) /= " ") then
                        call parquet_flush_doi_item(metadata, doi_idx, doi_value, doi_type)
                        doi_value = ""
                        doi_type = ""
                        tline = trim(adjustl(tline(2:)))
                        if (len_trim(tline) > 0) then
                            call parquet_split_key_value(tline, key, cvalue)
                            if (parquet_to_lower(key) == "doi") doi_value = parquet_unquote(cvalue)
                        end if
                        cycle
                    else if (line(1:1) == " " .and. index(tline, ":") > 0) then
                        call parquet_split_key_value(tline, key, cvalue)
                        select case (parquet_to_lower(key))
                        case ("doi")
                            doi_value = parquet_unquote(cvalue)
                        case ("type")
                            doi_type = parquet_unquote(cvalue)
                        end select
                        cycle
                    else
                        call parquet_flush_doi_item(metadata, doi_idx, doi_value, doi_type)
                        in_doiarray = .false.
                    end if
                end if

                if (in_dependsarray) then
                    if (index(tline, "-") == 1 .and. line(1:1) /= " ") then
                        call parquet_flush_depends_item(metadata, depends_idx, &
                            depends_survey, depends_dataset, depends_table, depends_version)
                        depends_survey = ""
                        depends_dataset = ""
                        depends_table = ""
                        depends_version = ""
                        tline = trim(adjustl(tline(2:)))
                        if (len_trim(tline) > 0) then
                            call parquet_split_key_value(tline, key, cvalue)
                            select case (parquet_to_lower(key))
                            case ("survey")
                                depends_survey = parquet_unquote(cvalue)
                            case ("dataset")
                                depends_dataset = parquet_unquote(cvalue)
                            case ("table")
                                depends_table = parquet_unquote(cvalue)
                            case ("version")
                                depends_version = parquet_unquote(cvalue)
                            end select
                        end if
                        cycle
                    else if (line(1:1) == " " .and. index(tline, ":") > 0) then
                        call parquet_split_key_value(tline, key, cvalue)
                        select case (parquet_to_lower(key))
                        case ("survey")
                            depends_survey = parquet_unquote(cvalue)
                        case ("dataset")
                            depends_dataset = parquet_unquote(cvalue)
                        case ("table")
                            depends_table = parquet_unquote(cvalue)
                        case ("version")
                            depends_version = parquet_unquote(cvalue)
                        end select
                        cycle
                    else
                        call parquet_flush_depends_item(metadata, depends_idx, &
                            depends_survey, depends_dataset, depends_table, depends_version)
                        in_dependsarray = .false.
                    end if
                end if

                if (in_extra) then
                    ! extra:'s content is fully opaque/discarded: unlike the
                    ! generic in_list handler below, no metadata entry is
                    ! ever produced for it, whether its children are nested
                    ! maps or a top-level dash list (col_map: parsing, which
                    ! specifically looks inside extra:, works directly off
                    ! `lines`, independent of this skip).
                    if ((line(1:1) /= " " .and. index(tline, "-") /= 1)) then
                        in_extra = .false.
                    else
                        cycle
                    end if
                end if

                if (in_list) then
                    if (index(tline, "- ") == 1) then
                        list_item_idx = list_item_idx + 1
                        if (list_key == "comments" .or. list_key == "comment") then
                            write(idx_buf, '(I0)') list_item_idx
                            call metadata%add_metadata("comment_" // trim(idx_buf), parquet_unquote(tline(3:)))
                        else if (list_key == "coauthors" .or. list_key == "coauthor") then
                            write(idx_buf, '(I0)') list_item_idx
                            call metadata%add_metadata("coauthor_" // trim(idx_buf), parquet_unquote(tline(3:)))
                        else if (list_key == "keywords" .or. list_key == "keyword") then
                            if (len_trim(keywords_value) > 0) then
                                keywords_value = trim(keywords_value) // ";" // trim(parquet_unquote(tline(3:)))
                            else
                                keywords_value = trim(parquet_unquote(tline(3:)))
                            end if
                        else
                            call metadata%add_metadata(list_key, parquet_unquote(tline(3:)))
                        end if
                        cycle
                    else if (index(tline, ":") > 0 .and. line(1:1) /= " ") then
                        if (list_key == "keywords" .or. list_key == "keyword") then
                            call parquet_flush_keywords(metadata, keywords_value)
                            keywords_value = ""
                        end if
                        in_list = .false.
                    else
                        cycle
                    end if
                end if

                if (tline == "fields:") in_fields = .true.
                if (in_fields) cycle

                if (tline == "keyarray:") then
                    in_keyarray = .true.
                    ka_key = ""
                    ka_value = ""
                    ka_comment = ""
                    cycle
                end if

                if (parquet_to_lower(tline) == "dois:") then
                    in_doiarray = .true.
                    doi_idx = 0
                    doi_value = ""
                    doi_type = ""
                    cycle
                end if

                if (parquet_to_lower(tline) == "depends:") then
                    in_dependsarray = .true.
                    depends_idx = 0
                    depends_survey = ""
                    depends_dataset = ""
                    depends_table = ""
                    depends_version = ""
                    cycle
                end if

                if (parquet_to_lower(tline) == "extra:") then
                    in_extra = .true.
                    cycle
                end if

                call parquet_split_key_value(tline, key, cvalue)
                if (len_trim(key) == 0) cycle

                key = parquet_to_lower(key)

                if (key == "fields") then
                    in_fields = .true.
                else if (len_trim(cvalue) > 0) then
                    call metadata%add_metadata(key, parquet_unquote(cvalue))
                else
                    in_list = .true.
                    list_key = key
                    list_item_idx = 0
                end if
                cycle
            end if

            if (index(tline, "- ") /= 1 .and. index(tline, ":") > 0 .and. line(1:1) /= " ") exit

            if (index(tline, "-") == 1 .and. line(1:1) /= " ") then
                call parquet_append_empty_cinfo(tmp, n)
                have_current = .true.
                tline = trim(adjustl(tline(2:)))
                if (len_trim(tline) == 0) cycle
            end if

            if (.not. have_current) cycle

            if (in_field_list) then
                if (index(tline, "- ") == 1 .and. line(1:1) == " ") then
                    list_item = parquet_unquote(tline(3:))
                    select case (field_list_key)
                    case ("ucd")
                        if (.not. allocated(tmp(n)%ucd) .or. len_trim(tmp(n)%ucd) == 0) then
                            tmp(n)%ucd = trim(list_item)
                        else
                            tmp(n)%ucd = trim(tmp(n)%ucd) // ";" // trim(list_item)
                        end if
                    end select
                    cycle
                else
                    in_field_list = .false.
                    field_list_key = ""
                end if
            end if

            if (in_qc) then
                call parquet_split_key_value(tline, key, cvalue)
                select case (parquet_to_lower(key))
                case ("min")
                    call parquet_set_qc_bound(tmp(n)%has_qc_min, tmp(n)%qc_min_op, tmp(n)%qc_min_raw, cvalue, ">=")
                    cycle
                case ("max")
                    call parquet_set_qc_bound(tmp(n)%has_qc_max, tmp(n)%qc_max_op, tmp(n)%qc_max_raw, cvalue, "<=")
                    cycle
                case ("miss")
                    cycle
                case default
                    in_qc = .false.
                end select
            end if

            call parquet_split_key_value(tline, key, cvalue)
            if (len_trim(key) == 0) cycle

            select case (parquet_to_lower(key))
            case ("name")
                tmp(n)%name = parquet_unquote(cvalue)
            case ("unit")
                tmp(n)%unit = parquet_unquote(cvalue)
                if (parquet_to_lower(tmp(n)%unit) == "unitless") tmp(n)%unit = ""
            case ("info")
                tmp(n)%info = parquet_unquote(cvalue)
            case ("ucd")
                if (len_trim(cvalue) > 0) then
                    tmp(n)%ucd = parquet_unquote(cvalue)
                else
                    tmp(n)%ucd = ""
                    in_field_list = .true.
                    field_list_key = "ucd"
                end if
            case ("data_type")
                cvalue = parquet_to_lower(parquet_unquote(cvalue))
                if (index(cvalue, "string") == 1) then
                    cvalue = "string"
                end if
                tmp(n)%data_type = cvalue
            case ("array_size")
                read(cvalue, *, iostat=ios) tmp(n)%array_size
                if (ios /= 0) tmp(n)%array_size = 1
            case ("col_size")
                read(cvalue, *, iostat=ios) tmp(n)%col_size
                if (ios /= 0) tmp(n)%col_size = 1
            case ("qc")
                in_qc = .true.
            end select
        end do

        if (in_keyarray) call parquet_flush_keyarray_item(metadata, ka_key, ka_value, ka_comment)
        if (in_doiarray) call parquet_flush_doi_item(metadata, doi_idx, doi_value, doi_type)
        if (in_dependsarray) call parquet_flush_depends_item(metadata, depends_idx, &
            depends_survey, depends_dataset, depends_table, depends_version)
        if (in_list .and. (list_key == "keywords" .or. list_key == "keyword")) then
            call parquet_flush_keywords(metadata, keywords_value)
        end if

        if (n <= 0) then
            allocate(cinfo%col(0))
            call parquet_maml_unlock()
            return
        end if

        do i = 1, n
            if (.not. allocated(tmp(i)%name)) error stop "parquet_read_maml: missing field name in fields block"
            if (.not. allocated(tmp(i)%data_type)) error stop "parquet_read_maml: missing data_type in fields block"
            if (.not. allocated(tmp(i)%unit)) tmp(i)%unit = ""
            if (.not. allocated(tmp(i)%info)) tmp(i)%info = ""
            if (.not. allocated(tmp(i)%ucd)) tmp(i)%ucd = ""
            if (tmp(i)%array_size <= 0) tmp(i)%array_size = 1
            if (tmp(i)%col_size <= 0) tmp(i)%col_size = 1
            tmp(i)%is_set = .true.
        end do

        ! Apply this MAML's own col_map: (if any): a field declared under
        ! `output_name` in fields: is renamed in place to `internal_name`,
        ! keeping its originally-declared name as output_name. Unmapped
        ! fields keep output_name == name (identity). See
        ! parquet_column_type%output_name and parquet_parse_col_map.
        col_map = parquet_parse_col_map(lines)
        do i = 1, n
            tmp(i)%output_name = tmp(i)%name
            do j = 1, size(col_map)
                if (trim(col_map(j)%output_name) == trim(tmp(i)%name)) then
                    tmp(i)%name = col_map(j)%internal_name
                    exit
                end if
            end do
        end do

        ! extra: protected_cols: names this MAML's own fields: (matched
        ! by output_name, the name as literally declared under fields: in
        ! this file, before any col_map: rename) -- marked here so
        ! parquet_write_column can error stop if an is_valid mask with any
        ! .false. entry is ever passed for one of these columns.
        block
            character(len=:), allocatable :: protected_names(:)
            call parquet_parse_protected_cols(lines, protected_names)
            do i = 1, n
                do j = 1, size(protected_names)
                    if (trim(protected_names(j)) == trim(tmp(i)%output_name)) then
                        tmp(i)%is_protected = .true.
                        exit
                    end if
                end do
            end do
        end block

        call move_alloc(tmp, cinfo%col)
        call parquet_maml_unlock()
    end procedure parquet_parse_maml_lines

    module procedure parquet_append_keyarray_line
        character(len=:), allocatable :: entries(:), new_lines(:)
        integer :: insert_pos, n_old, n_new, new_len
        logical :: need_header

        ! See g_maml_mutex in parquet_wrapper.cpp.
        call parquet_maml_lock()
        call parquet_locate_keyarray_insert(lines, insert_pos, need_header)

        new_len = max(len(lines), 7+len(key), 9+len(value), 11+len(desc))

        if (need_header) then
            allocate(character(len=new_len) :: entries(4))
            entries(1) = "keyarray:"
            entries(2) = "- key: " // key
            entries(3) = "  value: " // value
            entries(4) = "  comment: " // desc
        else
            allocate(character(len=new_len) :: entries(3))
            entries(1) = "- key: " // key
            entries(2) = "  value: " // value
            entries(3) = "  comment: " // desc
        end if

        n_old = size(lines)
        n_new = size(entries)

        allocate(character(len=new_len) :: new_lines(n_old + n_new))
        if (insert_pos > 1) new_lines(1:insert_pos-1) = lines(1:insert_pos-1)
        new_lines(insert_pos:insert_pos+n_new-1) = entries
        if (insert_pos <= n_old) new_lines(insert_pos+n_new:) = lines(insert_pos:n_old)

        call move_alloc(new_lines, lines)
        call parquet_maml_unlock()
    end procedure parquet_append_keyarray_line

    !> Locates where a new keyarray entry should be inserted in `lines`.
    !> If a top-level `keyarray:` header already exists, `insert_pos` points
    !> just past its last item (need_header = .false.). Otherwise, `insert_pos`
    !> points at the top-level `extra:` line if present, else at the top-level
    !> `fields:` line (always present), and need_header = .true.
    subroutine parquet_locate_keyarray_insert(lines, insert_pos, need_header)
        character(len=*), intent(in) :: lines(:) !! raw MAML source lines to scan.
        integer, intent(out) :: insert_pos !! 1-based line index to insert the new entry (or header) at.
        logical, intent(out) :: need_header !! .true. if a keyarray: header line must be synthesized too.
        integer :: i, n
        character(len=:), allocatable :: tline

        n = size(lines)
        need_header = .true.
        insert_pos = n + 1

        do i = 1, n
            tline = trim(adjustl(lines(i)))
            if (lines(i)(1:1) /= " " .and. tline == "keyarray:") then
                need_header = .false.
                insert_pos = i + 1
                do while (insert_pos <= n)
                    if (len_trim(lines(insert_pos)) == 0) exit
                    if (lines(insert_pos)(1:1) /= " " .and. lines(insert_pos)(1:1) /= "-") exit
                    insert_pos = insert_pos + 1
                end do
                return
            end if
        end do

        do i = 1, n
            tline = trim(adjustl(lines(i)))
            if (lines(i)(1:1) /= " " .and. tline == "extra:") then
                insert_pos = i
                return
            end if
        end do

        do i = 1, n
            tline = trim(adjustl(lines(i)))
            if (lines(i)(1:1) /= " " .and. tline == "fields:") then
                insert_pos = i
                return
            end if
        end do
    end subroutine parquet_locate_keyarray_insert

    !> Appends one parsed `keyarray:` item (key/value/comment) as a table
    !> metadata entry; a no-op if `ka_key` is empty (an incomplete/malformed item).
    subroutine parquet_flush_keyarray_item(metadata, ka_key, ka_value, ka_comment)
        type(parquet_table_metadata), intent(inout) :: metadata !! table metadata gaining one entry.
        character(len=*), intent(in) :: ka_key !! parsed key: value.
        character(len=*), intent(in) :: ka_value !! parsed value: value.
        character(len=*), intent(in) :: ka_comment !! parsed comment: value (used as the entry's description).

        if (len_trim(ka_key) == 0) return
        call metadata%add_metadata(trim(ka_key), trim(ka_value), trim(ka_comment))
    end subroutine parquet_flush_keyarray_item

    !> Appends one parsed `DOIs:` list item as a "DOI_N" table metadata entry
    !> (N = doi_idx, incremented here); a no-op if `doi_value` is empty.
    subroutine parquet_flush_doi_item(metadata, doi_idx, doi_value, doi_type)
        type(parquet_table_metadata), intent(inout) :: metadata !! table metadata gaining one entry.
        integer, intent(inout) :: doi_idx !! running DOI count so far; incremented by 1 on a real flush.
        character(len=*), intent(in) :: doi_value !! parsed DOI: value.
        character(len=*), intent(in) :: doi_type !! parsed type: value (used as the entry's description).
        character(len=32) :: idx_buf

        if (len_trim(doi_value) == 0) return
        doi_idx = doi_idx + 1
        write(idx_buf, '(I0)') doi_idx
        call metadata%add_metadata("DOI_" // trim(idx_buf), trim(doi_value), trim(doi_type))
    end subroutine parquet_flush_doi_item

    !> Combines one `depends:` list entry's survey/dataset/table/version
    !> sub-keys into a single "survey;dataset;table;version" string, stored
    !> as table-level metadata "depends_N" (matching the coauthor_N/comment_N
    !> naming already used for other simple list sections). Skipped entirely
    !> if the entry had none of the four sub-keys set.
    subroutine parquet_flush_depends_item(metadata, depends_idx, survey, dataset, table, version)
        type(parquet_table_metadata), intent(inout) :: metadata !! table metadata gaining one entry.
        integer, intent(inout) :: depends_idx !! running depends: count so far; incremented by 1 on a real flush.
        character(len=*), intent(in) :: survey !! parsed survey: sub-key.
        character(len=*), intent(in) :: dataset !! parsed dataset: sub-key.
        character(len=*), intent(in) :: table !! parsed table: sub-key.
        character(len=*), intent(in) :: version !! parsed version: sub-key.
        character(len=32) :: idx_buf

        if (len_trim(survey) == 0 .and. len_trim(dataset) == 0 .and. &
            len_trim(table) == 0 .and. len_trim(version) == 0) return

        depends_idx = depends_idx + 1
        write(idx_buf, '(I0)') depends_idx
        call metadata%add_metadata("depends_" // trim(idx_buf), &
            trim(survey) // ";" // trim(dataset) // ";" // trim(table) // ";" // trim(version))
    end subroutine parquet_flush_depends_item

    !> Stores a `keywords:` (or `keyword:`) plain-string list as a single
    !> semicolon-separated "keywords" metadata entry, rather than one entry
    !> per item (which is what a generic plain-string list gets otherwise).
    subroutine parquet_flush_keywords(metadata, keywords_value)
        type(parquet_table_metadata), intent(inout) :: metadata !! table metadata gaining the keywords entry.
        character(len=*), intent(in) :: keywords_value !! semicolon-joined keywords: list text.

        if (len_trim(keywords_value) == 0) return ! GCOVR_EXCL_LINE
        call metadata%add_metadata("keywords", trim(keywords_value))
    end subroutine parquet_flush_keywords

    module procedure parquet_parse_col_map
        type(parquet_maml_col_map_entry), allocatable :: tmp(:)
        character(len=:), allocatable :: tline, key, cvalue
        integer :: i, n, idx_extra, extra_end, idx_col_map, n_entries

        allocate(col_map(0))

        ! col_map: is only valid nested inside extra: (extra:'s own internal
        ! structure is otherwise entirely unvalidated/opaque -- see
        ! allowed_maml_sections -- so this is a dedicated, narrow lookup
        ! rather than a generically-validated section of its own).
        n = size(lines)
        idx_extra = 0
        do i = 1, n
            if (lines(i)(1:1) /= " " .and. trim(adjustl(lines(i))) == "extra:") then
                idx_extra = i
                exit
            end if
        end do
        if (idx_extra == 0) return

        ! extra:'s block runs until the next top-level (non-indented) line.
        extra_end = n
        do i = idx_extra + 1, n
            if (len_trim(lines(i)) == 0) cycle
            if (lines(i)(1:1) /= " ") then
                extra_end = i - 1
                exit
            end if
        end do

        idx_col_map = 0
        do i = idx_extra + 1, extra_end
            if (len_trim(lines(i)) == 0) cycle
            if (trim(adjustl(lines(i))) == "col_map:") then
                idx_col_map = i
                exit
            end if
        end do
        if (idx_col_map == 0) return

        ! See g_maml_mutex in parquet_wrapper.cpp.
        call parquet_maml_lock()
        do i = idx_col_map + 1, extra_end
            if (len_trim(lines(i)) == 0) cycle

            tline = trim(adjustl(lines(i)))
            if (tline(1:1) /= "-") exit
            tline = trim(adjustl(tline(2:)))
            if (len_trim(tline) == 0) cycle

            call parquet_split_key_value(tline, key, cvalue)
            if (len_trim(key) == 0 .or. len_trim(cvalue) == 0) cycle

            n_entries = size(col_map)
            allocate(tmp(n_entries + 1))
            if (n_entries > 0) tmp(1:n_entries) = col_map
            tmp(n_entries + 1)%internal_name = parquet_unquote(key)
            tmp(n_entries + 1)%output_name = parquet_unquote(cvalue)
            call move_alloc(tmp, col_map)
        end do
        call parquet_maml_unlock()
    end procedure parquet_parse_col_map

    module subroutine parquet_parse_protected_cols(lines, names)
        character(len=*), intent(in) :: lines(:) !! raw MAML source lines to scan.
        character(len=:), allocatable, intent(out) :: names(:) !! trimmed, unquoted protected column names.
        character(len=:), allocatable :: tmp(:)
        character(len=:), allocatable :: tline, key, cvalue, token
        integer :: i, n, idx_extra, extra_end, idx_key, n_names, p, sep
        integer :: maxlen

        maxlen = 0
        do i = 1, size(lines)
            maxlen = max(maxlen, len_trim(lines(i)))
        end do
        allocate(character(len=max(maxlen,1)) :: names(0))

        n = size(lines)
        idx_extra = 0
        do i = 1, n
            if (lines(i)(1:1) /= " " .and. trim(adjustl(lines(i))) == "extra:") then
                idx_extra = i
                exit
            end if
        end do
        if (idx_extra == 0) return

        extra_end = n
        do i = idx_extra + 1, n
            if (len_trim(lines(i)) == 0) cycle
            if (lines(i)(1:1) /= " ") then
                extra_end = i - 1
                exit
            end if
        end do

        idx_key = 0
        do i = idx_extra + 1, extra_end
            if (len_trim(lines(i)) == 0) cycle
            tline = trim(adjustl(lines(i)))
            call parquet_split_key_value(tline, key, cvalue)
            if (parquet_to_lower(trim(key)) == "protected_cols") then
                idx_key = i
                exit
            end if
        end do
        if (idx_key == 0) return

        ! See g_maml_mutex in parquet_wrapper.cpp.
        call parquet_maml_lock()
        if (len_trim(cvalue) > 0) then
            ! Scalar semicolon-separated form: protected_cols: col1;col2; col3
            cvalue = trim(adjustl(cvalue))
            p = 1
            do while (p <= len(cvalue))
                sep = index(cvalue(p:), ";")
                if (sep == 0) then
                    token = cvalue(p:)
                    p = len(cvalue) + 1
                else
                    token = cvalue(p:p+sep-2)
                    p = p + sep
                end if
                token = trim(adjustl(parquet_unquote(token)))
                if (len_trim(token) > 0) then
                    n_names = size(names)
                    allocate(character(len=len(names)) :: tmp(n_names + 1))
                    if (n_names > 0) tmp(1:n_names) = names
                    tmp(n_names + 1) = token
                    call move_alloc(tmp, names)
                end if
            end do
            call parquet_maml_unlock()
            return
        end if

        ! Dash-list form: protected_cols: (empty) followed by "- col1" lines.
        do i = idx_key + 1, extra_end
            if (len_trim(lines(i)) == 0) cycle
            tline = trim(adjustl(lines(i)))
            if (tline(1:1) /= "-") exit
            tline = trim(adjustl(tline(2:)))
            token = trim(adjustl(parquet_unquote(tline)))
            if (len_trim(token) == 0) cycle

            n_names = size(names)
            allocate(character(len=len(names)) :: tmp(n_names + 1))
            if (n_names > 0) tmp(1:n_names) = names
            tmp(n_names + 1) = token
            call move_alloc(tmp, names)
        end do
        call parquet_maml_unlock()
    end subroutine parquet_parse_protected_cols

end submodule parquet_metadata
