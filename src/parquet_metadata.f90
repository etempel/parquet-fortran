!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
submodule (parquet) parquet_metadata
    implicit none

    ! Valid values for a field's data_type: in a MAML file, checked by parquet_validate_maml.
    ! Add new supported types here as needed.
    character(len=7), parameter :: valid_maml_data_types(6) = [character(len=7) :: &
        "int32", "int64", "string", "boolean", "float32", "float64"]

contains

    module procedure parquet_read_maml_file
        type(parquet_maml_file) :: maml

        maml = parquet_load_maml_file(maml_filename)
        call parquet_parse_maml_lines(maml%lines, cinfo, metadata)
    end procedure parquet_read_maml_file

    module procedure parquet_read_maml_internal
        call parquet_parse_maml_lines(maml%lines, cinfo, metadata)
    end procedure parquet_read_maml_internal

    module procedure parquet_validate_user_maml
        type(parquet_column_info) :: base_cinfo, user_cinfo
        type(parquet_table_metadata) :: base_metadata, user_metadata
        character(len=:), allocatable :: bad_names
        integer :: i, j
        logical :: found

        call parquet_validate_maml(base_maml)
        call parquet_validate_maml(user_maml)

        call parquet_parse_maml_lines(base_maml%lines, base_cinfo, base_metadata)
        call parquet_parse_maml_lines(user_maml%lines, user_cinfo, user_metadata)

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
    end procedure parquet_validate_user_maml

    module procedure parquet_validate_maml
        type(parquet_column_info) :: cinfo
        type(parquet_table_metadata) :: metadata
        character(len=:), allocatable :: errors
        character(len=:), allocatable :: cur_name
        character(len=32) :: idx_buf
        integer :: i, j
        logical :: type_ok, has_table

        call parquet_parse_maml_lines(maml%lines, cinfo, metadata)

        errors = ""

        if (.not. allocated(cinfo%col)) then
            errors = errors // "no fields defined; "
        else if (size(cinfo%col) == 0) then
            errors = errors // "no fields defined; "
        else
            do i = 1, size(cinfo%col)
                cur_name = trim(cinfo%col(i)%name)

                if (len_trim(cur_name) == 0) then
                    write(idx_buf, '(I0)') i
                    errors = errors // "field #" // trim(idx_buf) // " has an empty name; "
                    cycle
                end if

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

        if (len_trim(errors) > 0) then
            error stop "parquet_validate_maml: " // trim(errors)
        end if
    end procedure parquet_validate_maml

    module procedure parquet_load_maml_file
        character(len=1024), allocatable :: lines(:)
        character(len=1024) :: line
        integer :: unit, ios, nlines, i, max_len

        nlines = 0
        open(newunit=unit, file=trim(maml_filename), status="old", action="read", iostat=ios)
        if (ios /= 0) error stop "parquet_load_maml_file: cannot open file: " // trim(maml_filename)

        do
            read(unit, '(A)', iostat=ios) line
            if (ios /= 0) exit
            nlines = nlines + 1
            call parquet_append_line(lines, nlines, line)
        end do

        close(unit)

        maml%name = trim(maml_filename)

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

    module procedure parquet_parse_maml_lines
        type(parquet_column_type), allocatable :: tmp(:)
        character(len=1024) :: line
        character(len=:), allocatable :: tline, key, cvalue
        logical :: in_fields, have_current, in_list, in_field_list, in_keyarray, in_doiarray
        character(len=:), allocatable :: list_key, field_list_key, list_item
        character(len=:), allocatable :: ka_key, ka_value, ka_comment
        character(len=:), allocatable :: doi_value, doi_type
        integer :: ios, n, i, list_item_idx, doi_idx
        character(len=32) :: idx_buf

        in_fields = .false.
        have_current = .false.
        in_list = .false.
        in_field_list = .false.
        in_keyarray = .false.
        in_doiarray = .false.
        list_item_idx = 0
        doi_idx = 0
        n = 0
        list_key = ""
        field_list_key = ""
        ka_key = ""
        ka_value = ""
        ka_comment = ""
        doi_value = ""
        doi_type = ""
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

                if (in_list) then
                    if (index(tline, "- ") == 1) then
                        list_item_idx = list_item_idx + 1
                        if (list_key == "comments" .or. list_key == "comment") then
                            write(idx_buf, '(I0)') list_item_idx
                            call metadata%add_metadata("comment_" // trim(idx_buf), parquet_unquote(tline(3:)))
                        else if (list_key == "coauthors" .or. list_key == "coauthor") then
                            write(idx_buf, '(I0)') list_item_idx
                            call metadata%add_metadata("coauthor_" // trim(idx_buf), parquet_unquote(tline(3:)))
                        else
                            call metadata%add_metadata(list_key, parquet_unquote(tline(3:)))
                        end if
                        cycle
                    else if (index(tline, ":") > 0 .and. line(1:1) /= " ") then
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
            end select
        end do

        if (in_keyarray) call parquet_flush_keyarray_item(metadata, ka_key, ka_value, ka_comment)
        if (in_doiarray) call parquet_flush_doi_item(metadata, doi_idx, doi_value, doi_type)

        if (n <= 0) then
            allocate(cinfo%col(0))
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

        call move_alloc(tmp, cinfo%col)
    end procedure parquet_parse_maml_lines

    module procedure parquet_append_line
        character(len=1024), allocatable :: tmp(:)

        if (.not. allocated(lines)) then
            allocate(lines(1))
            lines(1) = line
            return
        end if

        allocate(tmp(n))
        tmp(1:n-1) = lines
        tmp(n) = line
        call move_alloc(tmp, lines)
    end procedure parquet_append_line

    module procedure parquet_metadata_append_entry
        type(parquet_metadata_entry), allocatable :: tmp(:)
        integer :: n
        character(len=:), allocatable :: desc_val

        if (len_trim(key) == 0) return

        desc_val = ""
        if (present(description)) desc_val = trim(description)

        if (.not. allocated(metadata%items)) then
            allocate(metadata%items(1))
            metadata%items(1)%key = trim(key)
            metadata%items(1)%value = trim(value)
            metadata%items(1)%description = desc_val
            return
        end if

        n = size(metadata%items)
        allocate(tmp(n+1))
        tmp(1:n) = metadata%items
        tmp(n+1)%key = trim(key)
        tmp(n+1)%value = trim(value)
        tmp(n+1)%description = desc_val
        call move_alloc(tmp, metadata%items)
    end procedure parquet_metadata_append_entry

    module procedure add_metadata_int32
        character(len=64) :: cval

        write(cval, '(I0)') val
        call parquet_metadata_append_entry(this, key, trim(cval), desc)
    end procedure add_metadata_int32

    module procedure add_metadata_int64
        character(len=64) :: cval

        write(cval, '(I0)') val
        call parquet_metadata_append_entry(this, key, trim(cval), desc)
    end procedure add_metadata_int64

    module procedure add_metadata_float32
        character(len=64) :: cval

        if (present(fmt)) then
            write(cval, '(' // trim(fmt) // ')') val
        else
            write(cval, '(ES15.7E3)') val
        end if
        call parquet_metadata_append_entry(this, key, trim(adjustl(cval)), desc)
    end procedure add_metadata_float32

    module procedure add_metadata_float64
        character(len=64) :: cval

        if (present(fmt)) then
            write(cval, '(' // trim(fmt) // ')') val
        else
            write(cval, '(ES24.16E3)') val
        end if
        call parquet_metadata_append_entry(this, key, trim(adjustl(cval)), desc)
    end procedure add_metadata_float64

    module procedure add_metadata_logical
        if (val) then
            call parquet_metadata_append_entry(this, key, "true", desc)
        else
            call parquet_metadata_append_entry(this, key, "false", desc)
        end if
    end procedure add_metadata_logical

    module procedure add_metadata_string
        call parquet_metadata_append_entry(this, key, trim(val), desc)
    end procedure add_metadata_string

    module procedure add_metadata_int32_array
        character(len=64) :: cval
        character(len=:), allocatable :: joined
        integer :: i

        joined = "["
        do i = 1, size(val)
            write(cval, '(I0)') val(i)
            if (i > 1) joined = joined // ", "
            joined = joined // trim(cval)
        end do
        joined = joined // "]"
        call parquet_metadata_append_entry(this, key, joined, desc)
    end procedure add_metadata_int32_array

    module procedure add_metadata_int64_array
        character(len=64) :: cval
        character(len=:), allocatable :: joined
        integer :: i

        joined = "["
        do i = 1, size(val)
            write(cval, '(I0)') val(i)
            if (i > 1) joined = joined // ", "
            joined = joined // trim(cval)
        end do
        joined = joined // "]"
        call parquet_metadata_append_entry(this, key, joined, desc)
    end procedure add_metadata_int64_array

    module procedure add_metadata_float32_array
        character(len=64) :: cval
        character(len=:), allocatable :: joined
        integer :: i

        joined = "["
        do i = 1, size(val)
            if (present(fmt)) then
                write(cval, '(' // trim(fmt) // ')') val(i)
            else
                write(cval, '(ES15.7E3)') val(i)
            end if
            if (i > 1) joined = joined // ", "
            joined = joined // trim(adjustl(cval))
        end do
        joined = joined // "]"
        call parquet_metadata_append_entry(this, key, joined, desc)
    end procedure add_metadata_float32_array

    module procedure add_metadata_float64_array
        character(len=64) :: cval
        character(len=:), allocatable :: joined
        integer :: i

        joined = "["
        do i = 1, size(val)
            if (present(fmt)) then
                write(cval, '(' // trim(fmt) // ')') val(i)
            else
                write(cval, '(ES24.16E3)') val(i)
            end if
            if (i > 1) joined = joined // ", "
            joined = joined // trim(adjustl(cval))
        end do
        joined = joined // "]"
        call parquet_metadata_append_entry(this, key, joined, desc)
    end procedure add_metadata_float64_array

    module procedure add_metadata_logical_array
        character(len=:), allocatable :: joined
        integer :: i

        joined = "["
        do i = 1, size(val)
            if (i > 1) joined = joined // ", "
            if (val(i)) then
                joined = joined // "true"
            else
                joined = joined // "false"
            end if
        end do
        joined = joined // "]"
        call parquet_metadata_append_entry(this, key, joined, desc)
    end procedure add_metadata_logical_array

    module procedure add_metadata_string_array
        character(len=:), allocatable :: joined
        integer :: i

        joined = "["
        do i = 1, size(val)
            if (i > 1) joined = joined // ", "
            joined = joined // trim(val(i))
        end do
        joined = joined // "]"
        call parquet_metadata_append_entry(this, key, joined, desc)
    end procedure add_metadata_string_array

    module procedure get_column_index
        integer :: i

        get_column_index = 0
        if (allocated(this%col)) then
            do i = 1, size(this%col)
                if (allocated(this%col(i)%name)) then
                    if (trim(this%col(i)%name) == trim(name)) then
                        get_column_index = i
                        exit
                    end if
                end if
            end do
        end if

        if (get_column_index == 0) then
            error stop "parquet_column_info%get_column_index: column not found: " // trim(name)
        end if
    end procedure get_column_index

    module procedure set_unavailable
        if (present(name)) then
            this%col(this%get_column_index(name))%is_set = .false.
        else if (allocated(this%col)) then
            this%col(:)%is_set = .false.
        end if
    end procedure set_unavailable

    module procedure set_available
        if (present(name)) then
            this%col(this%get_column_index(name))%is_set = .true.
        else if (allocated(this%col)) then
            this%col(:)%is_set = .true.
        end if
    end procedure set_available

    module procedure parquet_append_empty_cinfo
        type(parquet_column_type), allocatable :: tmp(:)

        if (.not. allocated(columns)) then
            allocate(columns(1))
            n = 1
        else
            allocate(tmp(size(columns) + 1))
            if (size(columns) > 0) tmp(1:size(columns)) = columns
            call move_alloc(tmp, columns)
            n = size(columns)
        end if

        columns(n)%is_set = .true.
        columns(n)%array_size = 1
        columns(n)%col_size = 1
    end procedure parquet_append_empty_cinfo

    module procedure parquet_split_key_value
        integer :: p

        p = index(line, ":")
        if (p <= 0) then
            key = ""
            value = ""
            return
        end if

        key = trim(adjustl(line(1:p-1)))
        if (p < len_trim(line)) then
            value = trim(adjustl(line(p+1:len_trim(line))))
        else
            value = ""
        end if
    end procedure parquet_split_key_value

    module procedure parquet_unquote
        integer :: n

        out = trim(adjustl(s))
        n = len_trim(out)
        if (n >= 2) then
            if ((out(1:1) == '"' .and. out(n:n) == '"') .or. (out(1:1) == "'" .and. out(n:n) == "'")) then
                out = out(2:n-1)
            end if
        end if
    end procedure parquet_unquote

    module procedure parquet_to_lower
        integer :: i, c

        out = s
        do i = 1, len(out)
            c = iachar(out(i:i))
            if (c >= iachar('A') .and. c <= iachar('Z')) out(i:i) = achar(c + 32)
        end do
    end procedure parquet_to_lower

    subroutine parquet_flush_keyarray_item(metadata, ka_key, ka_value, ka_comment)
        type(parquet_table_metadata), intent(inout) :: metadata
        character(len=*), intent(in) :: ka_key
        character(len=*), intent(in) :: ka_value
        character(len=*), intent(in) :: ka_comment

        if (len_trim(ka_key) == 0) return
        call metadata%add_metadata(trim(ka_key), trim(ka_value), trim(ka_comment))
    end subroutine parquet_flush_keyarray_item

    subroutine parquet_flush_doi_item(metadata, doi_idx, doi_value, doi_type)
        type(parquet_table_metadata), intent(inout) :: metadata
        integer, intent(inout) :: doi_idx
        character(len=*), intent(in) :: doi_value
        character(len=*), intent(in) :: doi_type
        character(len=32) :: idx_buf

        if (len_trim(doi_value) == 0) return
        doi_idx = doi_idx + 1
        write(idx_buf, '(I0)') doi_idx
        call metadata%add_metadata("DOI_" // trim(idx_buf), trim(doi_value), trim(doi_type))
    end subroutine parquet_flush_doi_item

end submodule parquet_metadata
