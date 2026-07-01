!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
submodule (parquet) parquet_metadata
    implicit none
contains

    module procedure parquet_read_maml_file
        character(len=1024), allocatable :: lines(:)
        character(len=1024) :: line
        integer :: unit, ios, nlines

        nlines = 0
        open(newunit=unit, file=trim(maml_filename), status="old", action="read", iostat=ios)
        if (ios /= 0) error stop "parquet_read_maml: cannot open file: " // trim(maml_filename)

        do
            read(unit, '(A)', iostat=ios) line
            if (ios /= 0) exit
            nlines = nlines + 1
            call parquet_append_line(lines, nlines, line)
        end do

        close(unit)

        call parquet_parse_maml_lines(lines, cinfo, metadata)
    end procedure parquet_read_maml_file

    module procedure parquet_read_maml_internal
        call parquet_parse_maml_lines(maml%lines, cinfo, metadata)
    end procedure parquet_read_maml_internal

    module procedure parquet_parse_maml_lines
        type(parquet_column_info), allocatable :: tmp(:)
        character(len=1024) :: line
        character(len=:), allocatable :: tline, key, cvalue
        logical :: in_fields, have_current, in_list, in_field_list
        character(len=:), allocatable :: list_key, field_list_key, list_item
        integer :: ios, n, i, list_item_idx
        character(len=32) :: idx_buf

        in_fields = .false.
        have_current = .false.
        in_list = .false.
        in_field_list = .false.
        list_item_idx = 0
        n = 0
        list_key = ""
        field_list_key = ""
        if (allocated(metadata%items)) deallocate(metadata%items)

        do i = 1, size(lines)
            line = lines(i)

            tline = trim(adjustl(line))
            if (len_trim(tline) == 0) cycle
            if (tline(1:1) == "#") cycle

            if (.not. in_fields) then
                if (in_list) then
                    if (index(tline, "- ") == 1) then
                        list_item_idx = list_item_idx + 1
                        if (list_key == "comments" .or. list_key == "comment") then
                            write(idx_buf, '(I0)') list_item_idx
                            call metadata%add_metadata("comment_" // trim(idx_buf), parquet_unquote(tline(3:)))
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

        if (n <= 0) then
            allocate(cinfo(0))
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

        call move_alloc(tmp, cinfo)
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

        if (len_trim(key) == 0) return

        if (.not. allocated(metadata%items)) then
            allocate(metadata%items(1))
            metadata%items(1)%key = trim(key)
            metadata%items(1)%value = trim(value)
            return
        end if

        n = size(metadata%items)
        allocate(tmp(n+1))
        tmp(1:n) = metadata%items
        tmp(n+1)%key = trim(key)
        tmp(n+1)%value = trim(value)
        call move_alloc(tmp, metadata%items)
    end procedure parquet_metadata_append_entry

    module procedure add_metadata_int32
        character(len=64) :: cval

        write(cval, '(I0)') val
        call parquet_metadata_append_entry(this, key, trim(cval))
    end procedure add_metadata_int32

    module procedure add_metadata_int64
        character(len=64) :: cval

        write(cval, '(I0)') val
        call parquet_metadata_append_entry(this, key, trim(cval))
    end procedure add_metadata_int64

    module procedure add_metadata_float32
        character(len=64) :: cval

        if (present(fmt)) then
            write(cval, '(' // trim(fmt) // ')') val
        else
            write(cval, '(ES15.7E3)') val
        end if
        call parquet_metadata_append_entry(this, key, trim(adjustl(cval)))
    end procedure add_metadata_float32

    module procedure add_metadata_float64
        character(len=64) :: cval

        if (present(fmt)) then
            write(cval, '(' // trim(fmt) // ')') val
        else
            write(cval, '(ES24.16E3)') val
        end if
        call parquet_metadata_append_entry(this, key, trim(adjustl(cval)))
    end procedure add_metadata_float64

    module procedure add_metadata_logical
        if (val) then
            call parquet_metadata_append_entry(this, key, "true")
        else
            call parquet_metadata_append_entry(this, key, "false")
        end if
    end procedure add_metadata_logical

    module procedure add_metadata_string
        call parquet_metadata_append_entry(this, key, trim(val))
    end procedure add_metadata_string

    module procedure parquet_append_empty_cinfo
        type(parquet_column_info), allocatable :: tmp(:)

        if (.not. allocated(cinfo)) then
            allocate(cinfo(1))
            n = 1
        else
            allocate(tmp(size(cinfo) + 1))
            if (size(cinfo) > 0) tmp(1:size(cinfo)) = cinfo
            call move_alloc(tmp, cinfo)
            n = size(cinfo)
        end if

        cinfo(n)%is_set = .true.
        cinfo(n)%array_size = 1
        cinfo(n)%col_size = 1
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

end submodule parquet_metadata
