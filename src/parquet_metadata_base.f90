!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!> Generic parquet_column_info/parquet_table_metadata plumbing shared by both
!> the write (MAML-build) and read sides: the base add_metadata family,
!> per-field column_info lookups/toggles, and low-level MAML string helpers
!> (parquet_split_key_value/parquet_unquote/parquet_to_lower) reused across
!> submodules.
submodule (parquet:parquet_metadata) parquet_metadata_base
    implicit none
contains

    module procedure parquet_append_line
        character(len=1024), allocatable :: tmp(:)

        ! See g_maml_mutex in parquet_wrapper.cpp.
        call parquet_maml_lock()
        if (.not. allocated(lines)) then
            allocate(lines(1))
            lines(1) = line
            call parquet_maml_unlock()
            return
        end if

        allocate(tmp(n))
        tmp(1:n-1) = lines
        tmp(n) = line
        call move_alloc(tmp, lines)
        call parquet_maml_unlock()
    end procedure parquet_append_line

    module procedure parquet_metadata_append_entry
        type(parquet_metadata_entry), allocatable :: tmp(:)
        integer :: n
        character(len=:), allocatable :: desc_val

        if (len_trim(key) == 0) return

        desc_val = ""
        if (present(description)) desc_val = trim(description)

        ! See g_maml_mutex in parquet_wrapper.cpp / parquet_parse_maml_lines
        ! above: growing metadata%items this way is not safely reentrant
        ! under genuine concurrent threads even with -frecursive.
        call parquet_maml_lock()
        if (.not. allocated(metadata%items)) then
            allocate(metadata%items(1))
            metadata%items(1)%key = trim(key)
            metadata%items(1)%value = trim(value)
            metadata%items(1)%description = desc_val
        else
            n = size(metadata%items)
            allocate(tmp(n+1))
            tmp(1:n) = metadata%items
            tmp(n+1)%key = trim(key)
            tmp(n+1)%value = trim(value)
            tmp(n+1)%description = desc_val
            call move_alloc(tmp, metadata%items)
        end if
        call parquet_maml_unlock()

        ! metadata%source_maml_lines is only allocated once parquet_read_maml has
        ! finished parsing (see parquet_read_maml_file/_internal below), so this
        ! never fires for the add_metadata calls the parser itself makes while
        ! building up metadata%items above -- only for calls made by external
        ! code after parquet_read_maml has returned.
        if (allocated(metadata%source_maml_lines)) then
            call parquet_append_keyarray_line(metadata%source_maml_lines, trim(key), trim(value), desc_val)
        end if
    end procedure parquet_metadata_append_entry

    module procedure add_metadata_int32
        character(len=64) :: cval

        write(cval, '(I0)') value
        call parquet_metadata_append_entry(this, key, trim(cval), description)
    end procedure add_metadata_int32

    module procedure add_metadata_int64
        character(len=64) :: cval

        write(cval, '(I0)') value
        call parquet_metadata_append_entry(this, key, trim(cval), description)
    end procedure add_metadata_int64

    module procedure add_metadata_float32
        character(len=64) :: cval

        if (present(fmt)) then
            write(cval, '(' // trim(fmt) // ')') value
        else
            write(cval, '(ES15.7E3)') value
        end if
        call parquet_metadata_append_entry(this, key, trim(adjustl(cval)), description)
    end procedure add_metadata_float32

    module procedure add_metadata_float64
        character(len=64) :: cval

        if (present(fmt)) then
            write(cval, '(' // trim(fmt) // ')') value
        else
            write(cval, '(ES24.16E3)') value
        end if
        call parquet_metadata_append_entry(this, key, trim(adjustl(cval)), description)
    end procedure add_metadata_float64

    module procedure add_metadata_logical
        if (value) then
            call parquet_metadata_append_entry(this, key, "true", description)
        else
            call parquet_metadata_append_entry(this, key, "false", description)
        end if
    end procedure add_metadata_logical

    module procedure add_metadata_string
        call parquet_metadata_append_entry(this, key, trim(value), description)
    end procedure add_metadata_string

    module procedure add_metadata_int32_array
        character(len=64) :: cval
        character(len=:), allocatable :: joined
        integer :: i

        joined = "["
        do i = 1, size(value)
            write(cval, '(I0)') value(i)
            if (i > 1) joined = joined // ", "
            joined = joined // trim(cval)
        end do
        joined = joined // "]"
        call parquet_metadata_append_entry(this, key, joined, description)
    end procedure add_metadata_int32_array

    module procedure add_metadata_int64_array
        character(len=64) :: cval
        character(len=:), allocatable :: joined
        integer :: i

        joined = "["
        do i = 1, size(value)
            write(cval, '(I0)') value(i)
            if (i > 1) joined = joined // ", "
            joined = joined // trim(cval)
        end do
        joined = joined // "]"
        call parquet_metadata_append_entry(this, key, joined, description)
    end procedure add_metadata_int64_array

    module procedure add_metadata_float32_array
        character(len=64) :: cval
        character(len=:), allocatable :: joined
        integer :: i

        joined = "["
        do i = 1, size(value)
            if (present(fmt)) then
                write(cval, '(' // trim(fmt) // ')') value(i)
            else
                write(cval, '(ES15.7E3)') value(i)
            end if
            if (i > 1) joined = joined // ", "
            joined = joined // trim(adjustl(cval))
        end do
        joined = joined // "]"
        call parquet_metadata_append_entry(this, key, joined, description)
    end procedure add_metadata_float32_array

    module procedure add_metadata_float64_array
        character(len=64) :: cval
        character(len=:), allocatable :: joined
        integer :: i

        joined = "["
        do i = 1, size(value)
            if (present(fmt)) then
                write(cval, '(' // trim(fmt) // ')') value(i)
            else
                write(cval, '(ES24.16E3)') value(i)
            end if
            if (i > 1) joined = joined // ", "
            joined = joined // trim(adjustl(cval))
        end do
        joined = joined // "]"
        call parquet_metadata_append_entry(this, key, joined, description)
    end procedure add_metadata_float64_array

    module procedure add_metadata_logical_array
        character(len=:), allocatable :: joined
        integer :: i

        joined = "["
        do i = 1, size(value)
            if (i > 1) joined = joined // ", "
            if (value(i)) then
                joined = joined // "true"
            else
                joined = joined // "false"
            end if
        end do
        joined = joined // "]"
        call parquet_metadata_append_entry(this, key, joined, description)
    end procedure add_metadata_logical_array

    module procedure add_metadata_string_array
        character(len=:), allocatable :: joined
        integer :: i

        joined = "["
        do i = 1, size(value)
            if (i > 1) joined = joined // ", "
            joined = joined // trim(value(i))
        end do
        joined = joined // "]"
        call parquet_metadata_append_entry(this, key, joined, description)
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

    module procedure get_num_fields
        if (allocated(this%col)) then
            get_num_fields = size(this%col)
        else
            get_num_fields = 0
        end if
    end procedure get_num_fields

    module procedure get_field_name
        character(len=16) :: index_str, count_str

        if (index < 1 .or. index > this%get_num_fields()) then
            write(index_str, '(I0)') index
            write(count_str, '(I0)') this%get_num_fields()
            error stop "parquet_column_info%get_field_name: index " // trim(index_str) // &
                " out of range (1.." // trim(count_str) // ")"
        end if

        name = this%col(index)%name
    end procedure get_field_name

    module procedure set_unavailable
        integer :: idx, i

        if (present(name)) then
            idx = this%get_column_index(name)
            if (this%col(idx)%is_deactivated) then
                error stop "parquet_column_info%set_column_unavailable: column is deactivated: " // trim(name)
            end if
            this%col(idx)%is_set = .false.
        else if (allocated(this%col)) then
            do i = 1, size(this%col)
                if (.not. this%col(i)%is_deactivated) this%col(i)%is_set = .false.
            end do
        end if
    end procedure set_unavailable

    module procedure set_available
        integer :: idx, i

        if (present(name)) then
            idx = this%get_column_index(name)
            if (this%col(idx)%is_deactivated) then
                error stop "parquet_column_info%set_column_available: column is deactivated: " // trim(name)
            end if
            this%col(idx)%is_set = .true.
        else if (allocated(this%col)) then
            do i = 1, size(this%col)
                if (.not. this%col(i)%is_deactivated) this%col(i)%is_set = .true.
            end do
        end if
    end procedure set_available

    module procedure parquet_append_empty_cinfo
        type(parquet_column_type), allocatable :: tmp(:)

        ! See g_maml_mutex in parquet_wrapper.cpp.
        call parquet_maml_lock()
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
        call parquet_maml_unlock()
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

end submodule parquet_metadata_base
