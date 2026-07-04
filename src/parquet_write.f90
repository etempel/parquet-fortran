!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
submodule (parquet) parquet_write
contains

    module procedure parquet_get_enabled_column_index
        integer :: i

        parquet_get_enabled_column_index = 0
        if (.not. allocated(writer%enabled_columns)) return

        do i = 1, size(writer%enabled_columns)
            if (trim(writer%enabled_columns(i)%name) == trim(name)) then
                parquet_get_enabled_column_index = i
                return
            end if
        end do
    end procedure parquet_get_enabled_column_index

    module procedure parquet_get_defined_column_index
        integer :: i

        parquet_get_defined_column_index = 0
        if (.not. allocated(writer%all_columns)) return

        do i = 1, size(writer%all_columns)
            if (trim(writer%all_columns(i)%name) == trim(name)) then
                parquet_get_defined_column_index = i
                return
            end if
        end do
    end procedure parquet_get_defined_column_index

    module procedure parquet_is_type_compatible
        character(len=:), allocatable :: schema_type, write_type

        schema_type = trim(actual_type)
        write_type = trim(expected_type)

        select case (schema_type)
        case ("boolean")
            parquet_is_type_compatible = write_type == "boolean" .or. write_type == "bool8"
        case ("float32", "float64")
            parquet_is_type_compatible = write_type == "float32" .or. write_type == "float64" .or. &
                                          write_type == "int32" .or. write_type == "int64"
        case ("int32", "int64")
            parquet_is_type_compatible = write_type == "int32" .or. write_type == "int64" .or. &
                                          write_type == "float32" .or. write_type == "float64"
        case default
            parquet_is_type_compatible = write_type == schema_type
        end select
    end procedure parquet_is_type_compatible

    function parquet_get_schema_type(writer, name) result(schema_type)
        type(parquet_writer), intent(in) :: writer
        character(len=*), intent(in) :: name
        character(len=:), allocatable :: schema_type
        integer :: idx

        schema_type = ""
        if (.not. writer%enforce_schema) return
        idx = parquet_get_defined_column_index(writer, name)
        if (idx == 0) return
        schema_type = trim(writer%all_columns(idx)%data_type)
    end function parquet_get_schema_type

    function parquet_narrow_int64_to_int32(name, src) result(dst)
        character(len=*), intent(in) :: name
        integer(int64), intent(in) :: src(:)
        integer(int32), allocatable :: dst(:)
        integer :: i

        allocate(dst(size(src)))
        do i = 1, size(src)
            if (src(i) < -2147483648_int64 .or. src(i) > 2147483647_int64) then
                error stop "parquet_write_column: int64 value out of int32 range for column " // trim(name)
            end if
            dst(i) = int(src(i), kind=int32)
        end do
    end function parquet_narrow_int64_to_int32

    function parquet_float64_to_int32(name, src) result(dst)
        character(len=*), intent(in) :: name
        real(real64), intent(in) :: src(:)
        integer(int32), allocatable :: dst(:)
        integer :: i

        allocate(dst(size(src)))
        do i = 1, size(src)
            if (src(i) /= anint(src(i))) then
                error stop "parquet_write_column: non-integral float value written to int column " // trim(name)
            end if
            if (src(i) < -2147483648.0_real64 .or. src(i) > 2147483647.0_real64) then
                error stop "parquet_write_column: float value out of int32 range for column " // trim(name)
            end if
            dst(i) = int(src(i), kind=int32)
        end do
    end function parquet_float64_to_int32

    function parquet_float64_to_int64(name, src) result(dst)
        character(len=*), intent(in) :: name
        real(real64), intent(in) :: src(:)
        integer(int64), allocatable :: dst(:)
        integer :: i

        allocate(dst(size(src)))
        do i = 1, size(src)
            if (src(i) /= anint(src(i))) then
                error stop "parquet_write_column: non-integral float value written to int column " // trim(name)
            end if
            if (src(i) < -9223372036854775808.0_real64 .or. src(i) >= 9223372036854775808.0_real64) then
                error stop "parquet_write_column: float value out of int64 range for column " // trim(name)
            end if
            dst(i) = int(src(i), kind=int64)
        end do
    end function parquet_float64_to_int64

    subroutine parquet_append_as_schema_int32(writer, name, values, nrows, asize)
        type(parquet_writer), intent(in) :: writer
        character(len=*), intent(in) :: name
        integer(int32), intent(in) :: values(:)
        integer(c_long_long), intent(in) :: nrows, asize
        character(len=:), allocatable :: schema_type
        integer(int64), allocatable :: i64values(:)
        real(real32), allocatable :: f32values(:)
        real(real64), allocatable :: f64values(:)

        schema_type = parquet_get_schema_type(writer, name)
        select case (schema_type)
        case ("int64")
            allocate(i64values(size(values)))
            i64values = int(values, kind=int64)
            call parquet_append_int64_column(writer%handle, trim(name)//char(0), i64values, nrows, asize)
        case ("float32")
            allocate(f32values(size(values)))
            f32values = real(values, kind=real32)
            call parquet_append_float32_column(writer%handle, trim(name)//char(0), f32values, nrows, asize)
        case ("float64")
            allocate(f64values(size(values)))
            f64values = real(values, kind=real64)
            call parquet_append_float64_column(writer%handle, trim(name)//char(0), f64values, nrows, asize)
        case default
            call parquet_append_int32_column(writer%handle, trim(name)//char(0), values, nrows, asize)
        end select
    end subroutine parquet_append_as_schema_int32

    subroutine parquet_append_as_schema_int64(writer, name, values, nrows, asize)
        type(parquet_writer), intent(in) :: writer
        character(len=*), intent(in) :: name
        integer(int64), intent(in) :: values(:)
        integer(c_long_long), intent(in) :: nrows, asize
        character(len=:), allocatable :: schema_type
        integer(int32), allocatable :: i32values(:)
        real(real32), allocatable :: f32values(:)
        real(real64), allocatable :: f64values(:)

        schema_type = parquet_get_schema_type(writer, name)
        select case (schema_type)
        case ("int32")
            i32values = parquet_narrow_int64_to_int32(name, values)
            call parquet_append_int32_column(writer%handle, trim(name)//char(0), i32values, nrows, asize)
        case ("float32")
            allocate(f32values(size(values)))
            f32values = real(values, kind=real32)
            call parquet_append_float32_column(writer%handle, trim(name)//char(0), f32values, nrows, asize)
        case ("float64")
            allocate(f64values(size(values)))
            f64values = real(values, kind=real64)
            call parquet_append_float64_column(writer%handle, trim(name)//char(0), f64values, nrows, asize)
        case default
            call parquet_append_int64_column(writer%handle, trim(name)//char(0), values, nrows, asize)
        end select
    end subroutine parquet_append_as_schema_int64

    subroutine parquet_append_as_schema_float32(writer, name, values, nrows, asize)
        type(parquet_writer), intent(in) :: writer
        character(len=*), intent(in) :: name
        real(real32), intent(in) :: values(:)
        integer(c_long_long), intent(in) :: nrows, asize
        character(len=:), allocatable :: schema_type
        integer(int32), allocatable :: i32values(:)
        integer(int64), allocatable :: i64values(:)
        real(real64), allocatable :: f64values(:)

        schema_type = parquet_get_schema_type(writer, name)
        select case (schema_type)
        case ("int32")
            i32values = parquet_float64_to_int32(name, real(values, kind=real64))
            call parquet_append_int32_column(writer%handle, trim(name)//char(0), i32values, nrows, asize)
        case ("int64")
            i64values = parquet_float64_to_int64(name, real(values, kind=real64))
            call parquet_append_int64_column(writer%handle, trim(name)//char(0), i64values, nrows, asize)
        case ("float64")
            allocate(f64values(size(values)))
            f64values = real(values, kind=real64)
            call parquet_append_float64_column(writer%handle, trim(name)//char(0), f64values, nrows, asize)
        case default
            call parquet_append_float32_column(writer%handle, trim(name)//char(0), values, nrows, asize)
        end select
    end subroutine parquet_append_as_schema_float32

    subroutine parquet_append_as_schema_float64(writer, name, values, nrows, asize)
        type(parquet_writer), intent(in) :: writer
        character(len=*), intent(in) :: name
        real(real64), intent(in) :: values(:)
        integer(c_long_long), intent(in) :: nrows, asize
        character(len=:), allocatable :: schema_type
        integer(int32), allocatable :: i32values(:)
        integer(int64), allocatable :: i64values(:)
        real(real32), allocatable :: f32values(:)

        schema_type = parquet_get_schema_type(writer, name)
        select case (schema_type)
        case ("int32")
            i32values = parquet_float64_to_int32(name, values)
            call parquet_append_int32_column(writer%handle, trim(name)//char(0), i32values, nrows, asize)
        case ("int64")
            i64values = parquet_float64_to_int64(name, values)
            call parquet_append_int64_column(writer%handle, trim(name)//char(0), i64values, nrows, asize)
        case ("float32")
            allocate(f32values(size(values)))
            f32values = real(values, kind=real32)
            call parquet_append_float32_column(writer%handle, trim(name)//char(0), f32values, nrows, asize)
        case default
            call parquet_append_float64_column(writer%handle, trim(name)//char(0), values, nrows, asize)
        end select
    end subroutine parquet_append_as_schema_float64

    module procedure parquet_assert_column_type
        integer :: idx

        if (.not. writer%enforce_schema) return

        idx = parquet_get_defined_column_index(writer, name)
        if (idx == 0) then
            error stop "parquet_write_column: column not defined in parquet_open_writer: " // trim(name)
        end if

        if (.not. parquet_is_type_compatible(writer%all_columns(idx)%data_type, expected_type)) then
            error stop "parquet_write_column: type mismatch for column " // trim(name) // &
                                  " (expected " // trim(expected_type) // ", got " // trim(writer%all_columns(idx)%data_type) // ")"
        end if
    end procedure parquet_assert_column_type

    module procedure parquet_mark_column_written
        integer :: idx

        if (.not. allocated(writer%enabled_columns)) return

        idx = parquet_get_enabled_column_index(writer, name)
        if (idx == 0) return

        if (writer%write_counts(idx) > 0) then
            error stop "parquet_write_column: column written more than once: " // trim(name)
        end if
        writer%write_counts(idx) = writer%write_counts(idx) + 1
    end procedure parquet_mark_column_written

    module procedure parquet_is_column_enabled
        parquet_is_column_enabled = .true.
        if (.not. allocated(writer%enabled_columns)) return

        parquet_is_column_enabled = parquet_get_enabled_column_index(writer, name) > 0
    end procedure parquet_is_column_enabled

    module procedure parquet_get_column_col_size
        integer :: i

        parquet_get_column_col_size = 1
        if (.not. allocated(writer%enabled_columns)) return

        do i = 1, size(writer%enabled_columns)
            if (trim(writer%enabled_columns(i)%name) == trim(name)) then
                parquet_get_column_col_size = max(1, writer%enabled_columns(i)%col_size)
                return
            end if
        end do
    end procedure parquet_get_column_col_size

    module procedure parquet_get_column_array_size
        parquet_get_column_array_size = parquet_get_column_col_size(writer, name)
    end procedure parquet_get_column_array_size

    module procedure parquet_open_writer
        integer :: i, k, n_enabled

        writer%handle = create_parquet_writer(trim(filename)//char(0))
        writer%enforce_schema = present(cinfo)

        if (present(cinfo)) then
            allocate(writer%all_columns(size(cinfo%col)))
            writer%all_columns = cinfo%col

            n_enabled = 0
            do i = 1, size(cinfo%col)
                if (cinfo%col(i)%is_set) n_enabled = n_enabled + 1
            end do

            if (n_enabled > 0) then
                allocate(writer%enabled_columns(n_enabled))
                allocate(writer%write_counts(n_enabled))
                writer%write_counts = 0
                k = 0
            end if

            do i = 1, size(cinfo%col)
                if (cinfo%col(i)%is_set) then
                    k = k + 1
                    writer%enabled_columns(k) = cinfo%col(i)
                    call parquet_add_column_info(&
                        writer, &
                        cinfo%col(i)%name, &
                        cinfo%col(i)%unit, &
                        cinfo%col(i)%info, &
                        cinfo%col(i)%ucd, &
                        cinfo%col(i)%data_type, &
                        cinfo%col(i)%array_size, &
                        cinfo%col(i)%col_size )
                end if
            end do
        end if

        if (present(metadata)) then
            if (allocated(metadata%items)) then
                do i = 1, size(metadata%items)
                    call parquet_add_table_metadata(writer%handle, &
                        trim(metadata%items(i)%key)//char(0), &
                        trim(metadata%items(i)%value)//char(0), &
                        trim(metadata%items(i)%description)//char(0))
                end do
            end if
        end if

        if (present(write_maml)) then
            if (write_maml) then
                if (.not. present(metadata)) then
                    error stop "parquet_open_writer: write_maml=.true. requires metadata " // &
                        "(populated by parquet_read_maml) to be present"
                end if
                if (.not. allocated(metadata%source_maml_lines)) then
                    error stop "parquet_open_writer: write_maml=.true. requires metadata " // &
                        "obtained from parquet_read_maml (no source MAML content found)"
                end if
                block
                    character(len=:), allocatable :: sidecar_lines(:)
                    sidecar_lines = metadata%source_maml_lines
                    if (present(cinfo)) call parquet_prune_disabled_fields(sidecar_lines, cinfo)
                    call parquet_write_maml_sidecar(filename, sidecar_lines)
                end block
            end if
        end if
    end procedure parquet_open_writer

    !> Removes, from `lines` (a working copy of metadata%source_maml_lines),
    !> the `fields:` entries whose column is disabled (is_set = .false.) in
    !> `cinfo`, so that a sidecar .maml written via write_maml=.true. only
    !> lists the columns actually present in the .parquet file. Matches source
    !> MAML field blocks to cinfo%col by name; a block runs from its top-level
    !> "- ..." line up to (but not including) the next top-level line. If every
    !> column in cinfo is disabled, lines is left untouched instead of emptying
    !> out fields: entirely, since a MAML file with no fields fails
    !> parquet_validate_maml on the next read.
    subroutine parquet_prune_disabled_fields(lines, cinfo)
        character(len=:), allocatable, intent(inout) :: lines(:)
        type(parquet_column_info), intent(in) :: cinfo
        logical, allocatable :: keep(:)
        character(len=:), allocatable :: tline, key, cvalue, field_name
        character(len=:), allocatable :: new_lines(:)
        integer :: i, j, k, n, idx_fields, block_start, block_end, col_idx, n_keep

        if (.not. allocated(cinfo%col)) return
        if (size(cinfo%col) == 0) return
        if (.not. any(cinfo%col(:)%is_set)) return

        n = size(lines)
        idx_fields = 0
        do i = 1, n
            if (lines(i)(1:1) /= " " .and. trim(adjustl(lines(i))) == "fields:") then
                idx_fields = i
                exit
            end if
        end do
        if (idx_fields == 0) return

        allocate(keep(n))
        keep = .true.

        i = idx_fields + 1
        do while (i <= n)
            if (len_trim(lines(i)) == 0) then
                i = i + 1
                cycle
            end if

            if (lines(i)(1:1) /= " " .and. index(trim(adjustl(lines(i))), "-") /= 1) exit

            if (lines(i)(1:1) /= " ") then
                ! Top-level "- ..." line: start of a new field block. It runs
                ! until the next top-level (non-indented) line.
                block_start = i
                block_end = i
                j = i + 1
                do while (j <= n)
                    if (len_trim(lines(j)) == 0) exit
                    if (lines(j)(1:1) /= " ") exit
                    block_end = j
                    j = j + 1
                end do

                field_name = ""
                do k = block_start, block_end
                    if (k == block_start) then
                        tline = trim(adjustl(lines(k)))
                        tline = trim(adjustl(tline(2:)))
                        if (len_trim(tline) == 0) cycle
                    else
                        tline = trim(adjustl(lines(k)))
                    end if
                    call parquet_split_key_value(tline, key, cvalue)
                    if (len_trim(key) == 0) cycle
                    if (parquet_to_lower(trim(key)) == "name") then
                        field_name = parquet_unquote(cvalue)
                        exit
                    end if
                end do

                col_idx = 0
                if (len_trim(field_name) > 0) then
                    do k = 1, size(cinfo%col)
                        if (trim(cinfo%col(k)%name) == trim(field_name)) then
                            col_idx = k
                            exit
                        end if
                    end do
                end if

                if (col_idx > 0) then
                    if (.not. cinfo%col(col_idx)%is_set) keep(block_start:block_end) = .false.
                end if

                i = block_end + 1
                cycle
            end if

            i = i + 1
        end do

        n_keep = count(keep)
        if (n_keep == n) return

        allocate(character(len=len(lines)) :: new_lines(n_keep))
        j = 0
        do i = 1, n
            if (keep(i)) then
                j = j + 1
                new_lines(j) = lines(i)
            end if
        end do

        call move_alloc(new_lines, lines)
    end subroutine parquet_prune_disabled_fields

    !> Writes `lines` to a sidecar .maml file next to `parquet_filename`: the same
    !> path with a trailing ".parquet" replaced by ".maml", or ".maml" appended if
    !> there is no ".parquet" suffix.
    subroutine parquet_write_maml_sidecar(parquet_filename, lines)
        character(len=*), intent(in) :: parquet_filename
        character(len=*), intent(in) :: lines(:)
        character(len=:), allocatable :: maml_filename
        integer :: unit, i, n

        n = len(parquet_filename)
        if (n >= 8) then
            if (parquet_filename(n-7:n) == ".parquet") then
                maml_filename = parquet_filename(1:n-8) // ".maml"
            else
                maml_filename = parquet_filename // ".maml"
            end if
        else
            maml_filename = parquet_filename // ".maml"
        end if

        open(newunit=unit, file=maml_filename, status="replace", action="write", form="formatted")
        do i = 1, size(lines)
            write(unit, '(a)') trim(lines(i))
        end do
        close(unit)
    end subroutine parquet_write_maml_sidecar

    module procedure parquet_add_column_info
        call parquet_add_column_metadata(&
            writer%handle, &
            trim(name)//char(0), &
            trim(unit)//char(0), &
            trim(description)//char(0), &
            trim(ucd)//char(0), &
            trim(data_type)//char(0), &
            int(array_size, kind=c_long_long), &
            int(col_size, kind=c_long_long) )
    end procedure parquet_add_column_info

    module procedure parquet_write_int32_column
        integer :: asize, nrows, idx

        if (writer%enforce_schema) then
            idx = parquet_get_defined_column_index(writer, name)
            if (idx == 0) error stop "parquet_write_column: column not defined in parquet_open_writer: " // trim(name)
            if (.not. writer%all_columns(idx)%is_set) return
        end if

        call parquet_assert_column_type(writer, name, "int32")

        if (.not. parquet_is_column_enabled(writer, name)) return
        call parquet_mark_column_written(writer, name)

        asize = parquet_get_column_col_size(writer, name)
        if (mod(size(data), asize) /= 0) stop "parquet_write_int32_column: data size is not divisible by col_size"
        nrows = size(data) / asize

        call parquet_append_as_schema_int32(writer, name, data, int(nrows, kind=c_long_long), int(asize, kind=c_long_long))
    end procedure parquet_write_int32_column

    module procedure parquet_write_int32_matrix_column
        integer :: asize, nrows, idx
        integer(int32), allocatable :: packed(:)

        asize = size(data, 1)
        nrows = size(data, 2)

        if (writer%enforce_schema) then
            idx = parquet_get_defined_column_index(writer, name)
            if (idx == 0) error stop "parquet_write_column: column not defined in parquet_open_writer: " // trim(name)
            if (.not. writer%all_columns(idx)%is_set) return
            if (writer%all_columns(idx)%col_size /= asize) then
                error stop "parquet_write_column: array size mismatch for column " // trim(name)
            end if
        end if

        call parquet_assert_column_type(writer, name, "int32")

        if (.not. parquet_is_column_enabled(writer, name)) return
        call parquet_mark_column_written(writer, name)

        allocate(packed(size(data)))
        packed = reshape(data, [size(data)])

        call parquet_append_as_schema_int32(writer, name, packed, int(nrows, kind=c_long_long), int(asize, kind=c_long_long))
    end procedure parquet_write_int32_matrix_column

    module procedure parquet_write_int64_column
        integer :: asize, nrows, idx

        if (writer%enforce_schema) then
            idx = parquet_get_defined_column_index(writer, name)
            if (idx == 0) error stop "parquet_write_column: column not defined in parquet_open_writer: " // trim(name)
            if (.not. writer%all_columns(idx)%is_set) return
        end if

        call parquet_assert_column_type(writer, name, "int64")

        if (.not. parquet_is_column_enabled(writer, name)) return
        call parquet_mark_column_written(writer, name)

        asize = parquet_get_column_col_size(writer, name)
        if (mod(size(data), asize) /= 0) stop "parquet_write_int64_column: data size is not divisible by col_size"
        nrows = size(data) / asize

        call parquet_append_as_schema_int64(writer, name, data, int(nrows, kind=c_long_long), int(asize, kind=c_long_long))
    end procedure parquet_write_int64_column

    module procedure parquet_write_int64_matrix_column
        integer :: asize, nrows, idx
        integer(int64), allocatable :: packed(:)

        asize = size(data, 1)
        nrows = size(data, 2)

        if (writer%enforce_schema) then
            idx = parquet_get_defined_column_index(writer, name)
            if (idx == 0) error stop "parquet_write_column: column not defined in parquet_open_writer: " // trim(name)
            if (.not. writer%all_columns(idx)%is_set) return
            if (writer%all_columns(idx)%col_size /= asize) then
                error stop "parquet_write_column: array size mismatch for column " // trim(name)
            end if
        end if

        call parquet_assert_column_type(writer, name, "int64")

        if (.not. parquet_is_column_enabled(writer, name)) return
        call parquet_mark_column_written(writer, name)

        allocate(packed(size(data)))
        packed = reshape(data, [size(data)])

        call parquet_append_as_schema_int64(writer, name, packed, int(nrows, kind=c_long_long), int(asize, kind=c_long_long))
    end procedure parquet_write_int64_matrix_column

    module procedure parquet_write_float32_column
        integer :: asize, nrows, idx

        if (writer%enforce_schema) then
            idx = parquet_get_defined_column_index(writer, name)
            if (idx == 0) error stop "parquet_write_column: column not defined in parquet_open_writer: " // trim(name)
            if (.not. writer%all_columns(idx)%is_set) return
        end if

        call parquet_assert_column_type(writer, name, "float32")

        if (.not. parquet_is_column_enabled(writer, name)) return
        call parquet_mark_column_written(writer, name)

        asize = parquet_get_column_col_size(writer, name)
        if (mod(size(data), asize) /= 0) stop "parquet_write_float32_column: data size is not divisible by col_size"
        nrows = size(data) / asize

        call parquet_append_as_schema_float32(writer, name, data, int(nrows, kind=c_long_long), int(asize, kind=c_long_long))
    end procedure parquet_write_float32_column

    module procedure parquet_write_float32_matrix_column
        integer :: asize, nrows, idx
        real(real32), allocatable :: packed(:)

        asize = size(data, 1)
        nrows = size(data, 2)

        if (writer%enforce_schema) then
            idx = parquet_get_defined_column_index(writer, name)
            if (idx == 0) error stop "parquet_write_column: column not defined in parquet_open_writer: " // trim(name)
            if (.not. writer%all_columns(idx)%is_set) return
            if (writer%all_columns(idx)%col_size /= asize) then
                error stop "parquet_write_column: array size mismatch for column " // trim(name)
            end if
        end if

        call parquet_assert_column_type(writer, name, "float32")

        if (.not. parquet_is_column_enabled(writer, name)) return
        call parquet_mark_column_written(writer, name)

        allocate(packed(size(data)))
        packed = reshape(data, [size(data)])

        call parquet_append_as_schema_float32(writer, name, packed, int(nrows, kind=c_long_long), int(asize, kind=c_long_long))
    end procedure parquet_write_float32_matrix_column

    module procedure parquet_write_float64_column
        integer :: asize, nrows, idx

        if (writer%enforce_schema) then
            idx = parquet_get_defined_column_index(writer, name)
            if (idx == 0) error stop "parquet_write_column: column not defined in parquet_open_writer: " // trim(name)
            if (.not. writer%all_columns(idx)%is_set) return
        end if

        call parquet_assert_column_type(writer, name, "float64")

        if (.not. parquet_is_column_enabled(writer, name)) return
        call parquet_mark_column_written(writer, name)

        asize = parquet_get_column_col_size(writer, name)
        if (mod(size(data), asize) /= 0) stop "parquet_write_float64_column: data size is not divisible by col_size"
        nrows = size(data) / asize

        call parquet_append_as_schema_float64(writer, name, data, int(nrows, kind=c_long_long), int(asize, kind=c_long_long))
    end procedure parquet_write_float64_column

    module procedure parquet_write_float64_matrix_column
        integer :: asize, nrows, idx
        real(real64), allocatable :: packed(:)

        asize = size(data, 1)
        nrows = size(data, 2)

        if (writer%enforce_schema) then
            idx = parquet_get_defined_column_index(writer, name)
            if (idx == 0) error stop "parquet_write_column: column not defined in parquet_open_writer: " // trim(name)
            if (.not. writer%all_columns(idx)%is_set) return
            if (writer%all_columns(idx)%col_size /= asize) then
                error stop "parquet_write_column: array size mismatch for column " // trim(name)
            end if
        end if

        call parquet_assert_column_type(writer, name, "float64")

        if (.not. parquet_is_column_enabled(writer, name)) return
        call parquet_mark_column_written(writer, name)

        allocate(packed(size(data)))
        packed = reshape(data, [size(data)])

        call parquet_append_as_schema_float64(writer, name, packed, int(nrows, kind=c_long_long), int(asize, kind=c_long_long))
    end procedure parquet_write_float64_matrix_column

    module procedure parquet_write_logical_column
        integer :: asize, nrows, i, idx
        integer(c_int8_t), allocatable :: bool_data(:)

        if (writer%enforce_schema) then
            idx = parquet_get_defined_column_index(writer, name)
            if (idx == 0) error stop "parquet_write_column: column not defined in parquet_open_writer: " // trim(name)
            if (.not. writer%all_columns(idx)%is_set) return
        end if

        call parquet_assert_column_type(writer, name, "boolean")

        if (.not. parquet_is_column_enabled(writer, name)) return
        call parquet_mark_column_written(writer, name)

        asize = parquet_get_column_col_size(writer, name)
        if (mod(size(data), asize) /= 0) stop "parquet_write_logical_column: data size is not divisible by col_size"
        nrows = size(data) / asize

        allocate(bool_data(size(data)))
        do i = 1, size(data)
            if (data(i)) then
                bool_data(i) = 1_c_int8_t
            else
                bool_data(i) = 0_c_int8_t
            end if
        end do

        call parquet_append_bool8_column(&
            writer%handle, &
            trim(name)//char(0), &
            bool_data, &
            int(nrows, kind=c_long_long), &
            int(asize, kind=c_long_long) )
    end procedure parquet_write_logical_column

    module procedure parquet_write_logical_matrix_column
        integer :: asize, nrows, idx
        integer(c_int8_t), allocatable :: bool_data(:)

        asize = size(data, 1)
        nrows = size(data, 2)

        if (writer%enforce_schema) then
            idx = parquet_get_defined_column_index(writer, name)
            if (idx == 0) error stop "parquet_write_column: column not defined in parquet_open_writer: " // trim(name)
            if (.not. writer%all_columns(idx)%is_set) return
            if (writer%all_columns(idx)%col_size /= asize) then
                error stop "parquet_write_column: array size mismatch for column " // trim(name)
            end if
        end if

        call parquet_assert_column_type(writer, name, "boolean")

        if (.not. parquet_is_column_enabled(writer, name)) return
        call parquet_mark_column_written(writer, name)

        allocate(bool_data(size(data)))
        bool_data = merge(1_c_int8_t, 0_c_int8_t, reshape(data, [size(data)]))

        call parquet_append_bool8_column(&
            writer%handle, &
            trim(name)//char(0), &
            bool_data, &
            int(nrows, kind=c_long_long), &
            int(asize, kind=c_long_long) )
    end procedure parquet_write_logical_matrix_column

    module procedure parquet_write_string_column
        character(kind=c_char), allocatable :: packed(:)
        integer :: i, j, k, nrows, item_len, idx, asize, nitems, max_item_len, max_string_len

        if (writer%enforce_schema) then
            idx = parquet_get_defined_column_index(writer, name)
            if (idx == 0) error stop "parquet_write_column: column not defined in parquet_open_writer: " // trim(name)
            if (.not. writer%all_columns(idx)%is_set) return
        end if

        call parquet_assert_column_type(writer, name, "string")

        if (.not. parquet_is_column_enabled(writer, name)) return
        call parquet_mark_column_written(writer, name)

        asize = parquet_get_column_col_size(writer, name)
        nitems = size(data)
        if (nitems <= 0) return
        if (mod(nitems, asize) /= 0) stop "parquet_write_string_column: data size is not divisible by col_size"

        if (writer%enforce_schema) then
            max_string_len = max(1, writer%all_columns(idx)%array_size)
            max_item_len = maxval([(len_trim(data(i)), i=1,nitems)])
            if (max_item_len > max_string_len) then
                error stop "parquet_write_string_column: string length exceeds declared array_size for column: " // trim(name)
            end if
        end if

        nrows = nitems / asize

        item_len = len(data(1))
        allocate(packed(item_len*nitems))

        k = 0
        do i = 1, nitems
            do j = 1, item_len
                k = k + 1
                packed(k) = achar(iachar(data(i)(j:j)), kind=c_char)
            end do
        end do

        if (asize == 1) then
            call parquet_append_string_column(&
                writer%handle, &
                trim(name)//char(0), &
                packed, &
                int(item_len, kind=c_long_long), &
                int(nrows, kind=c_long_long) )
        else
            call parquet_append_string_array_column(&
                writer%handle, &
                trim(name)//char(0), &
                packed, &
                int(item_len, kind=c_long_long), &
                int(nrows, kind=c_long_long), &
                int(asize, kind=c_long_long) )
        end if
    end procedure parquet_write_string_column

    module procedure parquet_write_string_matrix_column
        character(kind=c_char), allocatable :: packed(:)
        integer :: i, j, k, l, nrows, asize, item_len, idx
        integer :: nitems

        asize = size(data, 1)
        nrows = size(data, 2)

        if (writer%enforce_schema) then
            idx = parquet_get_defined_column_index(writer, name)
            if (idx == 0) error stop "parquet_write_column: column not defined in parquet_open_writer: " // trim(name)
            if (.not. writer%all_columns(idx)%is_set) return
            if (writer%all_columns(idx)%col_size /= asize) then
                error stop "parquet_write_column: array size mismatch for column " // trim(name)
            end if
        end if

        call parquet_assert_column_type(writer, name, "string")

        if (.not. parquet_is_column_enabled(writer, name)) return
        call parquet_mark_column_written(writer, name)

        nitems = size(data)
        if (nitems <= 0) return

        item_len = len(data(1, 1))
        allocate(packed(item_len * nitems))

        k = 0
        do i = 1, nrows
            do j = 1, asize
                do l = 1, item_len
                    k = k + 1
                    packed(k) = achar(iachar(data(j, i)(l:l)), kind=c_char)
                end do
            end do
        end do

        call parquet_append_string_array_column(&
            writer%handle, &
            trim(name)//char(0), &
            packed, &
            int(item_len, kind=c_long_long), &
            int(nrows, kind=c_long_long), &
            int(asize, kind=c_long_long) )
    end procedure parquet_write_string_matrix_column

    module procedure parquet_close_writer
        integer :: i

        if (writer%enforce_schema .and. allocated(writer%enabled_columns)) then
            do i = 1, size(writer%enabled_columns)
                if (writer%write_counts(i) == 0) then
                    error stop "parquet_close_writer: missing write for enabled column: " // trim(writer%enabled_columns(i)%name)
                end if
            end do
        end if

        if (c_associated(writer%handle)) then
            call close_parquet_writer(writer%handle)
            writer%handle = c_null_ptr
        end if
        if (allocated(writer%all_columns)) deallocate(writer%all_columns)
        if (allocated(writer%write_counts)) deallocate(writer%write_counts)
        if (allocated(writer%enabled_columns)) deallocate(writer%enabled_columns)
        writer%enforce_schema = .false.
    end procedure parquet_close_writer

    !> Safety net for a writer whose handle is still open when it goes out of
    !> scope or is overwritten (e.g. reassigned, or an early RETURN between
    !> parquet_open_writer and parquet_close_writer): frees the underlying
    !> C++ object so the process doesn't leak it. This intentionally skips
    !> parquet_close_writer's enforce_schema check (erroring from an implicit
    !> finalizer on an incompletely-written file would be surprising) --
    !> always prefer calling parquet_close_writer explicitly.
    module procedure parquet_writer_finalize
        if (c_associated(this%handle)) then
            call close_parquet_writer(this%handle)
            this%handle = c_null_ptr
        end if
    end procedure parquet_writer_finalize

end submodule parquet_write
