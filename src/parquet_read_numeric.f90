!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!> Numeric read specifics (int32/int64/float32/float64/logical: column_1d,
!> array_full, array_row_mode[_row_index], array_element_mode, and
!> column_chunk/array_column_chunk, both row-group-index kinds): bodies of
!> the module procedures declared in parquet_core.f90's interface block, plus
!> the numeric-only private per-mode worker helpers they depend on.
submodule (parquet_core:parquet_read) parquet_read_numeric
    implicit none
contains

    module procedure parquet_read_int32_column_1d
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        integer(int64) :: i

        call check_reader_open(reader, "parquet_read_column")
        call check_column_exists(reader, name, "parquet_read_column")
        call parquet_check_read_row_count(reader, name, size(values, kind=c_long_long))
        call make_valid_buf(present(null_value) .or. present(is_valid), size(values, kind=int64), valid_buf, valid_ptr)
        call parquet_read_int32_column(reader%handle, trim(name)//char(0), values, size(values, kind=c_long_long), valid_ptr)
        if (present(is_valid)) is_valid = valid_buf /= 0_c_int8_t
        if (present(null_value)) then
            do i = 1_int64, size(values, kind=int64)
                if (valid_buf(i) == 0_c_int8_t) values(i) = null_value
            end do
        end if
    end procedure parquet_read_int32_column_1d
    module procedure parquet_read_int64_column_1d
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        integer(int64) :: i

        call check_reader_open(reader, "parquet_read_column")
        call check_column_exists(reader, name, "parquet_read_column")
        call parquet_check_read_row_count(reader, name, size(values, kind=c_long_long))
        call make_valid_buf(present(null_value) .or. present(is_valid), size(values, kind=int64), valid_buf, valid_ptr)
        call parquet_read_int64_column(reader%handle, trim(name)//char(0), values, size(values, kind=c_long_long), valid_ptr)
        if (present(is_valid)) is_valid = valid_buf /= 0_c_int8_t
        if (present(null_value)) then
            do i = 1_int64, size(values, kind=int64)
                if (valid_buf(i) == 0_c_int8_t) values(i) = null_value
            end do
        end if
    end procedure parquet_read_int64_column_1d
    module procedure parquet_read_float32_column_1d
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        integer(int64) :: i

        call check_reader_open(reader, "parquet_read_column")
        call check_column_exists(reader, name, "parquet_read_column")
        call parquet_check_read_row_count(reader, name, size(values, kind=c_long_long))
        call make_valid_buf(present(null_value) .or. present(is_valid), size(values, kind=int64), valid_buf, valid_ptr)
        call parquet_read_float32_column(reader%handle, trim(name)//char(0), values, size(values, kind=c_long_long), valid_ptr)
        if (present(is_valid)) is_valid = valid_buf /= 0_c_int8_t
        if (present(null_value)) then
            do i = 1_int64, size(values, kind=int64)
                if (valid_buf(i) == 0_c_int8_t) values(i) = null_value
            end do
        end if
    end procedure parquet_read_float32_column_1d
    module procedure parquet_read_float64_column_1d
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        integer(int64) :: i

        call check_reader_open(reader, "parquet_read_column")
        call check_column_exists(reader, name, "parquet_read_column")
        call parquet_check_read_row_count(reader, name, size(values, kind=c_long_long))
        call make_valid_buf(present(null_value) .or. present(is_valid), size(values, kind=int64), valid_buf, valid_ptr)
        call parquet_read_float64_column(reader%handle, trim(name)//char(0), values, size(values, kind=c_long_long), valid_ptr)
        if (present(is_valid)) is_valid = valid_buf /= 0_c_int8_t
        if (present(null_value)) then
            do i = 1_int64, size(values, kind=int64)
                if (valid_buf(i) == 0_c_int8_t) values(i) = null_value
            end do
        end if
    end procedure parquet_read_float64_column_1d
    module procedure parquet_read_logical_column_1d
        integer(c_int8_t), allocatable :: tmp(:)
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        integer(int64) :: i

        call check_reader_open(reader, "parquet_read_column")
        call check_column_exists(reader, name, "parquet_read_column")
        call parquet_check_read_row_count(reader, name, size(values, kind=c_long_long))
        allocate(tmp(size(values, kind=int64)))
        call make_valid_buf(present(null_value) .or. present(is_valid), size(values, kind=int64), valid_buf, valid_ptr)
        call parquet_read_bool8_column(reader%handle, trim(name)//char(0), tmp, size(tmp, kind=c_long_long), valid_ptr)
        do i = 1_int64, size(values, kind=int64)
            values(i) = tmp(i) /= 0_c_int8_t
        end do
        if (present(is_valid)) is_valid = valid_buf /= 0_c_int8_t
        if (present(null_value)) then
            do i = 1_int64, size(values, kind=int64)
                if (valid_buf(i) == 0_c_int8_t) values(i) = null_value
            end do
        end if
    end procedure parquet_read_logical_column_1d
    module procedure parquet_read_int32_array_full
        integer(c_int32_t), allocatable :: flat(:)
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        integer(int64) :: asize, nrows, i, j, k

        asize = size(values, 1, kind=int64)
        nrows = size(values, 2, kind=int64)
        call check_reader_open(reader, "parquet_read_column")
        call check_column_exists(reader, name, "parquet_read_column")
        call parquet_check_read_row_count(reader, name, nrows)
        allocate(flat(asize*nrows))
        call make_valid_buf(present(null_value) .or. present(is_valid), asize*nrows, valid_buf, valid_ptr)
        call parquet_read_int32_array_column(reader%handle, trim(name)//char(0), flat, nrows, asize, valid_ptr)
        do i = 1_int64, nrows
            do j = 1_int64, asize
                k = (i-1)*asize + j
                values(j, i) = flat(k)
                if (present(is_valid)) is_valid(j, i) = valid_buf(k) /= 0_c_int8_t
                if (present(null_value)) then
                    if (valid_buf(k) == 0_c_int8_t) values(j, i) = null_value
                end if
            end do
        end do
    end procedure parquet_read_int32_array_full
    module procedure parquet_read_int64_array_full
        integer(c_int64_t), allocatable :: flat(:)
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        integer(int64) :: asize, nrows, i, j, k

        asize = size(values, 1, kind=int64)
        nrows = size(values, 2, kind=int64)
        call check_reader_open(reader, "parquet_read_column")
        call check_column_exists(reader, name, "parquet_read_column")
        call parquet_check_read_row_count(reader, name, nrows)
        allocate(flat(asize*nrows))
        call make_valid_buf(present(null_value) .or. present(is_valid), asize*nrows, valid_buf, valid_ptr)
        call parquet_read_int64_array_column(reader%handle, trim(name)//char(0), flat, nrows, asize, valid_ptr)
        do i = 1_int64, nrows
            do j = 1_int64, asize
                k = (i-1)*asize + j
                values(j, i) = flat(k)
                if (present(is_valid)) is_valid(j, i) = valid_buf(k) /= 0_c_int8_t
                if (present(null_value)) then
                    if (valid_buf(k) == 0_c_int8_t) values(j, i) = null_value
                end if
            end do
        end do
    end procedure parquet_read_int64_array_full
    module procedure parquet_read_float32_array_full
        real(c_float), allocatable :: flat(:)
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        integer(int64) :: asize, nrows, i, j, k

        asize = size(values, 1, kind=int64)
        nrows = size(values, 2, kind=int64)
        call check_reader_open(reader, "parquet_read_column")
        call check_column_exists(reader, name, "parquet_read_column")
        call parquet_check_read_row_count(reader, name, nrows)
        allocate(flat(asize*nrows))
        call make_valid_buf(present(null_value) .or. present(is_valid), asize*nrows, valid_buf, valid_ptr)
        call parquet_read_float32_array_column(reader%handle, trim(name)//char(0), flat, nrows, asize, valid_ptr)
        do i = 1_int64, nrows
            do j = 1_int64, asize
                k = (i-1)*asize + j
                values(j, i) = flat(k)
                if (present(is_valid)) is_valid(j, i) = valid_buf(k) /= 0_c_int8_t
                if (present(null_value)) then
                    if (valid_buf(k) == 0_c_int8_t) values(j, i) = null_value
                end if
            end do
        end do
    end procedure parquet_read_float32_array_full
    module procedure parquet_read_float64_array_full
        real(c_double), allocatable :: flat(:)
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        integer(int64) :: asize, nrows, i, j, k

        asize = size(values, 1, kind=int64)
        nrows = size(values, 2, kind=int64)
        call check_reader_open(reader, "parquet_read_column")
        call check_column_exists(reader, name, "parquet_read_column")
        call parquet_check_read_row_count(reader, name, nrows)
        allocate(flat(asize*nrows))
        call make_valid_buf(present(null_value) .or. present(is_valid), asize*nrows, valid_buf, valid_ptr)
        call parquet_read_float64_array_column(reader%handle, trim(name)//char(0), flat, nrows, asize, valid_ptr)
        do i = 1_int64, nrows
            do j = 1_int64, asize
                k = (i-1)*asize + j
                values(j, i) = flat(k)
                if (present(is_valid)) is_valid(j, i) = valid_buf(k) /= 0_c_int8_t
                if (present(null_value)) then
                    if (valid_buf(k) == 0_c_int8_t) values(j, i) = null_value
                end if
            end do
        end do
    end procedure parquet_read_float64_array_full
    module procedure parquet_read_logical_array_full
        integer(c_int8_t), allocatable :: flat(:)
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        integer(int64) :: asize, nrows, i, j, k

        asize = size(values, 1, kind=int64)
        nrows = size(values, 2, kind=int64)
        call check_reader_open(reader, "parquet_read_column")
        call check_column_exists(reader, name, "parquet_read_column")
        call parquet_check_read_row_count(reader, name, nrows)
        allocate(flat(asize*nrows))
        call make_valid_buf(present(null_value) .or. present(is_valid), asize*nrows, valid_buf, valid_ptr)
        call parquet_read_bool8_array_column(reader%handle, trim(name)//char(0), flat, nrows, asize, valid_ptr)
        do i = 1_int64, nrows
            do j = 1_int64, asize
                k = (i-1)*asize + j
                values(j, i) = flat(k) /= 0_c_int8_t
                if (present(is_valid)) is_valid(j, i) = valid_buf(k) /= 0_c_int8_t
                if (present(null_value)) then
                    if (valid_buf(k) == 0_c_int8_t) values(j, i) = null_value
                end if
            end do
        end do
    end procedure parquet_read_logical_array_full
    !> Shared body of parquet_read_int32_array_row_mode/_row_index_int64 -- see the generic
    !> interface's own doc comment in parquet_core.f90 for why row_index has two kind-specifics.
    subroutine parquet_read_int32_array_row_mode_impl(reader, name, values, row_index, null_value, is_valid)
        type(parquet_reader), intent(in) :: reader !! open reader.
        character(len=*), intent(in) :: name !! vector column name.
        integer(int32), intent(out) :: values(:) !! that row's element vector.
        integer(int64), intent(in) :: row_index !! 1-based row.
        integer(int32), intent(in), optional :: null_value !! fill value for missing entries.
        logical, intent(out), optional :: is_valid(:) !! per-element validity mask.
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        integer(int64) :: i

        call check_reader_open(reader, "parquet_read_array_row_mode")
        call check_column_exists(reader, name, "parquet_read_array_row_mode")
        call make_valid_buf(present(null_value) .or. present(is_valid), size(values, kind=int64), valid_buf, valid_ptr)
        call parquet_read_int32_array_row(reader%handle, trim(name)//char(0), int(row_index, kind=c_long_long), values, &
            size(values, kind=c_long_long), valid_ptr)
        if (present(is_valid)) is_valid = valid_buf /= 0_c_int8_t
        if (present(null_value)) then
            do i = 1_int64, size(values, kind=int64)
                if (valid_buf(i) == 0_c_int8_t) values(i) = null_value
            end do
        end if
    end subroutine parquet_read_int32_array_row_mode_impl
    module procedure parquet_read_int32_array_row_mode
        call parquet_read_int32_array_row_mode_impl(reader, name, values, int(row_index, kind=int64), null_value, is_valid)
    end procedure parquet_read_int32_array_row_mode
    module procedure parquet_read_int32_array_row_mode_row_index_int64
        call parquet_read_int32_array_row_mode_impl(reader, name, values, row_index, null_value, is_valid)
    end procedure parquet_read_int32_array_row_mode_row_index_int64
    !> Shared body of parquet_read_int64_array_row_mode/_row_index_int64.
    subroutine parquet_read_int64_array_row_mode_impl(reader, name, values, row_index, null_value, is_valid)
        type(parquet_reader), intent(in) :: reader !! open reader.
        character(len=*), intent(in) :: name !! vector column name.
        integer(int64), intent(out) :: values(:) !! that row's element vector.
        integer(int64), intent(in) :: row_index !! 1-based row.
        integer(int64), intent(in), optional :: null_value !! fill value for missing entries.
        logical, intent(out), optional :: is_valid(:) !! per-element validity mask.
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        integer(int64) :: i

        call check_reader_open(reader, "parquet_read_array_row_mode")
        call check_column_exists(reader, name, "parquet_read_array_row_mode")
        call make_valid_buf(present(null_value) .or. present(is_valid), size(values, kind=int64), valid_buf, valid_ptr)
        call parquet_read_int64_array_row(reader%handle, trim(name)//char(0), int(row_index, kind=c_long_long), values, &
            size(values, kind=c_long_long), valid_ptr)
        if (present(is_valid)) is_valid = valid_buf /= 0_c_int8_t
        if (present(null_value)) then
            do i = 1_int64, size(values, kind=int64)
                if (valid_buf(i) == 0_c_int8_t) values(i) = null_value
            end do
        end if
    end subroutine parquet_read_int64_array_row_mode_impl
    module procedure parquet_read_int64_array_row_mode
        call parquet_read_int64_array_row_mode_impl(reader, name, values, int(row_index, kind=int64), null_value, is_valid)
    end procedure parquet_read_int64_array_row_mode
    module procedure parquet_read_int64_array_row_mode_row_index_int64
        call parquet_read_int64_array_row_mode_impl(reader, name, values, row_index, null_value, is_valid)
    end procedure parquet_read_int64_array_row_mode_row_index_int64
    !> Shared body of parquet_read_float32_array_row_mode/_row_index_int64.
    subroutine parquet_read_float32_array_row_mode_impl(reader, name, values, row_index, null_value, is_valid)
        type(parquet_reader), intent(in) :: reader !! open reader.
        character(len=*), intent(in) :: name !! vector column name.
        real(real32), intent(out) :: values(:) !! that row's element vector.
        integer(int64), intent(in) :: row_index !! 1-based row.
        real(real32), intent(in), optional :: null_value !! fill value for missing entries.
        logical, intent(out), optional :: is_valid(:) !! per-element validity mask.
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        integer(int64) :: i

        call check_reader_open(reader, "parquet_read_array_row_mode")
        call check_column_exists(reader, name, "parquet_read_array_row_mode")
        call make_valid_buf(present(null_value) .or. present(is_valid), size(values, kind=int64), valid_buf, valid_ptr)
        call parquet_read_float32_array_row(reader%handle, trim(name)//char(0), int(row_index, kind=c_long_long), values, &
            size(values, kind=c_long_long), valid_ptr)
        if (present(is_valid)) is_valid = valid_buf /= 0_c_int8_t
        if (present(null_value)) then
            do i = 1_int64, size(values, kind=int64)
                if (valid_buf(i) == 0_c_int8_t) values(i) = null_value
            end do
        end if
    end subroutine parquet_read_float32_array_row_mode_impl
    module procedure parquet_read_float32_array_row_mode
        call parquet_read_float32_array_row_mode_impl(reader, name, values, int(row_index, kind=int64), null_value, &
            is_valid)
    end procedure parquet_read_float32_array_row_mode
    module procedure parquet_read_float32_array_row_mode_row_index_int64
        call parquet_read_float32_array_row_mode_impl(reader, name, values, row_index, null_value, is_valid)
    end procedure parquet_read_float32_array_row_mode_row_index_int64
    !> Shared body of parquet_read_float64_array_row_mode/_row_index_int64.
    subroutine parquet_read_float64_array_row_mode_impl(reader, name, values, row_index, null_value, is_valid)
        type(parquet_reader), intent(in) :: reader !! open reader.
        character(len=*), intent(in) :: name !! vector column name.
        real(real64), intent(out) :: values(:) !! that row's element vector.
        integer(int64), intent(in) :: row_index !! 1-based row.
        real(real64), intent(in), optional :: null_value !! fill value for missing entries.
        logical, intent(out), optional :: is_valid(:) !! per-element validity mask.
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        integer(int64) :: i

        call check_reader_open(reader, "parquet_read_array_row_mode")
        call check_column_exists(reader, name, "parquet_read_array_row_mode")
        call make_valid_buf(present(null_value) .or. present(is_valid), size(values, kind=int64), valid_buf, valid_ptr)
        call parquet_read_float64_array_row(reader%handle, trim(name)//char(0), int(row_index, kind=c_long_long), values, &
            size(values, kind=c_long_long), valid_ptr)
        if (present(is_valid)) is_valid = valid_buf /= 0_c_int8_t
        if (present(null_value)) then
            do i = 1_int64, size(values, kind=int64)
                if (valid_buf(i) == 0_c_int8_t) values(i) = null_value
            end do
        end if
    end subroutine parquet_read_float64_array_row_mode_impl
    module procedure parquet_read_float64_array_row_mode
        call parquet_read_float64_array_row_mode_impl(reader, name, values, int(row_index, kind=int64), null_value, &
            is_valid)
    end procedure parquet_read_float64_array_row_mode
    module procedure parquet_read_float64_array_row_mode_row_index_int64
        call parquet_read_float64_array_row_mode_impl(reader, name, values, row_index, null_value, is_valid)
    end procedure parquet_read_float64_array_row_mode_row_index_int64
    !> Shared body of parquet_read_logical_array_row_mode/_row_index_int64.
    subroutine parquet_read_logical_array_row_mode_impl(reader, name, values, row_index, null_value, is_valid)
        type(parquet_reader), intent(in) :: reader !! open reader.
        character(len=*), intent(in) :: name !! vector column name.
        logical, intent(out) :: values(:) !! that row's element vector.
        integer(int64), intent(in) :: row_index !! 1-based row.
        logical, intent(in), optional :: null_value !! fill value for missing entries.
        logical, intent(out), optional :: is_valid(:) !! per-element validity mask.
        integer(c_int8_t), allocatable :: tmp(:)
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        integer(int64) :: i

        call check_reader_open(reader, "parquet_read_array_row_mode")
        call check_column_exists(reader, name, "parquet_read_array_row_mode")
        allocate(tmp(size(values, kind=int64)))
        call make_valid_buf(present(null_value) .or. present(is_valid), size(values, kind=int64), valid_buf, valid_ptr)
        call parquet_read_bool8_array_row(reader%handle, trim(name)//char(0), int(row_index, kind=c_long_long), tmp, &
            size(values, kind=c_long_long), valid_ptr)
        do i = 1_int64, size(values, kind=int64)
            values(i) = tmp(i) /= 0_c_int8_t
        end do
        if (present(is_valid)) is_valid = valid_buf /= 0_c_int8_t
        if (present(null_value)) then
            do i = 1_int64, size(values, kind=int64)
                if (valid_buf(i) == 0_c_int8_t) values(i) = null_value
            end do
        end if
    end subroutine parquet_read_logical_array_row_mode_impl
    module procedure parquet_read_logical_array_row_mode
        call parquet_read_logical_array_row_mode_impl(reader, name, values, int(row_index, kind=int64), null_value, &
            is_valid)
    end procedure parquet_read_logical_array_row_mode
    module procedure parquet_read_logical_array_row_mode_row_index_int64
        call parquet_read_logical_array_row_mode_impl(reader, name, values, row_index, null_value, is_valid)
    end procedure parquet_read_logical_array_row_mode_row_index_int64
    module procedure parquet_read_int32_array_element_mode
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        integer(int64) :: i

        call check_reader_open(reader, "parquet_read_array_element_mode")
        call check_column_exists(reader, name, "parquet_read_array_element_mode")
        call make_valid_buf(present(null_value) .or. present(is_valid), size(values, kind=int64), valid_buf, valid_ptr)
        call parquet_read_int32_array_element(reader%handle, trim(name)//char(0), int(elem_index, kind=c_long_long), values, &
            size(values, kind=c_long_long), 0_c_long_long, valid_ptr)
        if (present(is_valid)) is_valid = valid_buf /= 0_c_int8_t
        if (present(null_value)) then
            do i = 1_int64, size(values, kind=int64)
                if (valid_buf(i) == 0_c_int8_t) values(i) = null_value
            end do
        end if
    end procedure parquet_read_int32_array_element_mode
    module procedure parquet_read_int64_array_element_mode
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        integer(int64) :: i

        call check_reader_open(reader, "parquet_read_array_element_mode")
        call check_column_exists(reader, name, "parquet_read_array_element_mode")
        call make_valid_buf(present(null_value) .or. present(is_valid), size(values, kind=int64), valid_buf, valid_ptr)
        call parquet_read_int64_array_element(reader%handle, trim(name)//char(0), int(elem_index, kind=c_long_long), values, &
            size(values, kind=c_long_long), 0_c_long_long, valid_ptr)
        if (present(is_valid)) is_valid = valid_buf /= 0_c_int8_t
        if (present(null_value)) then
            do i = 1_int64, size(values, kind=int64)
                if (valid_buf(i) == 0_c_int8_t) values(i) = null_value
            end do
        end if
    end procedure parquet_read_int64_array_element_mode
    module procedure parquet_read_float32_array_element_mode
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        integer(int64) :: i

        call check_reader_open(reader, "parquet_read_array_element_mode")
        call check_column_exists(reader, name, "parquet_read_array_element_mode")
        call make_valid_buf(present(null_value) .or. present(is_valid), size(values, kind=int64), valid_buf, valid_ptr)
        call parquet_read_float32_array_element(reader%handle, trim(name)//char(0), int(elem_index, kind=c_long_long), values, &
            size(values, kind=c_long_long), 0_c_long_long, valid_ptr)
        if (present(is_valid)) is_valid = valid_buf /= 0_c_int8_t
        if (present(null_value)) then
            do i = 1_int64, size(values, kind=int64)
                if (valid_buf(i) == 0_c_int8_t) values(i) = null_value
            end do
        end if
    end procedure parquet_read_float32_array_element_mode
    module procedure parquet_read_float64_array_element_mode
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        integer(int64) :: i

        call check_reader_open(reader, "parquet_read_array_element_mode")
        call check_column_exists(reader, name, "parquet_read_array_element_mode")
        call make_valid_buf(present(null_value) .or. present(is_valid), size(values, kind=int64), valid_buf, valid_ptr)
        call parquet_read_float64_array_element(reader%handle, trim(name)//char(0), int(elem_index, kind=c_long_long), values, &
            size(values, kind=c_long_long), 0_c_long_long, valid_ptr)
        if (present(is_valid)) is_valid = valid_buf /= 0_c_int8_t
        if (present(null_value)) then
            do i = 1_int64, size(values, kind=int64)
                if (valid_buf(i) == 0_c_int8_t) values(i) = null_value
            end do
        end if
    end procedure parquet_read_float64_array_element_mode
    module procedure parquet_read_logical_array_element_mode
        integer(c_int8_t), allocatable :: tmp(:)
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        integer(int64) :: i

        call check_reader_open(reader, "parquet_read_array_element_mode")
        call check_column_exists(reader, name, "parquet_read_array_element_mode")
        allocate(tmp(size(values, kind=int64)))
        call make_valid_buf(present(null_value) .or. present(is_valid), size(values, kind=int64), valid_buf, valid_ptr)
        call parquet_read_bool8_array_element(reader%handle, trim(name)//char(0), int(elem_index, kind=c_long_long), tmp, &
            size(values, kind=c_long_long), 0_c_long_long, valid_ptr)
        do i = 1_int64, size(values, kind=int64)
            values(i) = tmp(i) /= 0_c_int8_t
        end do
        if (present(is_valid)) is_valid = valid_buf /= 0_c_int8_t
        if (present(null_value)) then
            do i = 1_int64, size(values, kind=int64)
                if (valid_buf(i) == 0_c_int8_t) values(i) = null_value
            end do
        end if
    end procedure parquet_read_logical_array_element_mode
    !> Shared body of parquet_read_int32_column_chunk_rg32/_rg64 -- see the generic
    !> interface's own doc comment in parquet_core.f90 for why row_group has two kind-specifics.
    subroutine parquet_read_int32_column_chunk_impl(reader, name, row_group, values, null_value, is_valid)
        type(parquet_reader), intent(in) :: reader !! open reader.
        character(len=*), intent(in) :: name !! column name.
        integer(int64), intent(in) :: row_group !! 1-based row group.
        integer(int32), intent(out) :: values(:) !! one value per row of the selected row group.
        integer(int32), intent(in), optional :: null_value !! fill value for missing entries.
        logical, intent(out), optional :: is_valid(:) !! per-row validity mask.
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        integer(int64) :: i

        call check_reader_open(reader, "parquet_read_column_chunk")
        call check_column_exists(reader, name, "parquet_read_column_chunk")
        call check_reader_no_sort(reader, "parquet_read_column_chunk")
        call check_row_group_valid(reader, row_group, "parquet_read_column_chunk")
        call make_valid_buf(present(null_value) .or. present(is_valid), size(values, kind=int64), valid_buf, valid_ptr)
        call parquet_read_int32_column_chunk(reader%handle, trim(name)//char(0), row_group, values, &
            size(values, kind=c_long_long), valid_ptr)
        if (present(is_valid)) is_valid = valid_buf /= 0_c_int8_t
        if (present(null_value)) then
            do i = 1_int64, size(values, kind=int64)
                if (valid_buf(i) == 0_c_int8_t) values(i) = null_value
            end do
        end if
    end subroutine parquet_read_int32_column_chunk_impl
    !> Shared body of parquet_read_int32_array_column_chunk_rg32/_rg64 -- see the generic
    !> interface's own doc comment in parquet_core.f90 for why row_group has two kind-specifics.
    subroutine parquet_read_int32_array_column_chunk_impl(reader, name, row_group, values, null_value, is_valid)
        type(parquet_reader), intent(in) :: reader !! open reader.
        character(len=*), intent(in) :: name !! vector column name.
        integer(int64), intent(in) :: row_group !! 1-based row group.
        integer(int32), intent(out) :: values(:, :) !! (element, row) values of the selected row group.
        integer(int32), intent(in), optional :: null_value !! fill value for missing entries.
        logical, intent(out), optional :: is_valid(:, :) !! per-element validity mask.
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        integer(int64) :: asize, nrows, i, j, k

        asize = size(values, 1, kind=int64)
        nrows = size(values, 2, kind=int64)
        call check_reader_open(reader, "parquet_read_column_chunk")
        call check_column_exists(reader, name, "parquet_read_column_chunk")
        call check_reader_no_sort(reader, "parquet_read_column_chunk")
        call check_row_group_valid(reader, row_group, "parquet_read_column_chunk")
        call make_valid_buf(present(null_value) .or. present(is_valid), asize*nrows, valid_buf, valid_ptr)
        call parquet_read_int32_array_column_chunk(reader%handle, trim(name)//char(0), row_group, values, &
            nrows, asize, valid_ptr)
        if (present(is_valid) .or. present(null_value)) then
            do i = 1_int64, nrows
                do j = 1_int64, asize
                    k = (i-1)*asize + j
                    if (present(is_valid)) is_valid(j, i) = valid_buf(k) /= 0_c_int8_t
                    if (present(null_value)) then
                        if (valid_buf(k) == 0_c_int8_t) values(j, i) = null_value
                    end if
                end do
            end do
        end if
    end subroutine parquet_read_int32_array_column_chunk_impl
    !> Shared body of parquet_read_int64_column_chunk_rg32/_rg64 -- see the generic
    !> interface's own doc comment in parquet_core.f90 for why row_group has two kind-specifics.
    subroutine parquet_read_int64_column_chunk_impl(reader, name, row_group, values, null_value, is_valid)
        type(parquet_reader), intent(in) :: reader !! open reader.
        character(len=*), intent(in) :: name !! column name.
        integer(int64), intent(in) :: row_group !! 1-based row group.
        integer(int64), intent(out) :: values(:) !! one value per row of the selected row group.
        integer(int64), intent(in), optional :: null_value !! fill value for missing entries.
        logical, intent(out), optional :: is_valid(:) !! per-row validity mask.
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        integer(int64) :: i

        call check_reader_open(reader, "parquet_read_column_chunk")
        call check_column_exists(reader, name, "parquet_read_column_chunk")
        call check_reader_no_sort(reader, "parquet_read_column_chunk")
        call check_row_group_valid(reader, row_group, "parquet_read_column_chunk")
        call make_valid_buf(present(null_value) .or. present(is_valid), size(values, kind=int64), valid_buf, valid_ptr)
        call parquet_read_int64_column_chunk(reader%handle, trim(name)//char(0), row_group, values, &
            size(values, kind=c_long_long), valid_ptr)
        if (present(is_valid)) is_valid = valid_buf /= 0_c_int8_t
        if (present(null_value)) then
            do i = 1_int64, size(values, kind=int64)
                if (valid_buf(i) == 0_c_int8_t) values(i) = null_value
            end do
        end if
    end subroutine parquet_read_int64_column_chunk_impl
    !> Shared body of parquet_read_int64_array_column_chunk_rg32/_rg64 -- see the generic
    !> interface's own doc comment in parquet_core.f90 for why row_group has two kind-specifics.
    subroutine parquet_read_int64_array_column_chunk_impl(reader, name, row_group, values, null_value, is_valid)
        type(parquet_reader), intent(in) :: reader !! open reader.
        character(len=*), intent(in) :: name !! vector column name.
        integer(int64), intent(in) :: row_group !! 1-based row group.
        integer(int64), intent(out) :: values(:, :) !! (element, row) values of the selected row group.
        integer(int64), intent(in), optional :: null_value !! fill value for missing entries.
        logical, intent(out), optional :: is_valid(:, :) !! per-element validity mask.
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        integer(int64) :: asize, nrows, i, j, k

        asize = size(values, 1, kind=int64)
        nrows = size(values, 2, kind=int64)
        call check_reader_open(reader, "parquet_read_column_chunk")
        call check_column_exists(reader, name, "parquet_read_column_chunk")
        call check_reader_no_sort(reader, "parquet_read_column_chunk")
        call check_row_group_valid(reader, row_group, "parquet_read_column_chunk")
        call make_valid_buf(present(null_value) .or. present(is_valid), asize*nrows, valid_buf, valid_ptr)
        call parquet_read_int64_array_column_chunk(reader%handle, trim(name)//char(0), row_group, values, &
            nrows, asize, valid_ptr)
        if (present(is_valid) .or. present(null_value)) then
            do i = 1_int64, nrows
                do j = 1_int64, asize
                    k = (i-1)*asize + j
                    if (present(is_valid)) is_valid(j, i) = valid_buf(k) /= 0_c_int8_t
                    if (present(null_value)) then
                        if (valid_buf(k) == 0_c_int8_t) values(j, i) = null_value
                    end if
                end do
            end do
        end if
    end subroutine parquet_read_int64_array_column_chunk_impl
    !> Shared body of parquet_read_float32_column_chunk_rg32/_rg64 -- see the generic
    !> interface's own doc comment in parquet_core.f90 for why row_group has two kind-specifics.
    subroutine parquet_read_float32_column_chunk_impl(reader, name, row_group, values, null_value, is_valid)
        type(parquet_reader), intent(in) :: reader !! open reader.
        character(len=*), intent(in) :: name !! column name.
        integer(int64), intent(in) :: row_group !! 1-based row group.
        real(real32), intent(out) :: values(:) !! one value per row of the selected row group.
        real(real32), intent(in), optional :: null_value !! fill value for missing entries.
        logical, intent(out), optional :: is_valid(:) !! per-row validity mask.
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        integer(int64) :: i

        call check_reader_open(reader, "parquet_read_column_chunk")
        call check_column_exists(reader, name, "parquet_read_column_chunk")
        call check_reader_no_sort(reader, "parquet_read_column_chunk")
        call check_row_group_valid(reader, row_group, "parquet_read_column_chunk")
        call make_valid_buf(present(null_value) .or. present(is_valid), size(values, kind=int64), valid_buf, valid_ptr)
        call parquet_read_float32_column_chunk(reader%handle, trim(name)//char(0), row_group, values, &
            size(values, kind=c_long_long), valid_ptr)
        if (present(is_valid)) is_valid = valid_buf /= 0_c_int8_t
        if (present(null_value)) then
            do i = 1_int64, size(values, kind=int64)
                if (valid_buf(i) == 0_c_int8_t) values(i) = null_value
            end do
        end if
    end subroutine parquet_read_float32_column_chunk_impl
    !> Shared body of parquet_read_float32_array_column_chunk_rg32/_rg64 -- see the generic
    !> interface's own doc comment in parquet_core.f90 for why row_group has two kind-specifics.
    subroutine parquet_read_float32_array_column_chunk_impl(reader, name, row_group, values, null_value, is_valid)
        type(parquet_reader), intent(in) :: reader !! open reader.
        character(len=*), intent(in) :: name !! vector column name.
        integer(int64), intent(in) :: row_group !! 1-based row group.
        real(real32), intent(out) :: values(:, :) !! (element, row) values of the selected row group.
        real(real32), intent(in), optional :: null_value !! fill value for missing entries.
        logical, intent(out), optional :: is_valid(:, :) !! per-element validity mask.
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        integer(int64) :: asize, nrows, i, j, k

        asize = size(values, 1, kind=int64)
        nrows = size(values, 2, kind=int64)
        call check_reader_open(reader, "parquet_read_column_chunk")
        call check_column_exists(reader, name, "parquet_read_column_chunk")
        call check_reader_no_sort(reader, "parquet_read_column_chunk")
        call check_row_group_valid(reader, row_group, "parquet_read_column_chunk")
        call make_valid_buf(present(null_value) .or. present(is_valid), asize*nrows, valid_buf, valid_ptr)
        call parquet_read_float32_array_column_chunk(reader%handle, trim(name)//char(0), row_group, values, &
            nrows, asize, valid_ptr)
        if (present(is_valid) .or. present(null_value)) then
            do i = 1_int64, nrows
                do j = 1_int64, asize
                    k = (i-1)*asize + j
                    if (present(is_valid)) is_valid(j, i) = valid_buf(k) /= 0_c_int8_t
                    if (present(null_value)) then
                        if (valid_buf(k) == 0_c_int8_t) values(j, i) = null_value
                    end if
                end do
            end do
        end if
    end subroutine parquet_read_float32_array_column_chunk_impl
    !> Shared body of parquet_read_float64_column_chunk_rg32/_rg64 -- see the generic
    !> interface's own doc comment in parquet_core.f90 for why row_group has two kind-specifics.
    subroutine parquet_read_float64_column_chunk_impl(reader, name, row_group, values, null_value, is_valid)
        type(parquet_reader), intent(in) :: reader !! open reader.
        character(len=*), intent(in) :: name !! column name.
        integer(int64), intent(in) :: row_group !! 1-based row group.
        real(real64), intent(out) :: values(:) !! one value per row of the selected row group.
        real(real64), intent(in), optional :: null_value !! fill value for missing entries.
        logical, intent(out), optional :: is_valid(:) !! per-row validity mask.
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        integer(int64) :: i

        call check_reader_open(reader, "parquet_read_column_chunk")
        call check_column_exists(reader, name, "parquet_read_column_chunk")
        call check_reader_no_sort(reader, "parquet_read_column_chunk")
        call check_row_group_valid(reader, row_group, "parquet_read_column_chunk")
        call make_valid_buf(present(null_value) .or. present(is_valid), size(values, kind=int64), valid_buf, valid_ptr)
        call parquet_read_float64_column_chunk(reader%handle, trim(name)//char(0), row_group, values, &
            size(values, kind=c_long_long), valid_ptr)
        if (present(is_valid)) is_valid = valid_buf /= 0_c_int8_t
        if (present(null_value)) then
            do i = 1_int64, size(values, kind=int64)
                if (valid_buf(i) == 0_c_int8_t) values(i) = null_value
            end do
        end if
    end subroutine parquet_read_float64_column_chunk_impl
    !> Shared body of parquet_read_float64_array_column_chunk_rg32/_rg64 -- see the generic
    !> interface's own doc comment in parquet_core.f90 for why row_group has two kind-specifics.
    subroutine parquet_read_float64_array_column_chunk_impl(reader, name, row_group, values, null_value, is_valid)
        type(parquet_reader), intent(in) :: reader !! open reader.
        character(len=*), intent(in) :: name !! vector column name.
        integer(int64), intent(in) :: row_group !! 1-based row group.
        real(real64), intent(out) :: values(:, :) !! (element, row) values of the selected row group.
        real(real64), intent(in), optional :: null_value !! fill value for missing entries.
        logical, intent(out), optional :: is_valid(:, :) !! per-element validity mask.
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        integer(int64) :: asize, nrows, i, j, k

        asize = size(values, 1, kind=int64)
        nrows = size(values, 2, kind=int64)
        call check_reader_open(reader, "parquet_read_column_chunk")
        call check_column_exists(reader, name, "parquet_read_column_chunk")
        call check_reader_no_sort(reader, "parquet_read_column_chunk")
        call check_row_group_valid(reader, row_group, "parquet_read_column_chunk")
        call make_valid_buf(present(null_value) .or. present(is_valid), asize*nrows, valid_buf, valid_ptr)
        call parquet_read_float64_array_column_chunk(reader%handle, trim(name)//char(0), row_group, values, &
            nrows, asize, valid_ptr)
        if (present(is_valid) .or. present(null_value)) then
            do i = 1_int64, nrows
                do j = 1_int64, asize
                    k = (i-1)*asize + j
                    if (present(is_valid)) is_valid(j, i) = valid_buf(k) /= 0_c_int8_t
                    if (present(null_value)) then
                        if (valid_buf(k) == 0_c_int8_t) values(j, i) = null_value
                    end if
                end do
            end do
        end if
    end subroutine parquet_read_float64_array_column_chunk_impl
    !> Shared body of parquet_read_logical_column_chunk_rg32/_rg64 -- see the generic
    !> interface's own doc comment in parquet_core.f90 for why row_group has two kind-specifics.
    subroutine parquet_read_logical_column_chunk_impl(reader, name, row_group, values, null_value, is_valid)
        type(parquet_reader), intent(in) :: reader !! open reader.
        character(len=*), intent(in) :: name !! column name.
        integer(int64), intent(in) :: row_group !! 1-based row group.
        logical, intent(out) :: values(:) !! one value per row of the selected row group.
        logical, intent(in), optional :: null_value !! fill value for missing entries.
        logical, intent(out), optional :: is_valid(:) !! per-row validity mask.
        integer(c_int8_t), allocatable :: tmp(:)
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        integer(int64) :: i

        call check_reader_open(reader, "parquet_read_column_chunk")
        call check_column_exists(reader, name, "parquet_read_column_chunk")
        call check_reader_no_sort(reader, "parquet_read_column_chunk")
        call check_row_group_valid(reader, row_group, "parquet_read_column_chunk")
        allocate(tmp(size(values, kind=int64)))
        call make_valid_buf(present(null_value) .or. present(is_valid), size(values, kind=int64), valid_buf, valid_ptr)
        call parquet_read_bool8_column_chunk(reader%handle, trim(name)//char(0), row_group, tmp, &
            size(tmp, kind=c_long_long), valid_ptr)
        do i = 1_int64, size(values, kind=int64)
            values(i) = tmp(i) /= 0_c_int8_t
        end do
        if (present(is_valid)) is_valid = valid_buf /= 0_c_int8_t
        if (present(null_value)) then
            do i = 1_int64, size(values, kind=int64)
                if (valid_buf(i) == 0_c_int8_t) values(i) = null_value
            end do
        end if
    end subroutine parquet_read_logical_column_chunk_impl
    !> Shared body of parquet_read_logical_array_column_chunk_rg32/_rg64 -- see the generic
    !> interface's own doc comment in parquet_core.f90 for why row_group has two kind-specifics.
    subroutine parquet_read_logical_array_column_chunk_impl(reader, name, row_group, values, null_value, is_valid)
        type(parquet_reader), intent(in) :: reader !! open reader.
        character(len=*), intent(in) :: name !! vector column name.
        integer(int64), intent(in) :: row_group !! 1-based row group.
        logical, intent(out) :: values(:, :) !! (element, row) values of the selected row group.
        logical, intent(in), optional :: null_value !! fill value for missing entries.
        logical, intent(out), optional :: is_valid(:, :) !! per-element validity mask.
        integer(c_int8_t), allocatable :: flat(:)
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        integer(int64) :: asize, nrows, i, j, k

        asize = size(values, 1, kind=int64)
        nrows = size(values, 2, kind=int64)
        call check_reader_open(reader, "parquet_read_column_chunk")
        call check_column_exists(reader, name, "parquet_read_column_chunk")
        call check_reader_no_sort(reader, "parquet_read_column_chunk")
        call check_row_group_valid(reader, row_group, "parquet_read_column_chunk")
        allocate(flat(asize*nrows))
        call make_valid_buf(present(null_value) .or. present(is_valid), asize*nrows, valid_buf, valid_ptr)
        call parquet_read_bool8_array_column_chunk(reader%handle, trim(name)//char(0), row_group, flat, &
            nrows, asize, valid_ptr)
        do i = 1_int64, nrows
            do j = 1_int64, asize
                k = (i-1)*asize + j
                values(j, i) = flat(k) /= 0_c_int8_t
                if (present(is_valid)) is_valid(j, i) = valid_buf(k) /= 0_c_int8_t
                if (present(null_value)) then
                    if (valid_buf(k) == 0_c_int8_t) values(j, i) = null_value
                end if
            end do
        end do
    end subroutine parquet_read_logical_array_column_chunk_impl
    module procedure parquet_read_int32_column_chunk_rg32
        call parquet_read_int32_column_chunk_impl(reader, name, int(row_group, kind=int64), values, null_value, &
            is_valid)
    end procedure parquet_read_int32_column_chunk_rg32
    module procedure parquet_read_int32_column_chunk_rg64
        call parquet_read_int32_column_chunk_impl(reader, name, row_group, values, null_value, is_valid)
    end procedure parquet_read_int32_column_chunk_rg64
    module procedure parquet_read_int32_array_column_chunk_rg32
        call parquet_read_int32_array_column_chunk_impl(reader, name, int(row_group, kind=int64), values, &
            null_value, is_valid)
    end procedure parquet_read_int32_array_column_chunk_rg32
    module procedure parquet_read_int32_array_column_chunk_rg64
        call parquet_read_int32_array_column_chunk_impl(reader, name, row_group, values, null_value, is_valid)
    end procedure parquet_read_int32_array_column_chunk_rg64
    module procedure parquet_read_int64_column_chunk_rg32
        call parquet_read_int64_column_chunk_impl(reader, name, int(row_group, kind=int64), values, null_value, &
            is_valid)
    end procedure parquet_read_int64_column_chunk_rg32
    module procedure parquet_read_int64_column_chunk_rg64
        call parquet_read_int64_column_chunk_impl(reader, name, row_group, values, null_value, is_valid)
    end procedure parquet_read_int64_column_chunk_rg64
    module procedure parquet_read_int64_array_column_chunk_rg32
        call parquet_read_int64_array_column_chunk_impl(reader, name, int(row_group, kind=int64), values, &
            null_value, is_valid)
    end procedure parquet_read_int64_array_column_chunk_rg32
    module procedure parquet_read_int64_array_column_chunk_rg64
        call parquet_read_int64_array_column_chunk_impl(reader, name, row_group, values, null_value, is_valid)
    end procedure parquet_read_int64_array_column_chunk_rg64
    module procedure parquet_read_float32_column_chunk_rg32
        call parquet_read_float32_column_chunk_impl(reader, name, int(row_group, kind=int64), values, null_value, &
            is_valid)
    end procedure parquet_read_float32_column_chunk_rg32
    module procedure parquet_read_float32_column_chunk_rg64
        call parquet_read_float32_column_chunk_impl(reader, name, row_group, values, null_value, is_valid)
    end procedure parquet_read_float32_column_chunk_rg64
    module procedure parquet_read_float32_array_column_chunk_rg32
        call parquet_read_float32_array_column_chunk_impl(reader, name, int(row_group, kind=int64), values, &
            null_value, is_valid)
    end procedure parquet_read_float32_array_column_chunk_rg32
    module procedure parquet_read_float32_array_column_chunk_rg64
        call parquet_read_float32_array_column_chunk_impl(reader, name, row_group, values, null_value, is_valid)
    end procedure parquet_read_float32_array_column_chunk_rg64
    module procedure parquet_read_float64_column_chunk_rg32
        call parquet_read_float64_column_chunk_impl(reader, name, int(row_group, kind=int64), values, null_value, &
            is_valid)
    end procedure parquet_read_float64_column_chunk_rg32
    module procedure parquet_read_float64_column_chunk_rg64
        call parquet_read_float64_column_chunk_impl(reader, name, row_group, values, null_value, is_valid)
    end procedure parquet_read_float64_column_chunk_rg64
    module procedure parquet_read_float64_array_column_chunk_rg32
        call parquet_read_float64_array_column_chunk_impl(reader, name, int(row_group, kind=int64), values, &
            null_value, is_valid)
    end procedure parquet_read_float64_array_column_chunk_rg32
    module procedure parquet_read_float64_array_column_chunk_rg64
        call parquet_read_float64_array_column_chunk_impl(reader, name, row_group, values, null_value, is_valid)
    end procedure parquet_read_float64_array_column_chunk_rg64
    module procedure parquet_read_logical_column_chunk_rg32
        call parquet_read_logical_column_chunk_impl(reader, name, int(row_group, kind=int64), values, null_value, &
            is_valid)
    end procedure parquet_read_logical_column_chunk_rg32
    module procedure parquet_read_logical_column_chunk_rg64
        call parquet_read_logical_column_chunk_impl(reader, name, row_group, values, null_value, is_valid)
    end procedure parquet_read_logical_column_chunk_rg64
    module procedure parquet_read_logical_array_column_chunk_rg32
        call parquet_read_logical_array_column_chunk_impl(reader, name, int(row_group, kind=int64), values, &
            null_value, is_valid)
    end procedure parquet_read_logical_array_column_chunk_rg32
    module procedure parquet_read_logical_array_column_chunk_rg64
        call parquet_read_logical_array_column_chunk_impl(reader, name, row_group, values, null_value, is_valid)
    end procedure parquet_read_logical_array_column_chunk_rg64

end submodule parquet_read_numeric
