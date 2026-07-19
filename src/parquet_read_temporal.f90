!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!> Temporal (date/time/timestamp) read specifics: column_1d, array_full,
!> column_chunk/array_column_chunk (both row-group-index kinds),
!> array_row_mode[_row_index], and array_element_mode: bodies of the module
!> procedures declared in parquet.f90's interface block, plus the
!> temporal-only private per-mode worker helpers they depend on.
submodule (parquet:parquet_read) parquet_read_temporal
    implicit none
contains

    module procedure parquet_read_date_column_1d
        integer(c_int32_t), allocatable :: days(:)
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        integer(int64) :: i, n

        call check_reader_open(reader, "parquet_read_column")
        call check_column_exists(reader, name, "parquet_read_column")
        n = size(values, kind=int64)
        call parquet_check_read_row_count(reader, name, n)
        allocate(days(n))
        call make_valid_buf(.true., n, valid_buf, valid_ptr)
        call parquet_read_date_column(reader%handle, trim(name)//char(0), days, n, valid_ptr)
        do i = 1_int64, n
            if (valid_buf(i) /= 0_c_int8_t) then
                call values(i)%set_raw(days(i))
            else
                call values(i)%set_null()
            end if
        end do
    end procedure parquet_read_date_column_1d
    module procedure parquet_read_date_array_full
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
        call make_valid_buf(.true., asize*nrows, valid_buf, valid_ptr)
        call parquet_read_date_array_column(reader%handle, trim(name)//char(0), flat, nrows, asize, valid_ptr)
        do i = 1_int64, nrows
            do j = 1_int64, asize
                k = (i-1)*asize + j
                if (valid_buf(k) /= 0_c_int8_t) then
                    call values(j, i)%set_raw(flat(k))
                else
                    call values(j, i)%set_null()
                end if
            end do
        end do
    end procedure parquet_read_date_array_full
    module procedure parquet_read_time_column_1d
        integer(c_int64_t), allocatable :: ns(:)
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        integer(int64) :: i, n

        call check_reader_open(reader, "parquet_read_column")
        call check_column_exists(reader, name, "parquet_read_column")
        n = size(values, kind=int64)
        call parquet_check_read_row_count(reader, name, n)
        allocate(ns(n))
        call make_valid_buf(.true., n, valid_buf, valid_ptr)
        call parquet_read_time_column(reader%handle, trim(name)//char(0), ns, n, valid_ptr)
        do i = 1_int64, n
            if (valid_buf(i) /= 0_c_int8_t) then
                call values(i)%set_raw(ns(i))
            else
                call values(i)%set_null()
            end if
        end do
    end procedure parquet_read_time_column_1d
    module procedure parquet_read_time_array_full
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
        call make_valid_buf(.true., asize*nrows, valid_buf, valid_ptr)
        call parquet_read_time_array_column(reader%handle, trim(name)//char(0), flat, nrows, asize, valid_ptr)
        do i = 1_int64, nrows
            do j = 1_int64, asize
                k = (i-1)*asize + j
                if (valid_buf(k) /= 0_c_int8_t) then
                    call values(j, i)%set_raw(flat(k))
                else
                    call values(j, i)%set_null()
                end if
            end do
        end do
    end procedure parquet_read_time_array_full
    module procedure parquet_read_timestamp_column_1d
        integer(c_int64_t), allocatable :: vals(:)
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        integer(c_int32_t) :: unit_out
        integer(int64) :: i, n

        call check_reader_open(reader, "parquet_read_column")
        call check_column_exists(reader, name, "parquet_read_column")
        n = size(values, kind=int64)
        call parquet_check_read_row_count(reader, name, n)
        allocate(vals(n))
        call make_valid_buf(.true., n, valid_buf, valid_ptr)
        call parquet_read_timestamp_column(reader%handle, trim(name)//char(0), vals, n, unit_out, valid_ptr)
        do i = 1_int64, n
            if (valid_buf(i) /= 0_c_int8_t) then
                call values(i)%set_unix(vals(i), int(unit_out))
            else
                call values(i)%set_null()
            end if
        end do
    end procedure parquet_read_timestamp_column_1d
    module procedure parquet_read_timestamp_array_full
        integer(c_int64_t), allocatable :: flat(:)
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        integer(c_int32_t) :: unit_out
        integer(int64) :: asize, nrows, i, j, k

        asize = size(values, 1, kind=int64)
        nrows = size(values, 2, kind=int64)
        call check_reader_open(reader, "parquet_read_column")
        call check_column_exists(reader, name, "parquet_read_column")
        call parquet_check_read_row_count(reader, name, nrows)
        allocate(flat(asize*nrows))
        call make_valid_buf(.true., asize*nrows, valid_buf, valid_ptr)
        call parquet_read_timestamp_array_column(reader%handle, trim(name)//char(0), flat, nrows, asize, &
            unit_out, valid_ptr)
        do i = 1_int64, nrows
            do j = 1_int64, asize
                k = (i-1)*asize + j
                if (valid_buf(k) /= 0_c_int8_t) then
                    call values(j, i)%set_unix(flat(k), int(unit_out))
                else
                    call values(j, i)%set_null()
                end if
            end do
        end do
    end procedure parquet_read_timestamp_array_full
    ! ---- Temporal streaming (row-group-chunked) reads. Each type has a scalar and a vector
    !      _impl (row_group as int64) shared by its int32/int64 row_group rg32/rg64 specifics.
    subroutine read_date_column_chunk_impl(reader, name, row_group, values)
        type(parquet_reader), intent(in) :: reader !! open reader.
        character(len=*), intent(in) :: name !! column name.
        integer(int64), intent(in) :: row_group !! 1-based row group.
        type(parquet_date), intent(out) :: values(:) !! that row group's dates.
        integer(c_int32_t), allocatable :: days(:)
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        integer(int64) :: i, n

        call check_reader_open(reader, "parquet_read_column_chunk")
        call check_column_exists(reader, name, "parquet_read_column_chunk")
        call check_reader_no_filter(reader, "parquet_read_column_chunk")
        call check_row_group_valid(reader, row_group, "parquet_read_column_chunk")
        n = size(values, kind=int64)
        allocate(days(n))
        call make_valid_buf(.true., n, valid_buf, valid_ptr)
        call parquet_read_date_column_chunk(reader%handle, trim(name)//char(0), row_group, days, n, valid_ptr)
        do i = 1_int64, n
            if (valid_buf(i) /= 0_c_int8_t) then
                call values(i)%set_raw(days(i))
            else
                call values(i)%set_null()
            end if
        end do
    end subroutine read_date_column_chunk_impl
    subroutine read_date_array_column_chunk_impl(reader, name, row_group, values)
        type(parquet_reader), intent(in) :: reader !! open reader.
        character(len=*), intent(in) :: name !! vector column name.
        integer(int64), intent(in) :: row_group !! 1-based row group.
        type(parquet_date), intent(out) :: values(:,:) !! (element, row) dates.
        integer(c_int32_t), allocatable :: flat(:)
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        integer(int64) :: asize, nrows, i, j, k

        asize = size(values, 1, kind=int64)
        nrows = size(values, 2, kind=int64)
        call check_reader_open(reader, "parquet_read_column_chunk")
        call check_column_exists(reader, name, "parquet_read_column_chunk")
        call check_reader_no_filter(reader, "parquet_read_column_chunk")
        call check_row_group_valid(reader, row_group, "parquet_read_column_chunk")
        allocate(flat(asize*nrows))
        call make_valid_buf(.true., asize*nrows, valid_buf, valid_ptr)
        call parquet_read_date_array_column_chunk(reader%handle, trim(name)//char(0), row_group, flat, nrows, asize, valid_ptr)
        do i = 1_int64, nrows
            do j = 1_int64, asize
                k = (i-1)*asize + j
                if (valid_buf(k) /= 0_c_int8_t) then
                    call values(j, i)%set_raw(flat(k))
                else
                    call values(j, i)%set_null()
                end if
            end do
        end do
    end subroutine read_date_array_column_chunk_impl
    subroutine read_time_column_chunk_impl(reader, name, row_group, values)
        type(parquet_reader), intent(in) :: reader !! open reader.
        character(len=*), intent(in) :: name !! column name.
        integer(int64), intent(in) :: row_group !! 1-based row group.
        type(parquet_time), intent(out) :: values(:) !! that row group's times.
        integer(c_int64_t), allocatable :: ns(:)
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        integer(int64) :: i, n

        call check_reader_open(reader, "parquet_read_column_chunk")
        call check_column_exists(reader, name, "parquet_read_column_chunk")
        call check_reader_no_filter(reader, "parquet_read_column_chunk")
        call check_row_group_valid(reader, row_group, "parquet_read_column_chunk")
        n = size(values, kind=int64)
        allocate(ns(n))
        call make_valid_buf(.true., n, valid_buf, valid_ptr)
        call parquet_read_time_column_chunk(reader%handle, trim(name)//char(0), row_group, ns, n, valid_ptr)
        do i = 1_int64, n
            if (valid_buf(i) /= 0_c_int8_t) then
                call values(i)%set_raw(ns(i))
            else
                call values(i)%set_null()
            end if
        end do
    end subroutine read_time_column_chunk_impl
    subroutine read_time_array_column_chunk_impl(reader, name, row_group, values)
        type(parquet_reader), intent(in) :: reader !! open reader.
        character(len=*), intent(in) :: name !! vector column name.
        integer(int64), intent(in) :: row_group !! 1-based row group.
        type(parquet_time), intent(out) :: values(:,:) !! (element, row) times.
        integer(c_int64_t), allocatable :: flat(:)
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        integer(int64) :: asize, nrows, i, j, k

        asize = size(values, 1, kind=int64)
        nrows = size(values, 2, kind=int64)
        call check_reader_open(reader, "parquet_read_column_chunk")
        call check_column_exists(reader, name, "parquet_read_column_chunk")
        call check_reader_no_filter(reader, "parquet_read_column_chunk")
        call check_row_group_valid(reader, row_group, "parquet_read_column_chunk")
        allocate(flat(asize*nrows))
        call make_valid_buf(.true., asize*nrows, valid_buf, valid_ptr)
        call parquet_read_time_array_column_chunk(reader%handle, trim(name)//char(0), row_group, flat, nrows, asize, valid_ptr)
        do i = 1_int64, nrows
            do j = 1_int64, asize
                k = (i-1)*asize + j
                if (valid_buf(k) /= 0_c_int8_t) then
                    call values(j, i)%set_raw(flat(k))
                else
                    call values(j, i)%set_null()
                end if
            end do
        end do
    end subroutine read_time_array_column_chunk_impl
    subroutine read_timestamp_column_chunk_impl(reader, name, row_group, values)
        type(parquet_reader), intent(in) :: reader !! open reader.
        character(len=*), intent(in) :: name !! column name.
        integer(int64), intent(in) :: row_group !! 1-based row group.
        type(parquet_timestamp), intent(out) :: values(:) !! that row group's instants.
        integer(c_int64_t), allocatable :: vals(:)
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        integer(c_int32_t) :: unit_out
        integer(int64) :: i, n

        call check_reader_open(reader, "parquet_read_column_chunk")
        call check_column_exists(reader, name, "parquet_read_column_chunk")
        call check_reader_no_filter(reader, "parquet_read_column_chunk")
        call check_row_group_valid(reader, row_group, "parquet_read_column_chunk")
        n = size(values, kind=int64)
        allocate(vals(n))
        call make_valid_buf(.true., n, valid_buf, valid_ptr)
        call parquet_read_timestamp_column_chunk(reader%handle, trim(name)//char(0), row_group, vals, n, unit_out, valid_ptr)
        do i = 1_int64, n
            if (valid_buf(i) /= 0_c_int8_t) then
                call values(i)%set_unix(vals(i), int(unit_out))
            else
                call values(i)%set_null()
            end if
        end do
    end subroutine read_timestamp_column_chunk_impl
    subroutine read_timestamp_array_column_chunk_impl(reader, name, row_group, values)
        type(parquet_reader), intent(in) :: reader !! open reader.
        character(len=*), intent(in) :: name !! vector column name.
        integer(int64), intent(in) :: row_group !! 1-based row group.
        type(parquet_timestamp), intent(out) :: values(:,:) !! (element, row) instants.
        integer(c_int64_t), allocatable :: flat(:)
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        integer(c_int32_t) :: unit_out
        integer(int64) :: asize, nrows, i, j, k

        asize = size(values, 1, kind=int64)
        nrows = size(values, 2, kind=int64)
        call check_reader_open(reader, "parquet_read_column_chunk")
        call check_column_exists(reader, name, "parquet_read_column_chunk")
        call check_reader_no_filter(reader, "parquet_read_column_chunk")
        call check_row_group_valid(reader, row_group, "parquet_read_column_chunk")
        allocate(flat(asize*nrows))
        call make_valid_buf(.true., asize*nrows, valid_buf, valid_ptr)
        call parquet_read_timestamp_array_column_chunk(reader%handle, trim(name)//char(0), row_group, flat, nrows, asize, &
            unit_out, valid_ptr)
        do i = 1_int64, nrows
            do j = 1_int64, asize
                k = (i-1)*asize + j
                if (valid_buf(k) /= 0_c_int8_t) then
                    call values(j, i)%set_unix(flat(k), int(unit_out))
                else
                    call values(j, i)%set_null()
                end if
            end do
        end do
    end subroutine read_timestamp_array_column_chunk_impl
    module procedure parquet_read_date_column_chunk_rg32
        call read_date_column_chunk_impl(reader, name, int(row_group, kind=int64), values)
    end procedure parquet_read_date_column_chunk_rg32
    module procedure parquet_read_date_column_chunk_rg64
        call read_date_column_chunk_impl(reader, name, row_group, values)
    end procedure parquet_read_date_column_chunk_rg64
    module procedure parquet_read_date_array_column_chunk_rg32
        call read_date_array_column_chunk_impl(reader, name, int(row_group, kind=int64), values)
    end procedure parquet_read_date_array_column_chunk_rg32
    module procedure parquet_read_date_array_column_chunk_rg64
        call read_date_array_column_chunk_impl(reader, name, row_group, values)
    end procedure parquet_read_date_array_column_chunk_rg64
    module procedure parquet_read_time_column_chunk_rg32
        call read_time_column_chunk_impl(reader, name, int(row_group, kind=int64), values)
    end procedure parquet_read_time_column_chunk_rg32
    module procedure parquet_read_time_column_chunk_rg64
        call read_time_column_chunk_impl(reader, name, row_group, values)
    end procedure parquet_read_time_column_chunk_rg64
    module procedure parquet_read_time_array_column_chunk_rg32
        call read_time_array_column_chunk_impl(reader, name, int(row_group, kind=int64), values)
    end procedure parquet_read_time_array_column_chunk_rg32
    module procedure parquet_read_time_array_column_chunk_rg64
        call read_time_array_column_chunk_impl(reader, name, row_group, values)
    end procedure parquet_read_time_array_column_chunk_rg64
    module procedure parquet_read_timestamp_column_chunk_rg32
        call read_timestamp_column_chunk_impl(reader, name, int(row_group, kind=int64), values)
    end procedure parquet_read_timestamp_column_chunk_rg32
    module procedure parquet_read_timestamp_column_chunk_rg64
        call read_timestamp_column_chunk_impl(reader, name, row_group, values)
    end procedure parquet_read_timestamp_column_chunk_rg64
    module procedure parquet_read_timestamp_array_column_chunk_rg32
        call read_timestamp_array_column_chunk_impl(reader, name, int(row_group, kind=int64), values)
    end procedure parquet_read_timestamp_array_column_chunk_rg32
    module procedure parquet_read_timestamp_array_column_chunk_rg64
        call read_timestamp_array_column_chunk_impl(reader, name, row_group, values)
    end procedure parquet_read_timestamp_array_column_chunk_rg64
    ! ---- Temporal row-mode reads (one row's element vector). Each type has one _impl (int64
    !      row_index) shared by its int32/int64 row_index specifics. Nulls fill their elements.
    subroutine read_date_array_row_mode_impl(reader, name, values, row_index)
        type(parquet_reader), intent(in) :: reader !! open reader.
        character(len=*), intent(in) :: name !! vector column name.
        type(parquet_date), intent(out) :: values(:) !! that row's element vector.
        integer(int64), intent(in) :: row_index !! 1-based row.
        integer(c_int32_t), allocatable :: buf(:)
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        integer(int64) :: i, n

        call check_reader_open(reader, "parquet_read_array_row_mode")
        call check_column_exists(reader, name, "parquet_read_array_row_mode")
        n = size(values, kind=int64)
        allocate(buf(n))
        call make_valid_buf(.true., n, valid_buf, valid_ptr)
        call parquet_read_date_array_row(reader%handle, trim(name)//char(0), int(row_index, c_long_long), buf, n, valid_ptr)
        do i = 1_int64, n
            if (valid_buf(i) /= 0_c_int8_t) then
                call values(i)%set_raw(buf(i))
            else
                call values(i)%set_null()
            end if
        end do
    end subroutine read_date_array_row_mode_impl
    subroutine read_time_array_row_mode_impl(reader, name, values, row_index)
        type(parquet_reader), intent(in) :: reader !! open reader.
        character(len=*), intent(in) :: name !! vector column name.
        type(parquet_time), intent(out) :: values(:) !! that row's element vector.
        integer(int64), intent(in) :: row_index !! 1-based row.
        integer(c_int64_t), allocatable :: buf(:)
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        integer(int64) :: i, n

        call check_reader_open(reader, "parquet_read_array_row_mode")
        call check_column_exists(reader, name, "parquet_read_array_row_mode")
        n = size(values, kind=int64)
        allocate(buf(n))
        call make_valid_buf(.true., n, valid_buf, valid_ptr)
        call parquet_read_time_array_row(reader%handle, trim(name)//char(0), int(row_index, c_long_long), buf, n, valid_ptr)
        do i = 1_int64, n
            if (valid_buf(i) /= 0_c_int8_t) then
                call values(i)%set_raw(buf(i))
            else
                call values(i)%set_null()
            end if
        end do
    end subroutine read_time_array_row_mode_impl
    subroutine read_timestamp_array_row_mode_impl(reader, name, values, row_index)
        type(parquet_reader), intent(in) :: reader !! open reader.
        character(len=*), intent(in) :: name !! vector column name.
        type(parquet_timestamp), intent(out) :: values(:) !! that row's element vector.
        integer(int64), intent(in) :: row_index !! 1-based row.
        integer(c_int64_t), allocatable :: buf(:)
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        integer(c_int32_t) :: unit_out
        integer(int64) :: i, n

        call check_reader_open(reader, "parquet_read_array_row_mode")
        call check_column_exists(reader, name, "parquet_read_array_row_mode")
        n = size(values, kind=int64)
        allocate(buf(n))
        call make_valid_buf(.true., n, valid_buf, valid_ptr)
        call parquet_read_timestamp_array_row(reader%handle, trim(name)//char(0), int(row_index, c_long_long), buf, n, &
            unit_out, valid_ptr)
        do i = 1_int64, n
            if (valid_buf(i) /= 0_c_int8_t) then
                call values(i)%set_unix(buf(i), int(unit_out))
            else
                call values(i)%set_null()
            end if
        end do
    end subroutine read_timestamp_array_row_mode_impl
    module procedure parquet_read_date_array_row_mode
        call read_date_array_row_mode_impl(reader, name, values, int(row_index, kind=int64))
    end procedure parquet_read_date_array_row_mode
    module procedure parquet_read_date_array_row_mode_row_index_int64
        call read_date_array_row_mode_impl(reader, name, values, row_index)
    end procedure parquet_read_date_array_row_mode_row_index_int64
    module procedure parquet_read_time_array_row_mode
        call read_time_array_row_mode_impl(reader, name, values, int(row_index, kind=int64))
    end procedure parquet_read_time_array_row_mode
    module procedure parquet_read_time_array_row_mode_row_index_int64
        call read_time_array_row_mode_impl(reader, name, values, row_index)
    end procedure parquet_read_time_array_row_mode_row_index_int64
    module procedure parquet_read_timestamp_array_row_mode
        call read_timestamp_array_row_mode_impl(reader, name, values, int(row_index, kind=int64))
    end procedure parquet_read_timestamp_array_row_mode
    module procedure parquet_read_timestamp_array_row_mode_row_index_int64
        call read_timestamp_array_row_mode_impl(reader, name, values, row_index)
    end procedure parquet_read_timestamp_array_row_mode_row_index_int64
    ! ---- Temporal element-mode reads (one element position across all rows). Nulls fill their
    !      elements. `col_size` is passed as 0 to C (ignored; element mode resolves it there).
    module procedure parquet_read_date_array_element_mode
        integer(c_int32_t), allocatable :: buf(:)
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        integer(int64) :: i, n

        call check_reader_open(reader, "parquet_read_array_element_mode")
        call check_column_exists(reader, name, "parquet_read_array_element_mode")
        n = size(values, kind=int64)
        allocate(buf(n))
        call make_valid_buf(.true., n, valid_buf, valid_ptr)
        call parquet_read_date_array_element(reader%handle, trim(name)//char(0), int(elem_index, c_long_long), buf, n, &
            0_c_long_long, valid_ptr)
        do i = 1_int64, n
            if (valid_buf(i) /= 0_c_int8_t) then
                call values(i)%set_raw(buf(i))
            else
                call values(i)%set_null()
            end if
        end do
    end procedure parquet_read_date_array_element_mode
    module procedure parquet_read_time_array_element_mode
        integer(c_int64_t), allocatable :: buf(:)
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        integer(int64) :: i, n

        call check_reader_open(reader, "parquet_read_array_element_mode")
        call check_column_exists(reader, name, "parquet_read_array_element_mode")
        n = size(values, kind=int64)
        allocate(buf(n))
        call make_valid_buf(.true., n, valid_buf, valid_ptr)
        call parquet_read_time_array_element(reader%handle, trim(name)//char(0), int(elem_index, c_long_long), buf, n, &
            0_c_long_long, valid_ptr)
        do i = 1_int64, n
            if (valid_buf(i) /= 0_c_int8_t) then
                call values(i)%set_raw(buf(i))
            else
                call values(i)%set_null()
            end if
        end do
    end procedure parquet_read_time_array_element_mode
    module procedure parquet_read_timestamp_array_element_mode
        integer(c_int64_t), allocatable :: buf(:)
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        integer(c_int32_t) :: unit_out
        integer(int64) :: i, n

        call check_reader_open(reader, "parquet_read_array_element_mode")
        call check_column_exists(reader, name, "parquet_read_array_element_mode")
        n = size(values, kind=int64)
        allocate(buf(n))
        call make_valid_buf(.true., n, valid_buf, valid_ptr)
        call parquet_read_timestamp_array_element(reader%handle, trim(name)//char(0), int(elem_index, c_long_long), buf, n, &
            0_c_long_long, unit_out, valid_ptr)
        do i = 1_int64, n
            if (valid_buf(i) /= 0_c_int8_t) then
                call values(i)%set_unix(buf(i), int(unit_out))
            else
                call values(i)%set_null()
            end if
        end do
    end procedure parquet_read_timestamp_array_element_mode

end submodule parquet_read_temporal
