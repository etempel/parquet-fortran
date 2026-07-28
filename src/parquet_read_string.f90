!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!> String read specifics (column_1d, array_full, the fixed-width "compact"
!> column form, array_row_mode[_row_index], array_element_mode, and
!> column_chunk/array_column_chunk incl. the compact chunked form): bodies
!> of the module procedures declared in parquet.f90's interface block, plus
!> the string-only private per-mode worker helpers they depend on.
submodule (parquet:parquet_read) parquet_read_string
    implicit none
contains

    module procedure parquet_read_string_column_1d
        character(kind=c_char), allocatable :: packed(:)
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        integer(int64) :: nrows, i, k
        integer :: item_len, j

        nrows = size(values, kind=int64)
        call check_reader_open(reader, "parquet_read_column")
        call check_column_exists(reader, name, "parquet_read_column")
        call parquet_check_read_row_count(reader, name, nrows)
        item_len = len(values(1))
        allocate(packed(item_len*nrows))
        call make_valid_buf(present(null_value) .or. present(is_valid), nrows, valid_buf, valid_ptr)
        call parquet_read_string_column(reader%handle, trim(name)//char(0), packed, int(item_len, kind=c_long_long), &
            nrows, valid_ptr)

        k = 0_int64
        do i = 1_int64, nrows
            values(i) = ''
            do j = 1, item_len
                k = k + 1_int64
                values(i)(j:j) = achar(iachar(packed(k)))
            end do
        end do
        if (present(is_valid)) is_valid = valid_buf /= 0_c_int8_t
        if (present(null_value)) then
            do i = 1_int64, nrows
                if (valid_buf(i) == 0_c_int8_t) values(i) = null_value
            end do
        end if
    end procedure parquet_read_string_column_1d
    module procedure parquet_read_string_array_full
        character(kind=c_char), allocatable :: packed(:)
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        integer(int64) :: asize, nrows, i, j, k, p
        integer :: item_len, m

        asize = size(values, 1, kind=int64)
        nrows = size(values, 2, kind=int64)
        call check_reader_open(reader, "parquet_read_column")
        call check_column_exists(reader, name, "parquet_read_column")
        call parquet_check_read_row_count(reader, name, nrows)
        item_len = len(values(1,1))
        allocate(packed(item_len*asize*nrows))
        call make_valid_buf(present(null_value) .or. present(is_valid), asize*nrows, valid_buf, valid_ptr)
        call parquet_read_string_array_column(reader%handle, trim(name)//char(0), packed, int(item_len, kind=c_long_long), &
            nrows, asize, valid_ptr)

        p = 0_int64
        do i = 1_int64, nrows
            do j = 1_int64, asize
                values(j, i) = ''
                do m = 1, item_len
                    p = p + 1_int64
                    values(j, i)(m:m) = achar(iachar(packed(p)))
                end do
                k = (i-1)*asize + j
                if (present(is_valid)) is_valid(j, i) = valid_buf(k) /= 0_c_int8_t
                if (present(null_value)) then
                    if (valid_buf(k) == 0_c_int8_t) values(j, i) = null_value
                end if
            end do
        end do
    end procedure parquet_read_string_array_full
    !> Compact (parquet_string_column) specific of parquet_read_column: clears `values` then
    !> bulk-appends the whole column straight from its own decoded offsets/data/validity buffers
    !> (parquet_read_string_column_buffers), instead of copying one string at a time into a
    !> fixed-width padded array. Every Null lands as values%append_null() -- there is no
    !> null_value/is_valid to plumb through here, unlike every other parquet_read_column specific.
    module procedure parquet_read_string_column_compact
        integer(int64) :: nrows, nchars, validity_offset
        type(c_ptr) :: offsets_ptr, data_ptr, validity_ptr
        integer(c_int8_t) :: offsets_int32_flag

        call check_reader_open(reader, "parquet_read_column")
        call check_column_exists(reader, name, "parquet_read_column")
        call values%clear()
        call parquet_read_string_column_buffers(reader%handle, trim(name)//char(0), nrows, nchars, &
            offsets_ptr, data_ptr, validity_ptr, offsets_int32_flag, validity_offset)
        call values%append_buffers(nrows, nchars, offsets_ptr, data_ptr, validity_ptr, &
            offsets_int32_flag /= 0_c_int8_t, validity_offset)
    end procedure parquet_read_string_column_compact
    !> Shared body of parquet_read_string_array_row_mode/_row_index_int64.
    subroutine parquet_read_string_array_row_mode_impl(reader, name, values, row_index, null_value, is_valid)
        type(parquet_reader), intent(in) :: reader !! open reader.
        character(len=*), intent(in) :: name !! vector column name.
        character(len=*), intent(out) :: values(:) !! that row's element vector.
        integer(int64), intent(in) :: row_index !! 1-based row.
        character(len=*), intent(in), optional :: null_value !! fill value for missing entries.
        logical, intent(out), optional :: is_valid(:) !! per-element validity mask.
        character(kind=c_char), allocatable :: packed(:)
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        integer(int64) :: i, p
        integer :: item_len, j

        call check_reader_open(reader, "parquet_read_array_row_mode")
        call check_column_exists(reader, name, "parquet_read_array_row_mode")
        item_len = len(values(1))
        allocate(packed(item_len*size(values, kind=int64)))
        call make_valid_buf(present(null_value) .or. present(is_valid), size(values, kind=int64), valid_buf, valid_ptr)
        call parquet_read_string_array_row(reader%handle, trim(name)//char(0), int(row_index, kind=c_long_long), packed, &
            int(item_len, kind=c_long_long), size(values, kind=c_long_long), valid_ptr)
        p = 0_int64
        do i = 1_int64, size(values, kind=int64)
            values(i) = ''
            do j = 1, item_len
                p = p + 1_int64
                values(i)(j:j) = achar(iachar(packed(p)))
            end do
        end do
        if (present(is_valid)) is_valid = valid_buf /= 0_c_int8_t
        if (present(null_value)) then
            do i = 1_int64, size(values, kind=int64)
                if (valid_buf(i) == 0_c_int8_t) values(i) = null_value
            end do
        end if
    end subroutine parquet_read_string_array_row_mode_impl
    module procedure parquet_read_string_array_row_mode
        call parquet_read_string_array_row_mode_impl(reader, name, values, int(row_index, kind=int64), null_value, &
            is_valid)
    end procedure parquet_read_string_array_row_mode
    module procedure parquet_read_string_array_row_mode_row_index_int64
        call parquet_read_string_array_row_mode_impl(reader, name, values, row_index, null_value, is_valid)
    end procedure parquet_read_string_array_row_mode_row_index_int64
    module procedure parquet_read_string_array_element_mode
        character(kind=c_char), allocatable :: packed(:)
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        integer(int64) :: i, p
        integer :: item_len, j

        call check_reader_open(reader, "parquet_read_array_element_mode")
        call check_column_exists(reader, name, "parquet_read_array_element_mode")
        item_len = len(values(1))
        allocate(packed(item_len*size(values, kind=int64)))
        call make_valid_buf(present(null_value) .or. present(is_valid), size(values, kind=int64), valid_buf, valid_ptr)
        call parquet_read_string_array_element(reader%handle, trim(name)//char(0), int(elem_index, kind=c_long_long), packed, &
            int(item_len, kind=c_long_long), size(values, kind=c_long_long), 0_c_long_long, valid_ptr)
        p = 0_int64
        do i = 1_int64, size(values, kind=int64)
            values(i) = ''
            do j = 1, item_len
                p = p + 1_int64
                values(i)(j:j) = achar(iachar(packed(p)))
            end do
        end do
        if (present(is_valid)) is_valid = valid_buf /= 0_c_int8_t
        if (present(null_value)) then
            do i = 1_int64, size(values, kind=int64)
                if (valid_buf(i) == 0_c_int8_t) values(i) = null_value
            end do
        end if
    end procedure parquet_read_string_array_element_mode
    !> Shared body of parquet_read_string_column_chunk_rg32/_rg64 -- see the generic
    !> interface's own doc comment in parquet.f90 for why row_group has two kind-specifics.
    subroutine parquet_read_string_column_chunk_impl(reader, name, row_group, values, null_value, is_valid)
        type(parquet_reader), intent(in) :: reader !! open reader.
        character(len=*), intent(in) :: name !! column name.
        integer(int64), intent(in) :: row_group !! 1-based row group.
        character(len=*), intent(out) :: values(:) !! one value per row of the selected row group.
        character(len=*), intent(in), optional :: null_value !! fill value for missing entries.
        logical, intent(out), optional :: is_valid(:) !! per-row validity mask.
        character(kind=c_char), allocatable :: packed(:)
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        integer(int64) :: nrows, i, k
        integer :: item_len, j

        nrows = size(values, kind=int64)
        call check_reader_open(reader, "parquet_read_column_chunk")
        call check_column_exists(reader, name, "parquet_read_column_chunk")
        call check_reader_no_filter(reader, "parquet_read_column_chunk")
        call check_row_group_valid(reader, row_group, "parquet_read_column_chunk")
        item_len = len(values(1))
        allocate(packed(item_len*nrows))
        call make_valid_buf(present(null_value) .or. present(is_valid), nrows, valid_buf, valid_ptr)
        call parquet_read_string_column_chunk(reader%handle, trim(name)//char(0), row_group, packed, &
            int(item_len, kind=c_long_long), nrows, valid_ptr)

        k = 0_int64
        do i = 1_int64, nrows
            values(i) = ''
            do j = 1, item_len
                k = k + 1_int64
                values(i)(j:j) = achar(iachar(packed(k)))
            end do
        end do
        if (present(is_valid)) is_valid = valid_buf /= 0_c_int8_t
        if (present(null_value)) then
            do i = 1_int64, nrows
                if (valid_buf(i) == 0_c_int8_t) values(i) = null_value
            end do
        end if
    end subroutine parquet_read_string_column_chunk_impl
    !> Shared body of parquet_read_string_array_column_chunk_rg32/_rg64 -- see the generic
    !> interface's own doc comment in parquet.f90 for why row_group has two kind-specifics.
    subroutine parquet_read_string_array_column_chunk_impl(reader, name, row_group, values, null_value, is_valid)
        type(parquet_reader), intent(in) :: reader !! open reader.
        character(len=*), intent(in) :: name !! vector column name.
        integer(int64), intent(in) :: row_group !! 1-based row group.
        character(len=*), intent(out) :: values(:, :) !! (element, row) values of the selected row group.
        character(len=*), intent(in), optional :: null_value !! fill value for missing entries.
        logical, intent(out), optional :: is_valid(:, :) !! per-element validity mask.
        character(kind=c_char), allocatable :: packed(:)
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        integer(int64) :: asize, nrows, i, j, k, p
        integer :: item_len, m

        asize = size(values, 1, kind=int64)
        nrows = size(values, 2, kind=int64)
        call check_reader_open(reader, "parquet_read_column_chunk")
        call check_column_exists(reader, name, "parquet_read_column_chunk")
        call check_reader_no_filter(reader, "parquet_read_column_chunk")
        call check_row_group_valid(reader, row_group, "parquet_read_column_chunk")
        item_len = len(values(1,1))
        allocate(packed(item_len*asize*nrows))
        call make_valid_buf(present(null_value) .or. present(is_valid), asize*nrows, valid_buf, valid_ptr)
        call parquet_read_string_array_column_chunk(reader%handle, trim(name)//char(0), row_group, packed, &
            int(item_len, kind=c_long_long), nrows, asize, valid_ptr)

        p = 0_int64
        do i = 1_int64, nrows
            do j = 1_int64, asize
                values(j, i) = ''
                do m = 1, item_len
                    p = p + 1_int64
                    values(j, i)(m:m) = achar(iachar(packed(p)))
                end do
                k = (i-1)*asize + j
                if (present(is_valid)) is_valid(j, i) = valid_buf(k) /= 0_c_int8_t
                if (present(null_value)) then
                    if (valid_buf(k) == 0_c_int8_t) values(j, i) = null_value
                end if
            end do
        end do
    end subroutine parquet_read_string_array_column_chunk_impl
    !> Shared body of parquet_read_string_column_chunk_compact_rg32/_rg64 -- see the generic
    !> interface's own doc comment in parquet.f90 for why row_group has two kind-specifics, and
    !> parquet_read_string_column_compact's own doc comment for the parquet_string_column notes
    !> shared with this chunked counterpart.
    subroutine parquet_read_string_column_chunk_compact_impl(reader, name, row_group, values)
        type(parquet_reader), intent(in) :: reader !! open reader.
        character(len=*), intent(in) :: name !! column name.
        integer(int64), intent(in) :: row_group !! 1-based row group.
        type(parquet_string_column), intent(inout) :: values !! cleared, then filled with this row group's rows.
        integer(int64) :: nrows, nchars, validity_offset
        type(c_ptr) :: offsets_ptr, data_ptr, validity_ptr
        integer(c_int8_t) :: offsets_int32_flag

        call check_reader_open(reader, "parquet_read_column_chunk")
        call check_column_exists(reader, name, "parquet_read_column_chunk")
        call check_reader_no_filter(reader, "parquet_read_column_chunk")
        call check_row_group_valid(reader, row_group, "parquet_read_column_chunk")
        call values%clear()
        call parquet_read_string_column_chunk_buffers(reader%handle, trim(name)//char(0), row_group, nrows, nchars, &
            offsets_ptr, data_ptr, validity_ptr, offsets_int32_flag, validity_offset)
        call values%append_buffers(nrows, nchars, offsets_ptr, data_ptr, validity_ptr, &
            offsets_int32_flag /= 0_c_int8_t, validity_offset)
    end subroutine parquet_read_string_column_chunk_compact_impl
    module procedure parquet_read_string_column_chunk_compact_rg32
        call parquet_read_string_column_chunk_compact_impl(reader, name, int(row_group, kind=int64), values)
    end procedure parquet_read_string_column_chunk_compact_rg32
    module procedure parquet_read_string_column_chunk_compact_rg64
        call parquet_read_string_column_chunk_compact_impl(reader, name, row_group, values)
    end procedure parquet_read_string_column_chunk_compact_rg64
    module procedure parquet_read_string_column_chunk_rg32
        call parquet_read_string_column_chunk_impl(reader, name, int(row_group, kind=int64), values, null_value, &
            is_valid)
    end procedure parquet_read_string_column_chunk_rg32
    module procedure parquet_read_string_column_chunk_rg64
        call parquet_read_string_column_chunk_impl(reader, name, row_group, values, null_value, is_valid)
    end procedure parquet_read_string_column_chunk_rg64
    module procedure parquet_read_string_array_column_chunk_rg32
        call parquet_read_string_array_column_chunk_impl(reader, name, int(row_group, kind=int64), values, &
            null_value, is_valid)
    end procedure parquet_read_string_array_column_chunk_rg32
    module procedure parquet_read_string_array_column_chunk_rg64
        call parquet_read_string_array_column_chunk_impl(reader, name, row_group, values, null_value, is_valid)
    end procedure parquet_read_string_array_column_chunk_rg64

end submodule parquet_read_string
