!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
submodule (parquet) parquet_read
contains

	!> Allocates `valid_buf(n)` and points `valid_ptr` at it via c_loc only when
	!> the caller actually asked for null-tolerant reading (null_value and/or
	!> is_valid present); otherwise valid_ptr stays c_null_ptr so the C++ side
	!> keeps its default strict (error-on-null) behavior.
	subroutine make_valid_buf(want_report, n, valid_buf, valid_ptr)
		logical, intent(in) :: want_report
		integer, intent(in) :: n
		integer(c_int8_t), allocatable, target, intent(out) :: valid_buf(:)
		type(c_ptr), intent(out) :: valid_ptr

		if (want_report) then
			allocate(valid_buf(n))
			valid_ptr = c_loc(valid_buf)
		else
			valid_ptr = c_null_ptr
		end if
	end subroutine make_valid_buf

	module procedure parquet_open_reader
		reader%handle = create_parquet_reader(trim(filename)//char(0))
	end procedure parquet_open_reader

	module procedure parquet_close_reader
		if (c_associated(reader%handle)) then
			call close_parquet_reader(reader%handle)
			reader%handle = c_null_ptr
		end if
	end procedure parquet_close_reader

	!> Safety net for a reader whose handle is still open when it goes out of
	!> scope or is overwritten -- frees the underlying C++ object so the
	!> process doesn't leak it. Always prefer calling parquet_close_reader
	!> explicitly.
	module procedure reader_finalize
		if (c_associated(this%handle)) then
			call close_parquet_reader(this%handle)
			this%handle = c_null_ptr
		end if
	end procedure reader_finalize

	module procedure parquet_get_nrows_int64
		nrows = int(parquet_reader_get_nrows(reader%handle), kind=int64)
	end procedure parquet_get_nrows_int64

	module procedure parquet_get_nrows_int32
		integer(int64) :: nrows64
		nrows64 = int(parquet_reader_get_nrows(reader%handle), kind=int64)
		if (nrows64 > huge(0_int32)) then
			error stop "Number of rows exceeds int32 range"
		end if
		nrows = int(nrows64, kind=int32)
	end procedure parquet_get_nrows_int32

	module procedure parquet_get_col_size
		col_size = int(parquet_reader_get_column_array_size(reader%handle, trim(name)//char(0)))
	end procedure parquet_get_col_size

	module procedure parquet_get_column_total_elements_int64
		total_elements = int(parquet_reader_get_column_total_elements(reader%handle, trim(name)//char(0)), kind=int64)
	end procedure parquet_get_column_total_elements_int64

	module procedure parquet_get_column_total_elements_int32
		integer(int64) :: nelem64
		nelem64 = int(parquet_reader_get_column_total_elements(reader%handle, trim(name)//char(0)), kind=int64)
		if (nelem64 > huge(0_int32)) then
			error stop "Number of elements exceeds int32 range"
		end if
		total_elements = int(nelem64, kind=int32)
	end procedure parquet_get_column_total_elements_int32

	module procedure parquet_get_string_length
		max_string_length = int(parquet_reader_get_string_length(reader%handle, trim(name)//char(0)))
	end procedure parquet_get_string_length

	module procedure parquet_read_int32_column_1d
		integer(c_int8_t), allocatable, target :: valid_buf(:)
		type(c_ptr) :: valid_ptr
		integer :: i

		call make_valid_buf(present(null_value) .or. present(is_valid), size(values), valid_buf, valid_ptr)
		call parquet_read_int32_column(reader%handle, trim(name)//char(0), values, int(size(values), kind=c_long_long), valid_ptr)
		if (present(is_valid)) is_valid = valid_buf /= 0_c_int8_t
		if (present(null_value)) then
			do i = 1, size(values)
				if (valid_buf(i) == 0_c_int8_t) values(i) = null_value
			end do
		end if
	end procedure parquet_read_int32_column_1d

	module procedure parquet_read_int64_column_1d
		integer(c_int8_t), allocatable, target :: valid_buf(:)
		type(c_ptr) :: valid_ptr
		integer :: i

		call make_valid_buf(present(null_value) .or. present(is_valid), size(values), valid_buf, valid_ptr)
		call parquet_read_int64_column(reader%handle, trim(name)//char(0), values, int(size(values), kind=c_long_long), valid_ptr)
		if (present(is_valid)) is_valid = valid_buf /= 0_c_int8_t
		if (present(null_value)) then
			do i = 1, size(values)
				if (valid_buf(i) == 0_c_int8_t) values(i) = null_value
			end do
		end if
	end procedure parquet_read_int64_column_1d

	module procedure parquet_read_float32_column_1d
		integer(c_int8_t), allocatable, target :: valid_buf(:)
		type(c_ptr) :: valid_ptr
		integer :: i

		call make_valid_buf(present(null_value) .or. present(is_valid), size(values), valid_buf, valid_ptr)
		call parquet_read_float32_column(reader%handle, trim(name)//char(0), values, int(size(values), kind=c_long_long), valid_ptr)
		if (present(is_valid)) is_valid = valid_buf /= 0_c_int8_t
		if (present(null_value)) then
			do i = 1, size(values)
				if (valid_buf(i) == 0_c_int8_t) values(i) = null_value
			end do
		end if
	end procedure parquet_read_float32_column_1d

	module procedure parquet_read_float64_column_1d
		integer(c_int8_t), allocatable, target :: valid_buf(:)
		type(c_ptr) :: valid_ptr
		integer :: i

		call make_valid_buf(present(null_value) .or. present(is_valid), size(values), valid_buf, valid_ptr)
		call parquet_read_float64_column(reader%handle, trim(name)//char(0), values, int(size(values), kind=c_long_long), valid_ptr)
		if (present(is_valid)) is_valid = valid_buf /= 0_c_int8_t
		if (present(null_value)) then
			do i = 1, size(values)
				if (valid_buf(i) == 0_c_int8_t) values(i) = null_value
			end do
		end if
	end procedure parquet_read_float64_column_1d

	module procedure parquet_read_logical_column_1d
		integer(c_int8_t), allocatable :: tmp(:)
		integer(c_int8_t), allocatable, target :: valid_buf(:)
		type(c_ptr) :: valid_ptr
		integer :: i

		allocate(tmp(size(values)))
		call make_valid_buf(present(null_value) .or. present(is_valid), size(values), valid_buf, valid_ptr)
		call parquet_read_bool8_column(reader%handle, trim(name)//char(0), tmp, int(size(tmp), kind=c_long_long), valid_ptr)
		do i = 1, size(values)
			values(i) = tmp(i) /= 0_c_int8_t
		end do
		if (present(is_valid)) is_valid = valid_buf /= 0_c_int8_t
		if (present(null_value)) then
			do i = 1, size(values)
				if (valid_buf(i) == 0_c_int8_t) values(i) = null_value
			end do
		end if
	end procedure parquet_read_logical_column_1d

	module procedure parquet_read_string_column_1d
		character(kind=c_char), allocatable :: packed(:)
		integer(c_int8_t), allocatable, target :: valid_buf(:)
		type(c_ptr) :: valid_ptr
		integer :: nrows, item_len, i, j, k

		nrows = size(values)
		item_len = len(values(1))
		allocate(packed(item_len*nrows))
		call make_valid_buf(present(null_value) .or. present(is_valid), nrows, valid_buf, valid_ptr)
		call parquet_read_string_column(reader%handle, trim(name)//char(0), packed, int(item_len, kind=c_long_long), &
			int(nrows, kind=c_long_long), valid_ptr)

		k = 0
		do i = 1, nrows
			values(i) = ''
			do j = 1, item_len
				k = k + 1
				values(i)(j:j) = achar(iachar(packed(k)))
			end do
		end do
		if (present(is_valid)) is_valid = valid_buf /= 0_c_int8_t
		if (present(null_value)) then
			do i = 1, nrows
				if (valid_buf(i) == 0_c_int8_t) values(i) = null_value
			end do
		end if
	end procedure parquet_read_string_column_1d

	module procedure parquet_read_int32_array_full
		integer(c_int32_t), allocatable :: flat(:)
		integer(c_int8_t), allocatable, target :: valid_buf(:)
		type(c_ptr) :: valid_ptr
		integer :: asize, nrows, i, j, k

		asize = size(values, 1)
		nrows = size(values, 2)
		allocate(flat(asize*nrows))
		call make_valid_buf(present(null_value) .or. present(is_valid), asize*nrows, valid_buf, valid_ptr)
		call parquet_read_int32_array_column(reader%handle, trim(name)//char(0), flat, int(nrows, kind=c_long_long), &
			int(asize, kind=c_long_long), valid_ptr)
		do i = 1, nrows
			do j = 1, asize
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
		integer :: asize, nrows, i, j, k

		asize = size(values, 1)
		nrows = size(values, 2)
		allocate(flat(asize*nrows))
		call make_valid_buf(present(null_value) .or. present(is_valid), asize*nrows, valid_buf, valid_ptr)
		call parquet_read_int64_array_column(reader%handle, trim(name)//char(0), flat, int(nrows, kind=c_long_long), &
			int(asize, kind=c_long_long), valid_ptr)
		do i = 1, nrows
			do j = 1, asize
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
		integer :: asize, nrows, i, j, k

		asize = size(values, 1)
		nrows = size(values, 2)
		allocate(flat(asize*nrows))
		call make_valid_buf(present(null_value) .or. present(is_valid), asize*nrows, valid_buf, valid_ptr)
		call parquet_read_float32_array_column(reader%handle, trim(name)//char(0), flat, int(nrows, kind=c_long_long), &
			int(asize, kind=c_long_long), valid_ptr)
		do i = 1, nrows
			do j = 1, asize
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
		integer :: asize, nrows, i, j, k

		asize = size(values, 1)
		nrows = size(values, 2)
		allocate(flat(asize*nrows))
		call make_valid_buf(present(null_value) .or. present(is_valid), asize*nrows, valid_buf, valid_ptr)
		call parquet_read_float64_array_column(reader%handle, trim(name)//char(0), flat, int(nrows, kind=c_long_long), &
			int(asize, kind=c_long_long), valid_ptr)
		do i = 1, nrows
			do j = 1, asize
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
		integer :: asize, nrows, i, j, k

		asize = size(values, 1)
		nrows = size(values, 2)
		allocate(flat(asize*nrows))
		call make_valid_buf(present(null_value) .or. present(is_valid), asize*nrows, valid_buf, valid_ptr)
		call parquet_read_bool8_array_column(reader%handle, trim(name)//char(0), flat, int(nrows, kind=c_long_long), &
			int(asize, kind=c_long_long), valid_ptr)
		do i = 1, nrows
			do j = 1, asize
				k = (i-1)*asize + j
				values(j, i) = flat(k) /= 0_c_int8_t
				if (present(is_valid)) is_valid(j, i) = valid_buf(k) /= 0_c_int8_t
				if (present(null_value)) then
					if (valid_buf(k) == 0_c_int8_t) values(j, i) = null_value
				end if
			end do
		end do
	end procedure parquet_read_logical_array_full

	module procedure parquet_read_string_array_full
		character(kind=c_char), allocatable :: packed(:)
		integer(c_int8_t), allocatable, target :: valid_buf(:)
		type(c_ptr) :: valid_ptr
		integer :: asize, nrows, item_len, i, j, k, m, p

		asize = size(values, 1)
		nrows = size(values, 2)
		item_len = len(values(1,1))
		allocate(packed(item_len*asize*nrows))
		call make_valid_buf(present(null_value) .or. present(is_valid), asize*nrows, valid_buf, valid_ptr)
		call parquet_read_string_array_column(reader%handle, trim(name)//char(0), packed, int(item_len, kind=c_long_long), &
			int(nrows, kind=c_long_long), int(asize, kind=c_long_long), valid_ptr)

		p = 0
		do i = 1, nrows
			do j = 1, asize
				values(j, i) = ''
				do m = 1, item_len
					p = p + 1
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

	module procedure parquet_read_int32_array_row_mode
		integer(c_int8_t), allocatable, target :: valid_buf(:)
		type(c_ptr) :: valid_ptr
		integer :: i

		call make_valid_buf(present(null_value) .or. present(is_valid), size(values), valid_buf, valid_ptr)
		call parquet_read_int32_array_row(reader%handle, trim(name)//char(0), int(row_index, kind=c_long_long), values, &
			int(size(values), kind=c_long_long), valid_ptr)
		if (present(is_valid)) is_valid = valid_buf /= 0_c_int8_t
		if (present(null_value)) then
			do i = 1, size(values)
				if (valid_buf(i) == 0_c_int8_t) values(i) = null_value
			end do
		end if
	end procedure parquet_read_int32_array_row_mode

	module procedure parquet_read_int64_array_row_mode
		integer(c_int8_t), allocatable, target :: valid_buf(:)
		type(c_ptr) :: valid_ptr
		integer :: i

		call make_valid_buf(present(null_value) .or. present(is_valid), size(values), valid_buf, valid_ptr)
		call parquet_read_int64_array_row(reader%handle, trim(name)//char(0), int(row_index, kind=c_long_long), values, &
			int(size(values), kind=c_long_long), valid_ptr)
		if (present(is_valid)) is_valid = valid_buf /= 0_c_int8_t
		if (present(null_value)) then
			do i = 1, size(values)
				if (valid_buf(i) == 0_c_int8_t) values(i) = null_value
			end do
		end if
	end procedure parquet_read_int64_array_row_mode

	module procedure parquet_read_float32_array_row_mode
		integer(c_int8_t), allocatable, target :: valid_buf(:)
		type(c_ptr) :: valid_ptr
		integer :: i

		call make_valid_buf(present(null_value) .or. present(is_valid), size(values), valid_buf, valid_ptr)
		call parquet_read_float32_array_row(reader%handle, trim(name)//char(0), int(row_index, kind=c_long_long), values, &
			int(size(values), kind=c_long_long), valid_ptr)
		if (present(is_valid)) is_valid = valid_buf /= 0_c_int8_t
		if (present(null_value)) then
			do i = 1, size(values)
				if (valid_buf(i) == 0_c_int8_t) values(i) = null_value
			end do
		end if
	end procedure parquet_read_float32_array_row_mode

	module procedure parquet_read_float64_array_row_mode
		integer(c_int8_t), allocatable, target :: valid_buf(:)
		type(c_ptr) :: valid_ptr
		integer :: i

		call make_valid_buf(present(null_value) .or. present(is_valid), size(values), valid_buf, valid_ptr)
		call parquet_read_float64_array_row(reader%handle, trim(name)//char(0), int(row_index, kind=c_long_long), values, &
			int(size(values), kind=c_long_long), valid_ptr)
		if (present(is_valid)) is_valid = valid_buf /= 0_c_int8_t
		if (present(null_value)) then
			do i = 1, size(values)
				if (valid_buf(i) == 0_c_int8_t) values(i) = null_value
			end do
		end if
	end procedure parquet_read_float64_array_row_mode

	module procedure parquet_read_logical_array_row_mode
		integer(c_int8_t), allocatable :: tmp(:)
		integer(c_int8_t), allocatable, target :: valid_buf(:)
		type(c_ptr) :: valid_ptr
		integer :: i

		allocate(tmp(size(values)))
		call make_valid_buf(present(null_value) .or. present(is_valid), size(values), valid_buf, valid_ptr)
		call parquet_read_bool8_array_row(reader%handle, trim(name)//char(0), int(row_index, kind=c_long_long), tmp, &
			int(size(values), kind=c_long_long), valid_ptr)
		do i = 1, size(values)
			values(i) = tmp(i) /= 0_c_int8_t
		end do
		if (present(is_valid)) is_valid = valid_buf /= 0_c_int8_t
		if (present(null_value)) then
			do i = 1, size(values)
				if (valid_buf(i) == 0_c_int8_t) values(i) = null_value
			end do
		end if
	end procedure parquet_read_logical_array_row_mode

	module procedure parquet_read_string_array_row_mode
		character(kind=c_char), allocatable :: packed(:)
		integer(c_int8_t), allocatable, target :: valid_buf(:)
		type(c_ptr) :: valid_ptr
		integer :: item_len, i, j, p

		item_len = len(values(1))
		allocate(packed(item_len*size(values)))
		call make_valid_buf(present(null_value) .or. present(is_valid), size(values), valid_buf, valid_ptr)
		call parquet_read_string_array_row(reader%handle, trim(name)//char(0), int(row_index, kind=c_long_long), packed, &
			int(item_len, kind=c_long_long), int(size(values), kind=c_long_long), valid_ptr)
		p = 0
		do i = 1, size(values)
			values(i) = ''
			do j = 1, item_len
				p = p + 1
				values(i)(j:j) = achar(iachar(packed(p)))
			end do
		end do
		if (present(is_valid)) is_valid = valid_buf /= 0_c_int8_t
		if (present(null_value)) then
			do i = 1, size(values)
				if (valid_buf(i) == 0_c_int8_t) values(i) = null_value
			end do
		end if
	end procedure parquet_read_string_array_row_mode

	module procedure parquet_read_int32_array_element_mode
		integer(c_int8_t), allocatable, target :: valid_buf(:)
		type(c_ptr) :: valid_ptr
		integer :: i

		call make_valid_buf(present(null_value) .or. present(is_valid), size(values), valid_buf, valid_ptr)
		call parquet_read_int32_array_element(reader%handle, trim(name)//char(0), int(elem_index, kind=c_long_long), values, &
			int(size(values), kind=c_long_long), 0_c_long_long, valid_ptr)
		if (present(is_valid)) is_valid = valid_buf /= 0_c_int8_t
		if (present(null_value)) then
			do i = 1, size(values)
				if (valid_buf(i) == 0_c_int8_t) values(i) = null_value
			end do
		end if
	end procedure parquet_read_int32_array_element_mode

	module procedure parquet_read_int64_array_element_mode
		integer(c_int8_t), allocatable, target :: valid_buf(:)
		type(c_ptr) :: valid_ptr
		integer :: i

		call make_valid_buf(present(null_value) .or. present(is_valid), size(values), valid_buf, valid_ptr)
		call parquet_read_int64_array_element(reader%handle, trim(name)//char(0), int(elem_index, kind=c_long_long), values, &
			int(size(values), kind=c_long_long), 0_c_long_long, valid_ptr)
		if (present(is_valid)) is_valid = valid_buf /= 0_c_int8_t
		if (present(null_value)) then
			do i = 1, size(values)
				if (valid_buf(i) == 0_c_int8_t) values(i) = null_value
			end do
		end if
	end procedure parquet_read_int64_array_element_mode

	module procedure parquet_read_float32_array_element_mode
		integer(c_int8_t), allocatable, target :: valid_buf(:)
		type(c_ptr) :: valid_ptr
		integer :: i

		call make_valid_buf(present(null_value) .or. present(is_valid), size(values), valid_buf, valid_ptr)
		call parquet_read_float32_array_element(reader%handle, trim(name)//char(0), int(elem_index, kind=c_long_long), values, &
			int(size(values), kind=c_long_long), 0_c_long_long, valid_ptr)
		if (present(is_valid)) is_valid = valid_buf /= 0_c_int8_t
		if (present(null_value)) then
			do i = 1, size(values)
				if (valid_buf(i) == 0_c_int8_t) values(i) = null_value
			end do
		end if
	end procedure parquet_read_float32_array_element_mode

	module procedure parquet_read_float64_array_element_mode
		integer(c_int8_t), allocatable, target :: valid_buf(:)
		type(c_ptr) :: valid_ptr
		integer :: i

		call make_valid_buf(present(null_value) .or. present(is_valid), size(values), valid_buf, valid_ptr)
		call parquet_read_float64_array_element(reader%handle, trim(name)//char(0), int(elem_index, kind=c_long_long), values, &
			int(size(values), kind=c_long_long), 0_c_long_long, valid_ptr)
		if (present(is_valid)) is_valid = valid_buf /= 0_c_int8_t
		if (present(null_value)) then
			do i = 1, size(values)
				if (valid_buf(i) == 0_c_int8_t) values(i) = null_value
			end do
		end if
	end procedure parquet_read_float64_array_element_mode

	module procedure parquet_read_logical_array_element_mode
		integer(c_int8_t), allocatable :: tmp(:)
		integer(c_int8_t), allocatable, target :: valid_buf(:)
		type(c_ptr) :: valid_ptr
		integer :: i

		allocate(tmp(size(values)))
		call make_valid_buf(present(null_value) .or. present(is_valid), size(values), valid_buf, valid_ptr)
		call parquet_read_bool8_array_element(reader%handle, trim(name)//char(0), int(elem_index, kind=c_long_long), tmp, &
			int(size(values), kind=c_long_long), 0_c_long_long, valid_ptr)
		do i = 1, size(values)
			values(i) = tmp(i) /= 0_c_int8_t
		end do
		if (present(is_valid)) is_valid = valid_buf /= 0_c_int8_t
		if (present(null_value)) then
			do i = 1, size(values)
				if (valid_buf(i) == 0_c_int8_t) values(i) = null_value
			end do
		end if
	end procedure parquet_read_logical_array_element_mode

	module procedure parquet_read_string_array_element_mode
		character(kind=c_char), allocatable :: packed(:)
		integer(c_int8_t), allocatable, target :: valid_buf(:)
		type(c_ptr) :: valid_ptr
		integer :: item_len, i, j, p

		item_len = len(values(1))
		allocate(packed(item_len*size(values)))
		call make_valid_buf(present(null_value) .or. present(is_valid), size(values), valid_buf, valid_ptr)
		call parquet_read_string_array_element(reader%handle, trim(name)//char(0), int(elem_index, kind=c_long_long), packed, &
			int(item_len, kind=c_long_long), int(size(values), kind=c_long_long), 0_c_long_long, valid_ptr)
		p = 0
		do i = 1, size(values)
			values(i) = ''
			do j = 1, item_len
				p = p + 1
				values(i)(j:j) = achar(iachar(packed(p)))
			end do
		end do
		if (present(is_valid)) is_valid = valid_buf /= 0_c_int8_t
		if (present(null_value)) then
			do i = 1, size(values)
				if (valid_buf(i) == 0_c_int8_t) values(i) = null_value
			end do
		end if
	end procedure parquet_read_string_array_element_mode

end submodule parquet_read
