!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
submodule (parquet) parquet_read
contains

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
		call parquet_read_int32_column(reader%handle, trim(name)//char(0), values, int(size(values), kind=c_long_long))
	end procedure parquet_read_int32_column_1d

	module procedure parquet_read_int64_column_1d
		call parquet_read_int64_column(reader%handle, trim(name)//char(0), values, int(size(values), kind=c_long_long))
	end procedure parquet_read_int64_column_1d

	module procedure parquet_read_float32_column_1d
		call parquet_read_float32_column(reader%handle, trim(name)//char(0), values, int(size(values), kind=c_long_long))
	end procedure parquet_read_float32_column_1d

	module procedure parquet_read_float64_column_1d
		call parquet_read_float64_column(reader%handle, trim(name)//char(0), values, int(size(values), kind=c_long_long))
	end procedure parquet_read_float64_column_1d

	module procedure parquet_read_logical_column_1d
		integer(c_int8_t), allocatable :: tmp(:)
		integer :: i

		allocate(tmp(size(values)))
		call parquet_read_bool8_column(reader%handle, trim(name)//char(0), tmp, int(size(tmp), kind=c_long_long))
		do i = 1, size(values)
			values(i) = tmp(i) /= 0_c_int8_t
		end do
	end procedure parquet_read_logical_column_1d

	module procedure parquet_read_string_column_1d
		character(kind=c_char), allocatable :: packed(:)
		integer :: nrows, item_len, i, j, k

		nrows = size(values)
		item_len = len(values(1))
		allocate(packed(item_len*nrows))
		call parquet_read_string_column(reader%handle, trim(name)//char(0), packed, int(item_len, kind=c_long_long), int(nrows, kind=c_long_long))

		k = 0
		do i = 1, nrows
			values(i) = ''
			do j = 1, item_len
				k = k + 1
				values(i)(j:j) = achar(iachar(packed(k)))
			end do
		end do
	end procedure parquet_read_string_column_1d

	module procedure parquet_read_int32_array_full
		integer(c_int32_t), allocatable :: flat(:)
		integer :: asize, nrows, i, j

		asize = size(values, 1)
		nrows = size(values, 2)
		allocate(flat(asize*nrows))
		call parquet_read_int32_array_column(reader%handle, trim(name)//char(0), flat, int(nrows, kind=c_long_long), int(asize, kind=c_long_long))
		do i = 1, nrows
			do j = 1, asize
				values(j, i) = flat((i-1)*asize + j)
			end do
		end do
	end procedure parquet_read_int32_array_full

	module procedure parquet_read_int64_array_full
		integer(c_int64_t), allocatable :: flat(:)
		integer :: asize, nrows, i, j

		asize = size(values, 1)
		nrows = size(values, 2)
		allocate(flat(asize*nrows))
		call parquet_read_int64_array_column(reader%handle, trim(name)//char(0), flat, int(nrows, kind=c_long_long), int(asize, kind=c_long_long))
		do i = 1, nrows
			do j = 1, asize
				values(j, i) = flat((i-1)*asize + j)
			end do
		end do
	end procedure parquet_read_int64_array_full

	module procedure parquet_read_float32_array_full
		real(c_float), allocatable :: flat(:)
		integer :: asize, nrows, i, j

		asize = size(values, 1)
		nrows = size(values, 2)
		allocate(flat(asize*nrows))
		call parquet_read_float32_array_column(reader%handle, trim(name)//char(0), flat, int(nrows, kind=c_long_long), int(asize, kind=c_long_long))
		do i = 1, nrows
			do j = 1, asize
				values(j, i) = flat((i-1)*asize + j)
			end do
		end do
	end procedure parquet_read_float32_array_full

	module procedure parquet_read_float64_array_full
		real(c_double), allocatable :: flat(:)
		integer :: asize, nrows, i, j

		asize = size(values, 1)
		nrows = size(values, 2)
		allocate(flat(asize*nrows))
		call parquet_read_float64_array_column(reader%handle, trim(name)//char(0), flat, int(nrows, kind=c_long_long), int(asize, kind=c_long_long))
		do i = 1, nrows
			do j = 1, asize
				values(j, i) = flat((i-1)*asize + j)
			end do
		end do
	end procedure parquet_read_float64_array_full

	module procedure parquet_read_logical_array_full
		integer(c_int8_t), allocatable :: flat(:)
		integer :: asize, nrows, i, j

		asize = size(values, 1)
		nrows = size(values, 2)
		allocate(flat(asize*nrows))
		call parquet_read_bool8_array_column(reader%handle, trim(name)//char(0), flat, int(nrows, kind=c_long_long), int(asize, kind=c_long_long))
		do i = 1, nrows
			do j = 1, asize
				values(j, i) = flat((i-1)*asize + j) /= 0_c_int8_t
			end do
		end do
	end procedure parquet_read_logical_array_full

	module procedure parquet_read_string_array_full
		character(kind=c_char), allocatable :: packed(:)
		integer :: asize, nrows, item_len, i, j, k, p

		asize = size(values, 1)
		nrows = size(values, 2)
		item_len = len(values(1,1))
		allocate(packed(item_len*asize*nrows))
		call parquet_read_string_array_column(reader%handle, trim(name)//char(0), packed, int(item_len, kind=c_long_long), int(nrows, kind=c_long_long), int(asize, kind=c_long_long))

		p = 0
		do i = 1, nrows
			do j = 1, asize
				values(j, i) = ''
				do k = 1, item_len
					p = p + 1
					values(j, i)(k:k) = achar(iachar(packed(p)))
				end do
			end do
		end do
	end procedure parquet_read_string_array_full

	module procedure parquet_read_int32_array_row_mode
		call parquet_read_int32_array_row(reader%handle, trim(name)//char(0), int(row_index, kind=c_long_long), values, int(size(values), kind=c_long_long))
	end procedure parquet_read_int32_array_row_mode

	module procedure parquet_read_int64_array_row_mode
		call parquet_read_int64_array_row(reader%handle, trim(name)//char(0), int(row_index, kind=c_long_long), values, int(size(values), kind=c_long_long))
	end procedure parquet_read_int64_array_row_mode

	module procedure parquet_read_float32_array_row_mode
		call parquet_read_float32_array_row(reader%handle, trim(name)//char(0), int(row_index, kind=c_long_long), values, int(size(values), kind=c_long_long))
	end procedure parquet_read_float32_array_row_mode

	module procedure parquet_read_float64_array_row_mode
		call parquet_read_float64_array_row(reader%handle, trim(name)//char(0), int(row_index, kind=c_long_long), values, int(size(values), kind=c_long_long))
	end procedure parquet_read_float64_array_row_mode

	module procedure parquet_read_logical_array_row_mode
		integer(c_int8_t), allocatable :: tmp(:)
		integer :: i

		allocate(tmp(size(values)))
		call parquet_read_bool8_array_row(reader%handle, trim(name)//char(0), int(row_index, kind=c_long_long), tmp, int(size(values), kind=c_long_long))
		do i = 1, size(values)
			values(i) = tmp(i) /= 0_c_int8_t
		end do
	end procedure parquet_read_logical_array_row_mode

	module procedure parquet_read_string_array_row_mode
		character(kind=c_char), allocatable :: packed(:)
		integer :: item_len, i, j, p

		item_len = len(values(1))
		allocate(packed(item_len*size(values)))
		call parquet_read_string_array_row(reader%handle, trim(name)//char(0), int(row_index, kind=c_long_long), packed, int(item_len, kind=c_long_long), int(size(values), kind=c_long_long))
		p = 0
		do i = 1, size(values)
			values(i) = ''
			do j = 1, item_len
				p = p + 1
				values(i)(j:j) = achar(iachar(packed(p)))
			end do
		end do
	end procedure parquet_read_string_array_row_mode

	module procedure parquet_read_int32_array_element_mode
		call parquet_read_int32_array_element(reader%handle, trim(name)//char(0), int(elem_index, kind=c_long_long), values, int(size(values), kind=c_long_long), 0_c_long_long)
	end procedure parquet_read_int32_array_element_mode

	module procedure parquet_read_int64_array_element_mode
		call parquet_read_int64_array_element(reader%handle, trim(name)//char(0), int(elem_index, kind=c_long_long), values, int(size(values), kind=c_long_long), 0_c_long_long)
	end procedure parquet_read_int64_array_element_mode

	module procedure parquet_read_float32_array_element_mode
		call parquet_read_float32_array_element(reader%handle, trim(name)//char(0), int(elem_index, kind=c_long_long), values, int(size(values), kind=c_long_long), 0_c_long_long)
	end procedure parquet_read_float32_array_element_mode

	module procedure parquet_read_float64_array_element_mode
		call parquet_read_float64_array_element(reader%handle, trim(name)//char(0), int(elem_index, kind=c_long_long), values, int(size(values), kind=c_long_long), 0_c_long_long)
	end procedure parquet_read_float64_array_element_mode

	module procedure parquet_read_logical_array_element_mode
		integer(c_int8_t), allocatable :: tmp(:)
		integer :: i

		allocate(tmp(size(values)))
		call parquet_read_bool8_array_element(reader%handle, trim(name)//char(0), int(elem_index, kind=c_long_long), tmp, int(size(values), kind=c_long_long), 0_c_long_long)
		do i = 1, size(values)
			values(i) = tmp(i) /= 0_c_int8_t
		end do
	end procedure parquet_read_logical_array_element_mode

	module procedure parquet_read_string_array_element_mode
		character(kind=c_char), allocatable :: packed(:)
		integer :: item_len, i, j, p

		item_len = len(values(1))
		allocate(packed(item_len*size(values)))
		call parquet_read_string_array_element(reader%handle, trim(name)//char(0), int(elem_index, kind=c_long_long), packed, int(item_len, kind=c_long_long), int(size(values), kind=c_long_long), 0_c_long_long)
		p = 0
		do i = 1, size(values)
			values(i) = ''
			do j = 1, item_len
				p = p + 1
				values(i)(j:j) = achar(iachar(packed(p)))
			end do
		end do
	end procedure parquet_read_string_array_element_mode

end submodule parquet_read
