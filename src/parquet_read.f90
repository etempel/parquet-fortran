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

    !> Every parquet_read_column variant calls this first: reader%handle is
    !> c_null_ptr until parquet_open_reader sets it, and every C++ entry point
    !> dereferences the handle immediately (see ConcurrencyGuard in
    !> parquet_wrapper.cpp) with no null check of its own -- calling in with an
    !> unopened reader previously crashed with an unhelpful SIGSEGV instead of
    !> a clean, diagnosable error.
    subroutine check_reader_open(reader, context)
        type(parquet_reader), intent(in) :: reader
        character(len=*), intent(in) :: context
        if (.not. c_associated(reader%handle)) then
            error stop trim(context) // ": reader has not been opened (call parquet_open_reader first)"
        end if
    end subroutine check_reader_open

    !> Every reader-taking procedure that also names a specific column calls
    !> this right after check_reader_open: an unrecognized column name used
    !> to reach the C++ side's own "Column not found" exception uncaught
    !> (get_single_chunk_array/get_column_index in parquet_wrapper.cpp),
    !> crashing with an unhelpful SIGABRT instead of a clean, diagnosable
    !> error -- the same class of bug parquet_prefetch_columns already
    !> guarded against (it validates every name up front for the same
    !> reason). Reuses parquet_reader_has_column, the same non-throwing
    !> schema lookup parquet_prefetch_columns uses.
    subroutine check_column_exists(reader, name, context)
        type(parquet_reader), intent(in) :: reader
        character(len=*), intent(in) :: name, context
        if (parquet_reader_has_column(reader%handle, trim(name)//char(0)) == 0) then
            error stop trim(context) // ": column not found in parquet file: " // trim(name)
        end if
    end subroutine check_column_exists

    !> Splits one parquet_filter%add rule ("<column> <op> [value]") into its
    !> three parts, purely by syntax -- no schema access here, so this cannot
    !> check that `name` is a real column or that `value` is well-formed for
    !> that column's actual type (parquet_reader_set_filter does that, once
    !> the schema is available). Deliberately NOT a general boolean-expression
    !> parser: exactly one clause per rule, no AND/OR/parens inside the string
    !> itself -- see the parquet_filter type's own doc comment.
    subroutine parquet_tokenize_filter_rule(rule, name, op, value, is_string, ok, errmsg)
        character(len=*), intent(in) :: rule
        character(len=:), allocatable, intent(out) :: name, op, value, errmsg
        logical, intent(out) :: is_string, ok
        character(len=:), allocatable :: t, rest
        integer :: p

        ok = .false.
        is_string = .false.
        value = ""
        name = ""
        op = ""
        errmsg = ""
        t = trim(adjustl(rule))
        if (len(t) == 0) then
            errmsg = "empty filter rule"
            return
        end if

        p = index(t, " ")
        if (p == 0) then
            errmsg = "filter rule '" // t // "' is missing an operator"
            return
        end if
        name = t(1:p-1)
        rest = trim(adjustl(t(p+1:)))
        if (len(rest) == 0) then
            errmsg = "filter rule '" // t // "' is missing an operator"
            return
        end if

        p = index(rest, " ")
        if (p == 0) then
            op = rest
            rest = ""
        else
            op = rest(1:p-1)
            rest = trim(adjustl(rest(p+1:)))
        end if

        select case (trim(op))
        case ("is_null", "is_not_null")
            if (len(rest) > 0) then
                errmsg = "filter rule '" // t // "': " // trim(op) // " takes no value"
                return
            end if
        case (">", ">=", "<", "<=", "==", "/=")
            if (len(rest) == 0) then
                errmsg = "filter rule '" // t // "' is missing a value after '" // trim(op) // "'"
                return
            end if
            if (rest(1:1) == '"') then
                if (len(rest) < 2 .or. rest(len(rest):len(rest)) /= '"') then
                    errmsg = "filter rule '" // t // "' has an unterminated quoted value"
                    return
                end if
                value = rest(2:len(rest)-1)
                is_string = .true.
            else
                value = rest
                is_string = .false.
            end if
        case default
            errmsg = "filter rule '" // t // "' has an unknown operator '" // trim(op) // "'"
            return
        end select

        ok = .true.
    end subroutine parquet_tokenize_filter_rule

    !> Packs a fixed-width character array into the same "n fixed-width items
    !> back to back" convention used for names_packed elsewhere in this file
    !> (see parquet_prefetch_columns) -- shared here since apply_parquet_filter
    !> needs it for three separate arrays (names/ops/values).
    subroutine pack_fixed_width_strings(strs, packed)
        character(len=*), intent(in) :: strs(:)
        character(kind=c_char), allocatable, intent(out) :: packed(:)
        integer :: i, j, k, item_len, n

        item_len = len(strs)
        n = size(strs)
        allocate(packed(item_len * n))
        k = 0
        do i = 1, n
            do j = 1, item_len
                k = k + 1
                packed(k) = achar(iachar(strs(i)(j:j)), kind=c_char)
            end do
        end do
    end subroutine pack_fixed_width_strings

    !> Tokenizes and applies every rule in `filter` to `reader` -- called from
    !> parquet_open_reader right after the reader itself is created, so
    !> parquet_get_nrows and every column read afterward already reflect the
    !> filtered row set (see parquet_reader_set_filter in parquet_wrapper.cpp
    !> for the actual validation/masking).
    subroutine apply_parquet_filter(reader, filter)
        type(parquet_reader), intent(inout) :: reader
        type(parquet_filter), intent(in) :: filter
        character(len=64), allocatable :: names(:)
        character(len=16), allocatable :: ops(:)
        character(len=512), allocatable :: values(:)
        integer(c_int8_t), allocatable :: is_string_flags(:)
        character(kind=c_char), allocatable :: names_packed(:), ops_packed(:), values_packed(:)
        character(len=:), allocatable :: parsed_name, parsed_op, parsed_value, errmsg
        logical :: parsed_is_string, ok
        character(len=1024) :: c_err
        integer(c_long_long) :: status
        integer :: i, n

        n = filter%n
        allocate(names(n), ops(n), values(n), is_string_flags(n))

        do i = 1, n
            call parquet_tokenize_filter_rule(filter%rules(i), parsed_name, parsed_op, parsed_value, &
                parsed_is_string, ok, errmsg)
            if (.not. ok) error stop "parquet_open_reader: invalid filter rule: " // errmsg
            if (len(parsed_name) > len(names) .or. len(parsed_op) > len(ops) .or. len(parsed_value) > len(values)) then
                error stop "parquet_open_reader: filter rule exceeds an internal length limit: " // trim(filter%rules(i))
            end if
            names(i) = parsed_name
            ops(i) = parsed_op
            values(i) = parsed_value
            is_string_flags(i) = merge(1_c_int8_t, 0_c_int8_t, parsed_is_string)
        end do

        call pack_fixed_width_strings(names, names_packed)
        call pack_fixed_width_strings(ops, ops_packed)
        call pack_fixed_width_strings(values, values_packed)

        c_err = ""
        status = parquet_reader_set_filter(reader%handle, names_packed, int(len(names), kind=c_long_long), &
            ops_packed, int(len(ops), kind=c_long_long), values_packed, int(len(values), kind=c_long_long), &
            is_string_flags, int(n, kind=c_long_long), c_err, int(len(c_err), kind=c_long_long))

        if (status /= 0) error stop "parquet_open_reader: " // trim(c_err)
    end subroutine apply_parquet_filter

    module procedure parquet_open_reader
        logical :: use_threads_value

        use_threads_value = .true.
        if (present(use_threads)) use_threads_value = use_threads

        reader%handle = create_parquet_reader(trim(filename)//char(0), merge(1_c_int, 0_c_int, use_threads_value))

        if (present(filter)) then
            if (filter%n > 0) call apply_parquet_filter(reader, filter)
        end if
    end procedure parquet_open_reader

    module procedure parquet_close_reader
        if (.not. c_associated(reader%handle)) then
            error stop "parquet_close_reader: reader has not been opened, or was already closed"
        end if
        if (present(print_stat)) then
            if (print_stat) call parquet_reader_print_stat(reader%handle)
        end if
        call close_parquet_reader(reader%handle)
        reader%handle = c_null_ptr
    end procedure parquet_close_reader

    !> Warms the cache for every column in `names` with a single, parallelizable
    !> (use_threads is on -- see create_parquet_reader) Arrow read, instead of
    !> one lazy single-column read per name. Purely additive: parquet_read_column
    !> (or any other read call) still works for any column, prefetched or not --
    !> a non-prefetched name simply falls through to the existing lazy,
    !> read-on-first-request path exactly as if this was never called.
    module procedure parquet_prefetch_columns
        character(kind=c_char), allocatable :: packed(:)
        integer :: i, j, k, item_len, n

        call check_reader_open(reader, "parquet_prefetch_columns")
        n = size(names)
        if (n <= 0) return

        do i = 1, n
            if (parquet_reader_has_column(reader%handle, trim(names(i))//char(0)) == 0) then
                error stop "parquet_prefetch_columns: column not found in parquet file: " // trim(names(i))
            end if
        end do

        item_len = len(names(1))
        allocate(packed(item_len * n))

        k = 0
        do i = 1, n
            do j = 1, item_len
                k = k + 1
                packed(k) = achar(iachar(names(i)(j:j)), kind=c_char)
            end do
        end do

        call parquet_reader_prefetch_columns(reader%handle, packed, int(item_len, kind=c_long_long), int(n, kind=c_long_long))
    end procedure parquet_prefetch_columns

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
        call check_reader_open(reader, "parquet_get_nrows")
        nrows = int(parquet_reader_get_nrows(reader%handle), kind=int64)
    end procedure parquet_get_nrows_int64

    module procedure parquet_get_nrows_int32
        integer(int64) :: nrows64
        call check_reader_open(reader, "parquet_get_nrows")
        nrows64 = int(parquet_reader_get_nrows(reader%handle), kind=int64)
        if (nrows64 > huge(0_int32)) then
            error stop "parquet_get_nrows: number of rows exceeds int32 range"
        end if
        nrows = int(nrows64, kind=int32)
    end procedure parquet_get_nrows_int32

    module procedure parquet_get_col_size
        call check_reader_open(reader, "parquet_get_col_size")
        call check_column_exists(reader, name, "parquet_get_col_size")
        col_size = int(parquet_reader_get_column_col_size(reader%handle, trim(name)//char(0)))
    end procedure parquet_get_col_size

    module procedure parquet_get_column_total_elements_int64
        call check_reader_open(reader, "parquet_get_column_total_elements")
        call check_column_exists(reader, name, "parquet_get_column_total_elements")
        total_elements = int(parquet_reader_get_column_total_elements(reader%handle, trim(name)//char(0)), kind=int64)
    end procedure parquet_get_column_total_elements_int64

    module procedure parquet_get_column_total_elements_int32
        integer(int64) :: nelem64
        call check_reader_open(reader, "parquet_get_column_total_elements")
        call check_column_exists(reader, name, "parquet_get_column_total_elements")
        nelem64 = int(parquet_reader_get_column_total_elements(reader%handle, trim(name)//char(0)), kind=int64)
        if (nelem64 > huge(0_int32)) then
            error stop "parquet_get_column_total_elements: number of elements exceeds int32 range for column: " // trim(name)
        end if
        total_elements = int(nelem64, kind=int32)
    end procedure parquet_get_column_total_elements_int32

    module procedure parquet_get_string_length
        call check_reader_open(reader, "parquet_get_string_length")
        call check_column_exists(reader, name, "parquet_get_string_length")
        max_string_length = int(parquet_reader_get_string_length(reader%handle, trim(name)//char(0)))
    end procedure parquet_get_string_length

    module procedure parquet_check_read_row_count
        integer(c_long_long) :: file_nrows
        character(len=32) :: expected_str, got_str

        file_nrows = parquet_reader_get_nrows(reader%handle)
        if (given_nrows /= file_nrows) then
            write(expected_str, '(i0)') file_nrows
            write(got_str, '(i0)') given_nrows
            error stop "parquet_read_column: row count mismatch for column " // trim(name) // &
                ": file has " // trim(expected_str) // " rows but the values array implies " // trim(got_str)
        end if
    end procedure parquet_check_read_row_count

    module procedure parquet_read_int32_column_1d
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        integer :: i

        call check_reader_open(reader, "parquet_read_column")
        call check_column_exists(reader, name, "parquet_read_column")
        call parquet_check_read_row_count(reader, name, int(size(values), kind=c_long_long))
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

        call check_reader_open(reader, "parquet_read_column")
        call check_column_exists(reader, name, "parquet_read_column")
        call parquet_check_read_row_count(reader, name, int(size(values), kind=c_long_long))
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

        call check_reader_open(reader, "parquet_read_column")
        call check_column_exists(reader, name, "parquet_read_column")
        call parquet_check_read_row_count(reader, name, int(size(values), kind=c_long_long))
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

        call check_reader_open(reader, "parquet_read_column")
        call check_column_exists(reader, name, "parquet_read_column")
        call parquet_check_read_row_count(reader, name, int(size(values), kind=c_long_long))
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

        call check_reader_open(reader, "parquet_read_column")
        call check_column_exists(reader, name, "parquet_read_column")
        call parquet_check_read_row_count(reader, name, int(size(values), kind=c_long_long))
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
        call check_reader_open(reader, "parquet_read_column")
        call check_column_exists(reader, name, "parquet_read_column")
        call parquet_check_read_row_count(reader, name, int(nrows, kind=c_long_long))
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
        call check_reader_open(reader, "parquet_read_column")
        call check_column_exists(reader, name, "parquet_read_column")
        call parquet_check_read_row_count(reader, name, int(nrows, kind=c_long_long))
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
        call check_reader_open(reader, "parquet_read_column")
        call check_column_exists(reader, name, "parquet_read_column")
        call parquet_check_read_row_count(reader, name, int(nrows, kind=c_long_long))
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
        call check_reader_open(reader, "parquet_read_column")
        call check_column_exists(reader, name, "parquet_read_column")
        call parquet_check_read_row_count(reader, name, int(nrows, kind=c_long_long))
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
        call check_reader_open(reader, "parquet_read_column")
        call check_column_exists(reader, name, "parquet_read_column")
        call parquet_check_read_row_count(reader, name, int(nrows, kind=c_long_long))
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
        call check_reader_open(reader, "parquet_read_column")
        call check_column_exists(reader, name, "parquet_read_column")
        call parquet_check_read_row_count(reader, name, int(nrows, kind=c_long_long))
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
        call check_reader_open(reader, "parquet_read_column")
        call check_column_exists(reader, name, "parquet_read_column")
        call parquet_check_read_row_count(reader, name, int(nrows, kind=c_long_long))
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

        call check_reader_open(reader, "parquet_read_array_row_mode")
        call check_column_exists(reader, name, "parquet_read_array_row_mode")
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

        call check_reader_open(reader, "parquet_read_array_row_mode")
        call check_column_exists(reader, name, "parquet_read_array_row_mode")
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

        call check_reader_open(reader, "parquet_read_array_row_mode")
        call check_column_exists(reader, name, "parquet_read_array_row_mode")
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

        call check_reader_open(reader, "parquet_read_array_row_mode")
        call check_column_exists(reader, name, "parquet_read_array_row_mode")
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

        call check_reader_open(reader, "parquet_read_array_row_mode")
        call check_column_exists(reader, name, "parquet_read_array_row_mode")
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

        call check_reader_open(reader, "parquet_read_array_row_mode")
        call check_column_exists(reader, name, "parquet_read_array_row_mode")
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

        call check_reader_open(reader, "parquet_read_array_element_mode")
        call check_column_exists(reader, name, "parquet_read_array_element_mode")
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

        call check_reader_open(reader, "parquet_read_array_element_mode")
        call check_column_exists(reader, name, "parquet_read_array_element_mode")
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

        call check_reader_open(reader, "parquet_read_array_element_mode")
        call check_column_exists(reader, name, "parquet_read_array_element_mode")
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

        call check_reader_open(reader, "parquet_read_array_element_mode")
        call check_column_exists(reader, name, "parquet_read_array_element_mode")
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

        call check_reader_open(reader, "parquet_read_array_element_mode")
        call check_column_exists(reader, name, "parquet_read_array_element_mode")
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

        call check_reader_open(reader, "parquet_read_array_element_mode")
        call check_column_exists(reader, name, "parquet_read_array_element_mode")
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
