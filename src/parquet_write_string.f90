!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!> String write specifics (scalar and matrix, whole-column and
!> row-group-chunked, plus the fixed-width "compact" variants): bodies of the
!> module procedures declared in parquet.f90's interface block, plus the
!> string-only private qc: enforcement/bound-satisfaction helpers they
!> depend on.
submodule (parquet:parquet_write) parquet_write_string
    implicit none
contains

    !> String form of parquet_qc_numeric_satisfies: compares `value` against
    !> `bound` lexicographically (Fortran's intrinsic character relational
    !> operators) under min:/max: operator `op`.
    logical function parquet_qc_string_satisfies(value, bound, op) result(ok)
        character(len=*), intent(in) :: value !! value being checked.
        character(len=*), intent(in) :: bound !! declared qc: min:/max: bound.
        character(len=*), intent(in) :: op !! comparison operator (">=", "<=", ">", "<").

        select case (trim(op))
        case (">=")
            ok = value >= bound
        case ("<=")
            ok = value <= bound
        case (">")
            ok = value > bound
        case ("<")
            ok = value < bound
        case default
            ok = .true.
        end select
    end function parquet_qc_string_satisfies
    !> Same as parquet_check_qc_numeric but for a "string" column: bounds are
    !> compared as literal Fortran character strings (lexicographic, via the
    !> intrinsic relational operators) rather than parsed as numbers.
    subroutine parquet_check_qc_string(writer, name, values, is_valid_flat)
        type(parquet_writer), intent(in) :: writer !! open (schema-enforced) writer.
        character(len=*), intent(in) :: name !! string column name.
        character(len=*), intent(in) :: values(:) !! flattened column values.
        logical, intent(in) :: is_valid_flat(:) !! flattened validity mask (.true. => checked).
        integer :: idx
        integer(int64) :: i, n_valid, n_violate
        logical :: any_valid, ok
        character(len=:), allocatable :: data_min, data_max, bounds_desc, fmt_int, fmt_int2

        if (.not. writer%qc) return
        if (.not. writer%is_schema_enforced) return
        idx = parquet_get_defined_column_index(writer, name)
        if (idx == 0) return
        if (.not. (writer%all_columns(idx)%has_qc_min .or. writer%all_columns(idx)%has_qc_max)) return

        any_valid = .false.
        n_valid = 0_int64
        n_violate = 0_int64
        do i = 1_int64, size(values, kind=int64)
            if (.not. is_valid_flat(i)) cycle
            n_valid = n_valid + 1
            if (.not. any_valid) then
                data_min = trim(values(i))
                data_max = trim(values(i))
                any_valid = .true.
            else
                if (trim(values(i)) < data_min) data_min = trim(values(i))
                if (trim(values(i)) > data_max) data_max = trim(values(i))
            end if

            ok = .true.
            if (writer%all_columns(idx)%has_qc_min) then
                ok = ok .and. parquet_qc_string_satisfies( &
                    trim(values(i)), trim(writer%all_columns(idx)%qc_min_raw), writer%all_columns(idx)%qc_min_op)
            end if
            if (writer%all_columns(idx)%has_qc_max) then
                ok = ok .and. parquet_qc_string_satisfies( &
                    trim(values(i)), trim(writer%all_columns(idx)%qc_max_raw), writer%all_columns(idx)%qc_max_op)
            end if
            if (.not. ok) n_violate = n_violate + 1
        end do
        if (.not. any_valid .or. n_violate == 0) return

        bounds_desc = ""
        if (writer%all_columns(idx)%has_qc_min) then
            bounds_desc = "min " // trim(writer%all_columns(idx)%qc_min_op) // " '" // &
                trim(writer%all_columns(idx)%qc_min_raw) // "'"
        end if
        if (writer%all_columns(idx)%has_qc_max) then
            if (len_trim(bounds_desc) > 0) bounds_desc = bounds_desc // ", "
            bounds_desc = bounds_desc // "max " // trim(writer%all_columns(idx)%qc_max_op) // " '" // &
                trim(writer%all_columns(idx)%qc_max_raw) // "'"
        end if

        call parquet_qc_format_int(n_violate, fmt_int)
        call parquet_qc_format_int(n_valid, fmt_int2)
        print '(a)', "WARNING: qc violation for column '" // trim(name) // "': declared " // bounds_desc // &
            ", data range ['" // data_min // "', '" // data_max // "'], " // &
            fmt_int // " of " // fmt_int2 // " valid element(s) out of range"
    end subroutine parquet_check_qc_string
    !> Same as parquet_check_qc_string but reading from a parquet_string_column source directly
    !> (the compact write path, parquet_write_string_column_compact/_chunk_compact) instead of
    !> requiring a pre-materialized character(len=*) array -- so a compact write never needs a
    !> padded intermediate just to run this check. Unlike parquet_check_qc_string (which compares
    !> trimmed values, since a padded array's trailing blanks may just be padding), values here
    !> are compared verbatim: parquet_string_column stores content exactly as given, so a
    !> trailing space is always real data, never padding -- see "Trimming on append" in
    !> doc/pages/string-columns.md.
    subroutine parquet_check_qc_string_compact(writer, name, values)
        type(parquet_writer), intent(in) :: writer !! open (schema-enforced) writer.
        character(len=*), intent(in) :: name !! string column name.
        type(parquet_string_column), intent(in) :: values !! the compact column being written.
        integer :: idx
        integer(int64) :: i, n_valid, n_violate, nrows
        logical :: any_valid, ok
        character(len=:), allocatable :: data_min, data_max, bounds_desc, fmt_int, fmt_int2, s

        if (.not. writer%qc) return
        if (.not. writer%is_schema_enforced) return
        idx = parquet_get_defined_column_index(writer, name)
        if (idx == 0) return
        if (.not. (writer%all_columns(idx)%has_qc_min .or. writer%all_columns(idx)%has_qc_max)) return

        any_valid = .false.
        n_valid = 0_int64
        n_violate = 0_int64
        nrows = values%size()
        do i = 1_int64, nrows
            if (values%is_null(i)) cycle
            call values%get(i, s)
            n_valid = n_valid + 1
            if (.not. any_valid) then
                data_min = s
                data_max = s
                any_valid = .true.
            else
                if (s < data_min) data_min = s
                if (s > data_max) data_max = s
            end if

            ok = .true.
            if (writer%all_columns(idx)%has_qc_min) then
                ok = ok .and. parquet_qc_string_satisfies( &
                    s, trim(writer%all_columns(idx)%qc_min_raw), writer%all_columns(idx)%qc_min_op)
            end if
            if (writer%all_columns(idx)%has_qc_max) then
                ok = ok .and. parquet_qc_string_satisfies( &
                    s, trim(writer%all_columns(idx)%qc_max_raw), writer%all_columns(idx)%qc_max_op)
            end if
            if (.not. ok) n_violate = n_violate + 1
        end do
        if (.not. any_valid .or. n_violate == 0) return

        bounds_desc = ""
        if (writer%all_columns(idx)%has_qc_min) then
            bounds_desc = "min " // trim(writer%all_columns(idx)%qc_min_op) // " '" // &
                trim(writer%all_columns(idx)%qc_min_raw) // "'"
        end if
        if (writer%all_columns(idx)%has_qc_max) then
            if (len_trim(bounds_desc) > 0) bounds_desc = bounds_desc // ", "
            bounds_desc = bounds_desc // "max " // trim(writer%all_columns(idx)%qc_max_op) // " '" // &
                trim(writer%all_columns(idx)%qc_max_raw) // "'"
        end if

        call parquet_qc_format_int(n_violate, fmt_int)
        call parquet_qc_format_int(n_valid, fmt_int2)
        print '(a)', "WARNING: qc violation for column '" // trim(name) // "': declared " // bounds_desc // &
            ", data range ['" // data_min // "', '" // data_max // "'], " // &
            fmt_int // " of " // fmt_int2 // " valid element(s) out of range"
    end subroutine parquet_check_qc_string_compact
    module procedure parquet_write_string_column
        character(kind=c_char), allocatable :: packed(:)
        integer(int64) :: i, k, nrows, asize, nitems
        integer :: j, item_len, idx, max_item_len, max_string_len
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        character(len=:), allocatable :: ctx !! writer_context_suffix scratch.
        character(len=:), allocatable :: outname !! parquet_resolve_output_name scratch.
        logical, allocatable :: row_mask(:), elem_mask(:), is_valid_c(:)
        character(len=:), allocatable :: values_c(:)
        call check_writer_open(writer)

        if (writer%is_schema_enforced) then
            idx = parquet_get_defined_column_index(writer, name)
            call writer_context_suffix(writer, ctx)
            if (idx == 0) error stop &
                "parquet_write_column: column not defined in parquet_open_writer: " // trim(name) // ctx
            if (.not. writer%all_columns(idx)%is_set) return
        end if

        call parquet_assert_column_type(writer, name, "string")

        if (.not. parquet_is_column_enabled(writer, name)) return
        call parquet_mark_column_written(writer, name)

        asize = int(parquet_get_column_col_size(writer, name), kind=int64)
        nitems = size(values, kind=int64)
        if (nitems <= 0) return
        call writer_context_suffix(writer, ctx)
        if (mod(nitems, asize) /= 0) error stop &
            "parquet_write_string_column: values size is not divisible by col_size for column " // &
            trim(name) // ctx

        if (writer%is_schema_enforced) then
            call parquet_resolve_or_check_array_size(writer, name, idx, len(values(1)))
            max_string_len = max(1, writer%all_columns(idx)%array_size)
            max_item_len = maxval([(len_trim(values(i)), i=1_int64,nitems)])
            if (max_item_len > max_string_len) then
                error stop "parquet_write_string_column: string length exceeds declared array_size for column: " // trim(name)
            end if
        end if

        nrows = nitems / asize

        call parquet_writer_whole_column_mask(writer, name, nrows, row_mask)
        elem_mask = parquet_mask_expand_block(row_mask, asize)
        values_c = pack(values, elem_mask)
        nrows = count(row_mask, kind=int64)
        nitems = size(values_c, kind=int64)
        call parquet_check_row_count(writer, name, nrows)

        item_len = len(values(1))
        allocate(packed(item_len*nitems))

        k = 0_int64
        do i = 1_int64, nitems
            do j = 1, item_len
                k = k + 1_int64
                packed(k) = achar(iachar(values_c(i)(j:j)), kind=c_char)
            end do
        end do

        if (present(is_valid)) then
            is_valid_c = pack(is_valid, elem_mask)
            call parquet_check_protected(writer, name, is_valid_c)
            call parquet_check_qc_miss(writer, name, is_valid_c)
            call parquet_check_qc_string(writer, name, values_c, is_valid_c)
        else
            call parquet_check_qc_string(writer, name, values_c, spread(.true., 1, size(values_c, kind=int64)))
        end if
        call parquet_make_valid_buf_write(is_valid_c, valid_buf, valid_ptr)

        if (asize == 1) then
            call parquet_resolve_output_name(writer, name, outname)
            call parquet_append_string_column(&
                writer%handle, &
                trim(outname)//char(0), &
                packed, &
                int(item_len, kind=c_long_long), &
                nrows, &
                valid_ptr )
        else
            call parquet_resolve_output_name(writer, name, outname)
            call parquet_append_string_array_column(&
                writer%handle, &
                trim(outname)//char(0), &
                packed, &
                int(item_len, kind=c_long_long), &
                nrows, &
                asize, &
                valid_ptr )
        end if
    end procedure parquet_write_string_column
    module procedure parquet_write_string_matrix_column
        character(kind=c_char), allocatable :: packed(:)
        integer(int64) :: i, j, k, nrows, asize, nitems
        integer :: l, item_len, idx, max_item_len, max_string_len
        character(len=:), allocatable :: ctx !! writer_context_suffix scratch.
        character(len=:), allocatable :: outname !! parquet_resolve_output_name scratch.
        logical, allocatable :: valid_flat(:)
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        logical, allocatable :: row_mask(:), elem_mask(:)
        character(len=:), allocatable :: values_c(:,:)
        call check_writer_open(writer)

        asize = size(values, 1, kind=int64)
        nrows = size(values, 2, kind=int64)

        if (writer%is_schema_enforced) then
            idx = parquet_get_defined_column_index(writer, name)
            call writer_context_suffix(writer, ctx)
            if (idx == 0) error stop &
                "parquet_write_column: column not defined in parquet_open_writer: " // trim(name) // ctx
            if (.not. writer%all_columns(idx)%is_set) return
            call parquet_resolve_or_check_col_size(writer, name, idx, asize, "parquet_write_column")

            call parquet_resolve_or_check_array_size(writer, name, idx, len(values(1, 1)))
            max_string_len = max(1, writer%all_columns(idx)%array_size)
            max_item_len = maxval(len_trim(values))
            if (max_item_len > max_string_len) then
                error stop "parquet_write_string_matrix_column: string length exceeds declared array_size for column: " &
                    // trim(name)
            end if
        end if

        call parquet_assert_column_type(writer, name, "string")

        if (.not. parquet_is_column_enabled(writer, name)) return
        call parquet_mark_column_written(writer, name)

        nitems = size(values, kind=int64)
        if (nitems <= 0) return

        call parquet_writer_whole_column_mask(writer, name, nrows, row_mask)
        elem_mask = parquet_mask_expand_block(row_mask, asize)
        values_c = reshape(pack(values, spread(row_mask, 1, asize)), [asize, count(row_mask, kind=int64)])
        nrows = count(row_mask, kind=int64)
        nitems = size(values_c, kind=int64)
        call parquet_check_row_count(writer, name, nrows)

        item_len = len(values(1, 1))
        allocate(packed(item_len * nitems))

        k = 0_int64
        do i = 1_int64, nrows
            do j = 1_int64, asize
                do l = 1, item_len
                    k = k + 1_int64
                    packed(k) = achar(iachar(values_c(j, i)(l:l)), kind=c_char)
                end do
            end do
        end do

        if (present(is_valid)) then
            valid_flat = pack(reshape(is_valid, [size(is_valid, kind=int64)]), elem_mask)
            call parquet_check_protected(writer, name, valid_flat)
            call parquet_check_qc_miss(writer, name, valid_flat)
            call parquet_check_qc_string(writer, name, reshape(values_c, [size(values_c, kind=int64)]), valid_flat)
            call parquet_make_valid_buf_write(valid_flat, valid_buf, valid_ptr)
        else
            call parquet_check_qc_string(writer, name, reshape(values_c, [size(values_c, kind=int64)]), &
                spread(.true., 1, size(values_c, kind=int64)))
            call parquet_make_valid_buf_write(valid_buf=valid_buf, valid_ptr=valid_ptr)
        end if

        call parquet_resolve_output_name(writer, name, outname)
        call parquet_append_string_array_column(&
            writer%handle, &
            trim(outname)//char(0), &
            packed, &
            int(item_len, kind=c_long_long), &
            nrows, &
            asize, &
            valid_ptr )
    end procedure parquet_write_string_matrix_column
    module procedure parquet_write_string_column_compact
        integer(int64) :: nrows, nchars, i
        integer :: idx
        type(c_ptr) :: offsets_ptr, data_ptr, validity_ptr
        logical :: has_validity
        logical, allocatable :: is_valid_flat(:)
        character(len=:), allocatable :: ctx !! writer_context_suffix scratch.
        character(len=:), allocatable :: outname !! parquet_resolve_output_name scratch.
        logical, allocatable :: row_mask(:)
        type(parquet_string_column) :: values_c
        character(len=:), allocatable :: item_scratch
        call check_writer_open(writer)

        if (writer%is_schema_enforced) then
            idx = parquet_get_defined_column_index(writer, name)
            call writer_context_suffix(writer, ctx)
            if (idx == 0) error stop &
                "parquet_write_column: column not defined in parquet_open_writer: " // trim(name) // ctx
            if (.not. writer%all_columns(idx)%is_set) return
        end if

        call parquet_assert_column_type(writer, name, "string")

        if (writer%is_schema_enforced) then
            if (parquet_get_column_col_size(writer, name) /= 1) then
                call writer_context_suffix(writer, ctx)
                error stop "parquet_write_column: a parquet_string_column write requires a scalar " // &
                    "(col_size=1) column: " // trim(name) // ctx
            end if
        end if

        if (.not. parquet_is_column_enabled(writer, name)) return
        call parquet_mark_column_written(writer, name)

        nrows = values%size()
        if (nrows <= 0_int64) return

        call parquet_writer_whole_column_mask(writer, name, nrows, row_mask)
        do i = 1_int64, nrows
            if (.not. row_mask(i)) cycle
            if (values%is_null(i)) then
                call values_c%append_null()
            else
                call values%get(i, item_scratch, allow_null=.true.)
                call values_c%append_string(item_scratch)
            end if
        end do
        nrows = count(row_mask, kind=int64)
        call parquet_check_row_count(writer, name, nrows)

        if (values_c%null_count() > 0_int64) then
            allocate(is_valid_flat(nrows))
            do i = 1_int64, nrows
                is_valid_flat(i) = .not. values_c%is_null(i)
            end do
            call parquet_check_protected(writer, name, is_valid_flat)
            call parquet_check_qc_miss(writer, name, is_valid_flat)
        end if
        call parquet_check_qc_string_compact(writer, name, values_c)

        call values_c%raw_buffers(offsets_ptr, data_ptr, validity_ptr, nrows, nchars, has_validity)
        call parquet_resolve_output_name(writer, name, outname)
        call parquet_append_string_column_buffers(&
            writer%handle, &
            trim(outname)//char(0), &
            nrows, &
            nchars, &
            offsets_ptr, &
            data_ptr, &
            validity_ptr )
    end procedure parquet_write_string_column_compact
    module procedure parquet_write_string_column_chunk
        character(kind=c_char), allocatable :: packed(:)
        integer(int64) :: i, k, nrows, asize, nitems
        integer :: j, item_len, idx, max_item_len, max_string_len
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        character(len=:), allocatable :: ctx !! writer_context_suffix scratch.
        character(len=:), allocatable :: outname !! parquet_resolve_output_name scratch.
        logical, allocatable :: row_mask(:), elem_mask(:), is_valid_c(:)
        character(len=:), allocatable :: values_c(:)
        call check_writer_open(writer)

        if (writer%is_schema_enforced) then
            idx = parquet_get_defined_column_index(writer, name)
            call writer_context_suffix(writer, ctx)
            if (idx == 0) error stop &
                "parquet_write_column_chunk: column not defined in parquet_open_writer: " // &
                trim(name) // ctx
            if (.not. writer%all_columns(idx)%is_set) return
        end if

        call parquet_assert_column_type_exact(writer, name, "string")

        if (.not. parquet_is_column_enabled(writer, name)) return

        asize = int(parquet_get_column_col_size(writer, name), kind=int64)
        nitems = size(values, kind=int64)
        if (nitems <= 0) return
        call writer_context_suffix(writer, ctx)
        if (mod(nitems, asize) /= 0) error stop &
            "parquet_write_string_column_chunk: values size is not divisible by col_size for column " // &
            trim(name) // ctx

        if (writer%is_schema_enforced) then
            call parquet_resolve_or_check_array_size(writer, name, idx, len(values(1)))
            max_string_len = max(1, writer%all_columns(idx)%array_size)
            max_item_len = maxval([(len_trim(values(i)), i=1_int64,nitems)])
            if (max_item_len > max_string_len) then
                error stop "parquet_write_string_column_chunk: string length exceeds declared array_size " // &
                    "for column: " // trim(name)
            end if
        end if

        nrows = nitems / asize
        call parquet_check_row_group_row_count(writer, name, nrows, row_mask)
        elem_mask = parquet_mask_expand_block(row_mask, asize)
        values_c = pack(values, elem_mask)
        nrows = count(row_mask, kind=int64)
        nitems = size(values_c, kind=int64)

        item_len = len(values(1))
        allocate(packed(item_len*nitems))

        k = 0_int64
        do i = 1_int64, nitems
            do j = 1, item_len
                k = k + 1_int64
                packed(k) = achar(iachar(values_c(i)(j:j)), kind=c_char)
            end do
        end do

        if (present(is_valid)) then
            is_valid_c = pack(is_valid, elem_mask)
            call parquet_check_protected(writer, name, is_valid_c)
            call parquet_check_qc_miss(writer, name, is_valid_c)
            call parquet_check_qc_string(writer, name, values_c, is_valid_c)
        else
            call parquet_check_qc_string(writer, name, values_c, spread(.true., 1, size(values_c, kind=int64)))
        end if
        call parquet_make_valid_buf_write(is_valid_c, valid_buf, valid_ptr)
        call parquet_chunk_mark_written_if_first(writer, name)

        if (nrows > 0) then
            if (asize == 1) then
                call parquet_resolve_output_name(writer, name, outname)
                call parquet_append_string_column_chunk(&
                    writer%handle, &
                    trim(outname)//char(0), &
                    packed, &
                    int(item_len, kind=c_long_long), &
                    valid_ptr )
            else
                call parquet_resolve_output_name(writer, name, outname)
                call parquet_append_string_array_column_chunk(&
                    writer%handle, &
                    trim(outname)//char(0), &
                    packed, &
                    int(item_len, kind=c_long_long), &
                    asize, &
                    valid_ptr )
            end if
        end if
    end procedure parquet_write_string_column_chunk
    module procedure parquet_write_string_matrix_column_chunk
        character(kind=c_char), allocatable :: packed(:)
        integer(int64) :: i, j, k, nrows, asize, nitems
        integer :: l, item_len, idx, max_item_len, max_string_len
        character(len=:), allocatable :: ctx !! writer_context_suffix scratch.
        character(len=:), allocatable :: outname !! parquet_resolve_output_name scratch.
        logical, allocatable :: valid_flat(:)
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        logical, allocatable :: row_mask(:), elem_mask(:)
        character(len=:), allocatable :: values_c(:,:)
        call check_writer_open(writer)

        asize = size(values, 1, kind=int64)
        nrows = size(values, 2, kind=int64)

        if (writer%is_schema_enforced) then
            idx = parquet_get_defined_column_index(writer, name)
            call writer_context_suffix(writer, ctx)
            if (idx == 0) error stop &
                "parquet_write_column_chunk: column not defined in parquet_open_writer: " // &
                trim(name) // ctx
            if (.not. writer%all_columns(idx)%is_set) return
            call parquet_resolve_or_check_col_size(writer, name, idx, asize, "parquet_write_column_chunk")

            call parquet_resolve_or_check_array_size(writer, name, idx, len(values(1, 1)))
            max_string_len = max(1, writer%all_columns(idx)%array_size)
            max_item_len = maxval(len_trim(values))
            if (max_item_len > max_string_len) then
                error stop "parquet_write_string_matrix_column_chunk: string length exceeds declared " // &
                    "array_size for column: " // trim(name)
            end if
        end if

        call parquet_assert_column_type_exact(writer, name, "string")

        if (.not. parquet_is_column_enabled(writer, name)) return

        nitems = size(values, kind=int64)
        if (nitems <= 0) return
        call parquet_check_row_group_row_count(writer, name, nrows, row_mask)
        elem_mask = parquet_mask_expand_block(row_mask, asize)
        values_c = reshape(pack(values, spread(row_mask, 1, asize)), [asize, count(row_mask, kind=int64)])
        nrows = count(row_mask, kind=int64)
        nitems = size(values_c, kind=int64)

        item_len = len(values(1, 1))
        allocate(packed(item_len * nitems))

        k = 0_int64
        do i = 1_int64, nrows
            do j = 1_int64, asize
                do l = 1, item_len
                    k = k + 1_int64
                    packed(k) = achar(iachar(values_c(j, i)(l:l)), kind=c_char)
                end do
            end do
        end do

        if (present(is_valid)) then
            valid_flat = pack(reshape(is_valid, [size(is_valid, kind=int64)]), elem_mask)
            call parquet_check_protected(writer, name, valid_flat)
            call parquet_check_qc_miss(writer, name, valid_flat)
            call parquet_check_qc_string(writer, name, reshape(values_c, [size(values_c, kind=int64)]), valid_flat)
            call parquet_make_valid_buf_write(valid_flat, valid_buf, valid_ptr)
        else
            call parquet_check_qc_string(writer, name, reshape(values_c, [size(values_c, kind=int64)]), &
                spread(.true., 1, size(values_c, kind=int64)))
            call parquet_make_valid_buf_write(valid_buf=valid_buf, valid_ptr=valid_ptr)
        end if
        call parquet_chunk_mark_written_if_first(writer, name)

        call parquet_resolve_output_name(writer, name, outname)
        if (nrows > 0) call parquet_append_string_array_column_chunk(&
            writer%handle, &
            trim(outname)//char(0), &
            packed, &
            int(item_len, kind=c_long_long), &
            asize, &
            valid_ptr )
    end procedure parquet_write_string_matrix_column_chunk
    module procedure parquet_write_string_column_chunk_compact
        integer(int64) :: nrows, nchars, i
        integer :: idx
        type(c_ptr) :: offsets_ptr, data_ptr, validity_ptr
        logical :: has_validity
        logical, allocatable :: is_valid_flat(:)
        character(len=:), allocatable :: ctx !! writer_context_suffix scratch.
        character(len=:), allocatable :: outname !! parquet_resolve_output_name scratch.
        logical, allocatable :: row_mask(:)
        type(parquet_string_column) :: values_c
        character(len=:), allocatable :: item_scratch
        call check_writer_open(writer)

        if (writer%is_schema_enforced) then
            idx = parquet_get_defined_column_index(writer, name)
            call writer_context_suffix(writer, ctx)
            if (idx == 0) error stop &
                "parquet_write_column_chunk: column not defined in parquet_open_writer: " // &
                trim(name) // ctx
            if (.not. writer%all_columns(idx)%is_set) return
        end if

        call parquet_assert_column_type_exact(writer, name, "string")

        if (writer%is_schema_enforced) then
            if (parquet_get_column_col_size(writer, name) /= 1) then
                call writer_context_suffix(writer, ctx)
                error stop "parquet_write_column_chunk: a parquet_string_column write requires a " // &
                    "scalar (col_size=1) column: " // trim(name) // ctx
            end if
        end if

        if (.not. parquet_is_column_enabled(writer, name)) return

        nrows = values%size()
        if (nrows <= 0_int64) return
        call parquet_check_row_group_row_count(writer, name, nrows, row_mask)

        do i = 1_int64, nrows
            if (.not. row_mask(i)) cycle
            if (values%is_null(i)) then
                call values_c%append_null()
            else
                call values%get(i, item_scratch, allow_null=.true.)
                call values_c%append_string(item_scratch)
            end if
        end do
        nrows = count(row_mask, kind=int64)

        if (values_c%null_count() > 0_int64) then
            allocate(is_valid_flat(nrows))
            do i = 1_int64, nrows
                is_valid_flat(i) = .not. values_c%is_null(i)
            end do
            call parquet_check_protected(writer, name, is_valid_flat)
            call parquet_check_qc_miss(writer, name, is_valid_flat)
        end if
        call parquet_check_qc_string_compact(writer, name, values_c)
        call parquet_chunk_mark_written_if_first(writer, name)

        call values_c%raw_buffers(offsets_ptr, data_ptr, validity_ptr, nrows, nchars, has_validity)
        call parquet_resolve_output_name(writer, name, outname)
        if (nrows > 0) call parquet_write_string_column_chunk_buffers(&
            writer%handle, &
            trim(outname)//char(0), &
            nrows, &
            nchars, &
            offsets_ptr, &
            data_ptr, &
            validity_ptr )
    end procedure parquet_write_string_column_chunk_compact

end submodule parquet_write_string
