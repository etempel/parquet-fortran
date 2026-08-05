!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!> Numeric write specifics (int32/int64/float32/float64/logical, scalar and
!> matrix, whole-column and row-group-chunked): bodies of the module
!> procedures declared in parquet_core.f90's interface block, plus the
!> numeric-only private helpers (schema-type widening/narrowing, append,
!> qc: enforcement, qc: numeric-bound satisfaction, qc: real-value text
!> formatting) they depend on.
submodule (parquet_core:parquet_write) parquet_write_numeric
    implicit none
contains

    !> Narrows int64 `src` to int32, error stopping if any value is outside
    !> int32's representable range. Used when a schema declares a column
    !> int32 but the caller's parquet_write_column values are int64.
    function parquet_narrow_int64_to_int32(name, src) result(dst)
        character(len=*), intent(in) :: name !! column name, named only in the error-stop message.
        integer(int64), intent(in) :: src(:) !! values to narrow.
        integer(int32), allocatable :: dst(:) !! narrowed values.
        integer(int64) :: i

        allocate(dst(size(src, kind=int64)))
        do i = 1_int64, size(src, kind=int64)
            if (src(i) < -huge(0_int32) - 1_int64 .or. src(i) > huge(0_int32)) then
                error stop "parquet_write_column: int64 value out of int32 range for column " // trim(name)
            end if
            dst(i) = int(src(i), kind=int32)
        end do
    end function parquet_narrow_int64_to_int32
    !> Converts float64 `src` to int32, error stopping if any value is
    !> non-integral or outside int32's representable range. Used when a
    !> schema declares a column int32 but the caller's parquet_write_column
    !> values are float32/float64.
    function parquet_float64_to_int32(name, src) result(dst)
        character(len=*), intent(in) :: name !! column name, named only in the error-stop message.
        real(real64), intent(in) :: src(:) !! values to convert.
        integer(int32), allocatable :: dst(:) !! converted values.
        integer(int64) :: i

        allocate(dst(size(src, kind=int64)))
        do i = 1_int64, size(src, kind=int64)
            if (src(i) /= anint(src(i))) then
                error stop "parquet_write_column: non-integral float value written to int column " // trim(name)
            end if
            if (src(i) < -real(huge(0_int32), real64) - 1.0_real64 .or. src(i) > real(huge(0_int32), real64)) then
                error stop "parquet_write_column: float value out of int32 range for column " // trim(name)
            end if
            dst(i) = int(src(i), kind=int32)
        end do
    end function parquet_float64_to_int32
    !> Converts float64 `src` to int64, error stopping if any value is
    !> non-integral or outside int64's representable range. Used when a
    !> schema declares a column int64 but the caller's parquet_write_column
    !> values are float32/float64.
    function parquet_float64_to_int64(name, src) result(dst)
        character(len=*), intent(in) :: name !! column name, named only in the error-stop message.
        real(real64), intent(in) :: src(:) !! values to convert.
        integer(int64), allocatable :: dst(:) !! converted values.
        integer(int64) :: i

        allocate(dst(size(src, kind=int64)))
        do i = 1_int64, size(src, kind=int64)
            if (src(i) /= anint(src(i))) then
                error stop "parquet_write_column: non-integral float value written to int column " // trim(name)
            end if
            if (src(i) < -real(huge(0_int64), real64) .or. src(i) >= real(huge(0_int64), real64)) then
                error stop "parquet_write_column: float value out of int64 range for column " // trim(name)
            end if
            dst(i) = int(src(i), kind=int64)
        end do
    end function parquet_float64_to_int64
    !> Appends an int32 column to the C++ writer, widening `values` first if
    !> the schema declares this column as a wider/different numeric type
    !> (int64/float32/float64); calls the matching parquet_append_*_column
    !> C binding either way, so the data actually stored always matches the
    !> schema's declared type.
    subroutine parquet_append_as_schema_int32(writer, name, values, nrows, asize, valid_ptr)
        type(parquet_writer), intent(in) :: writer !! open writer.
        character(len=*), intent(in) :: name !! column name.
        integer(int32), intent(in) :: values(:) !! values as passed to parquet_write_column.
        integer(c_long_long), intent(in) :: nrows !! row count.
        integer(c_long_long), intent(in) :: asize !! vector-column element count (1 for a scalar column).
        type(c_ptr), intent(in) :: valid_ptr !! validity buffer, or c_null_ptr.
        character(len=:), allocatable :: schema_type
        integer(int64), allocatable :: i64values(:)
        real(real32), allocatable :: f32values(:)
        real(real64), allocatable :: f64values(:)
        character(len=:), allocatable :: outname !! parquet_resolve_output_name scratch.

        call parquet_get_schema_type(writer, name, schema_type)
        select case (schema_type)
        case ("int64")
            allocate(i64values(size(values, kind=int64)))
            i64values = int(values, kind=int64)
            call parquet_resolve_output_name(writer, name, outname)
            call parquet_append_int64_column(writer%handle, &
                trim(outname)//char(0), i64values, nrows, asize, valid_ptr)
        case ("float32")
            allocate(f32values(size(values, kind=int64)))
            f32values = real(values, kind=real32)
            call parquet_resolve_output_name(writer, name, outname)
            call parquet_append_float32_column(writer%handle, &
                trim(outname)//char(0), f32values, nrows, asize, valid_ptr)
        case ("float64")
            allocate(f64values(size(values, kind=int64)))
            f64values = real(values, kind=real64)
            call parquet_resolve_output_name(writer, name, outname)
            call parquet_append_float64_column(writer%handle, &
                trim(outname)//char(0), f64values, nrows, asize, valid_ptr)
        case default
            call parquet_resolve_output_name(writer, name, outname)
            call parquet_append_int32_column(writer%handle, &
                trim(outname)//char(0), values, nrows, asize, valid_ptr)
        end select
    end subroutine parquet_append_as_schema_int32
    !> Appends an int64 column to the C++ writer, narrowing/widening `values`
    !> first if the schema declares this column as a different numeric type
    !> (int32/float32/float64); see parquet_append_as_schema_int32.
    subroutine parquet_append_as_schema_int64(writer, name, values, nrows, asize, valid_ptr)
        type(parquet_writer), intent(in) :: writer !! open writer.
        character(len=*), intent(in) :: name !! column name.
        integer(int64), intent(in) :: values(:) !! values as passed to parquet_write_column.
        integer(c_long_long), intent(in) :: nrows !! row count.
        integer(c_long_long), intent(in) :: asize !! vector-column element count (1 for a scalar column).
        type(c_ptr), intent(in) :: valid_ptr !! validity buffer, or c_null_ptr.
        character(len=:), allocatable :: schema_type
        integer(int32), allocatable :: i32values(:)
        real(real32), allocatable :: f32values(:)
        real(real64), allocatable :: f64values(:)
        character(len=:), allocatable :: outname !! parquet_resolve_output_name scratch.

        call parquet_get_schema_type(writer, name, schema_type)
        select case (schema_type)
        case ("int32")
            i32values = parquet_narrow_int64_to_int32(name, values)
            call parquet_resolve_output_name(writer, name, outname)
            call parquet_append_int32_column(writer%handle, &
                trim(outname)//char(0), i32values, nrows, asize, valid_ptr)
        case ("float32")
            allocate(f32values(size(values, kind=int64)))
            f32values = real(values, kind=real32)
            call parquet_resolve_output_name(writer, name, outname)
            call parquet_append_float32_column(writer%handle, &
                trim(outname)//char(0), f32values, nrows, asize, valid_ptr)
        case ("float64")
            allocate(f64values(size(values, kind=int64)))
            f64values = real(values, kind=real64)
            call parquet_resolve_output_name(writer, name, outname)
            call parquet_append_float64_column(writer%handle, &
                trim(outname)//char(0), f64values, nrows, asize, valid_ptr)
        case default
            call parquet_resolve_output_name(writer, name, outname)
            call parquet_append_int64_column(writer%handle, &
                trim(outname)//char(0), values, nrows, asize, valid_ptr)
        end select
    end subroutine parquet_append_as_schema_int64
    !> Appends a float32 column to the C++ writer, converting `values` first
    !> if the schema declares this column as a different numeric type
    !> (int32/int64/float64); see parquet_append_as_schema_int32.
    subroutine parquet_append_as_schema_float32(writer, name, values, nrows, asize, valid_ptr)
        type(parquet_writer), intent(in) :: writer !! open writer.
        character(len=*), intent(in) :: name !! column name.
        real(real32), intent(in) :: values(:) !! values as passed to parquet_write_column.
        integer(c_long_long), intent(in) :: nrows !! row count.
        integer(c_long_long), intent(in) :: asize !! vector-column element count (1 for a scalar column).
        type(c_ptr), intent(in) :: valid_ptr !! validity buffer, or c_null_ptr.
        character(len=:), allocatable :: schema_type
        integer(int32), allocatable :: i32values(:)
        integer(int64), allocatable :: i64values(:)
        real(real64), allocatable :: f64values(:)
        character(len=:), allocatable :: outname !! parquet_resolve_output_name scratch.

        call parquet_get_schema_type(writer, name, schema_type)
        select case (schema_type)
        case ("int32")
            i32values = parquet_float64_to_int32(name, real(values, kind=real64))
            call parquet_resolve_output_name(writer, name, outname)
            call parquet_append_int32_column(writer%handle, &
                trim(outname)//char(0), i32values, nrows, asize, valid_ptr)
        case ("int64")
            i64values = parquet_float64_to_int64(name, real(values, kind=real64))
            call parquet_resolve_output_name(writer, name, outname)
            call parquet_append_int64_column(writer%handle, &
                trim(outname)//char(0), i64values, nrows, asize, valid_ptr)
        case ("float64")
            allocate(f64values(size(values, kind=int64)))
            f64values = real(values, kind=real64)
            call parquet_resolve_output_name(writer, name, outname)
            call parquet_append_float64_column(writer%handle, &
                trim(outname)//char(0), f64values, nrows, asize, valid_ptr)
        case default
            call parquet_resolve_output_name(writer, name, outname)
            call parquet_append_float32_column(writer%handle, &
                trim(outname)//char(0), values, nrows, asize, valid_ptr)
        end select
    end subroutine parquet_append_as_schema_float32
    !> Appends a float64 column to the C++ writer, converting `values` first
    !> if the schema declares this column as a different numeric type
    !> (int32/int64/float32); see parquet_append_as_schema_int32.
    subroutine parquet_append_as_schema_float64(writer, name, values, nrows, asize, valid_ptr)
        type(parquet_writer), intent(in) :: writer !! open writer.
        character(len=*), intent(in) :: name !! column name.
        real(real64), intent(in) :: values(:) !! values as passed to parquet_write_column.
        integer(c_long_long), intent(in) :: nrows !! row count.
        integer(c_long_long), intent(in) :: asize !! vector-column element count (1 for a scalar column).
        type(c_ptr), intent(in) :: valid_ptr !! validity buffer, or c_null_ptr.
        character(len=:), allocatable :: schema_type
        integer(int32), allocatable :: i32values(:)
        integer(int64), allocatable :: i64values(:)
        real(real32), allocatable :: f32values(:)
        character(len=:), allocatable :: outname !! parquet_resolve_output_name scratch.

        call parquet_get_schema_type(writer, name, schema_type)
        select case (schema_type)
        case ("int32")
            i32values = parquet_float64_to_int32(name, values)
            call parquet_resolve_output_name(writer, name, outname)
            call parquet_append_int32_column(writer%handle, &
                trim(outname)//char(0), i32values, nrows, asize, valid_ptr)
        case ("int64")
            i64values = parquet_float64_to_int64(name, values)
            call parquet_resolve_output_name(writer, name, outname)
            call parquet_append_int64_column(writer%handle, &
                trim(outname)//char(0), i64values, nrows, asize, valid_ptr)
        case ("float32")
            allocate(f32values(size(values, kind=int64)))
            f32values = real(values, kind=real32)
            call parquet_resolve_output_name(writer, name, outname)
            call parquet_append_float32_column(writer%handle, &
                trim(outname)//char(0), f32values, nrows, asize, valid_ptr)
        case default
            call parquet_resolve_output_name(writer, name, outname)
            call parquet_append_float64_column(writer%handle, &
                trim(outname)//char(0), values, nrows, asize, valid_ptr)
        end select
    end subroutine parquet_append_as_schema_float64
    !> True if `value` satisfies `bound` under the min:/max: operator `op`
    !> (">=", "<=", ">", "<"); any other `op` is treated as "no constraint"
    !> (always .true.).
    logical function parquet_qc_numeric_satisfies(value, bound, op) result(ok)
        real(real64), intent(in) :: value !! value being checked.
        real(real64), intent(in) :: bound !! declared qc: min:/max: bound.
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
    end function parquet_qc_numeric_satisfies
    !> Formats `value` for a qc-violation WARNING message: as a bare integer
    !> if it's exactly integral and within +/-1e15, else with 7 significant digits.
    subroutine parquet_qc_format_real(value, text)
        real(real64), intent(in) :: value !! value to format.
        character(len=:), allocatable, intent(out) :: text !! formatted, trimmed text.
        character(len=64) :: buf

        if (value == anint(value) .and. abs(value) < 1.0e15_real64) then
            write(buf, '(i0)') nint(value, kind=int64)
        else
            write(buf, '(g0.7)') value
        end if
        text = trim(adjustl(buf))

        ! g0 editing's leading zero for |value|<1 is implementation-defined: gfortran writes
        ! "0.5000000", but ifort/ifx write ".5000000" -- normalize so callers/tests can rely on
        ! a leading zero regardless of compiler. Not coverable under gfortran, this project's
        ! only tested compiler (confirmed: gfortran always emits the leading zero, checked
        ! across 0.5, -0.5, 0.00001, -0.00001, 1e20 and -1e-20) -- kept for ifort/ifx builds.
        if (len(text) > 0) then
            if (text(1:1) == ".") then
                text = "0" // text ! GCOVR_EXCL_LINE
            else if (len(text) > 1) then
                if (text(1:2) == "-.") text = "-0" // text(2:)
            end if
        end if
    end subroutine parquet_qc_format_real
    !> If writer%qc is set and the column has a schema-declared qc: min:
    !> and/or max:, checks every element of `values64` where `is_valid_flat`
    !> is .true. against those bounds (absent is_valid means every element
    !> counts, matching the write-side is_valid convention elsewhere) and
    !> prints a single WARNING to stdout naming the column, its declared
    !> bound(s), the observed data range among valid elements, and how many
    !> of them violate at least one bound. Never errors -- writing proceeds
    !> regardless. No-op for a schema-less writer, a column without qc:, or
    !> when there are no valid elements to check at all.
    subroutine parquet_check_qc_numeric(writer, name, values64, is_valid_flat)
        type(parquet_writer), intent(in) :: writer !! open (schema-enforced) writer.
        character(len=*), intent(in) :: name !! numeric column name.
        real(real64), intent(in) :: values64(:) !! flattened column values, widened to real64.
        logical, intent(in) :: is_valid_flat(:) !! flattened validity mask (.true. => checked).
        integer :: idx
        integer(int64) :: i, n_valid, n_violate
        real(real64) :: min_bound, max_bound, data_min, data_max
        logical :: any_valid, have_min_bound, have_max_bound, ok
        character(len=:), allocatable :: bounds_desc, fmt_num, fmt_num2, fmt_int, fmt_int2

        if (.not. writer%qc) return
        if (.not. writer%is_schema_enforced) return
        idx = parquet_get_defined_column_index(writer, name)
        if (idx == 0) return
        if (.not. (writer%all_columns(idx)%has_qc_min .or. writer%all_columns(idx)%has_qc_max)) return

        have_min_bound = .false.
        have_max_bound = .false.
        if (writer%all_columns(idx)%has_qc_min) then
            have_min_bound = parquet_qc_numeric_bound( &
                writer%all_columns(idx)%qc_min_raw, writer%all_columns(idx)%data_type, min_bound)
        end if
        if (writer%all_columns(idx)%has_qc_max) then
            have_max_bound = parquet_qc_numeric_bound( &
                writer%all_columns(idx)%qc_max_raw, writer%all_columns(idx)%data_type, max_bound)
        end if
        if (.not. (have_min_bound .or. have_max_bound)) return

        any_valid = .false.
        n_valid = 0_int64
        n_violate = 0_int64
        do i = 1_int64, size(values64, kind=int64)
            if (.not. is_valid_flat(i)) cycle
            n_valid = n_valid + 1
            if (.not. any_valid) then
                data_min = values64(i)
                data_max = values64(i)
                any_valid = .true.
            else
                data_min = min(data_min, values64(i))
                data_max = max(data_max, values64(i))
            end if

            ok = .true.
            if (have_min_bound) ok = ok .and. &
                parquet_qc_numeric_satisfies(values64(i), min_bound, writer%all_columns(idx)%qc_min_op)
            if (have_max_bound) ok = ok .and. &
                parquet_qc_numeric_satisfies(values64(i), max_bound, writer%all_columns(idx)%qc_max_op)
            if (.not. ok) n_violate = n_violate + 1
        end do
        if (.not. any_valid .or. n_violate == 0) return

        bounds_desc = ""
        if (have_min_bound) then
            call parquet_qc_format_real(min_bound, fmt_num)
            bounds_desc = "min " // trim(writer%all_columns(idx)%qc_min_op) // " " // fmt_num
        end if
        if (have_max_bound) then
            if (len_trim(bounds_desc) > 0) bounds_desc = bounds_desc // ", "
            call parquet_qc_format_real(max_bound, fmt_num)
            bounds_desc = bounds_desc // "max " // trim(writer%all_columns(idx)%qc_max_op) // " " // fmt_num
        end if

        call parquet_qc_format_real(data_min, fmt_num)
        call parquet_qc_format_real(data_max, fmt_num2)
        call parquet_qc_format_int(n_violate, fmt_int)
        call parquet_qc_format_int(n_valid, fmt_int2)
        call parquet_emit_warning("qc violation for column '" // trim(name) // "': declared " // bounds_desc // &
            ", data range [" // fmt_num // ", " // fmt_num2 // "], " // &
            fmt_int // " of " // fmt_int2 // " valid element(s) out of range")
    end subroutine parquet_check_qc_numeric
    module procedure parquet_write_int32_column
        integer :: idx
        integer(int64) :: asize, nrows
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        character(len=:), allocatable :: ctx !! writer_context_suffix scratch.
        logical, allocatable :: row_mask(:), elem_mask(:), is_valid_c(:)
        integer(int32), allocatable :: values_c(:)
        call check_writer_open(writer)

        if (writer%is_schema_enforced) then
            idx = parquet_get_defined_column_index(writer, name)
            call writer_context_suffix(writer, ctx)
            if (idx == 0) error stop &
                "parquet_write_column: column not defined in parquet_open_writer: " // trim(name) // ctx
            if (.not. writer%all_columns(idx)%is_set) return
        end if

        call parquet_assert_column_type(writer, name, "int32")

        if (.not. parquet_is_column_enabled(writer, name)) return
        call parquet_mark_column_written(writer, name)

        asize = int(parquet_get_column_col_size(writer, name), kind=int64)
        call writer_context_suffix(writer, ctx)
        if (mod(size(values, kind=int64), asize) /= 0) error stop &
            "parquet_write_int32_column: values size is not divisible by col_size for column " // &
            trim(name) // ctx
        nrows = size(values, kind=int64) / asize

        call parquet_writer_whole_column_mask(writer, name, nrows, row_mask)
        elem_mask = parquet_mask_expand_block(row_mask, asize)
        values_c = pack(values, elem_mask)
        nrows = count(row_mask, kind=int64)

        if (present(is_valid)) then
            is_valid_c = pack(is_valid, elem_mask)
            call parquet_check_protected(writer, name, is_valid_c)
            call parquet_check_qc_miss(writer, name, is_valid_c)
            if (writer%qc .and. writer%is_schema_enforced) then
                call parquet_check_qc_numeric(writer, name, real(values_c, kind=real64), is_valid_c)
            end if
        else
            if (writer%qc .and. writer%is_schema_enforced) then
                call parquet_check_qc_numeric(writer, name, real(values_c, kind=real64), &
                    spread(.true., 1, size(values_c, kind=int64)))
            end if
        end if
        call parquet_make_valid_buf_write(is_valid_c, valid_buf, valid_ptr)
        call parquet_check_row_count(writer, name, nrows)

        call parquet_append_as_schema_int32(writer, name, values_c, nrows, asize, valid_ptr)
    end procedure parquet_write_int32_column
    module procedure parquet_write_int32_matrix_column
        integer :: idx
        integer(int64) :: asize, nrows
        integer(int32), allocatable :: packed(:)
        character(len=:), allocatable :: ctx !! writer_context_suffix scratch.
        logical, allocatable :: valid_flat(:)
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        logical, allocatable :: row_mask(:), elem_mask(:)
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
        end if

        call parquet_assert_column_type(writer, name, "int32")

        if (.not. parquet_is_column_enabled(writer, name)) return
        call parquet_mark_column_written(writer, name)

        allocate(packed(size(values, kind=int64)))
        packed = reshape(values, [size(values, kind=int64)])

        call parquet_writer_whole_column_mask(writer, name, nrows, row_mask)
        elem_mask = parquet_mask_expand_block(row_mask, asize)
        packed = pack(packed, elem_mask)
        nrows = count(row_mask, kind=int64)

        if (present(is_valid)) then
            valid_flat = pack(reshape(is_valid, [size(is_valid, kind=int64)]), elem_mask)
            call parquet_check_protected(writer, name, valid_flat)
            call parquet_check_qc_miss(writer, name, valid_flat)
            if (writer%qc .and. writer%is_schema_enforced) then
                call parquet_check_qc_numeric(writer, name, real(packed, kind=real64), valid_flat)
            end if
            call parquet_make_valid_buf_write(valid_flat, valid_buf, valid_ptr)
        else
            if (writer%qc .and. writer%is_schema_enforced) then
                call parquet_check_qc_numeric(writer, name, real(packed, kind=real64), &
                    spread(.true., 1, size(packed, kind=int64)))
            end if
            call parquet_make_valid_buf_write(valid_buf=valid_buf, valid_ptr=valid_ptr)
        end if
        call parquet_check_row_count(writer, name, nrows)

        call parquet_append_as_schema_int32(writer, name, packed, nrows, asize, valid_ptr)
    end procedure parquet_write_int32_matrix_column
    module procedure parquet_write_int64_column
        integer :: idx
        integer(int64) :: asize, nrows
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        character(len=:), allocatable :: ctx !! writer_context_suffix scratch.
        logical, allocatable :: row_mask(:), elem_mask(:), is_valid_c(:)
        integer(int64), allocatable :: values_c(:)
        call check_writer_open(writer)

        if (writer%is_schema_enforced) then
            idx = parquet_get_defined_column_index(writer, name)
            call writer_context_suffix(writer, ctx)
            if (idx == 0) error stop &
                "parquet_write_column: column not defined in parquet_open_writer: " // trim(name) // ctx
            if (.not. writer%all_columns(idx)%is_set) return
        end if

        call parquet_assert_column_type(writer, name, "int64")

        if (.not. parquet_is_column_enabled(writer, name)) return
        call parquet_mark_column_written(writer, name)

        asize = int(parquet_get_column_col_size(writer, name), kind=int64)
        call writer_context_suffix(writer, ctx)
        if (mod(size(values, kind=int64), asize) /= 0) error stop &
            "parquet_write_int64_column: values size is not divisible by col_size for column " // &
            trim(name) // ctx
        nrows = size(values, kind=int64) / asize

        call parquet_writer_whole_column_mask(writer, name, nrows, row_mask)
        elem_mask = parquet_mask_expand_block(row_mask, asize)
        values_c = pack(values, elem_mask)
        nrows = count(row_mask, kind=int64)

        if (present(is_valid)) then
            is_valid_c = pack(is_valid, elem_mask)
            call parquet_check_protected(writer, name, is_valid_c)
            call parquet_check_qc_miss(writer, name, is_valid_c)
            if (writer%qc .and. writer%is_schema_enforced) then
                call parquet_check_qc_numeric(writer, name, real(values_c, kind=real64), is_valid_c)
            end if
        else
            if (writer%qc .and. writer%is_schema_enforced) then
                call parquet_check_qc_numeric(writer, name, real(values_c, kind=real64), &
                    spread(.true., 1, size(values_c, kind=int64)))
            end if
        end if
        call parquet_make_valid_buf_write(is_valid_c, valid_buf, valid_ptr)
        call parquet_check_row_count(writer, name, nrows)

        call parquet_append_as_schema_int64(writer, name, values_c, nrows, asize, valid_ptr)
    end procedure parquet_write_int64_column
    module procedure parquet_write_int64_matrix_column
        integer :: idx
        integer(int64) :: asize, nrows
        integer(int64), allocatable :: packed(:)
        character(len=:), allocatable :: ctx !! writer_context_suffix scratch.
        logical, allocatable :: valid_flat(:)
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        logical, allocatable :: row_mask(:), elem_mask(:)
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
        end if

        call parquet_assert_column_type(writer, name, "int64")

        if (.not. parquet_is_column_enabled(writer, name)) return
        call parquet_mark_column_written(writer, name)

        allocate(packed(size(values, kind=int64)))
        packed = reshape(values, [size(values, kind=int64)])

        call parquet_writer_whole_column_mask(writer, name, nrows, row_mask)
        elem_mask = parquet_mask_expand_block(row_mask, asize)
        packed = pack(packed, elem_mask)
        nrows = count(row_mask, kind=int64)

        if (present(is_valid)) then
            valid_flat = pack(reshape(is_valid, [size(is_valid, kind=int64)]), elem_mask)
            call parquet_check_protected(writer, name, valid_flat)
            call parquet_check_qc_miss(writer, name, valid_flat)
            if (writer%qc .and. writer%is_schema_enforced) then
                call parquet_check_qc_numeric(writer, name, real(packed, kind=real64), valid_flat)
            end if
            call parquet_make_valid_buf_write(valid_flat, valid_buf, valid_ptr)
        else
            if (writer%qc .and. writer%is_schema_enforced) then
                call parquet_check_qc_numeric(writer, name, real(packed, kind=real64), &
                    spread(.true., 1, size(packed, kind=int64)))
            end if
            call parquet_make_valid_buf_write(valid_buf=valid_buf, valid_ptr=valid_ptr)
        end if
        call parquet_check_row_count(writer, name, nrows)

        call parquet_append_as_schema_int64(writer, name, packed, nrows, asize, valid_ptr)
    end procedure parquet_write_int64_matrix_column
    module procedure parquet_write_float32_column
        integer :: idx
        integer(int64) :: asize, nrows
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        character(len=:), allocatable :: ctx !! writer_context_suffix scratch.
        logical, allocatable :: row_mask(:), elem_mask(:), is_valid_c(:)
        real(real32), allocatable :: values_c(:)
        call check_writer_open(writer)

        if (writer%is_schema_enforced) then
            idx = parquet_get_defined_column_index(writer, name)
            call writer_context_suffix(writer, ctx)
            if (idx == 0) error stop &
                "parquet_write_column: column not defined in parquet_open_writer: " // trim(name) // ctx
            if (.not. writer%all_columns(idx)%is_set) return
        end if

        call parquet_assert_column_type(writer, name, "float32")

        if (.not. parquet_is_column_enabled(writer, name)) return
        call parquet_mark_column_written(writer, name)

        asize = int(parquet_get_column_col_size(writer, name), kind=int64)
        call writer_context_suffix(writer, ctx)
        if (mod(size(values, kind=int64), asize) /= 0) error stop &
            "parquet_write_float32_column: values size is not divisible by col_size for column " // &
            trim(name) // ctx
        nrows = size(values, kind=int64) / asize

        call parquet_writer_whole_column_mask(writer, name, nrows, row_mask)
        elem_mask = parquet_mask_expand_block(row_mask, asize)
        values_c = pack(values, elem_mask)
        nrows = count(row_mask, kind=int64)

        if (present(is_valid)) then
            is_valid_c = pack(is_valid, elem_mask)
            call parquet_check_protected(writer, name, is_valid_c)
            call parquet_check_qc_miss(writer, name, is_valid_c)
            if (writer%qc .and. writer%is_schema_enforced) then
                call parquet_check_qc_numeric(writer, name, real(values_c, kind=real64), is_valid_c)
            end if
        else
            if (writer%qc .and. writer%is_schema_enforced) then
                call parquet_check_qc_numeric(writer, name, real(values_c, kind=real64), &
                    spread(.true., 1, size(values_c, kind=int64)))
            end if
        end if
        call parquet_make_valid_buf_write(is_valid_c, valid_buf, valid_ptr)
        call parquet_check_row_count(writer, name, nrows)

        call parquet_append_as_schema_float32(writer, name, values_c, nrows, asize, valid_ptr)
    end procedure parquet_write_float32_column
    module procedure parquet_write_float32_matrix_column
        integer :: idx
        integer(int64) :: asize, nrows
        real(real32), allocatable :: packed(:)
        character(len=:), allocatable :: ctx !! writer_context_suffix scratch.
        logical, allocatable :: valid_flat(:)
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        logical, allocatable :: row_mask(:), elem_mask(:)
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
        end if

        call parquet_assert_column_type(writer, name, "float32")

        if (.not. parquet_is_column_enabled(writer, name)) return
        call parquet_mark_column_written(writer, name)

        allocate(packed(size(values, kind=int64)))
        packed = reshape(values, [size(values, kind=int64)])

        call parquet_writer_whole_column_mask(writer, name, nrows, row_mask)
        elem_mask = parquet_mask_expand_block(row_mask, asize)
        packed = pack(packed, elem_mask)
        nrows = count(row_mask, kind=int64)

        if (present(is_valid)) then
            valid_flat = pack(reshape(is_valid, [size(is_valid, kind=int64)]), elem_mask)
            call parquet_check_protected(writer, name, valid_flat)
            call parquet_check_qc_miss(writer, name, valid_flat)
            if (writer%qc .and. writer%is_schema_enforced) then
                call parquet_check_qc_numeric(writer, name, real(packed, kind=real64), valid_flat)
            end if
            call parquet_make_valid_buf_write(valid_flat, valid_buf, valid_ptr)
        else
            if (writer%qc .and. writer%is_schema_enforced) then
                call parquet_check_qc_numeric(writer, name, real(packed, kind=real64), &
                    spread(.true., 1, size(packed, kind=int64)))
            end if
            call parquet_make_valid_buf_write(valid_buf=valid_buf, valid_ptr=valid_ptr)
        end if
        call parquet_check_row_count(writer, name, nrows)

        call parquet_append_as_schema_float32(writer, name, packed, nrows, asize, valid_ptr)
    end procedure parquet_write_float32_matrix_column
    module procedure parquet_write_float64_column
        integer :: idx
        integer(int64) :: asize, nrows
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        character(len=:), allocatable :: ctx !! writer_context_suffix scratch.
        logical, allocatable :: row_mask(:), elem_mask(:), is_valid_c(:)
        real(real64), allocatable :: values_c(:)
        call check_writer_open(writer)

        if (writer%is_schema_enforced) then
            idx = parquet_get_defined_column_index(writer, name)
            call writer_context_suffix(writer, ctx)
            if (idx == 0) error stop &
                "parquet_write_column: column not defined in parquet_open_writer: " // trim(name) // ctx
            if (.not. writer%all_columns(idx)%is_set) return
        end if

        call parquet_assert_column_type(writer, name, "float64")

        if (.not. parquet_is_column_enabled(writer, name)) return
        call parquet_mark_column_written(writer, name)

        asize = int(parquet_get_column_col_size(writer, name), kind=int64)
        call writer_context_suffix(writer, ctx)
        if (mod(size(values, kind=int64), asize) /= 0) error stop &
            "parquet_write_float64_column: values size is not divisible by col_size for column " // &
            trim(name) // ctx
        nrows = size(values, kind=int64) / asize

        call parquet_writer_whole_column_mask(writer, name, nrows, row_mask)
        elem_mask = parquet_mask_expand_block(row_mask, asize)
        values_c = pack(values, elem_mask)
        nrows = count(row_mask, kind=int64)

        if (present(is_valid)) then
            is_valid_c = pack(is_valid, elem_mask)
            call parquet_check_protected(writer, name, is_valid_c)
            call parquet_check_qc_miss(writer, name, is_valid_c)
            if (writer%qc .and. writer%is_schema_enforced) then
                call parquet_check_qc_numeric(writer, name, real(values_c, kind=real64), is_valid_c)
            end if
        else
            if (writer%qc .and. writer%is_schema_enforced) then
                call parquet_check_qc_numeric(writer, name, real(values_c, kind=real64), &
                    spread(.true., 1, size(values_c, kind=int64)))
            end if
        end if
        call parquet_make_valid_buf_write(is_valid_c, valid_buf, valid_ptr)
        call parquet_check_row_count(writer, name, nrows)

        call parquet_append_as_schema_float64(writer, name, values_c, nrows, asize, valid_ptr)
    end procedure parquet_write_float64_column
    module procedure parquet_write_float64_matrix_column
        integer :: idx
        integer(int64) :: asize, nrows
        real(real64), allocatable :: packed(:)
        character(len=:), allocatable :: ctx !! writer_context_suffix scratch.
        logical, allocatable :: valid_flat(:)
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        logical, allocatable :: row_mask(:), elem_mask(:)
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
        end if

        call parquet_assert_column_type(writer, name, "float64")

        if (.not. parquet_is_column_enabled(writer, name)) return
        call parquet_mark_column_written(writer, name)

        allocate(packed(size(values, kind=int64)))
        packed = reshape(values, [size(values, kind=int64)])

        call parquet_writer_whole_column_mask(writer, name, nrows, row_mask)
        elem_mask = parquet_mask_expand_block(row_mask, asize)
        packed = pack(packed, elem_mask)
        nrows = count(row_mask, kind=int64)

        if (present(is_valid)) then
            valid_flat = pack(reshape(is_valid, [size(is_valid, kind=int64)]), elem_mask)
            call parquet_check_protected(writer, name, valid_flat)
            call parquet_check_qc_miss(writer, name, valid_flat)
            if (writer%qc .and. writer%is_schema_enforced) then
                call parquet_check_qc_numeric(writer, name, real(packed, kind=real64), valid_flat)
            end if
            call parquet_make_valid_buf_write(valid_flat, valid_buf, valid_ptr)
        else
            if (writer%qc .and. writer%is_schema_enforced) then
                call parquet_check_qc_numeric(writer, name, real(packed, kind=real64), &
                    spread(.true., 1, size(packed, kind=int64)))
            end if
            call parquet_make_valid_buf_write(valid_buf=valid_buf, valid_ptr=valid_ptr)
        end if
        call parquet_check_row_count(writer, name, nrows)

        call parquet_append_as_schema_float64(writer, name, packed, nrows, asize, valid_ptr)
    end procedure parquet_write_float64_matrix_column
    module procedure parquet_write_logical_column
        integer :: idx
        integer(int64) :: asize, nrows, i
        integer(c_int8_t), allocatable :: bool_data(:)
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        character(len=:), allocatable :: ctx !! writer_context_suffix scratch.
        character(len=:), allocatable :: outname !! parquet_resolve_output_name scratch.
        logical, allocatable :: row_mask(:), elem_mask(:), is_valid_c(:)
        call check_writer_open(writer)

        if (writer%is_schema_enforced) then
            idx = parquet_get_defined_column_index(writer, name)
            call writer_context_suffix(writer, ctx)
            if (idx == 0) error stop &
                "parquet_write_column: column not defined in parquet_open_writer: " // trim(name) // ctx
            if (.not. writer%all_columns(idx)%is_set) return
        end if

        call parquet_assert_column_type(writer, name, "boolean")

        if (.not. parquet_is_column_enabled(writer, name)) return
        call parquet_mark_column_written(writer, name)

        asize = int(parquet_get_column_col_size(writer, name), kind=int64)
        call writer_context_suffix(writer, ctx)
        if (mod(size(values, kind=int64), asize) /= 0) error stop &
            "parquet_write_logical_column: values size is not divisible by col_size for column " // &
            trim(name) // ctx
        nrows = size(values, kind=int64) / asize

        allocate(bool_data(size(values, kind=int64)))
        do i = 1_int64, size(values, kind=int64)
            if (values(i)) then
                bool_data(i) = 1_c_int8_t
            else
                bool_data(i) = 0_c_int8_t
            end if
        end do

        call parquet_writer_whole_column_mask(writer, name, nrows, row_mask)
        elem_mask = parquet_mask_expand_block(row_mask, asize)
        bool_data = pack(bool_data, elem_mask)
        nrows = count(row_mask, kind=int64)

        if (present(is_valid)) then
            is_valid_c = pack(is_valid, elem_mask)
            call parquet_check_protected(writer, name, is_valid_c)
            call parquet_check_qc_miss(writer, name, is_valid_c)
        end if
        call parquet_make_valid_buf_write(is_valid_c, valid_buf, valid_ptr)
        call parquet_check_row_count(writer, name, nrows)

        call parquet_resolve_output_name(writer, name, outname)
        call parquet_append_bool8_column(&
            writer%handle, &
            trim(outname)//char(0), &
            bool_data, &
            nrows, &
            asize, &
            valid_ptr )
    end procedure parquet_write_logical_column
    module procedure parquet_write_logical_matrix_column
        integer :: idx
        integer(int64) :: asize, nrows
        integer(c_int8_t), allocatable :: bool_data(:)
        character(len=:), allocatable :: ctx !! writer_context_suffix scratch.
        character(len=:), allocatable :: outname !! parquet_resolve_output_name scratch.
        logical, allocatable :: valid_flat(:)
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        logical, allocatable :: row_mask(:), elem_mask(:)
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
        end if

        call parquet_assert_column_type(writer, name, "boolean")

        if (.not. parquet_is_column_enabled(writer, name)) return
        call parquet_mark_column_written(writer, name)

        allocate(bool_data(size(values, kind=int64)))
        bool_data = merge(1_c_int8_t, 0_c_int8_t, reshape(values, [size(values, kind=int64)]))

        call parquet_writer_whole_column_mask(writer, name, nrows, row_mask)
        elem_mask = parquet_mask_expand_block(row_mask, asize)
        bool_data = pack(bool_data, elem_mask)
        nrows = count(row_mask, kind=int64)

        if (present(is_valid)) then
            valid_flat = pack(reshape(is_valid, [size(is_valid, kind=int64)]), elem_mask)
            call parquet_check_protected(writer, name, valid_flat)
            call parquet_check_qc_miss(writer, name, valid_flat)
            call parquet_make_valid_buf_write(valid_flat, valid_buf, valid_ptr)
        else
            call parquet_make_valid_buf_write(valid_buf=valid_buf, valid_ptr=valid_ptr)
        end if
        call parquet_check_row_count(writer, name, nrows)

        call parquet_resolve_output_name(writer, name, outname)
        call parquet_append_bool8_column(&
            writer%handle, &
            trim(outname)//char(0), &
            bool_data, &
            nrows, &
            asize, &
            valid_ptr )
    end procedure parquet_write_logical_matrix_column
    module procedure parquet_write_int32_column_chunk
        integer :: idx
        integer(int64) :: asize, nrows
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        character(len=:), allocatable :: ctx !! writer_context_suffix scratch.
        character(len=:), allocatable :: outname !! parquet_resolve_output_name scratch.
        logical, allocatable :: row_mask(:), elem_mask(:), is_valid_c(:)
        integer(int32), allocatable :: values_c(:)
        call check_writer_open(writer)

        if (writer%is_schema_enforced) then
            idx = parquet_get_defined_column_index(writer, name)
            call writer_context_suffix(writer, ctx)
            if (idx == 0) error stop &
                "parquet_write_column_chunk: column not defined in parquet_open_writer: " // &
                trim(name) // ctx
            if (.not. writer%all_columns(idx)%is_set) return
        end if

        call parquet_assert_column_type_exact(writer, name, "int32")

        if (.not. parquet_is_column_enabled(writer, name)) return

        asize = int(parquet_get_column_col_size(writer, name), kind=int64)
        call writer_context_suffix(writer, ctx)
        if (mod(size(values, kind=int64), asize) /= 0) error stop &
            "parquet_write_int32_column_chunk: values size is not divisible by col_size for column " // &
            trim(name) // ctx
        nrows = size(values, kind=int64) / asize
        call parquet_check_row_group_row_count(writer, name, nrows, row_mask)
        elem_mask = parquet_mask_expand_block(row_mask, asize)
        values_c = pack(values, elem_mask)
        nrows = count(row_mask, kind=int64)

        if (present(is_valid)) then
            is_valid_c = pack(is_valid, elem_mask)
            call parquet_check_protected(writer, name, is_valid_c)
            call parquet_check_qc_miss(writer, name, is_valid_c)
            if (writer%qc .and. writer%is_schema_enforced) then
                call parquet_check_qc_numeric(writer, name, real(values_c, kind=real64), is_valid_c)
            end if
        else
            if (writer%qc .and. writer%is_schema_enforced) then
                call parquet_check_qc_numeric(writer, name, real(values_c, kind=real64), &
                    spread(.true., 1, size(values_c, kind=int64)))
            end if
        end if
        call parquet_make_valid_buf_write(is_valid_c, valid_buf, valid_ptr)
        call parquet_chunk_mark_written_if_first(writer, name)

        call parquet_resolve_output_name(writer, name, outname)
        if (nrows > 0) call parquet_append_int32_column_chunk(writer%handle, &
            trim(outname)//char(0), values_c, asize, valid_ptr)
    end procedure parquet_write_int32_column_chunk
    module procedure parquet_write_int32_matrix_column_chunk
        integer :: idx
        integer(int64) :: asize, nrows
        integer(int32), allocatable :: packed(:)
        character(len=:), allocatable :: ctx !! writer_context_suffix scratch.
        character(len=:), allocatable :: outname !! parquet_resolve_output_name scratch.
        logical, allocatable :: valid_flat(:)
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        logical, allocatable :: row_mask(:), elem_mask(:)
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
        end if

        call parquet_assert_column_type_exact(writer, name, "int32")

        if (.not. parquet_is_column_enabled(writer, name)) return
        call parquet_check_row_group_row_count(writer, name, nrows, row_mask)
        elem_mask = parquet_mask_expand_block(row_mask, asize)

        allocate(packed(size(values, kind=int64)))
        packed = reshape(values, [size(values, kind=int64)])
        packed = pack(packed, elem_mask)
        nrows = count(row_mask, kind=int64)

        if (present(is_valid)) then
            valid_flat = pack(reshape(is_valid, [size(is_valid, kind=int64)]), elem_mask)
            call parquet_check_protected(writer, name, valid_flat)
            call parquet_check_qc_miss(writer, name, valid_flat)
            if (writer%qc .and. writer%is_schema_enforced) then
                call parquet_check_qc_numeric(writer, name, real(packed, kind=real64), valid_flat)
            end if
            call parquet_make_valid_buf_write(valid_flat, valid_buf, valid_ptr)
        else
            if (writer%qc .and. writer%is_schema_enforced) then
                call parquet_check_qc_numeric(writer, name, real(packed, kind=real64), &
                    spread(.true., 1, size(packed, kind=int64)))
            end if
            call parquet_make_valid_buf_write(valid_buf=valid_buf, valid_ptr=valid_ptr)
        end if
        call parquet_chunk_mark_written_if_first(writer, name)

        call parquet_resolve_output_name(writer, name, outname)
        if (nrows > 0) call parquet_append_int32_column_chunk(writer%handle, &
            trim(outname)//char(0), packed, asize, valid_ptr)
    end procedure parquet_write_int32_matrix_column_chunk
    module procedure parquet_write_int64_column_chunk
        integer :: idx
        integer(int64) :: asize, nrows
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        character(len=:), allocatable :: ctx !! writer_context_suffix scratch.
        character(len=:), allocatable :: outname !! parquet_resolve_output_name scratch.
        logical, allocatable :: row_mask(:), elem_mask(:), is_valid_c(:)
        integer(int64), allocatable :: values_c(:)
        call check_writer_open(writer)

        if (writer%is_schema_enforced) then
            idx = parquet_get_defined_column_index(writer, name)
            call writer_context_suffix(writer, ctx)
            if (idx == 0) error stop &
                "parquet_write_column_chunk: column not defined in parquet_open_writer: " // &
                trim(name) // ctx
            if (.not. writer%all_columns(idx)%is_set) return
        end if

        call parquet_assert_column_type_exact(writer, name, "int64")

        if (.not. parquet_is_column_enabled(writer, name)) return

        asize = int(parquet_get_column_col_size(writer, name), kind=int64)
        call writer_context_suffix(writer, ctx)
        if (mod(size(values, kind=int64), asize) /= 0) error stop &
            "parquet_write_int64_column_chunk: values size is not divisible by col_size for column " // &
            trim(name) // ctx
        nrows = size(values, kind=int64) / asize
        call parquet_check_row_group_row_count(writer, name, nrows, row_mask)
        elem_mask = parquet_mask_expand_block(row_mask, asize)
        values_c = pack(values, elem_mask)
        nrows = count(row_mask, kind=int64)

        if (present(is_valid)) then
            is_valid_c = pack(is_valid, elem_mask)
            call parquet_check_protected(writer, name, is_valid_c)
            call parquet_check_qc_miss(writer, name, is_valid_c)
            if (writer%qc .and. writer%is_schema_enforced) then
                call parquet_check_qc_numeric(writer, name, real(values_c, kind=real64), is_valid_c)
            end if
        else
            if (writer%qc .and. writer%is_schema_enforced) then
                call parquet_check_qc_numeric(writer, name, real(values_c, kind=real64), &
                    spread(.true., 1, size(values_c, kind=int64)))
            end if
        end if
        call parquet_make_valid_buf_write(is_valid_c, valid_buf, valid_ptr)
        call parquet_chunk_mark_written_if_first(writer, name)

        call parquet_resolve_output_name(writer, name, outname)
        if (nrows > 0) call parquet_append_int64_column_chunk(writer%handle, &
            trim(outname)//char(0), values_c, asize, valid_ptr)
    end procedure parquet_write_int64_column_chunk
    module procedure parquet_write_int64_matrix_column_chunk
        integer :: idx
        integer(int64) :: asize, nrows
        integer(int64), allocatable :: packed(:)
        character(len=:), allocatable :: ctx !! writer_context_suffix scratch.
        character(len=:), allocatable :: outname !! parquet_resolve_output_name scratch.
        logical, allocatable :: valid_flat(:)
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        logical, allocatable :: row_mask(:), elem_mask(:)
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
        end if

        call parquet_assert_column_type_exact(writer, name, "int64")

        if (.not. parquet_is_column_enabled(writer, name)) return
        call parquet_check_row_group_row_count(writer, name, nrows, row_mask)
        elem_mask = parquet_mask_expand_block(row_mask, asize)

        allocate(packed(size(values, kind=int64)))
        packed = reshape(values, [size(values, kind=int64)])
        packed = pack(packed, elem_mask)
        nrows = count(row_mask, kind=int64)

        if (present(is_valid)) then
            valid_flat = pack(reshape(is_valid, [size(is_valid, kind=int64)]), elem_mask)
            call parquet_check_protected(writer, name, valid_flat)
            call parquet_check_qc_miss(writer, name, valid_flat)
            if (writer%qc .and. writer%is_schema_enforced) then
                call parquet_check_qc_numeric(writer, name, real(packed, kind=real64), valid_flat)
            end if
            call parquet_make_valid_buf_write(valid_flat, valid_buf, valid_ptr)
        else
            if (writer%qc .and. writer%is_schema_enforced) then
                call parquet_check_qc_numeric(writer, name, real(packed, kind=real64), &
                    spread(.true., 1, size(packed, kind=int64)))
            end if
            call parquet_make_valid_buf_write(valid_buf=valid_buf, valid_ptr=valid_ptr)
        end if
        call parquet_chunk_mark_written_if_first(writer, name)

        call parquet_resolve_output_name(writer, name, outname)
        if (nrows > 0) call parquet_append_int64_column_chunk(writer%handle, &
            trim(outname)//char(0), packed, asize, valid_ptr)
    end procedure parquet_write_int64_matrix_column_chunk
    module procedure parquet_write_float32_column_chunk
        integer :: idx
        integer(int64) :: asize, nrows
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        character(len=:), allocatable :: ctx !! writer_context_suffix scratch.
        character(len=:), allocatable :: outname !! parquet_resolve_output_name scratch.
        logical, allocatable :: row_mask(:), elem_mask(:), is_valid_c(:)
        real(real32), allocatable :: values_c(:)
        call check_writer_open(writer)

        if (writer%is_schema_enforced) then
            idx = parquet_get_defined_column_index(writer, name)
            call writer_context_suffix(writer, ctx)
            if (idx == 0) error stop &
                "parquet_write_column_chunk: column not defined in parquet_open_writer: " // &
                trim(name) // ctx
            if (.not. writer%all_columns(idx)%is_set) return
        end if

        call parquet_assert_column_type_exact(writer, name, "float32")

        if (.not. parquet_is_column_enabled(writer, name)) return

        asize = int(parquet_get_column_col_size(writer, name), kind=int64)
        call writer_context_suffix(writer, ctx)
        if (mod(size(values, kind=int64), asize) /= 0) error stop &
            "parquet_write_float32_column_chunk: values size is not divisible by col_size for column " // &
            trim(name) // ctx
        nrows = size(values, kind=int64) / asize
        call parquet_check_row_group_row_count(writer, name, nrows, row_mask)
        elem_mask = parquet_mask_expand_block(row_mask, asize)
        values_c = pack(values, elem_mask)
        nrows = count(row_mask, kind=int64)

        if (present(is_valid)) then
            is_valid_c = pack(is_valid, elem_mask)
            call parquet_check_protected(writer, name, is_valid_c)
            call parquet_check_qc_miss(writer, name, is_valid_c)
            if (writer%qc .and. writer%is_schema_enforced) then
                call parquet_check_qc_numeric(writer, name, real(values_c, kind=real64), is_valid_c)
            end if
        else
            if (writer%qc .and. writer%is_schema_enforced) then
                call parquet_check_qc_numeric(writer, name, real(values_c, kind=real64), &
                    spread(.true., 1, size(values_c, kind=int64)))
            end if
        end if
        call parquet_make_valid_buf_write(is_valid_c, valid_buf, valid_ptr)
        call parquet_chunk_mark_written_if_first(writer, name)

        call parquet_resolve_output_name(writer, name, outname)
        if (nrows > 0) call parquet_append_float32_column_chunk(writer%handle, &
            trim(outname)//char(0), values_c, asize, valid_ptr)
    end procedure parquet_write_float32_column_chunk
    module procedure parquet_write_float32_matrix_column_chunk
        integer :: idx
        integer(int64) :: asize, nrows
        real(real32), allocatable :: packed(:)
        character(len=:), allocatable :: ctx !! writer_context_suffix scratch.
        character(len=:), allocatable :: outname !! parquet_resolve_output_name scratch.
        logical, allocatable :: valid_flat(:)
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        logical, allocatable :: row_mask(:), elem_mask(:)
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
        end if

        call parquet_assert_column_type_exact(writer, name, "float32")

        if (.not. parquet_is_column_enabled(writer, name)) return
        call parquet_check_row_group_row_count(writer, name, nrows, row_mask)
        elem_mask = parquet_mask_expand_block(row_mask, asize)

        allocate(packed(size(values, kind=int64)))
        packed = reshape(values, [size(values, kind=int64)])
        packed = pack(packed, elem_mask)
        nrows = count(row_mask, kind=int64)

        if (present(is_valid)) then
            valid_flat = pack(reshape(is_valid, [size(is_valid, kind=int64)]), elem_mask)
            call parquet_check_protected(writer, name, valid_flat)
            call parquet_check_qc_miss(writer, name, valid_flat)
            if (writer%qc .and. writer%is_schema_enforced) then
                call parquet_check_qc_numeric(writer, name, real(packed, kind=real64), valid_flat)
            end if
            call parquet_make_valid_buf_write(valid_flat, valid_buf, valid_ptr)
        else
            if (writer%qc .and. writer%is_schema_enforced) then
                call parquet_check_qc_numeric(writer, name, real(packed, kind=real64), &
                    spread(.true., 1, size(packed, kind=int64)))
            end if
            call parquet_make_valid_buf_write(valid_buf=valid_buf, valid_ptr=valid_ptr)
        end if
        call parquet_chunk_mark_written_if_first(writer, name)

        call parquet_resolve_output_name(writer, name, outname)
        if (nrows > 0) call parquet_append_float32_column_chunk(writer%handle, &
            trim(outname)//char(0), packed, asize, valid_ptr)
    end procedure parquet_write_float32_matrix_column_chunk
    module procedure parquet_write_float64_column_chunk
        integer :: idx
        integer(int64) :: asize, nrows
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        character(len=:), allocatable :: ctx !! writer_context_suffix scratch.
        character(len=:), allocatable :: outname !! parquet_resolve_output_name scratch.
        logical, allocatable :: row_mask(:), elem_mask(:), is_valid_c(:)
        real(real64), allocatable :: values_c(:)
        call check_writer_open(writer)

        if (writer%is_schema_enforced) then
            idx = parquet_get_defined_column_index(writer, name)
            call writer_context_suffix(writer, ctx)
            if (idx == 0) error stop &
                "parquet_write_column_chunk: column not defined in parquet_open_writer: " // &
                trim(name) // ctx
            if (.not. writer%all_columns(idx)%is_set) return
        end if

        call parquet_assert_column_type_exact(writer, name, "float64")

        if (.not. parquet_is_column_enabled(writer, name)) return

        asize = int(parquet_get_column_col_size(writer, name), kind=int64)
        call writer_context_suffix(writer, ctx)
        if (mod(size(values, kind=int64), asize) /= 0) error stop &
            "parquet_write_float64_column_chunk: values size is not divisible by col_size for column " // &
            trim(name) // ctx
        nrows = size(values, kind=int64) / asize
        call parquet_check_row_group_row_count(writer, name, nrows, row_mask)
        elem_mask = parquet_mask_expand_block(row_mask, asize)
        values_c = pack(values, elem_mask)
        nrows = count(row_mask, kind=int64)

        if (present(is_valid)) then
            is_valid_c = pack(is_valid, elem_mask)
            call parquet_check_protected(writer, name, is_valid_c)
            call parquet_check_qc_miss(writer, name, is_valid_c)
            if (writer%qc .and. writer%is_schema_enforced) then
                call parquet_check_qc_numeric(writer, name, real(values_c, kind=real64), is_valid_c)
            end if
        else
            if (writer%qc .and. writer%is_schema_enforced) then
                call parquet_check_qc_numeric(writer, name, real(values_c, kind=real64), &
                    spread(.true., 1, size(values_c, kind=int64)))
            end if
        end if
        call parquet_make_valid_buf_write(is_valid_c, valid_buf, valid_ptr)
        call parquet_chunk_mark_written_if_first(writer, name)

        call parquet_resolve_output_name(writer, name, outname)
        if (nrows > 0) call parquet_append_float64_column_chunk(writer%handle, &
            trim(outname)//char(0), values_c, asize, valid_ptr)
    end procedure parquet_write_float64_column_chunk
    module procedure parquet_write_float64_matrix_column_chunk
        integer :: idx
        integer(int64) :: asize, nrows
        real(real64), allocatable :: packed(:)
        character(len=:), allocatable :: ctx !! writer_context_suffix scratch.
        character(len=:), allocatable :: outname !! parquet_resolve_output_name scratch.
        logical, allocatable :: valid_flat(:)
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        logical, allocatable :: row_mask(:), elem_mask(:)
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
        end if

        call parquet_assert_column_type_exact(writer, name, "float64")

        if (.not. parquet_is_column_enabled(writer, name)) return
        call parquet_check_row_group_row_count(writer, name, nrows, row_mask)
        elem_mask = parquet_mask_expand_block(row_mask, asize)

        allocate(packed(size(values, kind=int64)))
        packed = reshape(values, [size(values, kind=int64)])
        packed = pack(packed, elem_mask)
        nrows = count(row_mask, kind=int64)

        if (present(is_valid)) then
            valid_flat = pack(reshape(is_valid, [size(is_valid, kind=int64)]), elem_mask)
            call parquet_check_protected(writer, name, valid_flat)
            call parquet_check_qc_miss(writer, name, valid_flat)
            if (writer%qc .and. writer%is_schema_enforced) then
                call parquet_check_qc_numeric(writer, name, real(packed, kind=real64), valid_flat)
            end if
            call parquet_make_valid_buf_write(valid_flat, valid_buf, valid_ptr)
        else
            if (writer%qc .and. writer%is_schema_enforced) then
                call parquet_check_qc_numeric(writer, name, real(packed, kind=real64), &
                    spread(.true., 1, size(packed, kind=int64)))
            end if
            call parquet_make_valid_buf_write(valid_buf=valid_buf, valid_ptr=valid_ptr)
        end if
        call parquet_chunk_mark_written_if_first(writer, name)

        call parquet_resolve_output_name(writer, name, outname)
        if (nrows > 0) call parquet_append_float64_column_chunk(writer%handle, &
            trim(outname)//char(0), packed, asize, valid_ptr)
    end procedure parquet_write_float64_matrix_column_chunk
    module procedure parquet_write_logical_column_chunk
        integer :: idx
        integer(int64) :: asize, nrows, i
        integer(c_int8_t), allocatable :: bool_data(:)
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        character(len=:), allocatable :: ctx !! writer_context_suffix scratch.
        character(len=:), allocatable :: outname !! parquet_resolve_output_name scratch.
        logical, allocatable :: row_mask(:), elem_mask(:), is_valid_c(:)
        call check_writer_open(writer)

        if (writer%is_schema_enforced) then
            idx = parquet_get_defined_column_index(writer, name)
            call writer_context_suffix(writer, ctx)
            if (idx == 0) error stop &
                "parquet_write_column_chunk: column not defined in parquet_open_writer: " // &
                trim(name) // ctx
            if (.not. writer%all_columns(idx)%is_set) return
        end if

        call parquet_assert_column_type_exact(writer, name, "boolean")

        if (.not. parquet_is_column_enabled(writer, name)) return

        asize = int(parquet_get_column_col_size(writer, name), kind=int64)
        call writer_context_suffix(writer, ctx)
        if (mod(size(values, kind=int64), asize) /= 0) error stop &
            "parquet_write_logical_column_chunk: values size is not divisible by col_size for column " // &
            trim(name) // ctx
        nrows = size(values, kind=int64) / asize
        call parquet_check_row_group_row_count(writer, name, nrows, row_mask)
        elem_mask = parquet_mask_expand_block(row_mask, asize)

        allocate(bool_data(size(values, kind=int64)))
        do i = 1_int64, size(values, kind=int64)
            if (values(i)) then
                bool_data(i) = 1_c_int8_t
            else
                bool_data(i) = 0_c_int8_t
            end if
        end do
        bool_data = pack(bool_data, elem_mask)
        nrows = count(row_mask, kind=int64)

        if (present(is_valid)) then
            is_valid_c = pack(is_valid, elem_mask)
            call parquet_check_protected(writer, name, is_valid_c)
            call parquet_check_qc_miss(writer, name, is_valid_c)
        end if
        call parquet_make_valid_buf_write(is_valid_c, valid_buf, valid_ptr)
        call parquet_chunk_mark_written_if_first(writer, name)

        call parquet_resolve_output_name(writer, name, outname)
        if (nrows > 0) call parquet_append_bool8_column_chunk(&
            writer%handle, &
            trim(outname)//char(0), &
            bool_data, &
            asize, &
            valid_ptr )
    end procedure parquet_write_logical_column_chunk
    module procedure parquet_write_logical_matrix_column_chunk
        integer :: idx
        integer(int64) :: asize, nrows
        integer(c_int8_t), allocatable :: bool_data(:)
        character(len=:), allocatable :: ctx !! writer_context_suffix scratch.
        character(len=:), allocatable :: outname !! parquet_resolve_output_name scratch.
        logical, allocatable :: valid_flat(:)
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        logical, allocatable :: row_mask(:), elem_mask(:)
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
        end if

        call parquet_assert_column_type_exact(writer, name, "boolean")

        if (.not. parquet_is_column_enabled(writer, name)) return
        call parquet_check_row_group_row_count(writer, name, nrows, row_mask)
        elem_mask = parquet_mask_expand_block(row_mask, asize)

        allocate(bool_data(size(values, kind=int64)))
        bool_data = merge(1_c_int8_t, 0_c_int8_t, reshape(values, [size(values, kind=int64)]))
        bool_data = pack(bool_data, elem_mask)
        nrows = count(row_mask, kind=int64)

        if (present(is_valid)) then
            valid_flat = pack(reshape(is_valid, [size(is_valid, kind=int64)]), elem_mask)
            call parquet_check_protected(writer, name, valid_flat)
            call parquet_check_qc_miss(writer, name, valid_flat)
            call parquet_make_valid_buf_write(valid_flat, valid_buf, valid_ptr)
        else
            call parquet_make_valid_buf_write(valid_buf=valid_buf, valid_ptr=valid_ptr)
        end if
        call parquet_chunk_mark_written_if_first(writer, name)

        call parquet_resolve_output_name(writer, name, outname)
        if (nrows > 0) call parquet_append_bool8_column_chunk(&
            writer%handle, &
            trim(outname)//char(0), &
            bool_data, &
            asize, &
            valid_ptr )
    end procedure parquet_write_logical_matrix_column_chunk

end submodule parquet_write_numeric
