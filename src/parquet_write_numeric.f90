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

    !> Checks every valid element of a numeric column against its schema-declared qc: min:/max:
    !> bounds and prints a single WARNING naming the column, its declared bound(s), the observed
    !> data range among valid elements, and how many of them violate at least one bound. Never
    !> errors -- writing proceeds regardless. A no-op for a schema-less writer, a writer with qc
    !> off, a column without a qc: block, or when there are no valid elements to check at all.
    !> An absent is_valid_flat means every element counts, matching the write-side is_valid
    !> convention elsewhere.
    !>
    !> One specific per value kind, rather than one procedure taking real64: the widening this
    !> used to require built a full float64 copy of the whole column purely to make the call, on
    !> every qc-enabled write. Each specific now converts the BOUND (once, before the loop) and
    !> leaves the values alone -- see qc_bound_as_int64 for the one case where that also changes
    !> the answer, for the better.
    interface parquet_check_qc_numeric
        procedure :: qc_numeric_i32
        procedure :: qc_numeric_i64
        procedure :: qc_numeric_r32
        procedure :: qc_numeric_r64
    end interface parquet_check_qc_numeric

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
    !> Resolves the qc: min:/max: bounds declared for column `name`, so the four per-kind
    !> parquet_check_qc_numeric specifics below share one copy of the lookup rather than each
    !> carrying their own. Returns .false. when there is nothing to check at all: a writer with
    !> qc off, a schema-less writer, a column that is not in the schema, a column with no qc:
    !> block, or bounds whose text does not parse for the column's declared data_type.
    logical function qc_numeric_bounds(writer, name, have_min, min_bound, min_op, &
        have_max, max_bound, max_op) result(any_bound)
        type(parquet_writer), intent(in) :: writer !! open writer.
        character(len=*), intent(in) :: name !! numeric column name.
        logical, intent(out) :: have_min !! .true. if a usable qc: min: bound was declared.
        real(real64), intent(out) :: min_bound !! the declared min bound (meaningful only if have_min).
        character(len=:), allocatable, intent(out) :: min_op !! its comparison operator, ">=" or ">".
        logical, intent(out) :: have_max !! .true. if a usable qc: max: bound was declared.
        real(real64), intent(out) :: max_bound !! the declared max bound (meaningful only if have_max).
        character(len=:), allocatable, intent(out) :: max_op !! its comparison operator, "<=" or "<".
        integer :: idx

        any_bound = .false.
        have_min = .false.
        have_max = .false.
        min_bound = 0.0_real64
        max_bound = 0.0_real64
        min_op = ""
        max_op = ""

        if (.not. writer%qc) return
        if (.not. writer%is_schema_enforced) return
        idx = parquet_get_defined_column_index(writer, name)
        if (idx == 0) return
        if (.not. (writer%all_columns(idx)%has_qc_min .or. writer%all_columns(idx)%has_qc_max)) return

        if (writer%all_columns(idx)%has_qc_min) then
            have_min = parquet_qc_numeric_bound( &
                writer%all_columns(idx)%qc_min_raw, writer%all_columns(idx)%data_type, min_bound)
            if (have_min) min_op = trim(writer%all_columns(idx)%qc_min_op)
        end if
        if (writer%all_columns(idx)%has_qc_max) then
            have_max = parquet_qc_numeric_bound( &
                writer%all_columns(idx)%qc_max_raw, writer%all_columns(idx)%data_type, max_bound)
            if (have_max) max_op = trim(writer%all_columns(idx)%qc_max_op)
        end if
        any_bound = have_min .or. have_max
    end function qc_numeric_bounds
    !> Emits the single WARNING a qc: min:/max: violation produces, naming the column, its
    !> declared bound(s), the observed data range among valid elements, and how many of them
    !> violate at least one bound. Shared by the four per-kind specifics so the message text
    !> stays identical across value kinds -- the observed range is reported in real64 for every
    !> kind, which is what keeps an int64 column's warning byte-for-byte what it has always been
    !> even though qc_numeric_i64 now compares in int64.
    subroutine qc_numeric_report(writer, name, have_min, min_bound, min_op, have_max, max_bound, max_op, &
        data_min, data_max, n_violate, n_valid)
        type(parquet_writer), intent(in) :: writer !! open writer.
        character(len=*), intent(in) :: name !! numeric column name.
        logical, intent(in) :: have_min !! whether a min bound was declared.
        real(real64), intent(in) :: min_bound !! the declared min bound.
        character(len=*), intent(in) :: min_op !! its comparison operator.
        logical, intent(in) :: have_max !! whether a max bound was declared.
        real(real64), intent(in) :: max_bound !! the declared max bound.
        character(len=*), intent(in) :: max_op !! its comparison operator.
        real(real64), intent(in) :: data_min !! smallest valid element seen.
        real(real64), intent(in) :: data_max !! largest valid element seen.
        integer(int64), intent(in) :: n_violate !! how many valid elements violate a bound.
        integer(int64), intent(in) :: n_valid !! how many elements were checked at all.
        character(len=:), allocatable :: bounds_desc, fmt_num, fmt_num2, fmt_int, fmt_int2

        bounds_desc = ""
        if (have_min) then
            call parquet_qc_format_real(min_bound, fmt_num)
            bounds_desc = "min " // trim(min_op) // " " // fmt_num
        end if
        if (have_max) then
            if (len_trim(bounds_desc) > 0) bounds_desc = bounds_desc // ", "
            call parquet_qc_format_real(max_bound, fmt_num)
            bounds_desc = bounds_desc // "max " // trim(max_op) // " " // fmt_num
        end if

        call parquet_qc_format_real(data_min, fmt_num)
        call parquet_qc_format_real(data_max, fmt_num2)
        call parquet_qc_format_int(n_violate, fmt_int)
        call parquet_qc_format_int(n_valid, fmt_int2)
        call parquet_emit_warning("qc violation for column '" // trim(name) // "': declared " // bounds_desc // &
            ", data range [" // fmt_num // ", " // fmt_num2 // "], " // &
            fmt_int // " of " // fmt_int2 // " valid element(s) out of range")
    end subroutine qc_numeric_report
    !> Converts a real64 qc bound to an exactly-equivalent int64 one, reporting .false. when it
    !> has no exact int64 equivalent (a fractional bound, or one beyond int64's range).
    !>
    !> Only qc_numeric_i64 needs this, and only because a real64 bound cannot represent every
    !> int64 value: past 2^53 the widening `real(value, real64)` this checker used to apply to
    !> every element rounds, so a value could be judged against the bound wrongly. int32,
    !> float32 and float64 values all convert to real64 exactly, so their specifics keep
    !> comparing in real64 with no change in results.
    !>
    !> A fractional bound reaches an int64 column's values only when the SCHEMA type is a float
    !> one and the caller passed int64 values (parquet_qc_numeric_bound rejects a fractional
    !> bound outright for an int32/int64 schema type), in which case comparing in real64 -- what
    !> the caller gets when this returns .false. -- is the correct reading of the declaration.
    logical function qc_bound_as_int64(bound, ibound) result(exact)
        real(real64), intent(in) :: bound !! the declared bound, as parsed into real64.
        integer(int64), intent(out) :: ibound !! its int64 equivalent (0 when not exact).
        real(real64) :: rounded

        exact = .false.
        ibound = 0_int64
        rounded = anint(bound)
        if (bound /= rounded) return
        if (rounded < -real(huge(0_int64), real64) .or. rounded >= real(huge(0_int64), real64)) return
        ibound = int(rounded, kind=int64)
        exact = .true.
    end function qc_bound_as_int64
    !> parquet_qc_numeric_satisfies' int64 counterpart: applies one declared qc bound's
    !> comparison operator to a value and bound that are both exact int64s.
    logical function qc_int64_satisfies(value, bound, op) result(ok)
        integer(int64), intent(in) :: value !! value being checked.
        integer(int64), intent(in) :: bound !! declared qc: min:/max: bound, as an exact int64.
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
    end function qc_int64_satisfies
    !> parquet_check_qc_numeric's int32 specific -- see the generic's own doc-comment above.
    subroutine qc_numeric_i32(writer, name, values, is_valid_flat)
        type(parquet_writer), intent(in) :: writer !! open (schema-enforced) writer.
        character(len=*), intent(in) :: name !! numeric column name.
        integer(int32), intent(in) :: values(:) !! flattened column values, in their own kind.
        logical, intent(in), optional :: is_valid_flat(:) !! flattened validity mask; absent = every element counts.
        logical :: have_min, have_max, any_valid, ok, use_mask
        real(real64) :: min_bound, max_bound, data_min, data_max, v
        character(len=:), allocatable :: min_op, max_op
        integer(int64) :: i, n_valid, n_violate

        if (.not. qc_numeric_bounds(writer, name, have_min, min_bound, min_op, have_max, max_bound, max_op)) return

        use_mask = present(is_valid_flat)
        any_valid = .false.
        n_valid = 0_int64
        n_violate = 0_int64
        data_min = 0.0_real64
        data_max = 0.0_real64
        do i = 1_int64, size(values, kind=int64)
            if (use_mask) then
                if (.not. is_valid_flat(i)) cycle
            end if
            v = real(values(i), kind=real64)
            n_valid = n_valid + 1
            if (.not. any_valid) then
                data_min = v
                data_max = v
                any_valid = .true.
            else
                data_min = min(data_min, v)
                data_max = max(data_max, v)
            end if

            ok = .true.
            if (have_min) ok = ok .and. parquet_qc_numeric_satisfies(v, min_bound, min_op)
            if (have_max) ok = ok .and. parquet_qc_numeric_satisfies(v, max_bound, max_op)
            if (.not. ok) n_violate = n_violate + 1
        end do
        if (.not. any_valid .or. n_violate == 0) return
        call qc_numeric_report(writer, name, have_min, min_bound, min_op, have_max, max_bound, max_op, &
            data_min, data_max, n_violate, n_valid)
    end subroutine qc_numeric_i32
    !> parquet_check_qc_numeric's int64 specific -- see the generic's own doc-comment above.
    !> The only specific that compares in something other than real64: see qc_bound_as_int64 for
    !> why, and for when it falls back to the real64 comparison the other three always use.
    subroutine qc_numeric_i64(writer, name, values, is_valid_flat)
        type(parquet_writer), intent(in) :: writer !! open (schema-enforced) writer.
        character(len=*), intent(in) :: name !! numeric column name.
        integer(int64), intent(in) :: values(:) !! flattened column values, in their own kind.
        logical, intent(in), optional :: is_valid_flat(:) !! flattened validity mask; absent = every element counts.
        logical :: have_min, have_max, any_valid, ok, use_mask, exact_min, exact_max
        real(real64) :: min_bound, max_bound, data_min, data_max, v
        character(len=:), allocatable :: min_op, max_op
        integer(int64) :: i, n_valid, n_violate, imin, imax, iv

        if (.not. qc_numeric_bounds(writer, name, have_min, min_bound, min_op, have_max, max_bound, max_op)) return

        exact_min = .false.
        exact_max = .false.
        imin = 0_int64
        imax = 0_int64
        if (have_min) exact_min = qc_bound_as_int64(min_bound, imin)
        if (have_max) exact_max = qc_bound_as_int64(max_bound, imax)

        use_mask = present(is_valid_flat)
        any_valid = .false.
        n_valid = 0_int64
        n_violate = 0_int64
        data_min = 0.0_real64
        data_max = 0.0_real64
        do i = 1_int64, size(values, kind=int64)
            if (use_mask) then
                if (.not. is_valid_flat(i)) cycle
            end if
            iv = values(i)
            v = real(iv, kind=real64)
            n_valid = n_valid + 1
            if (.not. any_valid) then
                data_min = v
                data_max = v
                any_valid = .true.
            else
                data_min = min(data_min, v)
                data_max = max(data_max, v)
            end if

            ok = .true.
            if (have_min) then
                if (exact_min) then
                    ok = ok .and. qc_int64_satisfies(iv, imin, min_op)
                else
                    ok = ok .and. parquet_qc_numeric_satisfies(v, min_bound, min_op)
                end if
            end if
            if (have_max) then
                if (exact_max) then
                    ok = ok .and. qc_int64_satisfies(iv, imax, max_op)
                else
                    ok = ok .and. parquet_qc_numeric_satisfies(v, max_bound, max_op)
                end if
            end if
            if (.not. ok) n_violate = n_violate + 1
        end do
        if (.not. any_valid .or. n_violate == 0) return
        call qc_numeric_report(writer, name, have_min, min_bound, min_op, have_max, max_bound, max_op, &
            data_min, data_max, n_violate, n_valid)
    end subroutine qc_numeric_i64
    !> parquet_check_qc_numeric's float32 specific -- see the generic's own doc-comment above.
    subroutine qc_numeric_r32(writer, name, values, is_valid_flat)
        type(parquet_writer), intent(in) :: writer !! open (schema-enforced) writer.
        character(len=*), intent(in) :: name !! numeric column name.
        real(real32), intent(in) :: values(:) !! flattened column values, in their own kind.
        logical, intent(in), optional :: is_valid_flat(:) !! flattened validity mask; absent = every element counts.
        logical :: have_min, have_max, any_valid, ok, use_mask
        real(real64) :: min_bound, max_bound, data_min, data_max, v
        character(len=:), allocatable :: min_op, max_op
        integer(int64) :: i, n_valid, n_violate

        if (.not. qc_numeric_bounds(writer, name, have_min, min_bound, min_op, have_max, max_bound, max_op)) return

        use_mask = present(is_valid_flat)
        any_valid = .false.
        n_valid = 0_int64
        n_violate = 0_int64
        data_min = 0.0_real64
        data_max = 0.0_real64
        do i = 1_int64, size(values, kind=int64)
            if (use_mask) then
                if (.not. is_valid_flat(i)) cycle
            end if
            v = real(values(i), kind=real64)
            n_valid = n_valid + 1
            if (.not. any_valid) then
                data_min = v
                data_max = v
                any_valid = .true.
            else
                data_min = min(data_min, v)
                data_max = max(data_max, v)
            end if

            ok = .true.
            if (have_min) ok = ok .and. parquet_qc_numeric_satisfies(v, min_bound, min_op)
            if (have_max) ok = ok .and. parquet_qc_numeric_satisfies(v, max_bound, max_op)
            if (.not. ok) n_violate = n_violate + 1
        end do
        if (.not. any_valid .or. n_violate == 0) return
        call qc_numeric_report(writer, name, have_min, min_bound, min_op, have_max, max_bound, max_op, &
            data_min, data_max, n_violate, n_valid)
    end subroutine qc_numeric_r32
    !> parquet_check_qc_numeric's float64 specific -- see the generic's own doc-comment above.
    subroutine qc_numeric_r64(writer, name, values, is_valid_flat)
        type(parquet_writer), intent(in) :: writer !! open (schema-enforced) writer.
        character(len=*), intent(in) :: name !! numeric column name.
        real(real64), intent(in) :: values(:) !! flattened column values, in their own kind.
        logical, intent(in), optional :: is_valid_flat(:) !! flattened validity mask; absent = every element counts.
        logical :: have_min, have_max, any_valid, ok, use_mask
        real(real64) :: min_bound, max_bound, data_min, data_max, v
        character(len=:), allocatable :: min_op, max_op
        integer(int64) :: i, n_valid, n_violate

        if (.not. qc_numeric_bounds(writer, name, have_min, min_bound, min_op, have_max, max_bound, max_op)) return

        use_mask = present(is_valid_flat)
        any_valid = .false.
        n_valid = 0_int64
        n_violate = 0_int64
        data_min = 0.0_real64
        data_max = 0.0_real64
        do i = 1_int64, size(values, kind=int64)
            if (use_mask) then
                if (.not. is_valid_flat(i)) cycle
            end if
            v = values(i)
            n_valid = n_valid + 1
            if (.not. any_valid) then
                data_min = v
                data_max = v
                any_valid = .true.
            else
                data_min = min(data_min, v)
                data_max = max(data_max, v)
            end if

            ok = .true.
            if (have_min) ok = ok .and. parquet_qc_numeric_satisfies(v, min_bound, min_op)
            if (have_max) ok = ok .and. parquet_qc_numeric_satisfies(v, max_bound, max_op)
            if (.not. ok) n_violate = n_violate + 1
        end do
        if (.not. any_valid .or. n_violate == 0) return
        call qc_numeric_report(writer, name, have_min, min_bound, min_op, have_max, max_bound, max_op, &
            data_min, data_max, n_violate, n_valid)
    end subroutine qc_numeric_r64
    ! ---- Flat write workers (one per type x whole-column/chunked) ----
    !
    ! Each of the twenty public numeric write specifics does its own schema/type/col_size
    ! prologue and then hands the values to one of these workers, which own everything from the
    ! row-mask decision onward. A scalar specific and its matrix sibling share a worker: `flat`
    ! is an assumed-size dummy, so a rank-2 actual argument sequence-associates with it directly
    ! and the matrix forms no longer build a `reshape(values, [n])` copy to pass down.
    !
    ! THE UNMASKED FAST PATH. parquet_writer_whole_column_mask / parquet_check_row_group_row_count
    ! report whether a row mask is actually in force. When one is not -- the overwhelmingly common
    ! case -- there is nothing to remove, so `vals` simply points at the caller's own array and no
    ! mask, no expanded element mask and no `pack` copy are built at all. When one is, the packed
    ! copy is built exactly as before and `vals` points at that instead. Pointing rather than
    ! copying is what keeps the two paths sharing one tail; `flat` and `valid` therefore carry the
    ! TARGET attribute, and the pointers are used only within the worker, never returned.
    !
    ! A caller that supplied no `is_valid` leaves `vmask` disassociated, which makes it ABSENT at
    ! every `optional` dummy it is passed on to (F2018 15.5.2.12) -- so the qc checker and
    ! parquet_make_valid_buf_write take their own no-mask paths without this code branching again.

    !> Whole-column write worker for parquet_write_int32_column/_matrix_column.
    subroutine write_int32_flat(writer, name, flat, nelem, asize, nrows, valid)
        type(parquet_writer), intent(inout) :: writer !! open writer.
        character(len=*), intent(in) :: name !! column name.
        integer(int32), intent(in), target :: flat(*) !! asize*nrows values in (element, row)-major order.
        integer(int64), intent(in) :: nelem !! number of values in `flat`, i.e. asize*nrows.
        integer(int64), intent(in) :: asize !! per-row element count (1 for a scalar column).
        integer(int64), intent(in) :: nrows !! pre-mask row count.
        logical, intent(in), optional, target :: valid(*) !! `nelem`-long validity mask, or absent.
        integer(int32), allocatable, target :: values_c(:) !! masked copy; unused on the fast path.
        logical, allocatable :: row_mask(:), elem_mask(:)
        logical, allocatable, target :: valid_c(:) !! masked copy of `valid`; unused on the fast path.
        integer(int32), pointer :: vals(:) !! the values actually written.
        logical, pointer :: vmask(:) !! the validity mask actually written, or disassociated.
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        integer(int64) :: nkeep
        logical :: masked

        call parquet_writer_whole_column_mask(writer, name, nrows, row_mask, masked)
        nullify(vmask)
        if (masked) then
            elem_mask = parquet_mask_expand_block(row_mask, asize)
            values_c = pack(flat(1:nelem), elem_mask)
            vals => values_c
            nkeep = count(row_mask, kind=int64)
            if (present(valid)) then
                valid_c = pack(valid(1:nelem), elem_mask)
                vmask => valid_c
            end if
        else
            vals => flat(1:nelem)
            nkeep = nrows
            if (present(valid)) vmask => valid(1:nelem)
        end if

        if (associated(vmask)) then
            call parquet_check_protected(writer, name, vmask)
            call parquet_check_qc_miss(writer, name, vmask)
        end if
        if (writer%qc .and. writer%is_schema_enforced) call parquet_check_qc_numeric(writer, name, vals, vmask)
        call parquet_make_valid_buf_write(vmask, valid_buf, valid_ptr)
        call parquet_check_row_count(writer, name, nkeep)
        call parquet_append_as_schema_int32(writer, name, vals, nkeep, asize, valid_ptr)
    end subroutine write_int32_flat
    !> Whole-column write worker for parquet_write_int64_column/_matrix_column; see write_int32_flat.
    subroutine write_int64_flat(writer, name, flat, nelem, asize, nrows, valid)
        type(parquet_writer), intent(inout) :: writer !! open writer.
        character(len=*), intent(in) :: name !! column name.
        integer(int64), intent(in), target :: flat(*) !! asize*nrows values in (element, row)-major order.
        integer(int64), intent(in) :: nelem !! number of values in `flat`, i.e. asize*nrows.
        integer(int64), intent(in) :: asize !! per-row element count (1 for a scalar column).
        integer(int64), intent(in) :: nrows !! pre-mask row count.
        logical, intent(in), optional, target :: valid(*) !! `nelem`-long validity mask, or absent.
        integer(int64), allocatable, target :: values_c(:) !! masked copy; unused on the fast path.
        logical, allocatable :: row_mask(:), elem_mask(:)
        logical, allocatable, target :: valid_c(:) !! masked copy of `valid`; unused on the fast path.
        integer(int64), pointer :: vals(:) !! the values actually written.
        logical, pointer :: vmask(:) !! the validity mask actually written, or disassociated.
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        integer(int64) :: nkeep
        logical :: masked

        call parquet_writer_whole_column_mask(writer, name, nrows, row_mask, masked)
        nullify(vmask)
        if (masked) then
            elem_mask = parquet_mask_expand_block(row_mask, asize)
            values_c = pack(flat(1:nelem), elem_mask)
            vals => values_c
            nkeep = count(row_mask, kind=int64)
            if (present(valid)) then
                valid_c = pack(valid(1:nelem), elem_mask)
                vmask => valid_c
            end if
        else
            vals => flat(1:nelem)
            nkeep = nrows
            if (present(valid)) vmask => valid(1:nelem)
        end if

        if (associated(vmask)) then
            call parquet_check_protected(writer, name, vmask)
            call parquet_check_qc_miss(writer, name, vmask)
        end if
        if (writer%qc .and. writer%is_schema_enforced) call parquet_check_qc_numeric(writer, name, vals, vmask)
        call parquet_make_valid_buf_write(vmask, valid_buf, valid_ptr)
        call parquet_check_row_count(writer, name, nkeep)
        call parquet_append_as_schema_int64(writer, name, vals, nkeep, asize, valid_ptr)
    end subroutine write_int64_flat
    !> Whole-column write worker for parquet_write_float32_column/_matrix_column; see write_int32_flat.
    subroutine write_float32_flat(writer, name, flat, nelem, asize, nrows, valid)
        type(parquet_writer), intent(inout) :: writer !! open writer.
        character(len=*), intent(in) :: name !! column name.
        real(real32), intent(in), target :: flat(*) !! asize*nrows values in (element, row)-major order.
        integer(int64), intent(in) :: nelem !! number of values in `flat`, i.e. asize*nrows.
        integer(int64), intent(in) :: asize !! per-row element count (1 for a scalar column).
        integer(int64), intent(in) :: nrows !! pre-mask row count.
        logical, intent(in), optional, target :: valid(*) !! `nelem`-long validity mask, or absent.
        real(real32), allocatable, target :: values_c(:) !! masked copy; unused on the fast path.
        logical, allocatable :: row_mask(:), elem_mask(:)
        logical, allocatable, target :: valid_c(:) !! masked copy of `valid`; unused on the fast path.
        real(real32), pointer :: vals(:) !! the values actually written.
        logical, pointer :: vmask(:) !! the validity mask actually written, or disassociated.
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        integer(int64) :: nkeep
        logical :: masked

        call parquet_writer_whole_column_mask(writer, name, nrows, row_mask, masked)
        nullify(vmask)
        if (masked) then
            elem_mask = parquet_mask_expand_block(row_mask, asize)
            values_c = pack(flat(1:nelem), elem_mask)
            vals => values_c
            nkeep = count(row_mask, kind=int64)
            if (present(valid)) then
                valid_c = pack(valid(1:nelem), elem_mask)
                vmask => valid_c
            end if
        else
            vals => flat(1:nelem)
            nkeep = nrows
            if (present(valid)) vmask => valid(1:nelem)
        end if

        if (associated(vmask)) then
            call parquet_check_protected(writer, name, vmask)
            call parquet_check_qc_miss(writer, name, vmask)
        end if
        if (writer%qc .and. writer%is_schema_enforced) call parquet_check_qc_numeric(writer, name, vals, vmask)
        call parquet_make_valid_buf_write(vmask, valid_buf, valid_ptr)
        call parquet_check_row_count(writer, name, nkeep)
        call parquet_append_as_schema_float32(writer, name, vals, nkeep, asize, valid_ptr)
    end subroutine write_float32_flat
    !> Whole-column write worker for parquet_write_float64_column/_matrix_column; see write_int32_flat.
    subroutine write_float64_flat(writer, name, flat, nelem, asize, nrows, valid)
        type(parquet_writer), intent(inout) :: writer !! open writer.
        character(len=*), intent(in) :: name !! column name.
        real(real64), intent(in), target :: flat(*) !! asize*nrows values in (element, row)-major order.
        integer(int64), intent(in) :: nelem !! number of values in `flat`, i.e. asize*nrows.
        integer(int64), intent(in) :: asize !! per-row element count (1 for a scalar column).
        integer(int64), intent(in) :: nrows !! pre-mask row count.
        logical, intent(in), optional, target :: valid(*) !! `nelem`-long validity mask, or absent.
        real(real64), allocatable, target :: values_c(:) !! masked copy; unused on the fast path.
        logical, allocatable :: row_mask(:), elem_mask(:)
        logical, allocatable, target :: valid_c(:) !! masked copy of `valid`; unused on the fast path.
        real(real64), pointer :: vals(:) !! the values actually written.
        logical, pointer :: vmask(:) !! the validity mask actually written, or disassociated.
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        integer(int64) :: nkeep
        logical :: masked

        call parquet_writer_whole_column_mask(writer, name, nrows, row_mask, masked)
        nullify(vmask)
        if (masked) then
            elem_mask = parquet_mask_expand_block(row_mask, asize)
            values_c = pack(flat(1:nelem), elem_mask)
            vals => values_c
            nkeep = count(row_mask, kind=int64)
            if (present(valid)) then
                valid_c = pack(valid(1:nelem), elem_mask)
                vmask => valid_c
            end if
        else
            vals => flat(1:nelem)
            nkeep = nrows
            if (present(valid)) vmask => valid(1:nelem)
        end if

        if (associated(vmask)) then
            call parquet_check_protected(writer, name, vmask)
            call parquet_check_qc_miss(writer, name, vmask)
        end if
        if (writer%qc .and. writer%is_schema_enforced) call parquet_check_qc_numeric(writer, name, vals, vmask)
        call parquet_make_valid_buf_write(vmask, valid_buf, valid_ptr)
        call parquet_check_row_count(writer, name, nkeep)
        call parquet_append_as_schema_float64(writer, name, vals, nkeep, asize, valid_ptr)
    end subroutine write_float64_flat
    !> Whole-column write worker for parquet_write_logical_column/_matrix_column.
    !>
    !> Unlike the four numeric workers there is no fast path that avoids a copy entirely: the C
    !> binding takes one int8 per element, so a Fortran LOGICAL array always has to be converted.
    !> The saving is that the conversion now writes only the elements that survive the mask,
    !> instead of converting every element into a full-length buffer and then packing that buffer
    !> down -- two passes and two full-size allocations become one of each.
    subroutine write_logical_flat(writer, name, flat, nelem, asize, nrows, valid)
        type(parquet_writer), intent(inout) :: writer !! open writer.
        character(len=*), intent(in) :: name !! column name.
        logical, intent(in) :: flat(*) !! asize*nrows values in (element, row)-major order.
        integer(int64), intent(in) :: nelem !! number of values in `flat`, i.e. asize*nrows.
        integer(int64), intent(in) :: asize !! per-row element count (1 for a scalar column).
        integer(int64), intent(in) :: nrows !! pre-mask row count.
        logical, intent(in), optional, target :: valid(*) !! `nelem`-long validity mask, or absent.
        integer(c_int8_t), allocatable :: bool_data(:)
        logical, allocatable :: row_mask(:), elem_mask(:)
        logical, allocatable, target :: valid_c(:) !! masked copy of `valid`; unused on the fast path.
        logical, pointer :: vmask(:) !! the validity mask actually written, or disassociated.
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        character(len=:), allocatable :: outname !! parquet_resolve_output_name scratch.
        integer(int64) :: i, k, nkeep
        logical :: masked

        call parquet_writer_whole_column_mask(writer, name, nrows, row_mask, masked)
        nullify(vmask)
        if (masked) then
            elem_mask = parquet_mask_expand_block(row_mask, asize)
            allocate(bool_data(count(elem_mask, kind=int64)))
            k = 0_int64
            do i = 1_int64, nelem
                if (.not. elem_mask(i)) cycle
                k = k + 1_int64
                bool_data(k) = merge(1_c_int8_t, 0_c_int8_t, flat(i))
            end do
            nkeep = count(row_mask, kind=int64)
            if (present(valid)) then
                valid_c = pack(valid(1:nelem), elem_mask)
                vmask => valid_c
            end if
        else
            allocate(bool_data(nelem))
            do i = 1_int64, nelem
                bool_data(i) = merge(1_c_int8_t, 0_c_int8_t, flat(i))
            end do
            nkeep = nrows
            if (present(valid)) vmask => valid(1:nelem)
        end if

        if (associated(vmask)) then
            call parquet_check_protected(writer, name, vmask)
            call parquet_check_qc_miss(writer, name, vmask)
        end if
        call parquet_make_valid_buf_write(vmask, valid_buf, valid_ptr)
        call parquet_check_row_count(writer, name, nkeep)

        call parquet_resolve_output_name(writer, name, outname)
        call parquet_append_bool8_column(writer%handle, trim(outname)//char(0), bool_data, nkeep, asize, valid_ptr)
    end subroutine write_logical_flat
    !> Row-group-chunked write worker for parquet_write_int32_column_chunk/_matrix_column_chunk;
    !> see write_int32_flat for the fast path, which is the same. The differences from the
    !> whole-column worker are the row-count check (a chunk is checked against the open row
    !> group, not the file), the mark-written bookkeeping, and appending to the open row group
    !> rather than to the file as a whole.
    subroutine write_int32_chunk_flat(writer, name, flat, nelem, asize, nrows, valid)
        type(parquet_writer), intent(inout) :: writer !! open writer with a row group open.
        character(len=*), intent(in) :: name !! column name.
        integer(int32), intent(in), target :: flat(*) !! asize*nrows values in (element, row)-major order.
        integer(int64), intent(in) :: nelem !! number of values in `flat`, i.e. asize*nrows.
        integer(int64), intent(in) :: asize !! per-row element count (1 for a scalar column).
        integer(int64), intent(in) :: nrows !! this chunk's pre-mask row count.
        logical, intent(in), optional, target :: valid(*) !! `nelem`-long validity mask, or absent.
        integer(int32), allocatable, target :: values_c(:) !! masked copy; unused on the fast path.
        logical, allocatable :: row_mask(:), elem_mask(:)
        logical, allocatable, target :: valid_c(:) !! masked copy of `valid`; unused on the fast path.
        integer(int32), pointer :: vals(:) !! the values actually written.
        logical, pointer :: vmask(:) !! the validity mask actually written, or disassociated.
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        character(len=:), allocatable :: outname !! parquet_resolve_output_name scratch.
        integer(int64) :: nkeep
        logical :: masked

        call parquet_check_row_group_row_count(writer, name, nrows, row_mask, masked)
        nullify(vmask)
        if (masked) then
            elem_mask = parquet_mask_expand_block(row_mask, asize)
            values_c = pack(flat(1:nelem), elem_mask)
            vals => values_c
            nkeep = count(row_mask, kind=int64)
            if (present(valid)) then
                valid_c = pack(valid(1:nelem), elem_mask)
                vmask => valid_c
            end if
        else
            vals => flat(1:nelem)
            nkeep = nrows
            if (present(valid)) vmask => valid(1:nelem)
        end if

        if (associated(vmask)) then
            call parquet_check_protected(writer, name, vmask)
            call parquet_check_qc_miss(writer, name, vmask)
        end if
        if (writer%qc .and. writer%is_schema_enforced) call parquet_check_qc_numeric(writer, name, vals, vmask)
        call parquet_make_valid_buf_write(vmask, valid_buf, valid_ptr)
        call parquet_chunk_mark_written_if_first(writer, name)

        call parquet_resolve_output_name(writer, name, outname)
        if (nkeep > 0) call parquet_append_int32_column_chunk(writer%handle, &
            trim(outname)//char(0), vals, asize, valid_ptr)
    end subroutine write_int32_chunk_flat
    !> Chunked write worker for parquet_write_int64_column_chunk/_matrix_column_chunk; see
    !> write_int32_chunk_flat.
    subroutine write_int64_chunk_flat(writer, name, flat, nelem, asize, nrows, valid)
        type(parquet_writer), intent(inout) :: writer !! open writer with a row group open.
        character(len=*), intent(in) :: name !! column name.
        integer(int64), intent(in), target :: flat(*) !! asize*nrows values in (element, row)-major order.
        integer(int64), intent(in) :: nelem !! number of values in `flat`, i.e. asize*nrows.
        integer(int64), intent(in) :: asize !! per-row element count (1 for a scalar column).
        integer(int64), intent(in) :: nrows !! this chunk's pre-mask row count.
        logical, intent(in), optional, target :: valid(*) !! `nelem`-long validity mask, or absent.
        integer(int64), allocatable, target :: values_c(:) !! masked copy; unused on the fast path.
        logical, allocatable :: row_mask(:), elem_mask(:)
        logical, allocatable, target :: valid_c(:) !! masked copy of `valid`; unused on the fast path.
        integer(int64), pointer :: vals(:) !! the values actually written.
        logical, pointer :: vmask(:) !! the validity mask actually written, or disassociated.
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        character(len=:), allocatable :: outname !! parquet_resolve_output_name scratch.
        integer(int64) :: nkeep
        logical :: masked

        call parquet_check_row_group_row_count(writer, name, nrows, row_mask, masked)
        nullify(vmask)
        if (masked) then
            elem_mask = parquet_mask_expand_block(row_mask, asize)
            values_c = pack(flat(1:nelem), elem_mask)
            vals => values_c
            nkeep = count(row_mask, kind=int64)
            if (present(valid)) then
                valid_c = pack(valid(1:nelem), elem_mask)
                vmask => valid_c
            end if
        else
            vals => flat(1:nelem)
            nkeep = nrows
            if (present(valid)) vmask => valid(1:nelem)
        end if

        if (associated(vmask)) then
            call parquet_check_protected(writer, name, vmask)
            call parquet_check_qc_miss(writer, name, vmask)
        end if
        if (writer%qc .and. writer%is_schema_enforced) call parquet_check_qc_numeric(writer, name, vals, vmask)
        call parquet_make_valid_buf_write(vmask, valid_buf, valid_ptr)
        call parquet_chunk_mark_written_if_first(writer, name)

        call parquet_resolve_output_name(writer, name, outname)
        if (nkeep > 0) call parquet_append_int64_column_chunk(writer%handle, &
            trim(outname)//char(0), vals, asize, valid_ptr)
    end subroutine write_int64_chunk_flat
    !> Chunked write worker for parquet_write_float32_column_chunk/_matrix_column_chunk; see
    !> write_int32_chunk_flat.
    subroutine write_float32_chunk_flat(writer, name, flat, nelem, asize, nrows, valid)
        type(parquet_writer), intent(inout) :: writer !! open writer with a row group open.
        character(len=*), intent(in) :: name !! column name.
        real(real32), intent(in), target :: flat(*) !! asize*nrows values in (element, row)-major order.
        integer(int64), intent(in) :: nelem !! number of values in `flat`, i.e. asize*nrows.
        integer(int64), intent(in) :: asize !! per-row element count (1 for a scalar column).
        integer(int64), intent(in) :: nrows !! this chunk's pre-mask row count.
        logical, intent(in), optional, target :: valid(*) !! `nelem`-long validity mask, or absent.
        real(real32), allocatable, target :: values_c(:) !! masked copy; unused on the fast path.
        logical, allocatable :: row_mask(:), elem_mask(:)
        logical, allocatable, target :: valid_c(:) !! masked copy of `valid`; unused on the fast path.
        real(real32), pointer :: vals(:) !! the values actually written.
        logical, pointer :: vmask(:) !! the validity mask actually written, or disassociated.
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        character(len=:), allocatable :: outname !! parquet_resolve_output_name scratch.
        integer(int64) :: nkeep
        logical :: masked

        call parquet_check_row_group_row_count(writer, name, nrows, row_mask, masked)
        nullify(vmask)
        if (masked) then
            elem_mask = parquet_mask_expand_block(row_mask, asize)
            values_c = pack(flat(1:nelem), elem_mask)
            vals => values_c
            nkeep = count(row_mask, kind=int64)
            if (present(valid)) then
                valid_c = pack(valid(1:nelem), elem_mask)
                vmask => valid_c
            end if
        else
            vals => flat(1:nelem)
            nkeep = nrows
            if (present(valid)) vmask => valid(1:nelem)
        end if

        if (associated(vmask)) then
            call parquet_check_protected(writer, name, vmask)
            call parquet_check_qc_miss(writer, name, vmask)
        end if
        if (writer%qc .and. writer%is_schema_enforced) call parquet_check_qc_numeric(writer, name, vals, vmask)
        call parquet_make_valid_buf_write(vmask, valid_buf, valid_ptr)
        call parquet_chunk_mark_written_if_first(writer, name)

        call parquet_resolve_output_name(writer, name, outname)
        if (nkeep > 0) call parquet_append_float32_column_chunk(writer%handle, &
            trim(outname)//char(0), vals, asize, valid_ptr)
    end subroutine write_float32_chunk_flat
    !> Chunked write worker for parquet_write_float64_column_chunk/_matrix_column_chunk; see
    !> write_int32_chunk_flat.
    subroutine write_float64_chunk_flat(writer, name, flat, nelem, asize, nrows, valid)
        type(parquet_writer), intent(inout) :: writer !! open writer with a row group open.
        character(len=*), intent(in) :: name !! column name.
        real(real64), intent(in), target :: flat(*) !! asize*nrows values in (element, row)-major order.
        integer(int64), intent(in) :: nelem !! number of values in `flat`, i.e. asize*nrows.
        integer(int64), intent(in) :: asize !! per-row element count (1 for a scalar column).
        integer(int64), intent(in) :: nrows !! this chunk's pre-mask row count.
        logical, intent(in), optional, target :: valid(*) !! `nelem`-long validity mask, or absent.
        real(real64), allocatable, target :: values_c(:) !! masked copy; unused on the fast path.
        logical, allocatable :: row_mask(:), elem_mask(:)
        logical, allocatable, target :: valid_c(:) !! masked copy of `valid`; unused on the fast path.
        real(real64), pointer :: vals(:) !! the values actually written.
        logical, pointer :: vmask(:) !! the validity mask actually written, or disassociated.
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        character(len=:), allocatable :: outname !! parquet_resolve_output_name scratch.
        integer(int64) :: nkeep
        logical :: masked

        call parquet_check_row_group_row_count(writer, name, nrows, row_mask, masked)
        nullify(vmask)
        if (masked) then
            elem_mask = parquet_mask_expand_block(row_mask, asize)
            values_c = pack(flat(1:nelem), elem_mask)
            vals => values_c
            nkeep = count(row_mask, kind=int64)
            if (present(valid)) then
                valid_c = pack(valid(1:nelem), elem_mask)
                vmask => valid_c
            end if
        else
            vals => flat(1:nelem)
            nkeep = nrows
            if (present(valid)) vmask => valid(1:nelem)
        end if

        if (associated(vmask)) then
            call parquet_check_protected(writer, name, vmask)
            call parquet_check_qc_miss(writer, name, vmask)
        end if
        if (writer%qc .and. writer%is_schema_enforced) call parquet_check_qc_numeric(writer, name, vals, vmask)
        call parquet_make_valid_buf_write(vmask, valid_buf, valid_ptr)
        call parquet_chunk_mark_written_if_first(writer, name)

        call parquet_resolve_output_name(writer, name, outname)
        if (nkeep > 0) call parquet_append_float64_column_chunk(writer%handle, &
            trim(outname)//char(0), vals, asize, valid_ptr)
    end subroutine write_float64_chunk_flat
    !> Chunked write worker for parquet_write_logical_column_chunk/_matrix_column_chunk; see
    !> write_logical_flat for why this one always converts rather than passing values through.
    subroutine write_logical_chunk_flat(writer, name, flat, nelem, asize, nrows, valid)
        type(parquet_writer), intent(inout) :: writer !! open writer with a row group open.
        character(len=*), intent(in) :: name !! column name.
        logical, intent(in) :: flat(*) !! asize*nrows values in (element, row)-major order.
        integer(int64), intent(in) :: nelem !! number of values in `flat`, i.e. asize*nrows.
        integer(int64), intent(in) :: asize !! per-row element count (1 for a scalar column).
        integer(int64), intent(in) :: nrows !! this chunk's pre-mask row count.
        logical, intent(in), optional, target :: valid(*) !! `nelem`-long validity mask, or absent.
        integer(c_int8_t), allocatable :: bool_data(:)
        logical, allocatable :: row_mask(:), elem_mask(:)
        logical, allocatable, target :: valid_c(:) !! masked copy of `valid`; unused on the fast path.
        logical, pointer :: vmask(:) !! the validity mask actually written, or disassociated.
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        character(len=:), allocatable :: outname !! parquet_resolve_output_name scratch.
        integer(int64) :: i, k, nkeep
        logical :: masked

        call parquet_check_row_group_row_count(writer, name, nrows, row_mask, masked)
        nullify(vmask)
        if (masked) then
            elem_mask = parquet_mask_expand_block(row_mask, asize)
            allocate(bool_data(count(elem_mask, kind=int64)))
            k = 0_int64
            do i = 1_int64, nelem
                if (.not. elem_mask(i)) cycle
                k = k + 1_int64
                bool_data(k) = merge(1_c_int8_t, 0_c_int8_t, flat(i))
            end do
            nkeep = count(row_mask, kind=int64)
            if (present(valid)) then
                valid_c = pack(valid(1:nelem), elem_mask)
                vmask => valid_c
            end if
        else
            allocate(bool_data(nelem))
            do i = 1_int64, nelem
                bool_data(i) = merge(1_c_int8_t, 0_c_int8_t, flat(i))
            end do
            nkeep = nrows
            if (present(valid)) vmask => valid(1:nelem)
        end if

        if (associated(vmask)) then
            call parquet_check_protected(writer, name, vmask)
            call parquet_check_qc_miss(writer, name, vmask)
        end if
        call parquet_make_valid_buf_write(vmask, valid_buf, valid_ptr)
        call parquet_chunk_mark_written_if_first(writer, name)

        call parquet_resolve_output_name(writer, name, outname)
        if (nkeep > 0) call parquet_append_bool8_column_chunk(writer%handle, &
            trim(outname)//char(0), bool_data, asize, valid_ptr)
    end subroutine write_logical_chunk_flat
    module procedure parquet_write_int32_column
        integer :: idx
        integer(int64) :: asize, nrows
        character(len=:), allocatable :: ctx !! writer_context_suffix scratch.
        type(writer_lock) :: lk !! Releases writer's concurrency guard on every exit path (FINAL).
        call check_writer_open(writer)
        call lk%claim(writer)

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

        call write_int32_flat(writer, name, values, size(values, kind=int64), asize, nrows, is_valid)
    end procedure parquet_write_int32_column
    module procedure parquet_write_int32_matrix_column
        integer :: idx
        integer(int64) :: asize, nrows
        character(len=:), allocatable :: ctx !! writer_context_suffix scratch.
        type(writer_lock) :: lk !! Releases writer's concurrency guard on every exit path (FINAL).
        call check_writer_open(writer)
        call lk%claim(writer)

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

        call write_int32_flat(writer, name, values, size(values, kind=int64), asize, nrows, is_valid)
    end procedure parquet_write_int32_matrix_column
    module procedure parquet_write_int64_column
        integer :: idx
        integer(int64) :: asize, nrows
        character(len=:), allocatable :: ctx !! writer_context_suffix scratch.
        type(writer_lock) :: lk !! Releases writer's concurrency guard on every exit path (FINAL).
        call check_writer_open(writer)
        call lk%claim(writer)

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

        call write_int64_flat(writer, name, values, size(values, kind=int64), asize, nrows, is_valid)
    end procedure parquet_write_int64_column
    module procedure parquet_write_int64_matrix_column
        integer :: idx
        integer(int64) :: asize, nrows
        character(len=:), allocatable :: ctx !! writer_context_suffix scratch.
        type(writer_lock) :: lk !! Releases writer's concurrency guard on every exit path (FINAL).
        call check_writer_open(writer)
        call lk%claim(writer)

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

        call write_int64_flat(writer, name, values, size(values, kind=int64), asize, nrows, is_valid)
    end procedure parquet_write_int64_matrix_column
    module procedure parquet_write_float32_column
        integer :: idx
        integer(int64) :: asize, nrows
        character(len=:), allocatable :: ctx !! writer_context_suffix scratch.
        type(writer_lock) :: lk !! Releases writer's concurrency guard on every exit path (FINAL).
        call check_writer_open(writer)
        call lk%claim(writer)

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

        call write_float32_flat(writer, name, values, size(values, kind=int64), asize, nrows, is_valid)
    end procedure parquet_write_float32_column
    module procedure parquet_write_float32_matrix_column
        integer :: idx
        integer(int64) :: asize, nrows
        character(len=:), allocatable :: ctx !! writer_context_suffix scratch.
        type(writer_lock) :: lk !! Releases writer's concurrency guard on every exit path (FINAL).
        call check_writer_open(writer)
        call lk%claim(writer)

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

        call write_float32_flat(writer, name, values, size(values, kind=int64), asize, nrows, is_valid)
    end procedure parquet_write_float32_matrix_column
    module procedure parquet_write_float64_column
        integer :: idx
        integer(int64) :: asize, nrows
        character(len=:), allocatable :: ctx !! writer_context_suffix scratch.
        type(writer_lock) :: lk !! Releases writer's concurrency guard on every exit path (FINAL).
        call check_writer_open(writer)
        call lk%claim(writer)

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

        call write_float64_flat(writer, name, values, size(values, kind=int64), asize, nrows, is_valid)
    end procedure parquet_write_float64_column
    module procedure parquet_write_float64_matrix_column
        integer :: idx
        integer(int64) :: asize, nrows
        character(len=:), allocatable :: ctx !! writer_context_suffix scratch.
        type(writer_lock) :: lk !! Releases writer's concurrency guard on every exit path (FINAL).
        call check_writer_open(writer)
        call lk%claim(writer)

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

        call write_float64_flat(writer, name, values, size(values, kind=int64), asize, nrows, is_valid)
    end procedure parquet_write_float64_matrix_column
    module procedure parquet_write_logical_column
        integer :: idx
        integer(int64) :: asize, nrows
        character(len=:), allocatable :: ctx !! writer_context_suffix scratch.
        type(writer_lock) :: lk !! Releases writer's concurrency guard on every exit path (FINAL).
        call check_writer_open(writer)
        call lk%claim(writer)

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

        call write_logical_flat(writer, name, values, size(values, kind=int64), asize, nrows, is_valid)
    end procedure parquet_write_logical_column
    module procedure parquet_write_logical_matrix_column
        integer :: idx
        integer(int64) :: asize, nrows
        character(len=:), allocatable :: ctx !! writer_context_suffix scratch.
        type(writer_lock) :: lk !! Releases writer's concurrency guard on every exit path (FINAL).
        call check_writer_open(writer)
        call lk%claim(writer)

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

        call write_logical_flat(writer, name, values, size(values, kind=int64), asize, nrows, is_valid)
    end procedure parquet_write_logical_matrix_column
    module procedure parquet_write_int32_column_chunk
        integer :: idx
        integer(int64) :: asize, nrows
        character(len=:), allocatable :: ctx !! writer_context_suffix scratch.
        type(writer_lock) :: lk !! Releases writer's concurrency guard on every exit path (FINAL).
        call check_writer_open(writer)
        call lk%claim(writer)

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

        call write_int32_chunk_flat(writer, name, values, size(values, kind=int64), asize, nrows, is_valid)
    end procedure parquet_write_int32_column_chunk
    module procedure parquet_write_int32_matrix_column_chunk
        integer :: idx
        integer(int64) :: asize, nrows
        character(len=:), allocatable :: ctx !! writer_context_suffix scratch.
        type(writer_lock) :: lk !! Releases writer's concurrency guard on every exit path (FINAL).
        call check_writer_open(writer)
        call lk%claim(writer)

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

        call write_int32_chunk_flat(writer, name, values, size(values, kind=int64), asize, nrows, is_valid)
    end procedure parquet_write_int32_matrix_column_chunk
    module procedure parquet_write_int64_column_chunk
        integer :: idx
        integer(int64) :: asize, nrows
        character(len=:), allocatable :: ctx !! writer_context_suffix scratch.
        type(writer_lock) :: lk !! Releases writer's concurrency guard on every exit path (FINAL).
        call check_writer_open(writer)
        call lk%claim(writer)

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

        call write_int64_chunk_flat(writer, name, values, size(values, kind=int64), asize, nrows, is_valid)
    end procedure parquet_write_int64_column_chunk
    module procedure parquet_write_int64_matrix_column_chunk
        integer :: idx
        integer(int64) :: asize, nrows
        character(len=:), allocatable :: ctx !! writer_context_suffix scratch.
        type(writer_lock) :: lk !! Releases writer's concurrency guard on every exit path (FINAL).
        call check_writer_open(writer)
        call lk%claim(writer)

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

        call write_int64_chunk_flat(writer, name, values, size(values, kind=int64), asize, nrows, is_valid)
    end procedure parquet_write_int64_matrix_column_chunk
    module procedure parquet_write_float32_column_chunk
        integer :: idx
        integer(int64) :: asize, nrows
        character(len=:), allocatable :: ctx !! writer_context_suffix scratch.
        type(writer_lock) :: lk !! Releases writer's concurrency guard on every exit path (FINAL).
        call check_writer_open(writer)
        call lk%claim(writer)

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

        call write_float32_chunk_flat(writer, name, values, size(values, kind=int64), asize, nrows, is_valid)
    end procedure parquet_write_float32_column_chunk
    module procedure parquet_write_float32_matrix_column_chunk
        integer :: idx
        integer(int64) :: asize, nrows
        character(len=:), allocatable :: ctx !! writer_context_suffix scratch.
        type(writer_lock) :: lk !! Releases writer's concurrency guard on every exit path (FINAL).
        call check_writer_open(writer)
        call lk%claim(writer)

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

        call write_float32_chunk_flat(writer, name, values, size(values, kind=int64), asize, nrows, is_valid)
    end procedure parquet_write_float32_matrix_column_chunk
    module procedure parquet_write_float64_column_chunk
        integer :: idx
        integer(int64) :: asize, nrows
        character(len=:), allocatable :: ctx !! writer_context_suffix scratch.
        type(writer_lock) :: lk !! Releases writer's concurrency guard on every exit path (FINAL).
        call check_writer_open(writer)
        call lk%claim(writer)

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

        call write_float64_chunk_flat(writer, name, values, size(values, kind=int64), asize, nrows, is_valid)
    end procedure parquet_write_float64_column_chunk
    module procedure parquet_write_float64_matrix_column_chunk
        integer :: idx
        integer(int64) :: asize, nrows
        character(len=:), allocatable :: ctx !! writer_context_suffix scratch.
        type(writer_lock) :: lk !! Releases writer's concurrency guard on every exit path (FINAL).
        call check_writer_open(writer)
        call lk%claim(writer)

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

        call write_float64_chunk_flat(writer, name, values, size(values, kind=int64), asize, nrows, is_valid)
    end procedure parquet_write_float64_matrix_column_chunk
    module procedure parquet_write_logical_column_chunk
        integer :: idx
        integer(int64) :: asize, nrows
        character(len=:), allocatable :: ctx !! writer_context_suffix scratch.
        type(writer_lock) :: lk !! Releases writer's concurrency guard on every exit path (FINAL).
        call check_writer_open(writer)
        call lk%claim(writer)

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

        call write_logical_chunk_flat(writer, name, values, size(values, kind=int64), asize, nrows, is_valid)
    end procedure parquet_write_logical_column_chunk
    module procedure parquet_write_logical_matrix_column_chunk
        integer :: idx
        integer(int64) :: asize, nrows
        character(len=:), allocatable :: ctx !! writer_context_suffix scratch.
        type(writer_lock) :: lk !! Releases writer's concurrency guard on every exit path (FINAL).
        call check_writer_open(writer)
        call lk%claim(writer)

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

        call write_logical_chunk_flat(writer, name, values, size(values, kind=int64), asize, nrows, is_valid)
    end procedure parquet_write_logical_matrix_column_chunk

end submodule parquet_write_numeric
