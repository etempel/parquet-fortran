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
    use ieee_arithmetic, only: ieee_is_nan
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
    function parquet_narrow_int64_to_int32(name, src, context) result(dst)
        character(len=*), intent(in) :: name !! column name, named only in the error-stop message.
        integer(int64), intent(in) :: src(:) !! values to narrow.
        character(len=*), intent(in), optional :: context !! calling procedure for the message; default
        !! "parquet_write_column". The chunked path passes its own name, so a caller is never told to
        !! look at a procedure it did not call.
        integer(int32), allocatable :: dst(:) !! narrowed values.
        integer(int64) :: i
        character(len=:), allocatable :: ctx

        ctx = "parquet_write_column"
        if (present(context)) ctx = context
        allocate(dst(size(src, kind=int64)))
        do i = 1_int64, size(src, kind=int64)
            if (src(i) < -huge(0_int32) - 1_int64 .or. src(i) > huge(0_int32)) then
                error stop ctx // ": int64 value out of int32 range for column " // trim(name)
            end if
            dst(i) = int(src(i), kind=int32)
        end do
    end function parquet_narrow_int64_to_int32
    !> .true. when `value` is exactly integral. The ONLY way this file may ask that question
    !> about caller-supplied data -- never `value == anint(value)` written out at the call site.
    !>
    !> `anint(NaN)` RAISES the IEEE invalid-operation flag, which is fatal under a compiler whose
    !> traps are unmasked: nagfor halts the process with "Arithmetic exception: Floating invalid
    !> operation", turning what should be a clean `error stop` naming the offending column into a
    !> crash naming nothing. `int(NaN)` traps the same way; comparisons (`/=`, `<`), `abs()` and
    !> `ieee_is_nan` do not, which is why the NaN test can be made safely and must come first.
    !>
    !> It must also be its own statement, not an operand: Fortran does not short-circuit `.and.`
    !> or `.or.`, so `ieee_is_nan(v) .or. v /= anint(v)` still evaluates the anint and still traps.
    !> gfortran/ifx/flang mask the traps by default and are unaffected either way, so nothing but a
    !> nagfor run will report a regression here.
    elemental logical function parquet_float_is_integral(value) result(integral)
        real(real64), intent(in) :: value !! value to test; may be NaN, Inf, or anything else.
        integral = .false.
        if (ieee_is_nan(value)) return
        integral = (value == anint(value))
    end function parquet_float_is_integral
    !> Converts float64 `src` to int32, error stopping if any value is
    !> non-integral or outside int32's representable range. Used when a
    !> schema declares a column int32 but the caller's parquet_write_column
    !> values are float32/float64.
    function parquet_float64_to_int32(name, src, context) result(dst)
        character(len=*), intent(in) :: name !! column name, named only in the error-stop message.
        real(real64), intent(in) :: src(:) !! values to convert.
        character(len=*), intent(in), optional :: context !! calling procedure for the message; see
        !! parquet_narrow_int64_to_int32.
        integer(int32), allocatable :: dst(:) !! converted values.
        integer(int64) :: i
        character(len=:), allocatable :: ctx

        ctx = "parquet_write_column"
        if (present(context)) ctx = context
        allocate(dst(size(src, kind=int64)))
        do i = 1_int64, size(src, kind=int64)
            if (.not. parquet_float_is_integral(src(i))) then
                error stop ctx // ": non-integral float value written to int column " // trim(name)
            end if
            if (src(i) < -real(huge(0_int32), real64) - 1.0_real64 .or. src(i) > real(huge(0_int32), real64)) then
                error stop ctx // ": float value out of int32 range for column " // trim(name)
            end if
            dst(i) = int(src(i), kind=int32)
        end do
    end function parquet_float64_to_int32
    !> Converts float64 `src` to int64, error stopping if any value is
    !> non-integral or outside int64's representable range. Used when a
    !> schema declares a column int64 but the caller's parquet_write_column
    !> values are float32/float64.
    function parquet_float64_to_int64(name, src, context) result(dst)
        character(len=*), intent(in) :: name !! column name, named only in the error-stop message.
        real(real64), intent(in) :: src(:) !! values to convert.
        character(len=*), intent(in), optional :: context !! calling procedure for the message; see
        !! parquet_narrow_int64_to_int32.
        integer(int64), allocatable :: dst(:) !! converted values.
        integer(int64) :: i
        character(len=:), allocatable :: ctx

        ctx = "parquet_write_column"
        if (present(context)) ctx = context
        allocate(dst(size(src, kind=int64)))
        do i = 1_int64, size(src, kind=int64)
            if (.not. parquet_float_is_integral(src(i))) then
                error stop ctx // ": non-integral float value written to int column " // trim(name)
            end if
            if (src(i) < -real(huge(0_int64), real64) .or. src(i) >= real(huge(0_int64), real64)) then
                error stop ctx // ": float value out of int64 range for column " // trim(name)
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
    !> Chunked counterpart of parquet_append_as_schema_int32: appends ONE ROW GROUP's int32 values,
    !> converting them first if the schema declares this column as a different numeric type. The
    !> conversion rules are deliberately identical to the whole-column family's, so
    !> parquet_write_column_chunk accepts exactly the values parquet_write_column would for the same
    !> schema -- the two families exist separately only because the C bindings differ (a chunk
    !> append takes no row count: the open row group already fixed it). Keep them in step; a
    !> divergence here is a silent behaviour difference between the two write paths, which is
    !> precisely what this family was added to remove.
    subroutine parquet_append_as_schema_chunk_int32(writer, name, values, asize, valid_ptr)
        type(parquet_writer), intent(in) :: writer !! open writer with a row group open.
        character(len=*), intent(in) :: name !! column name.
        integer(int32), intent(in) :: values(:) !! this row group's values, as passed to parquet_write_column_chunk.
        integer(c_long_long), intent(in) :: asize !! vector-column element count (1 for a scalar column).
        type(c_ptr), intent(in) :: valid_ptr !! validity buffer, or c_null_ptr.
        character(len=:), allocatable :: schema_type
        integer(int64), allocatable :: i64values(:)
        real(real32), allocatable :: f32values(:)
        real(real64), allocatable :: f64values(:)
        character(len=:), allocatable :: outname !! parquet_resolve_output_name scratch.

        call parquet_get_schema_type(writer, name, schema_type)
        call parquet_resolve_output_name(writer, name, outname)
        select case (schema_type)
        case ("int64")
            allocate(i64values(size(values, kind=int64)))
            i64values = int(values, kind=int64)
            call parquet_append_int64_column_chunk(writer%handle, trim(outname)//char(0), i64values, asize, valid_ptr)
        case ("float32")
            allocate(f32values(size(values, kind=int64)))
            f32values = real(values, kind=real32)
            call parquet_append_float32_column_chunk(writer%handle, trim(outname)//char(0), f32values, asize, valid_ptr)
        case ("float64")
            allocate(f64values(size(values, kind=int64)))
            f64values = real(values, kind=real64)
            call parquet_append_float64_column_chunk(writer%handle, trim(outname)//char(0), f64values, asize, valid_ptr)
        case default
            call parquet_append_int32_column_chunk(writer%handle, trim(outname)//char(0), values, asize, valid_ptr)
        end select
    end subroutine parquet_append_as_schema_chunk_int32
    !> Chunked counterpart of parquet_append_as_schema_int64; see
    !> parquet_append_as_schema_chunk_int32 for why this family exists and must stay in step.
    subroutine parquet_append_as_schema_chunk_int64(writer, name, values, asize, valid_ptr)
        type(parquet_writer), intent(in) :: writer !! open writer with a row group open.
        character(len=*), intent(in) :: name !! column name.
        integer(int64), intent(in) :: values(:) !! this row group's values.
        integer(c_long_long), intent(in) :: asize !! vector-column element count (1 for a scalar column).
        type(c_ptr), intent(in) :: valid_ptr !! validity buffer, or c_null_ptr.
        character(len=:), allocatable :: schema_type
        integer(int32), allocatable :: i32values(:)
        real(real32), allocatable :: f32values(:)
        real(real64), allocatable :: f64values(:)
        character(len=:), allocatable :: outname !! parquet_resolve_output_name scratch.

        call parquet_get_schema_type(writer, name, schema_type)
        call parquet_resolve_output_name(writer, name, outname)
        select case (schema_type)
        case ("int32")
            i32values = parquet_narrow_int64_to_int32(name, values, context="parquet_write_column_chunk")
            call parquet_append_int32_column_chunk(writer%handle, trim(outname)//char(0), i32values, asize, valid_ptr)
        case ("float32")
            allocate(f32values(size(values, kind=int64)))
            f32values = real(values, kind=real32)
            call parquet_append_float32_column_chunk(writer%handle, trim(outname)//char(0), f32values, asize, valid_ptr)
        case ("float64")
            allocate(f64values(size(values, kind=int64)))
            f64values = real(values, kind=real64)
            call parquet_append_float64_column_chunk(writer%handle, trim(outname)//char(0), f64values, asize, valid_ptr)
        case default
            call parquet_append_int64_column_chunk(writer%handle, trim(outname)//char(0), values, asize, valid_ptr)
        end select
    end subroutine parquet_append_as_schema_chunk_int64
    !> Chunked counterpart of parquet_append_as_schema_float32; see
    !> parquet_append_as_schema_chunk_int32 for why this family exists and must stay in step.
    subroutine parquet_append_as_schema_chunk_float32(writer, name, values, asize, valid_ptr)
        type(parquet_writer), intent(in) :: writer !! open writer with a row group open.
        character(len=*), intent(in) :: name !! column name.
        real(real32), intent(in) :: values(:) !! this row group's values.
        integer(c_long_long), intent(in) :: asize !! vector-column element count (1 for a scalar column).
        type(c_ptr), intent(in) :: valid_ptr !! validity buffer, or c_null_ptr.
        character(len=:), allocatable :: schema_type
        integer(int32), allocatable :: i32values(:)
        integer(int64), allocatable :: i64values(:)
        real(real64), allocatable :: f64values(:)
        character(len=:), allocatable :: outname !! parquet_resolve_output_name scratch.

        call parquet_get_schema_type(writer, name, schema_type)
        call parquet_resolve_output_name(writer, name, outname)
        select case (schema_type)
        case ("int32")
            i32values = parquet_float64_to_int32(name, real(values, kind=real64), context="parquet_write_column_chunk")
            call parquet_append_int32_column_chunk(writer%handle, trim(outname)//char(0), i32values, asize, valid_ptr)
        case ("int64")
            i64values = parquet_float64_to_int64(name, real(values, kind=real64), context="parquet_write_column_chunk")
            call parquet_append_int64_column_chunk(writer%handle, trim(outname)//char(0), i64values, asize, valid_ptr)
        case ("float64")
            allocate(f64values(size(values, kind=int64)))
            f64values = real(values, kind=real64)
            call parquet_append_float64_column_chunk(writer%handle, trim(outname)//char(0), f64values, asize, valid_ptr)
        case default
            call parquet_append_float32_column_chunk(writer%handle, trim(outname)//char(0), values, asize, valid_ptr)
        end select
    end subroutine parquet_append_as_schema_chunk_float32
    !> Chunked counterpart of parquet_append_as_schema_float64; see
    !> parquet_append_as_schema_chunk_int32 for why this family exists and must stay in step.
    subroutine parquet_append_as_schema_chunk_float64(writer, name, values, asize, valid_ptr)
        type(parquet_writer), intent(in) :: writer !! open writer with a row group open.
        character(len=*), intent(in) :: name !! column name.
        real(real64), intent(in) :: values(:) !! this row group's values.
        integer(c_long_long), intent(in) :: asize !! vector-column element count (1 for a scalar column).
        type(c_ptr), intent(in) :: valid_ptr !! validity buffer, or c_null_ptr.
        character(len=:), allocatable :: schema_type
        integer(int32), allocatable :: i32values(:)
        integer(int64), allocatable :: i64values(:)
        real(real32), allocatable :: f32values(:)
        character(len=:), allocatable :: outname !! parquet_resolve_output_name scratch.

        call parquet_get_schema_type(writer, name, schema_type)
        call parquet_resolve_output_name(writer, name, outname)
        select case (schema_type)
        case ("int32")
            i32values = parquet_float64_to_int32(name, values, context="parquet_write_column_chunk")
            call parquet_append_int32_column_chunk(writer%handle, trim(outname)//char(0), i32values, asize, valid_ptr)
        case ("int64")
            i64values = parquet_float64_to_int64(name, values, context="parquet_write_column_chunk")
            call parquet_append_int64_column_chunk(writer%handle, trim(outname)//char(0), i64values, asize, valid_ptr)
        case ("float32")
            allocate(f32values(size(values, kind=int64)))
            f32values = real(values, kind=real32)
            call parquet_append_float32_column_chunk(writer%handle, trim(outname)//char(0), f32values, asize, valid_ptr)
        case default
            call parquet_append_float64_column_chunk(writer%handle, trim(outname)//char(0), values, asize, valid_ptr)
        end select
    end subroutine parquet_append_as_schema_chunk_float64
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

        if (parquet_float_is_integral(value) .and. abs(value) < 1.0e15_real64) then
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
        have_max, max_bound, max_op, min_raw, max_raw) result(any_bound)
        type(parquet_writer), intent(in) :: writer !! open writer.
        character(len=*), intent(in) :: name !! numeric column name.
        logical, intent(out) :: have_min !! .true. if a usable qc: min: bound was declared.
        real(real64), intent(out) :: min_bound !! the declared min bound (meaningful only if have_min).
        character(len=:), allocatable, intent(out) :: min_op !! its comparison operator, ">=" or ">".
        logical, intent(out) :: have_max !! .true. if a usable qc: max: bound was declared.
        real(real64), intent(out) :: max_bound !! the declared max bound (meaningful only if have_max).
        character(len=:), allocatable, intent(out) :: max_op !! its comparison operator, "<=" or "<".
        !> The bound TEXT as declared, for a caller that must judge it in something other than
        !! real64 -- only qc_numeric_i64 does, and only so an int64 bound past 2**53 is compared
        !! exactly rather than against a rounded copy of itself. Comes back "" when that bound was
        !! not declared or did not parse.
        character(len=:), allocatable, intent(out), optional :: min_raw
        character(len=:), allocatable, intent(out), optional :: max_raw !! as min_raw, for qc: max:.
        integer :: idx

        any_bound = .false.
        have_min = .false.
        have_max = .false.
        min_bound = 0.0_real64
        max_bound = 0.0_real64
        min_op = ""
        max_op = ""
        if (present(min_raw)) min_raw = ""
        if (present(max_raw)) max_raw = ""

        if (.not. writer%qc) return
        if (.not. writer%is_schema_enforced) return
        idx = parquet_get_defined_column_index(writer, name)
        if (idx == 0) return
        if (.not. (writer%all_columns(idx)%has_qc_min .or. writer%all_columns(idx)%has_qc_max)) return

        if (writer%all_columns(idx)%has_qc_min) then
            have_min = parquet_qc_numeric_bound( &
                writer%all_columns(idx)%qc_min_raw, writer%all_columns(idx)%data_type, min_bound)
            if (have_min) then
                min_op = trim(writer%all_columns(idx)%qc_min_op)
                if (present(min_raw)) min_raw = trim(writer%all_columns(idx)%qc_min_raw)
            end if
        end if
        if (writer%all_columns(idx)%has_qc_max) then
            have_max = parquet_qc_numeric_bound( &
                writer%all_columns(idx)%qc_max_raw, writer%all_columns(idx)%data_type, max_bound)
            if (have_max) then
                max_op = trim(writer%all_columns(idx)%qc_max_op)
                if (present(max_raw)) max_raw = trim(writer%all_columns(idx)%qc_max_raw)
            end if
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
        data_min, data_max, n_violate, n_valid, imin_bound, imax_bound, idata_min, idata_max)
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
        !> Exact int64 renderings, supplied only by qc_numeric_i64. Without them an int64 column's
        !! message is built entirely from real64 copies, so a bound or an observed value past 2**53
        !! prints as something like `0.9007199E+16` -- and prints a DIFFERENT number from the one
        !! the comparison actually used, which is the worst of both. Absent for every other type,
        !! whose values and bounds are exact in real64 anyway.
        integer(int64), intent(in), optional :: imin_bound !! exact min bound; only when it is a plain integer.
        integer(int64), intent(in), optional :: imax_bound !! exact max bound; only when it is a plain integer.
        integer(int64), intent(in), optional :: idata_min !! smallest valid element, exactly.
        integer(int64), intent(in), optional :: idata_max !! largest valid element, exactly.
        character(len=:), allocatable :: bounds_desc, fmt_num, fmt_num2, fmt_int, fmt_int2

        bounds_desc = ""
        if (have_min) then
            if (present(imin_bound)) then
                call parquet_qc_format_int(imin_bound, fmt_num)
            else
                call parquet_qc_format_real(min_bound, fmt_num)
            end if
            bounds_desc = "min " // trim(min_op) // " " // fmt_num
        end if
        if (have_max) then
            if (len_trim(bounds_desc) > 0) bounds_desc = bounds_desc // ", "
            if (present(imax_bound)) then
                call parquet_qc_format_int(imax_bound, fmt_num)
            else
                call parquet_qc_format_real(max_bound, fmt_num)
            end if
            bounds_desc = bounds_desc // "max " // trim(max_op) // " " // fmt_num
        end if

        if (present(idata_min)) then
            call parquet_qc_format_int(idata_min, fmt_num)
        else
            call parquet_qc_format_real(data_min, fmt_num)
        end if
        if (present(idata_max)) then
            call parquet_qc_format_int(idata_max, fmt_num2)
        else
            call parquet_qc_format_real(data_max, fmt_num2)
        end if
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
        character(len=:), allocatable :: min_op, max_op, min_raw, max_raw
        integer(int64) :: i, n_valid, n_violate, imin, imax, iv, idata_min, idata_max

        if (.not. qc_numeric_bounds(writer, name, have_min, min_bound, min_op, have_max, max_bound, max_op, &
            min_raw, max_raw)) return

        ! Take the exact int64 bound from the declared TEXT, not from `min_bound`/`max_bound`: those
        ! have already been through real64, where a bound past 2**53 has lost the very digits this
        ! comparison exists to respect. qc_bound_as_int64 remains the fallback and is NOT dead code
        ! -- it serves the case where the text is not a plain integer at all but the values are
        ! int64 anyway, which happens whenever a column is DECLARED float64 (so a fractional bound
        ! validates) and written from an integer(int64) array. scenario_qc_int64_values_fractional_bound
        ! is exactly that, and it is the regression guard for this fallback.
        exact_min = .false.
        exact_max = .false.
        imin = 0_int64
        imax = 0_int64
        if (have_min) then
            exact_min = parquet_qc_bound_as_int64_text(min_raw, imin)
            if (.not. exact_min) exact_min = qc_bound_as_int64(min_bound, imin)
        end if
        if (have_max) then
            exact_max = parquet_qc_bound_as_int64_text(max_raw, imax)
            if (.not. exact_max) exact_max = qc_bound_as_int64(max_bound, imax)
        end if

        use_mask = present(is_valid_flat)
        any_valid = .false.
        n_valid = 0_int64
        n_violate = 0_int64
        data_min = 0.0_real64
        data_max = 0.0_real64
        idata_min = 0_int64
        idata_max = 0_int64
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
                idata_min = iv
                idata_max = iv
                any_valid = .true.
            else
                data_min = min(data_min, v)
                data_max = max(data_max, v)
                idata_min = min(idata_min, iv)
                idata_max = max(idata_max, iv)
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
        ! A bound is rendered exactly only when it WAS judged exactly -- a fractional bound on a
        ! float-declared column still prints as the real it is. The observed range is always exact,
        ! since the values are int64 whatever the bound turned out to be.
        if (exact_min .and. exact_max) then
            call qc_numeric_report(writer, name, have_min, min_bound, min_op, have_max, max_bound, max_op, &
                data_min, data_max, n_violate, n_valid, imin_bound=imin, imax_bound=imax, &
                idata_min=idata_min, idata_max=idata_max)
        else if (exact_min) then
            call qc_numeric_report(writer, name, have_min, min_bound, min_op, have_max, max_bound, max_op, &
                data_min, data_max, n_violate, n_valid, imin_bound=imin, &
                idata_min=idata_min, idata_max=idata_max)
        else if (exact_max) then
            call qc_numeric_report(writer, name, have_min, min_bound, min_op, have_max, max_bound, max_op, &
                data_min, data_max, n_violate, n_valid, imax_bound=imax, &
                idata_min=idata_min, idata_max=idata_max)
        else
            call qc_numeric_report(writer, name, have_min, min_bound, min_op, have_max, max_bound, max_op, &
                data_min, data_max, n_violate, n_valid, idata_min=idata_min, idata_max=idata_max)
        end if
    end subroutine qc_numeric_i64
    !> parquet_check_qc_numeric's float32 specific -- see the generic's own doc-comment above.
    subroutine qc_numeric_r32(writer, name, values, is_valid_flat)
        type(parquet_writer), intent(in) :: writer !! open (schema-enforced) writer.
        character(len=*), intent(in) :: name !! numeric column name.
        real(real32), intent(in) :: values(:) !! flattened column values, in their own kind.
        logical, intent(in), optional :: is_valid_flat(:) !! flattened validity mask; absent = every element counts.
        logical :: have_min, have_max, any_valid, ok, use_mask, saw_nan, any_range
        real(real64) :: min_bound, max_bound, data_min, data_max, v, nanv
        character(len=:), allocatable :: min_op, max_op
        integer(int64) :: i, n_valid, n_violate

        if (.not. qc_numeric_bounds(writer, name, have_min, min_bound, min_op, have_max, max_bound, max_op)) return

        use_mask = present(is_valid_flat)
        any_valid = .false.
        any_range = .false.
        saw_nan = .false.
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
            any_valid = .true.
            ! **A NaN never enters the observed range, and the reason is a trap.** It is
            ! already counted as a violation -- every comparison against a NaN is false, which is
            ! what `parquet_qc_numeric_satisfies` answers -- but it is still not a number this
            ! range can order, and `min`/`max` over one compile to x86 `minsd`/`maxsd`, which
            ! raise IEEE_INVALID for a QUIET-NaN operand. nagfor unmasks the IEEE traps by
            ! default (`-ieee=stop`), so reporting the violation would kill the process instead of
            ! naming the column, in an optimised build only. Same rule as
            ! `parquet_float_is_integral` above, one line further down the same path.
            if (v /= v) then
                if (.not. saw_nan) then
                    nanv = v
                    saw_nan = .true.
                end if
            else if (.not. any_range) then
                data_min = v
                data_max = v
                any_range = .true.
            else
                data_min = min(data_min, v)
                data_max = max(data_max, v)
            end if

            ok = .true.
            if (have_min) ok = ok .and. parquet_qc_numeric_satisfies(v, min_bound, min_op)
            if (have_max) ok = ok .and. parquet_qc_numeric_satisfies(v, max_bound, max_op)
            if (.not. ok) n_violate = n_violate + 1
        end do
        ! Every valid element was a NaN, so the range is one: reported as such rather than as
        ! the zero the accumulators still hold, which would read as a real observed range.
        if (.not. any_range .and. saw_nan) then
            data_min = nanv
            data_max = nanv
        end if
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
        logical :: have_min, have_max, any_valid, ok, use_mask, saw_nan, any_range
        real(real64) :: min_bound, max_bound, data_min, data_max, v, nanv
        character(len=:), allocatable :: min_op, max_op
        integer(int64) :: i, n_valid, n_violate

        if (.not. qc_numeric_bounds(writer, name, have_min, min_bound, min_op, have_max, max_bound, max_op)) return

        use_mask = present(is_valid_flat)
        any_valid = .false.
        any_range = .false.
        saw_nan = .false.
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
            any_valid = .true.
            ! **A NaN never enters the observed range, and the reason is a trap.** It is
            ! already counted as a violation -- every comparison against a NaN is false, which is
            ! what `parquet_qc_numeric_satisfies` answers -- but it is still not a number this
            ! range can order, and `min`/`max` over one compile to x86 `minsd`/`maxsd`, which
            ! raise IEEE_INVALID for a QUIET-NaN operand. nagfor unmasks the IEEE traps by
            ! default (`-ieee=stop`), so reporting the violation would kill the process instead of
            ! naming the column, in an optimised build only. Same rule as
            ! `parquet_float_is_integral` above, one line further down the same path.
            if (v /= v) then
                if (.not. saw_nan) then
                    nanv = v
                    saw_nan = .true.
                end if
            else if (.not. any_range) then
                data_min = v
                data_max = v
                any_range = .true.
            else
                data_min = min(data_min, v)
                data_max = max(data_max, v)
            end if

            ok = .true.
            if (have_min) ok = ok .and. parquet_qc_numeric_satisfies(v, min_bound, min_op)
            if (have_max) ok = ok .and. parquet_qc_numeric_satisfies(v, max_bound, max_op)
            if (.not. ok) n_violate = n_violate + 1
        end do
        ! Every valid element was a NaN, so the range is one: reported as such rather than as
        ! the zero the accumulators still hold, which would read as a real observed range.
        if (.not. any_range .and. saw_nan) then
            data_min = nanv
            data_max = nanv
        end if
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
    ! copying is what keeps the two paths sharing one tail; `flat` therefore carries the TARGET
    ! attribute, and the pointers are used only within the worker, never returned.
    !
    ! `valid` DELIBERATELY DOES NOT CARRY TARGET, and must not be given it back. nagfor 7.2
    ! miscompiles a call that passes an ABSENT optional actual to a dummy that is at once
    ! `optional`, assumed-size and `target`: the caller faults in its own argument setup, or
    ! computes a negative size for a compiler temporary and then hangs forever inside its own
    ! error termination. Every whole-column write here has that shape, so it cost
    ! 243 of 824 error scenarios; dropping TARGET from this one dummy is the entire fix, and with
    ! it the whole suite passes under -C=dangling AND -C=calls together, which is what puts both
    ! in fpm.toml's nagdeb feature. They are not two defects: either check alone is enough to
    ! trigger this one, and the TARGET attribute is the only ingredient that matters. The price is
    ! that the unmasked path copies the mask (`valid_c = valid(1:nelem)`) instead of pointing at
    ! it, which is why both branches now route the mask through `valid_c`. gfortran, ifx and flang
    ! are unaffected either way. See feature_nag_runtime_checks.md, Report 1.
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
        logical, intent(in), optional :: valid(*) !! `nelem`-long validity mask, or absent. No TARGET: see above.
        integer(int32), allocatable, target :: values_c(:) !! masked copy; unused on the fast path.
        logical, allocatable :: row_mask(:), elem_mask(:)
        logical, allocatable, target :: valid_c(:) !! the `valid` copy vmask points at, packed or plain.
        integer(int32), pointer :: vals(:) !! the values actually written.
        logical, pointer :: vmask(:) !! the validity mask actually written, or disassociated.
        logical :: protected !! .true. if this column is protected, so the mask above is erased.
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        integer(int64) :: nkeep
        logical :: masked

        call parquet_writer_whole_column_mask(writer, name, nrows, row_mask, masked)
        protected = .false.
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
            if (present(valid)) then
                valid_c = valid(1:nelem)
                vmask => valid_c
            end if
        end if

        if (associated(vmask)) then
            call parquet_check_protected(writer, name, vmask, protected)
            call parquet_check_qc_miss(writer, name, vmask)
            ! A protected column's mask is all-.true. by the check above, so it carries no
            ! information -- drop it, and the write proceeds exactly as an unmasked one (no
            ! validity buffer built, field non-nullable). See parquet_check_protected.
            if (protected) nullify(vmask)
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
        logical, intent(in), optional :: valid(*) !! `nelem`-long validity mask, or absent. No TARGET: see above.
        integer(int64), allocatable, target :: values_c(:) !! masked copy; unused on the fast path.
        logical, allocatable :: row_mask(:), elem_mask(:)
        logical, allocatable, target :: valid_c(:) !! the `valid` copy vmask points at, packed or plain.
        integer(int64), pointer :: vals(:) !! the values actually written.
        logical, pointer :: vmask(:) !! the validity mask actually written, or disassociated.
        logical :: protected !! .true. if this column is protected, so the mask above is erased.
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        integer(int64) :: nkeep
        logical :: masked

        call parquet_writer_whole_column_mask(writer, name, nrows, row_mask, masked)
        protected = .false.
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
            if (present(valid)) then
                valid_c = valid(1:nelem)
                vmask => valid_c
            end if
        end if

        if (associated(vmask)) then
            call parquet_check_protected(writer, name, vmask, protected)
            call parquet_check_qc_miss(writer, name, vmask)
            ! A protected column's mask is all-.true. by the check above, so it carries no
            ! information -- drop it, and the write proceeds exactly as an unmasked one (no
            ! validity buffer built, field non-nullable). See parquet_check_protected.
            if (protected) nullify(vmask)
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
        logical, intent(in), optional :: valid(*) !! `nelem`-long validity mask, or absent. No TARGET: see above.
        real(real32), allocatable, target :: values_c(:) !! masked copy; unused on the fast path.
        logical, allocatable :: row_mask(:), elem_mask(:)
        logical, allocatable, target :: valid_c(:) !! the `valid` copy vmask points at, packed or plain.
        real(real32), pointer :: vals(:) !! the values actually written.
        logical, pointer :: vmask(:) !! the validity mask actually written, or disassociated.
        logical :: protected !! .true. if this column is protected, so the mask above is erased.
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        integer(int64) :: nkeep
        logical :: masked

        call parquet_writer_whole_column_mask(writer, name, nrows, row_mask, masked)
        protected = .false.
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
            if (present(valid)) then
                valid_c = valid(1:nelem)
                vmask => valid_c
            end if
        end if

        if (associated(vmask)) then
            call parquet_check_protected(writer, name, vmask, protected)
            call parquet_check_qc_miss(writer, name, vmask)
            ! A protected column's mask is all-.true. by the check above, so it carries no
            ! information -- drop it, and the write proceeds exactly as an unmasked one (no
            ! validity buffer built, field non-nullable). See parquet_check_protected.
            if (protected) nullify(vmask)
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
        logical, intent(in), optional :: valid(*) !! `nelem`-long validity mask, or absent. No TARGET: see above.
        real(real64), allocatable, target :: values_c(:) !! masked copy; unused on the fast path.
        logical, allocatable :: row_mask(:), elem_mask(:)
        logical, allocatable, target :: valid_c(:) !! the `valid` copy vmask points at, packed or plain.
        real(real64), pointer :: vals(:) !! the values actually written.
        logical, pointer :: vmask(:) !! the validity mask actually written, or disassociated.
        logical :: protected !! .true. if this column is protected, so the mask above is erased.
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        integer(int64) :: nkeep
        logical :: masked

        call parquet_writer_whole_column_mask(writer, name, nrows, row_mask, masked)
        protected = .false.
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
            if (present(valid)) then
                valid_c = valid(1:nelem)
                vmask => valid_c
            end if
        end if

        if (associated(vmask)) then
            call parquet_check_protected(writer, name, vmask, protected)
            call parquet_check_qc_miss(writer, name, vmask)
            ! A protected column's mask is all-.true. by the check above, so it carries no
            ! information -- drop it, and the write proceeds exactly as an unmasked one (no
            ! validity buffer built, field non-nullable). See parquet_check_protected.
            if (protected) nullify(vmask)
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
        logical, intent(in), optional :: valid(*) !! `nelem`-long validity mask, or absent. No TARGET: see above.
        integer(c_int8_t), allocatable :: bool_data(:)
        logical, allocatable :: row_mask(:), elem_mask(:)
        logical, allocatable, target :: valid_c(:) !! the `valid` copy vmask points at, packed or plain.
        logical, pointer :: vmask(:) !! the validity mask actually written, or disassociated.
        logical :: protected !! .true. if this column is protected, so the mask above is erased.
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        character(len=:), allocatable :: outname !! parquet_resolve_output_name scratch.
        integer(int64) :: i, k, nkeep
        logical :: masked

        call parquet_writer_whole_column_mask(writer, name, nrows, row_mask, masked)
        protected = .false.
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
            if (present(valid)) then
                valid_c = valid(1:nelem)
                vmask => valid_c
            end if
        end if

        if (associated(vmask)) then
            call parquet_check_protected(writer, name, vmask, protected)
            call parquet_check_qc_miss(writer, name, vmask)
            ! A protected column's mask is all-.true. by the check above, so it carries no
            ! information -- drop it, and the write proceeds exactly as an unmasked one (no
            ! validity buffer built, field non-nullable). See parquet_check_protected.
            if (protected) nullify(vmask)
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
        logical, intent(in), optional :: valid(*) !! `nelem`-long validity mask, or absent. No TARGET: see above.
        integer(int32), allocatable, target :: values_c(:) !! masked copy; unused on the fast path.
        logical, allocatable :: row_mask(:), elem_mask(:)
        logical, allocatable, target :: valid_c(:) !! the `valid` copy vmask points at, packed or plain.
        integer(int32), pointer :: vals(:) !! the values actually written.
        logical, pointer :: vmask(:) !! the validity mask actually written, or disassociated.
        logical :: protected !! .true. if this column is protected, so the mask above is erased.
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        integer(int64) :: nkeep
        logical :: masked

        call parquet_check_row_group_row_count(writer, name, nrows, row_mask, masked)
        protected = .false.
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
            if (present(valid)) then
                valid_c = valid(1:nelem)
                vmask => valid_c
            end if
        end if

        if (associated(vmask)) then
            call parquet_check_protected(writer, name, vmask, protected)
            call parquet_check_qc_miss(writer, name, vmask)
            ! A protected column's mask is all-.true. by the check above, so it carries no
            ! information -- drop it, and the write proceeds exactly as an unmasked one (no
            ! validity buffer built, field non-nullable). See parquet_check_protected.
            if (protected) nullify(vmask)
        end if
        if (writer%qc .and. writer%is_schema_enforced) call parquet_check_qc_numeric(writer, name, vals, vmask)
        call parquet_make_valid_buf_write(vmask, valid_buf, valid_ptr)
        call parquet_chunk_mark_written_if_first(writer, name)

        if (nkeep > 0) call parquet_append_as_schema_chunk_int32(writer, name, vals, asize, valid_ptr)
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
        logical, intent(in), optional :: valid(*) !! `nelem`-long validity mask, or absent. No TARGET: see above.
        integer(int64), allocatable, target :: values_c(:) !! masked copy; unused on the fast path.
        logical, allocatable :: row_mask(:), elem_mask(:)
        logical, allocatable, target :: valid_c(:) !! the `valid` copy vmask points at, packed or plain.
        integer(int64), pointer :: vals(:) !! the values actually written.
        logical, pointer :: vmask(:) !! the validity mask actually written, or disassociated.
        logical :: protected !! .true. if this column is protected, so the mask above is erased.
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        integer(int64) :: nkeep
        logical :: masked

        call parquet_check_row_group_row_count(writer, name, nrows, row_mask, masked)
        protected = .false.
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
            if (present(valid)) then
                valid_c = valid(1:nelem)
                vmask => valid_c
            end if
        end if

        if (associated(vmask)) then
            call parquet_check_protected(writer, name, vmask, protected)
            call parquet_check_qc_miss(writer, name, vmask)
            ! A protected column's mask is all-.true. by the check above, so it carries no
            ! information -- drop it, and the write proceeds exactly as an unmasked one (no
            ! validity buffer built, field non-nullable). See parquet_check_protected.
            if (protected) nullify(vmask)
        end if
        if (writer%qc .and. writer%is_schema_enforced) call parquet_check_qc_numeric(writer, name, vals, vmask)
        call parquet_make_valid_buf_write(vmask, valid_buf, valid_ptr)
        call parquet_chunk_mark_written_if_first(writer, name)

        if (nkeep > 0) call parquet_append_as_schema_chunk_int64(writer, name, vals, asize, valid_ptr)
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
        logical, intent(in), optional :: valid(*) !! `nelem`-long validity mask, or absent. No TARGET: see above.
        real(real32), allocatable, target :: values_c(:) !! masked copy; unused on the fast path.
        logical, allocatable :: row_mask(:), elem_mask(:)
        logical, allocatable, target :: valid_c(:) !! the `valid` copy vmask points at, packed or plain.
        real(real32), pointer :: vals(:) !! the values actually written.
        logical, pointer :: vmask(:) !! the validity mask actually written, or disassociated.
        logical :: protected !! .true. if this column is protected, so the mask above is erased.
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        integer(int64) :: nkeep
        logical :: masked

        call parquet_check_row_group_row_count(writer, name, nrows, row_mask, masked)
        protected = .false.
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
            if (present(valid)) then
                valid_c = valid(1:nelem)
                vmask => valid_c
            end if
        end if

        if (associated(vmask)) then
            call parquet_check_protected(writer, name, vmask, protected)
            call parquet_check_qc_miss(writer, name, vmask)
            ! A protected column's mask is all-.true. by the check above, so it carries no
            ! information -- drop it, and the write proceeds exactly as an unmasked one (no
            ! validity buffer built, field non-nullable). See parquet_check_protected.
            if (protected) nullify(vmask)
        end if
        if (writer%qc .and. writer%is_schema_enforced) call parquet_check_qc_numeric(writer, name, vals, vmask)
        call parquet_make_valid_buf_write(vmask, valid_buf, valid_ptr)
        call parquet_chunk_mark_written_if_first(writer, name)

        if (nkeep > 0) call parquet_append_as_schema_chunk_float32(writer, name, vals, asize, valid_ptr)
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
        logical, intent(in), optional :: valid(*) !! `nelem`-long validity mask, or absent. No TARGET: see above.
        real(real64), allocatable, target :: values_c(:) !! masked copy; unused on the fast path.
        logical, allocatable :: row_mask(:), elem_mask(:)
        logical, allocatable, target :: valid_c(:) !! the `valid` copy vmask points at, packed or plain.
        real(real64), pointer :: vals(:) !! the values actually written.
        logical, pointer :: vmask(:) !! the validity mask actually written, or disassociated.
        logical :: protected !! .true. if this column is protected, so the mask above is erased.
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        integer(int64) :: nkeep
        logical :: masked

        call parquet_check_row_group_row_count(writer, name, nrows, row_mask, masked)
        protected = .false.
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
            if (present(valid)) then
                valid_c = valid(1:nelem)
                vmask => valid_c
            end if
        end if

        if (associated(vmask)) then
            call parquet_check_protected(writer, name, vmask, protected)
            call parquet_check_qc_miss(writer, name, vmask)
            ! A protected column's mask is all-.true. by the check above, so it carries no
            ! information -- drop it, and the write proceeds exactly as an unmasked one (no
            ! validity buffer built, field non-nullable). See parquet_check_protected.
            if (protected) nullify(vmask)
        end if
        if (writer%qc .and. writer%is_schema_enforced) call parquet_check_qc_numeric(writer, name, vals, vmask)
        call parquet_make_valid_buf_write(vmask, valid_buf, valid_ptr)
        call parquet_chunk_mark_written_if_first(writer, name)

        if (nkeep > 0) call parquet_append_as_schema_chunk_float64(writer, name, vals, asize, valid_ptr)
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
        logical, intent(in), optional :: valid(*) !! `nelem`-long validity mask, or absent. No TARGET: see above.
        integer(c_int8_t), allocatable :: bool_data(:)
        logical, allocatable :: row_mask(:), elem_mask(:)
        logical, allocatable, target :: valid_c(:) !! the `valid` copy vmask points at, packed or plain.
        logical, pointer :: vmask(:) !! the validity mask actually written, or disassociated.
        logical :: protected !! .true. if this column is protected, so the mask above is erased.
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        character(len=:), allocatable :: outname !! parquet_resolve_output_name scratch.
        integer(int64) :: i, k, nkeep
        logical :: masked

        call parquet_check_row_group_row_count(writer, name, nrows, row_mask, masked)
        protected = .false.
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
            if (present(valid)) then
                valid_c = valid(1:nelem)
                vmask => valid_c
            end if
        end if

        if (associated(vmask)) then
            call parquet_check_protected(writer, name, vmask, protected)
            call parquet_check_qc_miss(writer, name, vmask)
            ! A protected column's mask is all-.true. by the check above, so it carries no
            ! information -- drop it, and the write proceeds exactly as an unmasked one (no
            ! validity buffer built, field non-nullable). See parquet_check_protected.
            if (protected) nullify(vmask)
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
        call check_writer_open(writer, "parquet_write_column")
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
        call check_writer_open(writer, "parquet_write_column")
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
        call check_writer_open(writer, "parquet_write_column")
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
        call check_writer_open(writer, "parquet_write_column")
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
        call check_writer_open(writer, "parquet_write_column")
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
        call check_writer_open(writer, "parquet_write_column")
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
        call check_writer_open(writer, "parquet_write_column")
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
        call check_writer_open(writer, "parquet_write_column")
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
        call check_writer_open(writer, "parquet_write_column")
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
        call check_writer_open(writer, "parquet_write_column")
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
        call check_writer_open(writer, "parquet_write_column_chunk")
        call lk%claim(writer)

        if (writer%is_schema_enforced) then
            idx = parquet_get_defined_column_index(writer, name)
            call writer_context_suffix(writer, ctx)
            if (idx == 0) error stop &
                "parquet_write_column_chunk: column not defined in parquet_open_writer: " // &
                trim(name) // ctx
            if (.not. writer%all_columns(idx)%is_set) return
        end if

        call parquet_assert_column_type(writer, name, "int32", context="parquet_write_column_chunk")

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
        call check_writer_open(writer, "parquet_write_column_chunk")
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

        call parquet_assert_column_type(writer, name, "int32", context="parquet_write_column_chunk")

        if (.not. parquet_is_column_enabled(writer, name)) return

        call write_int32_chunk_flat(writer, name, values, size(values, kind=int64), asize, nrows, is_valid)
    end procedure parquet_write_int32_matrix_column_chunk
    module procedure parquet_write_int64_column_chunk
        integer :: idx
        integer(int64) :: asize, nrows
        character(len=:), allocatable :: ctx !! writer_context_suffix scratch.
        type(writer_lock) :: lk !! Releases writer's concurrency guard on every exit path (FINAL).
        call check_writer_open(writer, "parquet_write_column_chunk")
        call lk%claim(writer)

        if (writer%is_schema_enforced) then
            idx = parquet_get_defined_column_index(writer, name)
            call writer_context_suffix(writer, ctx)
            if (idx == 0) error stop &
                "parquet_write_column_chunk: column not defined in parquet_open_writer: " // &
                trim(name) // ctx
            if (.not. writer%all_columns(idx)%is_set) return
        end if

        call parquet_assert_column_type(writer, name, "int64", context="parquet_write_column_chunk")

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
        call check_writer_open(writer, "parquet_write_column_chunk")
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

        call parquet_assert_column_type(writer, name, "int64", context="parquet_write_column_chunk")

        if (.not. parquet_is_column_enabled(writer, name)) return

        call write_int64_chunk_flat(writer, name, values, size(values, kind=int64), asize, nrows, is_valid)
    end procedure parquet_write_int64_matrix_column_chunk
    module procedure parquet_write_float32_column_chunk
        integer :: idx
        integer(int64) :: asize, nrows
        character(len=:), allocatable :: ctx !! writer_context_suffix scratch.
        type(writer_lock) :: lk !! Releases writer's concurrency guard on every exit path (FINAL).
        call check_writer_open(writer, "parquet_write_column_chunk")
        call lk%claim(writer)

        if (writer%is_schema_enforced) then
            idx = parquet_get_defined_column_index(writer, name)
            call writer_context_suffix(writer, ctx)
            if (idx == 0) error stop &
                "parquet_write_column_chunk: column not defined in parquet_open_writer: " // &
                trim(name) // ctx
            if (.not. writer%all_columns(idx)%is_set) return
        end if

        call parquet_assert_column_type(writer, name, "float32", context="parquet_write_column_chunk")

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
        call check_writer_open(writer, "parquet_write_column_chunk")
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

        call parquet_assert_column_type(writer, name, "float32", context="parquet_write_column_chunk")

        if (.not. parquet_is_column_enabled(writer, name)) return

        call write_float32_chunk_flat(writer, name, values, size(values, kind=int64), asize, nrows, is_valid)
    end procedure parquet_write_float32_matrix_column_chunk
    module procedure parquet_write_float64_column_chunk
        integer :: idx
        integer(int64) :: asize, nrows
        character(len=:), allocatable :: ctx !! writer_context_suffix scratch.
        type(writer_lock) :: lk !! Releases writer's concurrency guard on every exit path (FINAL).
        call check_writer_open(writer, "parquet_write_column_chunk")
        call lk%claim(writer)

        if (writer%is_schema_enforced) then
            idx = parquet_get_defined_column_index(writer, name)
            call writer_context_suffix(writer, ctx)
            if (idx == 0) error stop &
                "parquet_write_column_chunk: column not defined in parquet_open_writer: " // &
                trim(name) // ctx
            if (.not. writer%all_columns(idx)%is_set) return
        end if

        call parquet_assert_column_type(writer, name, "float64", context="parquet_write_column_chunk")

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
        call check_writer_open(writer, "parquet_write_column_chunk")
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

        call parquet_assert_column_type(writer, name, "float64", context="parquet_write_column_chunk")

        if (.not. parquet_is_column_enabled(writer, name)) return

        call write_float64_chunk_flat(writer, name, values, size(values, kind=int64), asize, nrows, is_valid)
    end procedure parquet_write_float64_matrix_column_chunk
    module procedure parquet_write_logical_column_chunk
        integer :: idx
        integer(int64) :: asize, nrows
        character(len=:), allocatable :: ctx !! writer_context_suffix scratch.
        type(writer_lock) :: lk !! Releases writer's concurrency guard on every exit path (FINAL).
        call check_writer_open(writer, "parquet_write_column_chunk")
        call lk%claim(writer)

        if (writer%is_schema_enforced) then
            idx = parquet_get_defined_column_index(writer, name)
            call writer_context_suffix(writer, ctx)
            if (idx == 0) error stop &
                "parquet_write_column_chunk: column not defined in parquet_open_writer: " // &
                trim(name) // ctx
            if (.not. writer%all_columns(idx)%is_set) return
        end if

        call parquet_assert_column_type(writer, name, "boolean", context="parquet_write_column_chunk")

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
        call check_writer_open(writer, "parquet_write_column_chunk")
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

        call parquet_assert_column_type(writer, name, "boolean", context="parquet_write_column_chunk")

        if (.not. parquet_is_column_enabled(writer, name)) return

        call write_logical_chunk_flat(writer, name, values, size(values, kind=int64), asize, nrows, is_valid)
    end procedure parquet_write_logical_matrix_column_chunk

end submodule parquet_write_numeric
