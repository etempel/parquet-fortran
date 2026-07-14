!> Maintainer benchmarking tool (not part of the public library API): times a single write or
!> read of one synthetic multi-type Parquet file at a given Arrow thread-pool size. Invoked once
!> per (mode, threads) data point by tools/benchmark_threads.sh, which sweeps thread counts and
!> tabulates the results -- see that script for the sweep itself, and CONTRIBUTING.md for usage.
program benchmark_threads
    use iso_fortran_env, only : int32, int64, real32, real64, output_unit, error_unit
    use parquet
    implicit none

    ! Column shape is fixed here (one scalar + one vector field per supported type); file size
    ! and thread count are supplied per-run by tools/benchmark_threads.sh instead, since those
    ! are what the sweep varies -- this program is only ever one data point of it.
    integer, parameter :: VECTOR_COL_LEN = 10 !! Element count of each vector (matrix) column.
    integer, parameter :: STRING_LEN = 16 !! Max length of each string column entry.

    character(len=:), allocatable :: mode, file
    integer :: threads
    real(real64) :: size_gb
    integer(int64) :: nrows

    call parse_arguments(mode, threads, size_gb, file)

    call parquet_set_max_threads(threads)

    select case (mode)
    case ("write")
        nrows = estimate_nrows(size_gb, VECTOR_COL_LEN, STRING_LEN)
        call run_write_benchmark(file, nrows, threads)
    case ("read")
        call run_read_benchmark(file, threads)
    end select

contains

    subroutine print_usage()
        write(output_unit, '(a)') &
            "Usage: benchmark_threads --mode=write|read --threads=N --file=<path> [--size=<GB>]", &
            "  --mode=write|read   Benchmark mode (required).", &
            "  --threads=N         Arrow thread-pool size for this run (required, N>=1).", &
            "  --file=<path>       Parquet file to write to / read from (required).", &
            "  --size=<GB>         Target uncompressed size in GB (required for --mode=write)."
    end subroutine print_usage

    subroutine parse_arguments(mode, threads, size_gb, file)
        character(len=:), allocatable, intent(out) :: mode
        integer, intent(out) :: threads
        real(real64), intent(out) :: size_gb
        character(len=:), allocatable, intent(out) :: file

        integer :: i, nargs, eq_pos, ios
        character(len=256) :: arg, key, val

        mode = ""
        threads = 0
        size_gb = 0.0_real64
        file = ""

        nargs = command_argument_count()
        if (nargs == 0) then
            call print_usage()
            stop
        end if

        do i = 1, nargs
            call get_command_argument(i, arg)
            eq_pos = index(arg, "=")
            if (eq_pos < 2) then
                write(error_unit, '(a)') "benchmark_threads: bad argument '"//trim(arg)//"', expected --key=value"
                error stop 1
            end if
            key = arg(1:eq_pos - 1)
            val = arg(eq_pos + 1:)
            select case (trim(key))
            case ("--mode")
                mode = trim(val)
            case ("--threads")
                read(val, *, iostat=ios) threads
                if (ios /= 0) then
                    write(error_unit, '(a)') "benchmark_threads: --threads must be an integer"
                    error stop 1
                end if
            case ("--size")
                read(val, *, iostat=ios) size_gb
                if (ios /= 0) then
                    write(error_unit, '(a)') "benchmark_threads: --size must be a number"
                    error stop 1
                end if
            case ("--file")
                file = trim(val)
            case default
                write(error_unit, '(a)') "benchmark_threads: unknown argument '"//trim(key)//"'"
                error stop 1
            end select
        end do

        if (mode /= "write" .and. mode /= "read") then
            write(error_unit, '(a)') "benchmark_threads: --mode=write|read is required"
            error stop 1
        end if
        if (threads < 1) then
            write(error_unit, '(a)') "benchmark_threads: --threads=N (N>=1) is required"
            error stop 1
        end if
        if (len(file) == 0) then
            write(error_unit, '(a)') "benchmark_threads: --file=<path> is required"
            error stop 1
        end if
        if (mode == "write" .and. size_gb <= 0.0_real64) then
            write(error_unit, '(a)') "benchmark_threads: --size=<GB> (>0) is required for --mode=write"
            error stop 1
        end if
    end subroutine parse_arguments

    !> Uncompressed bytes/row of the fixed 12-column benchmark schema (see build_schema), used by
    !> estimate_nrows to size nrows for a target --size. Booleans are approximated as 1 byte/row
    !> (Arrow actually bit-packs them) -- close enough for sizing.
    function benchmark_bytes_per_row(vector_len, string_len) result(bytes_per_row)
        integer, intent(in) :: vector_len !! element count of each vector column.
        integer, intent(in) :: string_len !! max length of each string column entry.
        real(real64) :: bytes_per_row !! uncompressed bytes contributed by one table row.

        integer, parameter :: SCALAR_BYTES = 4 + 8 + 4 + 8 + 1 ! int32 + int64 + float32 + float64 + boolean

        bytes_per_row = real(SCALAR_BYTES + string_len, real64) * real(1 + vector_len, real64)
    end function benchmark_bytes_per_row

    !> Column count/shape is fixed (see build_schema); nrows is the derived quantity, sized so
    !> the 12-column table's uncompressed (in-memory) footprint approximates size_gb.
    function estimate_nrows(size_gb, vector_len, string_len) result(nrows)
        real(real64), intent(in) :: size_gb !! target uncompressed table size, in GB.
        integer, intent(in) :: vector_len !! element count of each vector column.
        integer, intent(in) :: string_len !! max length of each string column entry.
        integer(int64) :: nrows !! derived row count.

        real(real64) :: target_bytes

        target_bytes = size_gb * (1024.0_real64**3)
        nrows = max(1_int64, nint(target_bytes / benchmark_bytes_per_row(vector_len, string_len), int64))
    end function estimate_nrows

    !> Reports on stderr whether the "str"/"strv" (string / vector-of-strings) columns' total byte
    !> payload exceeds Arrow's int32 string-offset limit (2^31-1 bytes per column, the capacity of
    !> the default arrow::utf8() representation). This used to be a genuine crash risk, but
    !> src/parquet_wrapper.cpp now auto-detects exactly this case and transparently switches that
    !> column to arrow::large_utf8() (int64 offsets, no such limit) instead -- see
    !> doc/pages/supported-data-types.md's "Large string columns" section -- so exceeding it is
    !> informational only: the write still completes correctly, just via the large_utf8 path
    !> (visible in parquet_close_reader(print_stat=.true.)'s parquet_type column afterward).
    subroutine report_string_offset_limit_status(nrows, vector_len, string_len)
        integer(int64), intent(in) :: nrows !! row count this run's write is about to attempt.
        integer, intent(in) :: vector_len !! element count of each vector column.
        integer, intent(in) :: string_len !! max length of each string column entry.

        integer(int64), parameter :: arrow_int32_offset_limit = 2147483647_int64 ! 2^31 - 1
        integer(int64) :: str_bytes, strv_bytes

        str_bytes = nrows * int(string_len, int64)
        strv_bytes = nrows * int(vector_len, int64) * int(string_len, int64)

        write(error_unit, '(a, i0, a, i0, a, i0, a)') &
            "INFO: nrows=", nrows, " str column bytes=", str_bytes, " strv column bytes=", strv_bytes, &
            " (Arrow int32 string-offset limit: 2147483647 bytes/column)"
        flush(error_unit)

        if (str_bytes > arrow_int32_offset_limit .or. strv_bytes > arrow_int32_offset_limit) then
            write(error_unit, '(a)') &
                "INFO: 'str'/'strv' column byte payload exceeds Arrow's int32 string-offset limit " // &
                "for a regular utf8 array -- this library automatically writes that column as " // &
                "arrow::large_utf8() (64-bit offsets) instead, so the write still completes correctly."
            flush(error_unit)
        end if
    end subroutine report_string_offset_limit_status

    !> One scalar + one vector field per supported type (int32/int64/float32/float64/
    !> boolean/string) -- the fixed column set every write/read benchmark run uses.
    subroutine build_schema(schema, vector_len, string_len)
        type(parquet_schema), intent(out) :: schema
        integer, intent(in) :: vector_len
        integer, intent(in) :: string_len

        call schema%init(table="benchmark_threads_table")
        call schema%add_field("i32", "int32")
        call schema%add_field("i64", "int64")
        call schema%add_field("f32", "float32")
        call schema%add_field("f64", "float64")
        call schema%add_field("lg", "boolean")
        call schema%add_field("str", "string", array_size=string_len)
        call schema%add_field("i32v", "int32", col_size=vector_len)
        call schema%add_field("i64v", "int64", col_size=vector_len)
        call schema%add_field("f32v", "float32", col_size=vector_len)
        call schema%add_field("f64v", "float64", col_size=vector_len)
        call schema%add_field("lgv", "boolean", col_size=vector_len)
        call schema%add_field("strv", "string", col_size=vector_len, array_size=string_len)
        call parquet_parse_maml(schema)
    end subroutine build_schema

    !> Times parquet_open_writer/parquet_write_column/parquet_close_writer only -- each
    !> column's synthetic data is generated (untimed) immediately before it's written and
    !> freed immediately after, so generation cost doesn't pollute the reported elapsed time
    !> and peak Fortran-side memory stays near one column's size rather than the whole table.
    subroutine run_write_benchmark(file, nrows, threads)
        character(len=*), intent(in) :: file
        integer(int64), intent(in) :: nrows
        integer, intent(in) :: threads

        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        integer(int64) :: t0, t1, count_rate, elapsed_ticks
        integer(int64) :: i
        integer :: j
        real(real64) :: elapsed_s

        integer(int32), allocatable :: i32c(:), i32vc(:, :)
        integer(int64), allocatable :: i64c(:), i64vc(:, :)
        real(real32), allocatable :: f32c(:), f32vc(:, :)
        real(real64), allocatable :: f64c(:), f64vc(:, :)
        logical, allocatable :: lgc(:), lgvc(:, :)
        character(len=STRING_LEN), allocatable :: strc(:), strvc(:, :)

        call build_schema(schema, VECTOR_COL_LEN, STRING_LEN)
        call report_string_offset_limit_status(nrows, VECTOR_COL_LEN, STRING_LEN)

        elapsed_ticks = 0_int64
        call system_clock(t0, count_rate)
        call parquet_open_writer(writer, file, schema=schema, use_threads=.true.)
        call system_clock(t1)
        elapsed_ticks = elapsed_ticks + (t1 - t0)

        allocate(i32c(nrows))
        do i = 1, nrows
            i32c(i) = int(mod(i, 1000_int64), int32)
        end do
        call system_clock(t0)
        call parquet_write_column(writer, "i32", i32c)
        call system_clock(t1)
        elapsed_ticks = elapsed_ticks + (t1 - t0)
        deallocate(i32c)

        allocate(i64c(nrows))
        do i = 1, nrows
            i64c(i) = mod(i, 1000000_int64)
        end do
        call system_clock(t0)
        call parquet_write_column(writer, "i64", i64c)
        call system_clock(t1)
        elapsed_ticks = elapsed_ticks + (t1 - t0)
        deallocate(i64c)

        allocate(f32c(nrows))
        do i = 1, nrows
            f32c(i) = real(mod(i, 1000_int64), real32) * 0.001_real32
        end do
        call system_clock(t0)
        call parquet_write_column(writer, "f32", f32c)
        call system_clock(t1)
        elapsed_ticks = elapsed_ticks + (t1 - t0)
        deallocate(f32c)

        allocate(f64c(nrows))
        do i = 1, nrows
            f64c(i) = real(mod(i, 1000000_int64), real64) * 0.001_real64
        end do
        call system_clock(t0)
        call parquet_write_column(writer, "f64", f64c)
        call system_clock(t1)
        elapsed_ticks = elapsed_ticks + (t1 - t0)
        deallocate(f64c)

        allocate(lgc(nrows))
        do i = 1, nrows
            lgc(i) = mod(i, 2_int64) == 0_int64
        end do
        call system_clock(t0)
        call parquet_write_column(writer, "lg", lgc)
        call system_clock(t1)
        elapsed_ticks = elapsed_ticks + (t1 - t0)
        deallocate(lgc)

        allocate(strc(nrows))
        do i = 1, nrows
            strc(i) = repeat(achar(97 + int(mod(i, 26_int64))), STRING_LEN)
        end do
        call system_clock(t0)
        call parquet_write_column(writer, "str", strc)
        call system_clock(t1)
        elapsed_ticks = elapsed_ticks + (t1 - t0)
        deallocate(strc)

        allocate(i32vc(VECTOR_COL_LEN, nrows))
        do i = 1, nrows
            do j = 1, VECTOR_COL_LEN
                i32vc(j, i) = int(mod(i + j, 1000_int64), int32)
            end do
        end do
        call system_clock(t0)
        call parquet_write_column(writer, "i32v", i32vc)
        call system_clock(t1)
        elapsed_ticks = elapsed_ticks + (t1 - t0)
        deallocate(i32vc)

        allocate(i64vc(VECTOR_COL_LEN, nrows))
        do i = 1, nrows
            do j = 1, VECTOR_COL_LEN
                i64vc(j, i) = mod(i + j, 1000000_int64)
            end do
        end do
        call system_clock(t0)
        call parquet_write_column(writer, "i64v", i64vc)
        call system_clock(t1)
        elapsed_ticks = elapsed_ticks + (t1 - t0)
        deallocate(i64vc)

        allocate(f32vc(VECTOR_COL_LEN, nrows))
        do i = 1, nrows
            do j = 1, VECTOR_COL_LEN
                f32vc(j, i) = real(mod(i + j, 1000_int64), real32) * 0.001_real32
            end do
        end do
        call system_clock(t0)
        call parquet_write_column(writer, "f32v", f32vc)
        call system_clock(t1)
        elapsed_ticks = elapsed_ticks + (t1 - t0)
        deallocate(f32vc)

        allocate(f64vc(VECTOR_COL_LEN, nrows))
        do i = 1, nrows
            do j = 1, VECTOR_COL_LEN
                f64vc(j, i) = real(mod(i + j, 1000000_int64), real64) * 0.001_real64
            end do
        end do
        call system_clock(t0)
        call parquet_write_column(writer, "f64v", f64vc)
        call system_clock(t1)
        elapsed_ticks = elapsed_ticks + (t1 - t0)
        deallocate(f64vc)

        allocate(lgvc(VECTOR_COL_LEN, nrows))
        do i = 1, nrows
            do j = 1, VECTOR_COL_LEN
                lgvc(j, i) = mod(i + int(j, int64), 2_int64) == 0_int64
            end do
        end do
        call system_clock(t0)
        call parquet_write_column(writer, "lgv", lgvc)
        call system_clock(t1)
        elapsed_ticks = elapsed_ticks + (t1 - t0)
        deallocate(lgvc)

        allocate(strvc(VECTOR_COL_LEN, nrows))
        do i = 1, nrows
            do j = 1, VECTOR_COL_LEN
                strvc(j, i) = repeat(achar(97 + int(mod(i + j, 26_int64))), STRING_LEN)
            end do
        end do
        call system_clock(t0)
        call parquet_write_column(writer, "strv", strvc)
        call system_clock(t1)
        elapsed_ticks = elapsed_ticks + (t1 - t0)
        deallocate(strvc)

        call system_clock(t0)
        call parquet_close_writer(writer)
        call system_clock(t1)
        elapsed_ticks = elapsed_ticks + (t1 - t0)

        elapsed_s = real(elapsed_ticks, real64) / real(count_rate, real64)
        write(output_unit, '(a, i0, a, i0, a, f0.6)') &
            "RESULT mode=write threads=", threads, " nrows=", nrows, " elapsed_s=", elapsed_s
    end subroutine run_write_benchmark

    !> Times parquet_open_reader (prefetch=.true., so this is where the actual decode work
    !> happens)/parquet_read_column/parquet_close_reader; nrows comes from the file itself.
    subroutine run_read_benchmark(file, threads)
        character(len=*), intent(in) :: file
        integer, intent(in) :: threads

        type(parquet_reader) :: reader
        integer(int64) :: t0, t1, count_rate, elapsed_ticks, nrows
        real(real64) :: elapsed_s

        integer(int32), allocatable :: i32c(:), i32vc(:, :)
        integer(int64), allocatable :: i64c(:), i64vc(:, :)
        real(real32), allocatable :: f32c(:), f32vc(:, :)
        real(real64), allocatable :: f64c(:), f64vc(:, :)
        logical, allocatable :: lgc(:), lgvc(:, :)
        character(len=STRING_LEN), allocatable :: strc(:), strvc(:, :)

        elapsed_ticks = 0_int64
        call system_clock(t0, count_rate)
        call parquet_open_reader(reader, file, use_threads=.true., prefetch=.true.)
        call system_clock(t1)
        elapsed_ticks = elapsed_ticks + (t1 - t0)

        call parquet_get_nrows(reader, nrows)

        allocate(i32c(nrows))
        call system_clock(t0)
        call parquet_read_column(reader, "i32", i32c)
        call system_clock(t1)
        elapsed_ticks = elapsed_ticks + (t1 - t0)
        deallocate(i32c)

        allocate(i64c(nrows))
        call system_clock(t0)
        call parquet_read_column(reader, "i64", i64c)
        call system_clock(t1)
        elapsed_ticks = elapsed_ticks + (t1 - t0)
        deallocate(i64c)

        allocate(f32c(nrows))
        call system_clock(t0)
        call parquet_read_column(reader, "f32", f32c)
        call system_clock(t1)
        elapsed_ticks = elapsed_ticks + (t1 - t0)
        deallocate(f32c)

        allocate(f64c(nrows))
        call system_clock(t0)
        call parquet_read_column(reader, "f64", f64c)
        call system_clock(t1)
        elapsed_ticks = elapsed_ticks + (t1 - t0)
        deallocate(f64c)

        allocate(lgc(nrows))
        call system_clock(t0)
        call parquet_read_column(reader, "lg", lgc)
        call system_clock(t1)
        elapsed_ticks = elapsed_ticks + (t1 - t0)
        deallocate(lgc)

        allocate(strc(nrows))
        call system_clock(t0)
        call parquet_read_column(reader, "str", strc)
        call system_clock(t1)
        elapsed_ticks = elapsed_ticks + (t1 - t0)
        deallocate(strc)

        allocate(i32vc(VECTOR_COL_LEN, nrows))
        call system_clock(t0)
        call parquet_read_column(reader, "i32v", i32vc)
        call system_clock(t1)
        elapsed_ticks = elapsed_ticks + (t1 - t0)
        deallocate(i32vc)

        allocate(i64vc(VECTOR_COL_LEN, nrows))
        call system_clock(t0)
        call parquet_read_column(reader, "i64v", i64vc)
        call system_clock(t1)
        elapsed_ticks = elapsed_ticks + (t1 - t0)
        deallocate(i64vc)

        allocate(f32vc(VECTOR_COL_LEN, nrows))
        call system_clock(t0)
        call parquet_read_column(reader, "f32v", f32vc)
        call system_clock(t1)
        elapsed_ticks = elapsed_ticks + (t1 - t0)
        deallocate(f32vc)

        allocate(f64vc(VECTOR_COL_LEN, nrows))
        call system_clock(t0)
        call parquet_read_column(reader, "f64v", f64vc)
        call system_clock(t1)
        elapsed_ticks = elapsed_ticks + (t1 - t0)
        deallocate(f64vc)

        allocate(lgvc(VECTOR_COL_LEN, nrows))
        call system_clock(t0)
        call parquet_read_column(reader, "lgv", lgvc)
        call system_clock(t1)
        elapsed_ticks = elapsed_ticks + (t1 - t0)
        deallocate(lgvc)

        allocate(strvc(VECTOR_COL_LEN, nrows))
        call system_clock(t0)
        call parquet_read_column(reader, "strv", strvc)
        call system_clock(t1)
        elapsed_ticks = elapsed_ticks + (t1 - t0)
        deallocate(strvc)

        call system_clock(t0)
        call parquet_close_reader(reader)
        call system_clock(t1)
        elapsed_ticks = elapsed_ticks + (t1 - t0)

        elapsed_s = real(elapsed_ticks, real64) / real(count_rate, real64)
        write(output_unit, '(a, i0, a, i0, a, f0.6)') &
            "RESULT mode=read threads=", threads, " nrows=", nrows, " elapsed_s=", elapsed_s
    end subroutine run_read_benchmark

end program benchmark_threads
