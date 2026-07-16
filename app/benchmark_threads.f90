!> Maintainer benchmarking tool (not part of the public library API): times a single write or
!> read of one synthetic multi-type Parquet file at a given Arrow thread-pool size. Invoked once
!> per (mode, threads) data point by tools/benchmark_threads.sh, which sweeps thread counts and
!> tabulates the results -- see that script for the sweep itself, and CONTRIBUTING.md for usage.
program benchmark_threads
    use iso_fortran_env, only : int32, int64, real32, real64, output_unit, error_unit
    use parquet
    implicit none

    ! Column shape is fixed here (int32/int64/float32/float64/boolean scalar columns, replicated
    ! NMULT times); file size, thread count and NMULT are supplied per-run by
    ! tools/benchmark_threads.sh instead, since those are what the sweep varies -- this program
    ! is only ever one data point of it.
    character(len=:), allocatable :: mode, file
    integer :: threads, nmult
    real(real64) :: size_gb
    integer(int64) :: nrows

    call parse_arguments(mode, threads, size_gb, file, nmult)

    call parquet_set_max_threads(threads)

    select case (mode)
    case ("write")
        nrows = estimate_nrows(size_gb, nmult)
        call run_write_benchmark(file, nrows, threads, nmult)
    case ("read")
        call run_read_benchmark(file, threads, nmult)
    end select

contains

    subroutine print_usage()
        write(output_unit, '(a)') &
            "Usage: benchmark_threads --mode=write|read --threads=N --file=<path> [--size=<GB>] [--nmult=N]", &
            "  --mode=write|read   Benchmark mode (required).", &
            "  --threads=N         Arrow thread-pool size for this run (required, N>=1).", &
            "  --file=<path>       Parquet file to write to / read from (required).", &
            "  --size=<GB>         Target uncompressed size in GB (required for --mode=write).", &
            "  --nmult=N           Replicate the 5-column scalar schema N times (default 1, N>=1)."
    end subroutine print_usage

    subroutine parse_arguments(mode, threads, size_gb, file, nmult)
        character(len=:), allocatable, intent(out) :: mode
        integer, intent(out) :: threads
        real(real64), intent(out) :: size_gb
        character(len=:), allocatable, intent(out) :: file
        integer, intent(out) :: nmult

        integer :: i, nargs, eq_pos, ios
        character(len=256) :: arg, key, val

        mode = ""
        threads = 0
        size_gb = 0.0_real64
        file = ""
        nmult = 1

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
            case ("--nmult")
                read(val, *, iostat=ios) nmult
                if (ios /= 0) then
                    write(error_unit, '(a)') "benchmark_threads: --nmult must be an integer"
                    error stop 1
                end if
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
        if (nmult < 1) then
            write(error_unit, '(a)') "benchmark_threads: --nmult=N (N>=1) is required"
            error stop 1
        end if
    end subroutine parse_arguments

    !> Uncompressed bytes/row of the 5*nmult-column benchmark schema (see build_schema), used by
    !> estimate_nrows to size nrows for a target --size. Booleans are approximated as 1 byte/row
    !> (Arrow actually bit-packs them) -- close enough for sizing.
    function benchmark_bytes_per_row(nmult) result(bytes_per_row)
        integer, intent(in) :: nmult !! number of times the 5-column scalar schema is replicated.
        real(real64) :: bytes_per_row !! uncompressed bytes contributed by one table row.

        integer, parameter :: SCALAR_BYTES = 4 + 8 + 4 + 8 + 1 ! int32 + int64 + float32 + float64 + boolean

        bytes_per_row = real(SCALAR_BYTES, real64) * real(nmult, real64)
    end function benchmark_bytes_per_row

    !> Column count is fixed at 5*nmult (see build_schema); nrows is the derived quantity, sized
    !> so the table's uncompressed (in-memory) footprint approximates size_gb.
    function estimate_nrows(size_gb, nmult) result(nrows)
        real(real64), intent(in) :: size_gb !! target uncompressed table size, in GB.
        integer, intent(in) :: nmult !! number of times the 5-column scalar schema is replicated.
        integer(int64) :: nrows !! derived row count.

        real(real64) :: target_bytes

        target_bytes = size_gb * (1024.0_real64**3)
        nrows = max(1_int64, nint(target_bytes / benchmark_bytes_per_row(nmult), int64))
    end function estimate_nrows

    !> Builds the "<base>_<k>" column name used for replica k (1..nmult) of one of the 5 base
    !> scalar columns (e.g. "i32_1", "i32_2", ...).
    subroutine make_col_name(base, k, name)
        character(len=*), intent(in) :: base !! base column name (i32/i64/f32/f64/lg).
        integer, intent(in) :: k !! replica index, 1..nmult.
        character(len=:), allocatable, intent(out) :: name !! resulting "<base>_<k>" column name.

        character(len=16) :: kstr

        write(kstr, '(i0)') k
        name = base//"_"//trim(kstr)
    end subroutine make_col_name

    !> int32/int64/float32/float64/boolean scalar fields, each replicated nmult times (e.g.
    !> i32_1..i32_nmult) -- the column set every write/read benchmark run uses.
    subroutine build_schema(schema, nmult)
        type(parquet_schema), intent(out) :: schema
        integer, intent(in) :: nmult

        integer :: k
        character(len=:), allocatable :: name

        call schema%init(table="benchmark_threads_table")
        do k = 1, nmult
            call make_col_name("i32", k, name)
            call schema%add_field(name, "int32")
            call make_col_name("i64", k, name)
            call schema%add_field(name, "int64")
            call make_col_name("f32", k, name)
            call schema%add_field(name, "float32")
            call make_col_name("f64", k, name)
            call schema%add_field(name, "float64")
            call make_col_name("lg", k, name)
            call schema%add_field(name, "boolean")
        end do
        call parquet_parse_maml(schema)
    end subroutine build_schema

    !> Times parquet_open_writer/parquet_write_column/parquet_close_writer only -- each column
    !> type's synthetic data is generated (untimed) once and then written under nmult distinct
    !> replica names, so generation cost doesn't pollute the reported elapsed time and peak
    !> Fortran-side memory stays near one column's size rather than the whole table.
    subroutine run_write_benchmark(file, nrows, threads, nmult)
        character(len=*), intent(in) :: file
        integer(int64), intent(in) :: nrows
        integer, intent(in) :: threads
        integer, intent(in) :: nmult

        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        integer(int64) :: t0, t1, count_rate, elapsed_ticks
        integer(int64) :: i
        integer :: k
        real(real64) :: elapsed_s
        character(len=:), allocatable :: name

        integer(int32), allocatable :: i32c(:)
        integer(int64), allocatable :: i64c(:)
        real(real32), allocatable :: f32c(:)
        real(real64), allocatable :: f64c(:)
        logical, allocatable :: lgc(:)

        call build_schema(schema, nmult)

        elapsed_ticks = 0_int64
        call system_clock(t0, count_rate)
        call parquet_open_writer(writer, file, schema=schema, use_threads=.true.)
        call system_clock(t1)
        elapsed_ticks = elapsed_ticks + (t1 - t0)

        allocate(i32c(nrows))
        do i = 1, nrows
            i32c(i) = int(mod(i, 1000_int64), int32)
        end do
        do k = 1, nmult
            call make_col_name("i32", k, name)
            call system_clock(t0)
            call parquet_write_column(writer, name, i32c)
            call system_clock(t1)
            elapsed_ticks = elapsed_ticks + (t1 - t0)
        end do
        deallocate(i32c)

        allocate(i64c(nrows))
        do i = 1, nrows
            i64c(i) = mod(i, 1000000_int64)
        end do
        do k = 1, nmult
            call make_col_name("i64", k, name)
            call system_clock(t0)
            call parquet_write_column(writer, name, i64c)
            call system_clock(t1)
            elapsed_ticks = elapsed_ticks + (t1 - t0)
        end do
        deallocate(i64c)

        allocate(f32c(nrows))
        do i = 1, nrows
            f32c(i) = real(mod(i, 1000_int64), real32) * 0.001_real32
        end do
        do k = 1, nmult
            call make_col_name("f32", k, name)
            call system_clock(t0)
            call parquet_write_column(writer, name, f32c)
            call system_clock(t1)
            elapsed_ticks = elapsed_ticks + (t1 - t0)
        end do
        deallocate(f32c)

        allocate(f64c(nrows))
        do i = 1, nrows
            f64c(i) = real(mod(i, 1000000_int64), real64) * 0.001_real64
        end do
        do k = 1, nmult
            call make_col_name("f64", k, name)
            call system_clock(t0)
            call parquet_write_column(writer, name, f64c)
            call system_clock(t1)
            elapsed_ticks = elapsed_ticks + (t1 - t0)
        end do
        deallocate(f64c)

        allocate(lgc(nrows))
        do i = 1, nrows
            lgc(i) = mod(i, 2_int64) == 0_int64
        end do
        do k = 1, nmult
            call make_col_name("lg", k, name)
            call system_clock(t0)
            call parquet_write_column(writer, name, lgc)
            call system_clock(t1)
            elapsed_ticks = elapsed_ticks + (t1 - t0)
        end do
        deallocate(lgc)

        call system_clock(t0)
        call parquet_close_writer(writer)
        call system_clock(t1)
        elapsed_ticks = elapsed_ticks + (t1 - t0)

        elapsed_s = real(elapsed_ticks, real64) / real(count_rate, real64)
        write(output_unit, '(a, i0, a, i0, a, i0, a, f0.6)') &
            "RESULT mode=write threads=", threads, " nmult=", nmult, " nrows=", nrows, " elapsed_s=", elapsed_s
    end subroutine run_write_benchmark

    !> Times parquet_open_reader (prefetch=.true., so this is where the actual decode work
    !> happens)/parquet_read_column/parquet_close_reader; nrows comes from the file itself.
    subroutine run_read_benchmark(file, threads, nmult)
        character(len=*), intent(in) :: file
        integer, intent(in) :: threads
        integer, intent(in) :: nmult

        type(parquet_reader) :: reader
        integer(int64) :: t0, t1, count_rate, elapsed_ticks, nrows
        integer :: k
        real(real64) :: elapsed_s
        character(len=:), allocatable :: name

        integer(int32), allocatable :: i32c(:)
        integer(int64), allocatable :: i64c(:)
        real(real32), allocatable :: f32c(:)
        real(real64), allocatable :: f64c(:)
        logical, allocatable :: lgc(:)

        elapsed_ticks = 0_int64
        call system_clock(t0, count_rate)
        call parquet_open_reader(reader, file, use_threads=.true., prefetch=.true.)
        call system_clock(t1)
        elapsed_ticks = elapsed_ticks + (t1 - t0)

        call parquet_get_nrows(reader, nrows)

        allocate(i32c(nrows))
        do k = 1, nmult
            call make_col_name("i32", k, name)
            call system_clock(t0)
            call parquet_read_column(reader, name, i32c)
            call system_clock(t1)
            elapsed_ticks = elapsed_ticks + (t1 - t0)
        end do
        deallocate(i32c)

        allocate(i64c(nrows))
        do k = 1, nmult
            call make_col_name("i64", k, name)
            call system_clock(t0)
            call parquet_read_column(reader, name, i64c)
            call system_clock(t1)
            elapsed_ticks = elapsed_ticks + (t1 - t0)
        end do
        deallocate(i64c)

        allocate(f32c(nrows))
        do k = 1, nmult
            call make_col_name("f32", k, name)
            call system_clock(t0)
            call parquet_read_column(reader, name, f32c)
            call system_clock(t1)
            elapsed_ticks = elapsed_ticks + (t1 - t0)
        end do
        deallocate(f32c)

        allocate(f64c(nrows))
        do k = 1, nmult
            call make_col_name("f64", k, name)
            call system_clock(t0)
            call parquet_read_column(reader, name, f64c)
            call system_clock(t1)
            elapsed_ticks = elapsed_ticks + (t1 - t0)
        end do
        deallocate(f64c)

        allocate(lgc(nrows))
        do k = 1, nmult
            call make_col_name("lg", k, name)
            call system_clock(t0)
            call parquet_read_column(reader, name, lgc)
            call system_clock(t1)
            elapsed_ticks = elapsed_ticks + (t1 - t0)
        end do
        deallocate(lgc)

        call system_clock(t0)
        call parquet_close_reader(reader)
        call system_clock(t1)
        elapsed_ticks = elapsed_ticks + (t1 - t0)

        elapsed_s = real(elapsed_ticks, real64) / real(count_rate, real64)
        write(output_unit, '(a, i0, a, i0, a, i0, a, f0.6)') &
            "RESULT mode=read threads=", threads, " nmult=", nmult, " nrows=", nrows, " elapsed_s=", elapsed_s
    end subroutine run_read_benchmark

end program benchmark_threads
