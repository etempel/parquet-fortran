!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> Measures what the `parquet_table` layer costs relative to reading straight into user arrays.
!!
!! `parquet_table` accepts one extra full Fortran-side copy of every column, and frees the Arrow
!! buffers once that copy exists (feature_table.md D3). Both halves of that trade are asserted
!! rather than measured everywhere else in this repository, so this program measures them:
!!
!!   * **table vs. raw**  -- open+materialize a whole file as a table, against opening a reader
!!                           and reading the same columns into plain arrays. The gap is what the
!!                           table layer costs.
!!   * **peak RSS**       -- resident memory after the table is fully materialized, against the
!!                           file's own uncompressed size. THIS is the number that proves the
!!                           Arrow-side release actually happens: without it, a fully
!!                           materialized table holds roughly two copies of the file.
!!   * **get vs. col**    -- the widening copy path against the zero-copy pointer path, on one
!!                           large column, which is what the guide's advice rests on.
!!   * **write**          -- parquet_write_table against a hand-written per-column write loop.
!!
!! Maintainer tool, never run by `fpm test` (CLAUDE.md: anything needing this much memory/time
!! lives under app/ with a shell wrapper under tools/). Drive it with tools/benchmark_table.sh.
program benchmark_table
    use parquet
    use parquet_tables
    use iso_fortran_env, only : int32, int64, real32, real64, error_unit, output_unit
    use iso_c_binding, only : c_int64_t
    implicit none

    !> Bytes currently held by Arrow's process-wide memory pool. Declared locally rather than in
    !! src/parquet_bindings.f90 because it is a maintainer diagnostic, not public API -- the same
    !! convention the debug-only hooks in test/error_scenarios.f90 follow.
    !!
    !! This, not RSS, is what shows whether the Arrow-side buffers were released: Arrow's pool
    !! keeps freed pages rather than returning them to the OS, so RSS stays high either way.
    interface
        function parquet_get_arrow_bytes_allocated() &
                bind(C, name="parquet_get_arrow_bytes_allocated") result(bytes)
            import :: c_int64_t
            integer(c_int64_t) :: bytes !! bytes currently allocated from Arrow's default pool.
        end function parquet_get_arrow_bytes_allocated
    end interface

    character(len=:), allocatable :: mode, file
    real(real64) :: size_gb
    integer :: ncols

    call parse_arguments(mode, size_gb, file, ncols)

    select case (mode)
    case ("write_fixture")
        call write_fixture(file, size_gb, ncols)
    case ("read_raw")
        call bench_read_raw(file)
    case ("read_table")
        call bench_read_table(file)
    case ("access")
        call bench_access(file)
    case ("write")
        call bench_write(file)
    case default
        write(error_unit, '(a)') "benchmark_table: unknown --mode '"//mode//"'"
        error stop 1
    end select

contains

    subroutine print_usage()
        write(output_unit, '(a)') "Usage: benchmark_table --mode=<write_fixture|read|access|write>"
        write(output_unit, '(a)') "                       [--size=<GB>] [--file=<path>] [--ncols=<n>]"
        write(output_unit, '(a)') ""
        write(output_unit, '(a)') "  --mode=write_fixture  generate the synthetic input file"
        write(output_unit, '(a)') "  --mode=read_raw       time a reader + plain arrays, and report RSS"
        write(output_unit, '(a)') "  --mode=read_table     time a table open+materialize, and report RSS"
        write(output_unit, '(a)') "  --mode=access         time %get (copy, widening) vs. %col (pointer)"
        write(output_unit, '(a)') "  --mode=write          time parquet_write_table vs. a hand-written loop"
        write(output_unit, '(a)') "  --size=<GB>           approximate uncompressed size (write_fixture)"
        write(output_unit, '(a)') "  --ncols=<n>           float64 columns in the fixture (default 8)"
    end subroutine print_usage

    subroutine parse_arguments(mode, size_gb, file, ncols)
        character(len=:), allocatable, intent(out) :: mode !! which measurement to run.
        real(real64), intent(out) :: size_gb               !! target fixture size in GB.
        character(len=:), allocatable, intent(out) :: file !! fixture path.
        integer, intent(out) :: ncols                      !! float64 columns in the fixture.

        integer :: i, nargs, eq_pos, ios
        character(len=512) :: arg, key, val

        mode = ""
        size_gb = 0.25_real64
        file = "benchmark_table.parquet"
        ncols = 8

        nargs = command_argument_count()
        if (nargs == 0) then
            call print_usage()
            stop
        end if

        do i = 1, nargs
            call get_command_argument(i, arg)
            eq_pos = index(arg, "=")
            if (eq_pos < 2) then
                write(error_unit, '(a)') "benchmark_table: bad argument '"//trim(arg)//"', expected --key=value"
                error stop 1
            end if
            key = arg(1:eq_pos - 1)
            val = arg(eq_pos + 1:)
            select case (trim(key))
            case ("--mode")
                mode = trim(val)
            case ("--size")
                read(val, *, iostat=ios) size_gb
                if (ios /= 0) then
                    write(error_unit, '(a)') "benchmark_table: --size must be a number"
                    error stop 1
                end if
            case ("--file")
                file = trim(val)
            case ("--ncols")
                read(val, *, iostat=ios) ncols
                if (ios /= 0) then
                    write(error_unit, '(a)') "benchmark_table: --ncols must be an integer"
                    error stop 1
                end if
            case default
                write(error_unit, '(a)') "benchmark_table: unknown option '"//trim(key)//"'"
                error stop 1
            end select
        end do

        if (len(mode) == 0) then
            write(error_unit, '(a)') "benchmark_table: --mode is required"
            error stop 1
        end if
    end subroutine parse_arguments

    !> Resident set size of this process in MiB, read from the OS rather than computed, so it
    !! reflects what actually stayed allocated (including anything Arrow kept) rather than what
    !! this program thinks it allocated. Returns -1 when it cannot be determined.
    function rss_mib() result(mib)
        real(real64) :: mib !! resident set size, MiB, or -1.
        integer :: u, ios, pid
        character(len=256) :: line, cmdfile
        real(real64) :: kb

        mib = -1.0_real64
        pid = getpid()
        write(cmdfile, '(a,i0,a)') "/tmp/pf_rss_", pid, ".txt"
        ! `ps` is the one portable-enough route across Linux and macOS; /proc/self/status does
        ! not exist on macOS, and neither does a Fortran intrinsic for this.
        call execute_command_line("ps -o rss= -p " // itoa(pid) // " > " // trim(cmdfile), wait=.true.)
        open(newunit=u, file=trim(cmdfile), status="old", action="read", iostat=ios)
        if (ios /= 0) return
        read(u, '(a)', iostat=ios) line
        close(u, status="delete")
        if (ios /= 0) return
        read(line, *, iostat=ios) kb
        if (ios /= 0) return
        mib = kb / 1024.0_real64
    end function rss_mib

    function itoa(n) result(s)
        integer, intent(in) :: n              !! value to render.
        character(len=:), allocatable :: s    !! decimal text.
        character(len=32) :: buf
        write(buf, '(i0)') n
        s = trim(buf)
    end function itoa

    !> Wall-clock seconds since an arbitrary origin.
    function now() result(t)
        real(real64) :: t !! seconds.
        integer(int64) :: c, r
        call system_clock(count=c, count_rate=r)
        t = real(c, real64) / real(r, real64)
    end function now

    subroutine write_fixture(file, size_gb, ncols)
        character(len=*), intent(in) :: file !! output path.
        real(real64), intent(in) :: size_gb  !! approximate uncompressed size.
        integer, intent(in) :: ncols         !! float64 columns to write.
        type(parquet_writer) :: w
        real(real64), allocatable :: v(:)
        integer(int64) :: nrows
        integer :: c, i
        character(len=32) :: cname

        nrows = int(size_gb * 1.0e9_real64 / (8.0_real64 * real(ncols, real64)), int64)
        if (nrows < 1) nrows = 1
        write(output_unit, '(a,i0,a,i0,a)') "writing fixture: ", nrows, " rows x ", ncols, " float64 columns"
        allocate(v(nrows))
        call parquet_open_writer(w, file)
        do c = 1, ncols
            do i = 1, int(nrows)
                v(i) = real(i, real64) * real(c, real64)
            end do
            write(cname, '(a,i0)') "c", c
            call parquet_write_column(w, trim(cname), v)
        end do
        call parquet_close_writer(w)
        write(output_unit, '(a)') "fixture written: " // file
    end subroutine write_fixture

    !> Raw baseline: a reader plus one plain array per column.
    !!
    !! Deliberately its OWN process, not a phase of the table run. Neither glibc nor macOS
    !! malloc reliably returns freed pages to the OS, so measuring both paths in one process
    !! reports the high-water mark of the pair and makes the table look like it kept memory it
    !! had already released -- which is exactly the claim this benchmark exists to check.
    subroutine bench_read_raw(file)
        character(len=*), intent(in) :: file !! fixture to read.
        type(parquet_reader) :: r
        real(real64), allocatable :: v(:)
        character(len=:), allocatable :: names(:)
        real(real64) :: t0, t_raw, rss_after
        integer(int64) :: nrows
        integer :: i, nc

        t0 = now()
        call parquet_open_reader(r, file)
        call parquet_get_nrows(r, nrows)
        call parquet_get_column_names(r, names)
        nc = size(names)
        allocate(v(nrows))
        do i = 1, nc
            call parquet_read_column(r, trim(names(i)), v)
        end do
        t_raw = now() - t0
        rss_after = rss_mib()

        write(output_unit, '(a)') "--- read: raw (reader + one array, reused) ---"
        write(output_unit, '(a,i0,a,i0)') "rows: ", nrows, "   columns: ", nc
        write(output_unit, '(a,f10.3,a)') "time                        : ", t_raw, " s"
        write(output_unit, '(a,f10.1,a)') "RSS                         : ", rss_after, " MiB"
        write(output_unit, '(a,f10.1,a)') "Arrow pool still holding    : ", &
            real(parquet_get_arrow_bytes_allocated(), real64) / 1048576.0_real64, " MiB"
        write(output_unit, '(a,f10.1,a)') "one column (nrows*8)        : ", &
            real(nrows, real64) * 8.0_real64 / 1048576.0_real64, " MiB"
        write(output_unit, '(a)') "The reader caches every column it decodes, so the Arrow figure here"
        write(output_unit, '(a)') "is the whole file -- that is the baseline the table has to beat."
        call parquet_close_reader(r)
    end subroutine bench_read_raw

    !> The table path: open and materialize every column, then report time and resident memory.
    subroutine bench_read_table(file)
        character(len=*), intent(in) :: file !! fixture to read.
        type(parquet_table) :: t
        real(real64) :: t0, t_table, rss_after, data_mib, arrow_mib

        t0 = now()
        call parquet_open_table(t, file)
        t_table = now() - t0
        rss_after = rss_mib()
        arrow_mib = real(parquet_get_arrow_bytes_allocated(), real64) / 1048576.0_real64
        data_mib = real(t%nrows(), real64) * real(t%ncols(), real64) * 8.0_real64 / 1048576.0_real64

        write(output_unit, '(a)') "--- read: table (open + materialize every column) ---"
        write(output_unit, '(a,i0,a,i0)') "rows: ", t%nrows(), "   columns: ", t%ncols()
        write(output_unit, '(a,f10.3,a)') "time                        : ", t_table, " s"
        write(output_unit, '(a,f10.1,a)') "column data (nrows*ncols*8) : ", data_mib, " MiB"
        write(output_unit, '(a,f10.1,a)') "Arrow pool still holding    : ", arrow_mib, " MiB"
        write(output_unit, '(a,f10.1,a)') "RSS                         : ", rss_after, " MiB"
        if (data_mib > 0.0_real64) then
            write(output_unit, '(a,f10.2,a)') "Arrow / column data         : ", arrow_mib / data_mib, " x"
        end if
        write(output_unit, '(a)') "The Arrow figure is the one that matters: near 0x means every"
        write(output_unit, '(a)') "column's Arrow buffers were released once its Fortran copy existed."
        write(output_unit, '(a)') "RSS stays high either way -- Arrow's pool does not return freed"
        write(output_unit, '(a)') "pages to the OS, so RSS cannot tell the two cases apart."
    end subroutine bench_read_table

    subroutine bench_access(file)
        character(len=*), intent(in) :: file !! fixture to read.
        type(parquet_table) :: t
        real(real64), allocatable :: g(:)
        real(real64), pointer :: p(:)
        real(real64) :: t0, t_get, t_col, s1, s2
        character(len=:), allocatable :: names(:)

        call parquet_open_table(t, file)
        call t%column_names(names)

        t0 = now()
        call t%get(trim(names(1)), g)
        s1 = sum(g)
        t_get = now() - t0

        t0 = now()
        call t%col(trim(names(1)), p)
        s2 = sum(p)
        t_col = now() - t0

        write(output_unit, '(a)') "--- access ---"
        write(output_unit, '(a,i0)') "rows: ", t%nrows()
        write(output_unit, '(a,f10.4,a)') "%get (copy + widen)         : ", t_get, " s"
        write(output_unit, '(a,f10.4,a)') "%col (pointer)              : ", t_col, " s"
        if (t_col > 0.0_real64) then
            write(output_unit, '(a,f10.2,a)') "get / col                   : ", t_get / t_col, " x"
        end if
        ! Both must agree, or one of the two paths is wrong -- a benchmark that silently
        ! measured a broken path would be worse than no benchmark.
        if (abs(s1 - s2) > 1.0e-6_real64 * max(abs(s1), 1.0_real64)) then
            write(error_unit, '(a)') "benchmark_table: get and col disagree -- one path is broken"
            error stop 1
        end if
    end subroutine bench_access

    subroutine bench_write(file)
        character(len=*), intent(in) :: file !! fixture to read, then write back.
        type(parquet_table) :: t
        type(parquet_schema) :: s
        type(parquet_writer) :: w
        real(real64), allocatable :: g(:)
        character(len=:), allocatable :: names(:)
        real(real64) :: t0, t_table, t_raw
        integer :: i

        call parquet_open_table(t, file)
        call t%column_names(names)
        call s%init("bench")
        do i = 1, size(names)
            call s%add_field(trim(names(i)), "float64")
        end do
        call parquet_parse_maml(s)

        t0 = now()
        call parquet_write_table(t, "benchmark_table_out1.parquet", s)
        t_table = now() - t0

        t0 = now()
        call parquet_open_writer(w, "benchmark_table_out2.parquet", s)
        do i = 1, size(names)
            call t%get(trim(names(i)), g)
            call parquet_write_column(w, trim(names(i)), g)
        end do
        call parquet_close_writer(w)
        t_raw = now() - t0

        write(output_unit, '(a)') "--- write ---"
        write(output_unit, '(a,f10.3,a)') "parquet_write_table         : ", t_table, " s"
        write(output_unit, '(a,f10.3,a)') "hand-written per-column loop: ", t_raw, " s"
        if (t_raw > 0.0_real64) then
            write(output_unit, '(a,f10.2,a)') "table / hand-written        : ", t_table / t_raw, " x"
        end if
    end subroutine bench_write

end program benchmark_table
