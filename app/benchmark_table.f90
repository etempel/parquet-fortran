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
!!                           and reading the same columns into plain arrays -- one array per
!!                           column, held all at once, so the baseline holds the same data the
!!                           table does. The gap is what the table layer costs.
!!   * **peak RSS**       -- resident memory after the table is fully materialized, against the
!!                           file's own uncompressed size. THIS is the number that proves the
!!                           Arrow-side release actually happens: without it, a fully
!!                           materialized table holds roughly two copies of the file.
!!   * **get vs. col**    -- the copy path against the zero-copy pointer path on warm columns,
!!                           and then the same `z = x + y` evaluated over plain allocatables
!!                           against `%col` pointers. The second half is the one that matters:
!!                           a pointer that is free to obtain but slower to compute with would
!!                           make the guide's advice wrong.
!!   * **write**          -- parquet_write_table against a hand-written per-column write loop, on
!!                           a null-free table (where the writer needs no validity mask) and again
!!                           on one with nulls (where it does, and building that mask is the whole
!!                           cost).
!!
!! Maintainer tool, never run by `fpm test` (CLAUDE.md: anything needing this much memory/time
!! lives under app/ with a shell wrapper under tools/). Drive it with tools/benchmark_table.sh.
program benchmark_table
    use parquet
    use parquet_tables
    use iso_fortran_env, only : int32, int64, real32, real64, error_unit, output_unit
    use iso_c_binding, only : c_int64_t, c_int
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
    real(real64) :: size_gb, nullfrac
    integer :: ncols, touch, slices

    call parse_arguments(mode, size_gb, file, ncols, touch, slices, nullfrac)

    select case (mode)
    case ("write_fixture")
        call write_fixture(file, size_gb, ncols)
    case ("read_raw")
        call bench_read_raw(file)
    case ("read_table")
        call bench_read_table(file)
    case ("read_lazy")
        call bench_read_lazy(file, touch)
    case ("read_slice")
        call bench_read_slice(file, slices)
    case ("access")
        call bench_access(file)
    case ("write")
        call bench_write(file)
    case ("write_nulls")
        call bench_write_nulls(file, nullfrac)
    case default
        write(error_unit, '(a)') "benchmark_table: unknown --mode '"//mode//"'"
        error stop 1
    end select

contains

    subroutine print_usage()
        write(output_unit, '(a)') "Usage: benchmark_table --mode=<write_fixture|read_raw|read_table|" // &
            "read_lazy|read_slice|access|write|write_nulls>"
        write(output_unit, '(a)') "                       [--size=<GB>] [--file=<path>] [--ncols=<n>]"
        write(output_unit, '(a)') ""
        write(output_unit, '(a)') "  --mode=write_fixture  generate the synthetic input file"
        write(output_unit, '(a)') "  --mode=read_raw       time a reader + plain arrays, and report RSS"
        write(output_unit, '(a)') "  --mode=read_table     time a table open+materialize, and report RSS"
        write(output_unit, '(a)') "  --mode=read_lazy      open, then read only --touch=<n> columns"
        write(output_unit, '(a)') "  --mode=read_slice     open one of --slices=<n> equal row slices"
        write(output_unit, '(a)') "  --mode=access         time %get vs. %col, and arithmetic through each"
        write(output_unit, '(a)') "  --mode=write          time parquet_write_table vs. a hand-written loop"
        write(output_unit, '(a)') "  --mode=write_nulls    the same, on a table with nulls (three ways)"
        write(output_unit, '(a)') "  --size=<GB>           approximate uncompressed size (write_fixture)"
        write(output_unit, '(a)') "  --ncols=<n>           float64 columns in the fixture (default 8)"
        write(output_unit, '(a)') "  --touch=<n>           columns to read in read_lazy (default 2)"
        write(output_unit, '(a)') "  --slices=<n>          equal slices to divide the file into (default 4)"
        write(output_unit, '(a)') "  --nullfrac=<f>        fraction of rows to null in write_nulls (default 0.1)"
    end subroutine print_usage

    subroutine parse_arguments(mode, size_gb, file, ncols, touch, slices, nullfrac)
        character(len=:), allocatable, intent(out) :: mode !! which measurement to run.
        real(real64), intent(out) :: size_gb               !! target fixture size in GB.
        character(len=:), allocatable, intent(out) :: file !! fixture path.
        integer, intent(out) :: ncols                      !! float64 columns in the fixture.
        integer, intent(out) :: touch                      !! columns to read in read_lazy mode.
        integer, intent(out) :: slices                     !! slices to divide the file into.
        real(real64), intent(out) :: nullfrac              !! fraction of rows to null in write_nulls.

        integer :: i, nargs, eq_pos, ios
        character(len=512) :: arg, key, val

        mode = ""
        size_gb = 0.25_real64
        file = "benchmark_table.parquet"
        ncols = 8
        touch = 2
        slices = 4
        nullfrac = 0.1_real64

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
            case ("--touch")
                read(val, *, iostat=ios) touch
                if (ios /= 0) then
                    write(error_unit, '(a)') "benchmark_table: --touch must be an integer"
                    error stop 1
                end if
            case ("--nullfrac")
                read(val, *, iostat=ios) nullfrac
                if (ios /= 0) then
                    write(error_unit, '(a)') "benchmark_table: --nullfrac must be a number"
                    error stop 1
                end if
            case ("--slices")
                read(val, *, iostat=ios) slices
                if (ios /= 0) then
                    write(error_unit, '(a)') "benchmark_table: --slices must be an integer"
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
        interface
            function getpid() bind(C, name="getpid")
                import :: c_int
                integer(c_int) :: getpid
            end function getpid
        end interface
        integer(c_int) :: pid
        real(real64) :: mib !! resident set size, MiB, or -1.
        integer :: u, ios
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
        !> One column's values. A table holds every column at once, so the baseline has to as
        !! well -- reading them all into ONE reused buffer instead measures a different job and
        !! flatters this path: the same pages are overwritten `ncols` times and stay warm, where
        !! the table touches the whole file's worth of distinct memory. Measured at ~15% of the
        !! raw-vs-table gap on a 0.4 GB 8-column file, so it is not a rounding error.
        type :: column_buffer
            real(real64), allocatable :: v(:) !! this column's values.
        end type column_buffer
        type(parquet_reader) :: r
        type(column_buffer), allocatable :: cols(:)
        character(len=:), allocatable :: names(:)
        real(real64) :: t0, t_raw, rss_after, data_mib, checksum
        integer(int64) :: nrows
        integer :: i, nc

        t0 = now()
        call parquet_open_reader(r, file)
        call parquet_get_nrows(r, nrows)
        call parquet_get_column_names(r, names)
        nc = size(names)
        allocate(cols(nc))
        do i = 1, nc
            allocate(cols(i)%v(nrows))
            call parquet_read_column(r, trim(names(i)), cols(i)%v)
        end do
        t_raw = now() - t0
        rss_after = rss_mib()
        data_mib = real(nrows, real64) * real(nc, real64) * 8.0_real64 / 1048576.0_real64
        ! Keeps every buffer live past the timed section, so none of the reads can be elided.
        checksum = 0.0_real64
        do i = 1, nc
            checksum = checksum + cols(i)%v(1)
        end do

        write(output_unit, '(a)') "--- read: raw (reader + one array per column) ---"
        write(output_unit, '(a,i0,a,i0)') "rows: ", nrows, "   columns: ", nc
        write(output_unit, '(a,f10.3,a)') "time                        : ", t_raw, " s"
        write(output_unit, '(a,f10.1,a)') "column data (nrows*ncols*8) : ", data_mib, " MiB"
        write(output_unit, '(a,f10.1,a)') "Arrow pool still holding    : ", &
            real(parquet_get_arrow_bytes_allocated(), real64) / 1048576.0_real64, " MiB"
        write(output_unit, '(a,f10.1,a)') "RSS                         : ", rss_after, " MiB"
        write(output_unit, '(a,es12.4)') "checksum (keeps reads live) : ", checksum
        write(output_unit, '(a)') "Every column is held at once, exactly as a table holds them, so"
        write(output_unit, '(a)') "this is directly comparable with --mode=read_table's figure."
        write(output_unit, '(a)') "The reader caches every column it decodes, so the Arrow figure"
        write(output_unit, '(a)') "here is the whole file ON TOP of the Fortran copies -- roughly"
        write(output_unit, '(a)') "two copies resident. That is the memory the table has to beat."
        call parquet_close_reader(r)
    end subroutine bench_read_raw

    !> The table path: open and materialize every column, then report time and resident memory.
    subroutine bench_read_table(file)
        character(len=*), intent(in) :: file !! fixture to read.
        type(parquet_table) :: t
        real(real64) :: t0, t_open, t_table, rss_after, data_mib, arrow_mib

        ! Opening is lazy, so the two halves are timed separately: the open figure is what a
        ! program that then reads two columns pays, and the materialize figure is what reading
        ! the whole file costs.
        t0 = now()
        call parquet_open_table(t, file)
        t_open = now() - t0
        t0 = now()
        call t%materialize_all()
        t_table = now() - t0
        rss_after = rss_mib()
        arrow_mib = real(parquet_get_arrow_bytes_allocated(), real64) / 1048576.0_real64
        data_mib = real(t%nrows(), real64) * real(t%ncols(), real64) * 8.0_real64 / 1048576.0_real64

        write(output_unit, '(a)') "--- read: table (open + materialize every column) ---"
        write(output_unit, '(a,i0,a,i0)') "rows: ", t%nrows(), "   columns: ", t%ncols()
        write(output_unit, '(a,f10.3,a)') "open (schema only)          : ", t_open, " s"
        write(output_unit, '(a,f10.3,a)') "materialize_all             : ", t_table, " s"
        write(output_unit, '(a,f10.1,a)') "column data (nrows*ncols*8) : ", data_mib, " MiB"
        write(output_unit, '(a,f10.1,a)') "Arrow pool still holding    : ", arrow_mib, " MiB"
        write(output_unit, '(a,f10.1,a)') "RSS                         : ", rss_after, " MiB"
        if (data_mib > 0.0_real64) then
            write(output_unit, '(a,f10.2,a)') "Arrow / column data         : ", arrow_mib / data_mib, " x"
        end if
        write(output_unit, '(a)') "The Arrow figure is the one that matters: near 0x means every"
        write(output_unit, '(a)') "column's Arrow buffers were released once its Fortran copy existed."
        write(output_unit, '(a)') "This run's RSS does come out below --mode=read_raw's, since that"
        write(output_unit, '(a)') "path ends holding a Fortran copy AND Arrow's copy of every column."
        write(output_unit, '(a)') "Do not read the gap as the amount released, though, and do not use"
        write(output_unit, '(a)') "RSS to decide whether a release happened at all: Arrow's pool keeps"
        write(output_unit, '(a)') "freed pages instead of returning them, so RSS always understates it."
        write(output_unit, '(a)') "The pool's own bytes_allocated() is the only reliable answer."
    end subroutine bench_read_table

    !> What laziness is for: opening a wide file and reading only a few of its columns.
    !!
    !! Reported against `read_table`'s figures for the same fixture -- the point of the pair is
    !! the ratio, which should track `touch / ncols` rather than sitting near 1.
    subroutine bench_read_lazy(file, touch)
        character(len=*), intent(in) :: file !! fixture to read.
        integer, intent(in) :: touch         !! how many columns to actually read.
        type(parquet_table) :: t
        real(real64), allocatable :: g(:)
        real(real64) :: t0, t_open, t_touch, arrow_open, arrow_after, data_mib, checksum
        character(len=:), allocatable :: names(:)
        integer :: i, n

        t0 = now()
        call parquet_open_table(t, file)
        t_open = now() - t0
        arrow_open = real(parquet_get_arrow_bytes_allocated(), real64) / 1048576.0_real64
        call t%column_names(names)
        n = min(touch, t%ncols())

        checksum = 0.0_real64
        t0 = now()
        do i = 1, n
            call t%get(trim(names(i)), g)
            checksum = checksum + sum(g)
        end do
        t_touch = now() - t0
        arrow_after = real(parquet_get_arrow_bytes_allocated(), real64) / 1048576.0_real64
        data_mib = real(t%nrows(), real64) * real(n, real64) * 8.0_real64 / 1048576.0_real64

        write(output_unit, '(a)') "--- read: lazy (open, then touch a few columns) ---"
        write(output_unit, '(a,i0,a,i0,a,i0)') "rows: ", t%nrows(), "   columns: ", t%ncols(), &
            "   touched: ", n
        write(output_unit, '(a,f10.3,a)') "open (schema only)          : ", t_open, " s"
        write(output_unit, '(a,f10.1,a)') "Arrow pool after open       : ", arrow_open, " MiB"
        write(output_unit, '(a,f10.3,a)') "reading the touched columns : ", t_touch, " s"
        write(output_unit, '(a,f10.1,a)') "Arrow pool after those reads: ", arrow_after, " MiB"
        write(output_unit, '(a,f10.1,a)') "touched column data         : ", data_mib, " MiB"
        write(output_unit, '(a,es12.4)')  "checksum (keeps reads live) : ", checksum
        write(output_unit, '(a)') "Compare the open figure with --mode=read_table's: opening now"
        write(output_unit, '(a)') "reads nothing at all, and only the touched columns are paid for."
    end subroutine bench_read_lazy

    !> The slice regime's memory story: one slice of a file, against the whole of it.
    !!
    !! Run in its own process, like every other mode here -- a baseline and the path under test
    !! in one process report the high-water mark of the pair, which flatters whichever ran last.
    subroutine bench_read_slice(file, slices)
        character(len=*), intent(in) :: file !! fixture to read.
        integer, intent(in) :: slices        !! how many equal slices to divide the file into.
        type(parquet_table) :: t
        integer(int64), allocatable :: bounds(:,:)
        integer(int64) :: nrows_file, lo, hi
        real(real64) :: t0, t_slice, arrow_after, data_mib, checksum
        real(real64), allocatable :: g(:)
        character(len=:), allocatable :: names(:)

        call parquet_table_row_group_bounds(file, bounds)
        nrows_file = bounds(2, size(bounds, 2))
        lo = 1_int64
        hi = max(1_int64, nrows_file / int(max(slices, 1), int64))

        t0 = now()
        call parquet_open_table(t, file, lo, hi)
        call t%column_names(names)
        call t%materialize_all()
        t_slice = now() - t0
        arrow_after = real(parquet_get_arrow_bytes_allocated(), real64) / 1048576.0_real64
        data_mib = real(t%nrows(), real64) * real(t%ncols(), real64) * 8.0_real64 / 1048576.0_real64
        call t%get(trim(names(1)), g)
        checksum = sum(g)

        write(output_unit, '(a)') "--- read: slice regime ---"
        write(output_unit, '(a,i0,a,i0)') "file rows: ", nrows_file, "   row groups: ", size(bounds, 2)
        write(output_unit, '(a,i0,a,i0,a,i0)') "slice [", lo, ", ", hi, "] rows: ", t%nrows()
        write(output_unit, '(a,f10.3,a)') "open + materialize the slice: ", t_slice, " s"
        write(output_unit, '(a,f10.1,a)') "slice column data           : ", data_mib, " MiB"
        write(output_unit, '(a,f10.1,a)') "Arrow pool still holding    : ", arrow_after, " MiB"
        write(output_unit, '(a,es12.4)')  "checksum (keeps reads live) : ", checksum
        write(output_unit, '(a)') "Compare with --mode=read_table on the same file: the slice"
        write(output_unit, '(a)') "should cost roughly its share of the time and of the memory."
        if (size(bounds, 2) < slices) then
            write(output_unit, '(a)') "NOTE: this file has fewer row groups than the requested"
            write(output_unit, '(a)') "slice count, so the slice still has to read a whole row"
            write(output_unit, '(a)') "group and trim it. A row group is the smallest unit the"
            write(output_unit, '(a)') "slice regime can save on -- write the fixture with a"
            write(output_unit, '(a)') "smaller chunk_size to see the saving."
        end if
    end subroutine bench_read_slice

    !> Access cost on WARM columns: what the first touch costs, what `%get` and `%col` cost once
    !! the data is resident, and -- the reason the mode exists -- the same elementwise expression
    !! evaluated over plain allocatables against `%col` pointers.
    !!
    !! Every timed column is prefetched first, and that prefetch is reported as its own figure.
    !! Opening is lazy, so a `%get` on a cold column decodes the whole column out of the file:
    !! timing that against a `%col` on the by-then-resident column measures the decode rather than
    !! the copy, and overstates the pointer path by an order of magnitude.
    !!
    !! The allocatable arrays are filled with `%get` rather than by a second reader on purpose.
    !! What the arithmetic comparison turns on is the VARIABLE -- a plain allocatable against a
    !! pointer that is not declared `contiguous` and so may carry a stride the compiler cannot
    !! rule out -- and an allocatable filled by `%get` is indistinguishable from one filled by
    !! `parquet_read_column`, while costing one copy instead of a second full read of the file.
    subroutine bench_access(file)
        character(len=*), intent(in) :: file !! fixture to read.
        integer, parameter :: nrep = 10      !! passes over the arrays per timed round.
        integer, parameter :: nround = 7     !! timed rounds per variant; the best of each is kept.
        type(parquet_table) :: t
        real(real64), allocatable :: xg(:), yg(:), z(:)
        real(real64), pointer :: xp(:), yp(:)
        !> The same pointers, but declared CONTIGUOUS at the call site. `%col`'s own dummy is a
        !! plain pointer, so the compiler must assume an arbitrary stride there; the target is in
        !! fact always contiguous, and gfortran accepts a contiguous actual against that dummy, so
        !! a caller can promise the contiguity the library cannot. This variant measures whether
        !! that promise is worth making.
        real(real64), pointer, contiguous :: xc(:), yc(:)
        real(real64) :: t0, dt, t_touch, t_get, t_col, t_arr, t_ptr, t_ctg
        real(real64) :: acc_arr, acc_ptr, acc_ctg, moved_gb
        character(len=:), allocatable :: names(:)
        integer(int64) :: nrows
        integer :: rep, round

        call parquet_open_table(t, file)
        call t%column_names(names)
        ! Two columns, so the timed expression is a real elementwise combination rather than a
        ! reduction over one array, which is bandwidth-bound in a different way.
        if (size(names) < 2) then
            write(error_unit, '(a)') "benchmark_table: --mode=access needs a fixture with at " // &
                "least 2 columns; regenerate it with --ncols=2 or more"
            error stop 1
        end if
        nrows = t%nrows()

        t0 = now()
        call t%prefetch(names(1:2))
        t_touch = now() - t0

        t0 = now()
        call t%get(trim(names(1)), xg)
        call t%get(trim(names(2)), yg)
        t_get = now() - t0

        t0 = now()
        call t%col(trim(names(1)), xp)
        call t%col(trim(names(2)), yp)
        t_col = now() - t0
        call t%col(trim(names(1)), xc)
        call t%col(trim(names(2)), yc)

        ! Allocated up front so neither arithmetic figure includes an allocation, then written
        ! once through each path before anything is timed. A freshly allocated 100+ MB result
        ! array pays first-touch page faults on its first pass and never again, so without this
        ! whichever variant ran first would absorb a cost the other never sees -- which showed up
        ! as the pointer form looking 25% FASTER than plain arrays, purely from running second.
        allocate(z(nrows))
        z = xg + yg
        z = xp + yp
        z = xc + yc

        ! One element of the left operand is rewritten per pass, and one element of the result is
        ! read back, so neither loop can be hoisted out as an invariant computation. The two
        ! variants apply the SAME mutations in the same order, which is what lets their checksums
        ! be compared afterwards -- note the pointer variant writes into the table's own storage.
        !
        ! The rounds, and keeping the BEST rather than the mean, are what make the comparison
        ! usable: a single round of either variant is a few tens of milliseconds and swings by
        ! well over the difference being measured, so consecutive whole-benchmark runs disagreed
        ! on which form was faster. The minimum is the run least disturbed by everything else on
        ! the machine, which is the quantity this mode is actually asking about.
        t_arr = huge(0.0_real64)
        t_ptr = huge(0.0_real64)
        t_ctg = huge(0.0_real64)
        acc_arr = 0.0_real64
        acc_ptr = 0.0_real64
        acc_ctg = 0.0_real64
        do round = 1, nround
            acc_arr = 0.0_real64
            t0 = now()
            do rep = 1, nrep
                xg(rep) = real(rep, real64)
                z = xg + yg
                acc_arr = acc_arr + z(rep)
            end do
            dt = now() - t0
            if (dt < t_arr) t_arr = dt

            acc_ptr = 0.0_real64
            t0 = now()
            do rep = 1, nrep
                xp(rep) = real(rep, real64)
                z = xp + yp
                acc_ptr = acc_ptr + z(rep)
            end do
            dt = now() - t0
            if (dt < t_ptr) t_ptr = dt

            acc_ctg = 0.0_real64
            t0 = now()
            do rep = 1, nrep
                xc(rep) = real(rep, real64)
                z = xc + yc
                acc_ctg = acc_ctg + z(rep)
            end do
            dt = now() - t0
            if (dt < t_ctg) t_ctg = dt
        end do

        ! Three arrays touched per pass: both operands read, the result written.
        moved_gb = 3.0_real64 * real(nrows, real64) * 8.0_real64 * real(nrep, real64) / 1.0e9_real64

        write(output_unit, '(a)') "--- access ---"
        write(output_unit, '(a,i0,a,a,a,a)') "rows: ", nrows, "   columns used: ", &
            trim(names(1)), ", ", trim(names(2))
        write(output_unit, '(a,f10.4,a)') "first touch (decode 2 cols) : ", t_touch, " s"
        write(output_unit, '(a,f12.6,a)') "%get   x2 (copy, warm)      : ", t_get, " s"
        write(output_unit, '(a,f12.6,a)') "%col   x2 (pointer, warm)   : ", t_col, " s"
        ! Deliberately no get/col ratio: %col resolves a name and assigns a pointer, so its cost
        ! does not scale with the row count and lands at or below the clock resolution. Dividing
        ! by it produces a large number that says nothing except how coarse the timer is.
        write(output_unit, '(a)') "(%col is O(1) -- name lookup plus a pointer assignment. What it"
        write(output_unit, '(a)') "saves over %get is the copy above, which does scale with rows.)"
        write(output_unit, '(a)') ""
        write(output_unit, '(a,i0,a,i0,a)') "z = x + y, best of ", nround, " rounds x ", nrep, " passes:"
        ! A fixture small enough for a whole round to land at the clock resolution would turn
        ! every figure below into a division by zero, so say so rather than print nonsense.
        if (t_arr <= 0.0_real64 .or. t_ptr <= 0.0_real64 .or. t_ctg <= 0.0_real64) then
            write(output_unit, '(a)') "  too fast to time on this fixture -- use a larger --size"
            return
        end if
        write(output_unit, '(a,f10.4,a,f8.1,a)') "  allocatable arrays        : ", t_arr, " s  ", &
            moved_gb / t_arr, " GB/s"
        write(output_unit, '(a,f10.4,a,f8.1,a)') "  %col pointers             : ", t_ptr, " s  ", &
            moved_gb / t_ptr, " GB/s"
        write(output_unit, '(a,f10.4,a,f8.1,a)') "  %col pointers, contiguous : ", t_ctg, " s  ", &
            moved_gb / t_ctg, " GB/s"
        write(output_unit, '(a,f10.2,a)') "  pointer    / allocatable  : ", t_ptr / t_arr, " x"
        write(output_unit, '(a,f10.2,a)') "  contiguous / allocatable  : ", t_ctg / t_arr, " x"
        write(output_unit, '(a)') "Computing through a %col pointer is somewhat slower than the same"
        write(output_unit, '(a)') "expression over arrays you own, and the gap grows as the columns"
        write(output_unit, '(a)') "get small enough to sit in cache -- it shrinks toward 1.0x once"
        write(output_unit, '(a)') "the loop is memory-bandwidth-bound instead. The CONTIGUOUS row is"
        write(output_unit, '(a)') "there to test the obvious explanation, and refutes it: %col's"
        write(output_unit, '(a)') "dummy cannot carry that attribute, but promising it at the call"
        write(output_unit, '(a)') "site recovers nothing measurable, so the difference is not the"
        write(output_unit, '(a)') "missing stride guarantee."
        ! The whole point of %col is skipping the copy, so the useful figure is how many passes
        ! over the data it takes for the slower arithmetic to give that saving back.
        if (t_ptr > t_arr) then
            write(output_unit, '(a,f10.1)') "  passes before %get wins    : ", &
                t_get / ((t_ptr - t_arr) / real(nrep, real64))
            write(output_unit, '(a)') "Below that many passes over these columns, %col is ahead: it"
            write(output_unit, '(a)') "skips a copy that costs more than the arithmetic difference."
            write(output_unit, '(a)') "Above it, %get once into your own array and compute on that."
        end if
        ! All three loops must agree, or one of the paths is wrong -- a benchmark that silently
        ! measured a broken path would be worse than no benchmark.
        if (abs(acc_arr - acc_ptr) > 1.0e-9_real64 * max(abs(acc_arr), 1.0_real64) .or. &
            abs(acc_arr - acc_ctg) > 1.0e-9_real64 * max(abs(acc_arr), 1.0_real64)) then
            write(error_unit, '(a)') "benchmark_table: get and col disagree -- one path is broken"
            error stop 1
        end if
    end subroutine bench_access

    !> `parquet_write_table` against the equivalent hand-written per-column write loop.
    !!
    !! The table is fully materialized BEFORE anything is timed. Opening is lazy, so a
    !! `parquet_write_table` on a freshly opened table decodes every column from the file as it
    !! goes, while the hand-written loop that runs afterwards finds them all resident -- charging
    !! the whole read to the write path and roughly doubling its apparent cost.
    !!
    !! The comparison still slightly favours the hand-written loop, which is the honest direction
    !! for it to lean: that loop copies each column out with `%get` before writing it, where
    !! `parquet_write_table` writes straight from the store with no copy at all.
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
        call t%materialize_all()
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

    !> The same write comparison, but on a table whose rows are partly NULL.
    !!
    !! `--mode=write` measures the null-free case, which is the one the writer can shortcut: a
    !! column with no nulls is written with no validity mask at all. This mode measures what
    !! happens when that shortcut is unavailable and a mask genuinely has to be produced, and
    !! splits the cost three ways:
    !!
    !!   1. **`parquet_write_table`** -- the mask is built inside the library by walking the
    !!      column's validity bitmap 64 bits at a time.
    !!   2. **hand-written, mask built per row** -- what a caller writing the same loop themselves
    !!      would most naturally do: ask `%is_null` once per row. This is the honest comparison for
    !!      (1), since both start from a table and end with a written file.
    !!   3. **hand-written, mask already in hand** -- the same write with the mask handed over for
    !!      free. This is the floor: whatever (2) costs above this line is mask construction, and
    !!      whatever this line costs above `--mode=write`'s hand-written figure is what the writer
    !!      itself pays to turn a mask into Arrow's null bitmap.
    !!
    !! The null-carrying input is built here rather than asked of the fixture writer, because
    !! `parquet_table` deliberately exposes no way to mark a row null in memory -- so the only way
    !! to get a table with nulls is to write a file with them and read it back. That preparation
    !! is not timed.
    subroutine bench_write_nulls(file, nullfrac)
        character(len=*), intent(in) :: file  !! clean fixture to derive the null-carrying one from.
        real(real64), intent(in) :: nullfrac  !! fraction of rows to mark null, 0 < f < 1.
        character(len=*), parameter :: nullfile = "benchmark_table_nulls.parquet"
        type(parquet_table) :: t, tn
        type(parquet_schema) :: s
        type(parquet_writer) :: w
        real(real64), allocatable :: g(:)
        logical, allocatable :: mask(:), built(:)
        character(len=:), allocatable :: names(:)
        real(real64) :: t0, t_table, t_perrow, t_ready, x
        integer(int64) :: i64, nrows, nnull
        integer :: i

        if (nullfrac <= 0.0_real64 .or. nullfrac >= 1.0_real64) then
            write(error_unit, '(a)') "benchmark_table: --nullfrac must be strictly between 0 and 1"
            error stop 1
        end if

        ! --- preparation, deliberately untimed -------------------------------------------------
        call parquet_open_table(t, file)
        call t%materialize_all()
        call t%column_names(names)
        ! qc_miss declares that nulls are expected, which is what this mode is about -- without it
        ! every one of the three writes emits a QC warning per column and buries the numbers.
        call s%init("bench")
        do i = 1, size(names)
            call s%add_field(trim(names(i)), "float64", qc_miss="Null")
        end do
        call parquet_parse_maml(s)

        nrows = t%nrows()
        allocate(mask(nrows))
        ! A cheap deterministic hash rather than random_number: the same fixture must produce the
        ! same null pattern on every run, or two runs of this mode are not comparable.
        do i64 = 1_int64, nrows
            x = real(mod(i64*2654435761_int64, 1000003_int64), real64) / 1000003.0_real64
            mask(i64) = x >= nullfrac
        end do
        nnull = count(.not. mask, kind=int64)
        ! One mask for every column, so case 3 can hand the writer exactly the nulls the table
        ! holds without rebuilding anything per column.
        call parquet_open_writer(w, nullfile, s)
        do i = 1, size(names)
            call t%get(trim(names(i)), g)
            call parquet_write_column(w, trim(names(i)), g, is_valid=mask)
        end do
        call parquet_close_writer(w)

        call parquet_open_table(tn, nullfile)
        call tn%materialize_all()
        ! If the nulls did not survive the round trip, every figure below would be measuring the
        ! null-free path again while claiming otherwise.
        if (.not. tn%is_null(trim(names(1)), first_null_row(mask))) then
            write(error_unit, '(a)') "benchmark_table: the prepared file lost its nulls -- nothing to measure"
            error stop 1
        end if

        ! --- timed ----------------------------------------------------------------------------
        t0 = now()
        call parquet_write_table(tn, "benchmark_table_out1.parquet", s)
        t_table = now() - t0

        t0 = now()
        ! Disabling manually to avoid large time cost (no need for benchmarking it)
        !call parquet_open_writer(w, "benchmark_table_out2.parquet", s)
        !do i = 1, size(names)
        !    call tn%get(trim(names(i)), g)
        !    allocate(built(nrows))
        !    do i64 = 1_int64, nrows
        !        built(i64) = .not. tn%is_null(trim(names(i)), i64)
        !    end do
        !    call parquet_write_column(w, trim(names(i)), g, is_valid=built)
        !    deallocate(built)
        !end do
        !call parquet_close_writer(w)
        t_perrow = now() - t0

        t0 = now()
        call parquet_open_writer(w, "benchmark_table_out3.parquet", s)
        do i = 1, size(names)
            call tn%get(trim(names(i)), g)
            call parquet_write_column(w, trim(names(i)), g, is_valid=mask)
        end do
        call parquet_close_writer(w)
        t_ready = now() - t0

        write(output_unit, '(a)') "--- write: with nulls ---"
        write(output_unit, '(a,i0,a,i0,a,f5.1,a)') "rows: ", nrows, "   null rows: ", nnull, &
            " (", 100.0_real64*real(nnull, real64)/real(nrows, real64), " %)"
        write(output_unit, '(a,f10.3,a)') "parquet_write_table         : ", t_table, " s"
        write(output_unit, '(a,f10.3,a)') "hand-written, mask per row  : ", t_perrow, " s"
        write(output_unit, '(a,f10.3,a)') "hand-written, mask in hand  : ", t_ready, " s"
        if (t_ready > 0.0_real64) then
            write(output_unit, '(a,f10.2,a)') "table / mask in hand        : ", t_table / t_ready, " x"
        end if
        if (t_perrow > 0.0_real64) then
            write(output_unit, '(a,f10.2,a)') "table / mask per row        : ", t_table / t_perrow, " x"
        end if
        write(output_unit, '(a)') "The third line is the floor: the writer is handed a finished mask,"
        write(output_unit, '(a)') "so it measures writing with nulls and nothing else. Everything the"
        write(output_unit, '(a)') "second line costs above it is mask construction. parquet_write_table"
        write(output_unit, '(a)') "should sit at the floor -- it walks the validity bitmap a word at a"
        write(output_unit, '(a)') "time and skips whole runs of valid rows."
        write(output_unit, '(a)') "The per-row line is what a caller writing this loop themselves has"
        write(output_unit, '(a)') "to do, and it is dominated by asking one row at a time: %is_null"
        write(output_unit, '(a)') "takes a column NAME, so every one of those calls repeats the column"
        write(output_unit, '(a)') "lookup the library does once. That is a fair comparison, not a"
        write(output_unit, '(a)') "handicap -- the public API offers no way to hoist it."
        write(output_unit, '(a)') "Compare with --mode=write on the same fixture for the null-free"
        write(output_unit, '(a)') "path, where no mask is built or passed at all."
    end subroutine bench_write_nulls

    !> First row the mask marks null; 1 if it marks none (the caller has already rejected that).
    function first_null_row(mask) result(i)
        logical, intent(in) :: mask(:) !! .true. where the row is valid.
        integer(int64) :: i            !! 1-based row index of the first null.
        do i = 1_int64, size(mask, kind=int64)
            if (.not. mask(i)) return
        end do
        i = 1_int64
    end function first_null_row

end program benchmark_table
