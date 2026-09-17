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
!!   * **write stream**   -- the parquet_table_writer sink against the hand-written buffer loop
!!                           it replaces (they should be at parity), and the same loop with every
!!                           column declared protected_cols:, which is the only way to write a
!!                           streamed table without a validity mask per column per row group.
!!
!! Maintainer tool, never run by `fpm test` (CLAUDE.md: anything needing this much memory/time
!! lives under app/ with a shell wrapper under tools/). Drive it with bench/benchmark_table.sh.
program benchmark_table
    use parquet
    use parquet_tables
    ! The %apply callback of --mode=group, in a module because a callback may not be an internal
    ! procedure of a program (.claude/rules/fortran-gotchas.md, flang).
    use benchmark_group_callbacks, only : bench_group_payload, bench_group_sum
    use iso_fortran_env, only : int8, int32, int64, real64, error_unit, output_unit
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
        !> Threads the last row-structural table mutation resolved to; 1 when it ran serially.
        !!
        !! Used by `--mode=peakmem` only, to prove the run under measurement actually threaded --
        !! a peak-memory comparison between two thread counts is meaningless if the second one
        !! silently ran serially. Same local-declaration convention as the hook above.
        function parquet_debug_get_table_threads_used() &
                bind(C, name="parquet_debug_get_table_threads_used") result(res)
            import :: c_int64_t
            integer(c_int64_t) :: res !! resolved thread count of the last mutation.
        end function parquet_debug_get_table_threads_used
        !> The team the last per-group loop of a `parquet_grouping` ran on; 1 when it ran serially.
        !!
        !! Used by `--mode=group` as the CONTROL of its `%apply` thread ladder: "the ladder does
        !! not scale" and "no team ever opened" print the same times, and only this says which was
        !! measured. It also shows a rung the affinity clamp lowered. Same local-declaration
        !! convention as the hooks above.
        function parquet_debug_get_group_threads_used() &
                bind(C, name="parquet_debug_get_group_threads_used") result(res)
            import :: c_int64_t
            integer(c_int64_t) :: res !! the team; 1 means serial.
        end function parquet_debug_get_group_threads_used
        !> The resolved `use_threads` the most recently opened reader or writer was given.
        !!
        !! Used by `--mode=read_one` as its NEGATIVE CONTROL. That mode's whole result is "the two
        !! arms take the same time", and without this there is no way to tell that finding from the
        !! flag never having reached the reader at all -- which would produce an identical output.
        function parquet_debug_get_last_use_threads() &
                bind(C, name="parquet_debug_get_last_use_threads") result(res)
            import :: c_int
            integer(c_int) :: res !! 1 or 0 as resolved; -1 if nothing has been opened.
        end function parquet_debug_get_last_use_threads
        !> How many row groups the most recent filter install's statistics screen ruled out.
        !!
        !! Used by `--mode=read_filtered` to report what the screen did, because that decides
        !! which case the run measured: a filter the screen can prune is cheap on BOTH engines
        !! and says nothing about the bounded one. A run that prints 0 here is the scattered
        !! case the bounded engine exists for. Same local-declaration convention as above.
        function parquet_debug_get_row_groups_pruned() &
                bind(C, name="parquet_debug_get_row_groups_pruned") result(res)
            import :: c_int64_t
            integer(c_int64_t) :: res !! pruned row-group count of the last screen.
        end function parquet_debug_get_row_groups_pruned
    end interface

    character(len=:), allocatable :: mode, file
    real(real64) :: size_gb, nullfrac, select_frac
    integer :: ncols, touch, slices, threads, scatter, chunk, bounded, batches
    integer(int64) :: nrows_arg, groups_arg

    call parse_arguments(mode, size_gb, file, ncols, touch, slices, nullfrac, threads, nrows_arg, &
        select_frac, scatter, chunk, bounded, batches, groups_arg)

    select case (mode)
    case ("write_fixture")
        call write_fixture(file, size_gb, ncols, scatter, chunk)
    case ("read_raw")
        call bench_read_raw(file)
    case ("read_table")
        call bench_read_table(file)
    case ("read_lazy")
        call bench_read_lazy(file, touch)
    case ("read_slice")
        call bench_read_slice(file, slices)
    case ("read_filtered")
        call bench_read_filtered(file, select_frac, scatter, bounded /= 0)
    case ("access")
        call bench_access(file)
    case ("write")
        call bench_write(file)
    case ("write_nulls")
        call bench_write_nulls(file, nullfrac)
    case ("write_stream")
        call bench_write_stream(file, batches, chunk)
    case ("sort")
        call bench_sort(size_gb, ncols)
    case ("peakmem")
        call bench_peakmem(size_gb, ncols, threads)
    case ("read_one")
        call bench_read_one(file)
    case ("argsort")
        call bench_argsort(nrows_arg, threads)
    case ("group")
        call bench_group(nrows_arg, groups_arg)
    case default
        write(error_unit, '(a)') "benchmark_table: unknown --mode '"//mode//"'"
        error stop 1
    end select

contains

    subroutine print_usage()
        write(output_unit, '(a)') "Usage: benchmark_table --mode=<write_fixture|read_raw|read_table|" // &
            "read_lazy|read_slice|access|write|write_nulls|write_stream|sort|argsort>"
        write(output_unit, '(a)') "                       [--size=<GB>] [--file=<path>] [--ncols=<n>]"
        write(output_unit, '(a)') ""
        write(output_unit, '(a)') "  --mode=write_fixture  generate the synthetic input file"
        write(output_unit, '(a)') "  --mode=read_raw       time a reader + plain arrays, and report RSS"
        write(output_unit, '(a)') "  --mode=read_table     time a table open+materialize, and report RSS"
        write(output_unit, '(a)') "  --mode=read_lazy      open, then read only --touch=<n> columns"
        write(output_unit, '(a)') "  --mode=read_slice     open one of --slices=<n> equal row slices"
        write(output_unit, '(a)') "  --mode=read_filtered  open with filter= and materialize; needs --scatter"
        write(output_unit, '(a)') "  --mode=access         time %get vs. %col, and arithmetic through each"
        write(output_unit, '(a)') "  --mode=write          time parquet_write_table vs. a hand-written loop"
        write(output_unit, '(a)') "  --mode=write_nulls    the same, on a table with nulls (three ways)"
        write(output_unit, '(a)') "  --mode=write_stream   the sink vs the hand-written buffer it replaces"
        write(output_unit, '(a)') "  --mode=sort           what %sort_by spends re-validating one permutation"
        write(output_unit, '(a)') "  --mode=argsort        pf_argsort at one thread count, split by sort phase"
        write(output_unit, '(a)') "  --mode=group          %group_by and the grouping's verbs, against the"
        write(output_unit, '(a)') "                        compositions they replace; needs --groups"
        write(output_unit, '(a)') "  --mode=peakmem        build a table and sort it ONCE, for external peak-RSS"
        write(output_unit, '(a)') "  --mode=read_one       time ONE column's read with Arrow's use_threads on/off"
        write(output_unit, '(a)') "  --nrows=<n>           rows for argsort and group modes (default 20000000)"
        write(output_unit, '(a)') "  --groups=<n>          distinct key values the group mode's fixture holds"
        write(output_unit, '(a)') "                        (default 1000); the row count is --nrows"
        write(output_unit, '(a)') "  --threads=<n>         sort threads for argsort mode (1 = serial; default 1);"
        write(output_unit, '(a)') "                        in peakmem mode, the table-mutation thread cap"
        write(output_unit, '(a)') "                        (1 = serial, 0 = automatic)"
        write(output_unit, '(a)') "  --size=<GB>           approximate uncompressed size (write_fixture)"
        write(output_unit, '(a)') "  --ncols=<n>           float64 columns in the fixture (default 8)"
        write(output_unit, '(a)') "  --touch=<n>           columns to read in read_lazy (default 2)"
        write(output_unit, '(a)') "  --slices=<n>          equal slices to divide the file into (default 4)"
        write(output_unit, '(a)') "  --nullfrac=<f>        fraction of rows to null in write_nulls (default 0.1)"
        write(output_unit, '(a)') "  --batches=<n>         batches write_stream hands over (default 32)"
        write(output_unit, '(a)') "  --scatter=<n>         write_fixture: add an int32 'key' column cycling"
        write(output_unit, '(a)') "                        0..n-1, so no row group's key range excludes any"
        write(output_unit, '(a)') "                        filter value and the statistics screen prunes"
        write(output_unit, '(a)') "                        nothing (0, the default, writes no key column);"
        write(output_unit, '(a)') "                        read_filtered: the same n, to size its threshold"
        write(output_unit, '(a)') "  --select=<f>          fraction of rows read_filtered's filter keeps"
        write(output_unit, '(a)') "                        (default 0.01)"
        write(output_unit, '(a)') "  --chunk=<n>           write_fixture: rows per row group (0, the default,"
        write(output_unit, '(a)') "                        leaves the writer's own auto-sizing alone);"
        write(output_unit, '(a)') "                        write_stream: the flush threshold every arm uses"
        write(output_unit, '(a)') "                        (0, the default, is a quarter of the rows)"
        write(output_unit, '(a)') "  --bounded=<0|1>       read_filtered: open with bounded=.true. (default 0);"
        write(output_unit, '(a)') "                        run each arm in its OWN process, never both here"
    end subroutine print_usage

    subroutine parse_arguments(mode, size_gb, file, ncols, touch, slices, nullfrac, threads, nrows_arg, &
            select_frac, scatter, chunk, bounded, batches, groups_arg)
        character(len=:), allocatable, intent(out) :: mode !! which measurement to run.
        real(real64), intent(out) :: size_gb               !! target fixture size in GB.
        character(len=:), allocatable, intent(out) :: file !! fixture path.
        integer, intent(out) :: ncols                      !! float64 columns in the fixture.
        integer, intent(out) :: touch                      !! columns to read in read_lazy mode.
        integer, intent(out) :: slices                     !! slices to divide the file into.
        real(real64), intent(out) :: nullfrac              !! fraction of rows to null in write_nulls.
        integer, intent(out) :: threads                    !! sort threads in argsort mode.
        integer(int64), intent(out) :: nrows_arg           !! rows in argsort mode.
        real(real64), intent(out) :: select_frac           !! fraction of rows read_filtered keeps.
        integer, intent(out) :: scatter                    !! scattered key column's period; 0 = none.
        integer, intent(out) :: chunk                      !! explicit row-group size; 0 = auto.
        integer, intent(out) :: bounded                    !! 1 = open read_filtered with bounded=.true.
        integer, intent(out) :: batches                    !! batches the write_stream run hands over.
        integer(int64), intent(out) :: groups_arg          !! distinct key values in group mode.

        integer :: i, nargs, eq_pos, ios
        character(len=512) :: arg, key, val

        mode = ""
        size_gb = 0.25_real64
        file = "benchmark_table.parquet"
        ncols = 8
        touch = 2
        slices = 4
        nullfrac = 0.1_real64
        threads = 1
        nrows_arg = 20000000_int64
        select_frac = 0.01_real64
        scatter = 0
        chunk = 0
        bounded = 0
        batches = 32
        groups_arg = 1000_int64

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
            case ("--threads")
                read(val, *, iostat=ios) threads
                if (ios /= 0) then
                    write(error_unit, '(a)') "benchmark_table: --threads must be an integer"
                    error stop 1
                end if
            case ("--nrows")
                read(val, *, iostat=ios) nrows_arg
                if (ios /= 0) then
                    write(error_unit, '(a)') "benchmark_table: --nrows must be an integer"
                    error stop 1
                end if
            case ("--select")
                read(val, *, iostat=ios) select_frac
                if (ios /= 0) then
                    write(error_unit, '(a)') "benchmark_table: --select must be a number"
                    error stop 1
                end if
            case ("--scatter")
                read(val, *, iostat=ios) scatter
                if (ios /= 0) then
                    write(error_unit, '(a)') "benchmark_table: --scatter must be an integer"
                    error stop 1
                end if
            case ("--chunk")
                read(val, *, iostat=ios) chunk
                if (ios /= 0) then
                    write(error_unit, '(a)') "benchmark_table: --chunk must be an integer"
                    error stop 1
                end if
            case ("--bounded")
                read(val, *, iostat=ios) bounded
                if (ios /= 0) then
                    write(error_unit, '(a)') "benchmark_table: --bounded must be 0 or 1"
                    error stop 1
                end if
            case ("--batches")
                read(val, *, iostat=ios) batches
                if (ios /= 0) then
                    write(error_unit, '(a)') "benchmark_table: --batches must be an integer"
                    error stop 1
                end if
            case ("--groups")
                read(val, *, iostat=ios) groups_arg
                if (ios /= 0) then
                    write(error_unit, '(a)') "benchmark_table: --groups must be an integer"
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
        integer :: u, ios, cstat
        character(len=256) :: line, cmdfile
        real(real64) :: kb

        mib = -1.0_real64
        pid = getpid()
        write(cmdfile, '(a,i0,a)') "/tmp/pf_rss_", pid, ".txt"
        ! `ps` is the one portable-enough route across Linux and macOS; /proc/self/status does
        ! not exist on macOS, and neither does a Fortran intrinsic for this.
        ! cmdstat= is passed but never inspected: a nonzero `ps` exit would otherwise trigger
        ! ERROR TERMINATION under flang, which reads that as a cmdstat-worthy error condition.
        call execute_command_line("ps -o rss= -p " // itoa(pid) // " > " // trim(cmdfile), &
            wait=.true., cmdstat=cstat)
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

    subroutine write_fixture(file, size_gb, ncols, scatter, chunk)
        character(len=*), intent(in) :: file !! output path.
        real(real64), intent(in) :: size_gb  !! approximate uncompressed size.
        integer, intent(in) :: ncols         !! float64 columns to write.
        integer, intent(in) :: scatter       !! period of the scattered int32 key column; 0 = none.
        integer, intent(in) :: chunk         !! rows per row group; 0 leaves auto-sizing alone.
        type(parquet_writer) :: w
        real(real64), allocatable :: v(:)
        integer(int32), allocatable :: key(:)
        integer(int64) :: nrows
        integer :: c, i
        character(len=32) :: cname

        nrows = int(size_gb * 1.0e9_real64 / (8.0_real64 * real(ncols, real64)), int64)
        if (nrows < 1) nrows = 1
        write(output_unit, '(a,i0,a,i0,a)') "writing fixture: ", nrows, " rows x ", ncols, " float64 columns"
        allocate(v(nrows))
        ! Auto-sizing picks a row-group size from the target byte budget, which on a file of a few
        ! GB is a handful of very large row groups -- fine for every other mode, and useless for
        ! --mode=read_filtered, whose whole subject is per-row-group memory. An explicit --chunk is
        ! how that mode gets a file with enough row groups for "one row group at a time" to differ
        ! from "the whole column".
        if (chunk > 0) then
            write(output_unit, '(a,i0,a)') "  ... row groups of ", chunk, " rows (explicit chunk_size)"
            call parquet_open_writer(w, file, chunk_size=chunk)
        else
            call parquet_open_writer(w, file)
        end if
        ! The key column exists for --mode=read_filtered and nothing else, which is why it is
        ! off by default: adding a column would change every other mode's figures. Its values
        ! CYCLE rather than increase, so every row group holds the whole 0..scatter-1 range and
        ! the row-group statistics screen can prune nothing -- the scattered-survivor case a
        ! bounded read exists for. A monotone column (which is what every c<n> column here is)
        ! would let the screen prune almost everything and measure the opposite case.
        if (scatter > 0) then
            write(output_unit, '(a,i0)') "  ... plus an int32 'key' column cycling 0..", scatter - 1
            allocate(key(nrows))
            do i = 1, int(nrows)
                key(i) = int(mod(i - 1, scatter), int32)
            end do
            call parquet_write_column(w, "key", key)
            deallocate(key)
        end if
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
        write(output_unit, '(a)') "Expect this to come out SLOWER than --mode=read_table, and do"
        write(output_unit, '(a)') "not read that as table-layer overhead being negative: the table"
        write(output_unit, '(a)') "runs the same parquet_read_column per column and one release"
        write(output_unit, '(a)') "call more. The gap is that this path never releases, so every"
        write(output_unit, '(a)') "column needs fresh pages from the OS instead of reusing the"
        write(output_unit, '(a)') "ones Arrow already faulted in -- a whole file's worth of extra"
        write(output_unit, '(a)') "page faults. Calling parquet_release_column after each read"
        write(output_unit, '(a)') "closes it from this side too."
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
    !!
    !! The read is `%prefetch`, NOT a `%get` loop, and that is the whole reason this mode reads
    !! as it does. `%get` is a copy, not an accessor: it allocates a fresh array the size of the
    !! column and copies the values into it. A `%get` loop therefore measures the read PLUS a
    !! full extra allocation and copy per column, where `--mode=read_table`'s `%materialize_all`
    !! measures only the read -- so the two ratios are not comparable, and the touched columns
    !! come out roughly 1.8x what scaling `read_table` by `touch / ncols` predicts. That is not
    !! the library failing to scale; it is two different jobs. `%prefetch` is the same job.
    !!
    !! The copy is still worth knowing about, so it is timed on its own line afterwards, and the
    !! checksum -- another full pass over every touched column -- is taken outside both.
    subroutine bench_read_lazy(file, touch)
        character(len=*), intent(in) :: file !! fixture to read.
        integer, intent(in) :: touch         !! how many columns to actually read.
        type(parquet_table) :: t
        real(real64), allocatable :: g(:)
        real(real64), pointer :: p(:)
        real(real64) :: t0, t_open, t_touch, t_get, arrow_open, arrow_after, data_mib, checksum
        character(len=:), allocatable :: names(:)
        integer :: i, n

        t0 = now()
        call parquet_open_table(t, file)
        t_open = now() - t0
        arrow_open = real(parquet_get_arrow_bytes_allocated(), real64) / 1048576.0_real64
        call t%column_names(names)
        n = min(touch, t%ncols())

        t0 = now()
        call t%prefetch(names(1:n))
        t_touch = now() - t0
        arrow_after = real(parquet_get_arrow_bytes_allocated(), real64) / 1048576.0_real64

        ! Warm columns now, so this is the copy and nothing else.
        t0 = now()
        do i = 1, n
            call t%get(trim(names(i)), g)
        end do
        t_get = now() - t0

        ! Untimed: keeps every read live so none of it can be elided, without charging a full
        ! reduction over every touched column to either figure above.
        checksum = 0.0_real64
        do i = 1, n
            call t%col(trim(names(i)), p)
            checksum = checksum + sum(p)
        end do
        data_mib = real(t%nrows(), real64) * real(n, real64) * 8.0_real64 / 1048576.0_real64

        write(output_unit, '(a)') "--- read: lazy (open, then touch a few columns) ---"
        write(output_unit, '(a,i0,a,i0,a,i0)') "rows: ", t%nrows(), "   columns: ", t%ncols(), &
            "   touched: ", n
        write(output_unit, '(a,f10.3,a)') "open (schema only)          : ", t_open, " s"
        write(output_unit, '(a,f10.1,a)') "Arrow pool after open       : ", arrow_open, " MiB"
        write(output_unit, '(a,f10.3,a)') "prefetch the touched columns: ", t_touch, " s"
        write(output_unit, '(a,f10.1,a)') "Arrow pool after those reads: ", arrow_after, " MiB"
        write(output_unit, '(a,f10.3,a)') "%get copy-out of the same   : ", t_get, " s"
        write(output_unit, '(a,f10.1,a)') "touched column data         : ", data_mib, " MiB"
        write(output_unit, '(a,es12.4)')  "checksum (keeps reads live) : ", checksum
        write(output_unit, '(a)') "Compare the open figure with --mode=read_table's: opening now"
        write(output_unit, '(a)') "reads nothing at all, and only the touched columns are paid for."
        write(output_unit, '(a)') "The prefetch line is the one to scale against --mode=read_table's"
        write(output_unit, '(a)') "materialize_all by touched/columns -- both read, and do nothing"
        write(output_unit, '(a)') "else. The %get line is what asking for your OWN array costs on"
        write(output_unit, '(a)') "top: a fresh allocation the size of the column, plus the copy."
        write(output_unit, '(a)') "Use %col instead where the table's own storage will do."
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

    !> The FILTERED read: what `parquet_open_table(filter=)` plus `%materialize_all` costs in
    !! time and in Arrow memory, on a filter the row-group statistics screen cannot help with.
    !!
    !! **Needs a fixture written with `--scatter=<n>`** (see `write_fixture`), because the
    !! measurement is worthless without one: every other column this program writes is monotone
    !! in the row number, so a threshold on one of them is exactly the case the screen prunes
    !! almost entirely, and both filter engines are then cheap for a reason that has nothing to
    !! do with either. The scattered `key` column cycles `0..n-1`, so every row group's key range
    !! is the whole range, the screen can rule nothing out, and the survivors are spread over
    !! every row group -- the case a bounded read exists for. `pruned row groups` is printed for
    !! exactly this reason: a run reporting anything but 0 there did not measure that case.
    !!
    !! Two figures matter and they answer different questions. The TIME is the regression guard
    !! for the default engine. The ARROW POOL AFTER OPEN is the filter install's own footprint --
    !! on the default (caching) engine that is a full copy of the filter column's live rows, left
    !! decoded for a later read; a bounded open leaves nothing but the mask there.
    subroutine bench_read_filtered(file, select_frac, scatter, bounded)
        character(len=*), intent(in) :: file       !! fixture to read; must carry a `key` column.
        real(real64), intent(in) :: select_frac    !! fraction of rows the filter should keep.
        integer, intent(in) :: scatter             !! the key column's period, as written.
        logical, intent(in) :: bounded             !! open with bounded=.true.
        type(parquet_table) :: t
        type(parquet_filter) :: filt
        integer(int64), allocatable :: bounds(:,:)
        integer(int64) :: nrows_file, pruned
        real(real64) :: t0, t_open, t_mat, arrow_open, arrow_after, rss_after, checksum
        real(real64), allocatable :: g(:)
        integer :: threshold
        character(len=32) :: thr_s
        character(len=:), allocatable :: names(:)

        if (scatter <= 0) then
            write(error_unit, '(a)') "benchmark_table: --mode=read_filtered needs --scatter=<n>, " // &
                "matching the value the fixture was written with"
            error stop 1
        end if
        ! The threshold, not the fraction, is what the filter can express -- and it is rounded
        ! rather than truncated so that a fraction below one key step still keeps something.
        threshold = max(1, nint(select_frac * real(scatter, real64)))
        write(thr_s, '(i0)') threshold
        call parquet_table_row_group_bounds(file, bounds)
        nrows_file = bounds(2, size(bounds, 2))
        call filt%add("key < " // trim(thr_s))

        ! Timed separately, because they are two different costs: the open pays for the filter
        ! install (which engine matters here), and the materialize pays for the payload columns.
        t0 = now()
        if (bounded) then
            call parquet_open_table(t, file, filter=filt, bounded=.true.)
        else
            call parquet_open_table(t, file, filter=filt)
        end if
        t_open = now() - t0
        pruned = parquet_debug_get_row_groups_pruned()
        arrow_open = real(parquet_get_arrow_bytes_allocated(), real64) / 1048576.0_real64
        call t%column_names(names)
        t0 = now()
        call t%materialize_all()
        t_mat = now() - t0
        arrow_after = real(parquet_get_arrow_bytes_allocated(), real64) / 1048576.0_real64
        rss_after = rss_mib()
        ! The LAST column, never the first: the first is the int32 `key` the filter names, and
        ! %get into a real64 array is a kind mismatch rather than a conversion.
        call t%get(trim(names(size(names))), g)
        checksum = sum(g)

        write(output_unit, '(a)') "--- read: filtered table (open with filter= + materialize) ---"
        if (bounded) then
            write(output_unit, '(a)') "engine: BOUNDED (bounded=.true.: scoped install, chunked assembly)"
        else
            write(output_unit, '(a)') "engine: default (caching whole-file install, whole-column reads)"
        end if
        write(output_unit, '(a,i0,a,i0)') "file rows: ", nrows_file, "   row groups: ", size(bounds, 2)
        write(output_unit, '(a,a,a,i0)') "filter: key < ", trim(thr_s), "   of key period ", scatter
        write(output_unit, '(a,i0,a,f8.4)') "survivors: ", t%nrows(), "   selectivity: ", &
            real(t%nrows(), real64) / real(max(nrows_file, 1_int64), real64)
        write(output_unit, '(a,i0,a)') "pruned row groups: ", pruned, "   (0 = the screen could not help)"
        write(output_unit, '(a,f10.3,a)') "open (installs the filter)  : ", t_open, " s"
        write(output_unit, '(a,f10.3,a)') "materialize_all             : ", t_mat, " s"
        write(output_unit, '(a,f10.3,a)') "open + materialize          : ", t_open + t_mat, " s"
        write(output_unit, '(a,f10.1,a)') "Arrow pool after open       : ", arrow_open, " MiB"
        write(output_unit, '(a,f10.1,a)') "Arrow pool after materialize: ", arrow_after, " MiB"
        write(output_unit, '(a,f10.1,a)') "RSS                         : ", rss_after, " MiB"
        write(output_unit, '(a,es12.4)')  "checksum (keeps reads live) : ", checksum
        if (pruned > 0_int64) then
            write(output_unit, '(a)') "NOTE: the screen pruned row groups, so this run did NOT measure"
            write(output_unit, '(a)') "the scattered case. Check the fixture was written --scatter=<n>"
            write(output_unit, '(a)') "with the same n passed here."
        end if
        write(output_unit, '(a)') "The open figure is the filter install. On the default engine it"
        write(output_unit, '(a)') "reads every filter column over the live row groups in one batched"
        write(output_unit, '(a)') "pass; the materialize then reads each payload column whole and"
        write(output_unit, '(a)') "filters it. Both peak proportionally to the FILE, not to the"
        write(output_unit, '(a)') "survivor count above. Under bounded= the filter is evaluated one"
        write(output_unit, '(a)') "row group at a time and each column is assembled from per-row-"
        write(output_unit, '(a)') "group chunks, so both peaks are one row group's worth instead."
        write(output_unit, '(a)') ""
        write(output_unit, '(a)') "The Arrow figures here are what is RESIDENT when they are read,"
        write(output_unit, '(a)') "not the peak, and they are close to equal on the two engines:"
        write(output_unit, '(a)') "parquet_open_table releases every column it cached before it"
        write(output_unit, '(a)') "returns, and each engine's transient peak is inside a call. Read"
        write(output_unit, '(a)') "the peak with /usr/bin/time -l (macOS) or -v (Linux) around the"
        write(output_unit, '(a)') "whole run, one arm per process."
    end subroutine bench_read_filtered

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
        logical, allocatable :: mask(:)
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

    !> The `parquet_table_writer` sink against the hand-written buffer loop it replaces, and the
    !! always-present validity mask against a write that carries none. Three arms over the same
    !! prepared batches, one output file each:
    !!
    !!   1. **the sink** -- open it on the first batch, `%append` every batch, close.
    !!   2. **a hand-written buffer** -- `%clone_structure` a buffer, `%append` into it, and
    !!      `parquet_write_table_chunk` it whenever it reaches the threshold, emptying and
    !!      re-reserving afterwards. That is what the sink does internally, written out, so the
    !!      two should be at PARITY. A sink slower than this loop is the reset-and-reserve after
    !!      a flush having regressed (feature_risks.md Risk-227): without the reserve, every
    !!      append after a flush grows the buffer's columns again.
    !!   3. **the same loop with every column declared `protected_cols:`** -- identical Fortran,
    !!      one schema call different. The writer drops a protected column's all-`.true.` mask,
    !!      stores the field non-nullable and builds no null bitmap, so (2) minus (3) is what the
    !!      always-present mask costs. That is the figure doc/pages/tables/table-write.md sends
    !!      the reader here for.
    !!
    !! **Read the control before the timings.** Arms (2) and (3) run the same code over the same
    !! rows; had `%set_protected` not reached the writer they would measure one configuration
    !! twice and print a ratio of 1.00 -- which is also what "the mask costs nothing" looks like.
    !! So the mode reads each output's stored nullability back and prints all three: the sink's and
    !! arm (2)'s must be nullable and arm (3)'s must not, and no figure here means anything if they
    !! are not. Same shape, and for the same reason, as `--mode=read_one`'s use_threads control.
    !!
    !! The batches are opened and materialized before anything is timed, for the reason
    !! `--mode=write` materializes first: an append that had to decode a column from the file
    !! would charge that read to whichever arm ran first. The three arms alternate within each of
    !! three rounds and the best of the three is kept, so the cold first round -- the one that
    !! opens this process's first writer -- falls out rather than landing on arm 1. All three arms are given the SAME
    !! explicit `chunk_size`, a quarter of the rows unless `--chunk` says otherwise: the sink's
    !! own estimate for a narrow float64 table is larger than any fixture this program writes, so
    !! taking it would leave the buffer below the threshold from start to finish and measure a
    !! flush path that never ran.
    subroutine bench_write_stream(file, nbatch, chunk_arg)
        character(len=*), intent(in) :: file !! fixture to slice into batches.
        integer, intent(in) :: nbatch        !! batches to hand over, i.e. how many %append calls.
        integer, intent(in) :: chunk_arg     !! explicit rows per row group; 0 = a quarter of the rows.
        character(len=*), parameter :: f_sink = "benchmark_table_out1.parquet"
        character(len=*), parameter :: f_hand = "benchmark_table_out2.parquet"
        character(len=*), parameter :: f_prot = "benchmark_table_out3.parquet"
        type(parquet_table), allocatable :: batch(:)
        type(parquet_table) :: buf, bufp
        type(parquet_table_writer) :: out
        type(parquet_schema) :: s, sp
        type(parquet_writer) :: w
        character(len=:), allocatable :: names(:)
        integer(int64), allocatable :: bounds(:,:)
        integer(int64) :: nrows_file, per, lo, hi, n_sink, n_hand, n_prot, g_sink, g_hand, g_prot
        real(real64) :: t0, t_sink, t_hand, t_prot
        integer, parameter :: NROUNDS = 3
        integer :: b, i, chunk, rnd
        logical :: null_sink, null_hand, null_prot

        if (nbatch < 1) then
            write(error_unit, '(a)') "benchmark_table: --batches must be at least 1"
            error stop 1
        end if

        ! --- preparation, deliberately untimed --------------------------------------------------
        call parquet_table_row_group_bounds(file, bounds)
        nrows_file = bounds(2, size(bounds, 2))
        if (nrows_file < int(nbatch, int64)) then
            write(error_unit, '(a)') "benchmark_table: fewer rows than batches -- nothing to measure"
            error stop 1
        end if
        per = nrows_file / int(nbatch, int64)
        allocate(batch(nbatch))
        do b = 1, nbatch
            lo = 1_int64 + int(b - 1, int64)*per
            hi = lo + per - 1_int64
            if (b == nbatch) hi = nrows_file
            call parquet_open_table(batch(b), file, lo, hi)
            call batch(b)%materialize_all()
        end do
        call batch(1)%column_names(names)
        ! The threshold every arm uses, fixed here so all three chunk the same rows the same way.
        ! Without a --chunk the sink resolves its own estimate from the schema, which for a narrow
        ! float64 table is millions of rows -- more than a fixture of any reasonable size holds, so
        ! the buffer would never reach it and the flush path the parity arm exists to measure would
        ! never run at all. A quarter of the rows crosses it three times.
        if (chunk_arg > 0) then
            chunk = chunk_arg
        else
            chunk = int(min(max(1_int64, nrows_file/4_int64), int(huge(1), int64)))
        end if

        ! Three rounds, the arms alternating within each and the best of the three kept. The first
        ! round is the cold one -- this process has opened no writer yet, and the untimed
        ! materialization above has just left the allocator in a state arm 1 alone would be charged
        ! for -- and taking the minimum is what discards it.
        t_sink = huge(1.0_real64)
        t_hand = huge(1.0_real64)
        t_prot = huge(1.0_real64)
        do rnd = 1, NROUNDS
            ! --- 1: the sink --------------------------------------------------------------------
            t0 = now()
            call parquet_open_table_writer(out, f_sink, batch(1), chunk_size=chunk)
            do b = 1, nbatch
                call out%append(batch(b))
            end do
            call parquet_close_table_writer(out)
            t_sink = min(t_sink, now() - t0)

            ! --- 2: the same loop written out, carrying the always-present mask ------------------
            t0 = now()
            call parquet_derive_schema(batch(1), s)
            call parquet_open_writer_like(w, f_hand, batch(1), schema=s, chunk_size=chunk)
            call batch(1)%clone_structure(buf)
            call buf%reserve(int(chunk, int64))
            do b = 1, nbatch
                call buf%append(batch(b))
                if (buf%nrows() >= int(chunk, int64)) then
                    call parquet_write_table_chunk(w, buf)
                    call buf%truncate(0_int64)
                    call buf%reserve(int(chunk, int64))
                end if
            end do
            if (buf%nrows() > 0_int64) call parquet_write_table_chunk(w, buf)
            call parquet_close_writer(w)
            t_hand = min(t_hand, now() - t0)

            ! --- 3: the same again, every column protected, so no mask is passed -----------------
            t0 = now()
            call parquet_derive_schema(batch(1), sp)
            do i = 1, size(names)
                call sp%set_protected(trim(names(i)))
            end do
            call parquet_open_writer_like(w, f_prot, batch(1), schema=sp, chunk_size=chunk)
            call batch(1)%clone_structure(bufp)
            call bufp%reserve(int(chunk, int64))
            do b = 1, nbatch
                call bufp%append(batch(b))
                if (bufp%nrows() >= int(chunk, int64)) then
                    call parquet_write_table_chunk(w, bufp)
                    call bufp%truncate(0_int64)
                    call bufp%reserve(int(chunk, int64))
                end if
            end do
            if (bufp%nrows() > 0_int64) call parquet_write_table_chunk(w, bufp)
            call parquet_close_writer(w)
            t_prot = min(t_prot, now() - t0)
        end do

        ! --- what the three files actually hold ---------------------------------------------------
        call stream_file_facts(f_sink, trim(names(1)), n_sink, g_sink, null_sink)
        call stream_file_facts(f_hand, trim(names(1)), n_hand, g_hand, null_hand)
        call stream_file_facts(f_prot, trim(names(1)), n_prot, g_prot, null_prot)
        if (n_sink /= nrows_file .or. n_hand /= nrows_file .or. n_prot /= nrows_file) then
            write(error_unit, '(a)') "benchmark_table: the arms wrote different row counts -- one path is wrong"
            error stop 1
        end if

        write(output_unit, '(a)') "--- write: streamed, sink vs hand-written buffer ---"
        write(output_unit, '(a,i0,a,i0,a,i0,a,i0)') "rows: ", nrows_file, "   batches: ", nbatch, &
            "   chunk_size: ", chunk, "   best of ", NROUNDS
        if (chunk_arg <= 0) then
            write(output_unit, '(a)') "no --chunk given, so the threshold is a quarter of the rows and"
            write(output_unit, '(a)') "every arm crosses three flush boundaries. The sink's OWN estimate,"
            write(output_unit, '(a)') "which is what a caller passing no chunk_size= gets, is much larger"
            write(output_unit, '(a)') "for a table this narrow -- pass --chunk to measure at any other."
        end if
        write(output_unit, '(a,f10.3,a)') "parquet_table_writer        : ", t_sink, " s"
        write(output_unit, '(a,f10.3,a)') "hand-written buffer + chunk : ", t_hand, " s"
        write(output_unit, '(a,f10.3,a)') "the same, columns protected : ", t_prot, " s"
        if (t_hand > 0.0_real64) then
            write(output_unit, '(a,f10.2,a)') "sink / hand-written         : ", t_sink / t_hand, " x"
        end if
        if (t_prot > 0.0_real64) then
            write(output_unit, '(a,f10.2,a)') "masked / unmasked           : ", t_hand / t_prot, " x"
        end if
        write(output_unit, '(a,i0,a,i0,a,i0)') "row groups: sink ", g_sink, ", buffer ", g_hand, &
            ", protected ", g_prot
        write(output_unit, '(a,l1,a,l1,a,l1)') "CONTROL, stored nullability: sink ", null_sink, &
            ", masked ", null_hand, ", protected ", null_prot
        if (.not. null_sink .or. .not. null_hand .or. null_prot) then
            write(output_unit, '(a)') "CONTROL FAILED: the first two must be T and the third F. The"
            write(output_unit, '(a)') "protected arm wrote the same field nullability as the masked"
            write(output_unit, '(a)') "one, so the two measured one configuration twice and the mask"
            write(output_unit, '(a)') "ratio above means nothing -- or the sink stopped passing a mask"
            write(output_unit, '(a)') "at all, which is a defect, not a measurement. Fix it before"
            write(output_unit, '(a)') "reading any figure here."
        end if
        if (g_sink /= g_hand) then
            write(output_unit, '(a)') "NOTE: the sink and the buffer loop wrote different row-group"
            write(output_unit, '(a)') "counts, so they did not chunk the same rows the same way and"
            write(output_unit, '(a)') "the parity comparison above is not one. The loop here mirrors"
            write(output_unit, '(a)') "the sink's flush rule; one of the two has changed."
        end if
        write(output_unit, '(a)') "The first two lines should be at parity: the sink IS the second"
        write(output_unit, '(a)') "loop, with the bookkeeping inside. A slower sink means the empty"
        write(output_unit, '(a)') "and re-reserve after a flush regressed, and every append after"
        write(output_unit, '(a)') "the first row group is growing the buffer again."
        write(output_unit, '(a)') "The third line is the same write with no validity mask, which is"
        write(output_unit, '(a)') "what protected_cols: buys: a streamed table write passes a mask"
        write(output_unit, '(a)') "for every column it can, because a streamed column's nullability"
        write(output_unit, '(a)') "is fixed by its FIRST row group and a later Null could not then"
        write(output_unit, '(a)') "be written at all. Compare with --mode=write, whose hand-written"
        write(output_unit, '(a)') "loop passes no mask either."
    end subroutine bench_write_stream

    !> Reads one written file's row count, row-group count and one column's STORED nullability
    !! back -- the three facts `--mode=write_stream` compares its arms on. Its own reader, closed
    !! before it returns, so nothing it opens is alive during a timed arm.
    subroutine stream_file_facts(path, name, nrows, ngroups, nullable)
        character(len=*), intent(in) :: path   !! written file to inspect.
        character(len=*), intent(in) :: name   !! column whose stored nullability to report.
        integer(int64), intent(out) :: nrows   !! rows the file holds.
        integer(int64), intent(out) :: ngroups !! row groups the file holds.
        logical, intent(out) :: nullable       !! .true. when that field was stored nullable.
        type(parquet_reader) :: r

        call parquet_open_reader(r, path)
        call parquet_get_nrows(r, nrows)
        call parquet_get_num_row_groups(r, ngroups)
        call parquet_get_column_nullable(r, name, nullable)
        call parquet_close_reader(r)
    end subroutine stream_file_facts

    !> What `%sort_by` spends re-validating one permutation it produced itself:
    !! `parquet_column%reindex` validates its permutation unconditionally, and `%sort_by`
    !! calls it once per column, so an N-column table validates the same permutation N times.
    !!
    !! **The table is built in memory rather than read from a file**, which is the maintainer's own
    !! condition for this measurement -- every column is resident before anything is timed, so no
    !! timed region can absorb a lazy decode (CLAUDE.md's first benchmarking trap).
    !!
    !! **One string column is included deliberately.** A `PK_STRING` column is validated TWICE:
    !! once by `parquet_column%reindex` over the row permutation, and again by
    !! `parquet_string_column%reindex` over the expanded element permutation. A measurement over
    !! float columns alone would understate the real cost of a mixed table.
    !!
    !! The validation cost is measured by replicating the two candidate loops EXACTLY -- the
    !! `logical`-array one `reindex` runs today, and the bit-packed one `check_permutation`
    !! (`src/parquet_sorting_keys.f90`) already uses -- over the very permutation the sort just
    !! produced, in the same program and under the same flags. That separates the two possible
    !! savings: what bit-packing the seen-set alone would buy, and what removing the redundant
    !! calls would buy. Nothing in `src/` is modified to obtain it.
    !!
    !! The key column is re-scattered between rounds, outside every timed region, because a second
    !! sort of an already-sorted column would either return early or measure a reversal
    !! permutation -- a perfectly regular access pattern, unlike the scattered one a real sort
    !! walks.
    !> Times reading ONE whole column with Arrow's own `use_threads` on and off.
    !!
    !! **This measurement decides whether a milestone gets built at all**, and the decision rule runs
    !! the opposite way from the intuitive reading, so it is spelled out in the output rather than
    !! left to whoever runs it:
    !!
    !!   * `use_threads=.true.` clearly **faster** -> Arrow already parallelises a single
    !!     whole-column read -> there is nothing left for the library to win, and the idea of
    !!     splitting one column's read across row groups ourselves should be dropped.
    !!   * the two **within noise** -> Arrow does not parallelise it -> splitting it ourselves has
    !!     something to win.
    !!
    !! Three things about the method, each of which would otherwise produce a confident wrong answer:
    !!
    !!   * **A fresh reader per timed read.** A reader caches the decoded column, so reading the same
    !!     column twice on one reader times a cache hit the second time.
    !!   * **One untimed warm-up read first**, so the OS page cache holds the file for both arms.
    !!     Without it the first arm measured pays for the disk and the comparison is meaningless.
    !!   * **The arms alternate within each round** rather than running as two blocks, so a machine
    !!     that gets busier partway through disturbs both equally.
    !!
    !! The row-group count is reported because it is the ceiling on what the alternative could ever
    !! achieve: a single-row-group file has nothing to divide, and a result from one says nothing.
    subroutine bench_read_one(file)
        character(len=*), intent(in) :: file !! fixture to read.
        integer, parameter :: nround = 5     !! timed rounds; the best of each arm is kept.
        type(parquet_reader) :: rdr
        real(real64), allocatable :: v(:)
        integer(int64) :: nrows, ngroups
        real(real64) :: t0, dt, best_on, best_off
        integer :: r, saw_on, saw_off

        ! Warm-up, untimed: pulls the file into the OS page cache so neither arm pays for the disk.
        call parquet_open_reader(rdr, file)
        call parquet_get_nrows(rdr, nrows)
        call parquet_get_num_row_groups(rdr, ngroups)
        allocate(v(nrows))
        call parquet_read_column(rdr, "c1", v)
        call parquet_close_reader(rdr)
        write(output_unit, '(a,i0,a,i0,a)') "one column: ", nrows, " rows in ", ngroups, " row group(s)"
        if (ngroups < 2_int64) then
            write(output_unit, '(a)') "WARNING: a single row group -- neither Arrow nor a row-group split"
            write(output_unit, '(a)') "has anything to divide here, so this run cannot answer the question."
        end if

        best_on = huge(1.0_real64)
        best_off = huge(1.0_real64)
        saw_on = -1
        saw_off = -1
        do r = 1, nround
            call parquet_open_reader(rdr, file, use_threads=.true.)
            saw_on = parquet_debug_get_last_use_threads()
            t0 = now()
            call parquet_read_column(rdr, "c1", v)
            dt = now() - t0
            call parquet_close_reader(rdr)
            if (dt < best_on) best_on = dt

            call parquet_open_reader(rdr, file, use_threads=.false.)
            saw_off = parquet_debug_get_last_use_threads()
            t0 = now()
            call parquet_read_column(rdr, "c1", v)
            dt = now() - t0
            call parquet_close_reader(rdr)
            if (dt < best_off) best_off = dt
        end do

        ! Two controls. Without them "the two arms are equal" is indistinguishable from "both arms
        ! opened the same reader" and from "Arrow had one thread to work with" -- three different
        ! findings with identical output, and only one of them is an answer.
        write(output_unit, '(a)') ""
        write(output_unit, '(a,i0,a,i0)') "control 1: use_threads as the reader resolved it -- on arm: ", &
            saw_on, ", off arm: ", saw_off
        write(output_unit, '(a,i0)') "control 2: Arrow's CPU thread pool capacity              : ", &
            parquet_get_arrow_threads()
        if (saw_on == saw_off) then
            write(output_unit, '(a)') "FAILED CONTROL 1: both arms resolved to the same value, so the"
            write(output_unit, '(a)') "timings below compare one configuration against itself and say"
            write(output_unit, '(a)') "NOTHING about whether Arrow threads a single-column read."
        end if
        if (parquet_get_arrow_threads() < 2) then
            write(output_unit, '(a)') "FAILED CONTROL 2: Arrow has fewer than two threads available, so"
            write(output_unit, '(a)') "use_threads=.true. has nothing to thread WITH. That is a property"
            write(output_unit, '(a)') "of this machine's Arrow build, not an answer about Arrow's reader."
        end if
        write(output_unit, '(a)') ""
        write(output_unit, '(a,f10.4,a)') "read one column, use_threads=.true.  : ", best_on, " s"
        write(output_unit, '(a,f10.4,a)') "read one column, use_threads=.false. : ", best_off, " s"
        write(output_unit, '(a,f7.2,a)')  "  ratio (off/on)                     : ", best_off / best_on, "x"
        write(output_unit, '(a)') ""
        write(output_unit, '(a)') "Reading the result: a ratio near 1.00 means Arrow does NOT thread a"
        write(output_unit, '(a)') "single whole-column read, so splitting it across row groups ourselves"
        write(output_unit, '(a)') "has something to win. A ratio clearly above 1.00 means Arrow already"
        write(output_unit, '(a)') "does it, and there is nothing left to win."
        write(output_unit, '(a,es22.15)') "(sink, checksum ", sum(v)
    end subroutine bench_read_one

    !> Builds one table and sorts it exactly ONCE, so an external tool's peak-RSS figure describes
    !! `%sort_by` and nothing else.
    !!
    !! **This mode exists because `--mode=sort` cannot answer the peak-memory question, and its own
    !! figure looks as though it can.** That mode builds a *second*, standalone set of columns in
    !! order to time the reindex phase in isolation, so the process high-water mark is set by those
    !! rather than by the mutation — three separate machines reported an RSS figure from it and all
    !! three had to discard it (feature_table_parallel.md section 17.2). Nothing here allocates
    !! anything the sort does not need.
    !!
    !! **The answer is a DIFFERENCE between two runs, not this run's number.** Run it twice at the
    !! same `--size`/`--ncols`, once with `--threads=1` and once with `--threads=0` (automatic), each
    !! under `/usr/bin/time` (`-l` on macOS, `-v` on Linux). The builds are identical, so the whole
    !! difference in peak RSS is the mutation's transient. What is being tested is section 7.5's
    !! claim that a parallel mutation holds one transient column copy per thread and therefore at
    !! most doubles the table's peak: the predicted difference is `(T - 1)` copies of the largest
    !! column, which this mode prints so the two can be compared without arithmetic afterwards.
    !!
    !! In-process RSS is reported at two points as context. It is the CURRENT figure (`ps`), not the
    !! peak — the transient exists only while the mutation is running, and this program is inside
    !! that call when it happens — so it cannot answer the question on its own. That is precisely
    !! why the peak has to come from outside.
    subroutine bench_peakmem(size_gb, ncols, threads)
        real(real64), intent(in) :: size_gb !! approximate size of the float64 columns, in GB.
        integer, intent(in) :: ncols        !! float64 columns, the first of which is the sort key.
        integer, intent(in) :: threads      !! table-mutation thread cap; 1 serial, 0 automatic.
        integer, parameter :: slen = 16     !! width of the one character column.
        type(parquet_table) :: t
        type(parquet_string_column) :: sc
        real(real64), allocatable :: v(:)
        character(len=slen) :: sbuf
        real(real64), pointer :: kp(:)
        integer(int64) :: nrows, i, colbytes
        integer :: c, used
        real(real64) :: rss_built, rss_sorted, t0, dt, acc

        nrows = int(size_gb * 1.0e9_real64 / (8.0_real64 * real(max(ncols, 1), real64)), int64)
        if (nrows < 2_int64) nrows = 2_int64
        colbytes = nrows * 8_int64
        write(output_unit, '(a,i0,a,i0,a)') "in-memory table: ", nrows, " rows x ", ncols, &
            " float64 columns + 1 character column"

        ! Identical in shape to --mode=sort's table, so the two modes' figures describe the same
        ! object. One staging array, reused per column and released before the sort, so the build's
        ! own high-water mark stays as far below the mutation's as it can.
        allocate(v(nrows))
        call parquet_new_table(t)
        call scatter_key(v)
        call t%add_column("key", v)
        do c = 2, ncols
            do i = 1_int64, nrows
                v(i) = real(i, real64) * real(c, real64)
            end do
            call t%add_column("c"//itoa(c), v)
        end do
        deallocate(v)
        call sc%reserve(nrows, nrows * int(slen, int64))
        do i = 1_int64, nrows
            write(sbuf, '(i16.16)') i
            call sc%append_string(sbuf)
        end do
        call t%add_column("s", sc)
        call sc%clear()

        ! First-touch every column, so no page fault the build owes lands inside the measured sort
        ! and so the RSS reading below describes a fully resident table. This must SUM rather than
        ! merely take the pointer: `size(kp)` reads a descriptor and touches no data page at all,
        ! which leaves the whole table unfaulted and the reading meaningless.
        acc = 0.0_real64
        do c = 1, ncols
            if (c == 1) then
                call t%col("key", kp)
            else
                call t%col("c"//itoa(c), kp)
            end if
            acc = acc + sum(kp)
        end do

        rss_built = rss_mib()
        call parquet_set_table_threads(threads)
        t0 = now()
        call t%sort_by(["key"])
        dt = now() - t0
        used = int(parquet_debug_get_table_threads_used())
        rss_sorted = rss_mib()
        call parquet_reset_settings()

        write(output_unit, '(a)') ""
        write(output_unit, '(a,i0,a)') "--threads=", threads, "   (0 = automatic)"
        write(output_unit, '(a,i0)')   "threads the mutation actually used : ", used
        write(output_unit, '(a,f10.4,a)') "sort_by (one run)        : ", dt, " s"
        write(output_unit, '(a,f10.1,a)') "one column               : ", &
            real(colbytes, real64) / 1048576.0_real64, " MiB"
        write(output_unit, '(a,f10.1,a)') "predicted transient      : ", &
            real(max(used - 1, 0), real64) * real(colbytes, real64) / 1048576.0_real64, &
            " MiB   ((T-1) x one column)"
        write(output_unit, '(a,f10.1,a)') "current RSS after build  : ", rss_built, " MiB"
        write(output_unit, '(a,f10.1,a)') "current RSS after sort   : ", rss_sorted, " MiB"
        write(output_unit, '(a)') ""
        write(output_unit, '(a)') "The number that answers the question is the PEAK, which only an external"
        write(output_unit, '(a)') "tool can see -- the transient exists only while %sort_by is running. Run"
        write(output_unit, '(a)') "this mode twice, --threads=1 and --threads=0, under /usr/bin/time, and"
        write(output_unit, '(a)') "compare the two peaks against the predicted transient above."
        ! Keeps the first-touch pass from being optimised away -- without a use, -O3 is free to
        ! delete the very sum that makes the table resident.
        write(output_unit, '(a,es22.15)') "(sink, checksum ", acc
    end subroutine bench_peakmem

    subroutine bench_sort(size_gb, ncols)
        real(real64), intent(in) :: size_gb !! approximate size of the float64 columns, in GB.
        integer, intent(in) :: ncols        !! float64 columns, the first of which is the sort key.
        integer, parameter :: nround = 5    !! timed rounds; the best of them is kept.
        integer, parameter :: slen = 16     !! width of the one character column.
        type(parquet_table) :: t
        type(parquet_string_column) :: sc
        type(parquet_column), allocatable :: cols(:)
        type(parquet_column) :: scol
        real(real64), allocatable :: v(:)
        character(len=slen) :: sbuf
        real(real64), pointer :: kp(:)
        integer(int64), allocatable :: perm(:)
        integer(int64) :: nrows, i, sink
        integer :: c, round, nvalid
        real(real64) :: t0, t_sort, t_argsort, t_val_log, t_val_bit, dt, acc
        real(real64) :: total_log, total_bit, t_all, t_one

        nrows = int(size_gb * 1.0e9_real64 / (8.0_real64 * real(max(ncols, 1), real64)), int64)
        if (nrows < 2_int64) nrows = 2_int64
        write(output_unit, '(a,i0,a,i0,a)') "in-memory table: ", nrows, " rows x ", ncols, &
            " float64 columns + 1 character column"

        allocate(v(nrows))
        call parquet_new_table(t)
        call scatter_key(v)
        call t%add_column("key", v)
        do c = 2, ncols
            do i = 1_int64, nrows
                v(i) = real(i, real64) * real(c, real64)
            end do
            call t%add_column("c"//itoa(c), v)
        end do
        ! Built with %append_string into a parquet_string_column and handed over whole, NOT with
        ! %add_column over a character array. The latter goes through parquet_column%set_all, which
        ! calls parquet_string_column%set once per element, and every such set rewrites the offsets
        ! of all later elements -- so filling an empty string column that way is O(n^2) and does
        ! not finish at these row counts. %append_string grows geometrically and is linear.
        call sc%reserve(nrows, nrows * int(slen, int64))
        do i = 1_int64, nrows
            write(sbuf, '(i16.16)') i
            call sc%append_string(sbuf)
        end do
        call t%add_column("s", sc)
        call sc%clear()

        ! Every column was just written through %add_column, so its pages are already touched;
        ! this pass is cheap insurance against a first-touch cost landing inside a timed round.
        acc = 0.0_real64
        do c = 1, ncols
            if (c == 1) then
                call t%col("key", kp)
            else
                call t%col("c"//itoa(c), kp)
            end if
            acc = acc + sum(kp)
        end do

        ! The permutation %sort_by is about to build, obtained through public API so the replicas
        ! below walk exactly the memory-access pattern the real validation walks.
        call t%col("key", kp)
        t0 = now()
        call pf_argsort(kp, perm)
        t_argsort = now() - t0

        t_sort = huge(1.0_real64)
        do round = 1, nround
            ! Outside the timer: re-scatter the key, so every round sorts scattered data. The
            ! %col pointer is re-fetched each time because the previous round's sort reallocated
            ! every column's storage (feature_risks.md Risk-11).
            call t%col("key", kp)
            call scatter_key(kp)
            t0 = now()
            call t%sort_by(["key"])
            dt = now() - t0
            if (dt < t_sort) t_sort = dt
        end do

        ! The reindex phase itself, measured directly rather than derived by subtracting the
        ! permutation build from %sort_by: standalone columns of the same shape, reindexed the two
        ! ways %sort_by can do it. This is also the only observation that PROVES %reindex_trusted
        ! does something -- if it silently still validated, the two figures would coincide.
        t_all = huge(1.0_real64)
        t_one = huge(1.0_real64)
        do round = 1, nround
            call build_reindex_columns(cols, scol, nrows, ncols)
            t0 = now()
            do c = 1, ncols
                call cols(c)%reindex(perm)
            end do
            call scol%reindex(perm)
            dt = now() - t0
            if (dt < t_all) t_all = dt
            !
            call build_reindex_columns(cols, scol, nrows, ncols)
            t0 = now()
            call cols(1)%reindex(perm)
            do c = 2, ncols
                call cols(c)%reindex_trusted(perm)
            end do
            call scol%reindex_trusted(perm)
            dt = now() - t0
            if (dt < t_one) t_one = dt
        end do
        do c = 1, ncols
            call cols(c)%clear()
        end do
        call scol%clear()

        ! One validation of the permutation, each way, best of the same number of rounds.
        sink = 0_int64
        t_val_log = huge(1.0_real64)
        t_val_bit = huge(1.0_real64)
        do round = 1, nround
            t0 = now()
            call validate_logical(perm, sink)
            dt = now() - t0
            if (dt < t_val_log) t_val_log = dt
            t0 = now()
            call validate_bitpacked(perm, sink)
            dt = now() - t0
            if (dt < t_val_bit) t_val_bit = dt
        end do

        ! ncols + 2 validations happen per sort today: one per column from parquet_column%reindex
        ! (ncols float columns plus the character one), plus the string column's second,
        ! element-level one inside parquet_string_column%reindex.
        nvalid = ncols + 2
        total_log = real(nvalid, real64) * t_val_log
        total_bit = real(nvalid, real64) * t_val_bit

        write(output_unit, '(a)') ""
        write(output_unit, '(a,f9.4,a)') "sort_by (best of "//itoa(nround)//")     : ", t_sort, " s"
        write(output_unit, '(a,f9.4,a)') "  of which pf_argsort      : ", t_argsort, " s"
        write(output_unit, '(a)') ""
        write(output_unit, '(a,f9.4,a)') "reindex phase, all validate: ", t_all, " s"
        write(output_unit, '(a,f9.4,a)') "reindex phase, one validates: ", t_one, " s"
        write(output_unit, '(a,f6.2,a)') "  measured saving          : ", &
            100.0_real64 * (t_all - t_one) / t_all, "% of the reindex phase"
        write(output_unit, '(a)') ""
        write(output_unit, '(a)') "-- reference: the two seen-set representations, measured in isolation."
        write(output_unit, '(a)') "   `logical` is what reindex used before the bit-packed set replaced it,"
        write(output_unit, '(a)') "   so the (i)/(ii) lines below are what that change was worth, not a"
        write(output_unit, '(a)') "   further saving still available."
        write(output_unit, '(a,i0)')     "validations per sort_by    : ", nvalid
        write(output_unit, '(a,f9.4,a)') "one validation, logical    : ", t_val_log, " s"
        write(output_unit, '(a,f9.4,a)') "one validation, bit-packed : ", t_val_bit, " s"
        write(output_unit, '(a,f9.4,a,f6.2,a)') "all validations, logical   : ", total_log, &
            " s  = ", 100.0_real64 * total_log / t_sort, "% of sort_by"
        write(output_unit, '(a,f9.4,a,f6.2,a)') "all validations, bit-packed: ", total_bit, &
            " s  = ", 100.0_real64 * total_bit / t_sort, "% of sort_by"
        write(output_unit, '(a)') ""
        write(output_unit, '(a,f6.2,a)') "(i)  bit-pack only, keep all : saves ", &
            100.0_real64 * (total_log - total_bit) / t_sort, "% of sort_by"
        write(output_unit, '(a,f6.2,a)') "(ii) validate once, logical  : saves ", &
            100.0_real64 * (total_log - t_val_log) / t_sort, "% of sort_by"
        write(output_unit, '(a,f6.2,a)') "(ii) validate once, bit-pack : saves ", &
            100.0_real64 * (total_log - t_val_bit) / t_sort, "% of sort_by"
        write(output_unit, '(a)') ""
        write(output_unit, '(a,i0,a)') "scratch, logical  : ", 4_int64 * nrows / 1048576_int64, &
            " MiB live at a time (4 bytes per row: gfortran's default LOGICAL is 32 bits)"
        write(output_unit, '(a,i0,a)') "scratch, bit-packed: ", &
            max((nrows + 7_int64) / 8_int64 / 1048576_int64, 0_int64), " MiB live at a time"
        write(output_unit, '(a,i0,a,f0.1)') "(sink ", sink, ", checksum ", acc
    end subroutine bench_sort

    !> **How `pf_argsort` divides its time between sorting chunks and merging them**, at one thread
    !! count -- the measurement that decides whether a co-ranked parallel merge is worth building
    !! (`feature_sort_merge.md` step 0). Driven one data point at a time by
    !! `bench/benchmark_table.sh`'s argsort sweep.
    !!
    !! The engine sorts `T` contiguous chunks concurrently and then merges them **pairwise** in
    !! `log2(T)` rounds with `T/2, T/4, ..., 1` threads, so the last round merges two runs on ONE
    !! thread. That round is an O(n) pass that does not get faster as threads are added, and its
    !! share of wall time is the whole question here -- which is why the C++ side reports it
    !! separately from the earlier rounds (`parquet_debug_get_sort_phase_ns`).
    !!
    !! Three CLAUDE.md benchmarking rules apply and are all observed below: the key array and the
    !! result array are both warmed before anything is timed (a freshly allocated result pays
    !! first-touch page faults on its first pass and never again, which is enough to reverse a
    !! comparison); the best of several rounds is kept, because single rounds swing wider than the
    !! effect being measured; and the wrapper builds with `--profile release`, never with
    !! `FPM_FFLAGS`, which would replace the profile flags rather than add to them and measure -O0.
    !!
    !! `pf_argsort` does not modify its input, so unlike `bench_sort` there is nothing to re-scatter
    !! between rounds -- every round sorts the identical scattered array and does identical work.
    subroutine bench_argsort(nrows, threads)
        integer(int64), intent(in) :: nrows !! rows in the key array.
        integer, intent(in) :: threads      !! sort threads; 1 is the serial baseline.
        integer, parameter :: nround = 5    !! timed rounds; the best of them is kept.
        real(real64), allocatable :: v(:)
        integer(int64), allocatable :: perm(:)
        real(real64) :: pair_total, pair_chunk, pair_early, pair_final
        real(real64) :: cor_total, cor_chunk, cor_early, cor_final
        integer(int64) :: used, sink

        if (nrows < 2_int64) error stop "benchmark_table: --mode=argsort needs --nrows >= 2"
        allocate(v(nrows))
        call scatter_key(v)
        write(output_unit, '(a,i0,a,i0,a,i0,a)') "argsort: ", nrows, " rows x float64, threads=", &
            threads, " (best of ", nround, ")"

        ! Warm-up, untimed: touches every page of `v` a second time and, more importantly, allocates
        ! and first-touches `perm`, so no timed round pays its page faults. Without this the first
        ! round measured is systematically the slowest and the thread sweep reads as noise.
        call pf_argsort(v, perm, threads=threads)
        sink = perm(1) + perm(nrows)

        ! Both merges, in ONE process, back to back. Forcing the minimum segment size above the whole
        ! array makes every pair unsegmented, which IS the pairwise merge this feature replaced -- so
        ! the comparison below is a real measurement rather than a model, and it is immune to the
        ! machine drifting between two runs. It has to be: an earlier attempt compared a co-ranked
        ! build against a pairwise one measured on another day, and the serial baseline alone had
        ! moved 15% in between, which is larger than some of the effects being reported.
        call force_merge_segments(huge(1_int64) / 4_int64)
        call time_argsort(v, perm, threads, nround, pair_total, pair_chunk, pair_early, pair_final, used)
        call force_merge_segments(0_int64)
        call time_argsort(v, perm, threads, nround, cor_total, cor_chunk, cor_early, cor_final, used)

        write(output_unit, '(a)') ""
        if (used <= 1_int64) then
            write(output_unit, '(a,f9.4,a,f8.1,a)') "total                 : ", cor_total, " s   ", &
                cor_total * 1.0e9_real64 / real(nrows, real64), " ns/row"
            write(output_unit, '(a)') "(serial: the engine took the plain std::sort path, so there are no phases)"
        else
            write(output_unit, '(a)') "                          pairwise    co-ranked      change"
            call print_phase_pair("phase 1, chunk sorts ", pair_chunk, cor_chunk)
            call print_phase_pair("phase 2, early rounds", pair_early, cor_early)
            call print_phase_pair("phase 2, FINAL round ", pair_final, cor_final)
            call print_phase_pair("phase 2, all merging ", pair_early + pair_final, cor_early + cor_final)
            call print_phase_pair("whole argsort        ", pair_total, cor_total)
            write(output_unit, '(a,i0,a,f8.1,a,f8.1,a)') "threads used (phase 1): ", used, &
                "        ", pair_total * 1.0e9_real64 / real(nrows, real64), " ns/row  ", &
                cor_total * 1.0e9_real64 / real(nrows, real64), " ns/row"
        end if
        write(output_unit, '(a,i0,a,i0,a,f0.6,a,f0.6,a,f0.6,a,f0.6,a,f0.6,a,i0)') &
            "RESULT mode=argsort threads=", threads, " nrows=", nrows, &
            " elapsed_s=", cor_total, " chunk_s=", cor_chunk, " earlymerge_s=", cor_early, &
            " finalmerge_s=", cor_final, " pairwise_s=", pair_total, " threads_used=", used
        write(output_unit, '(a,i0,a)') "(sink ", sink, ")"
    end subroutine bench_argsort

    !> `nround` timed `pf_argsort`s, keeping the best total and that round's own phase breakdown.
    !!
    !! The breakdown comes from the round that produced the best total rather than being averaged:
    !! the fastest round is the one least disturbed by everything else on the machine, and mixing its
    !! total with another round's phases would not add up.
    subroutine time_argsort(v, perm, threads, nround, total, chunk, early, final, used)
        real(real64), intent(in) :: v(:)                    !! the key array.
        integer(int64), allocatable, intent(inout) :: perm(:) !! reused result array.
        integer, intent(in) :: threads                      !! sort threads.
        integer, intent(in) :: nround                        !! timed rounds; the best is kept.
        real(real64), intent(out) :: total                  !! best wall time, seconds.
        real(real64), intent(out) :: chunk                  !! phase 1 in that round, seconds.
        real(real64), intent(out) :: early                  !! merge rounds but the last, seconds.
        real(real64), intent(out) :: final                  !! the last merge round, seconds.
        integer(int64), intent(out) :: used                 !! threads phase 1 put to work.
        !> Where the last threaded sort spent its time. Declared locally rather than in
        !! src/parquet_bindings.f90 because it is a maintainer diagnostic, not public API -- the
        !! same convention every parquet_debug_* hook follows.
        interface
            function parquet_debug_get_sort_phase_ns(phase) &
                    bind(C, name="parquet_debug_get_sort_phase_ns") result(ns)
                import :: c_int, c_int64_t
                integer(c_int), value :: phase !! 0 = chunk sorts, 1 = merge rounds but the last, 2 = the last round.
                integer(c_int64_t) :: ns       !! nanoseconds in that phase, or 0 if it did not run.
            end function parquet_debug_get_sort_phase_ns
            function parquet_debug_get_sort_threads_used() &
                    bind(C, name="parquet_debug_get_sort_threads_used") result(n)
                import :: c_int64_t
                integer(c_int64_t) :: n !! threads the last threaded build put to work, caller included.
            end function parquet_debug_get_sort_threads_used
        end interface
        integer :: round
        real(real64) :: t0, dt

        total = huge(1.0_real64)
        chunk = 0.0_real64
        early = 0.0_real64
        final = 0.0_real64
        used = 1_int64
        do round = 1, nround
            t0 = now()
            call pf_argsort(v, perm, threads=threads)
            dt = now() - t0
            if (dt < total) then
                total = dt
                chunk = real(parquet_debug_get_sort_phase_ns(0_c_int), real64) * 1.0e-9_real64
                early = real(parquet_debug_get_sort_phase_ns(1_c_int), real64) * 1.0e-9_real64
                final = real(parquet_debug_get_sort_phase_ns(2_c_int), real64) * 1.0e-9_real64
                used = int(parquet_debug_get_sort_threads_used(), int64)
            end if
        end do
    end subroutine time_argsort

    !> Forces the smallest output range the co-ranked merge will give a thread of its own. A value
    !! larger than the whole array leaves every pair unsegmented, which is exactly the pairwise merge
    !! that preceded co-ranking -- so this is how one process measures both. 0 restores the real floor.
    subroutine force_merge_segments(min_segment)
        integer(int64), intent(in) :: min_segment !! floor in elements; 0 restores the built-in one.
        interface
            subroutine set_min_seg(n) bind(C, name="parquet_debug_set_sort_merge_min_segment")
                import :: c_int64_t
                integer(c_int64_t), value :: n !! elements; <= 0 restores the real floor.
            end subroutine set_min_seg
        end interface
        call set_min_seg(int(min_segment, c_int64_t))
    end subroutine force_merge_segments

    !> One `phase: pairwise co-ranked change` row, so the five of them cannot drift apart in
    !! formatting. A phase neither version spends time in prints a dash rather than a meaningless
    !! ratio.
    subroutine print_phase_pair(label, pairwise, coranked)
        character(len=*), intent(in) :: label  !! what the row is measuring.
        real(real64), intent(in) :: pairwise   !! seconds with every pair unsegmented.
        real(real64), intent(in) :: coranked   !! seconds with co-ranking in force.

        if (pairwise <= 0.0_real64 .or. coranked <= 0.0_real64) then
            write(output_unit, '(a,f9.4,a,f9.4,a)') label//": ", pairwise, " s ", coranked, " s        --"
        else
            write(output_unit, '(a,f9.4,a,f9.4,a,f7.2,a)') label//": ", pairwise, " s ", coranked, &
                " s   ", pairwise / coranked, "x"
        end if
    end subroutine print_phase_pair

    !> Builds `ncols` float64 columns plus one string column, each `nrows` rows, for the reindex
    !! measurement. Rebuilt before every timed round because `reindex` consumes its input ordering.
    subroutine build_reindex_columns(cols, scol, nrows, ncols)
        type(parquet_column), allocatable, intent(inout) :: cols(:) !! the float columns.
        type(parquet_column), intent(inout) :: scol                 !! the string column.
        integer(int64), intent(in) :: nrows                         !! rows per column.
        integer, intent(in) :: ncols                                !! float columns to build.
        real(real64), allocatable :: v(:)
        character(len=16), allocatable :: s(:)
        integer(int64) :: i
        integer :: c
        !
        if (allocated(cols)) then
            do c = 1, size(cols)
                call cols(c)%clear()
            end do
            deallocate(cols)
        end if
        call scol%clear()
        allocate(cols(ncols))
        allocate(v(nrows))
        do i = 1_int64, nrows
            v(i) = real(i, real64)
        end do
        do c = 1, ncols
            call cols(c)%init(PK_FLOAT64, nrows)
            call cols(c)%set_all(v)
        end do
        allocate(s(nrows))
        do i = 1_int64, nrows
            write(s(i), '(i16.16)') i
        end do
        call scol%init(PK_STRING, nrows)
        call scol%set_all(s)
    end subroutine build_reindex_columns

    !> Fills `v` with a scattered, deterministic key: distinct enough that the permutation is
    !! irregular, which is what the validation's random access into its seen-set actually costs.
    subroutine scatter_key(v)
        real(real64), intent(out) :: v(:) !! the key column's values.
        integer(int64) :: i
        do i = 1_int64, size(v, kind=int64)
            v(i) = real(mod(i * 2654435761_int64, 1000000007_int64), real64)
        end do
    end subroutine scatter_key

    !> An exact replica of the range/duplicate check `reindex` runs today
    !! (`src/parquet_columns_structural.f90`), including its `logical` scratch array and its
    !! allocate/deallocate. `sink` consumes a result so the loop cannot be optimized away.
    subroutine validate_logical(perm, sink)
        integer(int64), intent(in) :: perm(:)      !! the permutation to check.
        integer(int64), intent(inout) :: sink      !! kept-live accumulator.
        integer(int64) :: n, k, p
        logical, allocatable :: seen(:)
        n = size(perm, kind=int64)
        allocate(seen(n))
        seen = .false.
        do k = 1_int64, n
            p = perm(k)
            if (p < 1_int64 .or. p > n) error stop "validate_logical: permutation entry out of range"
            if (seen(p)) error stop "validate_logical: permutation contains a duplicate index"
            seen(p) = .true.
        end do
        if (seen(n)) sink = sink + 1_int64
        deallocate(seen)
    end subroutine validate_logical

    !> The same check with the bit-packed seen-set `check_permutation`
    !! (`src/parquet_sorting_keys.f90`) already uses -- 1 bit per row instead of 32.
    subroutine validate_bitpacked(perm, sink)
        integer(int64), intent(in) :: perm(:) !! the permutation to check.
        integer(int64), intent(inout) :: sink !! kept-live accumulator.
        integer(int64) :: n, k, val, word
        integer(int8), allocatable :: seen(:)
        n = size(perm, kind=int64)
        allocate(seen((n + 7_int64) / 8_int64))
        seen = 0_int8
        do k = 1_int64, n
            val = perm(k)
            if (val < 1_int64 .or. val > n) error stop "validate_bitpacked: entry out of range"
            word = (val - 1_int64) / 8_int64 + 1_int64
            if (btest(seen(word), int(mod(val - 1_int64, 8_int64)))) then
                error stop "validate_bitpacked: permutation contains a duplicate index"
            end if
            seen(word) = ibset(seen(word), int(mod(val - 1_int64, 8_int64)))
        end do
        if (seen(1) /= 0_int8) sink = sink + 1_int64
        deallocate(seen)
    end subroutine validate_bitpacked

    !> `%group_by` and the grouping's verbs, each against the composition it replaces. File-free,
    !! like the sort and argsort modes and for the same reason: what is measured is the cost of
    !! partitioning and walking an already-resident table, and reading a fixture first would add
    !! one decode to every arm.
    !!
    !! Four questions, in this order:
    !!
    !! 1. **What the OBJECT costs above the partition.** `%group_by` against a bare
    !!    `%argsort_by(keys, perm, group_offsets=)` -- the same sort, without the object -- so the
    !!    difference is the `dropna` pass and the two array copies, and nothing else. The
    !!    `dropna=.false.` arm is the same call with that pass skipped.
    !! 2. **What `%agg` costs against the compositions it replaces**, all three computing the same
    !!    per-group mean: `%agg("mean")`; a `%gather` into one caller-owned buffer plus `pf_mean`;
    !!    and `%get_slice(parquet_slice_list(rows))` plus `pf_mean`, which allocates twice per
    !!    group. Run this mode at several `--groups` values: the per-group overheads the third arm
    !!    carries are invisible at ten groups and dominate at a hundred thousand.
    !! 3. **What the `%apply` loop costs on a team.** A trivial module-procedure callback -- one
    !!    pass over the group's rows -- run with `threads=` absent (SERIAL by contract) and then at
    !!    1, 2, 4 and 8. The callback is trivial on purpose: a heavier one would flatter every
    !!    rung. Each rung prints the team the library actually resolved to, read back from the
    !!    debug hook, because "the ladder does not scale" and "no team ever opened" print the same
    !!    times otherwise; a rung whose recorded team is below the request was clamped to this
    !!    process's CPU affinity.
    !! 4. **What `%broadcast` costs per row**, against the `%group_ids` lookup a caller writes
    !!    instead -- one gather through the codes, with the dropped rows filled.
    !!
    !! The answers are checksummed against each other where they should agree exactly: the three
    !! mean arms and the two broadcast arms are printed as sums, and a mismatch there means the
    !! run measured different work rather than the same work two ways.
    subroutine bench_group(nrows, ngroups)
        integer(int64), intent(in) :: nrows   !! rows in the in-memory table.
        integer(int64), intent(in) :: ngroups !! distinct values of the key column.
        integer, parameter :: nround = 5      !! timed rounds; the best of them is kept.
        integer, parameter :: nladder = 4     !! rungs of the %apply thread ladder.
        integer, parameter :: ladder(nladder) = [1, 2, 4, 8] !! the rungs themselves.
        type(parquet_table) :: t
        type(parquet_grouping) :: grp
        integer(int32), allocatable :: k(:)
        real(real64), allocatable :: v(:), means(:), buf(:), slice(:), per_row(:), by_codes(:)
        real(real64), pointer :: vp(:)
        integer(int64), allocatable :: perm(:), go(:), rows(:), counts(:), codes(:)
        integer(int64) :: i, g, n, used
        integer :: round, r
        real(real64) :: t0, dt, t_grp, t_grp_keep, t_arg, t_agg, t_gather, t_slice, t_bcast, t_codes
        real(real64) :: s_agg, s_gather, s_slice, s_bcast, s_codes, t_apply_serial
        real(real64) :: t_apply(nladder)
        integer(int64) :: used_serial, used_rung(nladder)

        if (nrows < 2_int64) error stop "benchmark_table: --mode=group needs --nrows >= 2"
        if (ngroups < 1_int64) error stop "benchmark_table: --mode=group needs --groups >= 1"
        write(output_unit, '(a,i0,a,i0,a)') "in-memory table: ", nrows, " rows, ", ngroups, &
            " groups (int32 key + float64 payload)"

        ! The key spreads its groups over the rows by a multiplicative walk, as the sort mode's
        ! own fixture does: what these arms need is that a group's rows are scattered through the
        ! table rather than contiguous, not that the sequence is a shuffle.
        allocate(k(nrows), v(nrows))
        do i = 1_int64, nrows
            k(i) = int(mod(i * 2654435761_int64, ngroups), int32)
            v(i) = real(mod(i * 31_int64, 997_int64), real64) + real(i, real64) * 1.0e-6_real64
        end do
        call parquet_new_table(t)
        call t%add_column("k", k)
        call t%add_column("v", v)
        deallocate(k, v)

        ! ---- 1. the partition, and what the object adds to it.
        t_grp = huge(1.0_real64)
        t_grp_keep = huge(1.0_real64)
        t_arg = huge(1.0_real64)
        do round = 1, nround
            t0 = now()
            call t%group_by(["k"], grp)
            dt = now() - t0
            if (dt < t_grp) t_grp = dt
            t0 = now()
            call t%group_by(["k"], grp, dropna=.false.)
            dt = now() - t0
            if (dt < t_grp_keep) t_grp_keep = dt
            t0 = now()
            call t%argsort_by(["k"], perm, group_offsets=go)
            dt = now() - t0
            if (dt < t_arg) t_arg = dt
        end do
        call t%group_by(["k"], grp)
        write(output_unit, '(a)') ""
        write(output_unit, '(a,i0,a,i0,a)') "groups built: ", grp%ngroups(), " (largest ", grp%max_size(), " rows)"
        write(output_unit, '(a,f9.4,a)') "argsort_by(group_offsets=)  : ", t_arg, " s   the partition, no object"
        write(output_unit, '(a,f9.4,a)') "group_by (dropna=.false.)   : ", t_grp_keep, " s   + the object"
        write(output_unit, '(a,f9.4,a)') "group_by (dropna=.true.)    : ", t_grp, " s   + the dropna pass"

        ! ---- 2. one statistic per group, three ways. The buffers of the two composition arms are
        ! allocated and written before the timers start, as a reference must be.
        allocate(buf(grp%max_size()))
        buf = 0.0_real64
        allocate(means(grp%ngroups()))
        means = 0.0_real64
        t_agg = huge(1.0_real64)
        t_gather = huge(1.0_real64)
        t_slice = huge(1.0_real64)
        do round = 1, nround
            t0 = now()
            call grp%agg("v", "mean", means, threads=1)
            dt = now() - t0
            if (dt < t_agg) t_agg = dt
            if (round == 1) s_agg = sum(means)
            t0 = now()
            do g = 1_int64, grp%ngroups()
                call grp%gather("v", g, buf, n)
                call pf_mean(buf(1:n), means(g))
            end do
            dt = now() - t0
            if (dt < t_gather) t_gather = dt
            if (round == 1) s_gather = sum(means)
            t0 = now()
            do g = 1_int64, grp%ngroups()
                call grp%rows(g, rows)
                call t%get_slice("v", parquet_slice_list(rows), slice)
                call pf_mean(slice, means(g))
            end do
            dt = now() - t0
            if (dt < t_slice) t_slice = dt
            if (round == 1) s_slice = sum(means)
        end do
        write(output_unit, '(a)') ""
        write(output_unit, '(a,f9.4,a)') "agg(""mean""), threads=1      : ", t_agg, " s   one pass, no allocation"
        write(output_unit, '(a,f9.4,a,f6.2,a)') "gather + pf_mean            : ", t_gather, " s   = ", &
            t_gather / t_agg, "x agg"
        write(output_unit, '(a,f9.4,a,f6.2,a)') "rows + get_slice + pf_mean  : ", t_slice, " s   = ", &
            t_slice / t_agg, "x agg  (2 allocations per group)"

        ! ---- 3. the %apply loop, serial and on a ladder. The payload the callback reads is a
        ! pointer into the table's own store, taken once: nothing in this loop moves it.
        call t%col("v", vp)
        bench_group_payload => vp
        t_apply_serial = huge(1.0_real64)
        do round = 1, nround
            t0 = now()
            call grp%apply(bench_group_sum, means)
            dt = now() - t0
            if (dt < t_apply_serial) t_apply_serial = dt
        end do
        used_serial = parquet_debug_get_group_threads_used()
        do r = 1, nladder
            t_apply(r) = huge(1.0_real64)
            do round = 1, nround
                t0 = now()
                call grp%apply(bench_group_sum, means, threads=ladder(r))
                dt = now() - t0
                if (dt < t_apply(r)) t_apply(r) = dt
            end do
            used_rung(r) = parquet_debug_get_group_threads_used()
        end do
        write(output_unit, '(a)') ""
        write(output_unit, '(a,f9.4,a,i0,a)') "apply, threads absent       : ", t_apply_serial, &
            " s   team recorded ", used_serial, " (the serial default; 1 or the run means nothing)"
        do r = 1, nladder
            write(output_unit, '(a,i2,a,f9.4,a,f6.2,a,i0)') "apply, threads=", ladder(r), "           : ", &
                t_apply(r), " s   = ", t_apply_serial / t_apply(r), "x serial, team recorded ", used_rung(r)
        end do
        bench_group_payload => null()

        ! ---- 4. the per-group answer back onto the rows, against the lookup it replaces.
        call grp%size(counts)
        allocate(per_row(t%nrows()), by_codes(t%nrows()))
        per_row = 0.0_real64
        by_codes = 0.0_real64
        call grp%agg("v", "mean", means, threads=1)
        t_bcast = huge(1.0_real64)
        t_codes = huge(1.0_real64)
        do round = 1, nround
            t0 = now()
            call grp%broadcast(means, per_row, fill=0.0_real64, threads=1)
            dt = now() - t0
            if (dt < t_bcast) t_bcast = dt
            t0 = now()
            call grp%group_ids(codes)
            do i = 1_int64, t%nrows()
                if (codes(i) > 0_int64) then
                    by_codes(i) = means(codes(i))
                else
                    by_codes(i) = 0.0_real64
                end if
            end do
            dt = now() - t0
            if (dt < t_codes) t_codes = dt
        end do
        s_bcast = sum(per_row)
        s_codes = sum(by_codes)
        write(output_unit, '(a)') ""
        write(output_unit, '(a,f9.4,a,f7.2,a)') "broadcast, threads=1        : ", t_bcast, " s   = ", &
            1.0e9_real64 * t_bcast / real(t%nrows(), real64), " ns/row"
        write(output_unit, '(a,f9.4,a,f7.2,a)') "group_ids + lookup          : ", t_codes, " s   = ", &
            1.0e9_real64 * t_codes / real(t%nrows(), real64), " ns/row"

        ! ---- the checksums. Arms that computed the same thing must agree exactly; a run whose
        ! sums differ measured different work and its ratios say nothing.
        write(output_unit, '(a)') ""
        write(output_unit, '(a,3(1x,es22.15))') "mean checksums (agg, gather, get_slice):", s_agg, s_gather, s_slice
        write(output_unit, '(a,2(1x,es22.15))') "broadcast checksums (broadcast, codes) :", s_bcast, s_codes
        if (s_agg /= s_gather .or. s_agg /= s_slice) then
            write(output_unit, '(a)') "  *** the three mean arms disagree: they did not compute the same thing"
        end if
        if (s_bcast /= s_codes) then
            write(output_unit, '(a)') "  *** the two broadcast arms disagree: they did not compute the same thing"
        end if
        used = grp%nrows()
        write(output_unit, '(a,i0,a,i0,a)') "(grouped rows ", used, ", counts sum ", sum(counts), ")"
    end subroutine bench_group

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
