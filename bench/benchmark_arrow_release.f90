!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> Asserts that every `parquet_table` materialization path releases its Arrow-side buffers.
!!
!! `feature_risks.md` Risk-1: if a materialization path forgets to release the Arrow column after
!! copying it into the table, **nothing fails**. The values are right, every test passes, and the
!! table simply holds two copies of every column it reads. Only the Arrow pool counter notices.
!!
!! **This is a check, not a benchmark** -- it exits nonzero when a path retains more than its
!! tolerance, so it can be run as a regression gate. It lives under `app/` rather than `test/` for
!! two reasons, both of which the measurement depends on:
!!
!!   * **Each path must run in its OWN process.** A baseline and the path under test measured in one
!!     process report the high-water mark of the pair, which makes whichever ran second look like it
!!     retained memory it had already released. `bench/benchmark_arrow_release.sh` drives one process
!!     per mode for exactly this reason.
!!   * **RSS cannot answer this question at all.** Arrow's pool keeps freed pages instead of
!!     returning them to the OS (and so does glibc/macOS malloc for ordinary allocations), so a
!!     correct release and a complete failure to release look nearly identical in `ps`. See
!!     CLAUDE.md, "Measuring whether Arrow memory was actually freed".
!!
!! What each mode asserts: after the work is done and while the table is **still alive**, the Arrow
!! pool holds no more than `--tolerance` of one copy of the column data it just read. A table that
!! has materialized a column holds that column as a plain Fortran array; the Arrow buffer it was
!! decoded from must be gone. The tolerance exists because Arrow retains a little schema/metadata
!! state per open reader, not to leave room for a retained column.
!!
!! **`materialize_all`/`prefetch` must be run on one thread as well as on many**, which is why the
!! wrapper runs each of them twice. The internally-parallel `%prefetch` gives each thread its own
!! reader and closes it at the end of the region, and closing a reader frees whatever it had cached
!! whether or not the release call ran -- so the parallel path passes even with every
!! `parquet_release_column` call deleted (verified by deleting them). `OMP_NUM_THREADS=1` fails
!! `parallel_prefetch_ok`'s first clause and forces the serial batch-release path, where the release
!! is the only thing that can free the column.
!!
!! Maintainer tool, never run by `fpm test` (CLAUDE.md: anything needing this much memory/time lives
!! under `app/` with a shell wrapper under `tools/`). Drive it with `bench/benchmark_arrow_release.sh`.
program benchmark_arrow_release
    use parquet
    use parquet_tables
    use iso_fortran_env, only : int64, real64, error_unit, output_unit
    use iso_c_binding, only : c_int64_t
    implicit none

    !> Bytes currently held by Arrow's process-wide memory pool. Declared locally rather than in
    !! `src/parquet_bindings.f90` because it is a maintainer diagnostic, not public API -- the same
    !! convention the debug-only hooks in `test/error_scenarios.f90` follow, and the same interface
    !! `bench/benchmark_table.f90` declares for its own reporting.
    interface
        function parquet_get_arrow_bytes_allocated() &
                bind(C, name="parquet_get_arrow_bytes_allocated") result(bytes)
            import :: c_int64_t
            integer(c_int64_t) :: bytes !! bytes currently allocated from Arrow's default pool.
        end function parquet_get_arrow_bytes_allocated
    end interface

    character(len=:), allocatable :: mode, file
    real(real64) :: size_gb, tolerance
    integer :: ncols

    call parse_arguments(mode, size_gb, file, ncols, tolerance)

    select case (mode)
    case ("write_fixture")
        call write_fixture(file, size_gb, ncols)
    case ("materialize_all")
        call check_materialize_all(file, tolerance)
    case ("prefetch")
        call check_prefetch(file, tolerance)
    case ("get")
        call check_get(file, tolerance)
    case ("slice")
        call check_slice(file, tolerance)
    case ("write_release")
        call check_write_release(file, tolerance)
    case ("control")
        call check_control(file, tolerance)
    case default
        write(error_unit, '(a)') "check_arrow_release: unknown --mode='" // mode // "'"
        call print_usage()
        error stop 1
    end select

contains

    !> One float64 fixture, several columns, several row groups.
    !!
    !! Several row groups on purpose: the batch-release policy releases a top-level column once
    !! every leaf under it has been copied, and a single-row-group file would not exercise the
    !! chunked materialization path at all.
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
        if (nrows < 1000) nrows = 1000
        allocate(v(nrows))
        ! A row-group size well under the row count, so the file has many of them.
        call parquet_open_writer(w, file, chunk_size=int(max(nrows / 8_int64, 1_int64)))
        do c = 1, ncols
            do i = 1, int(nrows)
                v(i) = real(i, real64) * real(c, real64)
            end do
            write(cname, '(a,i0)') "c", c
            call parquet_write_column(w, trim(cname), v)
        end do
        call parquet_close_writer(w)
        write(output_unit, '(a,i0,a,i0,a)') "fixture: ", nrows, " rows x ", ncols, " float64 columns"
        write(output_unit, '(a)') "fixture written: " // file
    end subroutine write_fixture

    !> Reports one path's verdict and stops the process with its exit status.
    !!
    !! `data_bytes` is one copy of the column data the path was asked to read, which is what a
    !! failure to release would leave behind -- so the retained figure is reported as a fraction of
    !! it rather than in isolation, where no number is obviously wrong.
    subroutine verdict(label, before, after, data_bytes, tolerance)
        character(len=*), intent(in) :: label       !! the path measured.
        integer(c_int64_t), intent(in) :: before    !! pool bytes before the work.
        integer(c_int64_t), intent(in) :: after     !! pool bytes after it, table still alive.
        integer(int64), intent(in) :: data_bytes    !! one copy of the data read.
        real(real64), intent(in) :: tolerance       !! allowed retention, as a fraction of it.
        real(real64) :: retained, limit, frac

        retained = real(after - before, real64)
        limit = tolerance * real(data_bytes, real64)
        frac = 0.0_real64
        if (data_bytes > 0_int64) frac = retained / real(data_bytes, real64)
        write(output_unit, '(a)') "--- " // label
        write(output_unit, '(a,f12.3,a)') "  data read        : ", &
            real(data_bytes, real64) / 1048576.0_real64, " MiB"
        write(output_unit, '(a,f12.3,a,f8.4,a)') "  arrow retained   : ", &
            retained / 1048576.0_real64, " MiB  (", frac, " of one copy)"
        write(output_unit, '(a,f12.3,a)') "  tolerance        : ", &
            limit / 1048576.0_real64, " MiB"
        if (retained > limit) then
            write(output_unit, '(a)') "  [FAIL] this path retained an Arrow buffer it should have released"
            write(error_unit, '(a)') "check_arrow_release: " // label // " retained too much"
            error stop 1
        end if
        write(output_unit, '(a)') "  [PASS]"
    end subroutine verdict

    !> Bytes one whole copy of `table`'s resident columns occupies, for the verdict's denominator.
    function resident_bytes(table) result(bytes)
        type(parquet_table), intent(in) :: table !! the table just materialized.
        integer(int64) :: bytes                  !! float64 values it now holds.

        bytes = table%nrows() * int(table%ncols(resident_only=.true.), int64) * 8_int64
    end function resident_bytes

    !> `%materialize_all`: every column, the whole file.
    subroutine check_materialize_all(file, tolerance)
        character(len=*), intent(in) :: file  !! fixture to read.
        real(real64), intent(in) :: tolerance !! allowed retention.
        type(parquet_table) :: t
        integer(c_int64_t) :: before, after

        call parquet_open_table(t, file)
        before = parquet_get_arrow_bytes_allocated()
        call t%materialize_all()
        after = parquet_get_arrow_bytes_allocated()
        call verdict("materialize_all", before, after, resident_bytes(t), tolerance)
    end subroutine check_materialize_all

    !> `%prefetch`: a named subset, which takes the batch-release path rather than the whole-file one.
    subroutine check_prefetch(file, tolerance)
        character(len=*), intent(in) :: file  !! fixture to read.
        real(real64), intent(in) :: tolerance !! allowed retention.
        type(parquet_table) :: t
        character(len=:), allocatable :: names(:)
        integer(c_int64_t) :: before, after
        integer :: half

        call parquet_open_table(t, file)
        call t%column_names(names)
        half = max(size(names) / 2, 1)
        before = parquet_get_arrow_bytes_allocated()
        call t%prefetch(names(1:half))
        after = parquet_get_arrow_bytes_allocated()
        call verdict("prefetch (half the columns)", before, after, resident_bytes(t), tolerance)
    end subroutine check_prefetch

    !> `%get`: a single lazy first touch, the path every value accessor reaches.
    subroutine check_get(file, tolerance)
        character(len=*), intent(in) :: file  !! fixture to read.
        real(real64), intent(in) :: tolerance !! allowed retention.
        type(parquet_table) :: t
        character(len=:), allocatable :: names(:)
        real(real64), allocatable :: v(:)
        integer(c_int64_t) :: before, after

        call parquet_open_table(t, file)
        call t%column_names(names)
        before = parquet_get_arrow_bytes_allocated()
        call t%get(trim(names(1)), v)
        after = parquet_get_arrow_bytes_allocated()
        write(output_unit, '(a,es12.4)') "  checksum (keeps the read live) : ", sum(v)
        call verdict("get (one lazy first touch)", before, after, resident_bytes(t), tolerance)
    end subroutine check_get

    !> A slice: `materialize_slice` assembles the rows from several row groups, a different path.
    subroutine check_slice(file, tolerance)
        character(len=*), intent(in) :: file  !! fixture to read.
        real(real64), intent(in) :: tolerance !! allowed retention.
        type(parquet_table) :: full, sl
        integer(c_int64_t) :: before, after
        integer(int64) :: lo, hi

        call parquet_open_table(full, file)
        lo = full%nrows() / 4_int64 + 1_int64
        hi = (3_int64 * full%nrows()) / 4_int64
        call parquet_open_table(sl, file, lo, hi)
        before = parquet_get_arrow_bytes_allocated()
        call sl%materialize_all()
        after = parquet_get_arrow_bytes_allocated()
        call verdict("slice materialize_all", before, after, resident_bytes(sl), tolerance)
    end subroutine check_slice

    !> `parquet_write_table(release=.true.)`: the write materializes columns, then gives them back.
    subroutine check_write_release(file, tolerance)
        character(len=*), intent(in) :: file  !! fixture to read.
        real(real64), intent(in) :: tolerance !! allowed retention.
        type(parquet_table) :: t
        integer(c_int64_t) :: before, after
        integer(int64) :: bytes
        character(len=*), parameter :: out = "check_arrow_release_out.parquet"

        call parquet_open_table(t, file)
        bytes = t%nrows() * int(t%ncols(), int64) * 8_int64
        before = parquet_get_arrow_bytes_allocated()
        ! Schema-less, so it writes every resident column -- none are, so this materializes them
        ! all itself and then releases exactly what it materialized.
        call t%materialize_all()
        call parquet_write_table(t, out, overwrite=.true.)
        after = parquet_get_arrow_bytes_allocated()
        call verdict("write_table(release=.true.)", before, after, bytes, tolerance)
    end subroutine check_write_release

    !> The negative control: prove the counter moves at all on this build.
    !!
    !! Without it a checker that measured nothing -- a counter stuck at zero, an Arrow build with a
    !! different default pool -- would report PASS for every path above, which is the one failure
    !! mode a memory check cannot afford. Here the reader is asked for a column and NOT released,
    !! so the counter must rise by roughly one column's worth.
    subroutine check_control(file, tolerance)
        character(len=*), intent(in) :: file  !! fixture to read.
        real(real64), intent(in) :: tolerance !! unused; the control asserts the opposite direction.
        type(parquet_reader) :: r
        character(len=:), allocatable :: names(:)
        real(real64), allocatable :: v(:)
        integer(c_int64_t) :: before, after
        integer(int64) :: nrows, bytes
        real(real64) :: retained

        if (tolerance < 0.0_real64) continue      ! silences an unused-argument warning
        call parquet_open_reader(r, file)
        call parquet_get_column_names(r, names)
        call parquet_get_nrows(r, nrows)
        allocate(v(nrows))
        before = parquet_get_arrow_bytes_allocated()
        ! A plain reader caches every column it decodes, so this one is still held afterwards.
        call parquet_read_column(r, trim(names(1)), v)
        after = parquet_get_arrow_bytes_allocated()
        bytes = nrows * 8_int64
        retained = real(after - before, real64)
        write(output_unit, '(a)') "--- control (reader keeps its decoded column)"
        write(output_unit, '(a,f12.3,a)') "  arrow retained   : ", retained / 1048576.0_real64, " MiB"
        write(output_unit, '(a,f12.3,a)') "  expected at least: ", &
            0.5_real64 * real(bytes, real64) / 1048576.0_real64, " MiB"
        write(output_unit, '(a,es12.4)') "  checksum         : ", sum(v)
        if (retained < 0.5_real64 * real(bytes, real64)) then
            write(output_unit, '(a)') "  [FAIL] the pool counter did not move -- every other mode's PASS is meaningless"
            write(error_unit, '(a)') "check_arrow_release: control failed; the measurement itself is broken"
            error stop 1
        end if
        write(output_unit, '(a)') "  [PASS]"
    end subroutine check_control

    subroutine parse_arguments(mode, size_gb, file, ncols, tolerance)
        character(len=:), allocatable, intent(out) :: mode !! which check to run.
        real(real64), intent(out) :: size_gb               !! target fixture size in GB.
        character(len=:), allocatable, intent(out) :: file !! fixture path.
        integer, intent(out) :: ncols                      !! float64 columns in the fixture.
        real(real64), intent(out) :: tolerance             !! allowed retention, fraction of one copy.
        integer :: i, nargs, eq_pos, ios
        character(len=512) :: arg, key, val

        mode = ""
        size_gb = 0.05_real64
        file = "check_arrow_release.parquet"
        ncols = 6
        tolerance = 0.02_real64

        nargs = command_argument_count()
        if (nargs == 0) then
            call print_usage()
            stop
        end if
        do i = 1, nargs
            call get_command_argument(i, arg)
            eq_pos = index(arg, "=")
            if (eq_pos < 2) then
                write(error_unit, '(a)') "check_arrow_release: bad argument '"//trim(arg)//"', expected --key=value"
                error stop 1
            end if
            key = arg(1:eq_pos - 1)
            val = arg(eq_pos + 1:)
            select case (trim(key))
            case ("--mode")
                mode = trim(val)
            case ("--file")
                file = trim(val)
            case ("--size")
                read(val, *, iostat=ios) size_gb
                if (ios /= 0) error stop "check_arrow_release: --size needs a number"
            case ("--ncols")
                read(val, *, iostat=ios) ncols
                if (ios /= 0) error stop "check_arrow_release: --ncols needs an integer"
            case ("--tolerance")
                read(val, *, iostat=ios) tolerance
                if (ios /= 0) error stop "check_arrow_release: --tolerance needs a number"
            case default
                write(error_unit, '(a)') "check_arrow_release: unknown option '"//trim(key)//"'"
                error stop 1
            end select
        end do
    end subroutine parse_arguments

    subroutine print_usage()
        write(output_unit, '(a)') "Usage: fpm run check_arrow_release -- --mode=MODE [--file=PATH] [options]"
        write(output_unit, '(a)') "  --mode=write_fixture   write the test file (--size, --ncols)"
        write(output_unit, '(a)') "  --mode=materialize_all every column, whole file"
        write(output_unit, '(a)') "  --mode=prefetch        a named subset (batch release)"
        write(output_unit, '(a)') "  --mode=get             one lazy first touch"
        write(output_unit, '(a)') "  --mode=slice           a row slice assembled from several row groups"
        write(output_unit, '(a)') "  --mode=write_release   parquet_write_table(release=.true.)"
        write(output_unit, '(a)') "  --mode=control         negative control: the counter must MOVE"
        write(output_unit, '(a)') "  --tolerance=F          allowed retention as a fraction of one copy (default 0.02)"
        write(output_unit, '(a)') "Run one mode per process: see the header for why."
    end subroutine print_usage

end program benchmark_arrow_release
