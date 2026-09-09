!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!> Exercises the library from multiple OpenMP threads at once, since parquet
!> and arrow are linked with openmp = "*" in fpm.toml and callers may want to
!> read or write many files concurrently from a parallel region.
module test_openmp
    use parquet
    use parquet_maml_base, only : parquet_maml_file, get_parquet_maml
    use iso_fortran_env, only : real64, int32, int64
    use testdrive, only : new_unittest, unittest_type, error_type, check, test_failed
    !$ use omp_lib, only : omp_get_max_threads
    !
    implicit none
    private
    public :: collect_tests_parquet_openmp_write
    public :: collect_tests_parquet_openmp
    !
    integer, parameter :: nfiles = 8
    !
contains
    !
    !> testdrive's own run_testsuite runs every test *within one collection*
    !> concurrently with each other by default (parallel_ defaults to .true.
    !> in testdrive.F90, and this project never overrides it) -- tests in
    !> *different* collections/suites still run sequentially relative to each
    !> other, since run_tester.f90 calls run_testsuite once per suite in a
    !> plain sequential loop. test_write_parallel's output
    !> (test_run/test_openmp_write_*.parquet) is read by several of the other
    !> tests below, so it's split into its own collection here to guarantee
    !> it fully completes before any of them start, rather than racing them.
    subroutine collect_tests_parquet_openmp_write(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            new_unittest("write different parquet files in parallel", test_write_parallel), &
            new_unittest("streaming write: parallel-computed row groups, written serially, round-trip", &
                test_streaming_write_parallel_compute_serial_write) &
            ]
    end subroutine collect_tests_parquet_openmp_write

    !> Every test here only reads files it never writes itself: most read
    !> test_run/test_openmp_write_*.parquet (produced by
    !> collect_tests_parquet_openmp_write, run beforehand as its own suite --
    !> see the note there); test_shared_file_read_parallel instead reads
    !> test_run/test_simple.parquet, produced by test_writing.f90's suite
    !> (which run_tester.f90 also lists before this one). None of these tests
    !> write a file another test in this same collection could read, so it's
    !> safe for them to run concurrently with each other, exactly as
    !> testdrive's own default (parallel) test execution already does.
    subroutine collect_tests_parquet_openmp(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            new_unittest("read different parquet files in parallel", test_read_parallel), &
            new_unittest("mixed read/write of different files in parallel", test_mixed_read_write_parallel), &
            new_unittest("parse MAML files concurrently", test_maml_parallel), &
            new_unittest("repeatedly open/close readers on a shared file in parallel", test_shared_file_read_parallel), &
            new_unittest("stress: many threads, many files", test_stress_parallel), &
            new_unittest("a shared table's resident columns are read by many threads at once", &
                test_table_shared_read_parallel), &
            new_unittest("per-thread slice tables append into one shared table", &
                test_table_parallel_append), &
            new_unittest("materialize_all reads columns in parallel and agrees with the serial path", &
                test_table_parallel_prefetch_agrees), &
            new_unittest("one parquet_table_writer per thread, each writing its own file, is allowed", &
                test_sink_per_thread_allowed), &
            new_unittest("a thread-private table may still be mutated inside a parallel region", &
                test_table_private_mutation_allowed) &
            ]
    end subroutine collect_tests_parquet_openmp

    !> Each thread opens, writes and closes its own parquet_writer / file, so
    !> nothing is shared across threads except the loop index bookkeeping.
    subroutine test_write_parallel(error)
        type(error_type), allocatable, intent(out) :: error
        integer :: i, cmdstat
        logical :: ok(nfiles)
        character(len=64) :: filenames(nfiles)

        call execute_command_line("mkdir -p test_run", wait=.true., cmdstat=cmdstat)
        if (cmdstat /= 0) then
            call test_failed(error, "failed to create test_run directory")
            return
        end if

        do i = 1, nfiles
            write(filenames(i), '(A,I0,A)') "test_run/test_openmp_write_", i, ".parquet"
        end do

        ok = .false.
        !$omp parallel do default(shared) private(i)
        do i = 1, nfiles
            call write_one_file(filenames(i), i, ok(i))
        end do
        !$omp end parallel do

        do i = 1, nfiles
            call check(error, ok(i), &
                "ok(i)")
            if (allocated(error)) then
                call test_failed(error, "parallel write failed for file: " // trim(filenames(i)))
                return
            end if
        end do
    end subroutine test_write_parallel

    subroutine write_one_file(filename, seed, ok)
        character(len=*), intent(in) :: filename
        integer, intent(in) :: seed
        logical, intent(out) :: ok
        type(parquet_writer) :: writer
        real(real64), dimension(5) :: xdata
        integer :: j

        xdata = [(real(seed*10 + j, kind=real64), j=1,5)]

        call parquet_open_writer(writer, filename)
        call parquet_write_column(writer, "colx", xdata)
        call parquet_close_writer(writer)

        inquire(file=filename, exist=ok)
    end subroutine write_one_file

    !> Validates the specific "parallel compute, serial write" pattern this project recommends
    !> for the streaming row-group API (see doc/pages/operating/thread-safety.md): OpenMP threads only
    !> ever compute chunk data into private slots of a shared buffer, never touch the shared
    !> parquet_writer itself -- the actual parquet_new_row_group/parquet_write_column_chunk/
    !> parquet_finish_row_group calls stay on a single thread, in row-group order, exactly as
    !> the concurrency guard requires (see scenario_concurrent_calls_into_shared_writer in
    !> error_scenarios.f90 for what happens if that rule is broken instead).
    subroutine test_streaming_write_parallel_compute_serial_write(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        character(len=*), parameter :: out_file = "test_run/test_openmp_streaming_write.parquet"
        integer, parameter :: rows_per_group = 5
        integer, parameter :: num_groups = 6
        integer, parameter :: col_size = 3
        integer(int32) :: vec_buf(col_size, rows_per_group, num_groups)
        integer(int32) :: vec_expected(col_size, rows_per_group * num_groups)
        integer(int32) :: vec_back(col_size, rows_per_group * num_groups)
        integer :: g, r, e

        !$omp parallel do default(shared) private(g, r, e)
        do g = 1, num_groups
            do r = 1, rows_per_group
                do e = 1, col_size
                    vec_buf(e, r, g) = (g - 1) * rows_per_group * col_size + (r - 1) * col_size + e
                end do
            end do
        end do
        !$omp end parallel do

        vec_expected = reshape(vec_buf, [col_size, rows_per_group * num_groups])

        call parquet_open_writer(writer, out_file, chunk_size=rows_per_group)
        do g = 1, num_groups
            call parquet_new_row_group(writer, rows_per_group)
            call parquet_write_column_chunk(writer, "v", vec_buf(:, :, g))
            call parquet_finish_row_group(writer)
        end do
        call parquet_close_writer(writer)

        call parquet_open_reader(reader, out_file)
        call parquet_read_column(reader, "v", vec_back)
        call parquet_close_reader(reader)

        call check(error, all(vec_back == vec_expected), &
            "a streaming write fed by parallel-computed row-group data did not round-trip correctly")
    end subroutine test_streaming_write_parallel_compute_serial_write

    !> Reads back the files produced by test_write_parallel, again with each
    !> thread owning an independent parquet_reader instance.
    subroutine test_read_parallel(error)
        type(error_type), allocatable, intent(out) :: error
        integer :: i
        logical :: ok(nfiles)
        character(len=64) :: filenames(nfiles)

        do i = 1, nfiles
            write(filenames(i), '(A,I0,A)') "test_run/test_openmp_write_", i, ".parquet"
        end do

        ok = .false.
        !$omp parallel do default(shared) private(i)
        do i = 1, nfiles
            call read_one_file(filenames(i), i, ok(i))
        end do
        !$omp end parallel do

        do i = 1, nfiles
            call check(error, ok(i), &
                "ok(i)")
            if (allocated(error)) then
                call test_failed(error, "parallel read failed or returned wrong data for file: " // trim(filenames(i)))
                return
            end if
        end do
    end subroutine test_read_parallel

    subroutine read_one_file(filename, seed, ok)
        character(len=*), intent(in) :: filename
        integer, intent(in) :: seed
        logical, intent(out) :: ok
        type(parquet_reader) :: reader
        real(real64), dimension(:), allocatable :: xdata
        integer :: nrows, j

        ok = .false.

        call parquet_open_reader(reader, filename)
        call parquet_get_nrows(reader, nrows)
        if (nrows /= 5) then
            call parquet_close_reader(reader)
            return
        end if

        allocate(xdata(nrows))
        call parquet_read_column(reader, "colx", xdata)
        call parquet_close_reader(reader)

        do j = 1, 5
            if (xdata(j) /= real(seed*10 + j, kind=real64)) return
        end do

        ok = .true.
    end subroutine read_one_file

    !> Half the threads write brand-new files while the other half concurrently
    !> read files produced earlier by test_write_parallel; real usage rarely
    !> keeps read and write phases cleanly separated.
    subroutine test_mixed_read_write_parallel(error)
        type(error_type), allocatable, intent(out) :: error
        integer :: i, cmdstat
        logical :: ok(nfiles)
        character(len=64) :: write_files(nfiles), read_files(nfiles)

        call execute_command_line("mkdir -p test_run", wait=.true., cmdstat=cmdstat)
        if (cmdstat /= 0) then
            call test_failed(error, "failed to create test_run directory")
            return
        end if

        do i = 1, nfiles
            write(write_files(i), '(A,I0,A)') "test_run/test_openmp_mixed_", i, ".parquet"
            write(read_files(i), '(A,I0,A)') "test_run/test_openmp_write_", i, ".parquet"
        end do

        ok = .false.
        !$omp parallel do default(shared) private(i)
        do i = 1, nfiles
            if (mod(i,2) == 0) then
                call write_one_file(write_files(i), i, ok(i))
            else
                call read_one_file(read_files(i), i, ok(i))
            end if
        end do
        !$omp end parallel do

        do i = 1, nfiles
            call check(error, ok(i), &
                "ok(i)")
            if (allocated(error)) then
                if (mod(i,2) == 0) then
                    call test_failed(error, "parallel write failed for file: " // trim(write_files(i)))
                else
                    call test_failed(error, "parallel read failed or returned wrong data for file: " // trim(read_files(i)))
                end if
                return
            end if
        end do
    end subroutine test_mixed_read_write_parallel

    !> MAML parsing is separate code from the reader/writer path, so exercise
    !> it independently in case the underlying parser keeps any shared state.
    subroutine test_maml_parallel(error)
        type(error_type), allocatable, intent(out) :: error
        integer :: i
        logical :: ok(nfiles)

        ok = .false.
        !$omp parallel do default(shared) private(i)
        do i = 1, nfiles
            call parse_maml_once(ok(i))
        end do
        !$omp end parallel do

        do i = 1, nfiles
            call check(error, ok(i), &
                "ok(i)")
            if (allocated(error)) then
                call test_failed(error, "concurrent MAML parsing failed on one or more threads")
                return
            end if
        end do
    end subroutine test_maml_parallel

    subroutine parse_maml_once(ok)
        logical, intent(out) :: ok
        type(parquet_schema) :: schema
        integer :: idx

        ok = .false.

        schema%maml = get_parquet_maml("maml_example.maml")
        call parquet_parse_maml(schema)

        idx = schema%get_column_index("id0")
        if (idx /= 1) return

        idx = schema%get_column_index("idarr")
        if (idx /= 2) return

        ok = .true.
    end subroutine parse_maml_once

    !> All threads repeatedly open/close independent parquet_reader instances
    !> pointed at the *same* read-only file, which is more likely than
    !> distinct-file access to expose contention inside Arrow's file handling.
    subroutine test_shared_file_read_parallel(error)
        type(error_type), allocatable, intent(out) :: error
        integer :: i
        logical :: ok(nfiles)
        character(len=*), parameter :: shared_file = "test_run/test_simple.parquet"
        logical :: exists

        inquire(file=shared_file, exist=exists)
        call check(error, exists, &
            "exists")
        if (allocated(error)) then
            call test_failed(error, "input parquet file missing: expected " // shared_file)
            return
        end if

        ok = .false.
        !$omp parallel do default(shared) private(i)
        do i = 1, nfiles
            call read_shared_file_once(shared_file, ok(i))
        end do
        !$omp end parallel do

        do i = 1, nfiles
            call check(error, ok(i), &
                "ok(i)")
            if (allocated(error)) then
                call test_failed(error, "repeated concurrent reads of a shared file failed on one or more threads")
                return
            end if
        end do
    end subroutine test_shared_file_read_parallel

    subroutine read_shared_file_once(filename, ok)
        character(len=*), intent(in) :: filename
        logical, intent(out) :: ok
        type(parquet_reader) :: reader
        real(real64), dimension(:), allocatable :: xdata
        integer :: nrows, cycle_idx

        ok = .false.

        do cycle_idx = 1, 3
            call parquet_open_reader(reader, filename)
            call parquet_get_nrows(reader, nrows)
            if (nrows /= 5) then
                call parquet_close_reader(reader)
                return
            end if

            if (allocated(xdata)) deallocate(xdata)
            allocate(xdata(nrows))
            call parquet_read_column(reader, "colx", xdata)
            call parquet_close_reader(reader)

            if (xdata(1) /= 1.0_real64 .or. xdata(nrows) /= real(nrows, kind=real64)) return
        end do

        ok = .true.
    end subroutine read_shared_file_once

    !> Higher fan-out stress case sized off the runtime's actual thread count,
    !> to shake out races that only surface with real parallelism rather than
    !> the modest fixed fan-out used by the other tests in this module.
    subroutine test_stress_parallel(error)
        type(error_type), allocatable, intent(out) :: error
        integer :: i, cmdstat, nthreads, n
        logical, allocatable :: ok(:)
        character(len=64), allocatable :: filenames(:)

        nthreads = 1
        !$ nthreads = omp_get_max_threads()
        n = max(nfiles, nthreads*4)

        call execute_command_line("mkdir -p test_run", wait=.true., cmdstat=cmdstat)
        if (cmdstat /= 0) then
            call test_failed(error, "failed to create test_run directory")
            return
        end if

        allocate(ok(n), filenames(n))
        do i = 1, n
            write(filenames(i), '(A,I0,A)') "test_run/test_openmp_stress_", i, ".parquet"
        end do

        ok = .false.
        !$omp parallel do default(shared) private(i)
        do i = 1, n
            call write_one_file(filenames(i), i, ok(i))
        end do
        !$omp end parallel do

        do i = 1, n
            call check(error, ok(i), &
                "ok(i)")
            if (allocated(error)) then
                call test_failed(error, "stress write failed for file: " // trim(filenames(i)))
                return
            end if
        end do

        ok = .false.
        !$omp parallel do default(shared) private(i)
        do i = 1, n
            call read_one_file(filenames(i), i, ok(i))
        end do
        !$omp end parallel do

        do i = 1, n
            call check(error, ok(i), &
                "ok(i)")
            if (allocated(error)) then
                call test_failed(error, "stress read failed or returned wrong data for file: " // trim(filenames(i)))
                return
            end if
        end do
    end subroutine test_stress_parallel

    ! ---- parquet_table concurrency (milestone 3d) ------------------------------------------
    !
    !> Writes the multi-column fixture the table concurrency tests below read.
    !>
    !> Each of them writes its OWN file rather than sharing one: testdrive runs the tests in a
    !> collection concurrently, and two tests writing one path truncate it under each other (see
    !> CLAUDE.md, "Tests run concurrently").
    subroutine write_table_fixture(filename, nrows)
        character(len=*), intent(in) :: filename
        integer, intent(in) :: nrows
        type(parquet_writer) :: writer
        real(real64), allocatable :: a(:), b(:), c(:)
        integer(int32), allocatable :: id(:)
        integer :: j

        allocate(a(nrows), b(nrows), c(nrows), id(nrows))
        do j = 1, nrows
            id(j) = j
            a(j) = real(j, real64)
            b(j) = real(j, real64)*2.0_real64
            c(j) = real(j, real64)*3.0_real64
        end do
        call parquet_open_writer(writer, filename)
        call parquet_write_column(writer, "id", id)
        call parquet_write_column(writer, "a", a)
        call parquet_write_column(writer, "b", b)
        call parquet_write_column(writer, "c", c)
        call parquet_close_writer(writer)
    end subroutine write_table_fixture

    !> Use case A: one thread makes the columns resident, then many threads read them at once.
    !>
    !> This is the shape the whole design protects -- a read of a resident column takes no lock
    !> and no atomic -- so the test is that it produces the right answer under real concurrency,
    !> not that it is fast.
    subroutine test_table_shared_read_parallel(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), parameter :: fname = "test_run/test_openmp_table_read.parquet"
        integer, parameter :: nrows = 500
        type(parquet_table) :: t
        real(real64), pointer :: pa(:)
        real(real64) :: total, expect
        integer :: i

        call write_table_fixture(fname, nrows)
        call parquet_open_table(t, fname)
        ! Before the region, exactly as the contract requires: a first touch inside one is a hard
        ! error on a shared table.
        call t%prefetch(["a", "b"])
        call t%col("a", pa)

        total = 0.0_real64
        !$omp parallel do default(shared) private(i) reduction(+:total)
        do i = 1, nrows
            total = total + pa(i)
        end do
        !$omp end parallel do

        expect = real(nrows, real64)*real(nrows + 1, real64)/2.0_real64
        call check(error, abs(total - expect) < 1.0e-6_real64, &
            "a shared table's resident column must read the same from many threads as from one")
    end subroutine test_table_shared_read_parallel

    !> Use case B: each thread opens its own slice table, then appends into one shared table.
    !>
    !> The shared table's lock is what makes the append safe; nothing here writes an !$omp
    !> critical. Arrival order is non-deterministic, so the assertions are on the row count and
    !> on the SET of values, never on their order.
    subroutine test_table_parallel_append(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), parameter :: fname = "test_run/test_openmp_table_append.parquet"
        integer, parameter :: nrows = 400, nchunk = 8
        type(parquet_table) :: out
        real(real64), allocatable :: got(:)
        real(real64) :: total, expect
        real(real64) :: empty(0)
        integer :: g, i

        call write_table_fixture(fname, nrows)
        ! The destination needs the column before the region: %append refuses a batch carrying a
        ! column the destination does not have (it will not silently drop one), and adding a
        ! column is itself a structural change the shared-table guard would refuse inside the
        ! region -- which is exactly the "prepare before the region" shape the contract asks for.
        call parquet_new_table(out)
        call out%add_column("a", empty)

        !$omp parallel do default(shared) private(g) schedule(dynamic)
        do g = 1, nchunk
            block
                ! Declared in a block, NOT in a private() clause: parquet_table is finalizable and
                ! gfortran does not reliably default-initialise a private copy of such a type.
                type(parquet_table) :: mine, batch
                real(real64), allocatable :: vals(:)
                integer(int64) :: lo, hi
                lo = int((g - 1)*(nrows/nchunk) + 1, int64)
                hi = int(g*(nrows/nchunk), int64)
                call parquet_open_table(mine, fname, lo, hi)
                ! A first touch on a table THIS thread opened inside the region is permitted --
                ! that is the whole point of the ownership-keyed guard.
                call mine%get("a", vals)
                call parquet_new_table(batch)
                call batch%add_column("a", vals)
                ! Serialized by the shared table's own lock.
                call out%append(batch)
            end block
        end do
        !$omp end parallel do

        call check(error, out%nrows() == int(nrows, int64), &
            "every appended batch's rows must survive a concurrent append")
        if (allocated(error)) return
        call out%get("a", got)
        total = 0.0_real64
        do i = 1, nrows
            total = total + got(i)
        end do
        expect = real(nrows, real64)*real(nrows + 1, real64)/2.0_real64
        call check(error, abs(total - expect) < 1.0e-6_real64, &
            "a concurrent append must lose and duplicate no rows, whatever order they arrive in")
    end subroutine test_table_parallel_append

    !> Use case C: %materialize_all reads its columns on several threads internally.
    !>
    !> An A/B equality against the serial path, which is what a "same answer, just faster" claim
    !> actually needs -- the parallel path is only reachable with several columns and no read-time
    !> transform, so the serial half deliberately opens with a filter to force the other branch.
    subroutine test_table_parallel_prefetch_agrees(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), parameter :: fname = "test_run/test_openmp_table_prefetch.parquet"
        integer, parameter :: nrows = 300
        type(parquet_table) :: par, ser
        real(real64), allocatable :: pa(:), sa(:), pc(:), sc(:)
        integer :: i
        logical :: same

        call write_table_fixture(fname, nrows)
        ! No transform and four columns: takes the internally-parallel path when more than one
        ! thread is available, and the ordinary serial one otherwise. Either way the answer must
        ! match a table materialized one column at a time.
        call parquet_open_table(par, fname)
        call par%materialize_all()

        call parquet_open_table(ser, fname)
        call ser%get("a", sa)     ! one column at a time: the serial first-touch path
        call ser%get("c", sc)

        call par%get("a", pa)
        call par%get("c", pc)
        call check(error, size(pa) == size(sa) .and. size(pc) == size(sc), &
            "a parallel materialize_all must produce the same row counts as the serial path")
        if (allocated(error)) return
        same = .true.
        do i = 1, nrows
            if (abs(pa(i) - sa(i)) > 1.0e-9_real64) same = .false.
            if (abs(pc(i) - sc(i)) > 1.0e-9_real64) same = .false.
        end do
        call check(error, same, &
            "a parallel materialize_all must produce the same values as the serial path")
    end subroutine test_table_parallel_prefetch_agrees

    !> The negative control for the structural-mutation guard: a table a thread opened itself
    !> inside the region is thread-private, so mutating it must NOT be refused.
    !>
    !> Without this, a guard that fired unconditionally would pass every error scenario written
    !> for it while making the slice regime unusable.
    !> The negative control for the sink's shared-use refusal (`sink_shared_in_parallel`,
    !! test/error_scenarios.f90): a sink this thread opened inside the region is its own, so one
    !! sink per thread, each writing its own file, must go through. The sinks are elements of an
    !! array declared BEFORE the region, not block-locals and not `private()` copies: the type has
    !! allocatable components (ifx cannot privatize such a type in a block) and is finalizable
    !! through them (gfortran's `private()` copy is not reliably initialised) -- see
    !! doc/pages/operating/thread-safety.md.
    subroutine test_sink_per_thread_allowed(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), parameter :: fname = "test_run/test_openmp_sink_src.parquet"
        integer, parameter :: nrows = 200, nchunk = 4
        type(parquet_table_writer) :: sinks(nchunk)
        integer(int64) :: counts(nchunk)
        character(len=48) :: outs(nchunk)
        integer :: g
        logical :: ok

        call write_table_fixture(fname, nrows)
        counts = -1_int64
        do g = 1, nchunk
            write(outs(g), '(a, i0, a)') "test_run/test_openmp_sink_", g, ".parquet"
        end do

        !$omp parallel do default(shared) private(g)
        do g = 1, nchunk
            block
                type(parquet_table) :: mine
                integer(int64) :: lo, hi
                lo = int((g - 1)*(nrows/nchunk) + 1, int64)
                hi = int(g*(nrows/nchunk), int64)
                call parquet_open_table(mine, fname, lo, hi)
                call mine%materialize_all()
                call parquet_open_table_writer(sinks(g), trim(outs(g)), mine, chunk_size=10)
                call sinks(g)%append(mine)
                call sinks(g)%append(mine)
                counts(g) = sinks(g)%nrows()
                call parquet_close_table_writer(sinks(g))
            end block
        end do
        !$omp end parallel do

        ok = .true.
        do g = 1, nchunk
            if (counts(g) /= int(2*(nrows/nchunk), int64)) ok = .false.
        end do
        call check(error, ok, "a sink opened and used by one thread inside a region must be allowed")
        if (allocated(error)) return
        do g = 1, nchunk
            block
                type(parquet_table) :: back
                call parquet_open_table(back, trim(outs(g)))
                if (back%nrows() /= int(2*(nrows/nchunk), int64)) ok = .false.
            end block
        end do
        call check(error, ok, "every per-thread file holds the rows its thread appended")
    end subroutine test_sink_per_thread_allowed

    subroutine test_table_private_mutation_allowed(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), parameter :: fname = "test_run/test_openmp_table_private.parquet"
        integer, parameter :: nrows = 200, nchunk = 4
        integer(int64) :: counts(nchunk)
        integer :: g
        logical :: ok

        call write_table_fixture(fname, nrows)
        counts = -1_int64

        !$omp parallel do default(shared) private(g)
        do g = 1, nchunk
            block
                type(parquet_table) :: mine
                logical, allocatable :: keep(:)
                integer(int64) :: lo, hi
                integer :: k
                lo = int((g - 1)*(nrows/nchunk) + 1, int64)
                hi = int(g*(nrows/nchunk), int64)
                call parquet_open_table(mine, fname, lo, hi)
                call mine%materialize_all()
                allocate(keep(mine%nrows()))
                keep = .false.
                do k = 1, int(mine%nrows())
                    if (mod(k, 2) == 0) keep(k) = .true.
                end do
                ! Every one of these is a structural change, on a table this thread opened inside
                ! the region: all five must be permitted. %top_n is here because it takes the same
                ! table_check_not_shared guard as its neighbours, and a guard that fires
                ! unconditionally passes its own abort scenario while breaking the permitted case.
                call mine%filter_rows(keep)
                call mine%sort_by(["a"])
                call mine%top_n(["a"], int(mine%nrows()))
                call mine%rename_column("a", "aa")
                call mine%drop_column("b")
                ! %compact and %reserve take the same table_check_not_shared guard, and are the
                ! only two procedures in parquet_tables_mutate that reallocate storage -- so they
                ! need the same negative control as their neighbours above. A guard that refused a
                ! thread-private table here would make "reserve, fill, compact" unusable inside
                ! exactly the per-thread slice pattern it is most useful in.
                call mine%reserve(int(mine%nrows(), int32) + 50)
                call mine%compact()
                counts(g) = mine%nrows()
            end block
        end do
        !$omp end parallel do

        ok = .true.
        do g = 1, nchunk
            if (counts(g) /= int(nrows/nchunk/2, int64)) ok = .false.
        end do
        call check(error, ok, &
            "a thread-private table must still permit filter_rows/sort_by/rename/drop in a region")
    end subroutine test_table_private_mutation_allowed

end module test_openmp
