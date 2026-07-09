!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!> Exercises the library from multiple OpenMP threads at once, since parquet
!> and arrow are linked with openmp = "*" in fpm.toml and callers may want to
!> read or write many files concurrently from a parallel region.
module test_openmp
    use parquet
    use parquet_maml_base, only : parquet_maml_file, get_parquet_maml
    use iso_fortran_env, only : real64
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
            new_unittest("write different parquet files in parallel", test_write_parallel) &
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
            new_unittest("stress: many threads, many files", test_stress_parallel) &
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
            call check(error, ok(i))
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
            call check(error, ok(i))
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
            call check(error, ok(i))
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
            call check(error, ok(i))
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
        call check(error, exists)
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
            call check(error, ok(i))
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
            call check(error, ok(i))
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
            call check(error, ok(i))
            if (allocated(error)) then
                call test_failed(error, "stress read failed or returned wrong data for file: " // trim(filenames(i)))
                return
            end if
        end do
    end subroutine test_stress_parallel

end module test_openmp
