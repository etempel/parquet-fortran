!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> **`parquet_column%gather_from`'s within-column team**, the A/B between its serial and its
!! threaded form over every storable kind.
!!
!! Separate from `test_columns` for the reason `test_string_parallel` is separate from
!! `test_parquet_string`: test-drive runs a suite's tests inside its own `!$omp parallel do`, and a
!! team opened inside that region collapses to ONE thread (this library enables no nested
!! parallelism; `max-active-levels` is 1 on every runtime it is built with). A threaded arm run
!! there would be the serial arm under another name, and the test would pass while comparing
!! serial with serial -- this project's worst failure mode. This suite is excluded from that
!! parallelism in `test_runner_support.f90`, so `threads=` here really opens a team.
!!
!! **The test is an A/B against the serial form**, because the two are genuinely different code:
!! one range of rows against several, and one bitmap written whole against words written by
!! several threads on boundaries `gather_ranges` chooses. The contract is that they produce
!! identical columns, and the value oracle is the fixture's own code rather than either arm.
module test_columns_parallel
    use parquet_columns
    use parquet_strings, only : parquet_string_column, parquet_debug_set_string_min_bytes, &
        parquet_debug_string_bulk_threads
    use test_columns, only : EVERY_KIND, FIXW, make_any_fixture, expect_any_row
    use iso_fortran_env, only : int32, int64
    use testdrive, only : new_unittest, unittest_type, error_type, check, skip_test
#ifdef _OPENMP
    use omp_lib, only : omp_get_num_procs
#endif
    !
    implicit none
    private
    public :: collect_tests_columns_parallel
    !
    !> The team the threaded arm asks for. `parquet_strings`' break-even (`STRING_MIN_THREADS`, 4)
    !! is the floor the string kinds thread at, so this must stay at or above it, and the test
    !! skips on a machine with fewer processors -- the clamp would hand the string store 3 and it
    !! would decline.
    integer, parameter :: THREADS_FOR_TEST = 4
    !
    !> Payload floor low enough that a test-sized string column reaches the store's threaded path.
    integer(int64), parameter :: TINY_FLOOR = 64_int64
    !
contains
    !
    !> Registers this suite's tests.
    subroutine collect_tests_columns_parallel(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:) !! receives the tests.
        testsuite = [ &
            new_unittest("gather_from on a team equals gather_from on one thread, every kind", &
                test_gather_from_threaded_equals_serial) &
            ]
    end subroutine collect_tests_columns_parallel
    !
    !> Every kind: a source of 269 rows with a null every 7th row (and, on a vector kind, a null
    !! second element every 11th), gathered by a 600-entry scattered index with repeats under a
    !! mask that nulls every 13th destination row -- on one thread and on `THREADS_FOR_TEST`.
    !!
    !! 269 rows are 4 whole 64-row periods plus a ragged tail at width 1, and 8 periods plus a
    !! tail at width 2, so the ranges cover the interior words and the partial last word both. The
    !! negative control asks the library's own rule (`parquet_debug_column_gather_threads`, and
    !! the string store's `parquet_debug_string_bulk_threads`) whether the threaded arm really
    !! opens a team for this shape, so a rule that always answered 1 would fail here rather than
    !! pass as serial-equals-serial.
    subroutine test_gather_from_threaded_equals_serial(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_column) :: src, ser, par
        type(parquet_string_column), pointer :: sp
        integer(int64), parameter :: NSRC = 269_int64, M = 600_int64
        integer(int64) :: idx(M), k, e, w
        logical :: mask(M)
        integer :: ki, kind, team
        character(len=:), allocatable :: kn
        !
#ifndef _OPENMP
        call skip_test(error, "needs OpenMP: without it the !$omp region in gather_build is preprocessed " // &
            "out, so the threaded arm would run the serial copy and this would compare serial with serial")
        return
#else
        if (omp_get_num_procs() < THREADS_FOR_TEST) then
            call skip_test(error, "needs at least four processors: gather_from clamps an explicit team to " // &
                "omp_get_num_procs(), and the string store declines below its break-even of four")
            return
        end if
        do k = 1_int64, M
            idx(k) = 1_int64 + mod(k*37_int64, NSRC)
            mask(k) = mod(k, 13_int64) /= 0_int64
        end do
        do ki = 1, size(EVERY_KIND)
            kind = EVERY_KIND(ki)
            call parquet_kind_name(kind, kn)
            call make_any_fixture(src, kind, NSRC)
            w = int(src%colwidth(), int64)
            do k = 7_int64, NSRC, 7_int64
                call src%set_null(k)
            end do
            if (w > 1_int64) then
                do k = 11_int64, NSRC, 11_int64
                    call src%set_null(k, 2_int64)
                end do
            end if
            !
            call ser%gather_from(src, idx, valid=mask, threads=1)
            if (kind == PK_STRING .or. kind == PK_STRING_VEC) then
                call src%string_column(sp)
                call parquet_debug_set_string_min_bytes(TINY_FLOOR)
                team = parquet_debug_string_bulk_threads(sp, THREADS_FOR_TEST)
                call par%gather_from(src, idx, valid=mask, threads=THREADS_FOR_TEST)
                call parquet_debug_set_string_min_bytes(0_int64)
            else
                team = parquet_debug_column_gather_threads(M, int(w, int32), THREADS_FOR_TEST)
                call par%gather_from(src, idx, valid=mask, threads=THREADS_FOR_TEST)
            end if
            call check(error, team > 1, kn // ": negative control -- the library's own rule must open a team " // &
                "for this shape, or the arm above compared serial with serial")
            if (allocated(error)) return
            !
            call check(error, par%length() == M .and. ser%length() == M, kn // ": both arms must give M rows")
            if (allocated(error)) return
            do k = 1_int64, M
                do e = 1_int64, w
                    call check(error, par%is_null(k, e) .eqv. ser%is_null(k, e), &
                        kn // ": the threaded arm must place every null exactly where the serial arm does")
                    if (allocated(error)) return
                    call check(error, par%is_null(k, e) .eqv. (src%is_null(idx(k), e) .or. .not. mask(k)), &
                        kn // ": every null must be the source's at idx(k) OR the mask's, on the threaded arm")
                    if (allocated(error)) return
                end do
                if (par%is_null(k)) cycle
                call expect_any_row(par, kind, k, idx(k), kn // ": a threaded gather's row must hold source row idx(k)", &
                    error)
                if (allocated(error)) return
            end do
        end do
        call check(error, .true., "every kind's threaded gather_from equals its serial one")
#endif
    end subroutine test_gather_from_threaded_equals_serial
    !
end module test_columns_parallel
