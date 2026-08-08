!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> **`parquet_string_column`'s internally-threaded bulk operations.**
!!
!! Separate from `test_parquet_string` for the same reason `test_table_parallel` is separate from
!! `test_table`, and it cannot work any other way: a string operation resolves to ONE thread inside
!! an existing OpenMP parallel region, deliberately, since a nested region is the caller's business.
!! test-drive runs a suite's tests inside its own `!$omp parallel do`, so every test placed in a
!! parallelized suite would compare the serial path against itself and **pass while testing
!! nothing** — this project's worst failure mode, and one that already happened once here (see
!! `feature_string_parallel.md` S4, where two mutations survived for exactly this reason). This
!! suite is excluded from that parallelism in `run_tester.f90`.
!!
!! The tests also write process-global state — `parquet_set_string_threads` and the payload-floor
!! override — which is an independent reason for the exclusion.
!!
!! **Every test here is an A/B against the serial path**, because the threaded and serial forms are
!! genuinely different code (the serial one is a single pass; the threaded one is a three-phase
!! split), and the contract between them is that they produce **byte-identical** columns. An
!! equality assertion is the only thing that holds them to that.
module test_string_parallel
    use parquet
    use iso_fortran_env, only : int64
    use testdrive, only : new_unittest, unittest_type, error_type, check
    !
    implicit none
    private
    public :: collect_tests_string_parallel
    !
    !> Payload floor low enough that a test-sized column reaches the threaded path.
    integer(int64), parameter :: TINY_FLOOR = 64_int64
    !
contains
    !
    !> Registers this suite's tests.
    subroutine collect_tests_string_parallel(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:) !! receives the tests.
        testsuite = [ &
            new_unittest("threaded reindex equals the serial reindex, byte for byte", &
                test_reindex_threaded_equals_serial), &
            new_unittest("threaded reindex places nulls identically across byte boundaries", &
                test_reindex_threaded_nulls), &
            new_unittest("the thread floor declines below its break-even", test_thread_break_even) &
            ]
    end subroutine collect_tests_string_parallel
    !
    !> Builds a column of `n` elements with deterministic, varying lengths, optionally null every
    !! `null_every`-th element. Lengths vary so the payload split is uneven, which is where an
    !! offset bug shows.
    subroutine build(c, n, null_every)
        type(parquet_string_column), intent(inout) :: c !! receives the column.
        integer(int64), intent(in) :: n                 !! element count.
        integer(int64), intent(in) :: null_every        !! null stride; <= 0 for no nulls.
        integer(int64) :: k, l
        character(len=64) :: buf
        call c%clear()
        do k = 1_int64, n
            if (null_every > 0_int64) then
                if (mod(k, null_every) == 0_int64) then
                    call c%append_null()
                    cycle
                end if
            end if
            l = 1_int64 + mod(k*7_int64, 23_int64)
            buf = repeat(char(ichar("a") + int(mod(k, 26_int64))), int(l))
            call c%append_string(buf(1:int(l)))
        end do
    end subroutine build
    !
    !> Whether two columns are identical in every observable: rows, payload, nulls and every value.
    logical function same_column(a, b) result(res)
        type(parquet_string_column), intent(in) :: a !! first column.
        type(parquet_string_column), intent(in) :: b !! second column.
        character(len=:), allocatable :: sa, sb
        integer(int64) :: k
        res = .false.
        if (a%size() /= b%size()) return
        if (a%character_size() /= b%character_size()) return
        if (a%null_count() /= b%null_count()) return
        do k = 1_int64, a%size()
            if (a%is_null(k) .neqv. b%is_null(k)) return
            if (a%is_null(k)) cycle
            call a%get(k, sa)
            call b%get(k, sb)
            ! Nested rather than `.and.`-ed: Fortran does not short-circuit, so a length test and a
            ! content test on one line would compare two differently-sized strings.
            if (len(sa) /= len(sb)) return
            if (sa /= sb) return
        end do
        res = .true.
    end function same_column
    !
    !> **The core contract: the threaded rebuild and the serial one produce identical columns.**
    !!
    !! Both arms reindex the same source with the same permutation; one is forced serial with
    !! `parquet_set_string_threads(1)`, the other left automatic with the payload floor lowered so it
    !! actually threads. The **negative control is asserting that the threaded arm really did
    !! thread** — without it, a floor or a gate that silently declined would make this compare the
    !! serial path against itself and pass while testing nothing.
    !!
    !! Row counts are swept across byte boundaries (not multiples of 8) because the validity split is
    !! byte-aligned and an off-by-one there is invisible at round numbers.
    subroutine test_reindex_threaded_equals_serial(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_string_column) :: src, ser, par
        integer(int64), allocatable :: perm(:)
        integer(int64) :: n, k
        integer :: threaded_n
        logical :: ever_threaded
        !
        ever_threaded = .false.
        do n = 1000_int64, 1007_int64
            call build(src, n, 0_int64)
            allocate(perm(n))
            do k = 1_int64, n
                perm(k) = n - k + 1_int64       ! reversal: every element moves
            end do
            !
            call parquet_debug_set_string_min_bytes(0_int64)
            call parquet_set_string_threads(1)
            ser = src%clone()
            call ser%reindex_trusted(perm)
            !
            call parquet_set_string_threads(0)
            call parquet_debug_set_string_min_bytes(TINY_FLOOR)
            par = src%clone()
            threaded_n = parquet_debug_string_bulk_threads(par)
            if (threaded_n > 1) ever_threaded = .true.
            call par%reindex_trusted(perm)
            !
            call parquet_debug_set_string_min_bytes(0_int64)
            call check(error, same_column(ser, par), "threaded reindex must equal the serial reindex")
            if (allocated(error)) return
            call check(error, par%validate(), "the threaded result satisfies the class invariants")
            if (allocated(error)) return
            deallocate(perm)
        end do
        call check(error, ever_threaded, &
            "negative control: at least one arm must actually have threaded, or this compares serial with serial")
    end subroutine test_reindex_threaded_equals_serial
    !
    !> **The validity bitmap is the one thing a wrong split corrupts silently.**
    !!
    !! Eight rows share a byte, so two threads meeting inside one would lose each other's writes and
    !! the column would still validate — just with the wrong rows null. This drives null strides that
    !! are coprime with 8 (3, 5, 7) so nulls land at every bit position within a byte, over row
    !! counts that are not multiples of 8, and compares against the serial arm element by element.
    subroutine test_reindex_threaded_nulls(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_string_column) :: src, ser, par
        integer(int64), allocatable :: perm(:)
        integer(int64) :: n, k, stride
        logical :: ever_threaded
        !
        ever_threaded = .false.
        do stride = 3_int64, 7_int64, 2_int64
            do n = 997_int64, 1000_int64
                call build(src, n, stride)
                allocate(perm(n))
                do k = 1_int64, n
                    ! A stride-13 rotation rather than a reversal, so a null at bit p lands at a
                    ! different bit q in the result -- a byte-boundary bug cannot cancel out.
                    perm(k) = 1_int64 + mod(k*13_int64, n)
                end do
                !
                call parquet_debug_set_string_min_bytes(0_int64)
                call parquet_set_string_threads(1)
                ser = src%clone()
                call ser%reindex_trusted(perm)
                !
                call parquet_set_string_threads(0)
                call parquet_debug_set_string_min_bytes(TINY_FLOOR)
                par = src%clone()
                if (parquet_debug_string_bulk_threads(par) > 1) ever_threaded = .true.
                call par%reindex_trusted(perm)
                call parquet_debug_set_string_min_bytes(0_int64)
                !
                call check(error, ser%null_count() == par%null_count(), &
                    "threaded and serial reindex must agree on the null COUNT")
                if (allocated(error)) return
                call check(error, same_column(ser, par), &
                    "threaded and serial reindex must agree on which rows are null, and on every value")
                if (allocated(error)) return
                call check(error, par%validate(), "the threaded null result satisfies the class invariants")
                if (allocated(error)) return
                deallocate(perm)
            end do
        end do
        call check(error, ever_threaded, "negative control: the null arms must actually have threaded")
    end subroutine test_reindex_threaded_nulls
    !
    !> The thread floor declines below its break-even rather than running a slower shape on two
    !! threads.
    !!
    !! Making a rebuild splittable costs about 1.7x serially, so the parallelism has to beat that
    !! before it is worth doing at all — two threads measured as a net loss. This asserts the gate
    !! implements that, with the automatic answer as the negative control.
    subroutine test_thread_break_even(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_string_column) :: col
        integer :: n_auto, n_two, n_three
        !
        call build(col, 2000_int64, 0_int64)
        call parquet_debug_set_string_min_bytes(TINY_FLOOR)
        !
        call parquet_set_string_threads(0)
        n_auto = parquet_debug_string_bulk_threads(col)
        call parquet_set_string_threads(2)
        n_two = parquet_debug_string_bulk_threads(col)
        call parquet_set_string_threads(3)
        n_three = parquet_debug_string_bulk_threads(col)
        !
        call parquet_set_string_threads(0)
        call parquet_debug_set_string_min_bytes(0_int64)
        !
        call check(error, n_two == 1, "a cap of 2 is below the break-even and must resolve to serial")
        if (allocated(error)) return
        call check(error, n_three == 1, "a cap of 3 is below the break-even and must resolve to serial")
        if (allocated(error)) return
        if (n_auto > 1) then
            call check(error, n_auto >= 4, &
                "negative control: when it does thread, it uses at least the break-even count")
        end if
    end subroutine test_thread_break_even
    !
end module test_string_parallel
