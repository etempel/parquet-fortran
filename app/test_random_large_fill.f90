!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!> Manual large-scale check that `pf_random_fill_at` still works past 2**31 elements.
!>
!> **What this exists to catch, and why no `fpm test` can.** Both fill routines take their length
!> from `size(v)`. Asked without an explicit `kind=`, that returns a DEFAULT-kind integer, which
!> wraps for an array of 2**31 elements or more -- and the wrap fails silently in two different
!> ways, neither of which raises anything:
!>
!>   * a length that wraps NEGATIVE (2**31 elements exactly gives -2147483648) trips the
!>     zero-size guard, so the routine returns having written nothing at all and the caller's
!>     `intent(out)` array is left undefined;
!>   * a length that wraps to a small POSITIVE value (2**32 + 8 elements gives 8) fills that many
!>     elements and leaves the remaining billions undefined.
!>
!> Both were measured on the shipped module before the fix. The smallest array that reaches the
!> boundary is 2**31 `real32` values -- about 8.6 GB -- which is far past what `fpm test` or CI
!> should ever attempt, hence this program. `check_fill_size_kind` in
!> `tools/check_source_conventions.py` is the cheap always-on guard; this is the end-to-end proof.
!>
!> Correctness, not merely "something was written": every probed element is compared against
!> `pf_random32_at`/`pf_random_at` at the same position, which is the contract the fill promises.
!>
!> Run it through `tools/test_random_large_fill.sh`, which supplies `--profile release`.
program test_random_large_fill

    use iso_fortran_env, only: int32, int64, real32, real64, output_unit, error_unit
    use parquet_random

    implicit none

    !> Where to probe, as fractions of the array. The first and last elements are what the two
    !! observed failure modes distinguish: a wrapped-negative length writes neither, a wrapped-small
    !! positive length writes the first and not the last.
    real(real64), parameter :: probe_fractions(7) = &
        [0.0_real64, 1.0e-9_real64, 0.25_real64, 0.5_real64, 0.75_real64, 0.999_real64, 1.0_real64]

    integer(int64) :: n_elem
    integer :: kind_sel
    logical :: ok

    call parse_arguments(n_elem, kind_sel)

    write(output_unit, '(a)') "test_random_large_fill"
    write(output_unit, '(a,i0)') "  elements  : ", n_elem
    write(output_unit, '(a,i0)') "  real kind : ", merge(32, 64, kind_sel == 32)
    write(output_unit, '(a,f0.2,a)') "  memory    : ", &
        real(n_elem, real64) * merge(4.0_real64, 8.0_real64, kind_sel == 32) / 1.0e9_real64, " GB"

    ok = .true.
    if (kind_sel == 32) then
        call check_real32(n_elem, ok)
    else
        call check_real64(n_elem, ok)
    end if

    if (.not. ok) then
        write(error_unit, '(a)') "test_random_large_fill: FAILED"
        error stop 1
    end if
    write(output_unit, '(a)') "test_random_large_fill: PASSED"

contains

    !> Fills a `real32` array of `n` elements and verifies probed positions against the scalar draw.
    subroutine check_real32(n, ok)
        integer(int64), intent(in) :: n             !! how many elements to allocate
        logical, intent(inout) :: ok                !! cleared if any probe disagrees
        real(real32), allocatable :: v(:)
        real(real32), parameter :: sentinel = -1.0_real32
        integer :: stat
        integer(int64) :: p
        integer :: j

        allocate(v(n), stat=stat)
        if (stat /= 0) then
            write(error_unit, '(a)') "  allocation failed -- not enough memory for this size"
            ok = .false.
            return
        end if
        v = sentinel
        call pf_random_fill_at(12345_int64, 7_int64, v)

        do j = 1, size(probe_fractions)
            p = probe_position(n, j)
            if (v(p) == sentinel) then
                write(error_unit, '(a,i0,a)') "  element ", p, " was never written"
                ok = .false.
            else if (v(p) /= pf_random32_at(12345_int64, 7_int64, p)) then
                write(error_unit, '(a,i0,a)') "  element ", p, " does not match the scalar draw"
                ok = .false.
            end if
        end do
        deallocate(v)
    end subroutine check_real32

    !> Fills a `real64` array of `n` elements and verifies probed positions against the scalar draw.
    subroutine check_real64(n, ok)
        integer(int64), intent(in) :: n             !! how many elements to allocate
        logical, intent(inout) :: ok                !! cleared if any probe disagrees
        real(real64), allocatable :: v(:)
        real(real64), parameter :: sentinel = -1.0_real64
        integer :: stat
        integer(int64) :: p
        integer :: j

        allocate(v(n), stat=stat)
        if (stat /= 0) then
            write(error_unit, '(a)') "  allocation failed -- not enough memory for this size"
            ok = .false.
            return
        end if
        v = sentinel
        call pf_random_fill_at(12345_int64, 7_int64, v)

        do j = 1, size(probe_fractions)
            p = probe_position(n, j)
            if (v(p) == sentinel) then
                write(error_unit, '(a,i0,a)') "  element ", p, " was never written"
                ok = .false.
            else if (v(p) /= pf_random_at(12345_int64, 7_int64, p)) then
                write(error_unit, '(a,i0,a)') "  element ", p, " does not match the scalar draw"
                ok = .false.
            end if
        end do
        deallocate(v)
    end subroutine check_real64

    !> Turns probe `j` into a 1-based element index inside `1 .. n`.
    function probe_position(n, j) result(p)
        integer(int64), intent(in) :: n             !! element count
        integer, intent(in) :: j                    !! which probe
        integer(int64) :: p                         !! a valid 1-based index
        p = 1_int64 + int(probe_fractions(j) * real(n - 1_int64, real64), int64)
        if (p < 1_int64) p = 1_int64
        if (p > n) p = n
    end function probe_position

    !> Reads `--elements=` and `--kind=` from the command line.
    subroutine parse_arguments(n, kind_sel)
        integer(int64), intent(out) :: n            !! element count to allocate
        integer, intent(out) :: kind_sel            !! 32 or 64, selecting the real kind
        character(len=256) :: arg, key, val
        integer :: i, eq_pos, ios

        n = 2147483648_int64                        ! 2**31: the smallest size that wraps
        kind_sel = 32                               ! real32 -- half the memory of real64

        do i = 1, command_argument_count()
            call get_command_argument(i, arg)
            if (trim(arg) == "--help" .or. trim(arg) == "-h") then
                write(output_unit, '(a)') "usage: test_random_large_fill [--elements=N] [--kind=32|64]"
                write(output_unit, '(a)') "  --elements=N   how many array elements (default 2147483648 = 2**31)"
                write(output_unit, '(a)') "  --kind=32|64   real kind to fill (default 32; real32 halves the memory)"
                stop
            end if
            eq_pos = index(arg, "=")
            if (eq_pos < 2) then
                write(error_unit, '(a)') "test_random_large_fill: bad argument '" // trim(arg) // "', expected --key=value"
                error stop 1
            end if
            key = arg(1:eq_pos - 1)
            val = arg(eq_pos + 1:)
            select case (trim(key))
            case ("--elements")
                read(val, *, iostat=ios) n
                if (ios /= 0 .or. n < 1_int64) then
                    write(error_unit, '(a)') "test_random_large_fill: --elements must be a positive integer"
                    error stop 1
                end if
            case ("--kind")
                read(val, *, iostat=ios) kind_sel
                if (ios /= 0 .or. (kind_sel /= 32 .and. kind_sel /= 64)) then
                    write(error_unit, '(a)') "test_random_large_fill: --kind must be 32 or 64"
                    error stop 1
                end if
            case default
                write(error_unit, '(a)') "test_random_large_fill: unknown argument '" // trim(key) // "'"
                error stop 1
            end select
        end do
    end subroutine parse_arguments

end program test_random_large_fill
