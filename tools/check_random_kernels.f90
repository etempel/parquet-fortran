!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!> Driver for `tools/check_random_kernels.sh`: asserts `parquet_random`'s contract in whichever
!> route (e) kernel it was compiled with.
!>
!> **This file is deliberately NOT under `app/` or `test/`.** It `use`s the modules from `test/`,
!> which an `app/` target may not do, and it must be compilable on its own against a single
!> `src/` file rather than against the built library -- so it lives in `tools/`, which fpm does
!> not scan. Nothing builds it except its own shell wrapper.
!>
!> Why it exists at all: the wrapping kernel (the `#else` arm of the fork, which ships wherever
!> there is no 128-bit integer kind) is built by no routine check in this project, because every
!> compiler in the fleet except ifx has such a kind. It was miscompiled by two different gfortran
!> releases under LTO, in two DIFFERENT and non-overlapping sets of call shapes -- so this driver
!> sweeps the shapes rather than trusting any one of them, and the wrapper builds it both ways.
!>
!> Output is one machine-readable line per run, `KERNEL=... RESULT=... FAILED=...`, so the wrapper
!> can assert that the two builds really did differ rather than silently checking one kernel twice.
program check_random_kernels

    use iso_fortran_env, only: int32, int64, real32, real64, output_unit
    use parquet_random
    use test_random_vectors
    use test_random_reference

    implicit none

    integer :: failed
    character(len=8) :: kernel

    failed = 0
    if (parquet_debug_random_uses_int128()) then
        kernel = "int128"
    else
        kernel = "wrapping"
    end if

    call check_fork()
    call check_golden_scalar()
    call check_golden_int()
    call check_golden_key()
    call check_golden_fill()
    call check_literal_seed_shapes()
    call check_variable_shapes()
    call check_integer_widths()

    write(output_unit, '(a,a,a,a,a,i0)') "KERNEL=", trim(kernel), &
        "  RESULT=", trim(merge("PASS", "FAIL", failed == 0)), "  FAILED=", failed
    if (failed > 0) stop 1

contains

    !> Records one failed assertion, naming it once so a failure is diagnosable from the log.
    subroutine bad(what)
        character(len=*), intent(in) :: what        !! short description of the assertion
        failed = failed + 1
        if (failed <= 10) write(output_unit, '(a)') "  [FAIL] " // what
    end subroutine bad

    !> The compiled kernel must match the compiler's actual capability -- unless the wrapper has
    !> deliberately defeated the allowlist, which is the whole point of the second build.
    subroutine check_fork()
        logical :: capable
        capable = selected_int_kind(38) > 0
        if (capable .and. parquet_debug_random_uses_int128()) return       ! ordinary build
        if (.not. capable .and. .not. parquet_debug_random_uses_int128()) return
        if (capable .and. .not. parquet_debug_random_uses_int128()) return ! forced wrapping build
        call bad("fork reports int128 on a compiler with no 128-bit kind")
    end subroutine check_fork

    !> Every row of the scalar golden grid, in all three scalar forms.
    subroutine check_golden_scalar()
        integer :: k
        do k = 1, n_scalar
            if (pf_random_bits_at(scalar_seed(k), scalar_stream(k), scalar_draw(k)) /= scalar_bits(k)) &
                call bad("golden pf_random_bits_at")
            if (transfer(pf_random_at(scalar_seed(k), scalar_stream(k), scalar_draw(k)), 0_int64) &
                /= scalar_at_bits(k)) call bad("golden pf_random_at")
            if (transfer(pf_random32_at(scalar_seed(k), scalar_stream(k), scalar_draw(k)), 0_int32) &
                /= scalar_at32_bits(k)) call bad("golden pf_random32_at")
        end do
    end subroutine check_golden_scalar

    !> Every row of the integer golden table.
    subroutine check_golden_int()
        integer :: k
        do k = 1, n_int
            if (pf_random_int_at(int_seed(k), int_stream(k), int_lo(k), int_hi(k), int_draw(k)) /= int_value(k)) &
                call bad("golden pf_random_int_at")
        end do
    end subroutine check_golden_int

    !> Every row of the derived-key golden table.
    subroutine check_golden_key()
        integer :: k
        do k = 1, n_key
            if (pf_random_key(key_seed(k), key_label(k)) /= key_value(k)) call bad("golden pf_random_key")
        end do
    end subroutine check_golden_key

    !> Every fill case of the golden table, in both real kinds.
    subroutine check_golden_fill()
        integer :: c, k
        real(real64) :: v64(16)
        real(real32) :: v32(16)
        do c = 1, n_fill
            call pf_random_fill_at(fill_seed(c), fill_stream(c), v64(1:fill_count(c)), fill_start(c))
            call pf_random_fill_at(fill_seed(c), fill_stream(c), v32(1:fill_count(c)), fill_start(c))
            do k = 1, fill_count(c)
                if (transfer(v64(k), 0_int64) /= fill_at_bits(fill_first(c) + k - 1)) call bad("golden fill real64")
                if (transfer(v32(k), 0_int32) /= fill_at32_bits(fill_first(c) + k - 1)) call bad("golden fill real32")
            end do
        end do
    end subroutine check_golden_fill

    !> Literal-constant seed at the call site, over a NON-NEGATIVE stream range.
    !!
    !! **The stream range is the load-bearing part of this sweep, and it was measured, not
    !! guessed.** Building the wrapping kernel at `-O3 -flto` on gfortran 14.2.1, every stream range
    !! that the compiler can prove non-negative returns wrong `pf_random32_at` values -- `1..1`,
    !! `0..7`, `1..8`, `1..24` and `1..64` were each 100 % wrong -- while every range spanning zero
    !! was 100 % correct: `-3..3`, `-8..8` and `-40..40` all clean. Value-range propagation over the
    !! stream is what decides it.
    !!
    !! Two consequences a future reader must not undo. `do i = 1, n` is the module's own documented
    !! idiom, so the broken range is the one users actually write. And a sweep centred on zero --
    !! the natural way to write "cover negatives too", and what `test_agreement_scalar` does with
    !! `-40..40` -- lands squarely in the clean range and detects nothing. Sweep both signs, in
    !! separate loops, and never merge them into one symmetric range.
    subroutine check_literal_seed_shapes()
        integer(int64) :: i, d
        do i = 1_int64, 24_int64                       ! non-negative: the range that has failed
            do d = 1_int64, 4_int64
                if (pf_random_bits_at(12345_int64, i, d) /= ref_bits(12345_int64, i, d)) call bad("literal-seed bits")
                if (pf_random_at(12345_int64, i, d) /= ref_at(12345_int64, i, d)) call bad("literal-seed at")
                if (pf_random32_at(12345_int64, i, d) /= ref_at32(12345_int64, i, d)) call bad("literal-seed at32")
            end do
            if (pf_random_bits_at(12345_int64, i) /= ref_bits(12345_int64, i, 1_int64)) call bad("literal-seed bits, no draw")
            if (pf_random_at(12345_int64, i) /= ref_at(12345_int64, i, 1_int64)) call bad("literal-seed at, no draw")
            if (pf_random32_at(12345_int64, i) /= ref_at32(12345_int64, i, 1_int64)) call bad("literal-seed at32, no draw")
        end do
        do i = -24_int64, -1_int64                     ! strictly negative, as its own range
            if (pf_random_bits_at(12345_int64, i) /= ref_bits(12345_int64, i, 1_int64)) call bad("literal-seed bits, neg")
            if (pf_random_at(12345_int64, i) /= ref_at(12345_int64, i, 1_int64)) call bad("literal-seed at, neg")
            if (pf_random32_at(12345_int64, i) /= ref_at32(12345_int64, i, 1_int64)) call bad("literal-seed at32, neg")
        end do
    end subroutine check_literal_seed_shapes

    !> Seed, stream and draw all variables -- again with the stream range kept non-negative.
    !!
    !! Separate from the sweep above because the two are separately specialised by the compiler:
    !! gfortran 15.2 was reported miscompiling literal-seed shapes while all-variable shapes stayed
    !! correct, and 14.2.1 was measured doing the reverse on the same source. Neither is the safe
    !! one, so both are swept.
    subroutine check_variable_shapes()
        integer(int64) :: s, i, d
        do s = -3_int64, 3_int64
            do i = 1_int64, 12_int64
                do d = 1_int64, 4_int64
                    if (pf_random_bits_at(s, i, d) /= ref_bits(s, i, d)) call bad("variable-shape bits")
                    if (pf_random_at(s, i, d) /= ref_at(s, i, d)) call bad("variable-shape at")
                    if (pf_random32_at(s, i, d) /= ref_at32(s, i, d)) call bad("variable-shape at32")
                end do
                if (pf_random32_at(s, i) /= ref_at32(s, i, 1_int64)) call bad("variable-shape at32, no draw")
            end do
        end do
    end subroutine check_variable_shapes

    !> The integer rule across every width regime, which is where the wrapping arithmetic
    !! (`sub64`/`add64`, and the rejection threshold) is reached at all.
    subroutine check_integer_widths()
        integer(int64), parameter :: los(11) = [ &
            0_int64, 1_int64, -100_int64, -6148914691236517205_int64, 0_int64, &
            -huge(1_int64) - 1_int64, -huge(1_int64), 7_int64, -3_int64, 0_int64, 0_int64]
        integer(int64), parameter :: his(11) = [ &
            999999_int64, 6_int64, 100_int64, 6148914691236517205_int64, huge(1_int64), &
            huge(1_int64), huge(1_int64), 7_int64, 4294967296_int64, &
            7378697629483820645_int64, 5534023222112865484_int64]
        integer :: g
        integer(int64) :: i, d, value, refv, refr
        do g = 1, 11
            do i = 1_int64, 12_int64
                do d = 1_int64, 3_int64
                    value = pf_random_int_at(12345_int64, i, los(g), his(g), d)
                    call ref_int_at(12345_int64, i, los(g), his(g), d, refv, refr)
                    if (value /= refv) call bad("integer width regime")
                end do
            end do
        end do
    end subroutine check_integer_widths

end program check_random_kernels
