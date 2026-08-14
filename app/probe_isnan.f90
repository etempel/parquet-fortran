!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> Diagnostic probe: what does one NaN test cost, and which formulation is cheapest?
!!
!! **Why this exists.** The Stage 1e comparator campaign (`feature_benchmark_comparator.md`) found
!! the Fortran comparator slower than the C++ one on machine B and *faster* on machine A, and the
!! gap decomposes into two independent effects. The second one is confined to the `f64` fixtures and
!! is not reproducible on machine A at all:
!!
!! | | extra ns on the Fortran `f64` arm, over its own `i64` arm |
!! |---|---|
!! | machine A (arm64, gfortran 15.2) | **+0.16** |
!! | machine B, gfortran 14.2.1 | **+5.47** |
!! | machine B, ifx 2026.1.1 | **+3.45** |
!! | machine B, the C++ side, either toolchain | +0.16 to +1.27 |
!!
!! The **only** code difference between the engine's integer and real paths is `sort_tier_of`'s
!! `ieee_is_nan(key%reals(i))`, which runs twice per comparison. On machine A that compiles to a
!! single `fcmp d31, d31` with no libm call, i.e. ~0.08 ns. If it costs ~2.7 ns on machine B, the
!! whole second effect is explained by one intrinsic, and the fix is a formulation change rather
!! than anything structural.
!!
!! **This probe touches no library code.** It is a standalone loop over a plain `real(real64)` array,
!! so a result here cannot be blamed on `sort_key_buf`, on derived-type access, or on inlining
!! decisions inside `parquet_sorting` — which is exactly what makes it worth running before anything
!! is changed.
!!
!! **The four arms**, each a full pass over the same in-cache array:
!!
!!   * `ieee_is_nan`  — what `src/parquet_sorting_engine.f90` uses today.
!!   * `x /= x`       — the classic idiom. Correct (a NaN is the only value unequal to itself) but it
!!                      trips gfortran's `-Wcompare-reals`, which is why CLAUDE.md's convention
!!                      prefers `ieee_is_nan`. If it is dramatically cheaper on x86-64, that
!!                      convention has a cost nobody had measured.
!!   * `bit test`     — `transfer` to `int64`, mask off the sign, and test for exponent-all-ones with
!!                      a nonzero mantissa. Branch-free and touches no FP unit.
!!   * `control`      — an ordinary `<` comparison and nothing else, so the loop overhead itself can
!!                      be subtracted from the three above.
!!
!! Every arm accumulates into a counter that is printed, so none of them can be optimised away.
!!
!! Usage: `tools/probe_isnan.sh`. Delete this program once the question it asks is answered — it is a
!! diagnostic, not part of the benchmark suite.
program probe_isnan
    use iso_fortran_env, only : int64, real64, output_unit
    use ieee_arithmetic, only : ieee_is_nan, ieee_value, ieee_quiet_nan
#ifdef _OPENMP
    use omp_lib, only : omp_get_wtime
#endif
    implicit none
    !
    integer(int64), parameter :: n = 4096_int64      !! array length; 32 KB, deliberately in cache.
    integer(int64), parameter :: reps = 8192_int64   !! passes.
    integer, parameter :: rounds = 5                 !! timed rounds; the minimum is reported.
    real(real64), allocatable :: x(:)
    real(real64) :: qnan
    integer(int64) :: k, c1, c2, c3, c4
    real(real64) :: t1, t2, t3, t4
    integer :: j
    !
    allocate(x(n))
    qnan = ieee_value(1.0_real64, ieee_quiet_nan)
    ! ~3% NaN, matching the comparator benchmark's f64nan fixture. The proportion matters: an arm
    ! that is branchy rather than branch-free will look artificially good at 0% and bad at 50%.
    do k = 1_int64, n
        x(k) = real(k, real64) * 0.5_real64
        if (mod(k, 31_int64) == 0_int64) x(k) = qnan
    end do
    !
    t1 = huge(1.0_real64); t2 = t1; t3 = t1; t4 = t1
    do j = 1, rounds
        t1 = min(t1, time_ieee(x, c1))
        t2 = min(t2, time_selfne(x, c2))
        t3 = min(t3, time_bits(x, c3))
        t4 = min(t4, time_control(x, c4))
    end do
    !
    write(output_unit,'(a)') "=============================================================="
    write(output_unit,'(a)') "probe_isnan -- what does one NaN test cost?"
    write(output_unit,'(a,i0,a,i0,a,i0)') "  n: ", n, "   reps: ", reps, "   rounds (min): ", rounds
    write(output_unit,'(a,i0)') "  tests per timed pass: ", n * reps
    write(output_unit,'(a)') "=============================================================="
    write(output_unit,'(a)') ""
    write(output_unit,'(a)') "  arm            ns/test   minus control   count"
    call row("ieee_is_nan", t1, t4, c1)
    call row("x /= x", t2, t4, c2)
    call row("bit test", t3, t4, c3)
    call row("control (<)", t4, t4, c4)
    write(output_unit,'(a)') ""
    write(output_unit,'(a)') "The three NaN arms must report the SAME count (the control will not);"
    write(output_unit,'(a)') "a differing count means one of them is not answering the same question."
    if (c1 /= c2 .or. c1 /= c3) then
        write(output_unit,'(a)') "COUNTS DIFFER -- do not quote these timings."
        error stop 1
    end if
    write(output_unit,'(a)') "Counts agree."
    !
contains

    !> One result line, with the control subtracted so the loop overhead is not counted twice.
    subroutine row(label, t, tc, c)
        character(len=*), intent(in) :: label !! arm name.
        real(real64), intent(in) :: t         !! this arm's best time, seconds.
        real(real64), intent(in) :: tc        !! the control's best time, seconds.
        integer(int64), intent(in) :: c       !! its count, printed so nothing is elided.
        real(real64) :: per
        !
        per = 1.0e9_real64 / real(n * reps, real64)
        write(output_unit,'(a,a14,f9.3,a,f9.3,a,i0)') "  ", label, t * per, "   ", (t - tc) * per, &
            "        ", c
    end subroutine row

    !> `ieee_is_nan` — the formulation the engine uses today.
    function time_ieee(x, c) result(t)
        real(real64), intent(in) :: x(:)     !! the data.
        integer(int64), intent(out) :: c     !! NaNs seen.
        real(real64) :: t                    !! seconds.
        integer(int64) :: rep, k
        real(real64) :: t0
        t0 = wtime(); c = 0_int64
        do rep = 1_int64, reps
            do k = 1_int64, n
                if (ieee_is_nan(x(k))) c = c + 1_int64
            end do
        end do
        t = wtime() - t0
    end function time_ieee

    !> `x /= x` — correct, but it trips `-Wcompare-reals`.
    function time_selfne(x, c) result(t)
        real(real64), intent(in) :: x(:)     !! the data.
        integer(int64), intent(out) :: c     !! NaNs seen.
        real(real64) :: t                    !! seconds.
        integer(int64) :: rep, k
        real(real64) :: t0
        t0 = wtime(); c = 0_int64
        do rep = 1_int64, reps
            do k = 1_int64, n
                if (x(k) /= x(k)) c = c + 1_int64
            end do
        end do
        t = wtime() - t0
    end function time_selfne

    !> Integer bit test: exponent all ones and mantissa nonzero, sign masked off. No FP unit at all.
    function time_bits(x, c) result(t)
        real(real64), intent(in) :: x(:)     !! the data.
        integer(int64), intent(out) :: c     !! NaNs seen.
        real(real64) :: t                    !! seconds.
        integer(int64) :: rep, k, b
        integer(int64), parameter :: expmask = 9218868437227405312_int64 !! 0x7FF0000000000000
        integer(int64), parameter :: absmask = 9223372036854775807_int64 !! 0x7FFFFFFFFFFFFFFF
        real(real64) :: t0
        t0 = wtime(); c = 0_int64
        do rep = 1_int64, reps
            do k = 1_int64, n
                b = iand(transfer(x(k), 1_int64), absmask)
                if (b > expmask) c = c + 1_int64
            end do
        end do
        t = wtime() - t0
    end function time_bits

    !> Control: an ordinary comparison, so the loop's own cost can be subtracted.
    function time_control(x, c) result(t)
        real(real64), intent(in) :: x(:)     !! the data.
        integer(int64), intent(out) :: c     !! how many exceeded the threshold.
        real(real64) :: t                    !! seconds.
        integer(int64) :: rep, k
        real(real64) :: t0
        t0 = wtime(); c = 0_int64
        do rep = 1_int64, reps
            do k = 1_int64, n
                if (x(k) < 0.0_real64) c = c + 1_int64
            end do
        end do
        t = wtime() - t0
    end function time_control

    !> Wall clock.
    function wtime() result(t)
        real(real64) :: t !! seconds.
#ifndef _OPENMP
        integer(int64) :: cc, r
#endif
#ifdef _OPENMP
        t = omp_get_wtime()
#else
        call system_clock(count=cc, count_rate=r)
        t = real(cc, real64) / real(r, real64)
#endif
    end function wtime

end program probe_isnan
