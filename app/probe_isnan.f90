!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> Stand-in for `sort_key_buf`, and three comparators over it that differ ONLY in the value path.
!!
!! Second half of the Stage 1e diagnosis (`feature_sort.md` §1e-D). After the NaN fix, the Fortran
!! `f64` arm still costs **5.41 ns more than its own `i64` arm under gfortran 14.2.1 on machine B**,
!! against **0.49 ns under ifx** and 1.29 ns for the C++ engine on the same machine — and **0.18 ns
!! under gfortran 15.2 on machine A**, which is why it cannot be diagnosed there.
!!
!! The engine's real and integer paths differ in exactly two ways, and this module separates them:
!!
!!   * `cmp_int`         -- tier check, then compare `%ints(a)` against `%ints(b)`
!!   * `cmp_real_novtest` -- tier check, then compare `%reals(a)` against `%reals(b)`
!!   * `cmp_real`        -- tier check **including the NaN test**, then compare `%reals`
!!
!! `cmp_real_novtest - cmp_int` is the cost of reaching a **real** allocatable component rather than
!! an integer one. `cmp_real - cmp_real_novtest` is the cost of the **NaN test** in context.
!!
!! **CALIBRATION WARNING, and it is not a footnote.** On machine A this section reports 0.453 +
!! 0.806 = **1.26 ns**, while the engine's own `f64 − i64` gap on that machine is **0.18 ns** — it
!! over-predicts by **7x**. So its ABSOLUTE numbers must not be quoted as a decomposition of the
!! engine's gap. Use it only for DIRECTION and RATIO ("which of the two dominates on this
!! toolchain"), and confirm anything it suggests with an in-context A/B on the real comparator.
!!
!! This is the same failure mode that made the integer bit test look best here and 21% worse in
!! context (`feature_sort.md` §1e-C): an isolated loop does not reproduce a comparator's dependency
!! chain, its inlining, or its common-subexpression elimination. The probe was right about
!! `ieee_is_nan` — a 2.5 ns library call is far outside its error bar — and is not to be trusted at
!! the sub-nanosecond scale this section works at.
!!
!! **These are deliberately MODULE procedures, not internal ones.** On ELF with `-fPIC` that makes
!! them global symbols and therefore non-inlinable (`feature_sort.md` §1e-C), which is exactly the
!! condition the real comparator runs under on machine B. An internal procedure here would be
!! inlined, would not reproduce that, and would measure a different question.
!!
!! `kbuf` mirrors `sort_key_buf`'s SHAPE — five allocatable components, `reals` second — because
!! component offsets and the number of descriptors are part of what is being measured.
module probe_keys
    use iso_fortran_env, only : int8, int64, real64
    use iso_c_binding, only : c_char
    implicit none
    public
    !
    integer, parameter :: PK_INT = 1  !! integer-valued key.
    integer, parameter :: PK_REAL = 2 !! real-valued key.
    !
    !> Same component shape as the engine's `sort_key_buf`.
    type :: kbuf
        integer :: family = PK_INT                       !! PK_INT or PK_REAL.
        logical :: descending = .false.                  !! unused here; present for layout fidelity.
        logical :: nulls_first = .false.                 !! placement of the null tier.
        integer(int64), allocatable :: ints(:)           !! integer values.
        real(real64), allocatable :: reals(:)            !! real values.
        integer(int64), allocatable :: offsets(:)        !! unused here; layout fidelity.
        character(kind=c_char), allocatable :: data(:)   !! unused here; layout fidelity.
        integer(int8), allocatable :: valid(:)           !! 1 = valid; UNALLOCATED means no nulls.
    end type kbuf
    !
contains

    !> Null tier only: 0 for a value, 2 for a null (or the reverse under `nulls_first`).
    integer function tier_null(key, i)
        type(kbuf), intent(in) :: key    !! the key.
        integer(int64), intent(in) :: i  !! row, 1-based.
        logical :: is_null
        is_null = .false.
        if (allocated(key%valid)) is_null = (key%valid(i) == 0_int8)
        if (key%nulls_first) then
            tier_null = 0
            if (.not. is_null) tier_null = 2
        else
            tier_null = 0
            if (is_null) tier_null = 2
        end if
    end function tier_null

    !> Null tier plus the NaN tier -- the engine's real-key `sort_tier_of`, in full.
    integer function tier_nan(key, i)
        type(kbuf), intent(in) :: key    !! the key.
        integer(int64), intent(in) :: i  !! row, 1-based.
        logical :: is_null, is_nan
        is_null = .false.
        if (allocated(key%valid)) is_null = (key%valid(i) == 0_int8)
        is_nan = .false.
        if (.not. is_null .and. key%family == PK_REAL) is_nan = (key%reals(i) /= key%reals(i))
        if (key%nulls_first) then
            if (is_null) then
                tier_nan = 0
            else if (is_nan) then
                tier_nan = 1
            else
                tier_nan = 2
            end if
        else
            if (is_null) then
                tier_nan = 2
            else if (is_nan) then
                tier_nan = 1
            else
                tier_nan = 0
            end if
        end if
    end function tier_nan

    !> Integer path: the cheap reference.
    integer function cmp_int(key, a, b)
        type(kbuf), intent(in) :: key            !! the key.
        integer(int64), intent(in) :: a, b       !! rows, 1-based.
        integer :: ta, tb
        integer(int64) :: va, vb
        ta = tier_null(key, a)
        tb = tier_null(key, b)
        if (ta /= tb) then
            cmp_int = -1
            if (ta > tb) cmp_int = 1
            return
        end if
        cmp_int = 0
        if (ta /= 0) return
        va = key%ints(a)
        vb = key%ints(b)
        if (va < vb) cmp_int = -1
        if (va > vb) cmp_int = 1
    end function cmp_int

    !> Real path WITHOUT the NaN test: isolates the cost of reaching `%reals` instead of `%ints`.
    integer function cmp_real_novtest(key, a, b)
        type(kbuf), intent(in) :: key            !! the key.
        integer(int64), intent(in) :: a, b       !! rows, 1-based.
        integer :: ta, tb
        real(real64) :: va, vb
        ta = tier_null(key, a)
        tb = tier_null(key, b)
        if (ta /= tb) then
            cmp_real_novtest = -1
            if (ta > tb) cmp_real_novtest = 1
            return
        end if
        cmp_real_novtest = 0
        if (ta /= 0) return
        va = key%reals(a)
        vb = key%reals(b)
        if (va < vb) cmp_real_novtest = -1
        if (va > vb) cmp_real_novtest = 1
    end function cmp_real_novtest

    !> Real path WITH the NaN test -- what the engine actually does for a real key.
    integer function cmp_real(key, a, b)
        type(kbuf), intent(in) :: key            !! the key.
        integer(int64), intent(in) :: a, b       !! rows, 1-based.
        integer :: ta, tb
        real(real64) :: va, vb
        ta = tier_nan(key, a)
        tb = tier_nan(key, b)
        if (ta /= tb) then
            cmp_real = -1
            if (ta > tb) cmp_real = 1
            return
        end if
        cmp_real = 0
        if (ta /= 0) return
        va = key%reals(a)
        vb = key%reals(b)
        if (va < vb) cmp_real = -1
        if (va > vb) cmp_real = 1
    end function cmp_real

end module probe_keys

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
!!   * `ieee_is_nan`  — what `src/parquet_argsort_engine.f90` uses today.
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
    use probe_keys
    use iso_fortran_env, only : int8, int64, real64, output_unit
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
    call key_access_section()
    !
contains

    !> Second question: is the f64 penalty the REALS ACCESS or the NAN TEST?
    !!
    !! Three comparators over the same `kbuf`, differing only in the value path. The two differences
    !! are what the engine's real and integer paths actually differ by, so between them they account
    !! for the whole gap -- whichever is large is the answer.
    subroutine key_access_section()
        type(kbuf) :: ki, kr
        integer(int64) :: si, sn, sr, k
        real(real64) :: ti, tn, tr
        integer :: j
        !
        allocate(ki%ints(n), ki%reals(n), ki%valid(n))
        allocate(kr%ints(n), kr%reals(n), kr%valid(n))
        do k = 1_int64, n
            ! SAME values in both components: the three arms must do identical work, so their
            ! checksums must agree. Different cardinalities would give different tie rates and
            ! different branch behaviour, and the arms would not be comparable at all.
            ki%ints(k) = mod(k * 2654435761_int64, 64_int64)
            ki%reals(k) = real(ki%ints(k), real64)
            ki%valid(k) = 1_int8
            if (mod(k, 32_int64) == 0_int64) ki%valid(k) = 0_int8
        end do
        kr%ints = ki%ints
        kr%reals = ki%reals
        kr%valid = ki%valid
        ki%family = PK_INT
        kr%family = PK_REAL
        !
        ti = huge(1.0_real64); tn = ti; tr = ti
        do j = 1, rounds
            ti = min(ti, sweep(ki, 1, si))
            tn = min(tn, sweep(kr, 2, sn))
            tr = min(tr, sweep(kr, 3, sr))
        end do
        !
        write(output_unit,'(a)') ""
        write(output_unit,'(a)') "=============================================================="
        write(output_unit,'(a)') "key access -- is the f64 penalty the REALS ACCESS or the NAN TEST?"
        write(output_unit,'(a)') "=============================================================="
        write(output_unit,'(a)') ""
        write(output_unit,'(a)') "  arm                        ns/compare   checksum"
        call krow("int  (%ints, no NaN test)", ti, si)
        call krow("real (%reals, no NaN test)", tn, sn)
        call krow("real (%reals + NaN test)", tr, sr)
        write(output_unit,'(a)') ""
        write(output_unit,'(a,f9.3,a)') "  reals-vs-ints access costs : ", &
            (tn - ti) * 1.0e9_real64 / real(n * reps, real64), " ns/compare"
        write(output_unit,'(a,f9.3,a)') "  the NaN test costs         : ", &
            (tr - tn) * 1.0e9_real64 / real(n * reps, real64), " ns/compare"
        write(output_unit,'(a)') ""
        write(output_unit,'(a)') "  The engine's f64-minus-i64 gap should equal the sum of those two."
        write(output_unit,'(a)') "  On machine B under gfortran that gap is 5.41 ns; under ifx 0.49."
        if (si /= sn .or. si /= sr) then
            write(output_unit,'(a)') ""
            write(output_unit,'(a)') "  CHECKSUMS DIFFER -- the three arms did not do the same work,"
            write(output_unit,'(a)') "  so the two differences above are not comparable. Do not quote them."
            error stop 1
        end if
        write(output_unit,'(a)') "  All three arms agreed on their checksums."
    end subroutine key_access_section

    !> One timed sweep with comparator `which` (1 = int, 2 = real no-test, 3 = real + NaN test).
    function sweep(key, which, chk) result(t)
        type(kbuf), intent(in) :: key        !! the key.
        integer, intent(in) :: which         !! which comparator.
        integer(int64), intent(out) :: chk   !! checksum.
        real(real64) :: t                    !! seconds.
        integer(int64) :: rep, i, jj, stride
        real(real64) :: t0
        !
        t0 = wtime(); chk = 0_int64
        do rep = 0_int64, reps - 1_int64
            stride = 1_int64 + mod(rep, n - 1_int64)
            do i = 1_int64, n
                jj = i + stride
                if (jj > n) jj = jj - n
                select case (which)
                case (1)
                    chk = chk + int(cmp_int(key, i, jj), int64)
                case (2)
                    chk = chk + int(cmp_real_novtest(key, i, jj), int64)
                case default
                    chk = chk + int(cmp_real(key, i, jj), int64)
                end select
            end do
        end do
        t = wtime() - t0
    end function sweep

    !> One key-access result line.
    subroutine krow(label, t, c)
        character(len=*), intent(in) :: label !! arm name.
        real(real64), intent(in) :: t         !! best time, seconds.
        integer(int64), intent(in) :: c       !! checksum.
        write(output_unit,'(a,a28,f9.3,a,i0)') "  ", label, &
            t * 1.0e9_real64 / real(n * reps, real64), "     ", c
    end subroutine krow

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
