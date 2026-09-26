!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> Stage 1e: what does ONE comparison cost, in Fortran and in C++?
!!
!! `bench/benchmark_sort_engine.f90` measures whole sorts. That cannot answer the question Stage 1
!! leaves open (**O3**, *"whether the comparator can be made cheap enough in Fortran"*), because at
!! Stage 2 the introsort is new as well — a slow comparator and a slow sort would arrive together
!! and be indistinguishable. This program isolates the comparator while it is the only thing that
!! has changed.
!!
!! ## What it measures, and why it is shaped like this
!!
!! Two arms per fixture, timed identically:
!!
!!   * **fortran** — `parquet_debug_sort_sweep_less`/`_compare`, which loop inside `parquet_sorting`
!!     and call the new Fortran comparators directly.
!!   * **cpp** — `parquet_debug_sort_sweep_less_cpp`/`_compare_cpp`, which loop inside
!!     `src/parquet_wrapper.cpp` and call the shipped C++ comparators directly.
!!
!! **The sweep is batched on both sides, and that is the whole design.** A per-call harness would
!! cross `bind(C)` once per comparison, and that crossing costs more than the comparison — the
!! measurement would be of the crossing. One call per sweep amortises it to nothing.
!!
!! **The two loops are written to be identical**, down to the stride walk, and each returns a
!! checksum. *A run whose two checksums disagree is not a slow run, it is a wrong one* — the arms
!! did different work and the timings mean nothing. The program says so and exits nonzero.
!!
!! **Neither loop uses `mod` on a runtime divisor inside the inner loop.** That is an integer
!! division (~6 ns on x86-64), which would be most of a measurement of a ~5 ns comparator. CLAUDE.md
!! records a benchmark whose entire reported "floor" turned out to be exactly this.
!!
!! **A control arm is included** — a plain Fortran array sum over the same iteration count, which
!! neither comparator can influence. Its movement between two builds is a direct read on code
!! layout rather than on cost, which is what makes a cross-rebuild comparison interpretable at all.
!!
!! ## What it does NOT measure
!!
!! Not the sort. Not the marshalling the refactor deletes (`bench/benchmark_sort_engine.f90` covers
!! the end-to-end path). A comparator that wins here can still lose end to end, and the reverse.
!!
!! Usage: `bench/benchmark_sort_comparator.sh` (never a bare `fpm run` — see the wrapper).
program benchmark_sort_comparator
    use parquet
    use iso_fortran_env, only : int64, real64, output_unit
    use ieee_arithmetic, only : ieee_value, ieee_quiet_nan
    use iso_c_binding, only : c_ptr, c_loc, c_null_ptr, c_int8_t, c_char, c_long_long
    use parquet_bindings, only : parquet_sort_builder_new, parquet_sort_builder_free, &
        parquet_sort_builder_add_key_int64, parquet_sort_builder_add_key_double, &
        parquet_sort_builder_add_key_string
#ifdef _OPENMP
    use omp_lib, only : omp_get_wtime
#endif
    implicit none
    !
    interface
        function c_sweep_less(handle, nrows, nreps) &
                bind(C, name="parquet_debug_sort_sweep_less_cpp") result(r)
            import :: c_ptr, c_long_long
            type(c_ptr), value :: handle          !! the C++ builder handle.
            integer(c_long_long), value :: nrows  !! rows per pass.
            integer(c_long_long), value :: nreps  !! passes.
            integer(c_long_long) :: r             !! checksum, or -1 when unusable.
        end function c_sweep_less
        function c_sweep_compare(handle, nrows, nreps, nkeys) &
                bind(C, name="parquet_debug_sort_sweep_compare_cpp") result(r)
            import :: c_ptr, c_long_long
            type(c_ptr), value :: handle          !! the C++ builder handle.
            integer(c_long_long), value :: nrows  !! rows per pass.
            integer(c_long_long), value :: nreps  !! passes.
            integer(c_long_long), value :: nkeys  !! leading keys taking part.
            integer(c_long_long) :: r             !! checksum, or -1 when unusable.
        end function c_sweep_compare
    end interface
    !
    integer(int64) :: nrows = 4096_int64  !! rows per pass; small enough to stay in cache.
    integer(int64) :: nreps = 4096_int64  !! passes; nrows*nreps is the comparison count.
    integer :: rounds = 5                 !! timed rounds per arm; the minimum is reported.
    integer :: nfail = 0                  !! checksum mismatches seen.
    !
    call parse_args()
    call banner()
    call run_family("i64", 1)
    call run_family("f64", 1)
    call run_family("f64nan", 1)
    call run_family("str", 1)
    call run_family("multi3", 3)
    call run_control()
    !
    write(output_unit,'(a)') ""
    if (nfail > 0) then
        write(output_unit,'(a,i0,a)') "CHECKSUM MISMATCH on ", nfail, " arm(s) -- the two sweeps did &
            &NOT do the same work, so every timing above is meaningless. Do not report them."
        error stop 1
    end if
    write(output_unit,'(a)') "All arms agreed on their checksums."
    !
contains

    !> One fixture: build it for both engines, time both sweeps, print both rows.
    subroutine run_family(fam, nkeys)
        character(len=*), intent(in) :: fam !! i64 / f64 / f64nan / str / multi3.
        integer, intent(in) :: nkeys        !! engine keys the fixture holds.
        type(pf_sort_keys) :: keys
        type(c_ptr) :: builder
        integer(int64), allocatable, target :: k1(:)
        real(real64), allocatable, target :: k2(:)
        character(len=8), allocatable, target :: k3(:)
        integer(c_long_long), allocatable, target :: offs(:)
        character(kind=c_char), allocatable, target :: bytes(:)
        integer(c_int8_t), allocatable, target :: cvalid(:)
        logical, allocatable :: fvalid(:)
        integer(int64) :: fsum, csum
        integer :: j
        real(real64) :: tf, tc
        !
        call build_fixture(fam, k1, k2, k3, fvalid)
        allocate(cvalid(nrows))
        cvalid = merge(1_c_int8_t, 0_c_int8_t, fvalid)
        builder = parquet_sort_builder_new(int(nrows, c_long_long))
        !
        select case (fam)
        case ("i64")
            call keys%add(k1, is_valid=fvalid)
            call parquet_sort_builder_add_key_int64(builder, k1, c_loc(cvalid), 0_c_int8_t, 0_c_int8_t)
        case ("f64", "f64nan")
            call keys%add(k2, is_valid=fvalid)
            call parquet_sort_builder_add_key_double(builder, k2, c_loc(cvalid), 0_c_int8_t, 0_c_int8_t)
        case ("str")
            call pack_strings(k3, offs, bytes)
            call keys%add(k3, is_valid=fvalid)
            call parquet_sort_builder_add_key_string(builder, offs, bytes, c_loc(cvalid), &
                0_c_int8_t, 0_c_int8_t)
        case default
            call pack_strings(k3, offs, bytes)
            call keys%add(k1)
            call keys%add(k2, is_valid=fvalid)
            call keys%add(k3)
            call parquet_sort_builder_add_key_int64(builder, k1, c_null_ptr, 0_c_int8_t, 0_c_int8_t)
            call parquet_sort_builder_add_key_double(builder, k2, c_loc(cvalid), 0_c_int8_t, 0_c_int8_t)
            call parquet_sort_builder_add_key_string(builder, offs, bytes, c_null_ptr, &
                0_c_int8_t, 0_c_int8_t)
        end select
        !
        ! Warm both arms before timing: the first pass over a fresh fixture pays page faults that
        ! whichever arm runs first would otherwise absorb entirely.
        fsum = parquet_debug_sort_sweep_less(keys, nrows, 1_int64)
        csum = int(c_sweep_less(builder, int(nrows, c_long_long), 1_c_long_long), int64)
        !
        tf = huge(1.0_real64)
        tc = huge(1.0_real64)
        do j = 1, rounds
            tf = min(tf, time_fortran_less(keys, fsum))
            tc = min(tc, time_cpp_less(builder, csum))
        end do
        call report(fam // " row_less", tf, tc, fsum, csum)
        !
        tf = huge(1.0_real64)
        tc = huge(1.0_real64)
        do j = 1, rounds
            tf = min(tf, time_fortran_cmp(keys, nkeys, fsum))
            tc = min(tc, time_cpp_cmp(builder, nkeys, csum))
        end do
        call report(fam // " keys_compare", tf, tc, fsum, csum)
        !
        call parquet_sort_builder_free(builder)
    end subroutine run_family

    !> The control: a plain Fortran array sum, which no comparator change can reach.
    subroutine run_control()
        integer(int64), allocatable :: a(:)
        integer(int64) :: s, k, rep
        real(real64) :: t, t0
        integer :: j
        !
        allocate(a(nrows))
        do k = 1_int64, nrows
            a(k) = k * 2654435761_int64
        end do
        s = 0_int64
        t = huge(1.0_real64)
        do j = 1, rounds
            t0 = wtime()
            do rep = 1_int64, nreps
                do k = 1_int64, nrows
                    s = s + a(k)
                end do
            end do
            t = min(t, wtime() - t0)
        end do
        write(output_unit,'(a)') ""
        write(output_unit,'(a,f9.3,a,i0)') "control  array_sum      ns/iter: ", &
            t * 1.0e9_real64 / real(nrows * nreps, real64), "   checksum: ", s
        write(output_unit,'(a)') "  (the control cannot be affected by either comparator; its move &
            &between two BUILDS is layout noise, not cost)"
    end subroutine run_control

    !> Times one Fortran `sort_row_less` sweep.
    function time_fortran_less(keys, chk) result(t)
        type(pf_sort_keys), intent(in) :: keys !! the key set.
        integer(int64), intent(inout) :: chk   !! checksum, overwritten.
        real(real64) :: t                      !! seconds.
        real(real64) :: t0
        t0 = wtime()
        chk = parquet_debug_sort_sweep_less(keys, nrows, nreps)
        t = wtime() - t0
    end function time_fortran_less

    !> Times one C++ `SortRowLess` sweep.
    function time_cpp_less(builder, chk) result(t)
        type(c_ptr), intent(in) :: builder   !! the builder handle.
        integer(int64), intent(inout) :: chk !! checksum, overwritten.
        real(real64) :: t                    !! seconds.
        real(real64) :: t0
        t0 = wtime()
        chk = int(c_sweep_less(builder, int(nrows, c_long_long), int(nreps, c_long_long)), int64)
        t = wtime() - t0
    end function time_cpp_less

    !> Times one Fortran `sort_keys_compare` sweep.
    function time_fortran_cmp(keys, nkeys, chk) result(t)
        type(pf_sort_keys), intent(in) :: keys !! the key set.
        integer, intent(in) :: nkeys           !! leading keys.
        integer(int64), intent(inout) :: chk   !! checksum, overwritten.
        real(real64) :: t                      !! seconds.
        real(real64) :: t0
        t0 = wtime()
        chk = parquet_debug_sort_sweep_compare(keys, nrows, nreps, nkeys)
        t = wtime() - t0
    end function time_fortran_cmp

    !> Times one C++ `sort_keys_compare` sweep.
    function time_cpp_cmp(builder, nkeys, chk) result(t)
        type(c_ptr), intent(in) :: builder   !! the builder handle.
        integer, intent(in) :: nkeys         !! leading keys.
        integer(int64), intent(inout) :: chk !! checksum, overwritten.
        real(real64) :: t                    !! seconds.
        real(real64) :: t0
        t0 = wtime()
        chk = int(c_sweep_compare(builder, int(nrows, c_long_long), int(nreps, c_long_long), &
            int(nkeys, c_long_long)), int64)
        t = wtime() - t0
    end function time_cpp_cmp

    !> One result line, plus the checksum verdict that decides whether it may be quoted.
    subroutine report(label, tf, tc, fsum, csum)
        character(len=*), intent(in) :: label !! arm name.
        real(real64), intent(in) :: tf        !! best Fortran time, seconds.
        real(real64), intent(in) :: tc        !! best C++ time, seconds.
        integer(int64), intent(in) :: fsum    !! Fortran checksum.
        integer(int64), intent(in) :: csum    !! C++ checksum.
        real(real64) :: ncmp, nf, nc
        character(len=3) :: ok
        !
        ncmp = real(nrows * nreps, real64)
        nf = tf * 1.0e9_real64 / ncmp
        nc = tc * 1.0e9_real64 / ncmp
        ok = "ok "
        if (fsum /= csum) then
            ok = "BAD"
            nfail = nfail + 1
        end if
        write(output_unit,'(a,a20,a,f9.3,a,f9.3,a,f7.3,a,a,a,i0,a,i0)') &
            "  ", label, "   fortran ", nf, "   cpp ", nc, "   f/c ", nf / max(nc, 1.0e-12_real64), &
            "   checksum ", ok, "  ", fsum, " / ", csum
    end subroutine report

    !> Builds one fixture's key arrays, deterministically (xorshift64, identical on every compiler).
    subroutine build_fixture(fam, k1, k2, k3, fvalid)
        character(len=*), intent(in) :: fam                          !! fixture name.
        integer(int64), allocatable, intent(out) :: k1(:)            !! integer key.
        real(real64), allocatable, intent(out) :: k2(:)              !! real key.
        character(len=8), allocatable, intent(out) :: k3(:)          !! string key.
        logical, allocatable, intent(out) :: fvalid(:)               !! per-row validity.
        integer(int64) :: s, k, v
        integer :: j
        !
        allocate(k1(nrows), k2(nrows), k3(nrows), fvalid(nrows))
        s = 88172645463325252_int64
        do k = 1_int64, nrows
            s = ieor(s, ishft(s, 13))
            s = ieor(s, ishft(s, -7))
            s = ieor(s, ishft(s, 17))
            v = iand(s, 1048575_int64)
            ! Low cardinality on purpose: a comparator benchmark wants TIES, so that the key loop
            ! runs past its first key rather than deciding on the first byte every time.
            k1(k) = mod(v, 64_int64)
            ! SAME values as k1, deliberately. These two fixtures are subtracted from each other to
            ! isolate the cost of a real key over an integer one, so a different cardinality here
            ! would put a tie-rate difference into that subtraction as well. They differed (64
            ! against 128 distinct values) until 2026-08-14.
            k2(k) = real(k1(k), real64)
            do j = 1, 8
                k3(k)(j:j) = achar(97 + int(mod(v / int(j, int64) + int(j, int64), 26_int64)))
            end do
            ! ~3% null, and for f64nan ~3% NaN as well: enough that the tier arithmetic is exercised
            ! on real data rather than being predicted away by the branch predictor.
            fvalid(k) = (mod(v, 32_int64) /= 0_int64)
            if (fam == "f64nan" .and. mod(v, 31_int64) == 0_int64) then
                k2(k) = ieee_value(1.0_real64, ieee_quiet_nan)
            end if
        end do
    end subroutine build_fixture

    !> Packs a fixed-width string array into the offsets/data pair the C++ builder takes.
    subroutine pack_strings(vals, offs, bytes)
        character(len=*), intent(in) :: vals(:)                              !! the rows.
        integer(c_long_long), allocatable, intent(out) :: offs(:)            !! nrows+1 offsets.
        character(kind=c_char), allocatable, intent(out) :: bytes(:)         !! packed payload.
        integer(int64) :: w, k
        integer :: j
        !
        w = int(len(vals), int64)
        allocate(offs(nrows + 1_int64), bytes(nrows * w))
        do k = 1_int64, nrows + 1_int64
            offs(k) = int((k - 1_int64) * w, c_long_long)
        end do
        do k = 1_int64, nrows
            do j = 1, int(w)
                bytes((k - 1_int64) * w + j) = vals(k)(j:j)
            end do
        end do
    end subroutine pack_strings

    !> Wall clock. `omp_get_wtime` where OpenMP is present, `system_clock` otherwise.
    function wtime() result(t)
        real(real64) :: t !! seconds.
#ifndef _OPENMP
        integer(int64) :: c, r
#endif
#ifdef _OPENMP
        t = omp_get_wtime()
#else
        call system_clock(count=c, count_rate=r)
        t = real(c, real64) / real(r, real64)
#endif
    end function wtime

    !> `--rows=`, `--reps=`, `--rounds=`.
    subroutine parse_args()
        character(len=256) :: arg
        integer :: i, ios
        !
        do i = 1, command_argument_count()
            call get_command_argument(i, arg)
            if (arg(1:7) == "--rows=") then
                read(arg(8:), *, iostat=ios) nrows
            else if (arg(1:7) == "--reps=") then
                read(arg(8:), *, iostat=ios) nreps
            else if (arg(1:9) == "--rounds=") then
                read(arg(10:), *, iostat=ios) rounds
            end if
        end do
    end subroutine parse_args

    !> Provenance, printed before any number.
    subroutine banner()
        write(output_unit,'(a)') "=============================================================="
        write(output_unit,'(a)') "benchmark_sort_comparator -- Stage 1e"
        write(output_unit,'(a,i0,a,i0,a,i0)') "  rows: ", nrows, "   reps: ", nreps, &
            "   rounds (min reported): ", rounds
        write(output_unit,'(a,i0)') "  comparisons per timed sweep: ", nrows * nreps
#ifdef _OPENMP
        write(output_unit,'(a)') "  timer: omp_get_wtime"
#else
        write(output_unit,'(a)') "  timer: system_clock (no OpenMP)"
#endif
        write(output_unit,'(a)') "  arms are SERIAL by construction -- one comparator call at a time."
        write(output_unit,'(a)') "=============================================================="
        write(output_unit,'(a)') ""
        write(output_unit,'(a)') "  arm                        fortran(ns)      cpp(ns)     f/c   checksum"
    end subroutine banner

end program benchmark_sort_comparator
