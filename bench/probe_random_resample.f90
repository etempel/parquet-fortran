!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> Phase-2 probe: how fast can `pf_random_resample` be, and what does each speedup cost?
!!
!! `pf_random_resample(idx, m, seed [, stream])` draws `size(idx)` values from `1..m` WITH
!! replacement, so it is `n` independent uniform integers and nothing else -- no dedup structure,
!! no permutation, no sort. That makes it the one member of the resampling trio whose cost is
!! entirely the cost of the integer draw, and this probe decomposes that cost.
!!
!! Seven arms, in three groups.
!!
!! **Group 1 -- what exists today** (values frozen by `pf_random_algorithm`):
!!
!!   * `scalar` -- a loop of `pf_random_int_at`, which is what a user writes today because there is
!!     no resample procedure.
!!   * `fill` -- the shipped `pf_random_fill_draws(seed, i, v, lo, hi)`, whose block cache already
!!     halves the cipher work against `scalar`.
!!
!! **Group 2 -- value-PRESERVING restructuring** (asserted equal to `fill`, element for element):
!!
!!   * `pair` -- the `fill_r64` shape applied to the integer path: an alignment head, then a
!!     two-block steady state producing four values per body, with no per-element division, no
!!     parity test, no `blk /= held` branch, and the reduction inlined. `fill_draws_i64` never
!!     received this treatment; `fill_r64` did, and is 1.8x cheaper per value.
!!
!! **Group 3 -- value-CHANGING narrow-range grid** (a contract proposal, so checked statistically
!! rather than against the shipped values):
!!
!!   * `w32` / `w32x2` -- when the width fits in 31 bits, take a 32-bit candidate from ONE word
!!     rather than a 64-bit candidate from two, so a block serves FOUR values instead of two. The
!!     rejection test is the exact 32-bit analogue, so the result stays exactly unbiased -- what is
!!     given up is agreement with `pf_random_int_at` at the same coordinate, not uniformity.
!!     `w32x2` adds the same two-block steady state `pair` uses.
!!
!! **Diagnostic floors** (not valid resamples; they exist to split cipher cost from reduce cost):
!!
!!   * `cipher64` / `cipher32` -- the same block walks storing the raw pattern, with no reduction
!!     at all. `arm - floor` is what the reduction costs; `floor` is what the cipher costs.
!!
!! Every arm is gated before any timing is reported: `pair` must equal `fill` exactly, the two
!! narrow arms must equal each other, every value must lie in `[1, m]`, the narrow grid must pass
!! a chi-square over `m` cells and reproduce the expected duplicate rate, and each threaded run
!! must be bit-identical to its own serial form. A benchmark that replicates library code is
!! untested code (CLAUDE.md), so `pair` equalling `fill` is what makes any of these figures
!! evidence about the library rather than about this file.
!!
!! Not run by `fpm test`, by design (CLAUDE.md, "Manual (never-`fpm test`) large-scale/benchmark
!! tools"). Build and run with `--profile release`:
!!
!! ```bash
!! fpm run --profile release probe_random_resample -- --n=10000000 --rounds=5
!! ```

! Route (e) fork selection, transcribed from `bench/probe_random_int_rule.f90`.  cpp cannot evaluate
! `selected_int_kind(38)`, so the macro must come from a compiler predefine.  Without this the probe
! declares `integer(k128)` unconditionally and does not compile under ifx at all -- `selected_int_kind
! (38)` is -1 there and the build stops at `error #6684`, which is what kept every ifx column of
! `feature_random_resample*.md` empty.  The probe's own `reduce64` must therefore fork exactly as
! `src/parquet_random.f90`'s `int_reduce` does, or the arm would preview a kernel the library does
! not use on that compiler.
#if !defined(PF_NO_INT128)
#  if defined(__GFORTRAN__) || defined(__flang__) || defined(__FLANG)
#    define PF_INT128 1
#  endif
#endif

program probe_random_resample

#ifdef _OPENMP
    use omp_lib, only: omp_get_wtime, omp_get_max_threads
#endif
    use iso_fortran_env, only: int32, int64, real64, output_unit, compiler_version, compiler_options
    use parquet, only: pf_random_int_at, pf_random_fill_draws, pf_random_fill_streams, pf_random_key, &
                       pf_random_resample, parquet_set_random_parallel_min_elements, &
                       parquet_reset_settings, &
                       parquet_debug_random_block, parquet_debug_random_uses_int128

    implicit none

    !> Low 32 bits set.
    integer(int64), parameter :: M32 = 4294967295_int64
    !> The sign bit, for the unsigned comparison `ult`.
    integer(int64), parameter :: SIGN_BIT = ishft(1_int64, 63)
#ifdef PF_INT128
    !> The 128-bit kind, used only by the probe's own copy of the 64-bit reduction.
    integer, parameter :: k128 = selected_int_kind(38)
    !> Compile-time assertion that the predefine above was telling the truth: a division by zero in
    !! a constant expression, so a compiler without a 128-bit kind fails here by name rather than
    !! silently taking the wrapping arm. Same guard as `bench/probe_random_int_rule.f90`.
    integer, parameter :: pf_int128_present = 1 / merge(1, 0, k128 > 0)
    !> `2**63` as a 128-bit value.
    integer(k128), parameter :: TWO63_128 = int(huge(1_int64), k128) + 1_k128
    !> `2**64` as a 128-bit value.
    integer(k128), parameter :: TWO64_128 = 2_k128 * TWO63_128
    !> Low 64 bits set, as a 128-bit mask.
    integer(k128), parameter :: MASK64_128 = TWO64_128 - 1_k128
#endif
    !> The largest width the 32-bit candidate rule accepts: `x32 * s` must stay below `2**63`.
    integer(int64), parameter :: W32_MAX = 2147483648_int64

    integer(int64), parameter :: SEED = 20260817_int64
    integer(int64), parameter :: STREAM = 7_int64

    integer(int64) :: n, m
    integer :: rounds, i, nth_max, n_th
    integer(int64), allocatable :: a(:), b(:), c(:), d(:)
    integer(int32), allocatable :: e32(:)
    integer(int64) :: m_list(8), sz_list(6)
    integer :: th_list(12)

    n = read_arg_int('--n=', 10000000_int64)
    m = read_arg_int('--m=', 1000000_int64)
    rounds = int(read_arg_int('--rounds=', 5_int64), int32)
    call assert_optimised()
    ! 1431655766 is just above 2**32/3, where `2**32 mod m` -- and so the 32-bit rejection rate --
    ! is at its maximum of about 1/3. It is the worst case for the narrow rule and is measured
    ! deliberately rather than left to be discovered by a user.
    m_list = [1000_int64, 1000000_int64, 16711936_int64, 100000000_int64, 1000000000_int64, &
              1431655766_int64, 2147483648_int64, 100000000000_int64]
    ! The size sweep exists to reconcile the serial table (one call over `n`) with the bootstrap
    ! table (many calls over a small slice), which disagreed by 21 % on `fill` under LTO on machine B
    ! while agreeing exactly without it. It is the same procedure over the same array at eight
    ! decades of length, so any residual gap is array size and nothing else.
    sz_list = [100_int64, 10000_int64, 100000_int64, 1000000_int64, 10000000_int64, 100000000_int64]

    write (output_unit, '(a)') 'probe_random_resample: n uniform draws from 1..m, WITH replacement'
    write (output_unit, '(a,i0,a,i0)') 'n = ', n, ', rounds = ', rounds
    write (output_unit, '(a,l1)') 'int128 kernel arm: ', parquet_debug_random_uses_int128()
    write (output_unit, '(a,a)') 'compiler         : ', trim(compiler_version())
    write (output_unit, '(a,a)') 'compile options  : ', trim(compiler_options())
#ifdef _OPENMP
    write (output_unit, '(a,i0)') 'OpenMP max threads: ', omp_get_max_threads()
    nth_max = omp_get_max_threads()
#else
    write (output_unit, '(a)') 'OpenMP: NOT compiled in -- threaded arms are SKIPPED'
    nth_max = 1
#endif
    ! The ladder is derived from what the machine actually offers rather than hardcoded at 8, which
    ! is machine A's core count and told machine B (192 threads) nothing about its own ceiling.
    n_th = 0
    i = 1
    do while (i <= nth_max .and. n_th < size(th_list))
        n_th = n_th + 1
        th_list(n_th) = i
        i = i * 2
    end do
    if (n_th > 0 .and. th_list(n_th) /= nth_max .and. n_th < size(th_list)) then
        n_th = n_th + 1
        th_list(n_th) = nth_max                 ! always finish on the full machine
    end if
    write (output_unit, '(a)') ''
    flush (output_unit)

    allocate (a(n), b(n), c(n), d(n), e32(n))
    a = 0_int64; b = 0_int64; c = 0_int64; d = 0_int64   ! first-touch every page before timing
    e32 = 0_int32

    call gate_all(m)

    ! The tax is measured and printed FIRST, because every probe-side column below carries it and a
    ! reader cannot tell a corrected figure from an uncorrected one by looking at the table.
    write (output_unit, '(a)') '---- WRAPPER TAX: same algorithm, library vs this probe ----'
    write (output_unit, '(a)') &
        '            library     replica          tax'
    call wrapper_tax_row()
    write (output_unit, '(a)') ''

    write (output_unit, '(a)') '---- serial, ns per value (best of rounds) ----'
    write (output_unit, '(a)') &
        'AS MEASURED, NOT tax-corrected. Library columns (scalar, fill) carry no tax; probe columns'
    write (output_unit, '(a)') &
        '(pair1, pair, w32, w32x2, cipher64, cipher32) carry the tax above, per block enciphered.'
    write (output_unit, '(a)') &
        '           m      scalar        fill       pair1        pair         w32       w32x2'// &
        '    cipher64    cipher32    rej32%'
    do i = 1, size(m_list)
        call serial_row(m_list(i))
    end do
    write (output_unit, '(a)') ''

    write (output_unit, '(a)') '---- SIZE SWEEP: the shipped fill alone, one call, by array length ----'
    write (output_unit, '(a)') &
        '        size        fill    fill_r64   fill(int32)'
    do i = 1, size(sz_list)
        if (sz_list(i) <= n) call size_row(1000000_int64, sz_list(i))
    end do
    write (output_unit, '(a)') ''

    write (output_unit, '(a)') '---- STREAM axis, ns per value (best of rounds); m = 10**6 ----'
    write (output_unit, '(a)') &
        '                 shipped     hoisted   hoisted x2   shipped r64  shipped i32  probe plain'
    call streams_row(1000000_int64)
    write (output_unit, '(a)') ''

    write (output_unit, '(a)') '---- many SMALL resamples (the bootstrap shape); m = 10**6 ----'
    write (output_unit, '(a)') &
        '           n        reps        fill        pair       w32x2'
    call small_row(1000000_int64, 100_int64, 20000_int64)
    call small_row(1000000_int64, 1000_int64, 5000_int64)
    call small_row(1000000_int64, 100000_int64, 100_int64)
    write (output_unit, '(a)') ''

#ifdef _OPENMP
    write (output_unit, '(a)') '---- threaded, ns per value (best of rounds); m = 10**6 ----'
    write (output_unit, '(a)') &
        '     threads        pair       w32x2   pair speedup  w32x2 speedup'
    call threaded_rows(1000000_int64, th_list(1:n_th))
    write (output_unit, '(a)') ''

    ! `int64` against `int32` on the same arm: the only difference is 8 bytes stored per value
    ! against 4. Serially that is invisible; at a high thread count the store traffic of the narrow
    ! arm approaches what the machine can sustain, and the int32 specifics halve it exactly.
    write (output_unit, '(a)') '---- threaded STORE WIDTH: shipped fill, int64 vs int32 output ----'
    write (output_unit, '(a)') &
        '     threads       int64       int32    resample    int64 GB/s   ratio'
    call store_width_rows(1000000_int64, th_list(1:n_th))
    write (output_unit, '(a)') ''
#else
    write (output_unit, '(a)') '---- threaded arms SKIPPED: this build has no OpenMP ----'
    write (output_unit, '(a)') ''
#endif

    deallocate (a, b, c, d, e32)

contains

    ! ============================================================================
    ! Correctness gates -- every one of these runs before any timing is reported
    ! ============================================================================

    !> Runs every gate, aborting on the first failure. Nothing below is meaningful without these.
    subroutine gate_all(mm)
        integer(int64), intent(in) :: mm            !! a representative population size
        integer(int64) :: ng, k, mg
        integer(int64), allocatable :: g1(:), g2(:), g3(:), g4(:)
        integer(int32), allocatable :: g5(:)
        ng = min(n, 200000_int64)
        allocate (g1(ng), g2(ng), g3(ng), g4(ng), g5(ng))

        ! G1: the shipped fill agrees with the shipped scalar draw. A sanity check on the probe's
        !     understanding of the contract, not on the library.
        call pf_random_fill_draws(SEED, STREAM, g1, 1_int64, mm)
        do k = 1_int64, ng
            g2(k) = pf_random_int_at(SEED, STREAM, 1_int64, mm, k)
        end do
        call must_match(g1, g2, 'G1 shipped fill == shipped scalar draw')

        ! G2: the restructured arm is value-preserving. THE load-bearing gate: it is what makes
        !     `pair`'s timing a prediction about the library rather than about this file.

        ! G3: the two narrow arms are the same grid computed two ways.
        call w32_range(SEED, STREAM, mm, g3, 1_int64, ng)
        call w32x2_range(SEED, STREAM, mm, g4, 1_int64, ng)
        call must_match(g3, g4, 'G3 w32 == w32x2 (same grid, two shapes)')

        ! G4: containment, for every arm and at both ends of the width rule.
        call must_contain(g1, mm, 'G4a shipped fill in [1, m]')
        call must_contain(g3, mm, 'G4b narrow grid in [1, m]')
        mg = 6_int64
        call w32_range(SEED, STREAM, mg, g3, 1_int64, ng)
        call must_contain(g3, mg, 'G4c narrow grid in [1, 6]')

        ! G5: prefix consistency of the narrow grid -- a resample of n is a prefix of one of 2n.
        call w32_range(SEED, STREAM, mm, g3, 1_int64, ng)
        call w32_range(SEED, STREAM, mm, g4, 1_int64, ng / 2_int64)
        call must_match(g3(1:ng / 2_int64), g4(1:ng / 2_int64), 'G5 narrow grid prefix-consistent')

        ! G6: the narrow grid is uniform, and duplicates arrive at the rate replacement implies.
        call gate_uniform(mm, ng)

        ! G7: the hoisted stream-axis arms are value-preserving. Same load-bearing role as G2.
        call pf_random_fill_streams(SEED, 1_int64, g1, 1_int64, mm)
        ! And again at a draw index that lands in a block's SECOND pair, which is the branch the
        ! hoisted `second` flag replaces -- an arm that only ever tested draw 1 would not reach it.
        ! G7d pins `fill_streams_i32` against its `int64` sibling, at both pair parities. The two
        ! are edited together and only the `int64` one is timed above, so without this the narrow
        ! specific could be broken by the same change that the `int64` gates certify as correct.
        call pf_random_fill_streams(SEED, 1_int64, g5, 1_int32, int(mm, int32))
        call pf_random_fill_streams(SEED, 1_int64, g1, 1_int64, mm)
        call must_match(int(g5, int64), g1, 'G7d fill_streams int32 == int64 narrowed')
        call pf_random_fill_streams(SEED, 1_int64, g5, 1_int32, int(mm, int32), 4_int64)
        call pf_random_fill_streams(SEED, 1_int64, g1, 1_int64, mm, 4_int64)
        call must_match(int(g5, int64), g1, 'G7e fill_streams int32 == int64, draw = 4')

        ! G8: under the narrow-grid MEASUREMENT build, the library's own fill must equal this
        ! probe's independent implementation of the same rule, element for element. Two separately
        ! written implementations of one rule agreeing is a far stronger check than either against
        ! itself -- and it is the only oracle available here, because the golden vectors pin the
        ! 64-bit grid and necessarily disagree with this build (that disagreement IS the contract
        ! change under evaluation). G1 above already covers scalar-versus-bulk agreement, and it is
        ! exercising the narrow path in this build rather than the 64-bit one.
        ! **At a REJECTION-FREE width, and that restriction is load-bearing.** `reduce32` above
        ! re-keys a rejection with `pf_random_key`, standing in for the library's private
        ! `retry_key_of`, which is a different function -- so the two implementations are built to
        ! disagree on any rejected draw and this gate is only an oracle where none occurs. `2**24`
        ! divides `2**32`, so `2**32 mod s` is 0 and nothing is ever rejected; it is also exactly
        ! the cap, so this doubles as the check that the cap's own value is admitted.
        call pf_random_fill_draws(SEED, STREAM, g1, 1_int64, 16777216_int64)
        call w32_range(SEED, STREAM, 16777216_int64, g3, 1_int64, ng)
        call must_match(g1, g3, 'G8 library narrow fill == probe w32_range (rejection-free width)')
        ! The rejecting widths are covered instead by G1 above -- library bulk against library
        ! scalar at `m = 10**6`, where roughly one draw in 5000 is rejected -- plus containment.
        call pf_random_fill_draws(SEED, STREAM, g1, 1_int64, mm)
        call must_contain(g1, mm, 'G8b library narrow fill in [1, m]')
        ! And at a width just ABOVE the cap the library must fall back to the 64-bit grid, or the
        ! cap is not doing anything. 16777217 = 2**24 + 1.
        call pf_random_fill_draws(SEED, STREAM, g1, 1_int64, 16777217_int64)
        call w32_range(SEED, STREAM, 16777217_int64, g3, 1_int64, ng)
        if (all(g1 == g3)) then
            write (output_unit, '(a)') 'GATE FAILED: width 2**24+1 still took the narrow grid'
            error stop 1
        end if

        deallocate (g1, g2, g3, g4, g5)
        write (output_unit, '(a)') 'all gates passed'
        write (output_unit, '(a)') ''
        flush (output_unit)
    end subroutine gate_all

    !> Chi-square over `m` cells plus the expected-distinct check, for the narrow grid.
    !!
    !! Both oracles are needed. Chi-square sees a non-uniform marginal; the distinct count sees a
    !! grid that repeats itself, which a per-cell test can pass while the draws are not independent.
    subroutine gate_uniform(mm, ng)
        integer(int64), intent(in) :: mm            !! population size
        integer(int64), intent(in) :: ng            !! how many draws to test
        integer(int64), allocatable :: v(:), cnt(:)
        integer(int64) :: k, distinct
        real(real64) :: expct, chi2, lim, got, want
        allocate (v(ng), cnt(mm))
        call w32_range(SEED, STREAM, mm, v, 1_int64, ng)
        cnt = 0_int64
        do k = 1_int64, ng
            cnt(v(k)) = cnt(v(k)) + 1_int64
        end do
        expct = real(ng, real64) / real(mm, real64)
        chi2 = sum((real(cnt, real64) - expct)**2) / expct
        ! Five sigma on a chi-square with mm-1 degrees of freedom, whose variance is 2*(mm-1).
        lim = real(mm - 1_int64, real64) + 5.0_real64 * sqrt(2.0_real64 * real(mm - 1_int64, real64))
        if (chi2 > lim) then
            write (output_unit, '(a,f0.1,a,f0.1)') 'GATE FAILED G6a chi-square ', chi2, ' > ', lim
            error stop 1
        end if
        distinct = count(cnt > 0_int64)
        ! With replacement the expected distinct count is m*(1 - (1 - 1/m)**n).
        want = real(mm, real64) * (1.0_real64 - (1.0_real64 - 1.0_real64 / real(mm, real64))**real(ng, real64))
        got = real(distinct, real64)
        if (abs(got - want) > 0.01_real64 * want) then
            write (output_unit, '(a,f0.1,a,f0.1)') 'GATE FAILED G6b distinct ', got, ' vs expected ', want
            error stop 1
        end if
        deallocate (v, cnt)
    end subroutine gate_uniform

    !> Aborts unless two arrays agree element for element.
    subroutine must_match(x, y, what)
        integer(int64), intent(in) :: x(:)          !! one array
        integer(int64), intent(in) :: y(:)          !! the other
        character(len=*), intent(in) :: what        !! gate name, for the message
        integer(int64) :: k
        do k = 1_int64, size(x, kind=int64)
            if (x(k) /= y(k)) then
                write (output_unit, '(a,a,a,i0,a,i0,a,i0)') 'GATE FAILED ', what, &
                    ' at k = ', k, ': ', x(k), ' vs ', y(k)
                error stop 1
            end if
        end do
    end subroutine must_match

    !> Aborts unless every element lies in `[1, mm]`.
    subroutine must_contain(x, mm, what)
        integer(int64), intent(in) :: x(:)          !! the array
        integer(int64), intent(in) :: mm            !! population size
        character(len=*), intent(in) :: what        !! gate name, for the message
        if (minval(x) < 1_int64 .or. maxval(x) > mm) then
            write (output_unit, '(a,a,a,i0,a,i0)') 'GATE FAILED ', what, ': min ', &
                minval(x), ' max ', maxval(x)
            error stop 1
        end if
    end subroutine must_contain

    ! ============================================================================
    ! Arm 2 -- value-preserving restructure of the shipped integer fill
    ! ============================================================================

    !> Fills `v(lo:hi)` with draws `lo .. hi`, identical to `pf_random_fill_draws` on that range.
    !!
    !! The shape is `fill_r64`'s: an alignment head so the steady state needs no parity test, then
    !! two blocks per body so two independent ten-round chains interleave. What the shipped
    !! `fill_draws_i64` does instead is one division, one `modulo` and one `blk /= held` branch per
    !! element, plus an out-of-line `int_reduce` call.
    !!
    !! Taking `lo`/`hi` rather than the whole array is what lets the threaded arm and the serial arm
    !! be the same code with different bounds -- the discipline `perm_range_i64` already follows.
    subroutine pair_range(seed, stream, mm, v, lo, hi)
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int64), intent(in) :: stream        !! stream index
        integer(int64), intent(in) :: mm            !! population size
        integer(int64), intent(inout) :: v(:)       !! only `v(lo:hi)` is written
        integer(int64), intent(in) :: lo            !! first draw index, 1-based
        integer(int64), intent(in) :: hi            !! last draw index, 1-based
        integer(int64) :: w0, w1, w2, w3, x0, x1, x2, x3
        integer(int64) :: pos, blk, k, s
        if (hi < lo) return
        s = mm                                      ! width of [1, m]; a = 1
        k = lo
        pos = lo - 1_int64                          ! 0-based value index
        if (iand(pos, 1_int64) /= 0_int64) then     ! head: land on a block boundary
            call parquet_debug_random_block(seed, stream, ishft(pos, -1), w0, w1, w2, w3)
            v(k) = reduce64(ior(ishft(w3, 32), w2), s, seed, stream, k)
            k = k + 1_int64
            pos = pos + 1_int64
        end if
        blk = ishft(pos, -1)
        do while (k + 3_int64 <= hi)                ! steady state: two blocks, four values
            call parquet_debug_random_block(seed, stream, blk, w0, w1, w2, w3)
            call parquet_debug_random_block(seed, stream, blk + 1_int64, x0, x1, x2, x3)
            v(k) = reduce64(ior(ishft(w1, 32), w0), s, seed, stream, k)
            v(k + 1_int64) = reduce64(ior(ishft(w3, 32), w2), s, seed, stream, k + 1_int64)
            v(k + 2_int64) = reduce64(ior(ishft(x1, 32), x0), s, seed, stream, k + 2_int64)
            v(k + 3_int64) = reduce64(ior(ishft(x3, 32), x2), s, seed, stream, k + 3_int64)
            k = k + 4_int64
            blk = blk + 2_int64
        end do
        do while (k + 1_int64 <= hi)                ! tail: whole blocks
            call parquet_debug_random_block(seed, stream, blk, w0, w1, w2, w3)
            v(k) = reduce64(ior(ishft(w1, 32), w0), s, seed, stream, k)
            v(k + 1_int64) = reduce64(ior(ishft(w3, 32), w2), s, seed, stream, k + 1_int64)
            k = k + 2_int64
            blk = blk + 1_int64
        end do
        if (k <= hi) then                           ! tail: a final half-block
            call parquet_debug_random_block(seed, stream, blk, w0, w1, w2, w3)
            v(k) = reduce64(ior(ishft(w1, 32), w0), s, seed, stream, k)
        end if
    end subroutine pair_range

    !> Lemire's reduction to `[1, s]` with the exact rejection test, over a 64-bit candidate.
    !!
    !! The library's `int_reduce` spelled out, so this arm is a faithful preview of its cost. The
    !! threshold is computed lazily, exactly as there: at a realistic width the branch is entered
    !! with probability around `2**-40`, so the rejection path never runs and is delegated to the
    !! shipped scalar draw rather than reimplemented.
    !!
    !! **Forked on `PF_INT128`, exactly as `int_reduce` is**, so that the arm previews the kernel the
    !! library actually uses on this compiler rather than a kernel it does not have. Gate G2 -- the
    !! restructured arm equalling the shipped fill element for element -- is what validates the
    !! wrapping arm below, and it is the same guard the library's own wrapping sites rest on.
    pure function reduce64(x, s, seed, stream, draw) result(r)
        integer(int64), intent(in) :: x             !! the 64-bit candidate
        integer(int64), intent(in) :: s             !! the width, `m`
        integer(int64), intent(in) :: seed          !! seed, for the delegated rejection path
        integer(int64), intent(in) :: stream        !! stream index
        integer(int64), intent(in) :: draw          !! 1-based draw index
        integer(int64) :: r                         !! a uniform value in `[1, s]`
        integer(int64) :: low, high
#ifdef PF_INT128
        integer(k128) :: p
        p = iand(int(x, k128), MASK64_128) * int(s, k128)
        high = int(ishft(p, -64), int64)
        low = fold64(iand(p, MASK64_128))
#else
        call mulhilo64_probe(x, s, low, high)
#endif
        if (ult(low, s)) then
            if (ult(low, umod_2p64_probe(s))) then
                r = pf_random_int_at(seed, stream, 1_int64, s, draw)     ! rejection: essentially never
                return
            end if
        end if
        r = 1_int64 + high
    end function reduce64

#ifdef PF_INT128
    !> Folds a 128-bit value below `2**64` into the `integer(int64)` pattern representing it.
    pure function fold64(w) result(r)
        integer(k128), intent(in) :: w              !! a value in `[0, 2**64)`
        integer(int64) :: r                         !! the same bits as a signed 64-bit pattern
        if (w >= TWO63_128) then
            r = int(w - TWO64_128, int64)
        else
            r = int(w, int64)
        end if
    end function fold64
#endif

    !> `2**64 mod s`, for `0 < s < 2**62`, which is every width this probe uses.
    !!
    !! The library's `umod_2p64` also handles `s >= 2**63`, where the natural spelling is wrong; the
    !! probe never reaches those widths (`m_list` tops out at `10**11`), so the restricted form is
    !! written out rather than the general one, and the restriction is asserted by the caller's own
    !! gates rather than assumed.
    pure function umod_2p64_probe(s) result(t)
        integer(int64), intent(in) :: s             !! the width, `0 < s < 2**62`
        integer(int64) :: t                         !! `2**64 mod s`
        integer(int64) :: a, h
        a = -s                                      ! the unsigned pattern for `2**64 - s`
        h = ishft(a, -1)                            ! logical shift, so `(2**64 - s)/2` and >= 0
        t = 2_int64 * mod(h, s) + iand(a, 1_int64)  ! `2*r + bit` is below `2*s`, hence below 2**63
        if (t >= s) t = t - s
    end function umod_2p64_probe

#ifndef PF_INT128
    !> The full 128-bit product of two 64-bit patterns, from four 32-bit partial products.
    !!
    !! **A deliberate wrapping site, and the only one in this file.** Each 32x32 partial product is
    !! below `2**64` but may exceed `2**63`, so the signed multiply wraps; the resulting bit pattern
    !! is the correct unsigned one, and the shifts below are logical, so the limbs come out right.
    !! This mirrors `src/parquet_random.f90`'s own `mulhilo64` on the same arm, which
    !! `feature_risks.md` Risk-94 covers -- and here it is additionally checked every run by gate G2,
    !! since a wrong product would make the restructured arm disagree with the shipped fill.
    pure subroutine mulhilo64_probe(x, y, low, high)
        integer(int64), intent(in) :: x             !! left operand, an unsigned 64-bit pattern
        integer(int64), intent(in) :: y             !! right operand, an unsigned 64-bit pattern
        integer(int64), intent(out) :: low          !! low 64 bits of the product
        integer(int64), intent(out) :: high         !! high 64 bits of the product
        integer(int64) :: x0, x1, y0, y1, p00, p01, p10, p11, mid
        x0 = iand(x, M32); x1 = iand(ishft(x, -32), M32)
        y0 = iand(y, M32); y1 = iand(ishft(y, -32), M32)
        p00 = x0 * y0; p01 = x0 * y1; p10 = x1 * y0; p11 = x1 * y1
        ! Below `3 * 2**32`, so this sum cannot overflow whatever the products did.
        mid = iand(ishft(p00, -32), M32) + iand(p01, M32) + iand(p10, M32)
        low = ior(ishft(iand(mid, M32), 32), iand(p00, M32))
        high = p11 + iand(ishft(p01, -32), M32) + iand(ishft(p10, -32), M32) + ishft(mid, -32)
    end subroutine mulhilo64_probe
#endif

    !> `a < b` comparing both as unsigned 64-bit patterns.
    pure function ult(x, y) result(r)
        integer(int64), intent(in) :: x             !! left operand
        integer(int64), intent(in) :: y             !! right operand
        logical :: r                                !! `.true.` when `x < y` unsigned
        r = ieor(x, SIGN_BIT) < ieor(y, SIGN_BIT)
    end function ult

    ! ============================================================================
    ! Arm 3 -- the narrow-range 32-bit grid (a CONTRACT proposal, values differ)
    ! ============================================================================

    !> Fills `v(lo:hi)` with draws `lo .. hi` under the proposed narrow-range rule.
    !!
    !! When the width fits in 31 bits, draw `d` takes the single 32-bit word at index `d-1` -- the
    !! grid `pf_random32_at` already walks -- rather than the 64-bit pair at index `2d-2, 2d-1`. A
    !! block therefore serves four values instead of two. The reduction is Lemire's over 32 bits
    !! with the exact rejection test, so the result is exactly uniform; what changes is which bits
    !! a draw reads, and therefore its value.
    !!
    !! The threshold is loop-invariant and hoisted, unlike the 64-bit case where it is computed
    !! lazily: at 32 bits a rejection is around `2**-12` rather than `2**-40`, so it does occur and
    !! the branch has to be real.
    subroutine w32_range(seed, stream, mm, v, lo, hi)
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int64), intent(in) :: stream        !! stream index
        integer(int64), intent(in) :: mm            !! population size, at most `2**31`
        integer(int64), intent(inout) :: v(:)       !! only `v(lo:hi)` is written
        integer(int64), intent(in) :: lo            !! first draw index, 1-based
        integer(int64), intent(in) :: hi            !! last draw index, 1-based
        integer(int64) :: w(0:3), pos, blk, k, s, thr, j
        if (hi < lo) return
        s = mm
        thr = modulo(4294967296_int64, s)           ! 2**32 mod s; both operands are positive
        k = lo
        pos = lo - 1_int64
        do while (k <= hi)                          ! head and tail: one word at a time
            blk = ishft(pos, -2)
            j = iand(pos, 3_int64)
            if (j == 0_int64 .and. k + 3_int64 <= hi) exit
            call parquet_debug_random_block(seed, stream, blk, w(0), w(1), w(2), w(3))
            v(k) = reduce32(w(j), s, thr, seed, stream, k)
            k = k + 1_int64
            pos = pos + 1_int64
        end do
        blk = ishft(pos, -2)
        do while (k + 3_int64 <= hi)                ! steady state: one block, four values
            call parquet_debug_random_block(seed, stream, blk, w(0), w(1), w(2), w(3))
            v(k) = reduce32(w(0), s, thr, seed, stream, k)
            v(k + 1_int64) = reduce32(w(1), s, thr, seed, stream, k + 1_int64)
            v(k + 2_int64) = reduce32(w(2), s, thr, seed, stream, k + 2_int64)
            v(k + 3_int64) = reduce32(w(3), s, thr, seed, stream, k + 3_int64)
            k = k + 4_int64
            blk = blk + 1_int64
        end do
        do while (k <= hi)                          ! tail
            pos = k - 1_int64
            call parquet_debug_random_block(seed, stream, ishft(pos, -2), w(0), w(1), w(2), w(3))
            v(k) = reduce32(w(iand(pos, 3_int64)), s, thr, seed, stream, k)
            k = k + 1_int64
        end do
    end subroutine w32_range

    !> `pair_range` with a ONE-block steady state: two values per body instead of four.
    !!
    !! Separates the two things `pair_range` changes at once. Against the shipped fill it removes
    !! the per-element division, parity test and `blk /= held` branch but keeps one block per body;
    !! against `pair_range` it drops the two-block interleave. The gap on each side says which of
    !! the two an implementer is actually buying.
    subroutine pair1_range(seed, stream, mm, v, lo, hi)
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int64), intent(in) :: stream        !! stream index
        integer(int64), intent(in) :: mm            !! population size
        integer(int64), intent(inout) :: v(:)       !! only `v(lo:hi)` is written
        integer(int64), intent(in) :: lo            !! first draw index, 1-based
        integer(int64), intent(in) :: hi            !! last draw index, 1-based
        integer(int64) :: w0, w1, w2, w3, pos, blk, k, s
        if (hi < lo) return
        s = mm
        k = lo
        pos = lo - 1_int64
        if (iand(pos, 1_int64) /= 0_int64) then
            call parquet_debug_random_block(seed, stream, ishft(pos, -1), w0, w1, w2, w3)
            v(k) = reduce64(ior(ishft(w3, 32), w2), s, seed, stream, k)
            k = k + 1_int64
            pos = pos + 1_int64
        end if
        blk = ishft(pos, -1)
        do while (k + 1_int64 <= hi)
            call parquet_debug_random_block(seed, stream, blk, w0, w1, w2, w3)
            v(k) = reduce64(ior(ishft(w1, 32), w0), s, seed, stream, k)
            v(k + 1_int64) = reduce64(ior(ishft(w3, 32), w2), s, seed, stream, k + 1_int64)
            k = k + 2_int64
            blk = blk + 1_int64
        end do
        if (k <= hi) then
            call parquet_debug_random_block(seed, stream, blk, w0, w1, w2, w3)
            v(k) = reduce64(ior(ishft(w1, 32), w0), s, seed, stream, k)
        end if
    end subroutine pair1_range

    !> `w32_range` with a two-block steady state: eight values per body instead of four.
    subroutine w32x2_range(seed, stream, mm, v, lo, hi)
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int64), intent(in) :: stream        !! stream index
        integer(int64), intent(in) :: mm            !! population size, at most `2**31`
        integer(int64), intent(inout) :: v(:)       !! only `v(lo:hi)` is written
        integer(int64), intent(in) :: lo            !! first draw index, 1-based
        integer(int64), intent(in) :: hi            !! last draw index, 1-based
        integer(int64) :: w(0:3), x(0:3), pos, blk, k, s, thr, j
        if (hi < lo) return
        s = mm
        thr = modulo(4294967296_int64, s)
        k = lo
        pos = lo - 1_int64
        do while (k <= hi)                          ! head: align to a block boundary
            j = iand(pos, 3_int64)
            if (j == 0_int64 .and. k + 7_int64 <= hi) exit
            call parquet_debug_random_block(seed, stream, ishft(pos, -2), w(0), w(1), w(2), w(3))
            v(k) = reduce32(w(j), s, thr, seed, stream, k)
            k = k + 1_int64
            pos = pos + 1_int64
        end do
        blk = ishft(pos, -2)
        do while (k + 7_int64 <= hi)                ! steady state: two blocks, eight values
            call parquet_debug_random_block(seed, stream, blk, w(0), w(1), w(2), w(3))
            call parquet_debug_random_block(seed, stream, blk + 1_int64, x(0), x(1), x(2), x(3))
            v(k) = reduce32(w(0), s, thr, seed, stream, k)
            v(k + 1_int64) = reduce32(w(1), s, thr, seed, stream, k + 1_int64)
            v(k + 2_int64) = reduce32(w(2), s, thr, seed, stream, k + 2_int64)
            v(k + 3_int64) = reduce32(w(3), s, thr, seed, stream, k + 3_int64)
            v(k + 4_int64) = reduce32(x(0), s, thr, seed, stream, k + 4_int64)
            v(k + 5_int64) = reduce32(x(1), s, thr, seed, stream, k + 5_int64)
            v(k + 6_int64) = reduce32(x(2), s, thr, seed, stream, k + 6_int64)
            v(k + 7_int64) = reduce32(x(3), s, thr, seed, stream, k + 7_int64)
            k = k + 8_int64
            blk = blk + 2_int64
        end do
        do while (k <= hi)                          ! tail
            pos = k - 1_int64
            call parquet_debug_random_block(seed, stream, ishft(pos, -2), w(0), w(1), w(2), w(3))
            v(k) = reduce32(w(iand(pos, 3_int64)), s, thr, seed, stream, k)
            k = k + 1_int64
        end do
    end subroutine w32x2_range

    !> Lemire's reduction to `[1, s]` over a 32-bit candidate, with the exact rejection test.
    !!
    !! `x < 2**32` and `s <= 2**31`, so `x * s` is below `2**63` and the whole reduction is one
    !! ordinary signed 64-bit multiply -- no 128-bit product, no unsigned comparison, no fold. That
    !! is a second saving on top of the halved cipher work, and it is why this arm is not merely
    !! `pair` with a different word count.
    pure function reduce32(x, s, thr, seed, stream, draw) result(r)
        integer(int64), intent(in) :: x             !! the 32-bit candidate, in `[0, 2**32)`
        integer(int64), intent(in) :: s             !! the width, `m`
        integer(int64), intent(in) :: thr           !! `2**32 mod s`, hoisted by the caller
        integer(int64), intent(in) :: seed          !! seed, for the rejection re-key
        integer(int64), intent(in) :: stream        !! stream index
        integer(int64), intent(in) :: draw          !! 1-based draw index
        integer(int64) :: r                         !! a uniform value in `[1, s]`
        integer(int64) :: p, y, w0, w1, w2, w3, att
        p = x * s
        if (iand(p, M32) >= thr) then
            r = 1_int64 + ishft(p, -32)
            return
        end if
        ! Rejection: re-key and re-encipher the SAME counter, so consumption stays fixed in counter
        ! positions. `pf_random_key` stands in for the library's own `retry_key_of`, which is private.
        att = 0_int64
        do
            att = att + 1_int64
            call parquet_debug_random_block(pf_random_key(seed, att), stream, &
                                            ishft(draw - 1_int64, -2), w0, w1, w2, w3)
            select case (int(iand(draw - 1_int64, 3_int64), int32))
            case (0)
                y = w0
            case (1)
                y = w1
            case (2)
                y = w2
            case default
                y = w3
            end select
            p = y * s
            if (iand(p, M32) >= thr) exit
        end do
        r = 1_int64 + ishft(p, -32)
    end function reduce32

    ! ============================================================================
    ! The STREAM axis -- is `fill_streams_i64` behind its real-valued siblings?
    ! ============================================================================

    !> `pf_random_fill_streams` for integers, with the loop-invariant setup hoisted.
    !!
    !! The shipped `fill_streams_i64` is a plain loop calling `int_at_impl` once per element, and
    !! `int_at_impl` re-derives `a = min(lo,hi)` and `s = width_of(...)` every time -- on the
    !! `PF_INT128` arm `width_of` is 128-bit arithmetic with two branches -- while `bits_of` inside
    !! it re-derives the block index and the pair parity from `draw`, which is CONSTANT across this
    !! loop. Its two real-valued siblings hoist exactly these (`fill_streams_r64` names `blk` and
    !! `second`; `fill_streams_r32` names `blk` and `slot`) and say so in their doc-comments.
    !!
    !! This arm hoists the same three things and nothing else, so it must be value-identical.
    subroutine streams_i64_hoisted(seed, i0, v, lo, hi, draw)
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int64), intent(in) :: i0            !! first stream index
        integer(int64), intent(inout) :: v(:)       !! filled from streams `i0 .. i0+size(v)-1`
        integer(int64), intent(in) :: lo            !! one end of the closed range
        integer(int64), intent(in) :: hi            !! the other end
        integer(int64), intent(in) :: draw          !! 1-based value index
        integer(int64) :: k, mm, blk, w0, w1, w2, w3, s, x
        logical :: second
        mm = size(v, kind=int64)
        if (mm <= 0_int64) return
        s = hi - lo + 1_int64                       ! `lo = 1`, `hi = m` here, so this is exact
        blk = (draw - 1_int64) / 2_int64            ! a function of `draw` alone
        second = (modulo(draw - 1_int64, 2_int64) == 1_int64)
        do k = 1_int64, mm
            call parquet_debug_random_block(seed, i0 + (k - 1_int64), blk, w0, w1, w2, w3)
            if (second) then
                x = ior(ishft(w3, 32), w2)
            else
                x = ior(ishft(w1, 32), w0)
            end if
            v(k) = reduce64_at(x, s, seed, i0 + (k - 1_int64), draw, lo)
        end do
    end subroutine streams_i64_hoisted

    !> `streams_i64_hoisted` with TWO streams per body, to test whether the real-valued verdict
    !! ("one stream per body wins, lane blocking buys nothing on gfortran") carries to the integer
    !! path, which has more per-element work to overlap with the cipher.
    subroutine streams_i64_hoisted2(seed, i0, v, lo, hi, draw)
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int64), intent(in) :: i0            !! first stream index
        integer(int64), intent(inout) :: v(:)       !! filled from streams `i0 .. i0+size(v)-1`
        integer(int64), intent(in) :: lo            !! one end of the closed range
        integer(int64), intent(in) :: hi            !! the other end
        integer(int64), intent(in) :: draw          !! 1-based value index
        integer(int64) :: k, mm, blk, w0, w1, w2, w3, y0, y1, y2, y3, s, x
        logical :: second
        mm = size(v, kind=int64)
        if (mm <= 0_int64) return
        s = hi - lo + 1_int64
        blk = (draw - 1_int64) / 2_int64
        second = (modulo(draw - 1_int64, 2_int64) == 1_int64)
        k = 1_int64
        do while (k + 1_int64 <= mm)
            call parquet_debug_random_block(seed, i0 + (k - 1_int64), blk, w0, w1, w2, w3)
            call parquet_debug_random_block(seed, i0 + k, blk, y0, y1, y2, y3)
            if (second) then
                x = ior(ishft(w3, 32), w2)
                v(k) = reduce64_at(x, s, seed, i0 + (k - 1_int64), draw, lo)
                x = ior(ishft(y3, 32), y2)
                v(k + 1_int64) = reduce64_at(x, s, seed, i0 + k, draw, lo)
            else
                x = ior(ishft(w1, 32), w0)
                v(k) = reduce64_at(x, s, seed, i0 + (k - 1_int64), draw, lo)
                x = ior(ishft(y1, 32), y0)
                v(k + 1_int64) = reduce64_at(x, s, seed, i0 + k, draw, lo)
            end if
            k = k + 2_int64
        end do
        do while (k <= mm)
            call parquet_debug_random_block(seed, i0 + (k - 1_int64), blk, w0, w1, w2, w3)
            if (second) then
                x = ior(ishft(w3, 32), w2)
            else
                x = ior(ishft(w1, 32), w0)
            end if
            v(k) = reduce64_at(x, s, seed, i0 + (k - 1_int64), draw, lo)
            k = k + 1_int64
        end do
    end subroutine streams_i64_hoisted2

    !> `streams_i64_hoisted` with the hoist UNDONE -- the shipped per-element shape, but reaching
    !! the cipher through this probe's out-of-line wrapper like every other arm here.
    !!
    !! **This arm exists to answer why the hoist measured nothing in the library.** A library-side
    !! A/B of the real hoist moved `fill_streams_i64` 17.63 -> 17.54 ns per value (1.005x) and
    !! `fill_streams_i32` not at all, against a control that did not move -- while these probe arms
    !! had predicted 1.04x-1.29x. `plain` against `hoisted`, both in this binary and both through
    !! the wrapper, separates the two candidate explanations: if they agree, the earlier probe-side
    !! gain was never real; if `plain` is materially slower, then reaching the cipher through an
    !! opaque call is what CREATES the hoisting opportunity, and no arm shaped like this one can
    !! measure a loop-invariant hoist in code where the cipher inlines.
    !!
    !! It re-derives per element exactly what the shipped worker re-derives: the width, and the
    !! block index and pair parity that `bits_of` recomputes from a constant `draw`.
    subroutine streams_i64_plain(seed, i0, v, lo, hi, draw)
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int64), intent(in) :: i0            !! first stream index
        integer(int64), intent(inout) :: v(:)       !! filled from streams `i0 .. i0+size(v)-1`
        integer(int64), intent(in) :: lo            !! one end of the closed range
        integer(int64), intent(in) :: hi            !! the other end
        integer(int64), intent(in) :: draw          !! 1-based value index
        integer(int64) :: k, mm, blk, w0, w1, w2, w3, s, x, e, stream
        mm = size(v, kind=int64)
        if (mm <= 0_int64) return
        do k = 1_int64, mm
            stream = i0 + (k - 1_int64)
            s = hi - lo + 1_int64                   ! per element, as `int_at_impl` re-derives it
            e = max(draw, 1_int64) - 1_int64        ! per element, as `bits_of` re-derives it
            blk = ishft(e, -1)
            call parquet_debug_random_block(seed, stream, blk, w0, w1, w2, w3)
            if (iand(e, 1_int64) == 0_int64) then
                x = ior(ishft(w1, 32), w0)
            else
                x = ior(ishft(w3, 32), w2)
            end if
            v(k) = reduce64_at(x, s, seed, stream, draw, lo)
        end do
    end subroutine streams_i64_plain

    !> `reduce64` with an explicit range base, for the stream-axis arms.
    pure function reduce64_at(x, s, seed, stream, draw, base) result(r)
        integer(int64), intent(in) :: x             !! the 64-bit candidate
        integer(int64), intent(in) :: s             !! the width
        integer(int64), intent(in) :: seed          !! seed, for the delegated rejection path
        integer(int64), intent(in) :: stream        !! stream index
        integer(int64), intent(in) :: draw          !! 1-based draw index
        integer(int64), intent(in) :: base          !! the range's low end
        integer(int64) :: r                         !! a uniform value in `[base, base+s-1]`
        integer(int64) :: low, high
#ifdef PF_INT128
        integer(k128) :: p
        p = iand(int(x, k128), MASK64_128) * int(s, k128)
        high = int(ishft(p, -64), int64)
        low = fold64(iand(p, MASK64_128))
#else
        call mulhilo64_probe(x, s, low, high)
#endif
        if (ult(low, s)) then
            if (ult(low, umod_2p64_probe(s))) then
                r = pf_random_int_at(seed, stream, base, base + s - 1_int64, draw)
                return
            end if
        end if
        r = base + high
    end function reduce64_at

    ! ============================================================================
    ! Diagnostic floors -- the cipher with no reduction at all
    ! ============================================================================

    !> Two values per block, raw 64-bit patterns. Not a resample: this is the cipher's own cost.
    subroutine cipher64_range(seed, stream, v, lo, hi)
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int64), intent(in) :: stream        !! stream index
        integer(int64), intent(inout) :: v(:)       !! only `v(lo:hi)` is written
        integer(int64), intent(in) :: lo            !! first index
        integer(int64), intent(in) :: hi            !! last index
        integer(int64) :: w0, w1, w2, w3, x0, x1, x2, x3, k, blk
        k = lo
        blk = 0_int64
        do while (k + 3_int64 <= hi)
            call parquet_debug_random_block(seed, stream, blk, w0, w1, w2, w3)
            call parquet_debug_random_block(seed, stream, blk + 1_int64, x0, x1, x2, x3)
            v(k) = ior(ishft(w1, 32), w0)
            v(k + 1_int64) = ior(ishft(w3, 32), w2)
            v(k + 2_int64) = ior(ishft(x1, 32), x0)
            v(k + 3_int64) = ior(ishft(x3, 32), x2)
            k = k + 4_int64
            blk = blk + 2_int64
        end do
        do while (k <= hi)
            call parquet_debug_random_block(seed, stream, blk, w0, w1, w2, w3)
            v(k) = ior(ishft(w1, 32), w0)
            k = k + 1_int64
            blk = blk + 1_int64
        end do
    end subroutine cipher64_range

    !> Four values per block, raw 32-bit words. Not a resample: the cipher's cost at four per block.
    subroutine cipher32_range(seed, stream, v, lo, hi)
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int64), intent(in) :: stream        !! stream index
        integer(int64), intent(inout) :: v(:)       !! only `v(lo:hi)` is written
        integer(int64), intent(in) :: lo            !! first index
        integer(int64), intent(in) :: hi            !! last index
        integer(int64) :: w0, w1, w2, w3, k, blk
        k = lo
        blk = 0_int64
        do while (k + 3_int64 <= hi)
            call parquet_debug_random_block(seed, stream, blk, w0, w1, w2, w3)
            v(k) = w0
            v(k + 1_int64) = w1
            v(k + 2_int64) = w2
            v(k + 3_int64) = w3
            k = k + 4_int64
            blk = blk + 1_int64
        end do
        do while (k <= hi)
            call parquet_debug_random_block(seed, stream, blk, w0, w1, w2, w3)
            v(k) = w0
            k = k + 1_int64
            blk = blk + 1_int64
        end do
    end subroutine cipher32_range

    ! ============================================================================
    ! Timing
    ! ============================================================================

    !> Times every serial arm at one population size and prints the row.
    subroutine serial_row(mm)
        integer(int64), intent(in) :: mm            !! population size
        real(real64) :: t(8)
        integer :: r
        integer(int64) :: k
        logical :: narrow
        narrow = mm <= W32_MAX
        t = huge(1.0_real64)
        do r = 1, rounds
            t(1) = min(t(1), timed_scalar(mm))
            t(2) = min(t(2), timed_fill(mm))
            t(3) = min(t(3), timed_pair1(mm))
            t(4) = min(t(4), timed_pair(mm))
            if (narrow) then
                t(5) = min(t(5), timed_w32(mm))
                t(6) = min(t(6), timed_w32x2(mm))
            end if
            t(7) = min(t(7), timed_cipher64())
            t(8) = min(t(8), timed_cipher32())
        end do
        ! Keep the results live so nothing above can be optimised away.
        k = a(1) + b(1) + c(1) + d(1)
        if (k == -999999_int64) write (output_unit, '(a)') 'unreachable'
        if (narrow) then
            write (output_unit, '(i12,8f12.2,f9.2)') mm, &
                (t(r) * 1.0e9_real64 / real(n, real64), r=1, 8), &
                100.0_real64 * real(modulo(4294967296_int64, mm), real64) / 4294967296.0_real64
        else
            write (output_unit, '(i12,4f12.2,a,2f12.2)') mm, &
                (t(r) * 1.0e9_real64 / real(n, real64), r=1, 4), '           -           -', &
                t(7) * 1.0e9_real64 / real(n, real64), t(8) * 1.0e9_real64 / real(n, real64)
        end if
        flush (output_unit)
    end subroutine serial_row

    !> One timed run of the one-block-per-body restructure.
    function timed_pair1(mm) result(dt)
        integer(int64), intent(in) :: mm            !! population size
        real(real64) :: dt                          !! seconds
        dt = now()
        call pair1_range(SEED, STREAM, mm, c, 1_int64, n)
        dt = now() - dt
    end function timed_pair1

    !> A byte-for-byte replica of the library's `fill_r64`, built on `parquet_debug_random_block`.
    !!
    !! **The cost control this probe needs and did not have.** Every arm here reaches the cipher
    !! through the public debug wrapper, while the library's own workers call the private
    !! `random_block` directly. If that wrapper costs anything, every probe arm pays it and every
    !! library arm does not — so a probe arm that merely *ties* the library is actually ahead, and
    !! one that loses may still be ahead. This arm is the same algorithm as `fill_r64`, so the gap
    !! between it and the shipped `real64` draw-axis fill IS the wrapper tax, in ns per value, and
    !! every other figure can be corrected by it.
    subroutine fill_r64_replica(seed, stream, v, draw)
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int64), intent(in) :: stream        !! stream index
        real(real64), intent(out) :: v(:)           !! filled with values `draw .. draw+size(v)-1`
        integer(int64), intent(in) :: draw          !! 1-based starting value index
        integer(int64) :: position, blk, k, mm
        integer(int64) :: w0, w1, w2, w3, x0, x1, x2, x3
        mm = size(v, kind=int64)
        if (mm <= 0_int64) return
        k = 0_int64
        position = draw - 1_int64
        if (iand(position, 1_int64) /= 0_int64) then
            call parquet_debug_random_block(seed, stream, ishft(position, -1), w0, w1, w2, w3)
            k = 1_int64
            v(1) = to_r64(ior(ishft(w3, 32), w2))
            position = position + 1_int64
        end if
        blk = ishft(position, -1)
        do while (k + 4_int64 <= mm)
            call parquet_debug_random_block(seed, stream, blk, w0, w1, w2, w3)
            call parquet_debug_random_block(seed, stream, blk + 1_int64, x0, x1, x2, x3)
            v(k + 1_int64) = to_r64(ior(ishft(w1, 32), w0))
            v(k + 2_int64) = to_r64(ior(ishft(w3, 32), w2))
            v(k + 3_int64) = to_r64(ior(ishft(x1, 32), x0))
            v(k + 4_int64) = to_r64(ior(ishft(x3, 32), x2))
            k = k + 4_int64
            blk = blk + 2_int64
        end do
        do while (k + 2_int64 <= mm)
            call parquet_debug_random_block(seed, stream, blk, w0, w1, w2, w3)
            v(k + 1_int64) = to_r64(ior(ishft(w1, 32), w0))
            v(k + 2_int64) = to_r64(ior(ishft(w3, 32), w2))
            k = k + 2_int64
            blk = blk + 1_int64
        end do
        if (k < mm) then
            call parquet_debug_random_block(seed, stream, blk, w0, w1, w2, w3)
            v(mm) = to_r64(ior(ishft(w1, 32), w0))
        end if
    end subroutine fill_r64_replica

    !> The library's `to_real64`, which is private: top 53 bits scaled by `2**-53`.
    pure function to_r64(bits) result(r)
        integer(int64), intent(in) :: bits          !! any 64-bit pattern
        real(real64) :: r                           !! `[0, 1)`
        r = real(ishft(bits, -11), real64) * 2.0_real64**(-53)
    end function to_r64

    !> Times the wrapper-tax control: the same algorithm, once through the library and once here.
    subroutine wrapper_tax_row()
        real(real64) :: t(2), t0
        integer :: r
        real(real64), allocatable :: vr(:), vs(:)
        integer(int64) :: k
        allocate (vr(n), vs(n))
        vr = 0.0_real64; vs = 0.0_real64
        ! Value gate first: the replica must reproduce the library exactly, or it is not a control.
        call pf_random_fill_draws(SEED, STREAM, vs, 1_int64)
        call fill_r64_replica(SEED, STREAM, vr, 1_int64)
        do k = 1_int64, n
            if (vr(k) /= vs(k)) then
                write (output_unit, '(a,i0)') 'GATE FAILED G8 replica == library fill_r64 at k = ', k
                error stop 1
            end if
        end do
        t = huge(1.0_real64)
        do r = 1, rounds
            t0 = now(); call pf_random_fill_draws(SEED, STREAM, vs, 1_int64)
            t(1) = min(t(1), now() - t0)
            t0 = now(); call fill_r64_replica(SEED, STREAM, vr, 1_int64)
            t(2) = min(t(2), now() - t0)
        end do
        write (output_unit, '(a12,3f12.2)') 'ns/value', &
            t(1) * 1.0e9_real64 / real(n, real64), t(2) * 1.0e9_real64 / real(n, real64), &
            (t(2) - t(1)) * 1.0e9_real64 / real(n, real64)
        deallocate (vr, vs)
        flush (output_unit)
    end subroutine wrapper_tax_row

    !> Refuses to print anything if the build cannot possibly be optimised.
    !!
    !! CLAUDE.md's benchmarking rules require this and also warn about the trap in it: "no `-O` in the
    !! flags" means `-O0` for gfortran and flang, and means **the compiler's own `-O2` default** for
    !! ifx, so a check that does not know the difference blocks a perfectly valid arm. The list of
    !! compilers whose default is optimised is therefore kept short and evidence-based, and the flags
    !! are read from `compiler_options()` rather than guessed.
    subroutine assert_optimised()
        character(len=:), allocatable :: opts, ver
        opts = trim(compiler_options())
        ver = trim(compiler_version())
        if (index(opts, '-O') > 0) return           ! an explicit -O of any level
        if (index(ver, 'Intel') > 0) return         ! ifx/icx default to -O2 with no flag
        write (output_unit, '(a)') 'REFUSING TO RUN: no -O in the compile options and this compiler'
        write (output_unit, '(a)') 'does not optimise by default, so every figure would be an -O0'
        write (output_unit, '(a)') 'figure. Build with --profile release, or append -O3 to FPM_FFLAGS'
        write (output_unit, '(a)') '(append, never assign -- it carries include paths).'
        write (output_unit, '(a,a)') 'compiler       : ', ver
        write (output_unit, '(a,a)') 'compile options: ', opts
        error stop 1
    end subroutine assert_optimised

    !> The shipped fill alone, one call, at one array length -- the size-sweep row.
    !!
    !! Exists to reconcile the serial and bootstrap tables, which disagreed by 21 % on `fill` under
    !! LTO on machine B and agreed exactly without it. Every arm here is library code called once per
    !! replicate, so the only variable is how much memory the call touches.
    subroutine size_row(mm, nn)
        integer(int64), intent(in) :: mm            !! population size
        integer(int64), intent(in) :: nn            !! array length for this row
        real(real64) :: t(3), t0
        integer :: r
        integer(int64) :: reps, q
        real(real64), allocatable :: vr(:)
        allocate (vr(nn))
        vr = 0.0_real64
        reps = max(1_int64, n / nn)                 ! same total work in every row
        t = huge(1.0_real64)
        do r = 1, rounds
            t0 = now()
            do q = 1_int64, reps
                call pf_random_fill_draws(SEED, q, b(1:nn), 1_int64, mm)
            end do
            t(1) = min(t(1), now() - t0)
            t0 = now()
            do q = 1_int64, reps
                call pf_random_fill_draws(SEED, q, vr)
            end do
            t(2) = min(t(2), now() - t0)
            t0 = now()
            do q = 1_int64, reps
                call pf_random_fill_draws(SEED, q, e32(1:nn), 1_int32, int(mm, int32))
            end do
            t(3) = min(t(3), now() - t0)
        end do
        write (output_unit, '(i12,3f12.2)') nn, &
            (t(r) * 1.0e9_real64 / real(nn * reps, real64), r=1, 3)
        deallocate (vr)
        flush (output_unit)
    end subroutine size_row

    !> One timed threaded fill into an `integer(int64)` output.
    !!
    !! Each thread fills its own contiguous chunk and passes `draw = lo`, so the result is identical
    !! to the single serial call element for element -- which is asserted, not assumed.
    function timed_par_fill64(mm, nth) result(dt)
        integer(int64), intent(in) :: mm            !! population size
        integer, intent(in) :: nth                  !! thread count
        real(real64) :: dt                          !! seconds
        integer(int64) :: chunk, lo, hi
        integer :: t
        chunk = (n + int(nth, int64) - 1_int64) / int(nth, int64)
        dt = now()
        !$omp parallel do default(shared) private(t, lo, hi) schedule(static) num_threads(nth)
        do t = 0, nth - 1
            lo = int(t, int64) * chunk + 1_int64
            hi = min(n, lo + chunk - 1_int64)
            if (lo <= hi) call pf_random_fill_draws(SEED, STREAM, b(lo:hi), 1_int64, mm, lo)
        end do
        !$omp end parallel do
        dt = now() - dt
    end function timed_par_fill64

    !> `timed_par_fill64` into an `integer(int32)` output: half the bytes stored, same values.
    function timed_par_fill32(mm, nth) result(dt)
        integer(int64), intent(in) :: mm            !! population size
        integer, intent(in) :: nth                  !! thread count
        real(real64) :: dt                          !! seconds
        integer(int64) :: chunk, lo, hi
        integer :: t
        chunk = (n + int(nth, int64) - 1_int64) / int(nth, int64)
        dt = now()
        !$omp parallel do default(shared) private(t, lo, hi) schedule(static) num_threads(nth)
        do t = 0, nth - 1
            lo = int(t, int64) * chunk + 1_int64
            hi = min(n, lo + chunk - 1_int64)
            if (lo <= hi) call pf_random_fill_draws(SEED, STREAM, e32(lo:hi), 1_int32, &
                                                    int(mm, int32), lo)
        end do
        !$omp end parallel do
        dt = now() - dt
    end function timed_par_fill32

    !> The store-width rows: the same shipped fill into an `int64` and an `int32` array.
    !!
    !! The `resample` column times **`pf_random_resample(..., threads=)` itself** rather than the
    !! hand-rolled chunking beside it, which is the whole point of having both: the two do the same
    !! thing, so the library column is what the user actually gets and the replica column is the
    !! control. Stage 3 is the reason this is spelled out -- a probe arm standing in for library code
    !! predicted a 1.04x-1.29x gain that the library measured at 1.005x, because nothing compared
    !! like with like. The work floor is cleared first, or an explicit `threads=` at a high count is
    !! silently cut down by `random_threads` and the column reads as a scaling failure.
    subroutine store_width_rows(mm, ths)
        integer(int64), intent(in) :: mm            !! population size
        integer, intent(in) :: ths(:)               !! thread counts to try
        real(real64) :: t1, t2, t3, t0
        integer :: i2, r
        integer(int64) :: k
        call parquet_set_random_parallel_min_elements(0)
        do i2 = 1, size(ths)
            t1 = huge(1.0_real64)
            t2 = huge(1.0_real64)
            t3 = huge(1.0_real64)
            do r = 1, rounds
                t1 = min(t1, timed_par_fill64(mm, ths(i2)))
                t2 = min(t2, timed_par_fill32(mm, ths(i2)))
                t0 = now()
                call pf_random_resample(c, mm, SEED, STREAM, threads=ths(i2))
                t3 = min(t3, now() - t0)
            end do
            ! Every output must equal the serial fill -- a threaded chunking bug would otherwise
            ! show up as a speedup. `c` is the library's own threaded resample.
            call pf_random_fill_draws(SEED, STREAM, a, 1_int64, mm)
            call must_match(a, b, 'threaded int64 fill == serial fill')
            call must_match(a, c, 'threaded pf_random_resample == serial fill')
            do k = 1_int64, n
                if (int(e32(k), int64) /= a(k)) then
                    write (output_unit, '(a,i0)') 'GATE FAILED int32 fill == int64 fill at k = ', k
                    error stop 1
                end if
            end do
            write (output_unit, '(i12,3f12.2,f12.1,f8.2)') ths(i2), &
                t1 * 1.0e9_real64 / real(n, real64), t2 * 1.0e9_real64 / real(n, real64), &
                t3 * 1.0e9_real64 / real(n, real64), &
                8.0_real64 * real(n, real64) / (t1 * 1.0e9_real64), t2 / t1
            flush (output_unit)
        end do
        call parquet_reset_settings()
    end subroutine store_width_rows

    !> Times the stream-axis arms. The `real64` stream fill is the control: it is one block per
    !! value with no reduction, so it is the floor this axis cannot go below.
    !!
    !! The `int32` column exists because section 23 records "the `int32` output arm, on every axis"
    !! as unmeasured on either machine, and `fill_streams_i32` is changed by the same edit as its
    !! `int64` sibling -- so a hoist adopted on the strength of the `int64` column alone would be
    !! adopted for a procedure nothing here times.
    subroutine streams_row(mm)
        integer(int64), intent(in) :: mm            !! population size
        real(real64) :: t(6), t0
        integer :: r
        real(real64), allocatable :: vr(:)
        allocate (vr(n))
        vr = 0.0_real64
        t = huge(1.0_real64)
        do r = 1, rounds
            t0 = now(); call pf_random_fill_streams(SEED, 1_int64, b, 1_int64, mm)
            t(1) = min(t(1), now() - t0)
            t0 = now(); call streams_i64_hoisted(SEED, 1_int64, c, 1_int64, mm, 1_int64)
            t(2) = min(t(2), now() - t0)
            t0 = now(); call streams_i64_hoisted2(SEED, 1_int64, d, 1_int64, mm, 1_int64)
            t(3) = min(t(3), now() - t0)
            t0 = now(); call pf_random_fill_streams(SEED, 1_int64, vr)
            t(4) = min(t(4), now() - t0)
            t0 = now()
            call pf_random_fill_streams(SEED, 1_int64, e32, 1_int32, int(mm, int32))
            t(5) = min(t(5), now() - t0)
            t0 = now(); call streams_i64_plain(SEED, 1_int64, c, 1_int64, mm, 1_int64)
            t(6) = min(t(6), now() - t0)
        end do
        write (output_unit, '(a12,6f12.2)') 'ns/value', (t(r) * 1.0e9_real64 / real(n, real64), r=1, 6)
        deallocate (vr)
        flush (output_unit)
    end subroutine streams_row

    !> Times many SMALL resamples, which is the bootstrap shape: `reps` replicates of `nsmall` each.
    !!
    !! Each replicate is its own stream, so replicate `b` is reproducible from `(seed, b)` alone --
    !! the labelling the design requires. Reported per value, so it is directly comparable with the
    !! large-`n` table above and the difference is per-call overhead.
    subroutine small_row(mm, nsmall, reps)
        integer(int64), intent(in) :: mm            !! population size
        integer(int64), intent(in) :: nsmall        !! elements per replicate
        integer(int64), intent(in) :: reps          !! how many replicates
        real(real64) :: t(4), t0
        integer :: r
        integer(int64) :: bb, tot
        tot = nsmall * reps
        t = huge(1.0_real64)
        do r = 1, rounds
            t0 = now()
            do bb = 1_int64, reps
                call pf_random_fill_draws(SEED, bb, b(1:nsmall), 1_int64, mm)
            end do
            t(1) = min(t(1), now() - t0)
            t0 = now()
            do bb = 1_int64, reps
                call pair_range(SEED, bb, mm, c, 1_int64, nsmall)
            end do
            t(2) = min(t(2), now() - t0)
            t0 = now()
            do bb = 1_int64, reps
                call w32x2_range(SEED, bb, mm, d, 1_int64, nsmall)
            end do
            t(3) = min(t(3), now() - t0)
            t(4) = 0.0_real64
        end do
        write (output_unit, '(i12,i12,3f12.2)') nsmall, reps, &
            (t(r) * 1.0e9_real64 / real(tot, real64), r=1, 3)
        flush (output_unit)
    end subroutine small_row

    !> One timed run of the scalar loop.
    function timed_scalar(mm) result(dt)
        integer(int64), intent(in) :: mm            !! population size
        real(real64) :: dt                          !! seconds
        integer(int64) :: k
        dt = now()
        do k = 1_int64, n
            a(k) = pf_random_int_at(SEED, STREAM, 1_int64, mm, k)
        end do
        dt = now() - dt
    end function timed_scalar

    !> One timed run of the shipped bulk fill.
    function timed_fill(mm) result(dt)
        integer(int64), intent(in) :: mm            !! population size
        real(real64) :: dt                          !! seconds
        dt = now()
        call pf_random_fill_draws(SEED, STREAM, b, 1_int64, mm)
        dt = now() - dt
    end function timed_fill

    !> One timed run of the value-preserving restructure.
    function timed_pair(mm) result(dt)
        integer(int64), intent(in) :: mm            !! population size
        real(real64) :: dt                          !! seconds
        dt = now()
        call pair_range(SEED, STREAM, mm, c, 1_int64, n)
        dt = now() - dt
    end function timed_pair

    !> One timed run of the narrow grid, one block per body.
    function timed_w32(mm) result(dt)
        integer(int64), intent(in) :: mm            !! population size
        real(real64) :: dt                          !! seconds
        dt = now()
        call w32_range(SEED, STREAM, mm, d, 1_int64, n)
        dt = now() - dt
    end function timed_w32

    !> One timed run of the narrow grid, two blocks per body.
    function timed_w32x2(mm) result(dt)
        integer(int64), intent(in) :: mm            !! population size
        real(real64) :: dt                          !! seconds
        dt = now()
        call w32x2_range(SEED, STREAM, mm, d, 1_int64, n)
        dt = now() - dt
    end function timed_w32x2

    !> One timed run of the 64-bit cipher floor.
    function timed_cipher64() result(dt)
        real(real64) :: dt                          !! seconds
        dt = now()
        call cipher64_range(SEED, STREAM, a, 1_int64, n)
        dt = now() - dt
    end function timed_cipher64

    !> One timed run of the 32-bit cipher floor.
    function timed_cipher32() result(dt)
        real(real64) :: dt                          !! seconds
        dt = now()
        call cipher32_range(SEED, STREAM, a, 1_int64, n)
        dt = now() - dt
    end function timed_cipher32

#ifdef _OPENMP
    !> Times the two candidate shapes at each thread count, asserting bit-identity as it goes.
    subroutine threaded_rows(mm, ths)
        integer(int64), intent(in) :: mm            !! population size
        integer, intent(in) :: ths(:)               !! thread counts to try
        real(real64) :: t1, t2, base1, base2
        integer :: i2, r
        base1 = 0.0_real64
        base2 = 0.0_real64
        do i2 = 1, size(ths)
            t1 = huge(1.0_real64)
            t2 = huge(1.0_real64)
            do r = 1, rounds
                t1 = min(t1, timed_par_pair(mm, ths(i2)))
                t2 = min(t2, timed_par_w32x2(mm, ths(i2)))
            end do
            ! Bit-identity against the serial form: threading must not touch a value.
            call pair_range(SEED, STREAM, mm, a, 1_int64, n)
            call must_match(a, c, 'threaded pair == serial pair')
            call w32x2_range(SEED, STREAM, mm, a, 1_int64, n)
            call must_match(a, d, 'threaded w32x2 == serial w32x2')
            if (i2 == 1) then
                base1 = t1
                base2 = t2
            end if
            write (output_unit, '(i12,2f12.2,2f15.2)') ths(i2), &
                t1 * 1.0e9_real64 / real(n, real64), t2 * 1.0e9_real64 / real(n, real64), &
                base1 / t1, base2 / t2
            flush (output_unit)
        end do
    end subroutine threaded_rows

    !> One timed threaded run of the value-preserving restructure.
    function timed_par_pair(mm, nth) result(dt)
        integer(int64), intent(in) :: mm            !! population size
        integer, intent(in) :: nth                  !! thread count
        real(real64) :: dt                          !! seconds
        integer(int64) :: chunk, lo, hi
        integer :: t
        chunk = (n + int(nth, int64) - 1_int64) / int(nth, int64)
        dt = now()
        !$omp parallel do default(shared) private(t, lo, hi) schedule(static) num_threads(nth)
        do t = 0, nth - 1
            lo = int(t, int64) * chunk + 1_int64
            hi = min(n, lo + chunk - 1_int64)
            if (lo <= hi) call pair_range(SEED, STREAM, mm, c, lo, hi)
        end do
        !$omp end parallel do
        dt = now() - dt
    end function timed_par_pair

    !> One timed threaded run of the narrow grid.
    function timed_par_w32x2(mm, nth) result(dt)
        integer(int64), intent(in) :: mm            !! population size
        integer, intent(in) :: nth                  !! thread count
        real(real64) :: dt                          !! seconds
        integer(int64) :: chunk, lo, hi
        integer :: t
        chunk = (n + int(nth, int64) - 1_int64) / int(nth, int64)
        dt = now()
        !$omp parallel do default(shared) private(t, lo, hi) schedule(static) num_threads(nth)
        do t = 0, nth - 1
            lo = int(t, int64) * chunk + 1_int64
            hi = min(n, lo + chunk - 1_int64)
            if (lo <= hi) call w32x2_range(SEED, STREAM, mm, d, lo, hi)
        end do
        !$omp end parallel do
        dt = now() - dt
    end function timed_par_w32x2
#endif

    !> A monotonic clock in seconds.
    function now() result(t)
        real(real64) :: t                           !! seconds since an arbitrary origin
#ifdef _OPENMP
        t = omp_get_wtime()
#else
        integer(int64) :: c, rate
        call system_clock(c, rate)
        t = real(c, real64) / real(rate, real64)
#endif
    end function now

    !> Reads `--key=<int>` from the command line; returns `dflt` when absent.
    function read_arg_int(key, dflt) result(v)
        character(len=*), intent(in) :: key         !! the flag, including its trailing `=`
        integer(int64), intent(in) :: dflt          !! value when the flag is absent
        integer(int64) :: v                         !! the parsed value
        character(len=64) :: buf
        integer :: k, ios
        v = dflt
        do k = 1, command_argument_count()
            call get_command_argument(k, buf)
            if (index(buf, key) == 1) then
                read (buf(len(key) + 1:), *, iostat=ios) v
                if (ios /= 0) v = dflt
            end if
        end do
    end function read_arg_int

end program probe_random_resample
