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
program probe_random_resample

#ifdef _OPENMP
    use omp_lib, only: omp_get_wtime, omp_get_max_threads
#endif
    use iso_fortran_env, only: int32, int64, real64, output_unit
    use parquet, only: pf_random_int_at, pf_random_fill_draws, pf_random_key, &
                       parquet_debug_random_block, parquet_debug_random_uses_int128

    implicit none

    !> The 128-bit kind, used only by the probe's own copy of the 64-bit reduction.
    integer, parameter :: k128 = selected_int_kind(38)
    !> Low 32 bits set.
    integer(int64), parameter :: M32 = 4294967295_int64
    !> The sign bit, for the unsigned comparison `ult`.
    integer(int64), parameter :: SIGN_BIT = ishft(1_int64, 63)
    !> `2**63` as a 128-bit value.
    integer(k128), parameter :: TWO63_128 = int(huge(1_int64), k128) + 1_k128
    !> `2**64` as a 128-bit value.
    integer(k128), parameter :: TWO64_128 = 2_k128 * TWO63_128
    !> Low 64 bits set, as a 128-bit mask.
    integer(k128), parameter :: MASK64_128 = TWO64_128 - 1_k128
    !> The largest width the 32-bit candidate rule accepts: `x32 * s` must stay below `2**63`.
    integer(int64), parameter :: W32_MAX = 2147483648_int64

    integer(int64), parameter :: SEED = 20260817_int64
    integer(int64), parameter :: STREAM = 7_int64

    integer(int64) :: n, m
    integer :: rounds, i
    integer(int64), allocatable :: a(:), b(:), c(:), d(:)
    integer(int64) :: m_list(8)
    integer :: th_list(4)

    n = read_arg_int('--n=', 10000000_int64)
    m = read_arg_int('--m=', 1000000_int64)
    rounds = int(read_arg_int('--rounds=', 5_int64), int32)
    ! 1431655766 is just above 2**32/3, where `2**32 mod m` -- and so the 32-bit rejection rate --
    ! is at its maximum of about 1/3. It is the worst case for the narrow rule and is measured
    ! deliberately rather than left to be discovered by a user.
    m_list = [1000_int64, 1000000_int64, 16711936_int64, 100000000_int64, 1000000000_int64, &
              1431655766_int64, 2147483648_int64, 100000000000_int64]
    th_list = [1, 2, 4, 8]

    write (output_unit, '(a)') 'probe_random_resample: n uniform draws from 1..m, WITH replacement'
    write (output_unit, '(a,i0,a,i0)') 'n = ', n, ', rounds = ', rounds
    write (output_unit, '(a,l1)') 'int128 kernel arm: ', parquet_debug_random_uses_int128()
#ifdef _OPENMP
    write (output_unit, '(a,i0)') 'OpenMP max threads: ', omp_get_max_threads()
#else
    write (output_unit, '(a)') 'OpenMP: NOT compiled in -- threaded arms are skipped'
#endif
    write (output_unit, '(a)') ''
    flush (output_unit)

    allocate (a(n), b(n), c(n), d(n))
    a = 0_int64; b = 0_int64; c = 0_int64; d = 0_int64   ! first-touch every page before timing

    call gate_all(m)

    write (output_unit, '(a)') '---- serial, ns per value (best of rounds) ----'
    write (output_unit, '(a)') &
        '           m      scalar        fill       pair1        pair         w32       w32x2'// &
        '    cipher64    cipher32    rej32%'
    do i = 1, size(m_list)
        call serial_row(m_list(i))
    end do
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
    call threaded_rows(1000000_int64, th_list)
    write (output_unit, '(a)') ''
#endif

    deallocate (a, b, c, d)

contains

    ! ============================================================================
    ! Correctness gates -- every one of these runs before any timing is reported
    ! ============================================================================

    !> Runs every gate, aborting on the first failure. Nothing below is meaningful without these.
    subroutine gate_all(mm)
        integer(int64), intent(in) :: mm            !! a representative population size
        integer(int64) :: ng, k, mg
        integer(int64), allocatable :: g1(:), g2(:), g3(:), g4(:)
        ng = min(n, 200000_int64)
        allocate (g1(ng), g2(ng), g3(ng), g4(ng))

        ! G1: the shipped fill agrees with the shipped scalar draw. A sanity check on the probe's
        !     understanding of the contract, not on the library.
        call pf_random_fill_draws(SEED, STREAM, g1, 1_int64, mm)
        do k = 1_int64, ng
            g2(k) = pf_random_int_at(SEED, STREAM, 1_int64, mm, k)
        end do
        call must_match(g1, g2, 'G1 shipped fill == shipped scalar draw')

        ! G2: the restructured arm is value-preserving. THE load-bearing gate: it is what makes
        !     `pair`'s timing a prediction about the library rather than about this file.
        call pair_range(SEED, STREAM, mm, g3, 1_int64, ng)
        call must_match(g1, g3, 'G2 pair == shipped fill (value-preserving)')

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

        deallocate (g1, g2, g3, g4)
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
    pure function reduce64(x, s, seed, stream, draw) result(r)
        integer(int64), intent(in) :: x             !! the 64-bit candidate
        integer(int64), intent(in) :: s             !! the width, `m`
        integer(int64), intent(in) :: seed          !! seed, for the delegated rejection path
        integer(int64), intent(in) :: stream        !! stream index
        integer(int64), intent(in) :: draw          !! 1-based draw index
        integer(int64) :: r                         !! a uniform value in `[1, s]`
        integer(int64) :: low, high
        integer(k128) :: p
        p = iand(int(x, k128), MASK64_128) * int(s, k128)
        high = int(ishft(p, -64), int64)
        low = fold64(iand(p, MASK64_128))
        if (ult(low, s)) then
            if (ult(low, int(mod(TWO64_128, int(s, k128)), int64))) then
                r = pf_random_int_at(seed, stream, 1_int64, s, draw)     ! rejection: essentially never
                return
            end if
        end if
        r = 1_int64 + high
    end function reduce64

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
