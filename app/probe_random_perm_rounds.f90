!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> Prices the proposed 24-round + parity permutation against Fisher-Yates and against the shipped
!! 4-round kernel. Companion to `feature_random_feistel_A.md`.
!!
!! **Why this exists.** `feature_random_feistel_A.md` establishes that the shipped permutation
!! (`perm_rounds = 4`) is measurably non-uniform at small `m`, that no round count fixes the parity
!! lock at `m = 9, 25, 49, ...`, and that an exhaustive all-cells test puts the required round count
!! at **24** rather than the 10 a battery of marginal statistics suggested. That fix costs speed, and
!! this probe is what says how much -- against the only reference that matters, an exactly uniform
!! Fisher-Yates shuffle.
!!
!! **The three arms.**
!!
!!   * `fy` -- Fisher-Yates driven by `pf_random_int_at`, one counter-based integer draw per element.
!!     Exactly uniform, reproducible, and `O(m)` -- but it must materialise the whole population, and
!!     it has no random access at all. That last point is the whole reason the Feistel exists and is
!!     what `--mode=subset` measures.
!!   * `feistel-4` -- the shipped kernel, reproduced by this file's own replica.
!!   * `feistel-24p` -- the proposal: 24 rounds plus the seed-driven parity correction.
!!
!! **The replica is validated before anything is timed, and that gate is the point of the file.**
!! A hand-written copy of a kernel is untested code; at 4 rounds with the parity step off it must
!! reproduce `pf_random_perm_at` and `pf_random_permutation` exactly, element for element. If it does
!! not, every number below is about some other algorithm and the probe exits nonzero rather than
!! printing them. The bulk replica additionally mirrors `perm_range_i64`'s hoisted `(l, r)`
!! enumeration rather than calling the scalar form in a loop -- the library is about 2.4x cheaper in
!! bulk for exactly that reason, and a replica that skipped it would slander the Feistel by a
!! per-element division and key schedule it does not actually pay.
!!
!! **Modes** (`--mode=`, default `whole`):
!!
!!   * `whole` -- build an entire permutation of `1 .. m`. The like-for-like comparison: both
!!     algorithms are `O(m)` and produce the same object.
!!   * `subset` -- draw the first `n` of a permutation of `1 .. m` with `n << m`. Fisher-Yates has to
!!     shuffle all `m`; the Feistel reads `n` elements and never allocates the population. This is
!!     the structural difference, and it does not depend on the round count.
!!   * `scalar` -- single-element random access, `pf_random_perm_at`-style, against round count.
!!
!! Not run by `fpm test`, by design (CLAUDE.md, "Manual (never-`fpm test`) large-scale/benchmark
!! tools"). Driven by `tools/benchmark_perm_rounds.sh`; run directly with:
!!
!! ```bash
!! fpm run --profile release probe_random_perm_rounds -- --mode=whole --m=1000000
!! ```
program probe_random_perm_rounds

#ifdef _OPENMP
    use omp_lib, only: omp_get_wtime
#endif
    use iso_fortran_env, only: int32, int64, real64, output_unit, error_unit, &
                               compiler_version, compiler_options
    use parquet, only: pf_random_perm_at, pf_random_permutation, pf_random_int_at

    implicit none

    !> Low 32 bits set; the word mask the round function reduces to.
    integer(int64), parameter :: M32 = 4294967295_int64
    !> First odd multiplier of the round function's mix.
    integer(int64), parameter :: PC1 = 2654435761_int64
    !> Second odd multiplier of the round function's mix.
    integer(int64), parameter :: PC2 = 2246822519_int64
    !> Low 31 bits set; masking to this bounds every product below `2**63`.
    integer(int64), parameter :: PM31 = 2147483647_int64
    !> Largest left factor whose square is representable without overflow.
    integer(int64), parameter :: PAMAX = 3037000499_int64
    !> Highest round count this probe will accept, sizing every round-key array.
    integer, parameter :: MAXR = 128

    !> Round count of the shipped kernel, which the replica must reproduce exactly.
    integer, parameter :: R_SHIPPED = 4
    !> Round count of the proposal priced here.
    integer, parameter :: R_PROPOSED = 24

    character(len=32) :: mode
    integer(int64) :: m_arg, n_arg, reps
    integer :: rounds_arg

    mode = read_arg_str('--mode=', 'whole')
    m_arg = read_arg_int('--m=', 1000000_int64)
    n_arg = read_arg_int('--n=', 1000_int64)
    reps = read_arg_int('--reps=', 3_int64)
    rounds_arg = int(read_arg_int('--rounds=', int(R_PROPOSED, int64)), int32)

    call banner()
    call gates()

    select case (trim(mode))
    case ('whole')
        call mode_whole()
    case ('subset')
        call mode_subset()
    case ('scalar')
        call mode_scalar()
    case default
        write (error_unit, '(a)') 'probe_random_perm_rounds: unknown --mode=' // trim(mode) // &
            ' (expected whole, subset or scalar)'
        stop 1
    end select

contains

    ! ================================================================================
    ! The replica -- variable round count, optional parity correction
    ! ================================================================================

    !> One round-function evaluation. Every operand is masked to 31 bits before each multiply, so
    !! no product can reach `2**63` and no signed overflow is possible; see `perm_mix2` in
    !! `src/parquet_random.f90`, whose masks are a correctness requirement rather than tidiness.
    pure function mix2(rk, x) result(w)
        integer(int64), intent(in) :: rk            !! this round's key
        integer(int64), intent(in) :: x             !! the half being mixed, below `2**32`
        integer(int64) :: w                         !! a mixed word, below `2**32`
        integer(int64) :: z
        z = ieor(x, rk)
        z = iand(z, PM31) * PC1
        z = ieor(z, ishft(z, -29))
        z = iand(z, PM31) * PC2
        z = ieor(z, ishft(z, -31))
        w = iand(z, M32)
    end function mix2

    !> The width rule: `a = ceil(sqrt(m))`, `b = ceil(m/a)`. Mirrors `perm_factors`.
    pure subroutine factors(m, a, b)
        integer(int64), intent(in) :: m             !! population size, at least 2
        integer(int64), intent(out) :: a            !! left factor
        integer(int64), intent(out) :: b            !! right factor
        a = int(sqrt(real(m, real64)), int64)
        if (a < 1_int64) a = 1_int64
        if (a > PAMAX) a = PAMAX
        do while (a < PAMAX .and. a * a < m)
            a = a + 1_int64
        end do
        do while (a > 1_int64 .and. (a - 1_int64) * (a - 1_int64) >= m)
            a = a - 1_int64
        end do
        b = (m + a - 1_int64) / a
    end subroutine factors

    !> Derives `nr` round keys from the seed. Mirrors `perm_round_keys`, extended to any round count.
    pure subroutine round_keys(seed, nr, rk)
        integer(int64), intent(in) :: seed          !! the permutation family's seed
        integer, intent(in) :: nr                   !! how many keys to derive
        integer(int64), intent(out) :: rk(nr)       !! one key per round
        integer :: j
        do j = 1, nr
            rk(j) = mix2(int(j, int64) * PC1, iand(seed, M32))
            rk(j) = ieor(rk(j), mix2(int(j, int64), iand(ishft(seed, -32), M32)))
        end do
    end subroutine round_keys

    !> The rounds, from an already-split input. Mirrors `perm_feistel_lr`.
    pure function feistel_lr(rk, nr, a, b, l0, r0) result(y)
        integer(int64), intent(in) :: rk(:)         !! the round keys, at least `nr` of them
        integer, intent(in) :: nr                   !! round count; must be even
        integer(int64), intent(in) :: a             !! left factor
        integer(int64), intent(in) :: b             !! right factor
        integer(int64), intent(in) :: l0            !! left half of the input, in `[0, a)`
        integer(int64), intent(in) :: r0            !! right half of the input, in `[0, b)`
        integer(int64) :: y                         !! output in `[0, a*b)`
        integer(int64) :: l, r, t, p, q, sw
        integer :: j
        p = a
        q = b
        l = l0
        r = r0
        do j = 1, nr
            t = l + ishft(iand(mix2(rk(j), r), PM31) * p, -31)
            if (t >= p) t = t - p
            l = r
            r = t
            sw = p
            p = q
            q = sw
        end do
        y = l * q + r
    end function feistel_lr

    !> One application of the network from an unsplit input. Mirrors `perm_feistel`.
    pure function feistel(rk, nr, a, b, x) result(y)
        integer(int64), intent(in) :: rk(:)         !! the round keys
        integer, intent(in) :: nr                   !! round count
        integer(int64), intent(in) :: a             !! left factor
        integer(int64), intent(in) :: b             !! right factor
        integer(int64), intent(in) :: x             !! input in `[0, a*b)`
        integer(int64) :: y                         !! output in `[0, a*b)`
        integer(int64) :: l
        l = x / b
        y = feistel_lr(rk, nr, a, b, l, x - l * b)
    end function feistel

    !> Applies the proposed parity correction to a 0-based output.
    !!
    !! A distribution uniform on the alternating group, composed with a fair coin over
    !! `{identity, one fixed transposition}`, is uniform on the symmetric group. That is the whole
    !! mechanism, and it is what lifts the coset lock at `m = odd**2` which no round count can.
    pure function parity_fix(x, pbit) result(y)
        integer(int64), intent(in) :: x             !! the 0-based output of the network
        integer(int64), intent(in) :: pbit          !! the seed's parity bit, 0 or 1
        integer(int64) :: y                         !! `x`, with 0 and 1 swapped when `pbit` is 1
        y = x
        if (pbit == 1_int64) then
            if (x == 0_int64) then
                y = 1_int64
            else if (x == 1_int64) then
                y = 0_int64
            end if
        end if
    end function parity_fix

    !> The scalar entry point at `nr` rounds. With `nr == 4` and `par` false this IS the library.
    function perm_at_r(seed, m, k, nr, par) result(res)
        integer(int64), intent(in) :: seed          !! the permutation family's seed
        integer(int64), intent(in) :: m             !! population size
        integer(int64), intent(in) :: k             !! 1-based position, clamped into `[1, m]`
        integer, intent(in) :: nr                   !! round count
        logical, intent(in) :: par                  !! apply the parity correction
        integer(int64) :: res                       !! element `k`, in `[1, m]`
        integer(int64) :: a, b, x, n, rk(MAXR)
        n = m
        if (n < 1_int64) n = 1_int64
        x = k - 1_int64
        if (x < 0_int64) x = 0_int64
        if (x >= n) x = n - 1_int64
        if (n == 1_int64) then
            res = 1_int64
            return
        end if
        call factors(n, a, b)
        call round_keys(seed, nr + 1, rk(1:nr + 1))
        do
            x = feistel(rk, nr, a, b, x)
            if (x < n) exit
        end do
        if (par) x = parity_fix(x, iand(rk(nr + 1), 1_int64))
        res = x + 1_int64
    end function perm_at_r

    !> The bulk form at `nr` rounds, mirroring `perm_range_i64`'s hoisted `(l, r)` enumeration.
    !!
    !! The key schedule and the width rule are derived once per call and the loop indices already
    !! are the split, so no per-element division is paid. Reproducing that shape is what makes the
    !! comparison against Fisher-Yates fair.
    subroutine perm_fill_r(seed, m, n, v, nr, par)
        integer(int64), intent(in) :: seed          !! the permutation family's seed
        integer(int64), intent(in) :: m             !! population size
        integer(int64), intent(in) :: n             !! how many elements to produce, `0 <= n <= m`
        integer(int64), intent(out) :: v(:)         !! filled with elements `1 .. n`
        integer, intent(in) :: nr                   !! round count
        logical, intent(in) :: par                  !! apply the parity correction
        integer(int64) :: a, b, rk(MAXR), l, r0, rhi, y, k, r, pbit
        if (n <= 0_int64) return
        if (m <= 1_int64) then
            v(1) = 1_int64
            return
        end if
        call factors(m, a, b)
        call round_keys(seed, nr + 1, rk(1:nr + 1))
        pbit = iand(rk(nr + 1), 1_int64)
        k = 1_int64
        l = 0_int64
        r0 = 0_int64
        do while (k <= n)
            rhi = min(b - 1_int64, r0 + (n - k))
            do r = r0, rhi
                y = feistel_lr(rk, nr, a, b, l, r)
                do while (y >= m)
                    y = feistel(rk, nr, a, b, y)
                end do
                if (par) y = parity_fix(y, pbit)
                v(k + (r - r0)) = y + 1_int64
            end do
            k = k + (rhi - r0 + 1_int64)
            r0 = 0_int64
            l = l + 1_int64
        end do
    end subroutine perm_fill_r

    ! ================================================================================
    ! The reference: Fisher-Yates
    ! ================================================================================

    !> An exactly uniform shuffle of `1 .. m`, one counter-based integer draw per element.
    !!
    !! Reproducible and library-consistent, which is what makes it comparable; a non-reproducible
    !! shuffle over a cheaper sequential PRNG would be faster and would not be the same experiment.
    !! Note it is `O(m)` in time AND memory whatever the caller wanted -- see `--mode=subset`.
    subroutine fisher_yates(seed, m, v)
        integer(int64), intent(in) :: seed          !! the shuffle's seed
        integer(int64), intent(in) :: m             !! population size
        integer(int64), intent(out) :: v(:)         !! filled with a permutation of `1 .. m`
        integer(int64) :: k, j, t
        do k = 1_int64, m
            v(k) = k
        end do
        do k = m, 2_int64, -1_int64
            j = pf_random_int_at(seed, k, 1_int64, k)
            t = v(k)
            v(k) = v(j)
            v(j) = t
        end do
    end subroutine fisher_yates

    ! ================================================================================
    ! Gates -- nothing is timed until the replica is proved to be the library
    ! ================================================================================

    !> Proves the replica reproduces the shipped kernel, and that every arm is a real permutation.
    !!
    !! Without this the probe is timing an unknown algorithm and reporting it as the library's.
    subroutine gates()
        integer(int64) :: mlist(9), mm, s, kk, nbad, i
        integer(int64), allocatable :: v(:), w(:)
        logical, allocatable :: seen(:)
        integer :: mi

        mlist = [2_int64, 3_int64, 8_int64, 9_int64, 20_int64, 21_int64, 100_int64, 1023_int64, 65537_int64]

        ! G1 -- the scalar replica IS pf_random_perm_at at 4 rounds with parity off.
        nbad = 0_int64
        do mi = 1, size(mlist)
            mm = mlist(mi)
            do s = 1_int64, 200_int64
                do kk = 1_int64, min(mm, 40_int64)
                    if (perm_at_r(s, mm, kk, R_SHIPPED, .false.) /= pf_random_perm_at(s, mm, kk)) &
                        nbad = nbad + 1_int64
                end do
            end do
        end do
        call report_gate('G1 scalar replica == pf_random_perm_at (r=4)', nbad)

        ! G2 -- the bulk replica IS pf_random_permutation at 4 rounds with parity off.
        nbad = 0_int64
        do mi = 1, size(mlist)
            mm = mlist(mi)
            allocate (v(mm), w(mm))
            do s = 1_int64, 20_int64
                call perm_fill_r(s, mm, mm, v, R_SHIPPED, .false.)
                call pf_random_permutation(w, s)
                if (any(v /= w)) nbad = nbad + 1_int64
            end do
            deallocate (v, w)
        end do
        call report_gate('G2 bulk replica == pf_random_permutation (r=4)', nbad)

        ! G3/G4 -- Fisher-Yates and the proposal both produce genuine permutations.
        nbad = 0_int64
        mm = 4096_int64
        allocate (v(mm), seen(mm))
        do s = 1_int64, 20_int64
            call fisher_yates(s, mm, v)
            seen = .false.
            do i = 1_int64, mm
                if (v(i) < 1_int64 .or. v(i) > mm) then
                    nbad = nbad + 1_int64
                else if (seen(v(i))) then
                    nbad = nbad + 1_int64
                else
                    seen(v(i)) = .true.
                end if
            end do
        end do
        call report_gate('G3 Fisher-Yates output is a permutation', nbad)

        nbad = 0_int64
        do s = 1_int64, 20_int64
            call perm_fill_r(s, mm, mm, v, R_PROPOSED, .true.)
            seen = .false.
            do i = 1_int64, mm
                if (v(i) < 1_int64 .or. v(i) > mm) then
                    nbad = nbad + 1_int64
                else if (seen(v(i))) then
                    nbad = nbad + 1_int64
                else
                    seen(v(i)) = .true.
                end if
            end do
        end do
        call report_gate('G4 24-round + parity output is a permutation', nbad)
        deallocate (v, seen)
        write (output_unit, '(a)') ''
    end subroutine gates

    !> Prints one gate's verdict and aborts the run if it failed.
    subroutine report_gate(name, nbad)
        character(len=*), intent(in) :: name        !! the gate's description
        integer(int64), intent(in) :: nbad          !! how many mismatches it found; 0 to pass
        if (nbad == 0_int64) then
            write (output_unit, '(a,a)') '  [ok]   ', name
        else
            write (output_unit, '(a,a,a,i0,a)') '  [FAIL] ', name, ' -- ', nbad, ' mismatches'
            write (error_unit, '(a)') 'probe_random_perm_rounds: the replica is not the library; ' // &
                'every timing below would be about some other algorithm.'
            stop 1
        end if
    end subroutine report_gate

    ! ================================================================================
    ! Modes
    ! ================================================================================

    !> Whole permutation of `1 .. m`: the like-for-like comparison, both arms `O(m)`.
    subroutine mode_whole()
        integer(int64), allocatable :: v(:)
        real(real64) :: t_fy, t_r4, t_rp
        integer(int64) :: chk

        allocate (v(m_arg))
        v = 0_int64
        write (output_unit, '(a,i0,a,i0,a)') '--- mode=whole  m = ', m_arg, ',  best of ', reps, ' ---'

        t_fy = time_whole(1, v, chk)
        write (output_unit, '(a,f12.3,a,f9.2,a,i0,a)') '  fy            ', t_fy * 1.0e3_real64, &
            ' ms   ', t_fy * 1.0e9_real64 / real(m_arg, real64), ' ns/elem   [chk ', chk, ']'
        t_r4 = time_whole(2, v, chk)
        write (output_unit, '(a,f12.3,a,f9.2,a,i0,a)') '  feistel-4     ', t_r4 * 1.0e3_real64, &
            ' ms   ', t_r4 * 1.0e9_real64 / real(m_arg, real64), ' ns/elem   [chk ', chk, ']'
        t_rp = time_whole(3, v, chk)
        write (output_unit, '(a,f12.3,a,f9.2,a,i0,a)') '  feistel-24p   ', t_rp * 1.0e3_real64, &
            ' ms   ', t_rp * 1.0e9_real64 / real(m_arg, real64), ' ns/elem   [chk ', chk, ']'

        write (output_unit, '(a)') ''
        write (output_unit, '(a,f8.2,a)') '  feistel-24p vs feistel-4 : ', t_rp / t_r4, 'x'
        write (output_unit, '(a,f8.2,a)') '  feistel-24p vs fy        : ', t_rp / t_fy, 'x'
        write (output_unit, '(a,f8.2,a)') '  feistel-4   vs fy        : ', t_r4 / t_fy, 'x'
        deallocate (v)
    end subroutine mode_whole

    !> Times one whole-permutation arm, best of `reps`.
    function time_whole(arm, v, chk) result(best)
        integer, intent(in) :: arm                  !! 1 = fy, 2 = feistel-4, 3 = feistel-24p
        integer(int64), intent(inout) :: v(:)       !! scratch, size `m_arg`
        integer(int64), intent(out) :: chk          !! checksum, to keep the work live
        real(real64) :: best                        !! best wall time over `reps`, seconds
        real(real64) :: t0, t1
        integer(int64) :: rep
        best = huge(1.0_real64)
        do rep = 1_int64, reps
            t0 = wall()
            select case (arm)
            case (1)
                call fisher_yates(rep, m_arg, v)
            case (2)
                call perm_fill_r(rep, m_arg, m_arg, v, R_SHIPPED, .false.)
            case default
                call perm_fill_r(rep, m_arg, m_arg, v, rounds_arg, .true.)
            end select
            t1 = wall()
            best = min(best, t1 - t0)
        end do
        chk = sum(v(1:min(size(v, kind=int64), 64_int64)))
    end function time_whole

    !> First `n` of a permutation of `1 .. m`: the structural comparison, `O(m)` against `O(n)`.
    subroutine mode_subset()
        integer(int64), allocatable :: big(:), small(:)
        real(real64) :: t0, t1, t_fy, t_r4, t_rp
        integer(int64) :: rep, chk

        write (output_unit, '(a,i0,a,i0,a,i0,a)') '--- mode=subset  n = ', n_arg, ' of m = ', m_arg, &
            ',  best of ', reps, ' ---'
        allocate (big(m_arg), small(n_arg))

        t_fy = huge(1.0_real64)
        do rep = 1_int64, reps
            t0 = wall()
            call fisher_yates(rep, m_arg, big)
            small = big(1:n_arg)
            t1 = wall()
            t_fy = min(t_fy, t1 - t0)
        end do
        chk = sum(small)
        write (output_unit, '(a,f12.3,a,i0,a)') '  fy (shuffles all m)   ', t_fy * 1.0e3_real64, &
            ' ms   [chk ', chk, ']'

        t_r4 = huge(1.0_real64)
        do rep = 1_int64, reps
            t0 = wall()
            call perm_fill_r(rep, m_arg, n_arg, small, R_SHIPPED, .false.)
            t1 = wall()
            t_r4 = min(t_r4, t1 - t0)
        end do
        chk = sum(small)
        write (output_unit, '(a,f12.3,a,i0,a)') '  feistel-4  (reads n)  ', t_r4 * 1.0e3_real64, &
            ' ms   [chk ', chk, ']'

        t_rp = huge(1.0_real64)
        do rep = 1_int64, reps
            t0 = wall()
            call perm_fill_r(rep, m_arg, n_arg, small, rounds_arg, .true.)
            t1 = wall()
            t_rp = min(t_rp, t1 - t0)
        end do
        chk = sum(small)
        write (output_unit, '(a,f12.3,a,i0,a)') '  feistel-24p (reads n) ', t_rp * 1.0e3_real64, &
            ' ms   [chk ', chk, ']'

        write (output_unit, '(a)') ''
        write (output_unit, '(a,f10.1,a)') '  fy / feistel-24p : ', t_fy / t_rp, 'x'
        write (output_unit, '(a)') '  (the Feistel never allocates or touches the population;' // &
            ' this gap grows linearly with m)'
        deallocate (big, small)
    end subroutine mode_subset

    !> Single-element random access against round count.
    subroutine mode_scalar()
        integer :: rl(7), i
        real(real64) :: t0, t1, base, el
        integer(int64) :: s, acc, ncall

        rl = [4, 10, 16, 20, 24, 32, 48]
        ncall = 20000000_int64
        base = 0.0_real64
        write (output_unit, '(a,i0,a,i0,a)') '--- mode=scalar  m = ', m_arg, ',  ', ncall, ' calls ---'
        write (output_unit, '(a)') '  rounds    ns/element    vs r=4'
        do i = 1, size(rl)
            acc = 0_int64
            t0 = wall()
            do s = 1_int64, ncall
                acc = acc + perm_at_r(s, m_arg, iand(s, 1023_int64) + 1_int64, rl(i), .true.)
            end do
            t1 = wall()
            el = (t1 - t0) * 1.0e9_real64 / real(ncall, real64)
            if (i == 1) base = el
            write (output_unit, '(i8,f14.2,f10.2,a,i0,a)') rl(i), el, el / base, 'x   [chk ', acc, ']'
        end do
    end subroutine mode_scalar

    ! ================================================================================
    ! Helpers
    ! ================================================================================

    !> Wall-clock seconds. Uses `omp_get_wtime` when OpenMP is available, `system_clock` otherwise.
    function wall() result(t)
        real(real64) :: t                           !! seconds since an arbitrary origin
#ifdef _OPENMP
        t = omp_get_wtime()
#else
        integer(int64) :: c, rate
        call system_clock(c, rate)
        t = real(c, real64) / real(rate, real64)
#endif
    end function wall

    !> Prints the build and configuration this run measured.
    subroutine banner()
        write (output_unit, '(a)') '=== probe_random_perm_rounds ==='
        write (output_unit, '(a,a)') '  compiler : ', trim(compiler_version())
        write (output_unit, '(a,a)') '  options  : ', trim(compiler_options())
        write (output_unit, '(a,a,a,i0,a,i0)') '  mode     : ', trim(mode), '   m = ', m_arg, &
            '   proposed rounds = ', rounds_arg
        write (output_unit, '(a)') ''
    end subroutine banner

    !> Reads `--key=<int>` from the command line; returns `dflt` when absent or unparsable.
    function read_arg_int(key_, dflt) result(v)
        character(len=*), intent(in) :: key_        !! the flag, including its trailing `=`
        integer(int64), intent(in) :: dflt          !! value when the flag is absent
        integer(int64) :: v                         !! the parsed value
        character(len=64) :: arg
        integer :: j, ios
        v = dflt
        do j = 1, command_argument_count()
            call get_command_argument(j, arg)
            if (index(arg, key_) == 1) then
                read (arg(len(key_) + 1:), *, iostat=ios) v
                if (ios /= 0) v = dflt
            end if
        end do
    end function read_arg_int

    !> Reads `--key=<text>` from the command line; returns `dflt` when absent.
    function read_arg_str(key_, dflt) result(v)
        character(len=*), intent(in) :: key_        !! the flag, including its trailing `=`
        character(len=*), intent(in) :: dflt        !! value when the flag is absent
        character(len=32) :: v                      !! the parsed value
        character(len=64) :: arg
        integer :: j
        v = dflt
        do j = 1, command_argument_count()
            call get_command_argument(j, arg)
            if (index(arg, key_) == 1) v = trim(arg(len(key_) + 1:))
        end do
    end function read_arg_str

end program probe_random_perm_rounds
