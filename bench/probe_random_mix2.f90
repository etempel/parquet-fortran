!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!> Justifies `perm_mix2`'s two multipliers, which `feature_random_feistel.md` §9 flagged as
!> "conventional, not analysed" and left open when the Feistel permutation shipped.
!>
!> **The question this answers is narrower than it first looks, and stating it correctly is half
!> the work.** A Feistel network is a bijection for *any* round function, so the multipliers cannot
!> make `pf_random_perm_at` return a non-permutation, cannot make it non-reproducible, and cannot
!> make it overflow (the 31-bit masks handle that, and `--mode=props` re-derives the bound). The
!> only thing they can affect is **mixing quality**. So the useful question is not "are these two
!> constants optimal" -- there is no such thing for this purpose -- but:
!>
!>   *Does the shipped construction's structural clearance depend on the particular constants
!>    chosen, or on the round count?*
!>
!> If the verdict tracks the round count across a range of conventional multipliers, the constants
!> are doing an ordinary job and no lucky choice is being relied on. That is what `--mode=sweep`
!> measures, and it is the load-bearing mode.
!>
!> Four modes, and the first is a precondition for the rest:
!>
!>  * **`selfcheck`** -- the local replication of `perm_mix2`, the key schedule and the 4-round
!>    modular network must reproduce `pf_random_perm_at` **exactly**, at `m = 1024` where
!>    `a*b = 32*32 = m` so the cycle-walk never runs. Every other mode varies constants inside that
!>    replication, so a replication that is not faithful would produce a confident table about
!>    something other than the library. Runs first in `all`, and the program stops if it fails.
!>  * **`props`** -- the deterministic structural requirements: oddness, magnitude, the
!>    no-overflow bound, and the provenance of each constant.
!>  * **`avalanche`** -- the strict avalanche criterion for `perm_mix2` itself: flip one input bit,
!>    measure how often each output bit changes. Ideal is 0.5 everywhere.
!>  * **`sweep`** -- the modular structural distinguisher of `feature_random_feistel.md` §19, run
!>    over several multiplier pairs at 3 and 4 rounds, calibrated against Fisher-Yates. Includes
!>    two deliberately BAD pairs, without which "every conventional pair passes" would be vacuous.
!>
!> Not run by `fpm test`, by design (CLAUDE.md, "Manual (never-`fpm test`) large-scale/benchmark
!> tools"). Build with `--profile release`:
!>
!> ```bash
!> fpm run probe_random_mix2 --profile release -- --mode=all
!> ```
program probe_random_mix2

    use iso_fortran_env, only: int64, real64, output_unit
    use parquet, only: pf_random_perm_at, pf_random_int_at
    implicit none

    !> Low 32 bits set.
    integer(int64), parameter :: M32 = 4294967295_int64
    !> Low 31 bits set -- the mask that makes every product provably overflow-free.
    integer(int64), parameter :: M31 = 2147483647_int64
    !> The shipped first multiplier: the odd 32-bit golden-ratio constant (xxHash PRIME32_1).
    integer(int64), parameter :: C1_SHIPPED = 2654435761_int64
    !> The shipped second multiplier: xxHash PRIME32_2.
    integer(int64), parameter :: C2_SHIPPED = 2246822519_int64
    !> The shipped round count.
    integer, parameter :: R_SHIPPED = 4
    !> The structural probe's population: `32*32` exactly, so no cycle-walk runs.
    integer(int64), parameter :: MS = 1024_int64
    !> Its factors.
    integer(int64), parameter :: AA = 32_int64, BB = 32_int64
    !> Keys per arm in the sweep.
    integer, parameter :: NKEYS = 64
    !> Base seed, so every run of this probe is reproducible.
    integer(int64), parameter :: SD0 = 20260817_int64

    character(len=32) :: mode
    logical :: ok

    call get_mode(mode)

    if (mode == 'selfcheck' .or. mode == 'all') then
        call run_selfcheck(ok)
        if (.not. ok) then
            write (output_unit, '(a)') 'SELFCHECK FAILED -- the local replication is not the library.'
            write (output_unit, '(a)') 'Every other mode would describe something else. Stopping.'
            stop 1
        end if
    end if
    if (mode == 'props' .or. mode == 'all') call run_props()
    if (mode == 'avalanche' .or. mode == 'all') call run_avalanche()
    if (mode == 'sweep' .or. mode == 'all') call run_sweep()

contains

    !> Reads `--mode=<name>`; defaults to `all`.
    subroutine get_mode(m)
        character(len=*), intent(out) :: m          !! the selected mode
        character(len=64) :: arg
        integer :: k, n
        m = 'all'
        n = command_argument_count()
        do k = 1, n
            call get_command_argument(k, arg)
            if (arg(1:7) == '--mode=') m = trim(arg(8:))
        end do
    end subroutine get_mode

    ! ============================================================================================
    ! The replication -- kept byte-faithful to src/parquet_random.f90, with the constants opened up
    ! ============================================================================================

    !> `perm_mix2` with the two multipliers made arguments. Identical to the library at `(c1, c2) =
    !! (C1_SHIPPED, C2_SHIPPED)`, which `run_selfcheck` proves rather than asserts.
    pure function mix2(rk, x, c1, c2) result(w)
        integer(int64), intent(in) :: rk            !! this round's key
        integer(int64), intent(in) :: x             !! the half being mixed, below `2**32`
        integer(int64), intent(in) :: c1            !! first multiplier
        integer(int64), intent(in) :: c2            !! second multiplier
        integer(int64) :: w                         !! a mixed word, below `2**32`
        integer(int64) :: z
        z = ieor(x, rk)
        z = iand(z, M31) * c1
        z = ieor(z, ishft(z, -29))
        z = iand(z, M31) * c2
        z = ieor(z, ishft(z, -31))
        w = iand(z, M32)
    end function mix2

    !> The library's round-key schedule, with the multipliers opened up the same way.
    pure subroutine round_keys(seed, nr, c1, c2, rk)
        integer(int64), intent(in) :: seed          !! the permutation family's seed
        integer, intent(in) :: nr                   !! round count
        integer(int64), intent(in) :: c1            !! first multiplier
        integer(int64), intent(in) :: c2            !! second multiplier
        integer(int64), intent(out) :: rk(:)        !! one key per round
        integer :: j
        do j = 1, nr
            rk(j) = mix2(int(j, int64) * c1, iand(seed, M32), c1, c2)
            rk(j) = ieor(rk(j), mix2(int(j, int64), iand(ishft(seed, -32), M32), c1, c2))
        end do
    end subroutine round_keys

    !> The whole permutation of `0 .. a*b-1`, at a given round count and multiplier pair.
    subroutine build_perm(seed, nr, a, b, c1, c2, y)
        integer(int64), intent(in) :: seed          !! the permutation family's seed
        integer, intent(in) :: nr                   !! round count
        integer(int64), intent(in) :: a             !! left factor
        integer(int64), intent(in) :: b             !! right factor
        integer(int64), intent(in) :: c1            !! first multiplier
        integer(int64), intent(in) :: c2            !! second multiplier
        integer(int64), intent(out) :: y(0:)        !! filled with the permutation
        integer(int64) :: rk(16), x, l, r, t, p, q, sw
        integer :: j
        call round_keys(seed, nr, c1, c2, rk)
        do x = 0_int64, a * b - 1_int64
            l = x / b
            r = x - l * b
            p = a
            q = b
            do j = 1, nr
                t = l + ishft(iand(mix2(rk(j), r, c1, c2), M31) * p, -31)
                if (t >= p) t = t - p
                l = r
                r = t
                sw = p
                p = q
                q = sw
            end do
            y(x) = l * q + r
        end do
    end subroutine build_perm

    ! ============================================================================================
    ! Modes
    ! ============================================================================================

    !> The replication must reproduce the shipped `pf_random_perm_at` exactly.
    subroutine run_selfcheck(good)
        logical, intent(out) :: good                !! `.true.` when every element matched
        integer(int64) :: y(0:MS - 1), seed, k, bad
        integer :: s
        write (output_unit, '(a)') '=========================================================='
        write (output_unit, '(a)') 'SELFCHECK -- local replication against the shipped library'
        write (output_unit, '(a,i0,a)') 'm = ', MS, ' (a*b = 32*32 = m exactly, so no cycle-walk runs)'
        bad = 0_int64
        do s = 1, 8
            seed = SD0 + int(s, int64) * 7919_int64
            call build_perm(seed, R_SHIPPED, AA, BB, C1_SHIPPED, C2_SHIPPED, y)
            do k = 1_int64, MS
                if (y(k - 1_int64) + 1_int64 /= pf_random_perm_at(seed, MS, k)) bad = bad + 1_int64
            end do
        end do
        good = (bad == 0_int64)
        write (output_unit, '(a,i0,a)') 'mismatches over 8 seeds x 1024 elements: ', bad, &
            merge('   OK  ', '  FAIL ', bad == 0_int64)
        write (output_unit, '(a)') ''
        flush (output_unit)
    end subroutine run_selfcheck

    !> The deterministic structural requirements on the two constants.
    subroutine run_props()
        integer(int64) :: worst
        write (output_unit, '(a)') '=========================================================='
        write (output_unit, '(a)') 'PROPS -- what the constants must satisfy, and do'
        write (output_unit, '(a)') ''
        write (output_unit, '(a)') 'constant        value  odd?   < 2**32?   provenance'
        write (output_unit, '(a,i12,a,l1,a,l1,a)') 'perm_c1  ', C1_SHIPPED, '     ', &
            (modulo(C1_SHIPPED, 2_int64) == 1_int64), '        ', (C1_SHIPPED < 4294967296_int64), &
            '      2**32/phi, the golden-ratio odd constant (xxHash PRIME32_1)'
        write (output_unit, '(a,i12,a,l1,a,l1,a)') 'perm_c2  ', C2_SHIPPED, '     ', &
            (modulo(C2_SHIPPED, 2_int64) == 1_int64), '        ', (C2_SHIPPED < 4294967296_int64), &
            '      xxHash PRIME32_2'
        write (output_unit, '(a)') ''
        write (output_unit, '(a)') 'Why ODD is required: multiplication by an odd constant is a bijection modulo any'
        write (output_unit, '(a)') 'power of two, so it loses no entropy. An even multiplier drops at least the low bit'
        write (output_unit, '(a)') 'of the state permanently -- the sweep below carries one as a negative control.'
        write (output_unit, '(a)') ''
        worst = M31 * (4294967296_int64 - 1_int64)
        write (output_unit, '(a)') 'Why NO OVERFLOW is possible, re-derived rather than asserted:'
        write (output_unit, '(a,i0)') '  max masked operand   (2**31 - 1)          = ', M31
        write (output_unit, '(a,i0)') '  max multiplier       (2**32 - 1)          = ', 4294967296_int64 - 1_int64
        write (output_unit, '(a,i0)') '  worst-case product                        = ', worst
        write (output_unit, '(a,i0)') '  huge(int64)                               = ', huge(1_int64)
        write (output_unit, '(a,l1)') '  product fits with room to spare:            ', worst < huge(1_int64)
        write (output_unit, '(a)') 'So ANY multiplier below 2**32 is overflow-free here. The choice of constant is'
        write (output_unit, '(a)') 'therefore a mixing question only -- never a correctness or a Risk-94 question.'
        write (output_unit, '(a)') ''
        flush (output_unit)
    end subroutine run_props

    !> Strict avalanche criterion for `perm_mix2`: one input bit flipped, all output bits watched.
    subroutine run_avalanche()
        integer, parameter :: NS = 4096
        integer(int64) :: x, key, w0, w1, d
        real(real64) :: p, worst, best, tot
        integer :: i, j, s, bit, cnt(0:31, 0:31), nworst_i, nworst_j
        real(real64) :: rowmean(0:31)
        write (output_unit, '(a)') '=========================================================='
        write (output_unit, '(a)') 'AVALANCHE -- perm_mix2, strict avalanche criterion'
        write (output_unit, '(a,i0,a)') 'For each input bit i and output bit j: P(output bit j flips | input bit i flips), over ', &
            NS, ' random inputs x 8 keys.'
        write (output_unit, '(a)') 'Ideal is 0.5 for every one of the 32x32 cells.'
        cnt = 0
        do s = 1, 8
            key = pf_random_int_at(SD0, int(s, int64), 0_int64, M32, 1_int64)
            do j = 1, NS
                x = iand(pf_random_int_at(SD0 + 11_int64, int(s, int64), 0_int64, M32, int(j, int64)), M32)
                w0 = mix2(key, x, C1_SHIPPED, C2_SHIPPED)
                do i = 0, 31
                    w1 = mix2(key, ieor(x, ishft(1_int64, i)), C1_SHIPPED, C2_SHIPPED)
                    d = ieor(w0, w1)
                    do bit = 0, 31
                        if (btest(d, bit)) cnt(i, bit) = cnt(i, bit) + 1
                    end do
                end do
            end do
        end do
        worst = 0.0_real64
        best = 1.0_real64
        tot = 0.0_real64
        nworst_i = 0
        nworst_j = 0
        rowmean = 0.0_real64
        do i = 0, 31
            do j = 0, 31
                p = real(cnt(i, j), real64) / real(8 * NS, real64)
                tot = tot + abs(p - 0.5_real64)
                rowmean(i) = rowmean(i) + abs(p - 0.5_real64) / 32.0_real64
                if (abs(p - 0.5_real64) > worst) then
                    worst = abs(p - 0.5_real64)
                    nworst_i = i
                    nworst_j = j
                end if
                best = min(best, abs(p - 0.5_real64))
            end do
        end do
        write (output_unit, '(a)') ''
        write (output_unit, '(a)') '  per-INPUT-bit mean |P - 0.5| across the 32 output bits:'
        do i = 0, 31, 8
            write (output_unit, '(a,i2,a,8f8.4)') '    bits ', i, '-', &
                (rowmean(j), j = i, i + 7)
        end do
        write (output_unit, '(a)') ''
        write (output_unit, '(a,f9.5)') '  mean |P - 0.5| over all 1024 cells   : ', tot / 1024.0_real64
        write (output_unit, '(a,f9.5,a,i0,a,i0,a)') '  worst |P - 0.5|                      : ', worst, &
            '   (input bit ', nworst_i, ' -> output bit ', nworst_j, ')'
        write (output_unit, '(a,f9.5)') '  best  |P - 0.5|                      : ', best
        write (output_unit, '(a,f9.5)') '  sampling noise, 1 sd of a p=0.5 cell : ', &
            0.5_real64 / sqrt(real(8 * NS, real64))
        write (output_unit, '(a)') ''
        write (output_unit, '(a)') 'Read a cell against the sampling noise, not against zero -- and read the ROWS, not'
        write (output_unit, '(a)') 'the worst cell: a structural gap shows up as a whole row or column, which is exactly'
        write (output_unit, '(a)') 'what the last row above is. A dead row at 0.5 means that input bit changes NOTHING.'
        write (output_unit, '(a)') ''
        write (output_unit, '(a)') 'EXPECTED, and worth stating rather than discovering twice: perm_mix2 masks its input'
        write (output_unit, '(a)') 'to 31 bits (`iand(z, M31)`) before the first multiply, so INPUT BIT 31 IS DISCARDED.'
        write (output_unit, '(a)') 'That mask is the thing making every product provably overflow-free (Risk-94), so it'
        write (output_unit, '(a)') 'is not removable. It costs nothing while the half being mixed stays below 2**31 --'
        write (output_unit, '(a)') 'which holds whenever a, b <= 2**31, i.e. for every m <= 2**62 = 4.61e18. Above that'
        write (output_unit, '(a)') 'the round function stops seeing one bit of its input. The network is still a bijection'
        write (output_unit, '(a)') '(it is one for ANY round function), so this is a mixing-quality limit at extreme m,'
        write (output_unit, '(a)') 'not a correctness limit -- and 4.61e18 elements is 37 exabytes of int64 index.'
        write (output_unit, '(a)') ''
        flush (output_unit)
    end subroutine run_avalanche

    !> The load-bearing mode: does the structural verdict track the constants, or the round count?
    subroutine run_sweep()
        integer, parameter :: NPAIR = 8
        integer(int64) :: c1(NPAIR), c2(NPAIR)
        character(len=34) :: nm(NPAIR)
        integer(int64) :: y(0:MS - 1), key
        real(real64) :: f(4, NKEYS), mu(4), sd(4), ctl_mu(4), ctl_sd(4), z, zmax
        integer :: pj, ka, ri, nr, rounds(2)

        !                                                     the two BAD pairs are deliberate: see below
        c1(1) = C1_SHIPPED;   c2(1) = C2_SHIPPED;   nm(1) = 'SHIPPED  golden-ratio + PRIME32_2 '
        c1(2) = C2_SHIPPED;   c2(2) = C1_SHIPPED;   nm(2) = 'swapped  PRIME32_2 + golden-ratio '
        c1(3) = 3266489917_int64; c2(3) = 668265263_int64;  nm(3) = 'xxHash   PRIME32_3 + PRIME32_4    '
        c1(4) = 2246822507_int64; c2(4) = 3266489909_int64; nm(4) = 'Murmur3  fmix32 pair              '
        c1(5) = 2716044179_int64; c2(5) = 1597334677_int64; nm(5) = 'other    two arbitrary odd 32-bit '
        c1(6) = 1_int64;          c2(6) = 1_int64;          nm(6) = 'BAD      both 1 (no multiply)     '
        c1(7) = 3_int64;          c2(7) = 5_int64;          nm(7) = 'BAD      tiny odd multipliers     '
        c1(8) = 2654435760_int64; c2(8) = 2246822518_int64; nm(8) = 'BAD      both EVEN (lossy)        '
        rounds = [3, 4]

        write (output_unit, '(a)') '=========================================================='
        write (output_unit, '(a)') 'SWEEP -- modular structural distinguisher over multiplier pairs'
        write (output_unit, '(a,i0,a,i0,a,i0,a,i0,a)') 'Z_', AA, ' x Z_', BB, ', all ', &
            (BB * (AA * (AA - 1_int64)) / 2_int64 + AA * (BB * (BB - 1_int64)) / 2_int64), &
            ' pairs enumerated, ', NKEYS, ' keys per arm'
        write (output_unit, '(a)') 'z is against the Fisher-Yates control, in units of the control''s own key-to-key sd.'
        write (output_unit, '(a)') ''
        write (output_unit, '(a)') '   pair                                rounds     r->l     max|z|   verdict'
        flush (output_unit)

        ! The Fisher-Yates null first -- everything below is measured against it.
        do ka = 1, NKEYS
            key = SD0 + int(ka, int64) * 104729_int64
            call fisher_yates(key, MS, y)
            call relations(y, AA, BB, f(:, ka))
        end do
        call moments(f, mu, sd)
        ctl_mu = mu
        ctl_sd = sd
        write (output_unit, '(a,a,a,f11.5,a)') '   ', 'Fisher-Yates (the null)           ', &
            '      -', mu(1), '          -   (control)'

        do pj = 1, NPAIR
            do nr = 1, 2
                do ka = 1, NKEYS
                    key = SD0 + int(ka, int64) * 104729_int64
                    call build_perm(key, rounds(nr), AA, BB, c1(pj), c2(pj), y)
                    call relations(y, AA, BB, f(:, ka))
                end do
                call moments(f, mu, sd)
                zmax = 0.0_real64
                do ri = 1, 4
                    if (ctl_sd(ri) > 0.0_real64) &
                        zmax = max(zmax, abs(mu(ri) - ctl_mu(ri)) / (ctl_sd(ri) / sqrt(real(NKEYS, real64))))
                end do
                write (output_unit, '(a,a,i6,f11.5,f11.2,a)') '   ', nm(pj), rounds(nr), mu(1), zmax, &
                    merge('   clean   ', '  DETECTED ', zmax < 5.0_real64)
                flush (output_unit)
            end do
        end do

        write (output_unit, '(a)') ''
        write (output_unit, '(a)') 'How to read this table:'
        write (output_unit, '(a)') '  * Every CONVENTIONAL pair should be clean at 4 rounds and detected at 3. That is'
        write (output_unit, '(a)') '    the result being sought: the verdict tracks the ROUND COUNT, not the constants,'
        write (output_unit, '(a)') '    so the shipped clearance does not rest on a lucky choice.'
        write (output_unit, '(a)') '  * The three BAD rows are the negative control. Without them a table of all-clean'
        write (output_unit, '(a)') '    rows would be equally consistent with a distinguisher that cannot see anything.'
        write (output_unit, '(a)') '  * The two tiny-multiplier rows come out BYTE-IDENTICAL, and that is the finding'
        write (output_unit, '(a)') '    rather than a bug. The width rule uses the round function through'
        write (output_unit, '(a)') '        t = l + ishft(iand(F, M31) * p, -31)'
        write (output_unit, '(a)') '    which is a fixed-point multiply into [0, p) -- so at p = 32 only the TOP FIVE'
        write (output_unit, '(a)') '    bits of F are read. A multiplier''s real job is therefore to carry the low bits'
        write (output_unit, '(a)') '    of r UP into the top of the word. With c = 1 or c = 3, 5 and r < 32, the top'
        write (output_unit, '(a)') '    bits never move, F is constant in r, and the network degenerates identically in'
        write (output_unit, '(a)') '    both cases. THAT is why the constant must be large and odd, and it is a sharper'
        write (output_unit, '(a)') '    reason than "conventional": a golden-ratio-sized odd multiplier is chosen'
        write (output_unit, '(a)') '    precisely because its product spreads every input bit across the whole word.'
        write (output_unit, '(a)') '  * Note the corollary: small m tests the multiplier HARDEST here, because a small p'
        write (output_unit, '(a)') '    reads fewer bits of F. At large m, p approaches 2**31 and nearly every bit is read.'
        write (output_unit, '(a)') '  * A multiplier of 1 removes the multiply entirely; tiny odds mix only the low bits;'
        write (output_unit, '(a)') '    an even multiplier is not a bijection mod 2**k and loses state every round.'
        write (output_unit, '(a)') ''
        flush (output_unit)
    end subroutine run_sweep

    ! ============================================================================================
    ! Shared statistics
    ! ============================================================================================

    !> Per-relation mean and key-to-key standard deviation.
    pure subroutine moments(f, mu, sd)
        real(real64), intent(in) :: f(:, :)         !! relation x key
        real(real64), intent(out) :: mu(4)          !! per-relation mean
        real(real64), intent(out) :: sd(4)          !! per-relation sd across keys
        integer :: ri, n
        n = size(f, 2)
        do ri = 1, 4
            mu(ri) = sum(f(ri, :)) / real(n, real64)
            sd(ri) = sqrt(sum((f(ri, :) - mu(ri))**2) / real(n - 1, real64))
        end do
    end subroutine moments

    !> The four structural relations, over all pairs sharing an input component. `y` is 0-based.
    pure subroutine relations(y, a, b, f)
        integer(int64), intent(in) :: y(0:)         !! the permutation
        integer(int64), intent(in) :: a             !! left factor
        integer(int64), intent(in) :: b             !! right factor
        real(real64), intent(out) :: f(4)           !! r->l, r->r, l->l, l->r
        integer(int64) :: l1, l2, r1, r2, y1, y2, hit(4), npair(4)
        hit = 0_int64
        npair = 0_int64
        do r1 = 0_int64, b - 1_int64
            do l1 = 0_int64, a - 2_int64
                y1 = y(l1 * b + r1)
                do l2 = l1 + 1_int64, a - 1_int64
                    y2 = y(l2 * b + r1)
                    npair(1) = npair(1) + 1_int64
                    if (y1 / b == y2 / b) hit(1) = hit(1) + 1_int64
                    if (modulo(y1, b) == modulo(y2, b)) hit(2) = hit(2) + 1_int64
                end do
            end do
        end do
        npair(2) = npair(1)
        do l1 = 0_int64, a - 1_int64
            do r1 = 0_int64, b - 2_int64
                y1 = y(l1 * b + r1)
                do r2 = r1 + 1_int64, b - 1_int64
                    y2 = y(l1 * b + r2)
                    npair(3) = npair(3) + 1_int64
                    if (y1 / b == y2 / b) hit(3) = hit(3) + 1_int64
                    if (modulo(y1, b) == modulo(y2, b)) hit(4) = hit(4) + 1_int64
                end do
            end do
        end do
        npair(4) = npair(3)
        f = real(hit, real64) / real(npair, real64)
    end subroutine relations

    !> A uniform permutation of `0 .. m-1`, the calibration null.
    subroutine fisher_yates(seed, m, y)
        integer(int64), intent(in) :: seed          !! the seed
        integer(int64), intent(in) :: m             !! population size
        integer(int64), intent(out) :: y(0:)        !! filled with a permutation of `0 .. m-1`
        integer(int64) :: j, r, tmp
        do j = 0_int64, m - 1_int64
            y(j) = j
        end do
        do j = 1_int64, m
            r = pf_random_int_at(seed, 0_int64, j, m, j)
            tmp = y(j - 1_int64)
            y(j - 1_int64) = y(r - 1_int64)
            y(r - 1_int64) = tmp
        end do
    end subroutine fisher_yates

end program probe_random_mix2
