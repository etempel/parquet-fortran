!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> Phase-2 measurement 6: should `pf_random_permutation`/`pf_random_subset` be a keyed bijection?
!!
!! `feature_random_phase2.md` §3.5.2 proposes replacing the partial Fisher-Yates with a **Feistel
!! network keyed by the seed**, cycle-walked onto `[0, m)`. That makes `perm(k)` a pure function of
!! `(seed, m, k)`: O(1) memory, embarrassingly parallel, and coordinate-addressed like the rest of
!! the module. The cost is that a round-limited Feistel is **not** uniform over all `m!`
!! permutations, so the round count is a statistical question, and it is frozen contract.
!!
!! This probe answers all three parts of §11 measurement 6:
!!
!!   1. **Throughput** against dense Fisher-Yates at m = 10**6, 10**7, 10**8.
!!   2. **Thread scaling**, 1 to 64 threads -- the whole point of the design.
!!   3. **Statistical quality by round count**, against oracles for a PERMUTATION rather than a
!!      stream: fixed-point count, cycle count, position-value uniformity, subset membership, and a
!!      structural distinguisher that is exact for two rounds.
!!
!! **Fisher-Yates is the control throughout, not merely a rival.** Driven by the same generator it
!! samples uniformly from all `m!`, so running identical statistics on both arms is what turns "is
!! the Feistel uniform?" into "is it distinguishable from the construction we would otherwise
!! ship?" -- a question the same code can answer for both.
!!
!! The round function is this module's own kernel (`parquet_debug_random_block`), keyed by the seed
!! with the round number as the stream, so nothing here is a second cipher.
!!
!! Not run by `fpm test`, by design (CLAUDE.md, "Manual (never-`fpm test`) large-scale/benchmark
!! tools"). Build and run with `--profile release`.
module pf_probe_feistel

    use iso_fortran_env, only: int32, int64, real64
    use parquet, only: parquet_debug_random_block, pf_random_int_at

    implicit none

contains

    !> Smallest EVEN bit width whose domain covers `m`.
    !!
    !! Even so that the two Feistel halves are equal, which is what makes the network provably a
    !! bijection with the simplest possible round structure. The price is cycle-walking: the domain
    !! can be up to 4x `m`, so a walk costs up to about 4 applications on average. An unbalanced
    !! network would allow an odd `b` and roughly halve that at the sizes where it bites -- noted
    !! here because the measured walk cost below is the argument for or against bothering.
    pure function width_for(m) result(b)
        integer(int64), intent(in) :: m
        integer :: b
        integer(int64) :: cap
        b = 2
        cap = 4_int64
        do while (cap < m)
            b = b + 2
            cap = cap * 4_int64
        end do
    end function width_for

    !> One application of the Feistel network on `[0, 2**b)`; a bijection for any round function.
    pure function feistel_raw(key, rounds, h, x) result(y)
        integer(int64), intent(in) :: key           !! the seed
        integer, intent(in) :: rounds               !! how many rounds
        integer, intent(in) :: h                    !! half-width in bits
        integer(int64), intent(in) :: x             !! input in `[0, 2**(2h))`
        integer(int64) :: y                         !! output in `[0, 2**(2h))`
        integer(int64) :: l, r, t, mask, w0, w1, w2, w3
        integer :: k
        mask = ishft(1_int64, h) - 1_int64
        l = iand(ishft(x, -h), mask)
        r = iand(x, mask)
        do k = 1, rounds
            call parquet_debug_random_block(key, int(k, int64), r, w0, w1, w2, w3)
            t = ieor(l, iand(w0, mask))
            l = r
            r = t
        end do
        y = ior(ishft(l, h), r)
    end function feistel_raw

    !> The cycle-walked bijection on `[0, m)`: apply the network until the value is in range.
    !!
    !! Deterministic and exactly bijective -- the walk follows the network's own orbit, and every
    !! orbit returns, so a value in `[0, m)` maps to a value in `[0, m)` and no two collide. The
    !! trial count is reported so the walk's real cost is visible rather than assumed.
    pure subroutine feistel_at(key, m, rounds, h, k, v, trials)
        integer(int64), intent(in) :: key           !! the seed
        integer(int64), intent(in) :: m             !! population size
        integer, intent(in) :: rounds               !! how many rounds
        integer, intent(in) :: h                    !! half-width in bits
        integer(int64), intent(in) :: k             !! input in `[0, m)`
        integer(int64), intent(out) :: v            !! output in `[0, m)`
        integer, intent(out) :: trials              !! network applications this input needed
        v = k
        trials = 0
        do
            v = feistel_raw(key, rounds, h, v)
            trials = trials + 1
            if (v < m) exit
        end do
    end subroutine feistel_at

    !> Smallest bit width whose domain covers `m` -- no evenness requirement.
    pure function bits_for(m) result(b)
        integer(int64), intent(in) :: m
        integer :: b
        integer(int64) :: cap
        b = 1
        cap = 2_int64
        do while (cap < m)
            b = b + 1
            cap = cap * 2_int64
        end do
    end function bits_for

    !> Integer square root, rounded up: the left factor of the modular variant's domain.
    pure function ceil_sqrt(m) result(a)
        integer(int64), intent(in) :: m
        integer(int64) :: a
        a = int(sqrt(real(m, real64)), int64)
        do while (a * a < m)
            a = a + 1_int64
        end do
        do while ((a - 1_int64) * (a - 1_int64) >= m .and. a > 1_int64)
            a = a - 1_int64
        end do
    end function ceil_sqrt

    !> UNBALANCED power-of-two Feistel: halves of `hl` and `hr` bits, `hl + hr` exactly `bits_for(m)`.
    !!
    !! Dropping the evenness requirement is the whole point -- the domain becomes `2**bits_for(m)`
    !! rather than up to four times `m`, which is where the balanced form's cycle-walk cost comes
    !! from. The two halves swap widths every round, so the round count must be **even** for the
    !! output to be encoded the same way as the input; 4 is.
    pure function feistel_raw_unbal(key, rounds, hl, hr, x) result(y)
        integer(int64), intent(in) :: key           !! the seed
        integer, intent(in) :: rounds               !! how many rounds; must be even
        integer, intent(in) :: hl                   !! left half width in bits
        integer, intent(in) :: hr                   !! right half width in bits
        integer(int64), intent(in) :: x             !! input in `[0, 2**(hl+hr))`
        integer(int64) :: y                         !! output in the same domain
        integer(int64) :: l, r, t, w0, w1, w2, w3
        integer :: k, p, q, sw
        p = hl
        q = hr
        l = ishft(x, -q)
        r = iand(x, ishft(1_int64, q) - 1_int64)
        do k = 1, rounds
            call parquet_debug_random_block(key, int(k, int64), r, w0, w1, w2, w3)
            t = ieor(l, iand(w0, ishft(1_int64, p) - 1_int64))
            l = r
            r = t
            sw = p
            p = q
            q = sw
        end do
        y = ior(ishft(l, q), r)
    end function feistel_raw_unbal

    !> MODULAR Feistel on `Z_a x Z_b`, where `a*b` can sit essentially on top of `m`.
    !!
    !! Black-Rogaway's construction for an arbitrary finite domain. Where the power-of-two forms are
    !! stuck with a domain that is a power of two, this one picks `a = ceil(sqrt(m))` and
    !! `b = ceil(m/a)`, so the walk very nearly disappears. The price is a `mod` by a runtime value
    !! per round -- a hardware division, against a mask for the other two.
    !!
    !! The round function's `mod p` is slightly biased (a 32-bit word reduced mod ~10**4), which is
    !! harmless: a Feistel is a bijection for **any** round function, and the bias is about 2**-18.
    pure function feistel_raw_mod(key, rounds, a, b, x) result(y)
        integer(int64), intent(in) :: key           !! the seed
        integer, intent(in) :: rounds               !! how many rounds; must be even
        integer(int64), intent(in) :: a             !! left factor
        integer(int64), intent(in) :: b             !! right factor
        integer(int64), intent(in) :: x             !! input in `[0, a*b)`
        integer(int64) :: y                         !! output in the same domain
        integer(int64) :: l, r, t, w0, w1, w2, w3, p, q, sw
        integer :: k
        p = a
        q = b
        l = x / q
        r = modulo(x, q)
        do k = 1, rounds
            call parquet_debug_random_block(key, int(k, int64), r, w0, w1, w2, w3)
            t = modulo(l + modulo(w0, p), p)
            l = r
            r = t
            sw = p
            p = q
            q = sw
        end do
        y = l * q + r
    end function feistel_raw_mod

    !> SplitMix64's finaliser, copied here so the probe can use it as a cheap round function.
    !!
    !! `parquet_random`'s own `mix64` is private; if this variant is adopted the module would use
    !! that one rather than a second copy.
    pure function pmix64(x) result(z)
        integer(int64), intent(in) :: x
        integer(int64) :: z
        integer(int64), parameter :: A = ior(ishft(int(z'BF58476D', int64), 32), int(z'1CE4E5B9', int64))
        integer(int64), parameter :: B = ior(ishft(int(z'94D049BB', int64), 32), int(z'133111EB', int64))
        z = ieor(x, ishft(x, -30))
        z = z * A
        z = ieor(z, ishft(z, -27))
        z = z * B
        z = ieor(z, ishft(z, -31))
    end function pmix64

    !> A two-multiply keyed mixer that is **UB-free by construction**, not by measurement.
    !!
    !! `feature_risks.md` Risk-94 records this repository being caught with a wrapping multiply that
    !! measured correct while the optimiser used the overflow's undefinedness to delete a branch two
    !! functions away. So a new round function must not overflow at all. Every operand is masked to
    !! **31 bits** before each multiply and the constants are below `2**32`, so each product is
    !! bounded by `(2**31 - 1) * (2**32 - 1) < 2**63` and no signed overflow is possible.
    !!
    !! This is the difference between a probe result and a shippable kernel: the earlier `pmix64`
    !! arm in `run_opt` is SplitMix64's finaliser with plain 64-bit multiplies, which overflow and
    !! could not be adopted as written. `src/parquet_random.f90`'s own `mix64` avoids that through
    !! `mul64_lo_strict`, whose `#else` arm builds the product from 16-bit limbs -- correct, and far
    !! too expensive for a per-round hot path. Masking is the cheap way to get the same guarantee.
    pure function mix2(rk, x) result(w)
        integer(int64), intent(in) :: rk            !! round key
        integer(int64), intent(in) :: x             !! input, below `2**32`
        integer(int64) :: w                         !! mixed word, below `2**32`
        integer(int64), parameter :: M31 = 2147483647_int64
        integer(int64), parameter :: M32 = 4294967295_int64
        integer(int64), parameter :: C1 = 2654435761_int64      ! Knuth's 32-bit golden-ratio odd
        integer(int64), parameter :: C2 = 2246822519_int64      ! xxHash prime 3, odd, < 2**32
        integer(int64) :: z
        z = ieor(x, rk)
        z = iand(z, M31) * C1
        z = ieor(z, ishft(z, -29))
        z = iand(z, M31) * C2
        z = ieor(z, ishft(z, -31))
        w = iand(z, M32)
    end function mix2

    !> Round keys for `mix2`, derived once per permutation.
    pure subroutine mix2_keys(seed, rounds, rk)
        integer(int64), intent(in) :: seed          !! the seed
        integer, intent(in) :: rounds               !! how many rounds
        integer(int64), intent(out) :: rk(:)        !! one key per round
        integer :: k
        do k = 1, rounds
            rk(k) = mix2(int(k, int64) * 2654435761_int64, iand(seed, 4294967295_int64))
            rk(k) = ieor(rk(k), mix2(int(k, int64), ishft(seed, -32)))
        end do
    end subroutine mix2_keys

    !> The shippable candidate: modular Feistel, division-free, UB-free `mix2` round function.
    pure function feistel_mix2(rk, rounds, a, b, x) result(y)
        integer(int64), intent(in) :: rk(:)         !! per-permutation round keys
        integer, intent(in) :: rounds               !! how many rounds; must be even
        integer(int64), intent(in) :: a             !! left factor
        integer(int64), intent(in) :: b             !! right factor
        integer(int64), intent(in) :: x             !! input in `[0, a*b)`
        integer(int64) :: y                         !! output in the same domain
        integer(int64) :: l, r, t, p, q, sw
        integer :: k
        p = a
        q = b
        l = x / q
        r = x - l * q
        do k = 1, rounds
            t = l + ishft(iand(mix2(rk(k), r), 2147483647_int64) * p, -31)
            if (t >= p) t = t - p
            l = r
            r = t
            sw = p
            p = q
            q = sw
        end do
        y = l * q + r
    end function feistel_mix2

    !> Cycle-walked `mix2` bijection.
    pure subroutine mix2_at(rk, m, rounds, a, b, k, v, trials)
        integer(int64), intent(in) :: rk(:)         !! per-permutation round keys
        integer(int64), intent(in) :: m             !! population size
        integer, intent(in) :: rounds               !! how many rounds
        integer(int64), intent(in) :: a             !! left factor
        integer(int64), intent(in) :: b             !! right factor
        integer(int64), intent(in) :: k             !! input in `[0, m)`
        integer(int64), intent(out) :: v            !! output in `[0, m)`
        integer, intent(out) :: trials              !! network applications used
        v = k
        trials = 0
        do
            v = feistel_mix2(rk, rounds, a, b, v)
            trials = trials + 1
            if (v < m) exit
        end do
    end subroutine mix2_at

    !> LEVER 5: a blocked, branch-free bulk fill, written so a compiler can vectorise it.
    !!
    !! Elements are mutually independent, `mix2` is integer multiply-and-shift with no memory access
    !! and no branch, and under the modular rule the cycle-walk is taken essentially never (mean
    !! applications 1.0000). So a block of `k` can be driven through the rounds as array expressions
    !! and the walk handled afterwards as a rare fix-up over whatever landed out of range.
    !!
    !! This form exists only because Fisher-Yates is gone: a shuffle is inherently sequential, so
    !! there was no vectorisable shape to compare against while it was still in the design.
    pure subroutine perm_fill_block(rk, m, rounds, a, b, k0, v)
        integer(int64), intent(in) :: rk(:)         !! per-permutation round keys
        integer(int64), intent(in) :: m             !! population size
        integer, intent(in) :: rounds               !! how many rounds; must be even
        integer(int64), intent(in) :: a             !! left factor
        integer(int64), intent(in) :: b             !! right factor
        integer(int64), intent(in) :: k0            !! first input index, 0-based
        integer(int64), intent(inout) :: v(:)       !! filled with the images of `k0 .. k0+size(v)-1`
        integer(int64) :: l(size(v)), r(size(v)), t(size(v)), w(size(v))
        integer(int64), parameter :: M31 = 2147483647_int64
        integer(int64), parameter :: M32 = 4294967295_int64
        integer(int64), parameter :: C1 = 2654435761_int64
        integer(int64), parameter :: C2 = 2246822519_int64
        integer(int64) :: p, q, sw, x
        integer :: k, j, n, trials
        n = size(v)
        do j = 1, n
            v(j) = k0 + int(j, int64) - 1_int64
        end do
        p = a
        q = b
        l = v / q
        r = v - l * q
        do k = 1, rounds
            w = ieor(r, rk(k))
            w = iand(w, M31) * C1
            w = ieor(w, ishft(w, -29))
            w = iand(w, M31) * C2
            w = ieor(w, ishft(w, -31))
            w = iand(w, M32)
            t = l + ishft(w * p, -32)
            where (t >= p) t = t - p
            l = r
            r = t
            sw = p
            p = q
            q = sw
        end do
        v = l * q + r
        ! Fix-up: whatever fell outside `[0, m)` walks serially. Under the modular rule this is
        ! empty whenever `a*b == m`, and a handful of elements otherwise.
        do j = 1, n
            if (v(j) >= m) then
                x = v(j)
                do
                    x = feistel_mix2(rk, rounds, a, b, x)
                    if (x < m) exit
                end do
                v(j) = x
            end if
        end do
        trials = 0
    end subroutine perm_fill_block

    !> LEVER 6, done properly: a bulk fill that needs NO division at all.
    !!
    !! The scalar entry point must split `k` into `(l, r)` with `l = k / b`, and no SIMD unit has an
    !! integer divide -- which is what stops `perm_fill_block` above from vectorising and is why it
    !! matched the scalar form at every size but the smallest.
    !!
    !! A bulk fill does not need the division at all. As `k` runs `0 .. m-1`, the pair `(l, r)` runs
    !! `l = 0 .. a-1` crossed with `r = 0 .. b-1` in exactly that order, so the outer loop supplies
    !! `l` as a scalar and the inner one supplies `r` as a contiguous vector. **The split is loop
    !! structure rather than arithmetic**, and what remains inside the rounds is multiply, shift, xor
    !! and one masked compare -- all of which vectorise.
    !!
    !! The output index is `l*b + r + 1`, so writes stay sequential.
    pure subroutine perm_fill_lr(rk, m, rounds, a, b, v)
        integer(int64), intent(in) :: rk(:)         !! per-permutation round keys
        integer(int64), intent(in) :: m             !! population size
        integer, intent(in) :: rounds               !! how many rounds; must be even
        integer(int64), intent(in) :: a             !! left factor
        integer(int64), intent(in) :: b             !! right factor
        integer(int64), intent(inout) :: v(:)       !! filled with the image of `k` at `v(k+1)`
        integer(int64), parameter :: M31 = 2147483647_int64
        integer(int64), parameter :: M32 = 4294967295_int64
        integer(int64), parameter :: C1 = 2654435761_int64
        integer(int64), parameter :: C2 = 2246822519_int64
        integer(int64) :: lo, r0, n, base, p, q, sw, x
        integer(int64) :: l(1024), r(1024), t(1024), w(1024)
        integer :: k
        integer(int64) :: j
        do lo = 0_int64, a - 1_int64
            r0 = 0_int64
            do while (r0 < b)
                n = min(1024_int64, b - r0)
                base = lo * b + r0
                if (base >= m) exit
                if (base + n > m) n = m - base
                do j = 1_int64, n
                    l(j) = lo
                    r(j) = r0 + j - 1_int64
                end do
                p = a
                q = b
                do k = 1, rounds
                    w(1:n) = ieor(r(1:n), rk(k))
                    w(1:n) = iand(w(1:n), M31) * C1
                    w(1:n) = ieor(w(1:n), ishft(w(1:n), -29))
                    w(1:n) = iand(w(1:n), M31) * C2
                    w(1:n) = ieor(w(1:n), ishft(w(1:n), -31))
                    w(1:n) = iand(w(1:n), M32)
                    t(1:n) = l(1:n) + ishft(iand(w(1:n), M31) * p, -31)
                    where (t(1:n) >= p) t(1:n) = t(1:n) - p
                    l(1:n) = r(1:n)
                    r(1:n) = t(1:n)
                    sw = p
                    p = q
                    q = sw
                end do
                v(base + 1_int64:base + n) = l(1:n) * q + r(1:n)
                ! Rare fix-up: anything outside `[0, m)` walks serially. Empty when `a*b == m`.
                do j = 1_int64, n
                    if (v(base + j) >= m) then
                        x = v(base + j)
                        do
                            x = feistel_mix2(rk, rounds, a, b, x)
                            if (x < m) exit
                        end do
                        v(base + j) = x
                    end if
                end do
                r0 = r0 + n
            end do
        end do
    end subroutine perm_fill_lr

    !> Modular Feistel with **no divisions**, same round function (a full Philox block) as before.
    !!
    !! Two `modulo` calls per round become zero, and neither change touches the round function:
    !!
    !!   * `modulo(l + f, p)` -> `t = l + f; if (t >= p) t = t - p`. Exact, because `l < p` and
    !!     `f < p` give `t < 2p`, so at most one subtraction is ever needed.
    !!   * `modulo(w, p)` -> `ishft(iand(w, M32) * p, -32)`, Lemire's multiply-shift. This maps
    !!     `[0, 2**32)` onto `[0, p)` with a bias of about `p / 2**32`; a Feistel is a bijection for
    !!     **any** round function, so a slightly non-uniform `F` costs nothing but a little mixing.
    !!     Safe while `p < 2**31`, i.e. `m < 2**62`, since the product must stay under `2**63`.
    pure function feistel_mod_fast(key, rounds, a, b, x) result(y)
        integer(int64), intent(in) :: key           !! the seed
        integer, intent(in) :: rounds               !! how many rounds; must be even
        integer(int64), intent(in) :: a             !! left factor
        integer(int64), intent(in) :: b             !! right factor
        integer(int64), intent(in) :: x             !! input in `[0, a*b)`
        integer(int64) :: y                         !! output in the same domain
        integer(int64) :: l, r, t, w0, w1, w2, w3, p, q, sw
        integer :: k
        p = a
        q = b
        l = x / q
        r = x - l * q                               ! one division for the split, not two
        do k = 1, rounds
            call parquet_debug_random_block(key, int(k, int64), r, w0, w1, w2, w3)
            t = l + ishft(iand(w0, 4294967295_int64) * p, -32)
            if (t >= p) t = t - p
            l = r
            r = t
            sw = p
            p = q
            q = sw
        end do
        y = l * q + r
    end function feistel_mod_fast

    !> The same network with a **cheap round function**: one `pmix64` instead of a Philox block.
    !!
    !! A Feistel is a bijection for any `F`, so `F` need not be a cipher -- it needs enough mixing
    !! that four rounds are indistinguishable from a random permutation. Philox4x32-10 runs **ten**
    !! cipher rounds and produces four words of which this construction used one, so it was providing
    !! roughly forty times the mixing per round that the network can consume. The round keys are
    !! derived once per permutation and passed in, so the per-element cost is one multiply-heavy
    !! mixer per round.
    !!
    !! **This changes what the permutation is**, so it is a statistical question and not only a
    !! performance one: the battery in section 3.5.2.1 has to be re-run against it before it could be
    !! considered, which is exactly what the `stats` mode below does.
    pure function feistel_mix(rk, rounds, a, b, x) result(y)
        integer(int64), intent(in) :: rk(:)         !! per-permutation round keys, one per round
        integer, intent(in) :: rounds               !! how many rounds; must be even
        integer(int64), intent(in) :: a             !! left factor
        integer(int64), intent(in) :: b             !! right factor
        integer(int64), intent(in) :: x             !! input in `[0, a*b)`
        integer(int64) :: y                         !! output in the same domain
        integer(int64) :: l, r, t, w, p, q, sw
        integer :: k
        p = a
        q = b
        l = x / q
        r = x - l * q
        do k = 1, rounds
            w = pmix64(ieor(rk(k), r))
            t = l + ishft(iand(ishft(w, -32), 4294967295_int64) * p, -32)
            if (t >= p) t = t - p
            l = r
            r = t
            sw = p
            p = q
            q = sw
        end do
        y = l * q + r
    end function feistel_mix

    !> Round keys for `feistel_mix`, derived once per permutation from the seed.
    pure subroutine mix_round_keys(seed, rounds, rk)
        integer(int64), intent(in) :: seed          !! the seed
        integer, intent(in) :: rounds               !! how many rounds
        integer(int64), intent(out) :: rk(:)        !! one key per round
        integer :: k
        do k = 1, rounds
            rk(k) = pmix64(ieor(pmix64(seed), int(k, int64) * 2654435761_int64))
        end do
    end subroutine mix_round_keys

    !> Cycle-walked division-free modular bijection.
    pure subroutine mod_fast_at(key, m, rounds, a, b, k, v, trials)
        integer(int64), intent(in) :: key           !! the seed
        integer(int64), intent(in) :: m             !! population size
        integer, intent(in) :: rounds               !! how many rounds
        integer(int64), intent(in) :: a             !! left factor
        integer(int64), intent(in) :: b             !! right factor
        integer(int64), intent(in) :: k             !! input in `[0, m)`
        integer(int64), intent(out) :: v            !! output in `[0, m)`
        integer, intent(out) :: trials              !! network applications used
        v = k
        trials = 0
        do
            v = feistel_mod_fast(key, rounds, a, b, v)
            trials = trials + 1
            if (v < m) exit
        end do
    end subroutine mod_fast_at

    !> Cycle-walked cheap-round-function bijection.
    pure subroutine mix_at(rk, m, rounds, a, b, k, v, trials)
        integer(int64), intent(in) :: rk(:)         !! per-permutation round keys
        integer(int64), intent(in) :: m             !! population size
        integer, intent(in) :: rounds               !! how many rounds
        integer(int64), intent(in) :: a             !! left factor
        integer(int64), intent(in) :: b             !! right factor
        integer(int64), intent(in) :: k             !! input in `[0, m)`
        integer(int64), intent(out) :: v            !! output in `[0, m)`
        integer, intent(out) :: trials              !! network applications used
        v = k
        trials = 0
        do
            v = feistel_mix(rk, rounds, a, b, v)
            trials = trials + 1
            if (v < m) exit
        end do
    end subroutine mix_at

    !> Cycle-walked unbalanced power-of-two bijection on `[0, m)`.
    pure subroutine unbal_at(key, m, rounds, hl, hr, k, v, trials)
        integer(int64), intent(in) :: key           !! the seed
        integer(int64), intent(in) :: m             !! population size
        integer, intent(in) :: rounds               !! how many rounds
        integer, intent(in) :: hl                   !! left half width
        integer, intent(in) :: hr                   !! right half width
        integer(int64), intent(in) :: k             !! input in `[0, m)`
        integer(int64), intent(out) :: v            !! output in `[0, m)`
        integer, intent(out) :: trials              !! network applications used
        v = k
        trials = 0
        do
            v = feistel_raw_unbal(key, rounds, hl, hr, v)
            trials = trials + 1
            if (v < m) exit
        end do
    end subroutine unbal_at

    !> Cycle-walked modular bijection on `[0, m)`.
    pure subroutine mod_at(key, m, rounds, a, b, k, v, trials)
        integer(int64), intent(in) :: key           !! the seed
        integer(int64), intent(in) :: m             !! population size
        integer, intent(in) :: rounds               !! how many rounds
        integer(int64), intent(in) :: a             !! left factor
        integer(int64), intent(in) :: b             !! right factor
        integer(int64), intent(in) :: k             !! input in `[0, m)`
        integer(int64), intent(out) :: v            !! output in `[0, m)`
        integer, intent(out) :: trials              !! network applications used
        v = k
        trials = 0
        do
            v = feistel_raw_mod(key, rounds, a, b, v)
            trials = trials + 1
            if (v < m) exit
        end do
    end subroutine mod_at

    !> A dense partial Fisher-Yates over coordinate-addressed draws: the control construction.
    subroutine fy_permutation(seed, m, perm)
        integer(int64), intent(in) :: seed          !! the seed
        integer(int64), intent(in) :: m             !! population size
        integer(int64), intent(out) :: perm(:)      !! filled with a permutation of `0 .. m-1`
        integer(int64) :: j, r, tmp
        do j = 1_int64, m
            perm(j) = j - 1_int64
        end do
        do j = 1_int64, m
            r = pf_random_int_at(seed, 0_int64, j, m, j)
            ! `tmp` is NOT optional: by step j, slot j may already hold a value some earlier step
            ! swapped in, so writing the literal `j-1` here would duplicate one value and lose
            ! another. The result still looks plausible and is not a permutation -- it hung the
            ! cycle-count walk below, which was the only thing that noticed.
            tmp = perm(j)
            perm(j) = perm(r)
            perm(r) = tmp
        end do
    end subroutine fy_permutation

end module pf_probe_feistel

!> Drives the throughput, thread-scaling and statistical arms.
program probe_random_feistel

    use iso_fortran_env, only: int8, int32, int64, real64, output_unit
    use pf_probe_feistel
    use parquet, only: pf_random_int_at
#ifdef _OPENMP
    use omp_lib, only: omp_get_wtime, omp_get_max_threads
#endif

    implicit none

    integer(int64), parameter :: SEED = 20260816_int64
    integer :: g_rounds_default = 4
    integer(int64) :: g_shift = 0_int64
    integer :: thread_list(7) = [1, 2, 4, 8, 16, 32, 64]
    integer :: max_threads
    character(len=32) :: mode

    max_threads = 1
#ifdef _OPENMP
    max_threads = omp_get_max_threads()
#endif

    call get_mode(mode)

    write(output_unit, '(a)') 'probe_random_feistel: keyed bijection against partial Fisher-Yates'
#ifdef _OPENMP
    write(output_unit, '(a,i0)') 'OpenMP available, omp_get_max_threads() = ', max_threads
#else
    write(output_unit, '(a)') 'built WITHOUT OpenMP -- the thread sweep will report one thread only'
#endif
    write(output_unit, '(a,i0)') 'default rounds = ', g_rounds_default
    write(output_unit, '(a)') ''
    flush(output_unit)

    if (mode == 'stats' .or. mode == 'all') call run_statistics()
    if (mode == 'width' .or. mode == 'all') call run_width()
    if (mode == 'scale' .or. mode == 'all') call run_scale()
    if (mode == 'cores' .or. mode == 'all') call run_cores()
    if (mode == 'opt' .or. mode == 'all') call run_opt()
    if (mode == 'prog' .or. mode == 'all') call run_prog()
    if (mode == 'struct' .or. mode == 'all') call run_struct()
    if (mode == 'speed' .or. mode == 'all') call run_speed()

contains

    subroutine get_mode(m)
        character(len=*), intent(out) :: m
        integer :: k
        character(len=64) :: buf
        m = 'all'
        do k = 1, command_argument_count()
            call get_command_argument(k, buf)
            if (index(buf, '--mode=') == 1) m = trim(buf(8:))
            if (index(buf, '--rounds=') == 1) read(buf(10:), *) g_rounds_default
            if (index(buf, '--shift=') == 1) read(buf(9:), *) g_shift
        end do
    end subroutine get_mode

    real(real64) function wall()
#ifdef _OPENMP
        wall = omp_get_wtime()
#else
        block
            integer(int64) :: t, rate
            call system_clock(t, rate)
            wall = real(t, real64) / real(rate, real64)
        end block
#endif
    end function wall

    ! ================================================================================
    ! Part 3 -- statistical quality by round count
    ! ================================================================================

    subroutine run_statistics()
        integer(int64), parameter :: M = 1000_int64
        integer, parameter :: NSEED = 4000
        integer :: rounds_list(5) = [2, 3, 4, 6, 8]
        integer :: ri

        write(output_unit, '(a)') '=========================================================='
        write(output_unit, '(a)') 'PART 3 -- statistical quality of the permutation'
        write(output_unit, '(a,i0,a,i0,a)') 'm = ', M, ', independent permutations = ', NSEED, &
            ' (one per seed)'
        write(output_unit, '(a)') ''
        write(output_unit, '(a)') 'chi2 columns: a value far above its df is a detected departure.'
        write(output_unit, '(a)') 'fix   = fixed-point count vs Poisson(1),        df 5'
        write(output_unit, '(a)') 'pos   = value at position 0 vs uniform, 20 bins, df 19'
        write(output_unit, '(a)') 'sub   = membership of the first 4 elements,      df 19'
        write(output_unit, '(a)') 'cyc   = mean cycle count (expected H_m = 7.485)'
        write(output_unit, '(a)') 'struct= P(same output high half | same input low half), ALL pairs;'
        write(output_unit, '(a)') '        a random permutation of the 1024-element domain gives 31/1023 = 0.03030;'
        write(output_unit, '(a)') '        the control has no halves, so it reports -1 rather than a number'
        write(output_unit, '(a)') ''
        write(output_unit, '(a)') '   arm        rounds       fix        pos        sub      cyc     struct'
        flush(output_unit)

        call stats_arm(M, NSEED, -1, 'Fisher-Yates')
        do ri = 1, size(rounds_list)
            call stats_arm(M, NSEED, rounds_list(ri), 'bal-pow2    ', 1)
        end do
        call stats_arm(M, NSEED, 2, 'modular     ', 3)
        call stats_arm(M, NSEED, 3, 'modular     ', 3)
        call stats_arm(M, NSEED, 4, 'modular     ', 3)
        call stats_arm(M, NSEED, 6, 'modular     ', 3)
        call stats_arm(M, NSEED, 4, 'unbal-pow2  ', 2)
        call stats_arm(M, NSEED, 4, 'mod-nodiv   ', 4)
        call stats_arm(M, NSEED, 2, 'cheap-round ', 5)
        call stats_arm(M, NSEED, 3, 'cheap-round ', 5)
        call stats_arm(M, NSEED, 4, 'cheap-round ', 5)
        call stats_arm(M, NSEED, 6, 'cheap-round ', 5)
        call stats_arm(M, NSEED, 8, 'cheap-round ', 5)
        call stats_arm(M, NSEED, 2, 'mix2 UBfree ', 6)
        call stats_arm(M, NSEED, 3, 'mix2 UBfree ', 6)
        call stats_arm(M, NSEED, 4, 'mix2 UBfree ', 6)
        call stats_arm(M, NSEED, 6, 'mix2 UBfree ', 6)
        write(output_unit, '(a)') ''
        flush(output_unit)
    end subroutine run_statistics

    !> Runs every statistic on one arm. `rounds < 0` selects the Fisher-Yates control.
    subroutine stats_arm(m, nseed, rounds, label, variant)
        integer(int64), intent(in) :: m
        integer, intent(in) :: nseed
        integer, intent(in) :: rounds
        character(len=*), intent(in) :: label
        integer, intent(in), optional :: variant
        integer(int64), allocatable :: perm(:)
        integer :: fix_hist(0:5), pos_hist(0:19), sub_hist(0:19)
        integer(int64) :: cyc_total
        integer :: s, h, trials, k
        integer(int64) :: j, v, key, steps
        real(real64) :: chi_fix, chi_pos, chi_sub, cyc_mean, struct_frac
        integer :: nfix, ncyc, bin, vsel, hl, hr
        integer(int64) :: fa, fb, rkey(8)
        logical, allocatable :: seen(:)

        vsel = 1
        if (present(variant)) vsel = variant
        call mix_round_keys(SEED, max(rounds, 1), rkey)
        hl = bits_for(m) / 2
        hr = bits_for(m) - hl
        fa = ceil_sqrt(m)
        fb = (m + fa - 1_int64) / fa

        allocate(perm(m), seen(0:m - 1))
        fix_hist = 0
        pos_hist = 0
        sub_hist = 0
        cyc_total = 0_int64
        h = width_for(m) / 2

        do s = 1, nseed
            key = SEED + g_shift * 1000003_int64 + int(s, int64) * 7919_int64
            if (vsel == 5) call mix_round_keys(key, max(rounds, 1), rkey)
            if (vsel == 6) call mix2_keys(key, max(rounds, 1), rkey)
            if (rounds < 0) then
                call fy_permutation(key, m, perm)
            else
                do j = 1_int64, m
                    select case (vsel)
                    case (2)
                        call unbal_at(key, m, rounds, hl, hr, j - 1_int64, v, trials)
                    case (3)
                        call mod_at(key, m, rounds, fa, fb, j - 1_int64, v, trials)
                    case (4)
                        call mod_fast_at(key, m, rounds, fa, fb, j - 1_int64, v, trials)
                    case (5)
                        call mix_at(rkey, m, rounds, fa, fb, j - 1_int64, v, trials)
                    case (6)
                        call mix2_at(rkey, m, rounds, fa, fb, j - 1_int64, v, trials)
                    case default
                        call feistel_at(key, m, rounds, h, j - 1_int64, v, trials)
                    end select
                    perm(j) = v
                end do
            end if

            ! Fixed points.
            nfix = 0
            do j = 1_int64, m
                if (perm(j) == j - 1_int64) nfix = nfix + 1
            end do
            fix_hist(min(nfix, 5)) = fix_hist(min(nfix, 5)) + 1

            ! Value at position 0, binned into 20 equal bins.
            bin = int(perm(1) * 20_int64 / m, int32)
            pos_hist(min(bin, 19)) = pos_hist(min(bin, 19)) + 1

            ! Membership of the first four elements, binned the same way.
            do k = 1, 4
                bin = int(perm(k) * 20_int64 / m, int32)
                sub_hist(min(bin, 19)) = sub_hist(min(bin, 19)) + 1
            end do

            ! Cycle count.
            seen = .false.
            ncyc = 0
            do j = 0_int64, m - 1_int64
                if (seen(j)) cycle
                ncyc = ncyc + 1
                v = j
                steps = 0_int64
                do
                    seen(v) = .true.
                    v = perm(v + 1_int64)
                    steps = steps + 1_int64
                    if (v == j) exit
                    if (steps > m) then
                        ! Only reachable if `perm` is not a bijection, in which case this walk never
                        ! closes. Report it: an unbounded walk here is a hang, not a wrong number.
                        write(output_unit, '(a,a,a,i0)') 'FATAL: ', label, &
                            ' produced a non-permutation at seed ', s
                        error stop 1
                    end if
                end do
            end do
            cyc_total = cyc_total + int(ncyc, int64)
        end do

        chi_fix = chi2_poisson1(fix_hist, nseed)
        chi_pos = chi2_uniform(pos_hist, nseed)
        chi_sub = chi2_uniform(sub_hist, 4 * nseed)
        cyc_mean = real(cyc_total, real64) / real(nseed, real64)
        struct_frac = -1.0_real64
        if (vsel == 1) struct_frac = structure_probe(m, rounds, h)

        if (rounds < 0) then
            write(output_unit, '(a,a,3f11.1,f9.3,f11.5)') '   ', label // '     -', &
                chi_fix, chi_pos, chi_sub, cyc_mean, struct_frac
        else
            write(output_unit, '(a,a,i6,3f11.1,f9.3,f11.5)') '   ', label, rounds, &
                chi_fix, chi_pos, chi_sub, cyc_mean, struct_frac
        end if
        flush(output_unit)
        deallocate(perm, seen)
    end subroutine stats_arm

    !> The structural distinguisher: does sharing an input low half make outputs share a high half?
    !!
    !! **Enumerated exhaustively over the raw domain, not sampled.** A first version drew 200000
    !! pairs from `mod(p * odd_constant, 2**h)`, which has period `2**h` in `p` -- so it visited only
    !! **32 distinct pairs**, each 6250 times, and every result it produced was an exact multiple of
    !! 1/32. It reported 0.00000 for three round counts and 0.06250 for another, which reads as a
    !! precise measurement and is a quantisation artefact of a probe with 32 samples. The domain here
    !! is small enough to enumerate, so it now is.
    !!
    !! Two rounds is the case this exists for. The output's high half after two rounds is exactly
    !! `L xor F1(R)`, so two inputs sharing `R` and differing in `L` **cannot** share an output high
    !! half -- probability 0 against 31/1023 for a random permutation, which is as sharp a
    !! distinguisher in the negative direction as the coincidence would have been in the positive.
    !! (An earlier comment here predicted 1.0; that was the identity read backwards.)
    !!
    !! Run on the RAW network, before cycle-walking, because walking mixes the structure and would
    !! hide what is being asked.
    function structure_probe(m, rounds, h) result(frac)
        integer(int64), intent(in) :: m
        integer, intent(in) :: rounds
        integer, intent(in) :: h
        real(real64) :: frac
        integer(int64) :: lo, l1, l2, hi_n, ya, yb, npair, hits
        if (rounds < 0) then
            frac = -1.0_real64                      ! the control has no halves; see the header row
            return
        end if
        hi_n = ishft(1_int64, h)
        npair = 0_int64
        hits = 0_int64
        do lo = 0_int64, hi_n - 1_int64
            do l1 = 0_int64, hi_n - 2_int64
                ya = feistel_raw(SEED, rounds, h, ior(ishft(l1, h), lo))
                do l2 = l1 + 1_int64, hi_n - 1_int64
                    yb = feistel_raw(SEED, rounds, h, ior(ishft(l2, h), lo))
                    npair = npair + 1_int64
                    if (ishft(ya, -h) == ishft(yb, -h)) hits = hits + 1_int64
                end do
            end do
        end do
        frac = real(hits, real64) / real(npair, real64)
    end function structure_probe

    !> Chi-square of a fixed-point histogram against Poisson(1), bins 0..4 and 5+.
    real(real64) function chi2_poisson1(hist, n)
        integer, intent(in) :: hist(0:5)
        integer, intent(in) :: n
        real(real64) :: p(0:5), e, fact
        integer :: k
        fact = 1.0_real64
        p(0) = exp(-1.0_real64)
        do k = 1, 4
            fact = fact * real(k, real64)
            p(k) = exp(-1.0_real64) / fact
        end do
        p(5) = 1.0_real64 - sum(p(0:4))
        chi2_poisson1 = 0.0_real64
        do k = 0, 5
            e = p(k) * real(n, real64)
            if (e > 0.0_real64) chi2_poisson1 = chi2_poisson1 + (real(hist(k), real64) - e)**2 / e
        end do
    end function chi2_poisson1

    !> Chi-square of a histogram against a uniform expectation over its bins.
    real(real64) function chi2_uniform(hist, n)
        integer, intent(in) :: hist(0:19)
        integer, intent(in) :: n
        real(real64) :: e
        integer :: k
        e = real(n, real64) / 20.0_real64
        chi2_uniform = 0.0_real64
        do k = 0, 19
            chi2_uniform = chi2_uniform + (real(hist(k), real64) - e)**2 / e
        end do
    end function chi2_uniform

    ! ================================================================================
    ! Parts 1 and 2 -- throughput and thread scaling
    ! ================================================================================

    subroutine run_speed()
        integer(int64) :: sizes(3) = [1000000_int64, 10000000_int64, 100000000_int64]
        integer :: si

        write(output_unit, '(a)') '=========================================================='
        write(output_unit, '(a)') 'PARTS 1 and 2 -- throughput and thread scaling'
        write(output_unit, '(a,i0,a)') 'Feistel at ', g_rounds_default, ' rounds; times in ms'
        write(output_unit, '(a)') ''
        flush(output_unit)
        do si = 1, size(sizes)
            call speed_one(sizes(si))
        end do
    end subroutine run_speed

    !> Measures the three width rules side by side at one size: walk cost, one core, and 64 cores.
    subroutine width_one(m)
        integer(int64), intent(in) :: m
        integer(int64), allocatable :: perm(:)
        integer :: hb, hl, hr, h2, trials, ti, nt
        integer(int64) :: a, b, j, pv
        real(real64) :: t0, t1, w(3), t1c(3), t64(3)
        integer :: vi

        allocate(perm(m))
        h2 = width_for(m) / 2                       ! balanced: b even
        hb = bits_for(m)                            ! unbalanced: b exact
        hl = hb / 2
        hr = hb - hl
        a = ceil_sqrt(m)
        b = (m + a - 1_int64) / a                   ! modular: a*b just covers m

        write(output_unit, '(a,i0)') '---- m = ', m
        write(output_unit, '(a,i0,a,f7.4)') '   balanced   domain 2**', 2 * h2, &
            '   ratio ', real(ishft(1_int64, 2 * h2), real64) / real(m, real64)
        write(output_unit, '(a,i0,a,f7.4)') '   unbalanced domain 2**', hb, &
            '   ratio ', real(ishft(1_int64, hb), real64) / real(m, real64)
        write(output_unit, '(a,i0,a,i0,a,f7.4)') '   modular    domain ', a, ' x ', b, &
            '   ratio ', real(a * b, real64) / real(m, real64)
        flush(output_unit)

        do vi = 1, 3
            do ti = 1, 2
                nt = merge(1, 64, ti == 1)
                if (nt > max_threads) cycle
                t0 = wall()
#ifdef _OPENMP
                !$omp parallel do num_threads(nt) default(shared) private(j, trials, pv) schedule(static)
#endif
                do j = 1_int64, m
                    select case (vi)
                    case (1)
                        call feistel_at(SEED, m, g_rounds_default, h2, j - 1_int64, pv, trials)
                    case (2)
                        call unbal_at(SEED, m, g_rounds_default, hl, hr, j - 1_int64, pv, trials)
                    case default
                        call mod_at(SEED, m, g_rounds_default, a, b, j - 1_int64, pv, trials)
                    end select
                    perm(j) = pv
                end do
#ifdef _OPENMP
                !$omp end parallel do
#endif
                t1 = wall()
                if (ti == 1) then
                    t1c(vi) = (t1 - t0) * 1000.0_real64
                    call assert_permutation(perm, m, 'width variant')
                else
                    t64(vi) = (t1 - t0) * 1000.0_real64
                end if
            end do
            w(vi) = 0.0_real64
            do j = 1_int64, min(m, 200000_int64)
                select case (vi)
                case (1)
                    call feistel_at(SEED, m, g_rounds_default, h2, j - 1_int64, pv, trials)
                case (2)
                    call unbal_at(SEED, m, g_rounds_default, hl, hr, j - 1_int64, pv, trials)
                case default
                    call mod_at(SEED, m, g_rounds_default, a, b, j - 1_int64, pv, trials)
                end select
                w(vi) = w(vi) + real(trials, real64)
            end do
            w(vi) = w(vi) / real(min(m, 200000_int64), real64)
        end do

        write(output_unit, '(a)') '   variant        applications   1 core (ms)   64 cores (ms)   vs balanced'
        write(output_unit, '(a,f12.4,f14.1,f16.1,f14.2)') '   balanced   ', w(1), t1c(1), t64(1), 1.0_real64
        write(output_unit, '(a,f12.4,f14.1,f16.1,f14.2)') '   unbalanced ', w(2), t1c(2), t64(2), t1c(1) / t1c(2)
        write(output_unit, '(a,f12.4,f14.1,f16.1,f14.2)') '   modular    ', w(3), t1c(3), t64(3), t1c(1) / t1c(3)
        write(output_unit, '(a)') ''
        flush(output_unit)
        deallocate(perm)
    end subroutine width_one

    subroutine run_width()
        integer(int64) :: sizes(3) = [1000000_int64, 10000000_int64, 100000000_int64]
        integer :: si
        write(output_unit, '(a)') '=========================================================='
        write(output_unit, '(a)') 'WIDTH RULE -- balanced vs unbalanced vs modular, 4 rounds'
        write(output_unit, '(a)') ''
        flush(output_unit)
        do si = 1, size(sizes)
            call width_one(sizes(si))
        end do
    end subroutine run_width

    !> Single-core scaling sweep, plus the thread-independence check.
    subroutine run_scale()
        integer(int64) :: sizes(8) = [10_int64, 100_int64, 1000_int64, 10000_int64, &
                                      100000_int64, 1000000_int64, 10000000_int64, 100000000_int64]
        integer(int64), allocatable :: perm(:), other(:)
        integer(int64) :: m, a, b, j, pv, reps, r
        integer :: si, trials, rd
        real(real64) :: t0, t1, best_fy, best_bj, e

        write(output_unit, '(a)') '=========================================================='
        write(output_unit, '(a)') 'SINGLE-CORE SCALING -- Fisher-Yates vs bijection (modular, 4 rounds)'
        write(output_unit, '(a)') ''
        write(output_unit, '(a)') '           m        FY total     bij total      FY/elem    bij/elem   FY/bij'
        write(output_unit, '(a)') '                        (ms)          (ms)         (ns)        (ns)'
        flush(output_unit)

        do si = 1, size(sizes)
            m = sizes(si)
            a = ceil_sqrt(m)
            b = (m + a - 1_int64) / a
            allocate(perm(m))
            reps = max(1_int64, 20000000_int64 / m)     ! ~2e7 element-operations per timed region

            best_fy = huge(1.0_real64)
            best_bj = huge(1.0_real64)
            do rd = 1, 3
                t0 = wall()
                do r = 1_int64, reps
                    call fy_permutation(SEED + r, m, perm)
                end do
                t1 = wall()
                best_fy = min(best_fy, (t1 - t0) / real(reps, real64) * 1000.0_real64)

                t0 = wall()
                do r = 1_int64, reps
                    do j = 1_int64, m
                        call mod_at(SEED + r, m, 4, a, b, j - 1_int64, pv, trials)
                        perm(j) = pv
                    end do
                end do
                t1 = wall()
                best_bj = min(best_bj, (t1 - t0) / real(reps, real64) * 1000.0_real64)
                if (m > 1000000_int64) exit             ! one round is enough at the big sizes
            end do

            write(output_unit, '(i12,2f14.4,2f13.3,f9.2)') m, best_fy, best_bj, &
                best_fy * 1.0e6_real64 / real(m, real64), best_bj * 1.0e6_real64 / real(m, real64), &
                best_fy / best_bj
            flush(output_unit)
            deallocate(perm)
        end do

        write(output_unit, '(a)') ''
        write(output_unit, '(a)') '---- thread independence: is the bijection bit-identical at 1 and 64 threads?'
        flush(output_unit)
        do si = 6, 8
            m = sizes(si)
            a = ceil_sqrt(m)
            b = (m + a - 1_int64) / a
            allocate(perm(m), other(m))
            do j = 1_int64, m
                call mod_at(SEED, m, 4, a, b, j - 1_int64, pv, trials)
                perm(j) = pv
            end do
#ifdef _OPENMP
            !$omp parallel do num_threads(64) default(shared) private(j, trials, pv) schedule(dynamic, 997)
#endif
            do j = 1_int64, m
                call mod_at(SEED, m, 4, a, b, j - 1_int64, pv, trials)
                other(j) = pv
            end do
#ifdef _OPENMP
            !$omp end parallel do
#endif
            e = 0.0_real64
            do j = 1_int64, m
                if (perm(j) /= other(j)) e = e + 1.0_real64
            end do
            write(output_unit, '(a,i12,a,i0,a)') '   m = ', m, ':  differing elements = ', &
                int(e, int64), merge('   IDENTICAL', '   MISMATCH!', e == 0.0_real64)
            flush(output_unit)
            deallocate(perm, other)
        end do

        ! Fisher-Yates is deterministic too, but only because it is serial: the same call twice
        ! gives the same answer, and there is no parallel form of it to disagree with.
        m = 1000000_int64
        allocate(perm(m), other(m))
        call fy_permutation(SEED, m, perm)
        call fy_permutation(SEED, m, other)
        e = 0.0_real64
        do j = 1_int64, m
            if (perm(j) /= other(j)) e = e + 1.0_real64
        end do
        write(output_unit, '(a,i0)') '   Fisher-Yates, same seed twice, differing elements = ', int(e, int64)
        write(output_unit, '(a)') ''
        deallocate(perm, other)
        flush(output_unit)
    end subroutine run_scale

    !> The scaling sweep extended across thread counts, to locate the crossover per size.
    !!
    !! The parallel region is opened INSIDE the repetition loop, once per permutation, because that
    !! is what a caller experiences: `pf_random_perm_at` is elemental and the caller writes the loop,
    !! so the team is created and joined per call. Hoisting it outside would measure a configuration
    !! nobody runs and would hide the small-`m` result entirely, which is the interesting one.
    subroutine run_cores()
        integer(int64) :: sizes(8) = [10_int64, 100_int64, 1000_int64, 10000_int64, &
                                      100000_int64, 1000000_int64, 10000000_int64, 100000000_int64]
        integer :: tlist(5) = [1, 2, 4, 8, 16]
        integer(int64), allocatable :: perm(:)
        integer(int64) :: m, a, b, j, pv, reps, r
        integer :: si, ti, nt, trials, rd, cross
        real(real64) :: t0, t1, t_fy, t_bj(5), best

        write(output_unit, '(a)') '=========================================================='
        write(output_unit, '(a)') 'CROSSOVER -- Fisher-Yates (serial) against the bijection at 1..16 cores'
        write(output_unit, '(a)') 'ms per permutation; best of 3 where repetition is affordable'
        write(output_unit, '(a)') ''
        write(output_unit, '(a)') '           m       FY      bij@1      bij@2      bij@4      bij@8     bij@16   cross'
        flush(output_unit)

        do si = 1, size(sizes)
            m = sizes(si)
            a = ceil_sqrt(m)
            b = (m + a - 1_int64) / a
            allocate(perm(m))

            reps = max(1_int64, min(20000000_int64 / m, 2000000_int64))
            best = huge(1.0_real64)
            do rd = 1, 3
                t0 = wall()
                do r = 1_int64, reps
                    call fy_permutation(SEED + r, m, perm)
                end do
                t1 = wall()
                best = min(best, (t1 - t0) / real(reps, real64) * 1000.0_real64)
                if (m > 1000000_int64) exit
            end do
            t_fy = best

            do ti = 1, size(tlist)
                nt = tlist(ti)
                if (nt > max_threads) then
                    t_bj(ti) = -1.0_real64
                    cycle
                end if
                reps = max(1_int64, min(20000000_int64 / m, 20000_int64))
                best = huge(1.0_real64)
                do rd = 1, 3
                    t0 = wall()
                    do r = 1_int64, reps
#ifdef _OPENMP
                        !$omp parallel do num_threads(nt) default(shared) private(j, trials, pv) &
                        !$omp     schedule(static)
#endif
                        do j = 1_int64, m
                            call mod_at(SEED + r, m, 4, a, b, j - 1_int64, pv, trials)
                            perm(j) = pv
                        end do
#ifdef _OPENMP
                        !$omp end parallel do
#endif
                    end do
                    t1 = wall()
                    best = min(best, (t1 - t0) / real(reps, real64) * 1000.0_real64)
                    if (m > 1000000_int64) exit
                end do
                t_bj(ti) = best
            end do

            cross = 0
            do ti = 1, size(tlist)
                if (t_bj(ti) > 0.0_real64 .and. t_bj(ti) < t_fy) then
                    cross = tlist(ti)
                    exit
                end if
            end do

            if (cross > 0) then
                write(output_unit, '(i12,6f11.4,i8)') m, t_fy, t_bj(1), t_bj(2), t_bj(3), &
                    t_bj(4), t_bj(5), cross
            else
                write(output_unit, '(i12,6f11.4,a)') m, t_fy, t_bj(1), t_bj(2), t_bj(3), &
                    t_bj(4), t_bj(5), '     >16'
            end if
            flush(output_unit)
            deallocate(perm)
        end do
        write(output_unit, '(a)') ''
        flush(output_unit)
    end subroutine run_cores

    !> Single-core optimisation sweep: baseline modular, division-free, and cheap round function.
    subroutine run_opt()
        integer(int64) :: sizes(5) = [1000_int64, 100000_int64, 1000000_int64, &
                                      10000000_int64, 100000000_int64]
        integer(int64), allocatable :: perm(:), ref(:)
        integer(int64) :: m, a, b, j, pv, reps, r, rk(8)
        integer :: si, trials, rd, vi
        real(real64) :: t0, t1, best(3)

        write(output_unit, '(a)') '=========================================================='
        write(output_unit, '(a)') 'SERIAL OPTIMISATION -- ns per element, single core, 4 rounds'
        write(output_unit, '(a)') ''
        write(output_unit, '(a)') '           m    baseline   no-division   cheap-round   nodiv/base  cheap/base'
        flush(output_unit)

        do si = 1, size(sizes)
            m = sizes(si)
            a = ceil_sqrt(m)
            b = (m + a - 1_int64) / a
            call mix_round_keys(SEED, 4, rk)
            allocate(perm(m), ref(m))
            reps = max(1_int64, min(20000000_int64 / m, 20000_int64))

            do vi = 1, 3
                best(vi) = huge(1.0_real64)
                do rd = 1, 3
                    t0 = wall()
                    do r = 1_int64, reps
                        do j = 1_int64, m
                            select case (vi)
                            case (1)
                                call mod_at(SEED, m, 4, a, b, j - 1_int64, pv, trials)
                            case (2)
                                call mod_fast_at(SEED, m, 4, a, b, j - 1_int64, pv, trials)
                            case default
                                call mix_at(rk, m, 4, a, b, j - 1_int64, pv, trials)
                            end select
                            perm(j) = pv
                        end do
                    end do
                    t1 = wall()
                    best(vi) = min(best(vi), (t1 - t0) / real(reps, real64) / real(m, real64) * 1.0e9_real64)
                    if (m > 1000000_int64) exit
                end do
                call assert_permutation(perm, m, 'optimised variant')
                if (vi == 1) ref = perm
            end do

            write(output_unit, '(i12,3f14.3,2f13.2)') m, best(1), best(2), best(3), &
                best(1) / best(2), best(1) / best(3)
            flush(output_unit)
            deallocate(perm, ref)
        end do
        write(output_unit, '(a)') ''
        write(output_unit, '(a)') 'Note: no-division returns the SAME permutation as the baseline only if the'
        write(output_unit, '(a)') 'round function is unchanged -- it is not (multiply-shift replaces modulo), so'
        write(output_unit, '(a)') 'both optimised arms are different permutations and need their own statistics.'
        write(output_unit, '(a)') ''
        flush(output_unit)
    end subroutine run_opt

    !> The optimisation programme: every lever measured against the same baseline, single core.
    subroutine run_prog()
        integer(int64) :: sizes(4) = [10000_int64, 1000000_int64, 10000000_int64, 100000000_int64]
        integer(int64), allocatable :: perm(:), ref(:)
        integer(int64) :: m, a, b, j, pv, reps, r, rk(8), blk(512)
        integer :: si, trials, rd, vi, nb, jb
        real(real64) :: t0, t1, best(6)
        character(len=14) :: nm(6)

        nm(1) = 'baseline      '
        nm(2) = 'mix2 scalar   '
        nm(3) = 'mix2 blocked  '
        nm(4) = 'pmix64 (unsafe'
        nm(5) = 'nodiv philox  '
        nm(6) = 'mix2 lr-fill  '

        write(output_unit, '(a)') '=========================================================='
        write(output_unit, '(a)') 'OPTIMISATION PROGRAMME -- ns per element, single core, 4 rounds'
        write(output_unit, '(a)') 'modular width rule throughout; mix2 is the UB-free shippable mixer'
        write(output_unit, '(a)') ''
        write(output_unit, '(a)') &
            '           m    baseline  mix2 scalar mix2 blocked  pmix64(uns)  nodiv-phlx  mix2 lr-fill'
        flush(output_unit)

        do si = 1, size(sizes)
            m = sizes(si)
            a = ceil_sqrt(m)
            b = (m + a - 1_int64) / a
            call mix2_keys(SEED, 4, rk)
            allocate(perm(m), ref(m))
            reps = max(1_int64, min(20000000_int64 / m, 2000_int64))

            do vi = 1, 6
                best(vi) = huge(1.0_real64)
                do rd = 1, 3
                    t0 = wall()
                    do r = 1_int64, reps
                        if (vi == 6) then
                            call perm_fill_lr(rk, m, 4, a, b, perm)
                        else if (vi == 3) then
                            do jb = 1, int((m + 511_int64) / 512_int64, int32)
                                nb = int(min(512_int64, m - int(jb - 1, int64) * 512_int64), int32)
                                call perm_fill_block(rk, m, 4, a, b, int(jb - 1, int64) * 512_int64, &
                                                     blk(1:nb))
                                perm(int(jb - 1, int64) * 512_int64 + 1_int64: &
                                     int(jb - 1, int64) * 512_int64 + int(nb, int64)) = blk(1:nb)
                            end do
                        else
                            do j = 1_int64, m
                                select case (vi)
                                case (1)
                                    call mod_at(SEED, m, 4, a, b, j - 1_int64, pv, trials)
                                case (2)
                                    call mix2_at(rk, m, 4, a, b, j - 1_int64, pv, trials)
                                case (4)
                                    call mix_at(rk, m, 4, a, b, j - 1_int64, pv, trials)
                                case default
                                    call mod_fast_at(SEED, m, 4, a, b, j - 1_int64, pv, trials)
                                end select
                                perm(j) = pv
                            end do
                        end if
                    end do
                    t1 = wall()
                    best(vi) = min(best(vi), (t1 - t0) / real(reps, real64) / real(m, real64) * 1.0e9_real64)
                    if (m > 1000000_int64) exit
                end do
                call assert_permutation(perm, m, nm(vi))
                if (vi == 2) ref = perm
                ! The blocked form must reproduce the scalar mix2 EXACTLY -- it is the same kernel
                ! rearranged, so any difference is a defect in the rearrangement, not a variant.
                if (vi == 3 .or. vi == 6) then
                    do j = 1_int64, m
                        if (perm(j) /= ref(j)) then
                            write(output_unit, '(a,a,a,i0)') 'FATAL: ', nm(vi), ' differs from scalar at ', j
                            error stop 1
                        end if
                    end do
                end if
            end do

            write(output_unit, '(i12,6f13.3)') m, best(1), best(2), best(3), best(4), best(5), best(6)
            flush(output_unit)
            deallocate(perm, ref)
        end do
        write(output_unit, '(a)') ''
        flush(output_unit)
    end subroutine run_prog

    !> STAGE 0: the modular-domain structural distinguisher, over many keys.
    !!
    !! `structure_probe` is a power-of-two-HALVES test and cannot see the modular family at all, so
    !! every clearance the modular kernel has comes from marginal statistics -- the same half of the
    !! battery that passed 3-round balanced, which the structural test then caught at +2.9 sigma.
    !! This closes that gap, and the single-key limitation of §9 at the same time.
    !!
    !! **What it asks.** An input is a pair `(l, r)` in `Z_a x Z_b`; so is an output. If sharing one
    !! input component makes the outputs share a component more (or less) often than chance, the
    !! network has left structure behind. Four relations are enumerated **exhaustively** over the raw
    !! domain -- no sampling, so no power is lost to a quantisation artefact:
    !!
    !!   * same input `r`, differing `l` -> same output `l`?   (the two-round signature: exactly 0)
    !!   * same input `r`, differing `l` -> same output `r`?
    !!   * same input `l`, differing `r` -> same output `l`?
    !!   * same input `l`, differing `r` -> same output `r`?
    !!
    !! **Fisher-Yates supplies the null.** Rather than comparing against a formula, the control
    !! builds a genuinely uniform permutation of the same raw domain and runs the identical four
    !! counts on it. That is what the maintainer's decision to keep Fisher-Yates in `test/` buys:
    !! the calibration is measured, not assumed, so "3.4 % of pairs match" is read against what a
    !! correct construction actually does rather than against `(b-1)/(ab-1)`.
    !!
    !! Run on the RAW network, before cycle-walking, because walking mixes the structure and would
    !! hide what is being asked.
    subroutine run_struct()
        integer(int64), parameter :: MM = 1000_int64
        integer, parameter :: NKEYS = 64
        integer :: rlist(4) = [2, 3, 4, 6]
        integer(int64) :: a, b, nd, x, y1, y2, l1, l2, r1, r2, key, rk(8)
        integer(int64), allocatable :: y(:), fyperm(:)
        real(real64) :: f(4, NKEYS), mu(4), sd(4), ctl_mu(4), ctl_sd(4), z
        integer(int64) :: hit(4), npair(4)
        integer :: ri, ka, arm
        character(len=14) :: nm

        a = ceil_sqrt(MM)
        b = (MM + a - 1_int64) / a
        nd = a * b
        allocate(y(0:nd - 1), fyperm(nd))

        write(output_unit, '(a)') '=========================================================='
        write(output_unit, '(a)') 'STAGE 0 -- modular-domain structural distinguisher'
        write(output_unit, '(a,i0,a,i0,a,i0,a,i0,a)') 'raw domain Z_', a, ' x Z_', b, ' = ', nd, &
            ' elements, ', NKEYS, ' independent keys, all pairs enumerated'
        write(output_unit, '(a)') 'columns: r->l  r->r  l->l  l->r   (fraction of pairs sharing an output component)'
        write(output_unit, '(a)') 'z is against the Fisher-Yates control, in units of the control''s own key-to-key sd'
        write(output_unit, '(a)') ''
        write(output_unit, '(a)') '   arm            rounds       r->l       r->r       l->l       l->r        max|z|'
        flush(output_unit)

        do arm = 0, size(rlist)
            do ka = 1, NKEYS
                key = SEED + g_shift * 7919393_int64 + int(ka, int64) * 104729_int64
                if (arm == 0) then
                    call fy_permutation(key, nd, fyperm)
                    do x = 0_int64, nd - 1_int64
                        y(x) = fyperm(x + 1_int64)
                    end do
                else
                    call mix2_keys(key, rlist(arm), rk)
                    do x = 0_int64, nd - 1_int64
                        y(x) = feistel_mix2(rk, rlist(arm), a, b, x)
                    end do
                end if

                hit = 0_int64
                npair = 0_int64
                ! Pairs sharing the input right component.
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
                ! Pairs sharing the input left component.
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
                do ri = 1, 4
                    f(ri, ka) = real(hit(ri), real64) / real(npair(ri), real64)
                end do
            end do

            do ri = 1, 4
                mu(ri) = sum(f(ri, :)) / real(NKEYS, real64)
                sd(ri) = sqrt(sum((f(ri, :) - mu(ri))**2) / real(NKEYS - 1, real64))
            end do

            if (arm == 0) then
                ctl_mu = mu
                ctl_sd = sd
                nm = 'Fisher-Yates  '
                write(output_unit, '(a,a,a,4f11.5,a)') '   ', nm, '     -', mu(1), mu(2), mu(3), mu(4), &
                    '        (null)'
            else
                z = 0.0_real64
                do ri = 1, 4
                    if (ctl_sd(ri) > 0.0_real64) &
                        z = max(z, abs(mu(ri) - ctl_mu(ri)) / (ctl_sd(ri) / sqrt(real(NKEYS, real64))))
                end do
                nm = 'mix2 modular  '
                write(output_unit, '(a,a,i6,4f11.5,f14.2)') '   ', nm, rlist(arm), &
                    mu(1), mu(2), mu(3), mu(4), z
            end if
            flush(output_unit)
        end do
        write(output_unit, '(a)') ''
        write(output_unit, '(a)') 'A |z| of a few is ordinary across 4 relations x 5 arms; two rounds should be'
        write(output_unit, '(a)') 'unmistakable (r->l exactly 0), and 4 rounds should sit in the control''s range.'
        write(output_unit, '(a)') ''
        flush(output_unit)
        deallocate(y, fyperm)
    end subroutine run_struct

    subroutine speed_one(m)
        integer(int64), intent(in) :: m
        integer(int64), allocatable :: perm(:)
        real(real64) :: t0, t1, t_fy, t_f, t_one
        integer :: h, ti, nt, trials
        integer(int64) :: j, pv
        real(real64) :: walk_mean

        allocate(perm(m))
        h = width_for(m) / 2
        t_one = 0.0_real64

        write(output_unit, '(a,i0,a,i0,a,i0)') '---- m = ', m, '   domain = 2**', 2 * h, &
            ', walk ratio x1000 = ', ishft(1_int64, 2 * h) * 1000_int64 / m
        flush(output_unit)

        ! Fisher-Yates: serial by construction, so measured once.
        t0 = wall()
        call fy_permutation(SEED, m, perm)
        t1 = wall()
        t_fy = (t1 - t0) * 1000.0_real64
        call assert_permutation(perm, m, 'Fisher-Yates')
        write(output_unit, '(a,f12.1,a)') '   Fisher-Yates (serial only)      ', t_fy, ' ms'
        flush(output_unit)

        ! The bijection, at each thread count.
        do ti = 1, size(thread_list)
            nt = thread_list(ti)
            if (nt > max_threads) cycle
            t0 = wall()
#ifdef _OPENMP
            !$omp parallel do num_threads(nt) default(shared) private(j, trials, pv) schedule(static)
#endif
            do j = 1_int64, m
                call feistel_at(SEED, m, g_rounds_default, h, j - 1_int64, pv, trials)
                perm(j) = pv
            end do
#ifdef _OPENMP
            !$omp end parallel do
#endif
            t1 = wall()
            t_f = (t1 - t0) * 1000.0_real64
            if (ti == 1) then
                call assert_permutation(perm, m, 'Feistel')
                t_one = t_f
            end if
            write(output_unit, '(a,i4,a,f12.1,a,f8.2,a,f8.2)') '   Feistel, threads =', nt, '   ', &
                t_f, ' ms   vs FY x', t_fy / t_f, '   scaling x', t_one / t_f
            flush(output_unit)
        end do

        ! What the cycle-walk actually costs at this size.
        walk_mean = 0.0_real64
        do j = 1_int64, min(m, 200000_int64)
            call feistel_at(SEED, m, g_rounds_default, h, j - 1_int64, pv, trials)
            walk_mean = walk_mean + real(trials, real64)
        end do
        walk_mean = walk_mean / real(min(m, 200000_int64), real64)
        write(output_unit, '(a,f8.4)') '   mean network applications per element: ', walk_mean
        write(output_unit, '(a)') ''
        flush(output_unit)
        deallocate(perm)
    end subroutine speed_one

    !> Verifies the result really is a permutation of `0 .. m-1`; a bijection that is not one is
    !! the single failure this whole construction has to be held to.
    subroutine assert_permutation(perm, m, what)
        integer(int64), intent(in) :: perm(:)
        integer(int64), intent(in) :: m
        character(len=*), intent(in) :: what
        integer(int8), allocatable :: hit(:)
        integer(int64) :: j
        allocate(hit(0:m - 1))
        hit = 0_int8
        do j = 1_int64, m
            if (perm(j) < 0_int64 .or. perm(j) >= m) then
                write(output_unit, '(a,a,a,i0)') 'FATAL: ', what, ' produced out-of-range ', perm(j)
                error stop 1
            end if
            if (hit(perm(j)) /= 0_int8) then
                write(output_unit, '(a,a,a,i0)') 'FATAL: ', what, ' is not a bijection, repeat at ', perm(j)
                error stop 1
            end if
            hit(perm(j)) = 1_int8
        end do
        deallocate(hit)
    end subroutine assert_permutation

end program probe_random_feistel
