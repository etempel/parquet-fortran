!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!> The frozen `-log(u)` transform behind `pf_weighted_permutation`'s exponential race.
!>
!> **A leaf module on purpose, and the reason is a tool rather than taste.**
!> `tools/check_exp_key.sh` proves the transform gives the same bits under every IEEE-conforming
!> build, and it can only do that by compiling it standalone under several compilers and flag
!> sets. `parquet_random` gained a dependency on `parquet_sorting` -- and so, transitively, on
!> Arrow -- the moment the weighted permutation needed a sort, which put the transform out of that
!> check's reach. Splitting it out costs one file and keeps the check dependency-free, which
!> matters because that check is what found the FMA exposure to begin with.
!>
!> **Why this is not `log()` from the runtime.** Everything `parquet_random` promises is a pure
!> function of its coordinates, frozen by `pf_random_algorithm`, and every other generic delivers
!> that with integer arithmetic alone -- Philox, the Lemire reduction and the Feistel permutation
!> are all integer, so two machines agree bit for bit and the golden vectors say so. `log` breaks
!> that property class, because libm is not part of any contract this project controls. Measured on
!> machine B over two million deterministic inputs: **1.33%** of `-log(u)` values differ between
!> gfortran and ifx at their default flags, worst gap 126775 ulp; **0.083%** still differ under
!> `-fp-model=precise`; and ifx disagrees with ITSELF across flag settings, so this is not only a
!> cross-vendor problem.
!>
!> A weighted permutation is decided by the ORDER of `e/w`, so one differing key changes the answer
!> only when it crosses its neighbour -- rare at `n = 10**6`, approaching certain by `n = 10**9`.
!> That is a latent wrong answer with no symptom, which is the one failure mode this tier exists to
!> rule out.
!>
!> **The approximation costs nothing distributionally.** The race needs `E ~ Exp(1)` iid; a
!> transform accurate to about two ulp perturbs each key by ~2e-16 relative, eleven orders of
!> magnitude below the Monte Carlo standard error of any test that could detect it. Approximation
!> error buys exact reproducibility, which is the trade this tier exists to make.
!>
!> Depends on `iso_fortran_env` and nothing else. **Keep it that way** -- a dependency here is a
!> dependency the cross-build check cannot carry.
module parquet_expkey

    use iso_fortran_env, only: int64, real64

    implicit none
    private

    public :: exp_key
    public :: exp_key_contract_ok
    public :: parquet_debug_exp_key

    !> `1/sqrt(2)`, the point the mantissa is folded about so that `|f|` stays under 0.1716.
    real(real64), parameter :: ek_sqrt_half = 0.70710678118654752440_real64
    !> `log(2)`, upper half: its low mantissa bits are zero, so `n * ek_log2_hi` is EXACT for every
    !! exponent a `real64` can carry. That exactness is what keeps the large-`n` sum accurate.
    real(real64), parameter :: ek_log2_hi = 6.93147180369123816490e-01_real64
    !> `log(2)`, the remainder. `ek_log2_hi + ek_log2_lo` is `log(2)` to about 85 bits.
    real(real64), parameter :: ek_log2_lo = 1.90821492927058770002e-10_real64

    !> Frozen fingerprint of `exp_key` over 32 fixed inputs; see `exp_key_contract_ok`.
    !!
    !! Changing this value changes nothing about the library and everything about what it will
    !! ACCEPT: it is the constant that decides whether a build is allowed to produce weighted
    !! permutations at all. Re-derive it with `tools/check_exp_key.f90 --contract`, and only ever
    !! because the transform itself was deliberately changed.
    integer(int64), parameter :: ek_contract_fp = 6162015111561117238_int64

    !> The atanh series' coefficients, `ek_c(j) = 1/(2j+1)`. Twelve of them; see `exp_key`.
    real(real64), parameter :: ek_c(0:11) = [ &
        1.00000000000000000000_real64, 0.33333333333333333333_real64, 0.20000000000000000000_real64, &
        0.14285714285714285714_real64, 0.11111111111111111111_real64, 0.09090909090909090909_real64, &
        0.07692307692307692308_real64, 0.06666666666666666667_real64, 0.05882352941176470588_real64, &
        0.05263157894736842105_real64, 0.04761904761904761905_real64, 0.04347826086956521739_real64]

contains

    !> `-log(u)` for `u` in `[2**-53, 1]`, using only IEEE `+ - * /`. **Not a general-purpose log.**
    !!
    !! **The construction.** Split `u = m * 2**k` with `m` in `[1/sqrt(2), sqrt(2))`, so
    !! `log(u) = log(m) + k*log(2)`. Then with `f = (m-1)/(m+1)`, at most 0.1716 in magnitude,
    !! `log(m) = 2*atanh(f) = 2f*(1 + f**2/3 + f**4/5 + ...)`. Twelve terms leave a truncation error
    !! near `6e-21`, four orders below one ulp of the result -- and the series stays *relatively*
    !! accurate as `m` approaches 1, where `f -> 0` and the answer is essentially `2f`, which a
    !! naive `log(1+x)` would reach only through cancellation. **Measured worst error against a
    !! `real128` reference over every exponent a key can carry: 2.0 ulp** (`tools/check_exp_key.f90
    !! --accuracy`), not the 1 ulp originally aimed at; the floor is `f` itself, since `m+1` rounds
    !! and the division rounds again. Two ulp is `2e-16` relative, twelve orders below anything a
    !! distributional test could see, so closing it would buy nothing and would cost the two-part
    !! `f` that fdlibm needs for it.
    !!
    !! **`volatile` is what makes this reproducible, and it is the whole point of the procedure.**
    !! A Horner step is `a*b + c`, the exact shape a compiler fuses into an FMA -- which rounds
    !! once where IEEE rounds twice, and so answers differently. Storing each product through a
    !! `volatile` forces it to be rounded to `real64` before the add, which no compiler may skip.
    !! **This is not hypothetical and it is not exotic**: `gfortran -O3 -march=native` changes the
    !! fingerprint of the unbarriered form, and `-march=native` is an ordinary thing to build with.
    !! An earlier design note concluded FMA was harmless on the strength of four builds that
    !! happened not to enable it on gfortran; `tools/check_exp_key.sh` now sweeps for it.
    !!
    !! **The cost is 2.53x on the polynomial** (6.58 -> 16.68 ns per call, gfortran 15.2.1 `-O3
    !! -funroll-loops`, machine B) and it buys a contract that holds under every IEEE-conforming
    !! flag set. If the key loop ever dominates a real workload, the way to recover it is a
    !! table-driven reduction -- a 16-entry table of `log(m_i)` brings `|f|` under `2**-5` and the
    !! series down to six terms, halving the barriers -- not deleting the barriers.
    !!
    !! **`-ffast-math` / `-Ofast` are out of scope, and no library can bring them in.** They
    !! license the compiler to violate IEEE semantics outright, and the fingerprint moves under
    !! them even with the barriers (it is at least stable *within* fast-math, with and without
    !! FMA). `fpm --profile release` passes `-O3 -funroll-loops`, so nothing shipped is affected.
    !!
    !! **Not `pure`, and it cannot be**: both gfortran and ifx reject a `volatile` local in a pure
    !! procedure. That is a deliberate trade of `pure elemental` for a contract that actually
    !! holds; nothing in the library needs it in a pure context.
    !!
    !! **The domain is not general and the guard is the caller's.** The race feeds this `1 - u` for
    !! a uniform `u` in `[0, 1)`, so the argument lies in `[2**-53, 1]` and is never zero, never
    !! denormal and never above 1. `exponent`/`fraction` are exact bit operations on a normal
    !! number, so nothing rounds before the polynomial does. `u = 1` gives exactly `0`.
    function exp_key(u) result(e)
        real(real64), intent(in) :: u   !! a uniform in `[2**-53, 1]`; nothing validates this
        real(real64) :: e               !! `-log(u)`, in `[0, 36.74]`
        real(real64) :: m, f, s, poly, en, big, small, logm
        real(real64), volatile :: vt    !! the rounding barrier; see the note above
        integer :: k, i

        k = exponent(u)
        m = fraction(u)                 ! in [0.5, 1)
        if (m < ek_sqrt_half) then
            m = m + m
            k = k - 1
        end if
        f = (m - 1.0_real64) / (m + 1.0_real64)
        s = f * f
        poly = ek_c(11)
        do i = 10, 0, -1
            vt = poly * s               ! rounded to real64 here, so no FMA can span the add
            poly = vt + ek_c(i)
        end do
        ! `f + f` is exact and feeds a multiply rather than an add, so it needs no barrier; the
        ! three products that DO feed an add each get one.
        vt = (f + f) * poly
        logm = vt
        en = real(-k, real64)
        vt = en * ek_log2_hi
        big = vt
        vt = en * ek_log2_lo
        small = vt
        e = big + (small - logm)
    end function exp_key

    !> `exp_key` under a public name. **Test-only.**
    !!
    !! Public only because it has to be: this module reaches no `bind(C)` surface, so the C++-side
    !! debug-hook convention the rest of the library uses is unavailable to it, and a frozen
    !! transform that no test can call is a frozen transform nobody is checking. It is excluded from
    !! README.md's API overview, no library code calls it, and it is not a general-purpose
    !! logarithm -- see `exp_key` for the domain it is valid on.
    function parquet_debug_exp_key(u) result(e)
        real(real64), intent(in) :: u   !! a uniform in `[2**-53, 1]`
        real(real64) :: e               !! `-log(u)`

        e = exp_key(u)
    end function parquet_debug_exp_key

    !> Re-derives the frozen transform over 32 fixed inputs and compares against `ek_contract_fp`.
    !!
    !! **Why a run-time check for a compile-time property.** `exp_key` is reproducible under every
    !! IEEE-conforming build (`tools/check_exp_key.sh` sweeps eleven of them), but `-ffast-math` and
    !! ifx's `-fp-model=fast` are not IEEE: they may compute `(m-1)/(m+1)` by reciprocal
    !! approximation, which no rounding barrier can undo. A build like that produces a DIFFERENT
    !! permutation from every other build, and nothing anywhere would say so.
    !!
    !! **The exposure is not theoretical and it is not exotic.** `fpm --profile release` and
    !! `--profile debug` both pass `-fp-model=precise`, so anything shipped or benchmarked is fine
    !! -- but a bare `fpm build`/`fpm test` passes no fp-model flag at all and therefore takes ifx's
    !! default. That is the ordinary development command. This check turns an invisible divergence
    !! into a loud one naming the cause.
    !!
    !! Costs 32 `exp_key` calls, some hundreds of nanoseconds, against a permutation that is at
    !! best `O(n log n)`. That is cheap enough to run on every call rather than caching in a saved
    !! flag -- which would need thread-safety reasoning to save nothing worth saving.
    logical function exp_key_contract_ok()
        integer(int64) :: fp
        real(real64) :: u
        integer :: k

        fp = 0_int64
        do k = 0, 31
            u = scale(0.5_real64 + real(mod(k * 5, 8), real64) / 16.0_real64, -k)
            fp = ieor(fp, transfer(exp_key(u), 0_int64))
            fp = fp * 6364136223846793005_int64 + 1442695040888963407_int64
        end do
        exp_key_contract_ok = fp == ek_contract_fp
    end function exp_key_contract_ok


end module parquet_expkey
