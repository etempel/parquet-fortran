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
    public :: ek_rnd
    public :: exp_key_contract_ok
    public :: parquet_debug_exp_key
    public :: parquet_debug_set_exp_key_contract

    !> Forces `exp_key_contract_ok` to report failure. **Test-only**, and the only writer is the
    !! debug hook below.
    !!
    !! Process-global, which is not a compromise but the only shape available: this module reaches
    !! no `bind(C)` surface, so the C++-side debug-hook convention every other override in this
    !! library uses cannot be applied here (see CLAUDE.md, "A Fortran-side debug hook has to be
    !! PUBLIC, so prefer a C++ one").
    !!
    !! **It is nonetheless free of the shared-state hazard**, because of where it is written from:
    !! its single caller is `scenario_weighted_contract_failure` in `test/error_scenarios.f90`,
    !! and an error scenario is one whole PROCESS that sets the flag and then aborts. No other test
    !! runs beside it, so there is no reader to race with and no vacuous-pass risk of the kind that
    !! forces `filter_screen`/`sorting`/`settings` out of test-drive's per-suite parallelism. Keep it
    !! that way: a new in-process caller of `parquet_debug_set_exp_key_contract` would need that
    !! whole suite excluded, since a sibling test flipping this flag mid-run would leave
    !! `exp_key_contract_ok` answering about someone else's override.
    !!
    !! nagfor's `-thread_safe` reports the assignment below regardless -- it is a static "writes a
    !! variable from an outer scope" test that cannot see any of the above. Answered here rather
    !! than removed: dropping the flag would take with it the negative control that stops a
    !! `exp_key_contract_ok` returning `.true.` unconditionally from passing every test.
    logical, save :: ek_dbg_force_fail = .false.

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

    !> The rounding barrier: an identity that the optimiser may not fuse across.
    !!
    !! **The `volatile` local is the entire content of this procedure.** Without it,
    !! `ek_rnd(a * b) + c` is just `a * b + c` -- the exact shape a compiler with FMA contracts,
    !! rounding once where IEEE rounds twice, which changes the result and moves the frozen
    !! fingerprint. `volatile` obliges the compiler to store `x` to memory and read it back, so the
    !! product is a rounded `real64` by the time the add sees it, on every conforming compiler.
    !! Unlike a `noinline` directive this is not advice the optimiser may decline, which is why the
    !! procedure is written this way and not that way -- see `exp_key`'s doc-comment for the flang
    !! build that made the difference concrete.
    !!
    !! Do not make this `pure` (a pure procedure may not have a `volatile` local -- that
    !! restriction is what the directive form was working around), do not drop the `volatile`, and
    !! do not "simplify" the call sites back to bare arithmetic.
    !!
    !! **Public because a second module needs the same barrier for the same reason.**
    !! `parquet_random`'s polar normal composes `sqrt` and a division around `exp_key`, and those
    !! compositions are exposed to exactly the fusions and regroupings this closes -- so it imports
    !! this as `ek_round`. It is not part of the library's user-facing surface (`parquet_expkey` is
    !! not re-exported by the `parquet` facade, and every importer re-hides it), and it is not a
    !! general-purpose utility: it exists to hold ONE expression's grouping, and a caller reaching
    !! for it should be able to name the rewrite it is stopping.
    function ek_rnd(x) result(y)
        real(real64), intent(in) :: x   !! the product to round
        real(real64) :: y               !! the same value, rounded to real64
        real(real64), volatile :: t     !! the barrier: forces a store and a reload

        t = x
        y = t
    end function ek_rnd

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
    !! **The cost is about 2x on the polynomial** and it buys a contract that holds under every
    !! IEEE-conforming flag set. Measured over 5M calls at `-O3 -funroll-loops -march=native` on
    !! machine C, best of 3: gfortran 10.58 ns unbarriered -> 21.86 ns (2.07x), flang 12.46 ->
    !! 23.80 ns (1.91x). (An earlier figure here, 6.58 -> 16.68 ns / 2.53x, was the `noinline`
    !! barrier on machine B; the shape of the conclusion is unchanged.) If the key loop ever
    !! dominates a real workload, the way to recover it is a table-driven reduction -- a 16-entry
    !! table of `log(m_i)` brings `|f|` under `2**-5` and the series down to six terms, halving the
    !! barriers -- not deleting the barriers.
    !!
    !! **`-ffast-math` / `-Ofast` / ifx's default `-fp-model=fast` ARE in scope, and the barrier on
    !! the final sum is what brings them in.** An earlier version of this note said the opposite --
    !! that no library could survive them -- on the strength of a build that broke. The diagnosis
    !! behind that was wrong: the failure was blamed on reciprocal approximation of `(m-1)/(m+1)`,
    !! and it is not division at all. Measured on ifx 2026.1.1 at `-O2` with the default fast model:
    !! `-prec-div` does NOT fix it, `-no-fma` does NOT fix it, and `-assume protect_parens` DOES.
    !! The transformation was **reassociation**, and it had exactly one place to bite -- `e = big +
    !! (small - logm)`, where the parentheses were the only thing holding the grouping and
    !! fast-math is licensed to ignore them. `big` is about `k * 0.693` and `small` about
    !! `k * 1.9e-10`, so regrouping to `(big + small) - logm` annihilates the low-order correction.
    !! Wrapping that subtraction in `ek_rnd` makes the grouping a memory fact rather than a
    !! syntactic one, and the fingerprint then holds at every level on both compilers, `-Ofast`
    !! included. It also repaired gfortran `-Ofast`, which was silently wrong before.
    !!
    !! **The lesson generalises past this file: a barrier on every PRODUCT is not a barrier on the
    !! expression.** Each `ek_rnd` here stops an FMA from spanning an add; none of them stops the
    !! adds being re-grouped among themselves. When a value is built as a large term plus a small
    !! correction, the grouping IS the algorithm, and it needs its own barrier.
    !!
    !! **A SUM feeding a PRODUCT is the second shape, and `ek_c(0)` being exactly 1.0 is what makes
    !! the optimiser want it.** The last Horner step is `poly = t + ek_c(0)`, which `logm` then
    !! multiplies by `f + f`. Under `reassoc`, `g * (t + 1.0)` distributes to `g*t + g` -- free,
    !! because multiplying by one vanishes -- and that form is a single FMA. The barriers on the two
    !! products either side did not help, because the unbarriered add BETWEEN them was the target;
    !! `ek_rnd(poly)` is what closes it. **Measured on machine A (arm64): `flang -O3 -ffast-math`
    !! and `-Ofast` moved the fingerprint to 7108262605301466839, differing on 148 of 13824 inputs
    !! by 1 ulp each.** Changing `ek_c(0)` from 1.0 to 0.9 in a scratch copy removes the FMA, which
    !! is what identifies the mechanism rather than merely correlating with it.
    !!
    !! **That one is ARCHITECTURE-gated, so the same compiler and flags can pass on one machine and
    !! fail on another.** The rewrite fires only where the result becomes one instruction: the same
    !! `-O3 -ffast-math` emits the FMA on aarch64, where FMA is baseline, and on x86-64 under
    !! `-march=haswell` -- and does NOT emit it on baseline x86-64, which has no FMA to contract
    !! into. That is why machine C reported clean while machine A failed, and it is the same trap as
    !! the original `-mfma` discovery: a hazard hidden by builds that happen not to enable FMA. Note
    !! `-ffp-contract=off` does not undo it, because flang stamps `contract` on the IR from
    !! `-ffast-math` and the later flag does not clear it.
    !!
    !! **NOT `pure`, and the impurity is the price of the barrier being GUARANTEED.** A `volatile`
    !! local is standard Fortran and every compiler must honour it; `!GCC$`/`!DIR$ ATTRIBUTES
    !! NOINLINE` are directives a compiler is free to ignore. This transform was built on the
    !! directives for a while, precisely so that `exp_key` could stay `pure` -- and flang 22.1.8
    !! then proved that unsound: it warns on the `!DIR$` spelling, ignores the `!GCC$` one in
    !! silence, inlines the helper and fuses. **Measured on machine C (x86-64, i7-10700K),
    !! `flang -O3 -march=native` moved the full fingerprint** to -8585622607960331921, differing on
    !! 4 of 13824 swept inputs by 1 ulp each. So the directive form did not merely risk losing the
    !! barrier; it had already lost it on a compiler this project builds under.
    !!
    !! **Purity cost nothing to give up and the barrier got FASTER.** Nothing needs `exp_key` to be
    !! pure: its only caller is `wperm_impl`'s ordinary serial `do` loop, and the two other
    !! entry points here (`exp_key_contract_ok`, `parquet_debug_exp_key`) were never pure either.
    !! A stack store/reload also beats an out-of-line call -- measured over 5M calls at `-O3
    !! -funroll-loops -march=native` on machine C, best of 3:
    !!
    !! | barrier | gfortran | flang | reproduces the fingerprint |
    !! |---|---|---|---|
    !! | `noinline` helper (former) | 30.30 ns | 13.87 ns | **flang: NO** |
    !! | `transfer` round trip | 35.12 ns | 1787.45 ns | yes, at 129x the cost on flang |
    !! | `volatile` local (current) | **21.91 ns** | **23.83 ns** | yes |
    !!
    !! The `transfer(transfer(x, 0_int64), 0.0_real64)` row is recorded because it looks like the
    !! obvious way to keep `pure`: it does block the fusion on both compilers, and it is
    !! unaffordable on flang, which lowers it through memory per call. Do not re-adopt it.
    !!
    !! Inlining is now harmless -- an inlined `volatile` store and reload is still a store and a
    !! reload -- so the directives are gone, and with them flang's `-Wignored-directive` warning.
    !! `tools/check_exp_key.sh` remains the thing that would notice a lost barrier: it compares the
    !! fingerprint across every configuration a compiler supports. Verified on gfortran 15.2.1
    !! (7/7, machines B and C), ifx 2026.1.1 (4/4, machine B) and flang 22.1.8 (6/6, machine C).
    !!
    !! **`exp_key_contract_ok` is NOT a second line of defence against this** -- see its own
    !! doc-comment. It samples 32 inputs and reproduced the frozen value under the diverging flang
    !! build, so it did not see the divergence at all. The script is the check that works.
    !!
    !! **The domain is not general and the guard is the caller's.** The race feeds this `1 - u` for
    !! a uniform `u` in `[0, 1)`, so the argument lies in `[2**-53, 1]` and is never zero, never
    !! denormal and never above 1. `exponent`/`fraction` are exact bit operations on a normal
    !! number, so nothing rounds before the polynomial does. `u = 1` gives exactly `0`.
    function exp_key(u) result(e)
        real(real64), intent(in) :: u   !! a uniform in `[2**-53, 1]`; nothing validates this
        real(real64) :: e               !! `-log(u)`, in `[0, 36.74]`
        real(real64) :: m, f, s, poly, en, big, small, logm
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
            poly = ek_rnd(poly * s) + ek_c(i)
        end do
        logm = ek_rnd((f + f) * ek_rnd(poly))
        en = real(-k, real64)
        big = ek_rnd(en * ek_log2_hi)
        small = ek_rnd(en * ek_log2_lo)
        e = big + ek_rnd(small - logm)
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
    !! **Why a run-time check for a compile-time property.** The barriers now cover every build
    !! this project can test, fast-math included, so this check is expected to pass everywhere --
    !! which is exactly why it must stay. It is the only thing that would report a compiler, a
    !! version or a flag nobody has swept doing something no barrier anticipated. A build that
    !! diverges produces a DIFFERENT permutation from every other build, silently, and a weighted
    !! permutation is decided by the ORDER of the keys, so one differing key changes which items
    !! are drawn.
    !!
    !! **It has already earned its place once.** Before the final sum was barriered, a bare
    !! `fpm build`/`fpm test` under ifx -- which passes no fp-model flag, so ifx takes its own
    !! `-O2` and `-fp-model=fast` defaults -- produced exactly that divergence, and this check is
    !! what reported it instead of letting the permutations quietly disagree.
    !!
    !! Costs 32 `exp_key` calls, some hundreds of nanoseconds, against a permutation that is at
    !! best `O(n log n)`. That is cheap enough to run on every call rather than caching in a saved
    !! flag -- which would need thread-safety reasoning to save nothing worth saving.
    !!
    !! **What this canNOT do, because 32 inputs is a sparse sample.** It catches a build that has
    !! left IEEE semantics wholesale, which is what it is for and what the wording above describes.
    !! It does NOT reliably catch a barrier that has gone missing: measured on machine C, the
    !! former `noinline` barrier was lost entirely under `flang -O3 -march=native`, and this check
    !! still reproduced `ek_contract_fp` exactly, because the divergence touched 4 of the 13824
    !! inputs `tools/check_exp_key.sh` sweeps and none of the 32 sampled here (this check varies
    !! only 8 distinct mantissas). So do not cite it as a second line of defence for the barrier --
    !! the script is what found that, and the script is what would find the next one.
    logical function exp_key_contract_ok()
        integer(int64) :: fp
        real(real64) :: u
        integer :: k

        fp = 0_int64
        do k = 0, 31
            u = scale(0.5_real64 + real(mod(k * 5, 8), real64) / 16.0_real64, -k)
            fp = ieor(fp, transfer(exp_key(u), 0_int64))
            fp = fp_step(fp)
        end do
        exp_key_contract_ok = fp == ek_contract_fp .and. .not. ek_dbg_force_fail
    end function exp_key_contract_ok

    !> One step of the fingerprint's mixer: `fp * 6364136223846793005 + 1442695040888963407`,
    !! modulo `2**64`, computed so that nothing overflows.
    !!
    !! **The value is unchanged -- this is the same LCG step, spelled without undefined behaviour**,
    !! so `ek_contract_fp` is the same constant it always was and a mismatch still means `exp_key`
    !! moved. The plain spelling relied on the multiply and the add both wrapping, which is
    !! undefined rather than merely implementation-defined: it aborts under `nagfor -C=intovf` and
    !! `gfortran -ftrapv`, and an optimiser is entitled to reason from it (see `feature_risks.md`
    !! Risk-94, where exactly that deleted a branch two functions away in a sibling module).
    !!
    !! 16-bit limbs for the product and 32-bit halves for the sum, which is the same construction
    !! `parquet_random`'s `mul64_lo_strict` and `add64` use; nothing here reaches `2**35`. This runs
    !! 32 times per contract check and never on a draw, so the limb form costs nothing worth
    !! measuring -- which is why it is unconditional rather than forked per compiler.
    pure function fp_step(fp) result(r)
        integer(int64), intent(in) :: fp            !! the fingerprint so far
        integer(int64) :: r                         !! `fp * K + C` modulo `2**64`
        integer(int64), parameter :: K = 6364136223846793005_int64
        integer(int64), parameter :: C = 1442695040888963407_int64
        integer(int64), parameter :: MASK16 = 65535_int64
        integer(int64), parameter :: MASK32 = 4294967295_int64
        integer(int64) :: a0, a1, a2, a3, b0, b1, b2, b3, d0, d1, d2, d3, acc, p, low, high
        a0 = iand(fp, MASK16)
        a1 = iand(ishft(fp, -16), MASK16)
        a2 = iand(ishft(fp, -32), MASK16)
        a3 = iand(ishft(fp, -48), MASK16)
        b0 = iand(K, MASK16)
        b1 = iand(ishft(K, -16), MASK16)
        b2 = iand(ishft(K, -32), MASK16)
        b3 = iand(ishft(K, -48), MASK16)
        ! Column by column, carrying as we go. The widest accumulator value is below 2**35.
        acc = a0 * b0
        d0 = iand(acc, MASK16)
        acc = ishft(acc, -16) + a0 * b1 + a1 * b0
        d1 = iand(acc, MASK16)
        acc = ishft(acc, -16) + a0 * b2 + a1 * b1 + a2 * b0
        d2 = iand(acc, MASK16)
        acc = ishft(acc, -16) + a0 * b3 + a1 * b2 + a2 * b1 + a3 * b0
        d3 = iand(acc, MASK16)
        p = ior(ior(d0, ishft(d1, 16)), ior(ishft(d2, 32), ishft(d3, 48)))
        ! The sum on 32-bit halves, so a carry into bit 63 is a bit operation and not an overflow.
        low = iand(p, MASK32) + iand(C, MASK32)
        high = ishft(p, -32) + ishft(C, -32) + ishft(low, -32)
        r = ior(ishft(iand(high, MASK32), 32), iand(low, MASK32))
    end function fp_step


    !> Forces the frozen-transform check to fail. **Test-only.**
    !!
    !! Public only because it has to be: this module reaches no `bind(C)` surface, so the C++-side
    !! debug-hook convention is unavailable to it. It exists to give `exp_key_contract_ok` a
    !! negative control -- without one, a check that returned `.true.` unconditionally would pass
    !! every test ever written for it, which is exactly the failure mode a guard cannot afford.
    !! Reaching the real failure needs a build with `-ffast-math` or ifx's default `-fp-model=fast`,
    !! which no in-process test can produce.
    !!
    !! It is excluded from README.md's API overview and no library code calls it.
    subroutine parquet_debug_set_exp_key_contract(ok)
        logical, intent(in) :: ok   !! `.false.` makes the contract check report failure

        ek_dbg_force_fail = .not. ok
    end subroutine parquet_debug_set_exp_key_contract

end module parquet_expkey
