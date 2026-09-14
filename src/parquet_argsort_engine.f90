!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
! NOT a generated file -- unlike every other src/parquet_argsort*.f90 and
! src/parquet_sorting*.f90, which tools/generate_parquet_sorting.py emits. Edit THIS file directly.
! Its procedures' INTERFACES do live in the generated src/parquet_argsort.f90;
! change those in the generator's emit_engine_interfaces(), never in the generated output.
!
!> The serial core of the pure-Fortran sort engine: the ordering, and the introsort over it.
!!
!! Stages 1 and 2 of `feature_sort.md`. The comparators decide **what order rows go in**;
!! `sort_comparison_permutation` is the serial sort that uses them. The shipped path still crosses
!! `bind(C)` into the C++ engine — this side is reached only when
!! `parquet_debug_use_fortran_sort_engine(.true.)` has selected it, which is Stage 2 scaffolding and
!! goes away at the Stage 6 cutover. What proves this code correct is `test/test_sorting.f90`, which
!! asks the C++ engine the same questions — pair by pair for the comparators, whole-permutation for
!! the sort — and requires the same answers.
!!
!! **Both halves live in one file on purpose.** A call from a sibling submodule into `sort_row_less`
!! is a call to a global symbol in another translation unit, which no compiler can inline; keeping
!! the sort beside the comparator is the only thing that leaves inlining possible at all. It is not
!! sufficient — `feature_sort.md` §1e-B records that ELF semantic interposition under `-fPIC` blocks
!! it anyway on machine B — but a split would remove the possibility on every platform. Do not
!! separate them for tidiness.
!!
!! **`sort_compare_key` and `sort_tier_of` must keep FITTING GCC's default inlining budget, and
!! nothing fails when they stop.** Past the budget GCC splits `sort_compare_key` into a
!! `sort_compare_key.part.0` clone, inlines a cheap prologue and leaves the body out of line, so the
!! hot path takes a call on every comparison — on the critical path of a dependent branch chain.
!! Every answer stays identical and only speed moves: measured on machine A, gfortran 15.2,
!! `--profile release`, the serial `f64` argsort ran at 1.28x the C++ engine without the split and
!! 0.91x with it, a ~39% swing decided entirely by whether one procedure fit. Two places it hides:
!! `sort_row_less` is fully inlined either way, so grepping for a call to *it* reports success while
!! the damage sits one level down; and `bench/benchmark_sort_comparator.f90` does not see it either,
!! because a sweep's iterations are independent and the call overlaps with them where a partition's
!! next iteration depends on this comparison's branch. **So anything added to either procedure must
!! be paid for by taking something else out** — a new tier, a new key family arm written inline
!! rather than behind a call (as `compare_bytes` already is), a validity scheme needing more than one
!! test, or a branch hoisted "for clarity" can each cross the threshold, and all of them look free.
!! The check is one command against a release build, and it must read 0:
!!
!! ```
!! nm <build>/.../src_parquet_argsort_engine.f90.o | grep -c 'sort_compare_key\.part'
!! ```
!!
!! `bench/benchmark_sort_ab.sh` sizes the loss once the symbol is seen. The budget is a property of
!! the compiler and its version, so a future GCC may reintroduce the split with no source change.
!!
!! **Why a submodule rather than a module.** `sort_key_buf` is private to `parquet_sorting`, so a
!! standalone module could not see the type these procedures exist to compare.
!!
!! ## The contract, in four sentences
!!
!! Each row sits in exactly one **tier** under each key — `value`, `NaN` (real keys only) or `null` —
!! and the tiers are **absolute**: `descending` reorders *within* the value tier and never moves a
!! null or a NaN. Two rows in the same non-value tier compare **equal**, so they keep file order.
!! The sort comparator then adds a **row-index tiebreaker**, which makes it a total order in which no
!! two distinct rows compare equal. The tie-free comparator is the same walk without that tiebreaker,
!! and takes an `nkeys` prefix.
!!
!! ## Two things that are easy to get wrong and invisible when you do
!!
!! **The tier rule is invisible to any ascending, null-free test.** `feature_sort.md` §5.1 calls it
!! "the single most likely thing to get wrong here". A comparator that applies `descending` before
!! the tier test passes every ordinary test and silently moves nulls to the other end of a descending
!! sort.
!!
!! **String keys compare like `memcmp`, not like Fortran.** Fortran's own `<` on `character` blank-pads
!! the shorter operand, so `"ab" == "ab "`; `std::string_view::compare` — which this must reproduce
!! exactly — treats a prefix as *less*, so `"ab" < "ab "`. `compare_bytes` below does it byte by byte
!! and then by length, and reads each byte as **unsigned**, because `char_traits<char>::compare` is
!! `memcmp` and a byte ≥ 128 must sort high.
!!
!! ## Keep the two comparators adjacent
!!
!! `sort_row_less` and `sort_keys_compare` are one decision expressed twice, and `feature_risks.md`
!! **Risk-34** is about them never drifting apart. Both walk the keys in precedence order and both
!! delegate every actual comparison to `sort_compare_key`. A change to one is a change to the other.
!!
!! ## Performance shape (feature_sort.md §6 Stage 1e)
!!
!! Every dummy here is a plain `type(sort_key_buf)`, never `class`. Passing a `type(T)` actual to a
!! `class(T)` dummy across compilation units makes ifx build a runtime class descriptor in the
!! *caller's* prologue — one record per allocatable component, and `sort_key_buf` has five — which
!! this library has already measured at ~35 ns per call on a comparable type. Nothing in this file
!! may become polymorphic, and the `objdump` check in CLAUDE.md's typed-accessor-tier section is what
!! confirms it.
!!
!! An earlier note here said Stage 2 was where the value arrays get hoisted — resolving
!! `keys(k)%ints`/`%reals` into local `contiguous` pointers once, rather than re-reaching through the
!! derived type on every comparison. **It was not done, and the reason is worth recording**: hoisting
!! means a comparator that takes the hoisted state, i.e. a *second* expression of the ordering, and
!! `feature_risks.md` Risk-34 is precisely about those two never drifting apart. That is a real cost
!! against an unmeasured gain, so it stays a measurement to take later rather than a design decision
!! taken now — and if it is ever taken, the hoisted comparator must be generated from the same source
!! as this one, not written twice.
submodule (parquet_argsort) parquet_argsort_engine
#ifdef _OPENMP
    ! Stage 4. Guarded because a build without OpenMP must still COMPILE, not merely run serially:
    ! machine A's flang ships no `omp_lib.mod` at all, so an unguarded `use` here fails the build
    ! outright rather than taking the serial fallback the guards exist to provide. This project has
    ! shipped exactly that defect once, in `materialize_marked_parallel` (src/parquet_tables_read.f90),
    ! where it was invisible on both compilers that always supply the flag. Every `!$omp` construct
    ! below is guarded to match, and a flang build is the check that they all are.
    use omp_lib
#endif
    implicit none
    !
    !> Ranges of this size or smaller are left to the final insertion pass, as `std::sort` does.
    integer(int64), parameter :: SORT_INSERTION_CUTOFF = 16_int64
    !> Below this row count the single-key radix path declines and the introsort runs.
    !!
    !! **The crossover is MACHINE-DEPENDENT, and this value is a cross-machine choice rather than
    !! any one machine's measurement.** The radix path's fixed cost is zeroing the 8 x 256 histogram
    !! (16 KB) plus 2 KB of cursors, which does not scale with `n`; against that, the introsort's
    !! per-element cost grows with `log n`. Where the two cross depends on how many of the eight
    !! byte passes a family can skip, and on how well the host predicts the comparison sort's
    !! branches -- so it moves between machines. Measured on machine A (macOS, arm64, gfortran 15.2),
    !! radix / introsort at `--inner=300`, under 1.0 being a radix win:
    !!
    !!       n      i32    i64    f32    f64    str
    !!       96    1.47   1.42   0.66   0.84   0.62
    !!      128    1.34   0.99   0.49   0.63   0.46
    !!      192    1.22   0.77   0.36   0.44   0.35
    !!      256    0.81   0.67   0.28   0.37   0.28
    !!
    !! **`i32` still loses at 128 on machine A, and that is accepted deliberately.** A second
    !! machine measures every family, `i32` included, winning at 128, and the maintainer set the
    !! floor there on that basis. The cost is that an `int32` sort of 128-255 rows is somewhat
    !! slower on this architecture than the introsort would have been; the benefit is that every
    !! other family, and every family on the other machine, gets the radix ~2x sooner.
    !! **Do NOT raise this back to 256 on the strength of a machine-A measurement alone** -- that
    !! is the measurement above, it was already considered, and the decision went the other way.
    !!
    !! **What key widening costs an integer key, and what it does not explain.** Key extraction
    !! widens an int32 to int64, and a widened NEGATIVE value sign-extends to `0xFFFFFFFF...`, so a
    !! signed column's top four bytes take two distinct values and none of those four passes can be
    !! skipped. That mechanism is real and measured, and `sort_radix_permutation` now removes it: an
    !! integer key whose value SPAN is under 2^32 is imaged as `v - vmin`, whose top four bytes are
    !! constant, so those four passes are skipped after all. A float32 widened to float64 zeroes its
    !! low mantissa bytes instead, which is why `f32` skips passes for free.
    !!
    !! **It does not explain why `i32` sits latest in the ladder above, and an earlier version of
    !! this note claimed it did.** `i64` runs all eight passes too and crosses over EARLIER, so pass
    !! count cannot separate the two arms. The residual gap between two eight-pass integer arms is
    !! not established -- `feature_sort_radix.md` section 13.8 records the experiment that ruled out
    !! the two obvious explanations (the pass count, and the int32-to-int64 widening itself, worth
    !! 0.52 ns of a 4.1 ns gap) and the machines disagree on the sign of what remains. Do not guess
    !! at a third explanation here; measure it, or leave the question open as it stands.
    !!
    !! Overridable in both directions by `parquet_debug_set_sort_radix_min_rows`, which is what
    !! keeps the introsort's and the counting path's own negative controls non-vacuous now that this
    !! sits below their fixture sizes -- see `engine_only_introsort` (`test/test_sorting.f90`) and
    !! `feature_sort_radix.md`.
    integer(int64), parameter :: SORT_RADIX_MIN_ROWS = 128_int64
    !> Rows at or above which a SELECTION answers by ORDERING rather than by quickselecting.
    !!
    !! **Quickselect is asymptotically cheaper and measurably slower, at every size that matters.**
    !! `sort_nth_index`'s introselect is O(n) against the ordering engine's O(n log n), and it loses
    !! anyway for two compounding reasons. It compares through an index permutation, so every one of
    !! its ~2n comparisons is a random-access load into the key arrays, where the radix path never
    !! calls the comparator at all and walks memory in order. And it has no threaded form, where the
    !! ordering engine has had one since Stage 4. Measured on machine A (gfortran 15.2, real64,
    !! rank n/2, best of 7), `pf_nth_element` against `pf_argsort` + one index:
    !!
    !! | n | select, 8 threads | order, 8 threads | select, serial | order, serial |
    !! |---|---|---|---|---|
    !! | 100 | 0.013 ms | 0.027 ms | 0.010 ms | 0.021 ms |
    !! | 1 000 | 0.122 | 0.060 | 0.097 | 0.047 |
    !! | 10 000 | 0.896 | 0.526 | 0.767 | 0.443 |
    !! | 100 000 | 4.748 | 2.690 | 4.745 | 4.470 |
    !! | 1 000 000 | 59.148 | 21.377 | 62.111 | 49.391 |
    !! | 4 000 000 | 366.999 | 98.192 | 386.293 | 200.965 |
    !! | 16 000 000 | 1123.567 | 315.330 | 1179.944 | 803.804 |
    !!
    !! Ordering wins everywhere from 1000 up, and by more as `n` grows; only at 100 does the
    !! ordering engine's fixed setup cost dominate. The floor is therefore set inside that gap and
    !! deliberately nearer the losing end, because below it the absolute numbers are microseconds
    !! and nothing is at stake either way.
    !!
    !! **The two routes cannot disagree**, which is what makes this a pure performance choice: a
    !! selection is DEFINED here as the row a full stable sort would place at that rank, and
    !! `sort_row_less` ends in a row-index tiebreaker, so exactly one permutation is correct and
    !! both routes must find the same element. Overridable in both directions by
    !! `parquet_debug_set_sort_nth_order_min` -- needed because every fixture in the suite is far
    !! below this floor, so the shipped route would otherwise never be exercised.
    integer(int64), parameter :: SORT_NTH_ORDER_MIN = 256_int64
    !> Bytes of a string key that go into the radix; the rest is settled by the refine pass.
    integer(int64), parameter :: SORT_RADIX_PREFIX = 8_int64
    !> Rows a refine sub-bucket loop must have PER THREAD before it is worth opening a team.
    !!
    !! Same reasoning and same value as `SORT_REFINE_ELEMS_PER_THREAD` in `sort_radix_design_b`: the
    !! pass is threaded over one run's range, so what decides is elements per thread, not total rows.
    integer(int64), parameter :: SORT_REFINE_MIN_ROWS = 2048_int64
    !> Byte position past which the deep string refine stops recursing and lets the introsort finish.
    !!
    !! Bounds the recursion, whose depth is the run's common prefix length -- caller data, and so
    !! unbounded. Each frame holds a 256-element counter array (2 KB), so 64 bytes is at most ~128 KB
    !! of stack. Past it the introsort is correct, merely without the radix's help; a column whose
    !! values share a 64-byte prefix is pathological rather than ordinary.
    integer(int64), parameter :: SORT_RADIX_MAX_BYTE = 64_int64
    !> The int64 sign bit as a VALUE, since `ishft(1_int64, 63)` is awkward in a constant expression.
    integer(int64), parameter :: SORT_SIGN_BIT = -huge(0_int64) - 1_int64
    !
    ! ---- NaN detection: `x /= x`, deliberately NOT ieee_is_nan -------------------------------
    !
    ! `.claude/rules/fortran-gotchas.md`'s "General Fortran & language gotchas" says to test for NaN with `ieee_is_nan` rather than
    ! this idiom, because this one trips gfortran's -Wcompare-reals. **That convention is knowingly
    ! broken here, and only here**, for one measured reason:
    !
    ! **Under ifx, `ieee_is_nan` is a call into the Intel Fortran runtime, not an instruction.** The
    ! disassembly shows two `ieee_arithmetic_mp_for_ieee_is_nan_k8_` PLT calls per comparison, fired
    ! only on real keys -- exactly the shape of the f64-specific penalty machine B measured (+3.45 ns
    ! over its own i64 arm). gfortran inlines the same intrinsic to a native compare, so machine A
    ! never saw it. Measured on machine B, ifx 2026.1.1, ns per test above a bare `<`:
    !
    !     ieee_is_nan  +2.479      x /= x  +0.034      integer bit test  +0.003
    !
    ! **The bit test looks best there and was tried first; it is NOT used, because the probe
    ! mispredicts it.** In context on machine A it made the f64 arm 21% WORSE (3.52 -> 4.27 ns): on
    ! arm64 `ieee_is_nan` is a single native `fcmp`, while the bit test needs an FP-to-integer
    ! register move that stalls inside the comparator's dependency chain -- a cost the probe's
    ! independent iterations hide. `x /= x` measured 3.58 ns there, i.e. baseline within noise, while
    ! costing ifx almost nothing. It is the only one of the three that is cheap on both.
    !
    ! The general lesson, which is why this is written down at length: **a microbenchmark of an
    ! isolated operation does not predict its cost inside a dependency chain.** The probe was right
    ! about `ieee_is_nan` and wrong about its replacement.
    !
    ! `x /= x` is exact -- a NaN is the only value not equal to itself -- and correct for both
    ! infinities.
    !
    ! **And it turns out to cost no warning either, at least here.** `fpm build --profile debug`
    ! (which does carry `-Wall -Wextra`, checked) compiles this file with **zero** diagnostics on
    ! gfortran 15.2. CLAUDE.md's convention says the idiom "triggers gfortran's -Wcompare-reals
    ! warning"; that is evidently not true of a SELF-comparison on this version, whatever it may be
    ! for two distinct operands. Do not take this as settled for every compiler -- if a build does
    ! warn here, the answer is to suppress or accept it, NOT to restore `ieee_is_nan`, which costs
    ! ifx ~2.5 ns per test.
    !
contains

    ! ---- The two comparators. Adjacent on purpose -- Risk-34. ----------------------------------

    module procedure sort_tier_of
        ! **This returns the RAW tier and `nulls_first` does not appear.** That flag only ever
        ! REVERSES the order of the three tiers -- values(0)/NaNs(1)/nulls(2) becomes
        ! nulls(0)/NaNs(1)/values(2), which is `2 - tier` -- so `sort_compare_key` applies it once,
        ! by negating the tier comparison, instead of this procedure relabelling on every call.
        !
        ! That is not a tidy-up. This whole chain has to fit inside GCC's default inlining budget or
        ! the sort pays a CALL per comparison, and the pair of nested if-chains this replaces was
        ! most of the reason it did not: `sort_compare_key` was being split into a `.part.0` clone
        ! that the hot path called every time. Keep it small. See this file's header.
        !
        ! `descending` deliberately does not appear here either. A descending sort still puts nulls
        ! last by default; it does not flip them to the front.
        tier = 0
        ! `x /= x` rather than `ieee_is_nan` -- see this submodule's header. A deliberate, measured
        ! exception to a project-wide convention, not an oversight. A null row's slot is read here
        ! where it was previously skipped, which is harmless: the load is in-bounds, and whatever it
        ! answers is overwritten by the null test below.
        if (key%family == SK_REAL) then
            if (key%reals(i) /= key%reals(i)) tier = 1
        end if
        ! An UNALLOCATED `valid` means "this key has no nulls at all" -- the fast path, and the
        ! first thing a port of the C++ side gets wrong, because there the same state is an empty
        ! vector. Every caller must be safe against it. Tested LAST so that null wins over NaN.
        if (allocated(key%valid)) then
            if (key%valid(i) == 0_c_int8_t) tier = 2
        end if
    end procedure sort_tier_of

    !> Whether ANY row can land in a non-value tier under this key -- asked once per key, where
    !! `sort_tier_of` answers per row.
    !!
    !! **It sits here, immediately beside `sort_tier_of`, because it is the same rule and the two
    !! must not drift.** It is a second statement of which families can carry a tier, and the failure
    !! mode if they disagree is silent: a caller that skips its tier pass on this answer would leave
    !! a key's nulls or NaNs unordered, producing a wrong permutation with no abort. Adjacency is the
    !! enforcement -- a family that grows a tier has to be added to both, and they are four lines
    !! apart. Do not reimplement either test at a call site.
    !!
    !! Its whole purpose is to let a caller skip work rather than discover afterwards that there was
    !! none to do: the tier pass otherwise scans every row to count tiers and only THEN finds every
    !! row in one of them, which is the ordinary case for an ordinary column.
    function key_has_tiers(key) result(has)
        type(sort_key_buf), intent(in) :: key !! the bound key.
        logical :: has                        !! .true. when a null or a NaN is possible.
        !
        has = allocated(key%valid) .or. key%family == SK_REAL
    end function key_has_tiers

    module procedure sort_compare_key
        integer :: ta, tb       !! RAW tiers of `a` and `b`: 0 value, 1 NaN, 2 null.
        integer(int64) :: ia, ib !! integer key values.
        real(real64) :: ra, rb   !! real key values.
        !
        ta = sort_tier_of(key, a)
        tb = sort_tier_of(key, b)
        if (ta /= tb) then
            c = -1
            if (ta > tb) c = 1
            ! `nulls_first` reverses the tier ORDER and nothing else -- see `sort_tier_of`. This is
            ! the single place it is applied, and it is applied to the TIER comparison only, never
            ! to a value comparison.
            if (key%nulls_first) c = -c
            return
        end if
        !
        ! Same tier. If it is not the VALUE tier then both rows are null, or both are NaN, and the
        ! answer is EQUAL -- which is what leaves them in file order once the caller's index
        ! tiebreaker runs. The raw value tier is 0 whatever `nulls_first` says, which is the point of
        ! keeping the tiers raw.
        if (ta /= 0) then
            c = 0
            return
        end if
        !
        c = 0
        select case (key%family)
        case (SK_INT)
            ia = key%ints(a)
            ib = key%ints(b)
            if (ia < ib) then
                c = -1
            else if (ia > ib) then
                c = 1
            end if
        case (SK_REAL)
            ra = key%reals(a)
            rb = key%reals(b)
            if (ra < rb) then
                c = -1
            else if (ra > rb) then
                c = 1
            end if
        case default
            c = compare_bytes(key, a, b)
        end select
        !
        ! `descending` reverses the VALUE tier only -- every early return above skipped it.
        if (key%descending) c = -c
    end procedure sort_compare_key

    module procedure sort_row_less
        integer :: k !! key index.
        integer :: c !! this key's three-way answer.
        !
        do k = 1, size(keys)
            c = sort_compare_key(keys(k), a, b)
            if (c /= 0) then
                less = (c < 0)
                return
            end if
        end do
        !
        ! THE tiebreaker. It is what makes this a total order in which no two distinct rows compare
        ! equal, and three separate contracts rest on that: an unstable sort produces the stable
        ! answer, `nth_element` becomes deterministic, and a parallel result is bit-identical to the
        ! serial one by construction rather than by luck. Removing this line breaks all three
        ! silently -- every one of them still returns a correctly *sorted* answer.
        less = (a < b)
    end procedure sort_row_less

    module procedure sort_keys_compare
        integer :: k  !! key index.
        integer :: nk !! `nkeys`, clamped.
        !
        ! Clamped rather than validated: the C++ side clamps too, and a prefix longer than the key
        ! list is a caller asking for "all of them".
        nk = nkeys
        if (nk > size(keys)) nk = size(keys)
        !
        c = 0
        do k = 1, nk
            c = sort_compare_key(keys(k), a, b)
            if (c /= 0) return
        end do
    end procedure sort_keys_compare

    ! ---- String keys ---------------------------------------------------------------------------

    !> Bytewise comparison of two string-key rows: `memcmp` semantics, deliberately NOT Fortran's.
    !!
    !! Row `k` occupies `key%data(key%offsets(k) + 1 : key%offsets(k + 1))` — `offsets` holds 0-based
    !! byte positions in a 1-based array, which is the layout the C++ side indexes directly.
    !!
    !! Two departures from Fortran's own `character` comparison, both required to match
    !! `std::string_view::compare`: the shorter string is **less** when it is a prefix of the longer
    !! (Fortran would blank-pad and call them equal), and bytes are read as **unsigned** so that a
    !! byte ≥ 128 sorts above every ASCII one. `iand(..., 255)` is what guarantees the second
    !! regardless of whether the processor's `iachar` hands back a signed value.
    function compare_bytes(key, a, b) result(c)
        type(sort_key_buf), intent(in) :: key !! the bound string key.
        integer(int64), intent(in) :: a       !! first row, 1-based.
        integer(int64), intent(in) :: b       !! second row, 1-based.
        integer :: c                          !! -1, 0 or +1.
        !
        integer(int64) :: pa, pb !! first byte of each row, 1-based into `data`.
        integer(int64) :: na, nb !! byte length of each row.
        integer(int64) :: k, m   !! loop index, and the common prefix length.
        integer :: ba, bb        !! one byte from each row, as an unsigned 0..255.
        !
        pa = key%offsets(a) + 1_int64
        na = key%offsets(a + 1_int64) - key%offsets(a)
        pb = key%offsets(b) + 1_int64
        nb = key%offsets(b + 1_int64) - key%offsets(b)
        m = min(na, nb)
        !
        c = 0
        do k = 0_int64, m - 1_int64
            ba = iand(iachar(key%data(pa + k)), 255)
            bb = iand(iachar(key%data(pb + k)), 255)
            if (ba /= bb) then
                c = -1
                if (ba > bb) c = 1
                return
            end if
        end do
        !
        ! Equal over the common prefix: the shorter one is less.
        if (na < nb) then
            c = -1
        else if (na > nb) then
            c = 1
        end if
    end function compare_bytes

    ! ---- The engine entry point, and the integer counting fast path -----------------------------
    !
    ! Ported clause for clause from `sort_build_permutation`/`sort_counting_candidate`/
    ! `sort_counting_permutation` (src/parquet_wrapper.cpp), deliberately: every clause there encodes
    ! a measured or a correctness fact, and re-deriving them from the idea of a counting sort is how
    ! one of them gets lost. The C++ comments are the long-form reasoning; what follows states what
    ! must not change.
    !
    ! **The range scan skips nulls, and that is correctness rather than tidiness.** A null row's key
    ! slot holds whatever the buffer contained -- Arrow promises nothing there -- so counting it can
    ! widen the range past the bucket limit and decline the fast path for no reason, or size a bucket
    ! domain from garbage.
    !
    ! **A null-bearing key is ACCEPTED.** Declining one cost the entire fast path to a single null
    ! anywhere in the column: a measured 4.6-5.6x cliff at 0.1% null density on a 4M-row int32
    ! column, i.e. a step function of WHETHER a null exists rather than of how many. Nulls are
    ! tractable because they are a TIER here and never a value, so they form one contiguous block.
    !
    ! **`value_base` and `null_pos` do not mention `descending`, and must not learn to.** A
    ! descending sort still puts nulls last; `sort_compare_key` applies the tier test before the
    ! descending negation, and this path has to agree. It is the single most likely thing to get
    ! wrong here and it is invisible to any ascending test.
    !
    ! **The descending offset pass runs top-down** so the largest value lands at the front of the
    ! VALUE BLOCK while ties keep file order.
    !
    ! **Stability is by construction**: one forward pass in index order emits equal values in
    ! increasing row index, which is exactly what `sort_row_less`'s index tiebreaker produces. That
    ! is why the two paths can be A/B'd for equality at all.

    module procedure sort_build_permutation
        ! A forwarder rather than a copy, deliberately: this and `sort_build_permutation_threaded`
        ! must not be able to drift into answering differently, and the cheapest guarantee of that
        ! is that there is exactly one body for both to share.
        call sort_build_permutation_impl(keys, n, perm, 1)
    end procedure sort_build_permutation

    module procedure sort_build_permutation_threaded
        integer :: nt !! the team this build will actually open.
        !> Absolute row floor below which a team never pays, whatever the team size.
        integer(int64), parameter :: SORT_ENGINE_MIN_ROWS = 32768_int64
        !> Extra rows demanded per thread on top of that floor.
        integer(int64), parameter :: SORT_ENGINE_ELEMS_PER_THREAD = 2048_int64
        integer(int64) :: floor_rows !! resolved floor, after any debug override.
        !
        ! Two clauses, and both need the DATA to decide, which is why they live here rather than in
        ! `resolve_thread_count` with the rest of the thread policy.
        !
        ! **One thread is the SERIAL path, not a team of one.** The bucket-decomposed engine costs
        ! 0.85x of serial on one thread at n = 1e6 and only reaches 1.02x at n = 2e7
        ! (`feature_sort_parallel.md` §2.7), so a team of one is a regression at every row count --
        ! and at every row count a test can afford, by the wider of those two margins.
        !
        ! **The row floor** is where a team stops paying for itself at all. It used to read the
        ! published `sort_parallel_min_rows` setting, on the reasoning that its right value is a
        ! property of the machine and so belongs to the user. It is now derived instead, because the
        ! part that is a property of the machine is the TEAM SIZE, which the library already knows --
        ! and asking a user to track it by hand is asking them to re-tune on every machine.
        !
        ! `max(32768, 2048 * nt)`, measured on machine B with `benchmark_sort_tail` (threaded against
        ! serial, over n x team, `--profile release`). The flat 8192 it replaces is wrong by **4.83x
        ! under ifx and 20.11x under gfortran** at their worst points, with geometric means of 1.230
        ! and 1.429; this rule's worst cases are 1.10x and 1.17x, geometric means 1.007 and 1.006.
        ! Both compilers picked the same rule out of nine candidates, so no weighting was needed.
        !
        ! **The published `sort_parallel_min_rows` setting has been RETIRED** -- this rule replaced
        ! it here, and the C++ engine now uses its own internal constant `kSortParallelMinRows`
        ! (`src/parquet_wrapper.cpp`), overridable for tests only. See `feature_sort_report.md` §14.
        nt = 1
        floor_rows = max(SORT_ENGINE_MIN_ROWS, SORT_ENGINE_ELEMS_PER_THREAD * nthreads)
        if (dbg_sort_engine_min_rows >= 0_int64) floor_rows = dbg_sort_engine_min_rows
        if (nthreads > 1_int64 .and. n >= floor_rows) then
            ! Clamped into default INTEGER, which is what every OpenMP clause below takes. A caller
            ! may pass any `threads=` it likes, including a silly one, so the clamp belongs here --
            ! at the point the region is opened -- rather than in the policy layer.
            nt = int(min(nthreads, int(huge(0), int64)))
#ifdef _OPENMP
            ! **Never more threads than the machine has processors** (`feature_sort_parallel.md` §11
            ! step 6, §7.2). Measured on machine A, full-range int64, n = 5e6: without this clamp
            ! `threads=64` on eight cores costs **19.59 ns/element against 6.48 at 32** -- i.e. an
            ! already-parallel sort dragged back to its serial speed by oversubscription alone. The
            ! clamp belongs HERE, where the region is opened, rather than in `resolve_thread_count`:
            ! an explicit `threads=` is the caller saying what they want, and they are entitled to
            ! name a silly number without the policy layer second-guessing it everywhere else.
            if (nt > omp_get_num_procs()) nt = omp_get_num_procs()
            if (nt < 1) nt = 1
#endif
        end if
        call sort_build_permutation_impl(keys, n, perm, nt)
    end procedure sort_build_permutation_threaded

    !> The engine's one body: the counting fast path where it applies, then the radix, then the
    !! introsort — using `nt` threads in the phases that can take them.
    !!
    !! **The counting path and the introsort stay serial, deliberately.** The counting path is
    !! already 1.5–1.85x faster than the C++ one at 3.4 ns/element, so a thread team could plausibly
    !! cost more than the work it divides; `feature_sort_parallel.md` §11 step 7 sequences it last
    !! and gates it on a measurement of a real table sort rather than on the argument that it looks
    !! parallelisable. The introsort is the out-of-memory fallback and is reached only when the radix
    !! could not allocate, which is not a path worth threading.
    subroutine sort_build_permutation_impl(keys, n, perm, nt)
        use parquet_settings_base, only : parquet_get_sort_counting_path
        type(sort_key_buf), intent(in) :: keys(:) !! the keys, in precedence order.
        integer(int64), intent(in) :: n           !! rows to order.
        integer(int64), intent(inout) :: perm(:)  !! receives `n` 1-based row indices.
        integer, intent(in) :: nt                 !! team size; 1 is the serial path.
        integer(int64) :: lo, hi !! the counting path's value range, carried from the candidate test.
        logical :: did_radix     !! .false. when the radix path declined for want of memory.
        logical :: take_counting !! resolved counting-path decision, once the range is known.
        integer(int64) :: max_count_nt !! resolved team ceiling, after any debug override.
        !> Largest team that may still take the SERIAL counting sort.
        !!
        !! **The C++ engine -- the shipped one -- has never gated this on the team at all**
        !! (`sort_build_permutation` in `src/parquet_wrapper.cpp`: "the counting path is already
        !! O(n) and already produces this exact permutation, so it wins over any number of threads").
        !! This engine gated it at 1, and machine A reported the consequence: a low-cardinality
        !! integer key was 2.0x SLOWER at two threads than before the parallel campaign, because the
        !! key fell counting -> radix -> Design B declines -> Design A, ending worse than it started.
        !!
        !! Reproduced on machine B under ifx (counting 2.903 against a 2-thread radix at 4.237,
        !! 1.46x), which is the finding that settles it: **the two-thread loss is not the gfortran
        !! radix-constant defect of section 11.3 and is not protected by the decision to weight ifx,
        !! because ifx loses there too.** The grid that set the original rule stepped 1, 4, 16, 64
        !! threads and never measured two.
        !!
        !! Held at 2 rather than higher because that is where the compilers stop agreeing: from four
        !! threads ifx's radix wins at this range (2.067 against 2.903) while gfortran's does not
        !! until past eight. Raising it would be tuning for gfortran against ifx, which is the
        !! trade-off the maintainer's weighting rule resolves the other way.
        integer(int64), parameter :: SORT_COUNTING_MAX_THREADS = 2_int64
        !
        ! Recorded here rather than in either entry point, so the counter is current whichever one
        ! was called and a test can never read a figure left behind by an earlier sort. Nothing else
        ! can observe the team size -- see `parquet_debug_sort_threads_used`.
        dbg_sort_threads_used = int(nt, int64)
        ! Cleared per build, not merely set on success: "0 means Design B did not run" is the
        ! documented contract, and without this a decline would report whatever the PREVIOUS sort's
        ! split produced. A test asserting a decline would then pass or fail on call order.
        dbg_sort_split_buckets = 0_int64
        dbg_sort_design = 0_int64
        dbg_sort_refine_runs = 0_int64
        ! Nested rather than `.and.`-ed: Fortran does not short-circuit, so the one-line form would
        ! run the candidate's O(n) range scan even with the counting path switched off -- and would
        ! define `lo`/`hi` as a side effect while doing it. CLAUDE.md records both halves of this.
        ! **The team test comes FIRST, before the candidate's O(n) range scan.** The scan walks every
        ! row to find `lo`/`hi`, and a team above the ceiling is going to decline whatever it finds --
        ! so testing `nt` afterwards spends a whole extra pass over the column to reach a decision
        ! that was already made. Measured: it cost 0.6 ns/element of a 4.4 ns sort (ifx, n = 5e6,
        ! range/n = 2e-4, 4 threads) before the tests were nested this way.
        !
        ! Nested rather than `.and.`-ed, for the reason CLAUDE.md records: Fortran does not
        ! short-circuit, so the one-line form would run the scan anyway and define `lo`/`hi` as a side
        ! effect while doing it.
        max_count_nt = SORT_COUNTING_MAX_THREADS
        if (dbg_sort_counting_max_threads >= 0_int64) max_count_nt = dbg_sort_counting_max_threads
        if (int(nt, int64) <= max_count_nt) then
            if (parquet_get_sort_counting_path()) then
            if (sort_counting_candidate(keys, n, lo, hi)) then
                ! **The counting sort is SERIAL, so whether it wins is a function of the TEAM, not
                ! of the value range alone.** `sort_counting_candidate` answers the range question;
                ! this answers the team one, and it is deliberately here rather than inside the
                ! candidate because `hi - lo` is only safe to compute once the candidate has bounded
                ! it -- a key holding values near both ends of int64 overflows the subtraction, which
                ! is why the candidate itself compares in the rearranged form.
                !
                ! **Measured on machine B at n = 5e6, both compilers, counting against radix**
                ! (ns/element, end-to-end). The two compilers do NOT agree about the middle of this
                ! space, and the disagreement inverts:
                !
                !   range/n    ifx 1/4/16/64 thr                gfortran 1/4/16/64 thr
                !   2e-6       5.00 5.42 4.31 5.67 (counting)   10.80 6.60 5.34 5.15
                !              10.99 2.76 0.88 0.41 (radix)     19.51 11.14 6.48 4.80
                !   0.80       19.93 19.00 18.99 18.67          30.25 26.12 24.78 24.75
                !              14.11 4.63 2.24 3.43             30.65 17.36 9.89 10.08
                !
                ! ifx wants radix from 4 threads at EVERY range; gfortran wants counting almost
                ! everywhere, because its radix carries a per-element constant about 5x ifx's (see
                ! feature_sort_report.md section 11.3, which tracks that gap as an open defect). No
                ! single threshold is optimal for both, so **the maintainer's decision is to weight
                ! ifx** -- recorded here because a future reader looking only at gfortran numbers
                ! would otherwise read this rule as simply wrong.
                !
                ! The rule below follows the physical fact that the counting sort IS a serial
                ! algorithm: its admissible range narrows as the team grows, and past a small team it
                ! is not admitted at all. The 0.3 threshold is measured at one thread,
                ! where ifx and gfortran AGREE -- ifx counting/radix is 12.23/12.79 at range/n = 0.2
                ! and 14.64/14.17 at 0.4, so the crossover sits between them, and gfortran's 1-thread
                ! numbers put it no lower.
                !
                ! **Verified at both `--profile release` and fpm's default flags, on both compilers**
                ! (the default gives gfortran no -O at all and ifx its own -O2, so the two are not
                ! affected equally). The decision does not change with the profile; only the
                ! magnitudes do, and counting degrades MORE without -O than the radix does.
                !
                ! What this costs gfortran, stated plainly: in the small-range, many-thread band
                ! (range/n around 1e-4) it gives up to 2.5x, because there its radix is slow enough
                ! that a serial counting sort still wins. Closing 11.3 would remove that cost and let
                ! this rule be optimal for both.
                !
                ! **A TEAM does not merely disqualify the counting sort -- it moves the range at
                ! which the counting sort stops winning.** The threaded radix gets faster with the
                ! team while a serial counting sort does not, so counting's admissible range shrinks
                ! as `nt` grows. Measured on machine B, ifx, n = 5e6, `--profile release`, counting
                ! against the radix at two threads (ns/element):
                !
                !   range/n    counting   radix(nt=2)    winner
                !   2e-6         2.903       4.237       counting, 1.46x
                !   1e-4         3.073       6.985       counting, 2.27x
                !   1e-2         6.472       6.255       radix, 1.03x
                !   1e-1         8.734       7.436       radix, 1.17x
                !   3e-1        11.755       8.866       radix, 1.33x
                !
                ! so `n/100` at two threads, against `0.3n` at one. Note the radix's cost steps with
                ! the BYTE WIDTH of the range rather than growing smoothly (range 10 is one pass,
                ! range 500 is two), which is why its column is not monotone and why a threshold
                ! fitted to a single range would be worthless.
                if (nt <= 1) then
                    take_counting = (hi - lo < n / 10_int64 * 3_int64)
                else
                    take_counting = (hi - lo < n / 100_int64)
                end if
                if (take_counting) then
                    call sort_counting_permutation(keys(1), n, lo, hi, perm)
                    return
                end if
            end if
            end if
        end if
        ! Tried AFTER the counting path, never before it: on a narrow-range integer key the
        ! counting sort is one pass where this is up to eight, and Stage 6 requires the `i64lo`
        ! arm not to regress at all.
        if (sort_radix_candidate(keys, n)) then
            ! `did_radix` is .false. only when the scratch could not be allocated, in which case
            ! nothing has been written to `perm` and the comparison sort below finishes the job --
            ! it needs no scratch at all. See sort_radix_permutation for why this is not an abort.
            if (size(keys) == 1) then
                call sort_radix_permutation(keys, n, perm, did_radix, nt)
            else
                call sort_radix_multi_permutation(keys, n, perm, did_radix, nt)
            end if
            if (did_radix) return
        end if
        call sort_comparison_permutation(keys, n, perm)
    end subroutine sort_build_permutation_impl

    module procedure sort_counting_candidate
        use parquet_settings_base, only : parquet_get_sort_counting_bucket_limit
        integer(int64) :: i     !! row index.
        integer(int64) :: v     !! one key value.
        integer(int64) :: limit !! largest admissible value RANGE, from the setting.
        logical :: has_nulls    !! this key carries a validity array.
        logical :: seen         !! at least one valid row has been read.
        !
        ok = .false.
        lo = 0_int64
        hi = 0_int64
        if (size(keys) /= 1) return
        if (keys(1)%family /= SK_INT) return
        if (n < 2_int64) return
        !
        has_nulls = allocated(keys(1)%valid)
        seen = .false.
        do i = 1_int64, n
            if (has_nulls) then
                if (keys(1)%valid(i) == 0_c_int8_t) cycle
            end if
            v = keys(1)%ints(i)
            if (.not. seen) then
                lo = v
                hi = v
                seen = .true.
            else
                if (v < lo) lo = v
                if (v > hi) hi = v
            end if
        end do
        !
        ! Every row null: there is no range to bound, and the answer is file order because all nulls
        ! tie. `lo == hi == 0` leaves one empty bucket and the placement pass then emits the identity
        ! -- correct, and cheaper than letting the comparator path discover the same thing through
        ! n log n comparisons that all return 0.
        if (.not. seen) then
            ok = .true.
            return
        end if
        !
        ! **This is the ONE place the port is not clause for clause, and the reason is the language.**
        ! C++ computes `(uint64_t)hi - (uint64_t)lo` so that a range spanning both signs cannot
        ! overflow the check itself. Fortran has no portable unsigned integer, and signed overflow is
        ! not defined, so the subtraction cannot simply be repeated here. The comparison below is
        ! rearranged to keep every intermediate in range instead:
        !
        !   * when `lo` is within `limit` of `huge`, so is `hi` (they satisfy `lo <= hi <= huge`), so
        !     `hi - lo < limit` holds automatically and no arithmetic is needed at all;
        !   * otherwise `lo + limit` cannot overflow, and `hi < lo + limit` is exactly the same test.
        !
        ! Do not "simplify" this back into `hi - lo < limit`: that is correct for every fixture a
        ! test will ever build and wrong for a key holding values near both ends of int64.
        limit = parquet_get_sort_counting_bucket_limit()
        if (lo > huge(0_int64) - limit) then
            ok = .true.
        else
            ok = (hi < lo + limit)
        end if
    end procedure sort_counting_candidate

    !> Whether an integer key's whole value range spans less than 2^32, without ever overflowing.
    !!
    !! Placed beside `sort_counting_candidate` because it is the same trap in a different disguise,
    !! and that one documents it at length: `hi - lo` is WRONG for a range spanning both signs, since
    !! the subtraction itself overflows and signed overflow is not defined. Two branches, each
    !! keeping every intermediate in range:
    !!
    !! * both ends the same sign -- `hi - lo` cannot overflow, so ask directly;
    !! * `lo < 0 <= hi` -- then `lo + TWO32` cannot overflow either, because `lo` is negative and
    !!   `TWO32` is far below `huge`, so ask the rearranged question instead.
    !!
    !! **What this guard actually protects, stated precisely, because the obvious answer is wrong and
    !! mutation testing is what established it.** A wrong answer here does NOT reorder anything.
    !! `v - vmin` under wrapping arithmetic is exactly unsigned subtraction mod 2^64, and every int64
    !! range fits in 2^64, so the biased image is order-preserving as an unsigned pattern for ANY
    !! minimum -- replacing this function with the naive `hi - lo < TWO32`, or with a constant
    !! `.true.`, leaves every permutation in the suite bit-identical and every pass count unchanged.
    !! Both were tried. What the guard buys is the two things the mutations do not touch:
    !!
    !! * **defined arithmetic.** Signed overflow is undefined in Fortran, so an image whose
    !!   subtraction leaves int64 is relying on the compiler happening to wrap. This library does not
    !!   rely on that anywhere, and must not start here;
    !! * **the four skipped passes**, which is the entire point of the bias: only a span under 2^32
    !!   leaves the top four bytes constant.
    !!
    !! So this is REVIEWED rather than tested, and deliberately: no fixture can distinguish it,
    !! because the property it defends is not observable in an answer. Do not "simplify" it into one
    !! subtraction on the strength of a green suite or a surviving mutation -- both are expected.
    function sort_span_under_2p32(lo, hi) result(narrow)
        integer(int64), intent(in) :: lo !! smallest value over the key's VALID rows.
        integer(int64), intent(in) :: hi !! largest value over the key's valid rows.
        logical :: narrow                !! .true. when `hi - lo < 2^32`.
        !
        integer(int64), parameter :: TWO32 = 4294967296_int64
        !
        if (lo >= 0_int64 .or. hi < 0_int64) then
            narrow = (hi - lo < TWO32)
        else
            narrow = (hi < lo + TWO32)
        end if
    end function sort_span_under_2p32

    module procedure sort_counting_permutation
        integer(int64), allocatable :: counts(:)  !! rows per bucket.
        integer(int64), allocatable :: offsets(:) !! next output position per bucket, 1-based.
        integer(int64) :: nbuckets !! distinct values the range spans.
        integer(int64) :: i, b     !! row index, bucket index.
        integer(int64) :: nn       !! null rows.
        integer(int64) :: running  !! offset accumulator.
        integer(int64) :: value_base !! positions before the value block, 0-based.
        integer(int64) :: null_pos   !! next output position for a null, 1-based.
        logical :: has_nulls         !! this key carries a validity array.
        !
        ! Safe: `sort_counting_candidate` has already bounded `hi - lo` below the bucket limit, so
        ! both this and every `ints(i) - lo` below stay well inside int64 whatever the values are.
        nbuckets = hi - lo + 1_int64
        allocate(counts(nbuckets))
        counts = 0_int64
        has_nulls = allocated(key%valid)
        !
        nn = 0_int64
        do i = 1_int64, n
            if (has_nulls) then
                if (key%valid(i) == 0_c_int8_t) then
                    nn = nn + 1_int64
                    cycle
                end if
            end if
            b = key%ints(i) - lo + 1_int64
            counts(b) = counts(b) + 1_int64
        end do
        !
        ! Where each block starts. Deliberately free of `descending` -- see this section's header.
        value_base = 0_int64
        if (has_nulls .and. key%nulls_first) value_base = nn
        null_pos = n - nn + 1_int64
        if (has_nulls .and. key%nulls_first) null_pos = 1_int64
        !
        ! Each bucket's first output position: bottom-up ascending, top-down descending.
        allocate(offsets(nbuckets))
        running = value_base + 1_int64
        if (key%descending) then
            do b = nbuckets, 1_int64, -1_int64
                offsets(b) = running
                running = running + counts(b)
            end do
        else
            do b = 1_int64, nbuckets
                offsets(b) = running
                running = running + counts(b)
            end do
        end if
        !
        do i = 1_int64, n
            if (has_nulls) then
                if (key%valid(i) == 0_c_int8_t) then
                    perm(null_pos) = i
                    null_pos = null_pos + 1_int64
                    cycle
                end if
            end if
            b = key%ints(i) - lo + 1_int64
            perm(offsets(b)) = i
            offsets(b) = offsets(b) + 1_int64
        end do
    end procedure sort_counting_permutation

    ! ---- The single-key LSD radix path ----------------------------------------------------------
    !
    ! A stable least-significant-digit radix sort over ONE key, in place of the introsort. It has no
    ! C++ twin -- the C++ engine goes straight from the counting path to `std::sort` -- so unlike the
    ! counting path above there is no clause-for-clause port to preserve, and what follows is the
    ! whole justification.
    !
    ! **Why it exists.** The comparison path spends ~98% of an argsort inside the introsort, and the
    ! introsort is INSTRUCTION-bound rather than memory-bound: measured on machine B at n = 10^6,
    ! 2 x 10^9 instructions per sort at IPC 2.55, i.e. roughly 67 instructions per comparison, where
    ! the essential work is two loads and a compare. `sort_compare_key` is not inlined into
    ! `sort_partition` on any compiler this project has checked, so every one of ~28 x 10^6
    ! comparisons pays a call plus the tier/family/descending chain. Removing comparisons entirely is
    ! worth far more than making them cheaper: 9-17x per single-key arm on machine B.
    !
    ! **What makes it produce the SAME permutation, which is the only thing that matters.** This is a
    ! third expression of the ordering (feature_risks.md Risk-34), so each rule is stated against the
    ! comparator clause it reproduces:
    !
    !   * The three TIERS are split first, in row order. Two rows in the same non-value tier compare
    !     EQUAL under `sort_compare_key`, so the index tiebreaker leaves them in file order -- which
    !     is what a row-order walk writes directly. `value_base`/`nan_pos`/`null_pos` are free of
    !     `descending`, exactly as `sort_counting_permutation`'s are and for the same reason.
    !   * Each value is mapped to the 64-bit pattern whose UNSIGNED order is that value's own order,
    !     so a bytewise radix reproduces `<` on the original type. `-0.0` is forced onto `+0.0`'s
    !     image, because the two compare EQUAL under `<` and a radix that separated them would order
    !     a pair the comparator does not.
    !   * LSD radix is STABLE, so equal keys emerge in increasing row index -- the answer
    !     `sort_row_less`'s index tiebreaker gives.
    !   * `descending` COMPLEMENTS the mapped key rather than reversing the output block. Reversing
    !     would put ties backwards; complementing reverses the value order and leaves stability
    !     intact, which is the same "value tier only" rule `sort_compare_key` applies.
    !
    ! **A string key only gets its first `SORT_RADIX_PREFIX` bytes in**, so the radix leaves runs that
    ! share a prefix unordered and `sort_radix_refine_strings` finishes them with the ordinary
    ! comparator. See that procedure for why an unsigned compare of the padded prefix agrees with
    ! `compare_bytes` wherever two images differ.
    !
    ! **Cost.** Up to four n-element int64 buffers, i.e. ~32 bytes per row, against nothing at all for
    ! the introsort. That is the one thing this path is worse at, and it is why a caller sorting at
    ! the edge of memory needs a way to decline it -- see `feature_sort_radix.md`.

    !> Whether the single-key LSD radix path applies to this key list.
    !!
    !! Every family qualifies -- integer, real and string all reduce to a 64-bit unsigned image --
    !! so the only questions are whether there is exactly one key and whether `n` clears the
    !! histogram's fixed cost. A multi-key sort is deliberately excluded: LSD does compose across
    !! keys, but that is a larger change and is not implemented here.
    function sort_radix_candidate(keys, n) result(ok)
        use parquet_settings_base, only : parquet_get_sort_radix_path
        type(sort_key_buf), intent(in) :: keys(:) !! the keys, in precedence order.
        integer(int64), intent(in) :: n           !! rows to order.
        logical :: ok                             !! .true. when the radix path applies.
        !
        integer(int64) :: floor_rows !! the floor in force, built-in or forced by the debug hook.
        !
        integer :: k       !! key index, when checking a multi-key list.
        integer(int64) :: i !! row index, when measuring a string key's longest value.
        !
        ok = .false.
        if (.not. parquet_get_sort_radix_path()) return
        if (size(keys) < 1) return
        floor_rows = SORT_RADIX_MIN_ROWS
        if (dbg_sort_radix_min_rows >= 0_int64) floor_rows = dbg_sort_radix_min_rows
        if (n < floor_rows) return
        ! A STRING key in a MULTI-key sort is ordered by `sort_radix_string_key_pass`, an MSD radix
        ! that consumes bytes until the rows separate. Its tail is a stable insertion sort, which is
        ! O(m^2) and therefore only sound while m stays small -- and m stays small exactly while the
        ! recursion ends by running out of BYTES rather than by hitting `SORT_RADIX_MAX_BYTE`. So
        ! this refuses a string key whose longest value could reach that cap, which is the one
        ! precondition the tail's cost argument rests on. The single-key path has no such limit: its
        ! tail is the ordinary introsort, which is O(m log m) whatever m is.
        if (size(keys) > 1) then
            do k = 1, size(keys)
                if (keys(k)%family /= SK_STR) cycle
                if (.not. allocated(keys(k)%offsets)) return
                do i = 1_int64, n
                    if (keys(k)%offsets(i + 1_int64) - keys(k)%offsets(i) > SORT_RADIX_MAX_BYTE) return
                end do
            end do
        end if
        ok = .true.
    end function sort_radix_candidate

    !> The 64-bit images of a LIST of rows, each one's UNSIGNED order being that row's order under
    !! the key.
    !!
    !! Kept in one place so that the three families cannot drift apart -- that is
    !! `feature_risks.md` Risk-34's rule, and it is why there is no scalar twin of this beside it.
    !! It is also why the caller's loop reads as a walk over rows rather than as three interleaved
    !! encodings.
    !!
    !! **Bulk rather than per-row, and that is a measured choice.** As a per-element function this
    !! was reached by a real call for every row of every key -- ifx emitted a `PLT32` relocation for
    !! it rather than inlining it -- each call carrying a family `select case` and a `descending`
    !! branch, for work that is one XOR in the integer case. Hoisting the dispatch above the loop
    !! removed both and was worth 2.9% to 9.0% on every radix arm. Making the compiler inline it
    !! instead is not an available fix: this engine already has a recorded case (`sort_compare_key`
    !! into `sort_partition`) where ifx declined even after the interposable symbol was removed.
    !!
    !! **`descending` is an XOR mask, not a branch.** `not(t)` is `ieor(t, -1)`, so the complement
    !! costs nothing when the mask is 0 -- and for an integer key it folds one level further, since
    !! `ieor(ieor(v, SORT_SIGN_BIT), dmask)` is `ieor(v, ieor(SORT_SIGN_BIT, dmask))`. That leaves
    !! the whole integer transform as one XOR against a loop-invariant constant. Do not "simplify"
    !! either fold back into a branch.
    !!
    !! **It does not ask which tier a row is in, and callers must not assume it does.** The
    !! single-key caller passes only value-tier rows; the multi-key caller passes every row and
    !! overwrites the non-value ones with 0 afterwards. Imaging a null row is harmless -- its value
    !! slot is in bounds and Arrow simply promises nothing about the contents, the same read
    !! `sort_tier_of` already makes -- and nothing downstream sees the result.
    !! **`bias`, when present, replaces the sign flip with a subtraction**, which is the other
    !! order-preserving image an integer key can have and the one worth having when the range is
    !! narrow -- see `sort_radix_permutation`, which is the only thing that decides to supply it.
    !! It is optional rather than defaulted to 0 because 0 is not a no-op: subtracting nothing
    !! leaves a negative value's pattern above every positive one under UNSIGNED comparison, which
    !! is precisely what the sign flip exists to fix.
    subroutine sort_radix_images_range(key, rows, jlo, jhi, out, bias)
        type(sort_key_buf), intent(in) :: key  !! the bound key.
        integer(int64), intent(in) :: rows(:)  !! the rows to image, 1-based; `jlo..jhi` are read.
        integer(int64), intent(in) :: jlo      !! first entry of `rows` to image.
        integer(int64), intent(in) :: jhi      !! last entry to image; `jhi < jlo` images nothing.
        integer(int64), intent(inout) :: out(:)
        !! receives images at `jlo..jhi`, to be compared as UNSIGNED. `intent(inout)` rather than
        !! `intent(out)` precisely BECAUSE this writes a sub-range: under `intent(out)` a compiler
        !! may take the whole array as undefined on entry, which is false for every chunk but one
        !! when a team splits the build (`sort_radix_images_threaded`).
        integer(int64), intent(in), optional :: bias
        !! subtracted from an INTEGER key's value instead of flipping its sign bit. The caller must
        !! have established that every row imaged satisfies `bias <= v` and `v - bias < 2^32`.
        !
        integer(int64) :: j     !! walk index over `rows`.
        integer(int64) :: u     !! the raw bit pattern being transformed.
        integer(int64) :: t     !! one row's image, while it is being built.
        integer(int64) :: p0    !! first byte of a string row, 1-based into `data`.
        integer(int64) :: ln    !! byte length of a string row.
        integer(int64) :: nb    !! bytes of it that fit the prefix window.
        integer(int64) :: q     !! byte cursor within the window.
        integer(int64) :: dmask !! 0 ascending, -1 descending: the complement, as an XOR.
        integer(int64) :: imask !! the integer branch's whole transform, folded to one constant.
        real(real64) :: x       !! the real value being transformed.
        !
        dmask = 0_int64
        if (key%descending) dmask = -1_int64
        select case (key%family)
        case (SK_INT)
            if (present(bias)) then
                ! `v - bias` is monotone in `v` and lands in `[0, 2^32)`, so its signed order, its
                ! unsigned order and the value order all coincide -- which is all the radix needs.
                ! Its top four bytes are then zero (or, under `descending`, all ones), constant
                ! across the column either way, so the pass-skip drops four of the eight passes.
                do j = jlo, jhi
                    out(j) = ieor(key%ints(rows(j)) - bias, dmask)
                end do
            else
                ! Flipping the sign bit turns signed order into unsigned order.
                imask = ieor(SORT_SIGN_BIT, dmask)
                do j = jlo, jhi
                    out(j) = ieor(key%ints(rows(j)), imask)
                end do
            end if
        case (SK_REAL)
            do j = jlo, jhi
                x = key%reals(rows(j))
                ! -0.0 and +0.0 compare EQUAL under `<`, so they must share ONE image here or this
                ! path would order a pair `sort_compare_key` calls equal. Everything else is the
                ! standard IEEE total-order transform: complement a negative, flip a positive's
                ! sign bit.
                if (x == 0.0_real64) then
                    u = 0_int64
                else
                    u = transfer(x, 0_int64)
                end if
                if (u < 0_int64) then
                    t = not(u)
                else
                    t = ieor(u, SORT_SIGN_BIT)
                end if
                out(j) = ieor(t, dmask)
            end do
        case default
            do j = jlo, jhi
                ! The leading bytes, big-endian, zero-padded. No sign flip: already unsigned.
                p0 = key%offsets(rows(j)) + 1_int64
                ln = key%offsets(rows(j) + 1_int64) - key%offsets(rows(j))
                t = 0_int64
                if (ln > 0_int64) then
                    nb = min(ln, SORT_RADIX_PREFIX)
                    do q = 0_int64, nb - 1_int64
                        t = ior(ishft(t, 8), int(iand(iachar(key%data(p0 + q)), 255), int64))
                    end do
                    ! Guarded because a zero-length string never reaches here, so `nb >= 1` and the
                    ! shift is at most 56 -- shifting a 64-bit value by 64 is not defined.
                    if (nb < SORT_RADIX_PREFIX) t = ishft(t, int(8_int64 * (SORT_RADIX_PREFIX - nb)))
                end if
                out(j) = ieor(t, dmask)
            end do
        end select
    end subroutine sort_radix_images_range

    !> The image build split across a team of `nt` — Stage 4, `feature_sort_parallel.md` §11 step 1.
    !!
    !! Embarrassingly parallel and the easiest phase in the engine to thread: every row's image
    !! depends on that row alone, chunks are contiguous and disjoint, and no chunk reads another's
    !! output. So there is no reduction, no ordering subtlety and nothing to serialise.
    !!
    !! **The shape here is load-bearing and is the one both compilers accept.** `key` crosses into
    !! the region as a shared, read-only actual argument; nothing is CONSTRUCTED inside it.
    !! `sort_key_buf` has five allocatable components (`ints`, `reals`, `offsets`, `data`, `valid`),
    !! which puts it squarely in the class where ifx segfaults on a `block`-local declaration inside a
    !! parallel region, while gfortran is the one that mishandles a `private()` copy — the two forbid
    !! opposite shapes, so a type in the intersection can use neither. Declaring no instance at all
    !! sidesteps both. See `feature_sort_parallel.md` §11 step 3 and `feature_risks.md` Risk-45.
    !!
    !! Falls back to the identical serial code when there is no team or no OpenMP, so the answer
    !! cannot depend on either.
    subroutine sort_radix_images_threaded(key, rows, m, out, nt, bias)
        type(sort_key_buf), intent(in) :: key   !! the bound key.
        integer(int64), intent(in) :: rows(:)   !! the rows to image, 1-based; `1..m` are read.
        integer(int64), intent(in) :: m         !! how many of `rows` to image.
        integer(int64), intent(inout) :: out(:) !! receives `m` images, to be compared as UNSIGNED.
        integer, intent(in) :: nt               !! team size; 1 runs the serial path.
        integer(int64), intent(in), optional :: bias !! see `sort_radix_images_range`.
#ifdef _OPENMP
        integer :: tid          !! this thread's index in the team, 0-based.
        integer(int64) :: c_lo, c_hi !! this thread's chunk of `rows`, inclusive.
        !
        if (nt > 1) then
            !$omp parallel num_threads(nt) default(shared) private(tid, c_lo, c_hi)
            tid = omp_get_thread_num()
            call sort_chunk_bounds(m, nt, tid, c_lo, c_hi)
            if (c_hi >= c_lo) call sort_radix_images_range(key, rows, c_lo, c_hi, out, bias)
            !$omp end parallel
            return
        end if
#endif
        call sort_radix_images_range(key, rows, 1_int64, m, out, bias)
    end subroutine sort_radix_images_threaded

    !> Builds all eight per-digit histograms of `sk(1:m)` in ONE read, threaded when it can be.
    !!
    !! **Extracted so the single-key and multi-key paths share one implementation.** Both need the
    !! identical thing and the multi-key chain was doing it serially, which is a whole-column pass per
    !! key that no team ever touched -- part of why its 1 -> 64 scaling stopped at 1.33x.
    !!
    !! **A per-thread histogram plus one reduction, never an atomic update.** The reduction touches
    !! 2048 counters per thread and runs once; an atomic would run eight times per ROW, on the hottest
    !! loop in the phase. The per-thread array is allocated BEFORE the region, for the reason
    !! `sort_radix_images_threaded` spells out -- nothing may be constructed inside a parallel region
    !! here.
    !!
    !! **A failed allocation DECLINES to the serial loop** rather than aborting, exactly as the
    !! scratch allocations do: the serial path is a complete answer, so a failure costs speed and
    !! nothing else. That also means `nt > 1` never *guarantees* the threaded path ran, which is why
    !! the pass counter and the threads-used counter measure different things and both exist.
    subroutine sort_radix_hist_threaded(sk, m, hist, nt)
        integer(int64), intent(in) :: sk(:)             !! the images to count; `1..m` are read.
        integer(int64), intent(in) :: m                 !! how many images.
        integer(int64), intent(out) :: hist(0:255, 0:7) !! receives one histogram per byte position.
        integer, intent(in) :: nt                       !! team size; 1 runs the serial path.
        integer(int64) :: j, b, u
        integer :: p
#ifdef _OPENMP
        integer(int64), allocatable :: thist(:,:,:) !! per-thread histograms, reduced below.
        integer :: tid, tt, ios
        integer(int64) :: c_lo, c_hi
#endif
        !
        hist = 0_int64
#ifdef _OPENMP
        if (nt > 1) then
            allocate(thist(0:255, 0:7, 0:nt - 1), stat=ios)
            if (ios == 0) then
                thist = 0_int64
                !$omp parallel num_threads(nt) default(shared) private(tid, c_lo, c_hi, j, p, b, u)
                tid = omp_get_thread_num()
                call sort_chunk_bounds(m, nt, tid, c_lo, c_hi)
                do j = c_lo, c_hi
                    u = sk(j)
                    do p = 0, 7
                        b = iand(u, 255_int64)
                        thist(b, p, tid) = thist(b, p, tid) + 1_int64
                        u = ishft(u, -8)
                    end do
                end do
                !$omp end parallel
                do tt = 0, nt - 1
                    do p = 0, 7
                        do b = 0_int64, 255_int64
                            hist(b, p) = hist(b, p) + thist(b, p, tt)
                        end do
                    end do
                end do
                deallocate(thist)
                return
            end if
        end if
#endif
        do j = 1_int64, m
            u = sk(j)
            do p = 0, 7
                b = iand(u, 255_int64)
                hist(b, p) = hist(b, p) + 1_int64
                u = ishft(u, -8)
            end do
        end do
    end subroutine sort_radix_hist_threaded

    !> Copies `src(1:m)` into `dst(1:m)`, threaded when a team is available.
    !!
    !! The multi-key chain copies the permutation home once per key, and again inside its tier pass
    !! and its string pass -- whole-column passes that were serial and are charged to every multi-key
    !! sort. Trivial in itself; it is here rather than inline so the `#ifdef _OPENMP` fork and the
    !! serial fallback are written once instead of at each of the three sites.
    subroutine sort_copy_threaded(src, dst, m, nt)
        integer(int64), intent(in) :: src(:)    !! source; `1..m` are read.
        integer(int64), intent(inout) :: dst(:) !! destination; `1..m` are written.
        integer(int64), intent(in) :: m         !! elements to copy.
        integer, intent(in) :: nt               !! team size; 1 runs the serial loop.
        integer(int64) :: j
        !
#ifdef _OPENMP
        if (nt > 1) then
            !$omp parallel do num_threads(nt) default(shared) private(j) schedule(static)
            do j = 1_int64, m
                dst(j) = src(j)
            end do
            !$omp end parallel do
            return
        end if
#endif
        do j = 1_int64, m
            dst(j) = src(j)
        end do
    end subroutine sort_copy_threaded

    !> Splits `1..m` into `nt` contiguous, near-equal chunks and returns chunk `tid`'s bounds.
    !!
    !! **Contiguous, never strided.** Every phase this splits is memory-bound and walks its input
    !! sequentially, so a strided split would give each thread the same number of elements and a
    !! fraction of the bandwidth.
    !!
    !! **The first `rem` chunks take one extra element** rather than the remainder being left to the
    !! last chunk. At the thread counts this work targets that is the difference between a balanced
    !! split and one thread carrying up to `nt - 1` extra elements while the rest wait for it.
    !!
    !! Returns `jhi < jlo` — an EMPTY chunk — whenever `nt > m`. That is a normal outcome, not an
    !! error, and every caller must test for it rather than assume each thread gets work.
    pure subroutine sort_chunk_bounds(m, nt, tid, jlo, jhi)
        integer(int64), intent(in) :: m !! elements to split, indexed `1..m`.
        integer, intent(in) :: nt       !! number of chunks; must be at least 1.
        integer, intent(in) :: tid      !! which chunk, 0-based.
        integer(int64), intent(out) :: jlo !! first element of this chunk.
        integer(int64), intent(out) :: jhi !! last element; `jhi < jlo` when this chunk is empty.
        integer(int64) :: base, rem, t64
        !
        t64 = int(tid, int64)
        base = m / int(nt, int64)
        rem = m - base * int(nt, int64)
        jlo = t64 * base + min(t64, rem) + 1_int64
        jhi = jlo + base - 1_int64
        if (t64 < rem) jhi = jhi + 1_int64
    end subroutine sort_chunk_bounds

    !> One stable scatter of `lo..hi` on digit `p`, from one buffer pair into another.
    !!
    !! `off` enters holding each bucket's next free slot and leaves advanced past what was written,
    !! which is what lets a caller drive several ranges through the same cursor array.
    subroutine sort_radix_scatter(sk, sr, dk, dr, lo, hi, p, off)
        integer(int64), intent(in) :: sk(:), sr(:)    !! source images and rows.
        integer(int64), intent(inout) :: dk(:), dr(:) !! destination images and rows.
        integer(int64), intent(in) :: lo, hi          !! source range, inclusive.
        integer, intent(in) :: p                      !! byte position, 0 = least significant.
        integer(int64), intent(inout) :: off(0:255)   !! per-bucket output cursors, 1-based.
        integer(int64) :: j, b
        !
        do j = lo, hi
            b = iand(ishft(sk(j), -8 * p), 255_int64)
            dk(off(b)) = sk(j)
            dr(off(b)) = sr(j)
            off(b) = off(b) + 1_int64
        end do
    end subroutine sort_radix_scatter

    !> The same scatter, but the rows land straight in `perm` and the images are not carried.
    !!
    !! Saves the destination write and the final copy on whichever pass is known to be the last one
    !! that reorders anything — the same trick the serial loop plays with `last_p`, applied per
    !! bucket. Leaves the source's row array STALE, so nothing may read it afterwards.
    subroutine sort_radix_emit(sk, sr, lo, hi, p, off, perm, base)
        integer(int64), intent(in) :: sk(:), sr(:)   !! source images and rows.
        integer(int64), intent(in) :: lo, hi         !! source range, inclusive.
        integer, intent(in) :: p                     !! byte position, 0 = least significant.
        integer(int64), intent(inout) :: off(0:255)  !! per-bucket output cursors, 1-based.
        integer(int64), intent(inout) :: perm(:)     !! receives the row indices.
        integer(int64), intent(in) :: base           !! added to every cursor before writing.
        integer(int64) :: j, b
        !
        do j = lo, hi
            b = iand(ishft(sk(j), -8 * p), 255_int64)
            perm(base + off(b)) = sr(j)
            off(b) = off(b) + 1_int64
        end do
    end subroutine sort_radix_emit

    !> One bucket's serial LSD over digits `0 .. dmax`, ending with its rows written into `perm`.
    !!
    !! **This is the whole of Design B's second phase, and it is deliberately SERIAL.** Buckets are
    !! disjoint and are handed out one per thread, so the parallelism is across buckets; threading
    !! inside one as well would nest two levels for no reason and re-introduce the synchronisation
    !! the decomposition exists to remove.
    !!
    !! Every row in `lo..hi` agrees on the split digit and on every digit above it, so ordering them
    !! by digits `0..dmax` orders them completely. Correct because each pass is stable and the
    !! caller's split was stable, so rows equal on all eight digits keep the order they arrived in —
    !! which, at the top of the chain, is file order.
    !!
    !! Ping-pongs between the two buffer pairs **by parity over the SUB-RANGE**, never by
    !! `move_alloc`: the buffers are shared with every other bucket running concurrently, so
    !! exchanging them is not available here even though the serial path does exactly that.
    subroutine sort_radix_bucket(ka, ra, kb, rb, lo, hi, dmax, perm, base)
        integer(int64), intent(inout) :: ka(:), ra(:) !! the split's output; this bucket's input.
        integer(int64), intent(inout) :: kb(:), rb(:) !! scratch; this bucket's range only.
        integer(int64), intent(in) :: lo, hi          !! this bucket's range, inclusive.
        integer, intent(in) :: dmax                   !! highest digit to order by; 0..dmax.
        integer(int64), intent(inout) :: perm(:)      !! receives this bucket's row indices.
        integer(int64), intent(in) :: base            !! `perm` offset for index `lo`.
        !
        integer(int64) :: bh(0:255, 0:7) !! this bucket's own histograms, one per digit.
        integer(int64) :: off(0:255)     !! per-bucket output cursors within this range.
        integer(int64) :: m, j, b, u, t
        integer :: p, lastp
        logical :: src_is_a !! .true. while `ka`/`ra` hold this bucket's current state.
        !
        m = hi - lo + 1_int64
        if (m <= 0_int64) return
        if (m == 1_int64) then
            perm(base + lo) = ra(lo)
            return
        end if
        ! All the histograms this bucket needs in ONE read of its images, mirroring the whole-column
        ! build above. `dmax` is at most 6 here (the split digit is at least 1 and is excluded), so
        ! this is never the full 2048 counters.
        bh(:, 0:dmax) = 0_int64
        do j = lo, hi
            u = ka(j)
            do p = 0, dmax
                b = iand(u, 255_int64)
                bh(b, p) = bh(b, p) + 1_int64
                u = ishft(u, -8)
            end do
        end do
        ! The last pass that will actually run, so it can write into `perm` and skip its own scatter.
        lastp = -1
        do p = dmax, 0, -1
            b = iand(ishft(ka(lo), -8 * p), 255_int64)
            if (bh(b, p) /= m) then
                lastp = p
                exit
            end if
        end do
        if (lastp < 0) then
            ! Every digit below the split is constant across this bucket: it is already ordered.
            do j = lo, hi
                perm(base + j) = ra(j)
            end do
            return
        end if
        src_is_a = .true.
        do p = 0, dmax
            if (src_is_a) then
                b = iand(ishft(ka(lo), -8 * p), 255_int64)
            else
                b = iand(ishft(kb(lo), -8 * p), 255_int64)
            end if
            if (bh(b, p) == m) cycle
            t = lo
            do b = 0_int64, 255_int64
                off(b) = t
                t = t + bh(b, p)
            end do
            if (p == lastp) then
                if (src_is_a) then
                    call sort_radix_emit(ka, ra, lo, hi, p, off, perm, base)
                else
                    call sort_radix_emit(kb, rb, lo, hi, p, off, perm, base)
                end if
                return
            end if
            if (src_is_a) then
                call sort_radix_scatter(ka, ra, kb, rb, lo, hi, p, off)
            else
                call sort_radix_scatter(kb, rb, ka, ra, lo, hi, p, off)
            end if
            src_is_a = .not. src_is_a
        end do
    end subroutine sort_radix_bucket

#ifdef _OPENMP
    !> The tier split, threaded — `feature_sort_parallel.md` §11 step 1's last whole-column phase.
    !!
    !! Classifies every row as value / NaN / null, compacts the value rows into `ra(1:nv)` **in row
    !! order**, and reduces an integer key's value range along the way. The serial original does all
    !! of that in one pass; this needs **two**, because a stable compaction cannot know where a
    !! thread's output begins until every earlier thread has been counted.
    !!
    !! **Reading the column twice is the price of stability and it is worth paying.** The alternative
    !! — recording each row's class in a byte array during pass 1 — trades a recomputed test for an
    !! extra `n` bytes written and read, and the test is a null-bit load or a NaN compare, i.e.
    !! cheaper than the memory traffic it would save. For the common null-free integer column neither
    !! branch is taken at all and pass 2 is a bare `ra(k) = i`.
    !!
    !! **The value-range scan stays folded into pass 1**, for the reason the serial code documents:
    !! `key%ints(i)` is read SEQUENTIALLY here, where a separate gathered scan over `ra` would not be.
    !!
    !! Nulls never reach the range scan — the same `cycle` ordering as the serial loop — so a null
    !! row's undefined key slot cannot widen the range and cost the narrow-integer optimisation.
    subroutine sort_tier_split_par(key, n, has_nulls, is_real, is_int, nt, ra, nv, nnan, nnull, vmin, vmax)
        type(sort_key_buf), intent(in) :: key   !! the bound key.
        integer(int64), intent(in) :: n         !! rows in the column.
        logical, intent(in) :: has_nulls        !! whether the key carries a validity mask.
        logical, intent(in) :: is_real          !! whether a NaN tier is possible.
        logical, intent(in) :: is_int           !! whether the value range is wanted.
        integer, intent(in) :: nt               !! team size.
        integer(int64), intent(inout) :: ra(:)  !! receives the value-tier row indices, in row order.
        integer(int64), intent(out) :: nv       !! value-tier rows.
        integer(int64), intent(out) :: nnan     !! NaN-tier rows.
        integer(int64), intent(out) :: nnull    !! null-tier rows.
        integer(int64), intent(out) :: vmin, vmax !! the integer key's range over its VALID rows.
        !
        integer(int64), allocatable :: cv(:), cn(:), cu(:) !! per-thread value / NaN / null counts.
        integer(int64), allocatable :: lo_t(:), hi_t(:)    !! per-thread value range.
        integer(int64) :: i, k, t, held, c_lo, c_hi
        integer :: tid, tt, ios
        real(real64) :: x
        !
        nv = 0_int64
        nnan = 0_int64
        nnull = 0_int64
        vmin = huge(0_int64)
        vmax = -huge(0_int64) - 1_int64
        allocate(cv(0:nt - 1), cn(0:nt - 1), cu(0:nt - 1), lo_t(0:nt - 1), hi_t(0:nt - 1), stat=ios)
        if (ios /= 0 .or. dbg_sort_radix_fail_alloc == 3) then
            nv = -1_int64   ! the caller's signal to take the serial path
            return
        end if
        !
        !$omp parallel num_threads(nt) default(shared) private(tid, c_lo, c_hi, i, x, t)
        tid = omp_get_thread_num()
        call sort_chunk_bounds(n, nt, tid, c_lo, c_hi)
        cv(tid) = 0_int64
        cn(tid) = 0_int64
        cu(tid) = 0_int64
        lo_t(tid) = huge(0_int64)
        hi_t(tid) = -huge(0_int64) - 1_int64
        do i = c_lo, c_hi
            if (has_nulls) then
                if (key%valid(i) == 0_c_int8_t) then
                    cu(tid) = cu(tid) + 1_int64
                    cycle
                end if
            end if
            if (is_real) then
                ! `x /= x` rather than `ieee_is_nan` -- see this submodule's header.
                x = key%reals(i)
                if (x /= x) then
                    cn(tid) = cn(tid) + 1_int64
                    cycle
                end if
            end if
            cv(tid) = cv(tid) + 1_int64
            if (is_int) then
                t = key%ints(i)
                if (t < lo_t(tid)) lo_t(tid) = t
                if (t > hi_t(tid)) hi_t(tid) = t
            end if
        end do
        !$omp end parallel
        !
        ! Prefix over threads, and the totals. `cv` becomes each thread's first output slot, which is
        ! what makes the compaction stable: thread order is chunk order is row order.
        t = 1_int64
        do tt = 0, nt - 1
            held = cv(tt)
            cv(tt) = t
            t = t + held
            nnan = nnan + cn(tt)
            nnull = nnull + cu(tt)
            if (lo_t(tt) < vmin) vmin = lo_t(tt)
            if (hi_t(tt) > vmax) vmax = hi_t(tt)
        end do
        nv = t - 1_int64
        !
        !$omp parallel num_threads(nt) default(shared) private(tid, c_lo, c_hi, i, k, x)
        tid = omp_get_thread_num()
        call sort_chunk_bounds(n, nt, tid, c_lo, c_hi)
        k = cv(tid)
        do i = c_lo, c_hi
            if (has_nulls) then
                if (key%valid(i) == 0_c_int8_t) cycle
            end if
            if (is_real) then
                x = key%reals(i)
                if (x /= x) cycle
            end if
            ra(k) = i
            k = k + 1_int64
        end do
        !$omp end parallel
        deallocate(cv, cn, cu, lo_t, hi_t)
    end subroutine sort_tier_split_par

    !> Per-thread counts of digit `p` over `1..nv`, one column per thread.
    !!
    !! **Must be rebuilt after ANY reordering, and that is the trap this whole area carries.** Reusing
    !! one set of per-thread counts across two passes gives a silently wrong permutation from two
    !! threads upward — `feature_sort_parallel.md` §6.1 — because a row's *chunk* changes when rows
    !! move even though the column-wide multiset does not. The whole-column `hist` is the opposite
    !! case and stays valid across passes, since a permutation cannot change how many rows carry a
    !! given digit value.
    subroutine sort_radix_count_par(sk, nv, p, cnt, nt)
        integer(int64), intent(in) :: sk(:)         !! the images to count.
        integer(int64), intent(in) :: nv            !! rows, occupying `1..nv`.
        integer, intent(in) :: p                    !! byte position, 0 = least significant.
        integer, intent(in) :: nt                   !! team size.
        integer(int64), intent(out) :: cnt(0:255, 0:nt - 1) !! receives each thread's counts.
        integer :: tid
        integer(int64) :: j, b, c_lo, c_hi
        !
        ! Load-bearing, and NO TEST CAN CATCH ITS REMOVAL: `intent(out)` on an integer array does not
        ! initialise it, and in practice the allocator hands back zeroed pages — so deleting this line
        ! survives the whole suite while leaving the engine one unlucky allocation away from a
        ! corrupted permutation. Confirmed by mutation. Same class as the uninitialised-value trap
        ! CLAUDE.md documents under "An intermittent test failure has THREE causes".
        cnt = 0_int64
        !$omp parallel num_threads(nt) default(shared) private(tid, c_lo, c_hi, j, b)
        tid = omp_get_thread_num()
        call sort_chunk_bounds(nv, nt, tid, c_lo, c_hi)
        do j = c_lo, c_hi
            b = iand(ishft(sk(j), -8 * p), 255_int64)
            cnt(b, tid) = cnt(b, tid) + 1_int64
        end do
        !$omp end parallel
    end subroutine sort_radix_count_par

    !> Turns per-thread counts into per-thread output cursors — BUCKET MAJOR, THREAD MINOR.
    !!
    !! **That nesting is the stability of every parallel pass in this file.** Bucket-major with
    !! thread-minor lays each bucket's rows down in thread order, and each thread contributes its own
    !! chunk in its own order, so the result is exactly the order the rows were in beforehand. Swap
    !! the two loops and the sort still produces a correctly *ordered* answer that is not the unique
    !! correct permutation — which the A/B tests catch only because the fixture carries ties.
    !!
    !! Optionally reports each bucket's resulting range, which is what a bucket decomposition needs
    !! and an LSD pass does not.
    subroutine sort_radix_cursors(cnt, nt, blo, bhi, first)
        integer, intent(in) :: nt                   !! team size.
        integer(int64), intent(inout) :: cnt(0:255, 0:nt - 1) !! counts in, cursors out.
        integer(int64), intent(out), optional :: blo(0:255) !! each bucket's first slot.
        integer(int64), intent(out), optional :: bhi(0:255) !! each bucket's last slot.
        integer(int64), intent(in), optional :: first
        !! first output slot; defaults to 1. A SUB-RANGE refinement passes its own `lo`, so the
        !! cursors address the parent bucket's slice of the shared buffers rather than the column.
        integer :: tt
        integer(int64) :: b, t, held
        !
        t = 1_int64
        if (present(first)) t = first
        do b = 0_int64, 255_int64
            if (present(blo)) blo(b) = t
            do tt = 0, nt - 1
                held = cnt(b, tt)
                cnt(b, tt) = t
                t = t + held
            end do
            if (present(bhi)) bhi(b) = t - 1_int64
        end do
    end subroutine sort_radix_cursors

    !> One threaded stable scatter of `1..nv` on digit `p`, into the partner buffers.
    !!
    !! Each thread advances only its own cursor column, so the shared `cnt` carries no race and needs
    !! no atomic — the prefix has already reserved every thread's slots.
    subroutine sort_radix_scatter_par(sk, sr, dk, dr, nv, p, cnt, nt)
        integer(int64), intent(in) :: sk(:), sr(:)    !! source images and rows.
        integer(int64), intent(inout) :: dk(:), dr(:) !! destination images and rows.
        integer(int64), intent(in) :: nv              !! rows, occupying `1..nv`.
        integer, intent(in) :: p                      !! byte position.
        integer, intent(in) :: nt                     !! team size.
        integer(int64), intent(inout) :: cnt(0:255, 0:nt - 1) !! per-thread cursors, advanced here.
        integer :: tid
        integer(int64) :: j, b, c_lo, c_hi
        !
        !$omp parallel num_threads(nt) default(shared) private(tid, c_lo, c_hi, j, b)
        tid = omp_get_thread_num()
        call sort_chunk_bounds(nv, nt, tid, c_lo, c_hi)
        do j = c_lo, c_hi
            b = iand(ishft(sk(j), -8 * p), 255_int64)
            dk(cnt(b, tid)) = sk(j)
            dr(cnt(b, tid)) = sr(j)
            cnt(b, tid) = cnt(b, tid) + 1_int64
        end do
        !$omp end parallel
    end subroutine sort_radix_scatter_par

    !> The same threaded scatter, but the rows land straight in `perm` and images are not carried.
    !!
    !! Only ever called for the pass known to be the last that reorders anything, which is what makes
    !! discarding the images safe. Leaves the source's row array STALE.
    subroutine sort_radix_emit_par(sk, sr, nv, p, cnt, nt, perm, base)
        integer(int64), intent(in) :: sk(:), sr(:)    !! source images and rows.
        integer(int64), intent(in) :: nv              !! rows, occupying `1..nv`.
        integer, intent(in) :: p                      !! byte position.
        integer, intent(in) :: nt                     !! team size.
        integer(int64), intent(inout) :: cnt(0:255, 0:nt - 1) !! per-thread cursors, advanced here.
        integer(int64), intent(inout) :: perm(:)      !! receives the row indices.
        integer(int64), intent(in) :: base            !! added to every cursor before writing.
        integer :: tid
        integer(int64) :: j, b, c_lo, c_hi
        !
        !$omp parallel num_threads(nt) default(shared) private(tid, c_lo, c_hi, j, b)
        tid = omp_get_thread_num()
        call sort_chunk_bounds(nv, nt, tid, c_lo, c_hi)
        do j = c_lo, c_hi
            b = iand(ishft(sk(j), -8 * p), 255_int64)
            perm(base + cnt(b, tid)) = sr(j)
            cnt(b, tid) = cnt(b, tid) + 1_int64
        end do
        !$omp end parallel
    end subroutine sort_radix_emit_par

    !> The threaded count of `sort_radix_count_par`, restricted to one CONTIGUOUS SUB-RANGE.
    !!
    !! Exists for Design B's refinement, which re-splits one oversized bucket with the whole team
    !! rather than leaving it to the single thread that drew it. Chunking is over the sub-range, so
    !! every thread contributes to a bucket that is only a fraction of the column — which is the
    !! entire point, and is why the whole-column helper cannot simply be reused with a mask.
    !!
    !! Carries `sort_radix_count_par`'s `cnt = 0` for the same reason it does: `intent(out)` on an
    !! integer array does not initialise it, the allocator usually hands back zeroed pages, and so
    !! removing the line survives the whole suite while leaving a corrupted permutation one unlucky
    !! allocation away.
    subroutine sort_radix_count_range_par(sk, lo, hi, p, cnt, nt)
        integer(int64), intent(in) :: sk(:)         !! the images to count.
        integer(int64), intent(in) :: lo, hi        !! the sub-range, inclusive.
        integer, intent(in) :: p                    !! byte position, 0 = least significant.
        integer, intent(in) :: nt                   !! team size.
        integer(int64), intent(out) :: cnt(0:255, 0:nt - 1) !! receives each thread's counts.
        integer :: tid
        integer(int64) :: j, b, c_lo, c_hi, m
        !
        m = hi - lo + 1_int64
        cnt = 0_int64
        !$omp parallel num_threads(nt) default(shared) private(tid, c_lo, c_hi, j, b)
        tid = omp_get_thread_num()
        call sort_chunk_bounds(m, nt, tid, c_lo, c_hi)
        do j = lo + c_lo - 1_int64, lo + c_hi - 1_int64
            b = iand(ishft(sk(j), -8 * p), 255_int64)
            cnt(b, tid) = cnt(b, tid) + 1_int64
        end do
        !$omp end parallel
    end subroutine sort_radix_count_range_par

    !> The threaded stable scatter of `sort_radix_scatter_par`, restricted to one sub-range.
    !!
    !! The destination slots come from cursors the caller built with `sort_radix_cursors(..., first
    !! = lo)`, so rows never leave the parent bucket's slice — which is what keeps every other
    !! bucket's range untouched and lets refinement run while unrefined buckets sit in the partner
    !! buffer.
    subroutine sort_radix_scatter_range_par(sk, sr, dk, dr, lo, hi, p, cnt, nt)
        integer(int64), intent(in) :: sk(:), sr(:)    !! source images and rows.
        integer(int64), intent(inout) :: dk(:), dr(:) !! destination images and rows.
        integer(int64), intent(in) :: lo, hi          !! the sub-range, inclusive.
        integer, intent(in) :: p                      !! byte position.
        integer, intent(in) :: nt                     !! team size.
        integer(int64), intent(inout) :: cnt(0:255, 0:nt - 1) !! per-thread cursors, advanced here.
        integer :: tid
        integer(int64) :: j, b, c_lo, c_hi, m
        !
        m = hi - lo + 1_int64
        !$omp parallel num_threads(nt) default(shared) private(tid, c_lo, c_hi, j, b)
        tid = omp_get_thread_num()
        call sort_chunk_bounds(m, nt, tid, c_lo, c_hi)
        do j = lo + c_lo - 1_int64, lo + c_hi - 1_int64
            b = iand(ishft(sk(j), -8 * p), 255_int64)
            dk(cnt(b, tid)) = sk(j)
            dr(cnt(b, tid)) = sr(j)
            cnt(b, tid) = cnt(b, tid) + 1_int64
        end do
        !$omp end parallel
    end subroutine sort_radix_scatter_range_par

    !> Design A — the LSD structure kept, with every digit's pass threaded and synchronised.
    !!
    !! `feature_sort_parallel.md` §11 step 4. One count-prefix-scatter per digit, exactly the shape of
    !! Design B's split but applied to all of them, so there is no bucket decomposition and no
    !! dependence on the split digit's cardinality at all.
    !!
    !! **This is the fallback that machine B's report makes REQUIRED rather than optional.** On keys
    !! whose top varying digit has few distinct values, Design B reaches 1.39–1.85× where this reaches
    !! 2.70–4.74× at 64 threads, on both compilers — and Design B's run-to-run spread there reaches
    !! 96.5% against 1.0–15.0% here. It is slower than B on well-spread keys, which is why it is
    !! second in line and not first.
    !!
    !! Ping-pongs by PARITY rather than by exchanging the buffers, for the same reason the probe does:
    !! a swap is O(n) per pass and the serial path avoids it with `move_alloc`, which is unavailable
    !! here because these are plain array dummies.
    subroutine sort_radix_design_a(ka, ra, kb, rb, nv, last_p, hist, nt, perm, value_base, done)
        integer(int64), allocatable, intent(inout) :: ka(:), ra(:) !! the images and rows to order.
        integer(int64), allocatable, intent(inout) :: kb(:), rb(:) !! the partner buffers.
        integer(int64), intent(in) :: nv              !! value-tier rows, occupying `1..nv`.
        integer, intent(in) :: last_p
        !! the last pass that will execute, or **-1 meaning "emit nothing"** — which is how a STRING
        !! key asks for the images and rows to be left sorted in `ka`/`ra` for its refine to read.
        integer(int64), intent(in) :: hist(0:255, 0:7) !! whole-column histograms; valid every pass.
        integer, intent(in) :: nt                     !! team size.
        integer(int64), intent(inout) :: perm(:)      !! receives the ordered row indices.
        integer(int64), intent(in) :: value_base      !! `perm` offset of the value tier.
        logical, intent(out) :: done                  !! .false. when this declined and did nothing.
        !
        integer(int64), allocatable :: cnt(:,:) !! per-thread cursors, `(bucket, thread)`.
        integer(int64), allocatable :: tmp(:)   !! `move_alloc` intermediary for the buffer swap.
        integer(int64) :: b
        integer :: p, ios
        !
        ! **Ping-pongs with `move_alloc`, exactly as the serial loop does**, which is why the buffers
        ! are ALLOCATABLE dummies here and plain arrays in Design B. It costs O(1) per pass, it keeps
        ! `ka`/`ra` holding the current state at every point so there is no parity to track, and --
        ! the reason it matters beyond tidiness -- it lets this procedure finish with the answer left
        ! in `ka`/`ra` rather than written to `perm`. That is precisely what a string key needs.
        ! Design B cannot do this: its buckets are sorted concurrently out of shared buffers, so
        ! exchanging them is not available.
        done = .false.
        allocate(cnt(0:255, 0:nt - 1), stat=ios)
        if (ios /= 0) return
        do p = 0, 7
            ! The constant-digit skip. `hist` is still the right thing to ask even though rows have
            ! moved: a permutation cannot change how many rows carry a given digit value.
            b = iand(ishft(ka(1), -8 * p), 255_int64)
            if (hist(b, p) == nv) cycle
            dbg_sort_radix_passes = dbg_sort_radix_passes + 1_int64
            ! Rebuilt every pass, never reused: a row's CHUNK changes when rows move, which is
            ! `feature_sort_parallel.md` §6.1's silently-wrong-permutation trap.
            call sort_radix_count_par(ka, nv, p, cnt, nt)
            call sort_radix_cursors(cnt, nt)
            if (p == last_p) then
                ! The last reordering pass writes rows straight into `perm` and drops the images.
                ! Unreachable when `last_p` is -1, which is how the string path keeps `ra` live.
                call sort_radix_emit_par(ka, ra, nv, p, cnt, nt, perm, value_base)
                deallocate(cnt)
                dbg_sort_design = 1_int64
                done = .true.
                return
            end if
            call sort_radix_scatter_par(ka, ra, kb, rb, nv, p, cnt, nt)
            call move_alloc(ka, tmp)
            call move_alloc(kb, ka)
            call move_alloc(tmp, kb)
            call move_alloc(ra, tmp)
            call move_alloc(rb, ra)
            call move_alloc(tmp, rb)
        end do
        deallocate(cnt)
        dbg_sort_design = 1_int64
        done = .true.
    end subroutine sort_radix_design_a

    !> Design B — one synchronised MSD split, then every bucket sorted alone by one thread.
    !!
    !! `feature_sort_parallel.md` §11 steps 2 and 3. Splits on `dsplit`, the most significant digit
    !! that VARIES, so the buckets are already in order and each one only needs digits `0..dsplit-1`.
    !!
    !! **Declines rather than aborts**, by returning `done = .false.` with `perm` untouched, on any of
    !! four conditions — a failed allocation, one bucket, no digit below the split, or a failed
    !! balance test. Every one of them leaves the serial LSD loop to do the whole job, so a decline
    !! costs time and never an answer.
    !!
    !! **The balance test is what machine B's report makes load-bearing.** A key whose split digit has
    !! few distinct values gets 1.39–1.85× here where Design A gets 2.70–4.74×, and B's run-to-run
    !! spread on such a key reaches 96.5% — so declining is not a marginal call. Today a decline falls
    !! back to the serial loop; when Design A lands (§11 step 4) it becomes the fallback instead.
    !!
    !! **Stability, which is the whole correctness argument.** The split's prefix runs bucket-major
    !! and thread-minor, so within one bucket the rows arrive in thread order and, within a thread, in
    !! their original order — i.e. exactly their order before the split. Each bucket's LSD is stable in
    !! turn, so rows equal on every digit keep file order and the row-index tiebreaker never has to be
    !! consulted. Get the prefix nesting backwards and the sort still *works*, producing a valid order
    !! that is not the unique correct one, which the A/B tests catch and a spot check would not.
    subroutine sort_radix_design_b(ka, ra, kb, rb, nv, dsplit, hist, nt, perm, value_base, done)
        integer(int64), intent(inout) :: ka(:), ra(:) !! the images and rows to order.
        integer(int64), intent(inout) :: kb(:), rb(:) !! the partner buffers; scratch afterwards.
        integer(int64), intent(in) :: nv              !! value-tier rows, occupying `1..nv`.
        integer, intent(in) :: dsplit                 !! the digit to split on; at least 1.
        integer(int64), intent(in) :: hist(0:255, 0:7) !! whole-column histograms; nothing has moved.
        integer, intent(in) :: nt                     !! team size for the split pass.
        integer(int64), intent(inout) :: perm(:)      !! receives the ordered row indices.
        integer(int64), intent(in) :: value_base      !! `perm` offset of the value tier.
        logical, intent(out) :: done                  !! .false. when this declined and did nothing.
        !
        integer(int64), allocatable :: scnt(:,:) !! per-thread split cursors, `(bucket, thread)`.
        integer(int64), allocatable :: tlo(:), thi(:) !! each task's range in the split's output.
        integer, allocatable :: tdmax(:)   !! highest digit each task still has to order by.
        logical, allocatable :: tsrcb(:)
        !! .true. when a task's rows are in `kb`/`rb` -- the split's output, which is where every
        !! unrefined bucket sits. A refinement flips the flag, because it scatters the task back into
        !! the partner pair. Tasks are disjoint RANGES, so one holding data in `ka` while its
        !! neighbour uses `ka` as scratch is safe: neither touches the other's slots.
        integer(int64) :: blo(0:255), bhi(0:255) !! each bucket's range in the split's output.
        integer(int64) :: nbuckets   !! how many buckets at `dsplit` are non-empty.
        integer(int64) :: maxcard    !! most non-empty buckets any digit at or below `dsplit` has.
        integer(int64) :: b, nb, m, target, spent, budget, sub_biggest
        integer(int64) :: mincard   !! resolved `SORT_SPLIT_MIN_CARD`, after any debug override.
        integer(int64) :: floor_task !! resolved `SORT_TASK_FLOOR`, after any debug override.
        integer :: team, ios, i, ntask, head, d, maxtask
        logical :: insrc !! which buffer pair this refinement's output landed in.
        !> Elements each thread must get from a refinement pass for that pass to be worth opening.
        !! The task floor is this times the team: never subdivide below `SORT_REFINE_ELEMS_PER_THREAD
        !! * nt`.
        !!
        !! **A refinement pass is THREADED, so its cost scales with the team while its work does
        !! not** -- `sort_radix_count_range_par` and `sort_radix_scatter_range_par` dispatch the whole
        !! team over one task's range, so refining a 16 K range with 64 threads gives each thread
        !! 256 elements and pays a full barrier for them. That is why the floor has to be a function
        !! of `nt`: the flat 4096 it replaces described a task size, when the quantity that actually
        !! decides is elements PER THREAD.
        !!
        !! **Measured on machine B, ifx and gfortran, `--profile release`.** Refining versus never
        !! refining, over n x team, with the crossover read off in units of `target = nv/team`:
        !! refinement starts paying at target 32768 at 16 threads, 65536 at 32 and 131072 at 64 --
        !! i.e. at 2048 elements per thread, the same constant at every team size, on both
        !! compilers. Below it the cost is severe and grows with the team: one refinement pass on a
        !! 32768-row column cost **2.0x at 16 threads, 6.1x at 32 and 10.6x at 64**, which
        !! `parquet_debug_sort_radix_passes` shows directly as the pass count going 20 -> 40.
        !!
        !! **The bracket is [512, 2048] and 2048 is chosen deliberately**, because the error is
        !! asymmetric: refining too eagerly costs up to 10.6x, declining a refinement that would
        !! have paid costs at most 1.9x. When in doubt, do not refine.
        !!
        !! Consequence worth knowing: the floor binds while `nv < SORT_REFINE_ELEMS_PER_THREAD *
        !! nt**2`, so a 64-thread team does no refinement at all below ~8.4 M rows, and a 192-thread
        !! team below ~75 M. Above that `nv/team` dominates and this constant is inert -- it has no
        !! effect on the large-column case at all.
        integer(int64), parameter :: SORT_REFINE_ELEMS_PER_THREAD = 2048_int64
        !> Divisor in the distinct-value floor `max(2, nt / SORT_SPLIT_CARD_PER_THREAD)`: the split
        !! digit's column must reach that many distinct values before refinement is worth attempting.
        !!
        !! **Refinement subdivides; it cannot manufacture distinctions the key does not have**, so a
        !! key with too few distinct values for the team leaves one thread holding most of the rows
        !! however many digits are burned looking for a split — and the wider the team, the more
        !! distinct values it takes before that stops happening. Hence a floor that scales with `nt`
        !! rather than the flat 16 this replaced.
        !!
        !! **The flat 16 was reasoned, never measured, and was wrong by up to 5.95x.** Measured on
        !! machine B over cardinality 2..128 x team 4..64, with the low-cardinality values HASHED
        !! across the int64 range (see below): the flat floor's worst case is 5.95x under ifx and
        !! 2.25x under gfortran, with geometric means of 1.675 and 1.278 — i.e. it was declining a
        !! split that wins 2-5x on ordinary low-cardinality wide keys. This form's worst case is
        !! 1.50x (ifx) / 1.12x (gfortran), geometric means 1.019 / 1.006, and it was the best of
        !! eleven candidate forms on BOTH metrics under BOTH compilers — so there is no
        !! ifx-versus-gfortran trade-off to weigh here; they agree on the ranking and differ only in
        !! how much the old constant cost.
        !!
        !! **Two things a future measurement of this must get right, because the first version of
        !! this one got both wrong.** A low-cardinality key whose values are PACKED into `0..card-1`
        !! never reaches this test at all — every distinction lives in the lowest byte, so `dsplit`
        !! is 0 and the guard above returns first; forcing this floor to 0 changes nothing for such a
        !! key, at any cardinality, which is what the negative control showed. And a key whose
        !! distinct values are a STRIDE apart is equally unrepresentative in the other direction: it
        !! leaves only the top byte varying, so Design A finishes in one pass and beats the split by
        !! 3-4x at cardinalities where a realistic key loses by 2-8%. Only hashed values exercise the
        !! comparison a real column presents.
        integer(int64), parameter :: SORT_SPLIT_CARD_PER_THREAD = 8_int64
        !
        done = .false.
        if (dsplit < 1) return
        !
        nbuckets = 0_int64
        do b = 0_int64, 255_int64
            if (hist(b, dsplit) > 0_int64) nbuckets = nbuckets + 1_int64
        end do
        ! One bucket means the split digit does not vary after all, which `dsplit`'s derivation should
        ! already have excluded -- kept because reaching the bucket loop with a single bucket would
        ! serialise the whole sort behind one thread while looking like it had parallelised.
        if (nbuckets < 2_int64) return
        !
        ! **The cardinality test, which REPLACED a balance test that tightened with the team.** The
        ! old form declined when `biggest * team * 2 > nv`, i.e. it required the largest bucket to be
        ! within 2x of a perfect 256-way split at 4 threads and within 32x of it at 64 -- so on
        ! machine B it declined on every real key and Design B never ran at all. Balance is now
        ! achieved rather than demanded (the refinement below subdivides whatever is oversized), so
        ! the only question left upfront is whether the key HAS enough distinct values for
        ! subdivision to reach a balanced state. The most populated digit answers that for free from
        ! histograms already built: `maxcard` is a lower bound on the key's distinct-value count.
        maxcard = 0_int64
        do d = 0, dsplit
            nb = 0_int64
            do b = 0_int64, 255_int64
                if (hist(b, d) > 0_int64) nb = nb + 1_int64
            end do
            if (nb > maxcard) maxcard = nb
        end do
        mincard = max(2_int64, int(nt, int64) / SORT_SPLIT_CARD_PER_THREAD)
        if (dbg_sort_split_min_card >= 0_int64) mincard = dbg_sort_split_min_card
        if (maxcard < mincard) return
        team = nt
        ! **Task-list capacity, sized from the TEAM rather than fixed.** At most `team` tasks can
        ! exceed a fair share at any moment, and one refinement turns a task into at most 256, so
        ! `256 * (team + 2)` is a real bound on the walk rather than a ceiling it might trip over --
        ! which matters because a ceiling that binds stops refinement in the middle of the list, and
        ! then the tasks that keep their full size are whichever the walk had not reached.
        maxtask = 256 * (nt + 2)
        !
        allocate(scnt(0:255, 0:nt - 1), stat=ios)
        if (ios /= 0) return
        allocate(tlo(0:maxtask - 1), thi(0:maxtask - 1), &
                 tdmax(0:maxtask - 1), tsrcb(0:maxtask - 1), stat=ios)
        if (ios /= 0 .or. dbg_sort_radix_fail_alloc == 4) then
            deallocate(scnt)
            return
        end if
        !
        ! **The split is one count-prefix-scatter over the whole column** — the same three steps every
        ! Design A pass performs, which is why they are shared helpers rather than written twice here.
        ! The count costs one extra read of `ka` and is deliberately NOT taken from the histogram
        ! build's own per-thread array even though nothing has moved since: that array exists only
        ! when the threaded histogram path ran, and coupling the two would make a decline there
        ! silently disable this. §6.1's rule — rebuild after any reordering — is satisfied either way,
        ! since the split is the first thing that moves anything.
        call sort_radix_count_par(ka, nv, dsplit, scnt, nt)
        call sort_radix_cursors(scnt, nt, blo, bhi)
        call sort_radix_scatter_par(ka, ra, kb, rb, nv, dsplit, scnt, nt)
        dbg_sort_radix_passes = dbg_sort_radix_passes + 1_int64
        !
        ! The initial task list: one per non-empty bucket, all sitting in the split's output and all
        ! still owing digits `0..dsplit-1`.
        ntask = 0
        do b = 0_int64, 255_int64
            if (hist(b, dsplit) > 0_int64) then
                tlo(ntask) = blo(b)
                thi(ntask) = bhi(b)
                tdmax(ntask) = dsplit - 1
                tsrcb(ntask) = .true.
                ntask = ntask + 1
            end if
        end do
        !
        ! **Refinement -- the balanced split, and the reason this design no longer declines on a
        ! skewed key.** An oversized bucket is re-split on its next digit BY THE WHOLE TEAM, and its
        ! sub-buckets rejoin the task list owing one digit fewer.
        !
        ! **The work is not extra.** That bucket had to make a pass over digit `tdmax` regardless;
        ! refinement performs that same pass with `nt` threads instead of the one thread that would
        ! have drawn the bucket, and every sub-bucket then owes one digit fewer. What changes is
        ! *who* does it, not how much there is -- which is why this is worth doing even when the
        ! imbalance is mild, and why the budget below is a safety rail rather than a real constraint.
        !
        ! **A single forward walk of the task list, never a repeated search for the largest.** An
        ! earlier version picked the biggest oversized task each round, which is a better SCHEDULE
        ! and a much worse algorithm: the scan is O(ntask) per refinement, so at the task counts this
        ! design wants it became the serial bottleneck -- 64 threads measured SLOWER than 32 with it
        ! in place (8.22 against 4.94 ns/element at target 1024), because the scan does not shrink
        ! when the team grows. The walk below touches each entry once.
        !
        ! **The target is ONE FAIR SHARE of the column, and that is measured rather than reasoned.**
        ! Two instincts are both wrong here and both were tested. Aiming for several tasks per thread
        ! so `schedule(dynamic)` has slack costs more in refinement passes than it recovers in
        ! balance: at 64 threads, `nv / (team * 1)` measured **1.77 ns/element** against 2.63 at
        ! `* 2` and 4.03 at `* 4`. And an absolute, cache-sized target -- the shape a bucket's own
        ! working set would suggest -- is worse still, monotonically: 32768 gave 3.80, 8192 gave
        ! 3.80, 2048 gave 7.41, 1024 gave 8.22, because each halving of the target roughly doubles
        ! the number of threaded refinement passes and every one of them is a full read and write of
        ! its range. **Refinement passes are the cost; task count is not.** (f64, n = 5e6, ifx,
        ! machine B, `benchmark_sort_tail`.)
        !
        ! It is also the only team-dependent term left, and it moves the right way with the machine:
        ! a 4-core team asks for tasks of `nv / 4` and refines almost nothing, a 64-core team asks
        ! for `nv / 64` and refines whatever is above it.
        target = nv / int(team, int64)
        floor_task = SORT_REFINE_ELEMS_PER_THREAD * int(nt, int64)
        if (dbg_sort_task_floor >= 0_int64) floor_task = dbg_sort_task_floor
        if (target < floor_task) target = floor_task
        budget = 4_int64 * nv
        spent = 0_int64
        head = 0
        do while (head < ntask)
            m = thi(head) - tlo(head) + 1_int64
            if (tdmax(head) < 1 .or. m <= target) then
                head = head + 1
                cycle
            end if
            if (ntask + 256 > maxtask) exit
            if (spent + m > budget) exit
            spent = spent + m
            d = tdmax(head)
            !
            ! **Count first, scatter only if the digit actually splits.** A constant digit is common
            ! -- it is exactly why a bucket ended up oversized -- and scattering on one moves every
            ! row for nothing. Counting is half the cost and answers the question, after which the
            ! task simply drops a digit and is re-examined without advancing the queue. Lowering
            ! `tdmax` past a constant digit is sound because ordering by `0..d-1` and by `0..d` agree
            ! when every row in the range shares digit `d`.
            if (tsrcb(head)) then
                call sort_radix_count_range_par(kb, tlo(head), thi(head), d, scnt, nt)
            else
                call sort_radix_count_range_par(ka, tlo(head), thi(head), d, scnt, nt)
            end if
            call sort_radix_cursors(scnt, nt, blo, bhi, first=tlo(head))
            sub_biggest = 0_int64
            do b = 0_int64, 255_int64
                nb = bhi(b) - blo(b) + 1_int64
                if (nb > sub_biggest) sub_biggest = nb
            end do
            if (sub_biggest >= m) then
                tdmax(head) = d - 1
                cycle
            end if
            if (tsrcb(head)) then
                call sort_radix_scatter_range_par(kb, rb, ka, ra, tlo(head), thi(head), d, scnt, nt)
            else
                call sort_radix_scatter_range_par(ka, ra, kb, rb, tlo(head), thi(head), d, scnt, nt)
            end if
            dbg_sort_radix_passes = dbg_sort_radix_passes + 1_int64
            !
            ! The sub-buckets are appended at the TAIL and the parent is emptied, so the walk never
            ! revisits an index and a sub-bucket that is itself oversized is refined in turn when the
            ! head reaches it. An emptied entry costs `sort_radix_bucket` an immediate return.
            insrc = .not. tsrcb(head)
            do b = 0_int64, 255_int64
                if (bhi(b) >= blo(b)) then
                    tlo(ntask) = blo(b)
                    thi(ntask) = bhi(b)
                    tdmax(ntask) = d - 1
                    tsrcb(ntask) = insrc
                    ntask = ntask + 1
                end if
            end do
            thi(head) = tlo(head) - 1_int64
            head = head + 1
        end do
        deallocate(scnt)
        dbg_sort_split_buckets = int(ntask, int64)
        dbg_sort_design = 2_int64
        !
        ! Phase 2. `schedule(dynamic)` because task sizes vary by orders of magnitude on real data
        ! and a static split would leave every thread waiting on whichever drew the largest. Each
        ! task names which buffer pair holds its rows; the other pair is its scratch, over its own
        ! range only. Ranges are disjoint, so no per-thread allocation is needed at all.
        team = nt
        if (team > ntask) team = ntask
        !$omp parallel do num_threads(team) default(shared) private(i) schedule(dynamic)
        do i = 0, ntask - 1
            if (tsrcb(i)) then
                call sort_radix_bucket(kb, rb, ka, ra, tlo(i), thi(i), tdmax(i), perm, value_base)
            else
                call sort_radix_bucket(ka, ra, kb, rb, tlo(i), thi(i), tdmax(i), perm, value_base)
            end if
        end do
        !$omp end parallel do
        deallocate(tlo, thi, tdmax, tsrcb)
        done = .true.
    end subroutine sort_radix_design_b
#endif

    !> Fills `perm` by a stable LSD radix sort over ONE key. See this section's banner for why the
    !! result is the permutation `sort_comparison_permutation` would have produced.
    !!
    !! **Runs out of memory by DECLINING, not by aborting.** This path needs about 32 bytes per row
    !! of scratch (measured, and linear: 32.03 B/row at 5 M rows, 32.01 at 10 M, 31.92 at 20 M; a
    !! string column can add 8 more, but only if a shared-prefix run actually needs splitting)
    !! where the comparison sort needs none, so it is the one place in the engine where an ordinary
    !! sort of ordinary data can fail purely for being large. Both `allocate`s therefore carry
    !! `stat=`, and a failure returns `ok = .false.` with `perm` untouched, whereupon the caller
    !! runs the comparison sort instead. The answer is identical either way -- only the time differs
    !! -- so there is nothing to report and no reason to stop.
    !!
    !! **On Linux this is a partial defence and that is worth knowing.** Under the default
    !! overcommit policy a large `allocate` usually SUCCEEDS and the kernel kills the process on
    !! first touch, so `stat=` never sees it; the fallback works where allocation failure is really
    !! reported (macOS, and Linux with overcommit restricted). Where it cannot help, the honest
    !! answer is `parquet_set_sort_radix_path(.false.)`, which declines the scratch up front.
    subroutine sort_radix_permutation(keys, n, perm, ok, nt)
        type(sort_key_buf), intent(in) :: keys(:) !! the keys; exactly one, per `sort_radix_candidate`.
        integer(int64), intent(in) :: n           !! rows to order.
        integer(int64), intent(inout) :: perm(:)  !! receives `n` 1-based row indices.
        logical, intent(out) :: ok                !! .false. when the scratch could not be allocated.
        integer, intent(in) :: nt                 !! team size; 1 runs every phase serially.
        !
        integer(int64), allocatable :: ka(:), kb(:) !! key images, ping-ponged between passes.
        integer(int64), allocatable :: thist(:,:,:) !! per-thread histograms, `(bucket, byte, thread)`.
        integer :: tid !! this thread's index within the team, 0-based.
        integer :: tt  !! walk index over threads when the per-thread histograms are reduced.
        integer(int64) :: c_lo, c_hi !! one thread's chunk of the image array, inclusive.
        logical :: hist_done !! .true. once the histogram is built, by whichever of the two paths.
        logical :: did_par   !! .true. once Design A or B has ordered the value tier.
        logical :: tier_done !! .true. once the tier split has been done by the threaded path.
        integer(int64), allocatable :: ra(:), rb(:) !! the row indices travelling with them.
        integer(int64), allocatable :: tmp(:)       !! `move_alloc` intermediary for the swap.
        integer(int64) :: hist(0:255, 0:7) !! one histogram per byte position, all built in ONE pass.
        integer(int64) :: off(0:255)       !! running output cursor per bucket.
        integer(int64) :: i, j, b, t, u    !! row, output slot, bucket, key image, shift register.
        integer(int64) :: nv, nnan, nnull  !! rows in the value, NaN and null tiers.
        integer(int64) :: value_base       !! output positions before the value block, 0-based.
        integer(int64) :: nan_pos, null_pos !! next output position for a NaN / a null, 1-based.
        logical :: has_nulls, is_real, is_str, is_int !! hoisted family and nullability tests.
        integer(int64) :: vmin, vmax !! the integer key's value range over its VALID rows.
        logical :: narrow !! .true. when that range spans under 2^32, so the bias image applies.
        integer :: p !! byte position, 0 = least significant.
        integer :: last_p !! the last pass that will execute, or -1 when none writes into perm.
        integer :: ios !! allocation status; nonzero means decline, never abort.
        real(real64) :: x !! one real key value, for the NaN test.
        !
        ok = .false.
        has_nulls = allocated(keys(1)%valid)
        is_real = (keys(1)%family == SK_REAL)
        is_str = (keys(1)%family == SK_STR)
        is_int = (keys(1)%family == SK_INT)
        ! Seeded so that the tier-split loop below needs two compares per row and no "first value"
        ! test. `nv > 1` guards every read of them, which is what keeps this seed from ever reaching
        ! `sort_span_under_2p32` -- where `hi - lo` on it would overflow.
        vmin = huge(0_int64)
        vmax = -huge(0_int64) - 1_int64
        allocate(ka(n), ra(n), stat=ios)
        if (ios /= 0 .or. dbg_sort_radix_fail_alloc == 1) return
        !
        ! Pass 1 -- tier split and key transform, walked in ROW ORDER so that every tie and both
        ! non-value tiers keep file order with no later stable pass needed.
        !
        ! Threaded when there is a team, by the two-pass count-prefix-fill in `sort_tier_split_par`;
        ! it reports `nv < 0` if it could not allocate its per-thread counters, whereupon the serial
        ! loop below does the whole job. This is the last of §11 step 1's whole-column phases: it
        ! reads every row of the column and, left serial, it is a pure Amdahl term under every
        ! parallel design.
        tier_done = .false.
#ifdef _OPENMP
        if (nt > 1) then
            call sort_tier_split_par(keys(1), n, has_nulls, is_real, is_int, nt, ra, nv, nnan, nnull, &
                vmin, vmax)
            tier_done = (nv >= 0_int64)
        end if
#endif
        if (.not. tier_done) then
            nv = 0_int64
            nnan = 0_int64
            nnull = 0_int64
        end if
        do i = 1_int64, n
            ! An `exit` rather than wrapping the loop, so the serial body below stays exactly as it
            ! was and this reads as one added line rather than a re-indentation of thirty.
            if (tier_done) exit
            if (has_nulls) then
                if (keys(1)%valid(i) == 0_c_int8_t) then
                    nnull = nnull + 1_int64
                    cycle
                end if
            end if
            if (is_real) then
                ! `x /= x` rather than `ieee_is_nan` -- see this submodule's header.
                x = keys(1)%reals(i)
                if (x /= x) then
                    nnan = nnan + 1_int64
                    cycle
                end if
            end if
            nv = nv + 1_int64
            ra(nv) = i
            ! The integer key's value range, folded into this loop rather than taken in a pass of
            ! its own. The difference is not the two compares -- it is that `keys(1)%ints(i)` here is
            ! SEQUENTIAL, where a separate scan would walk `ra(j)` and gather. Measured on a
            ! full-range int64 column, where the range can never pay for itself: the separate
            ! gathered pass cost ~8%, folding it in costs nothing detectable.
            if (is_int) then
                t = keys(1)%ints(i)
                if (t < vmin) vmin = t
                if (t > vmax) vmax = t
            end if
        end do
        ! An INTEGER key whose whole value range spans under 2^32 is imaged as `v - vmin` rather than
        ! by flipping its sign bit. Both are order-preserving unsigned images, but the biased one
        ! lands in `[0, 2^32)`, so its top four bytes are constant and the pass-skip below drops four
        ! of the eight passes outright. That is the sign-extension cost `SORT_RADIX_MIN_ROWS`'s note
        ! describes, removed -- and its reach is the value SPAN rather than the declared type, so any
        ! int64 column of identifiers, epoch seconds, counts, dates or category codes gets it too.
        !
        ! **The range is scanned HERE rather than reused from `sort_counting_candidate`, which has
        ! usually just computed it.** Reusing it would be free, and it would couple this to that
        ! procedure's guards and to `parquet_set_sort_counting_path`: a user disabling the counting
        ! path would silently lose an unrelated optimisation, and a future early return added to the
        ! candidate after the point where it scans would leave a stale `lo = 0` here -- which is a
        ! bias by a value that is not the minimum, i.e. a subtraction that can leave int64 and so
        ! become undefined, and a column that quietly stops skipping its top passes. A scan costing
        ! two compares per element buys immunity from both. Do not "optimise" it into a reuse.
        !
        ! Nulls never reach the scan: it sits after this loop's null `cycle`, so a null row's key
        ! slot -- which holds whatever the buffer contained -- cannot widen the range past the test
        ! and cost the optimisation.
        !
        ! **Which mistakes here are dangerous, because it is exactly one direction.** The image is
        ! `v - vmin`, so the only unsafe error is a `vmin` ABOVE some value being imaged: that makes
        ! the difference negative and the unsigned order wrong. Every error in the other direction --
        ! a `vmin` too low, a `vmax` too high, a range widened by an unskipped null -- is safe by
        ! construction, because `sort_span_under_2p32` is then applied to a range that CONTAINS the
        ! true one, so a `narrow` answer still bounds every real difference. Those mistakes cost the
        ! four skipped passes and nothing else, which is why mutation testing finds them through the
        ! pass counter rather than through a wrong answer. Bias by `vmax` and the suite fails
        ! immediately; seed the scan at zero and only the far-from-zero fixture notices.
        narrow = .false.
        if (is_int .and. nv > 1_int64) narrow = sort_span_under_2p32(vmin, vmax)
        ! The images, in one bulk call rather than one call per row -- see `sort_radix_images_range`.
        ! This costs one extra sequential read of `ra` and removes `nv` calls, each of which carried a
        ! family dispatch. Every row handed over is a value-tier row by construction.
        if (narrow) then
            call sort_radix_images_threaded(keys(1), ra, nv, ka, nt, vmin)
        else
            call sort_radix_images_threaded(keys(1), ra, nv, ka, nt)
        end if
        !
        ! Where each block starts. Deliberately free of `descending`, exactly as the counting path
        ! is: a descending sort still puts nulls last.
        if (keys(1)%nulls_first) then
            null_pos = 1_int64
            nan_pos = nnull + 1_int64
            value_base = nnull + nnan
        else
            value_base = 0_int64
            nan_pos = nv + 1_int64
            null_pos = nv + nnan + 1_int64
        end if
        !
        last_p = -1
        if (nv > 1_int64) then
            ! All eight histograms in one read of `ka`, rather than one read per pass. Shared with
            ! the multi-key chain -- see `sort_radix_hist_threaded`, which owns the threading, the
            ! per-thread reduction and the decline-to-serial fallback that used to sit here inline.
            call sort_radix_hist_threaded(ka, nv, hist, nt)
            allocate(kb(nv), rb(nv), stat=ios)
            ! Nothing has been written to `perm` yet, so returning here really is a clean decline
            ! rather than a half-finished sort. Keep any future allocation ahead of the first
            ! `perm` write for the same reason.
            if (ios /= 0) return
            ! **Which pass is the LAST one that will run**, decided here because the whole histogram
            ! matrix is available and the answer is otherwise unknowable until the loop is over. That
            ! pass writes its rows straight into `perm`, which removes its `kb` write and the whole
            ! final copy -- 24 bytes per element of the ~296 this path moves.
            !
            ! `-1` means "no pass writes into `perm`", and it covers three cases that must all end at
            ! the copy below: a STRING key, whose refine reads the sorted `ka` and `ra` after the
            ! loop and would get a stale `ra` from a direct write; a column every digit of which is
            ! constant, which executes no pass at all; and `nv <= 1`.
            !
            ! Reading `ka(1)` is sound at any point in the loop even though the array is permuted
            ! under it: if every row agrees on digit `p` then any element answers for all of them,
            ! and if they do not then `hist(b, p) /= nv` whichever element is read.
            if (.not. is_str) then
                do p = 7, 0, -1
                    b = iand(ishft(ka(1), -8 * p), 255_int64)
                    if (hist(b, p) /= nv) then
                        last_p = p
                        exit
                    end if
                end do
            end if
            ! **Design B, tried before the serial loop and falling straight through when it declines.**
            ! Excluded for a STRING key on the same grounds `last_p` excludes it: the refine below
            ! reads the sorted `ka`/`ra`, which the split's buffer swap and the per-bucket emit both
            ! leave stale. `feature_sort_parallel.md` §11 step 5 is where strings get this properly,
            ! and its argument -- rows in different buckets differ in the split byte, so no
            ! shared-prefix run can straddle a boundary -- is why that is a real step and not a wish.
            did_par = .false.
#ifdef _OPENMP
            ! **A string key is kept out of Design B THREE times over, and all three are deliberate.**
            ! `.not. is_str` here; `last_p >= 1` here, which a string can never satisfy because
            ! `last_p` is forced to -1 for one above; and `dsplit < 1` inside Design B itself. Removing
            ! the first alone is a semantic NO-OP — confirmed by mutation, and by the string arm of
            ! `test_fortran_engine_threading`, whose `split_buckets == 0` assertion still held with it
            ! gone. Keep all three: the redundancy is what stops a future change to the `last_p`
            ! computation from quietly routing strings into a design that abandons the images their
            ! refine has to read.
            if (nt > 1 .and. .not. is_str .and. last_p >= 1) then
                call sort_radix_design_b(ka, ra, kb, rb, nv, last_p, hist, nt, perm, value_base, did_par)
            end if
            ! **Design A is the fallback, and it is reached exactly when B declined.** That is what
            ! machine B's report makes required rather than optional: on a key whose top varying digit
            ! has few distinct values -- which is every bounded-range real, and every string with a
            ! common stem -- B gets 1.39-1.85x where A gets 2.70-4.74x at 64 threads. Before this
            ! existed, such a key fell all the way back to the SERIAL loop.
            !
            ! `last_p >= 0` rather than `>= 1`: A needs no digit below the split because it has no
            ! split, so a single-pass column is still worth threading.
            ! **Strings reach Design A and only Design A**, which is where machine B's report puts
            ! them: a string column with a common stem is the low-cardinality-split shape, and there
            ! A gets 2.70-4.74x at 64 threads where B gets 1.39-1.85x. It needs no `last_p` guard --
            ! -1 is a valid argument meaning "leave the answer in `ka`/`ra`" -- and no `is_str` guard,
            ! because Design A never touches the images the refine goes on to read.
            if (.not. did_par .and. nt > 1) then
                call sort_radix_design_a(ka, ra, kb, rb, nv, last_p, hist, nt, perm, value_base, did_par)
            end if
#endif
            do p = 0, 7
                ! Design B ordered every bucket and wrote `perm` itself, so the serial loop has
                ! nothing left to do. Deliberately an `exit` rather than an early RETURN from the
                ! procedure: the two non-value tiers are written after this loop, and returning here
                ! would leave every NaN and every null row unwritten -- with `ok = .true.` claiming
                ! otherwise. `last_p >= 1` is a preconditon of Design B running, so the `last_p < 0`
                ! copy below is already skipped, and `is_str` excludes the string refine.
                if (did_par) exit
                ! A byte position every row agrees on cannot reorder anything. This is what makes a
                ! key narrower than 64 bits cost proportionately less -- an int32 or a float32 key
                ! leaves its top bytes constant and skips those passes outright, and the narrow-range
                ! bias above exists to put an integer key into exactly that state.
                b = iand(ishft(ka(1), -8 * p), 255_int64)
                if (hist(b, p) == nv) cycle
                dbg_sort_radix_passes = dbg_sort_radix_passes + 1_int64
                t = 1_int64
                do b = 0_int64, 255_int64
                    off(b) = t
                    t = t + hist(b, p)
                end do
                if (p == last_p) then
                    ! The rows land in their final places and the images are not carried forward --
                    ! nothing reads them again. `ra` is deliberately left STALE here; the string
                    ! refine is the only thing that would notice, and it is excluded above.
                    do j = 1_int64, nv
                        b = iand(ishft(ka(j), -8 * p), 255_int64)
                        perm(value_base + off(b)) = ra(j)
                        off(b) = off(b) + 1_int64
                    end do
                    exit
                end if
                do j = 1_int64, nv
                    b = iand(ishft(ka(j), -8 * p), 255_int64)
                    kb(off(b)) = ka(j)
                    rb(off(b)) = ra(j)
                    off(b) = off(b) + 1_int64
                end do
                call move_alloc(ka, tmp)
                call move_alloc(kb, ka)
                call move_alloc(tmp, kb)
                call move_alloc(ra, tmp)
                call move_alloc(rb, ra)
                call move_alloc(tmp, rb)
            end do
        end if
        if (last_p < 0) then
            do j = 1_int64, nv
                perm(value_base + j) = ra(j)
            end do
        end if
        if (is_str) call sort_radix_refine_strings(keys, 1, ka, ra, nv, value_base, perm, .true., nt)
        !
        ! The two non-value tiers, in row order -- both compare equal under `sort_compare_key`, so
        ! file order is the whole answer for them.
        if (nnan > 0_int64 .or. nnull > 0_int64) then
            do i = 1_int64, n
                if (has_nulls) then
                    if (keys(1)%valid(i) == 0_c_int8_t) then
                        perm(null_pos) = i
                        null_pos = null_pos + 1_int64
                        cycle
                    end if
                end if
                if (is_real) then
                    x = keys(1)%reals(i)
                    if (x /= x) then
                        perm(nan_pos) = i
                        nan_pos = nan_pos + 1_int64
                    end if
                end if
            end do
        end if
        ok = .true.
    end subroutine sort_radix_permutation

    !> Fills `perm` by running one stable radix pass per key, from the LAST key to the first.
    !!
    !! **Why right to left.** Each pass is stable, so a pass on key `k` leaves rows that tie on key
    !! `k` in the order the previous passes established — which is the order given by keys `k+1 …`.
    !! Running the keys in reverse therefore ends with the rows ordered by key 1, ties broken by
    !! key 2, and so on, which is exactly what `sort_row_less` walks. The initial order is the
    !! identity, so rows tying on every key keep file order — the row-index tiebreaker, obtained
    !! without ever comparing an index.
    !!
    !! **Every key carries its own tiers and its own flags**, which is the thing to hold on to here:
    !! `descending` and `nulls_first` are per key, a NaN tier exists only for a real key, and a row
    !! that is null under key 2 may be an ordinary value under key 1. Nothing about a row is decided
    !! once for all keys; each pass asks its own key afresh.
    !!
    !! String keys are excluded by `sort_radix_candidate` — see the note there.
    subroutine sort_radix_multi_permutation(keys, n, perm, ok, nt)
        type(sort_key_buf), intent(in) :: keys(:) !! the keys, in precedence order; at least two.
        integer(int64), intent(in) :: n           !! rows to order.
        integer(int64), intent(inout) :: perm(:)  !! receives `n` 1-based row indices.
        logical, intent(out) :: ok                !! .false. when the scratch could not be allocated.
        integer, intent(in) :: nt                 !! team size; 1 runs every pass serially.
        !
        integer(int64), allocatable :: code(:), pb(:), cb(:) !! key images, and the ping-pong buffers.
        integer(int64), allocatable :: cnt(:,:) !! per-thread scatter cursors; unallocated = serial.
        integer(int64) :: hist(0:255, 0:7) !! one histogram per byte position, all built in ONE pass.
        integer(int64) :: tier(0:2)        !! rows per tier, then the tier pass's output cursor.
        integer(int64) :: i, j, b          !! row, output slot, bucket.
        integer :: k, r                    !! key index, tier rank.
        integer :: vrank                   !! the rank a VALUE-tier row has under this key.
        integer :: ios                     !! allocation status; a failure declines, never aborts.
        logical :: has_tiers               !! .true. when this key can put a row outside the value tier.
        logical :: in_pb                   !! .true. when the live permutation is `pb`, not `perm`.
        !
        ok = .false.
        allocate(code(n), pb(n), cb(n), stat=ios)
        if (ios /= 0 .or. dbg_sort_radix_fail_alloc == 1) return
#ifdef _OPENMP
        ! Allocated ONCE for the whole chain rather than per key or per pass: it is 2 KB per thread
        ! and its contents are rewritten by every count. A failure leaves it unallocated, which is
        ! the signal each pass tests to fall back to its serial arms -- so running out of memory here
        ! costs speed and never an answer.
        if (nt > 1) then
            allocate(cnt(0:255, 0:nt - 1), stat=ios)
            if (ios /= 0 .and. allocated(cnt)) deallocate(cnt)
        end if
#endif
        ! Threaded on the same rule the single-key path's `fill_identity` uses: this is a whole
        ! column pass, and leaving it serial charges every multi-key sort for it.
#ifdef _OPENMP
        if (nt > 1) then
            !$omp parallel do num_threads(nt) default(shared) private(i) schedule(static)
            do i = 1_int64, n
                perm(i) = i
            end do
            !$omp end parallel do
        else
#endif
            do i = 1_int64, n
                perm(i) = i
            end do
#ifdef _OPENMP
        end if
#endif
        !
        do k = size(keys), 1, -1
            if (keys(k)%family == SK_STR) then
                call sort_radix_string_key_pass(keys, k, n, perm, code, cb, pb, cnt, nt)
                cycle
            end if
            ! The value image, and 0 for a row this key calls null or NaN -- those are ordered by
            ! the tier pass below, and giving them one shared image leaves them in the order the
            ! keys after this one established, which is what stability owes them.
            !
            ! Every row is imaged and the non-value ones are then overwritten, rather than each row
            ! being tested before it is imaged: that is what lets the image build be one bulk call
            ! with its family dispatch hoisted out. Imaging a null row is harmless -- see
            ! `sort_radix_images_range` -- and the result never survives this loop.
            !
            ! Both hoisted out of the row loop: `key_has_tiers` is a property of the KEY, and the
            ! value rank is loop-invariant too. A key with neither a validity array nor a NaN tier
            ! puts every row in the value tier, so the zeroing loop below and the whole tier pass
            ! further down are no-ops -- which is the ordinary case for an ordinary column, and was
            ! being discovered by scanning every row twice to find out.
            has_tiers = key_has_tiers(keys(k))
            vrank = sort_radix_value_rank(keys(k))
            call sort_radix_images_threaded(keys(k), perm, n, code, nt)
            if (has_tiers) then
                do j = 1_int64, n
                    if (sort_radix_tier_rank(keys(k), perm(j)) /= vrank) code(j) = 0_int64
                end do
            end if
            !
            call sort_radix_hist_threaded(code, n, hist, nt)
            ! The permutation ALTERNATES between `perm` and `pb` rather than being copied back after
            ! every pass. That copy was a full read and write of `n` int64 per pass per key, against
            ! the 32 bytes per element the scatter itself moves -- a third of the pass's whole
            ! traffic, for nothing. The images need no such flag: `move_alloc` leaves the live array
            ! in `code` either way, so only the permutation has a parity.
            !
            ! Reset PER KEY, not once for the whole list, because the copy-back below leaves `perm`
            ! live again at the end of every key.
            ! **The chain itself is shared with the STRING key pass** -- they were the same loop
            ! written twice, and only this copy was ever threaded. See `sort_radix_lsd_chain`.
            call sort_radix_lsd_chain(code, cb, perm, pb, n, hist, cnt, nt, in_pb)
            ! At most one copy-back per key instead of one per pass, and it must happen HERE: the
            ! tier pass below reads `perm`, and so does the next key's image build.
            if (in_pb) then
                call sort_copy_threaded(pb, perm, n, nt)
                in_pb = .false.
            end if
            !
            ! The tier pass, last because the tier outranks the value. Three buckets, whose ORDER
            ! already carries this key's `nulls_first` -- see `sort_radix_tier_rank`. Skipped
            ! outright for a key that cannot have a tier, rather than discovered by counting.
            if (.not. has_tiers) cycle
            tier = 0_int64
            do j = 1_int64, n
                r = sort_radix_tier_rank(keys(k), perm(j))
                tier(r) = tier(r) + 1_int64
            end do
            ! Every row in one tier: nothing to reorder, and the common case for an ordinary column.
            if (maxval(tier) == n) cycle
            i = 1_int64
            do r = 0, 2
                b = tier(r)
                tier(r) = i
                i = i + b
            end do
            do j = 1_int64, n
                r = sort_radix_tier_rank(keys(k), perm(j))
                pb(tier(r)) = perm(j)
                tier(r) = tier(r) + 1_int64
            end do
            call sort_copy_threaded(pb, perm, n, nt)
        end do
        ok = .true.
    end subroutine sort_radix_multi_permutation

    !> The count-prefix-scatter LSD chain over all eight byte positions of a prebuilt image array.
    !!
    !! **Extracted so the multi-key chain's NUMERIC and STRING key passes share one implementation.**
    !! They were the same loop written twice, and only the numeric copy had ever been threaded -- so a
    !! `multi3` sort scaled 1.49x against `multi2`'s 3.01x, with the string key's own contribution
    !! measured at 1.10x across 1 to 64 threads, i.e. not threaded at all. Sharing the loop is what
    !! gives the string key the threading rather than writing a third copy of it.
    !!
    !! **`pr` is the caller's permutation OR A CONTIGUOUS SLICE OF IT.** The numeric pass owns the
    !! whole column and passes `perm`; the string pass owns only the value tier and passes
    !! `perm(lo:hi)`. That costs nothing -- every dummy this hands the slice to is plain assumed-shape,
    !! so it travels as a descriptor and is never copied in or out, and `perm` traces back to an
    !! allocatable and so is contiguous anyway. An earlier reading of this file recorded the section
    !! as a copy-in/copy-out hazard and deferred the whole change for it; it is not one.
    !!
    !! **The images ping-pong with `move_alloc`, the rows with a parity flag**, for the reason the
    !! numeric pass already recorded: `pr` is a plain dummy array and cannot join a `move_alloc` swap.
    !! So the live images are ALWAYS in `code` on return -- there is no image parity to report and no
    !! image copy to make -- while the live rows are in `pb` when `in_alt` is `.true.` and in `pr`
    !! when it is `.false.`.
    subroutine sort_radix_lsd_chain(code, cb, pr, pb, nv, hist, cnt, nt, in_alt)
        integer(int64), allocatable, intent(inout) :: code(:), cb(:) !! images; swapped every pass.
        integer(int64), intent(inout) :: pr(:) !! rows in the caller's permutation, or a slice of it.
        integer(int64), intent(inout) :: pb(:) !! the partner row buffer; `1..nv` are used.
        integer(int64), intent(in) :: nv       !! rows, occupying `1..nv` of every array here.
        integer(int64), intent(in) :: hist(0:255, 0:7) !! prebuilt per-digit histograms; valid every pass.
        integer(int64), allocatable, intent(inout) :: cnt(:,:)
        !! per-thread cursors; UNALLOCATED is the signal to take the serial arms.
        integer, intent(in) :: nt              !! team size.
        logical, intent(out) :: in_alt         !! .true. when the live rows ended in `pb`, not `pr`.
        !
        integer(int64), allocatable :: tmp(:)  !! `move_alloc` intermediary for the image swap.
        integer(int64) :: off(0:255)           !! running output cursor per bucket, serial arms only.
        integer(int64) :: i, j, b
        integer :: p
        logical :: threaded_pass !! .true. when this pass's scatter was done by the team.
        !
        in_alt = .false.
        do p = 0, 7
            ! A byte position every row agrees on cannot reorder anything. `hist` stays the right
            ! thing to ask even though rows have moved: a permutation cannot change how many rows
            ! carry a given digit value. `code(1)` is always the live image array -- that is what
            ! `move_alloc` buys over a second parity flag.
            b = iand(ishft(code(1), -8 * p), 255_int64)
            if (hist(b, p) == nv) cycle
            dbg_sort_radix_passes = dbg_sort_radix_passes + 1_int64
            i = 1_int64
            do b = 0_int64, 255_int64
                off(b) = i
                i = i + hist(b, p)
            end do
            ! The serial 256-entry prefix above runs either way: it costs nothing beside a pass over
            ! `nv`, and computing it unconditionally leaves the serial arms below exactly as they are
            ! instead of re-indenting them under another guard.
            threaded_pass = .false.
#ifdef _OPENMP
            if (nt > 1 .and. allocated(cnt)) then
                ! Rebuilt every pass, never reused: a row's CHUNK changes when rows move, which is
                ! `feature_sort_parallel.md` section 6.1's silently-wrong-permutation trap. This is
                ! the one cost the threaded arm carries that the serial one does not -- the serial
                ! cursors come from the prebuilt `hist` for free, so a pass goes from one read and
                ! one write to two reads and one write, divided by the team. Break-even is below two
                ! threads.
                call sort_radix_count_par(code, nv, p, cnt, nt)
                call sort_radix_cursors(cnt, nt)
                if (in_alt) then
                    call sort_radix_scatter_par(code, pb, cb, pr, nv, p, cnt, nt)
                else
                    call sort_radix_scatter_par(code, pr, cb, pb, nv, p, cnt, nt)
                end if
                threaded_pass = .true.
                ! Reported as Design A because that is structurally what this is: one synchronised
                ! count-prefix-scatter per digit, no bucket decomposition. Without it nothing can
                ! observe that the chain threaded at all -- the serial arms produce the same
                ! permutation, so a fallback that always fired would pass every test.
                dbg_sort_design = 1_int64
            end if
#endif
            if (threaded_pass) then
                continue
            else if (in_alt) then
                do j = 1_int64, nv
                    b = iand(ishft(code(j), -8 * p), 255_int64)
                    cb(off(b)) = code(j)
                    pr(off(b)) = pb(j)
                    off(b) = off(b) + 1_int64
                end do
            else
                do j = 1_int64, nv
                    b = iand(ishft(code(j), -8 * p), 255_int64)
                    cb(off(b)) = code(j)
                    pb(off(b)) = pr(j)
                    off(b) = off(b) + 1_int64
                end do
            end if
            call move_alloc(code, tmp)
            call move_alloc(cb, code)
            call move_alloc(tmp, cb)
            ! Flipped only when a pass actually EXECUTED. Above the `cycle` it would invert the
            ! parity for a pass that moved nothing, and every later pass would read the buffer that
            ! does not hold the permutation -- a wrong answer, not a slower one.
            in_alt = .not. in_alt
        end do
    end subroutine sort_radix_lsd_chain

    !> One stable pass of the multi-key chain, for a STRING key: the tier split, then a packed-prefix
    !! LSD radix over the value block, then the shared refine for the runs it could not separate.
    !!
    !! **This is the single-key path's shape, and it is here for the single-key path's reason.** The
    !! obvious alternative -- an MSD radix from byte 0, which is what this did originally -- re-gathers
    !! `offsets` and `data` at every level, two random reads per row, and needs one level per byte of
    !! shared prefix. The packed image is built once and then read SEQUENTIALLY by eight LSD passes,
    !! so only the rows agreeing on all eight prefix bytes ever pay for a gather. Worth multi3 -34%
    !! when it was measured.
    !!
    !! **Why it is still a stable sort by key `kx` alone**, which is the only thing a pass in a
    !! multi-key chain owes -- it must never consult another key or a row index, or it destroys the
    !! order the later keys have already established:
    !!
    !! * the LSD radix over the image is stable, so rows sharing an image keep their incoming order;
    !! * rows whose images differ are ordered correctly BY the image -- the first position at which
    !!   two images differ holds either two real bytes, compared as unsigned exactly as
    !!   `compare_bytes` does, or a real byte against a pad, and a pad means that string ended, which
    !!   is `compare_bytes` calling the shorter one less. `sort_radix_refine_strings` states this in
    !!   full; it is unchanged by there being other keys;
    !! * each run of equal image goes to `sort_radix_refine_run` at byte `SORT_RADIX_PREFIX` with
    !!   `one_key = .false.`, which orders it by key `kx` alone and stably.
    !!
    !! **Entering the refine at byte 8 rather than 0 does not weaken `sort_radix_candidate`'s
    !! string-length guard**, which was re-read rather than assumed when this shape landed. That
    !! guard exists because the refine tail's O(m^2) insertion is sound only while the recursion ends
    !! by running out of BYTES rather than at `SORT_RADIX_MAX_BYTE`. The cap still bounds the
    !! recursion, the termination condition is untouched, and a run reaching the tail is under
    !! `SORT_INSERTION_CUTOFF` exactly as before -- starting deeper can only end it sooner.
    !!
    !! **It allocates nothing.** The caller's `code`/`cb` carry the images and its `pb` is the second
    !! row buffer, the first being `perm`'s own value block, so images and rows ping-pong together
    !! and the copy-back happens at most once rather than once per pass.
    !!
    !! The tier pass comes FIRST, unlike the numeric passes where it comes last, and the reason
    !! survives the change of shape: the refine walks runs within a CONTIGUOUS value block, and the
    !! tier split is what makes it contiguous. A string key has no NaN tier, but
    !! `sort_radix_tier_rank` answers for it anyway and costs nothing to reuse.
    subroutine sort_radix_string_key_pass(keys, kx, n, perm, code, cb, pb, cnt, nt)
        type(sort_key_buf), intent(in) :: keys(:)  !! the keys, in precedence order.
        integer, intent(in) :: kx                  !! which key to order by; family SK_STR.
        integer(int64), intent(in) :: n            !! rows.
        integer(int64), intent(inout) :: perm(:)   !! the permutation, ordered in place.
        integer(int64), allocatable, intent(inout) :: code(:)
        !! the caller's image buffer, `n` long. ALLOCATABLE because `sort_radix_lsd_chain` swaps it
        !! with `cb` by `move_alloc` -- harmless to the caller, which reuses both as scratch across
        !! keys and never depends on which physical array is which.
        integer(int64), allocatable, intent(inout) :: cb(:) !! the caller's second image buffer.
        integer(int64), intent(inout) :: pb(:)     !! the caller's scatter buffer, `n` long.
        integer(int64), allocatable, intent(inout) :: cnt(:,:)
        !! the caller's per-thread cursors; UNALLOCATED is the signal to run serially.
        integer, intent(in) :: nt                  !! team size; 1 runs every whole-column pass serially.
        !
        integer(int64) :: hist(0:255, 0:7) !! one histogram per byte position, all built in ONE pass.
        integer(int64) :: tier(0:2)      !! rows per tier, then the tier pass's output cursor.
        integer(int64) :: j, b, t        !! row, bucket, prefix-sum accumulator.
        integer(int64) :: lo, hi, nv     !! the value block's bounds, and how many rows it holds.
        integer(int64) :: base           !! `lo - 1`, added to a 1-based index within the block.
        integer :: r                     !! tier rank.
        logical :: in_alt                !! .true. when the chain left the live rows in `pb`.
        !
        ! The tier split, stable, three buckets whose ORDER already carries this key's nulls_first.
        ! A key that cannot have a tier skips all of it: every row is then a value, so the block is
        ! the whole range whichever rank `nulls_first` gives it, and there is nothing to scatter.
        if (.not. key_has_tiers(keys(kx))) then
            lo = 1_int64
            hi = n
        else
            tier = 0_int64
            do j = 1_int64, n
                r = sort_radix_tier_rank(keys(kx), perm(j))
                tier(r) = tier(r) + 1_int64
            end do
            lo = 1_int64
            do r = 0, sort_radix_value_rank(keys(kx)) - 1
                lo = lo + tier(r)
            end do
            hi = lo + tier(sort_radix_value_rank(keys(kx))) - 1_int64
            if (maxval(tier) < n) then
                b = 1_int64
                do r = 0, 2
                    t = tier(r)
                    tier(r) = b
                    b = b + t
                end do
                do j = 1_int64, n
                    r = sort_radix_tier_rank(keys(kx), perm(j))
                    pb(tier(r)) = perm(j)
                    tier(r) = tier(r) + 1_int64
                end do
                call sort_copy_threaded(pb, perm, n, nt)
            end if
        end if
        !
        if (hi <= lo) return
        base = lo - 1_int64
        nv = hi - base
        !
        ! **Imaged and ordered IN PLACE in `perm(lo:hi)`, the value tier's own slice.** The rows used
        ! to be copied into `pb` first purely so the hand-rolled loop below could start with them
        ! there; the shared chain takes the slice directly, so that whole O(nv) copy is gone. Passing
        ! the slice costs nothing -- every dummy it reaches is plain assumed-shape, so it travels as a
        ! descriptor rather than being copied in and out.
        call sort_radix_images_threaded(keys(kx), perm(lo:hi), nv, code, nt)
        call sort_radix_hist_threaded(code, nv, hist, nt)
        call sort_radix_lsd_chain(code, cb, perm(lo:hi), pb, nv, hist, cnt, nt, in_alt)
        !
        ! `sort_radix_refine_strings` reads images in `code(1:nv)` -- always live there, because the
        ! chain ping-pongs them with `move_alloc` -- beside a stable row array in `pb(1:nv)` and the
        ! `perm(lo:hi)` it is about to permute. **The two must be different arrays**, which is why one
        ! copy is made here whichever side the chain finished on rather than only when it finished in
        ! `pb`.
        if (in_alt) then
            call sort_copy_threaded(pb, perm(lo:hi), nv, nt)
        else
            call sort_copy_threaded(perm(lo:hi), pb, nv, nt)
        end if
        call sort_radix_refine_strings(keys, kx, code, pb, nv, base, perm, .false., nt)
    end subroutine sort_radix_string_key_pass

    !> Which tier one row sits in under one key, as a rank that sorts ASCENDING: 0 first, 2 last.
    !!
    !! `value`/`NaN`/`null` is 0/1/2 by default; `nulls_first` reverses the tier order outright,
    !! which is `2 - tier` and not a special case anywhere. `descending` is deliberately absent —
    !! it reorders WITHIN the value tier and never moves a null or a NaN, which is the single rule
    !! this engine is most often got wrong.
    function sort_radix_tier_rank(key, i) result(r)
        type(sort_key_buf), intent(in) :: key !! the bound key.
        integer(int64), intent(in) :: i       !! row, 1-based.
        integer :: r                          !! 0, 1 or 2.
        !
        real(real64) :: x !! the value being tested for NaN.
        !
        r = 0
        if (allocated(key%valid)) then
            if (key%valid(i) == 0_c_int8_t) r = 2
        end if
        if (r == 0 .and. key%family == SK_REAL) then
            ! `x /= x` rather than `ieee_is_nan` -- see this submodule's header.
            x = key%reals(i)
            if (x /= x) r = 1
        end if
        if (key%nulls_first) r = 2 - r
    end function sort_radix_tier_rank

    !> The rank `sort_radix_tier_rank` gives a VALUE-tier row under one key, so a caller can ask
    !! "is this row a value?" without restating the `nulls_first` arithmetic and getting it backwards.
    function sort_radix_value_rank(key) result(r)
        type(sort_key_buf), intent(in) :: key !! the bound key.
        integer :: r                          !! the rank a value-tier row has under this key.
        !
        r = 0
        if (key%nulls_first) r = 2
    end function sort_radix_value_rank

    !> Orders the string rows the radix could not separate, using the ordinary comparator.
    !!
    !! Only `SORT_RADIX_PREFIX` bytes went into the radix, so the value block is sorted by prefix
    !! and a run sharing one image is still in file order. Two facts make this the whole fix:
    !!
    !! **Where two images DIFFER, the radix's answer is already right.** The first position at which
    !! they differ holds either two real bytes — which `compare_bytes` compares as unsigned, exactly
    !! as the packed image does — or a real byte against a pad. A pad means that string ended there,
    !! and `compare_bytes` calls the shorter string less, which is what a zero pad does. So a run
    !! boundary in `ka` is a genuine ordering boundary and each run can be finished independently.
    !!
    !! **A run whose rows all fit the window AND share one LENGTH is already finished.** Only then are
    !! they byte-identical, so the comparator answers 0 for every pair and the radix's own stability
    !! has already left them in row order. Skipping those is what keeps a column of short repeated
    !! values — a category label, a status flag — off the comparator entirely.
    !!
    !! **The length half of that test is load-bearing and is easy to drop**, because "fits the window
    !! and shares an image" reads like it already means byte-identical. It does not: the image is
    !! zero-PADDED, so `"a"` and `"a"//char(0)` produce the same 8 bytes at lengths 1 and 2, and
    !! `compare_bytes` calls the shorter one less. Dropping the test leaves such a run in file order,
    !! which is a wrong permutation with no abort and nothing to notice it — `feature_risks.md`
    !! Risk-89, and `feature_sort_radix.md` §12.2 for the reproduction.
    !! **`ra` and `perm` must be DIFFERENT arrays.** This reads the row order from one while
    !! permuting the other, so passing the same actual for both would associate one array with a
    !! defined dummy and an `intent(in)` one at once -- which F2018 15.5.2.13 forbids and which
    !! neither compiler here diagnoses. The multi-key caller normalises its buffers specifically to
    !! satisfy this.
    subroutine sort_radix_refine_strings(keys, kx, ka, ra, nv, value_base, perm, one_key, nt)
        type(sort_key_buf), intent(in) :: keys(:) !! the keys, in precedence order.
        integer, intent(in) :: kx                 !! which key was radixed; it must be SK_STR.
        integer(int64), intent(in) :: ka(:)       !! the sorted key images, `1..nv`.
        integer(int64), intent(in) :: ra(:)       !! the row indices beside them, `1..nv`.
        integer(int64), intent(in) :: nv          !! rows in the value block.
        integer(int64), intent(in) :: value_base  !! output positions before the value block.
        integer(int64), intent(inout) :: perm(:)  !! the permutation, refined in place.
        logical, intent(in) :: one_key
        !! .true. when key `kx` is the ONLY key, so a tail may use the whole ordering; .false. in a
        !! multi-key chain, where the tail must order by key `kx` alone and preserve incoming order.
        integer, intent(in) :: nt !! team size; 1 refines serially.
        !
        integer(int64) :: s, e, j !! first and last index of a run, and a cursor within it.
        integer(int64) :: ln      !! byte length of row `j`.
        integer(int64) :: len0    !! byte length of the run's first row, the one the others must match.
        integer(int64) :: minlen, maxlen !! the run's length extremes, handed to the refine.
        logical :: refine         !! .true. when the run is not already in its final order.
        integer(int64), allocatable :: buf(:)
        !! scatter scratch for the deep refine, allocated only if some run actually needs it -- so a
        !! column whose values all fit the window never pays for it at all.
        integer(int64), allocatable :: rlo(:), rhi(:), rmin(:), rmax(:)
        !! the runs that need refining, collected before any of them is refined.
        integer(int64) :: nrun, r, cap
        integer :: ios
        !
        if (nv < 2_int64) return
        !
        ! **The runs are COLLECTED first, then refined.** They are disjoint ranges of `perm`, so
        ! refining them is embarrassingly parallel -- but the boundary scan that finds them is a
        ! sequential walk, and interleaving the two would serialise the whole thing behind it. The
        ! two-phase shape is what lets the second phase open a team at all.
        !
        ! Measured before this existed, on a fixture whose values share a leading prefix (which is
        ! what real string columns do -- urls, ids, dates as text): a 5e6-row string sort scaled
        ! **1.16x from 1 to 64 threads**, against 2.94x for the same column with no shared prefix.
        ! The refine was the whole difference and none of it was threaded.
        nrun = 0_int64
        cap = 0_int64
        s = 1_int64
        do while (s < nv)
            e = s
            do while (e < nv)
                if (ka(e + 1_int64) /= ka(s)) exit
                e = e + 1_int64
            end do
            if (e > s) then
                ! One scan answers both questions: whether this run needs refining at all, and the
                ! length extremes the refine then carries down instead of rescanning per level.
                len0 = keys(kx)%offsets(ra(s) + 1_int64) - keys(kx)%offsets(ra(s))
                minlen = len0
                maxlen = len0
                refine = .false.
                do j = s, e
                    ln = keys(kx)%offsets(ra(j) + 1_int64) - keys(kx)%offsets(ra(j))
                    if (ln < minlen) minlen = ln
                    if (ln > maxlen) maxlen = ln
                    ! Past the window: bytes the radix never saw may still separate these rows.
                    ! A differing length: same image, different string -- see the note above.
                    if (ln > SORT_RADIX_PREFIX .or. ln /= len0) refine = .true.
                end do
                if (refine) then
                    ! **The work-list is built ONLY when a team will use it.** Collecting it costs a
                    ! growing allocation and a store per run, and on a fixture with many small runs
                    ! that is not free: measured at **+47%** on a 5e6-row column with ~457k runs when
                    ! the list was built unconditionally. A serial refine has nothing to gain from
                    ! the list, so it refines each run where it finds it, exactly as before.
                    if (nt <= 1) then
                        call sort_radix_refine_run(keys, kx, value_base + s, value_base + e, &
                            SORT_RADIX_PREFIX, minlen, maxlen, perm, buf, one_key, 1)
                        s = e + 1_int64
                        cycle
                    end if
                    if (nrun >= cap) then
                        call grow_run_list(rlo, rhi, rmin, rmax, cap)
                        if (cap == 0_int64) then
                            ! Out of memory collecting the list. Refine this run immediately and
                            ! carry on serially: running out of scratch is a reason to be slower,
                            ! never a reason to fail or to answer differently.
                            call sort_radix_refine_run(keys, kx, value_base + s, value_base + e, &
                                SORT_RADIX_PREFIX, minlen, maxlen, perm, buf, one_key, 1)
                            s = e + 1_int64
                            cycle
                        end if
                    end if
                    nrun = nrun + 1_int64
                    rlo(nrun) = value_base + s
                    rhi(nrun) = value_base + e
                    rmin(nrun) = minlen
                    rmax(nrun) = maxlen
                end if
            end if
            s = e + 1_int64
        end do
        if (nrun < 1_int64) return
        !
        ! **Threaded over runs only when there are enough of them to fill the team.** With fewer,
        ! the team would sit mostly idle here AND `sort_radix_refine_run` would then decline to
        ! thread its own sub-bucket loop, because it refuses to nest -- so the two levels would
        ! cancel out and the biggest run, the one that matters, would be refined by one thread. The
        ! serial arm below leaves that decision to the run itself, which is where the work is.
        ! `2 * nt` for `schedule(dynamic)`'s sake: run sizes here vary by orders of magnitude.
#ifdef _OPENMP
        if (nt > 1 .and. nrun >= 2_int64 * int(nt, int64)) then
            ! Allocated HERE, before the region: `sort_radix_refine_run` allocates it lazily on
            ! first need, and several threads reaching that at once is a race on an allocatable.
            allocate(buf(size(perm, kind=int64)), stat=ios)
            if (ios == 0 .and. dbg_sort_radix_fail_alloc /= 2) then
                dbg_sort_refine_runs = dbg_sort_refine_runs + nrun
                !$omp parallel do num_threads(nt) default(shared) private(r) schedule(dynamic)
                do r = 1_int64, nrun
                    call sort_radix_refine_run(keys, kx, rlo(r), rhi(r), &
                        SORT_RADIX_PREFIX, rmin(r), rmax(r), perm, buf, one_key, 1)
                end do
                !$omp end parallel do
                return
            end if
            if (allocated(buf)) deallocate(buf)
        end if
#endif
        do r = 1_int64, nrun
            call sort_radix_refine_run(keys, kx, rlo(r), rhi(r), &
                SORT_RADIX_PREFIX, rmin(r), rmax(r), perm, buf, one_key, nt)
        end do
    end subroutine sort_radix_refine_strings

    !> Doubles the refine work-list, or reports failure by leaving `cap` at zero.
    !!
    !! **Four allocations in series, so `dbg_sort_radix_fail_alloc` gives each its own selector**
    !! (5, 6, 7, 8, in the order they appear below) for the reason that hook's own doc-comment
    !! records: with one flag the first failure returns before the later ones are reached, so
    !! three of the four fallbacks would ship untested. Failing any of them leaves `cap` at zero,
    !! whereupon the caller refines that run serially and carries on -- the answer is unchanged,
    !! which is why a permutation assertion alone cannot tell this path apart from the ordinary one.
    subroutine grow_run_list(rlo, rhi, rmin, rmax, cap)
        integer(int64), allocatable, intent(inout) :: rlo(:), rhi(:), rmin(:), rmax(:)
        integer(int64), intent(inout) :: cap !! capacity in, new capacity out; 0 means it could not grow.
        integer(int64), allocatable :: t(:)
        integer(int64) :: newcap
        integer :: ios
        !
        newcap = max(1024_int64, 2_int64 * cap)
        allocate(t(newcap), stat=ios)
        if (ios /= 0 .or. dbg_sort_radix_fail_alloc == 5) then
            cap = 0_int64
            return
        end if
        if (cap > 0_int64) t(1:cap) = rlo(1:cap)
        call move_alloc(t, rlo)
        allocate(t(newcap), stat=ios)
        if (ios /= 0 .or. dbg_sort_radix_fail_alloc == 6) then
            cap = 0_int64
            return
        end if
        if (cap > 0_int64) t(1:cap) = rhi(1:cap)
        call move_alloc(t, rhi)
        allocate(t(newcap), stat=ios)
        if (ios /= 0 .or. dbg_sort_radix_fail_alloc == 7) then
            cap = 0_int64
            return
        end if
        if (cap > 0_int64) t(1:cap) = rmin(1:cap)
        call move_alloc(t, rmin)
        allocate(t(newcap), stat=ios)
        if (ios /= 0 .or. dbg_sort_radix_fail_alloc == 8) then
            cap = 0_int64
            return
        end if
        if (cap > 0_int64) t(1:cap) = rmax(1:cap)
        call move_alloc(t, rmax)
        cap = newcap
    end subroutine grow_run_list

    !> Orders one tied run by continuing the radix a byte at a time, recursing on each new run.
    !!
    !! `perm(lo:hi)` all agree on bytes `[0, at)` -- under the PADDING convention, so a row shorter
    !! than `at` agrees by having NUL where a longer one has a real byte. This is MSD rather than
    !! LSD, which is what lets it subdivide: one counting pass over byte `at` splits the run into
    !! buckets that are already in final order relative to each other, and each bucket is then the
    !! same problem one byte deeper.
    !!
    !! **Why byte `at` orders the buckets correctly** is the sound half of the argument in the
    !! caller, one level down: two rows differing at byte `at` hold either two real bytes, which
    !! `compare_bytes` compares as unsigned exactly as the bucket index does, or a real byte against
    !! a row that has ended -- and a row that has ended pads to 0, which is the lowest bucket, which
    !! is `compare_bytes` calling the shorter string less.
    !!
    !! Three ways out, and each is there for a different reason:
    !!
    !! * **`at >= maxlen` with every length equal: the rows are byte-identical.** Stability has
    !!   already left them in row order, which is the whole answer. This is the case that makes an
    !!   all-equal string column O(n) instead of O(n log n) -- the one worst case the radix path had.
    !! * **`at >= maxlen` with lengths differing, or a run at the depth cap: hand it to the
    !!   introsort.** `sort_row_less` is the whole ordering, so it settles remaining bytes, lengths
    !!   and the index tiebreaker together and knows nothing about how the run was reached. Length
    !!   ordering is deliberately NOT reimplemented here: it would be a fourth expression of the
    !!   ordering, and `descending` would have to be applied to it by hand.
    !! * **A short run: hand it to the introsort too**, because 256 counters cost more than sorting
    !!   a handful of rows.
    !!
    !! The depth cap is what bounds the recursion. Depth grows with the common prefix length, which
    !! is caller data and therefore unbounded; each frame holds a 2 KB counter array, so an
    !! uncapped recursion on pathological input would exhaust the stack. Past the cap the introsort
    !! finishes the job correctly, just without the radix's help.
    !! Two shortcuts keep the all-equal case cheap, and both mirror something the main radix already
    !! does. **A byte every row agrees on cannot reorder anything**, so a single non-empty bucket
    !! recurses without scattering or copying — the same reasoning as the main pass-skip test, one
    !! level down. And **the run's length extremes are carried down rather than rescanned**, which
    !! is exact rather than approximate: a run that did not split has precisely the rows its parent
    !! had, so its extremes are its parent's. Together these take a column of identical long strings
    !! from four passes per byte to one.
    recursive subroutine sort_radix_refine_run(keys, kx, lo, hi, at_in, minlen, maxlen, perm, buf, &
            one_key, nt)
        type(sort_key_buf), intent(in) :: keys(:)   !! the keys, in precedence order.
        integer, intent(in) :: kx                   !! which key to order by; it must be SK_STR.
        integer(int64), intent(in) :: lo, hi        !! the run, as absolute `perm` positions.
        integer(int64), intent(in) :: at_in         !! byte position to split on, 0-based.
        integer(int64), intent(in) :: minlen, maxlen !! this run's own byte-length extremes.
        integer(int64), intent(inout) :: perm(:)    !! the permutation, refined in place.
        integer(int64), allocatable, intent(inout) :: buf(:)
        !! scatter scratch, allocated on first need and reused by every run below it.
        logical, intent(in) :: one_key
        !! .true. when key `kx` is the ONLY key, so a tail may use the whole ordering; .false. in a
        !! multi-key chain, where the tail must order by key `kx` alone and preserve incoming order.
        integer, intent(in) :: nt
        !! team size this run may use for its OWN sub-bucket loop. `sort_radix_refine_strings` passes
        !! 1 when it has already opened a team over runs, because the two levels must not nest.
        !
        integer(int64) :: cnt(0:255) !! rows per bucket, then the running output cursor.
        integer(int64) :: blo(0:255) !! each bucket's first slot, so the loop below needs no running cursor.
        integer(int64) :: i, t, b, at !! walk index, prefix-sum accumulator, bucket, byte position.
        integer(int64) :: sub_lo, sub_min, sub_max !! one child run and its own extremes.
        integer(int64) :: ln         !! byte length of one row, while scanning a child's extremes.
        integer :: ios               !! allocation status; a failure just means the introsort finishes.
        !
        if (hi <= lo) return
        at = at_in
        !
        ! Descend through bytes that separate nothing ITERATIVELY rather than by recursing. This is
        ! the common way to get deep -- a shared prefix is exactly a run of such bytes -- so making
        ! it a loop is what keeps recursion depth proportional to how often the run actually SPLITS
        ! rather than to how long the values are.
        do
            ! Every real byte is behind us: only length can separate these rows now.
            if (at >= maxlen) then
                if (minlen == maxlen) return
                call sort_radix_refine_tail(keys, kx, lo, hi, perm, one_key)
                return
            end if
            if (hi - lo < SORT_INSERTION_CUTOFF .or. at >= SORT_RADIX_MAX_BYTE) then
                call sort_radix_refine_tail(keys, kx, lo, hi, perm, one_key)
                return
            end if
            cnt = 0_int64
            do i = lo, hi
                b = sort_radix_byte_at(keys(kx), perm(i), at)
                cnt(b) = cnt(b) + 1_int64
            end do
            if (maxval(cnt) < hi - lo + 1_int64) exit
            at = at + 1_int64
        end do
        !
        if (.not. allocated(buf)) then
            allocate(buf(size(perm, kind=int64)), stat=ios)
            ! Same rule as the main path: running out of scratch is a reason to be slower, never a
            ! reason to fail. The introsort needs none. The debug hook covers BOTH allocations, so
            ! that one scenario reaches both fallbacks rather than leaving this one untested.
            if (ios /= 0 .or. dbg_sort_radix_fail_alloc == 2) then
                call sort_radix_refine_tail(keys, kx, lo, hi, perm, one_key)
                return
            end if
        end if
        t = lo
        do b = 0_int64, 255_int64
            i = cnt(b)
            cnt(b) = t
            t = t + i
        end do
        ! Stable by construction, which is what keeps the row-index tiebreaker satisfied without
        ! ever consulting it: equal bytes leave in the order they arrived.
        do i = lo, hi
            b = sort_radix_byte_at(keys(kx), perm(i), at)
            buf(cnt(b)) = perm(i)
            cnt(b) = cnt(b) + 1_int64
        end do
        perm(lo:hi) = buf(lo:hi)
        !
        ! `cnt(b)` is now one PAST that bucket's last slot, so the bucket is [previous end, cnt(b)).
        ! The run really split here, so each child's extremes are rescanned -- summed over the
        ! buckets that is one pass over the run, i.e. exactly what scanning in the child would cost.
        !
        ! **Each bucket's start is derived from the cursors, not carried in a running `t`.** The
        ! serial walk could keep one; the threaded loop below cannot, because bucket `b`'s first slot
        ! has to be computable without having visited `b - 1`. `blo` is that, and it costs one extra
        ! 256-entry pass.
        t = lo
        do b = 0_int64, 255_int64
            blo(b) = t
            if (cnt(b) > t) t = cnt(b)
        end do
        !
        ! **The sub-buckets are disjoint ranges of `perm`, and of `buf`** -- `buf` is indexed by
        ! absolute position, so two threads refining different buckets can never touch the same slot.
        ! That is what makes this safe with one shared scratch array rather than one per thread.
        !
        ! Threaded only when this run is the thing worth threading: a column whose whole radix prefix
        ! is shared collapses into ONE run, so `sort_radix_refine_strings` cannot thread over runs at
        ! all and this loop is the only parallelism available. `omp_in_parallel` is not consulted --
        ! the caller says so directly by passing `nt = 1`, which is precise where the ambient test
        ! would also fire for a caller that merely happens to be inside someone else's region.
#ifdef _OPENMP
        if (nt > 1 .and. hi - lo + 1_int64 >= SORT_REFINE_MIN_ROWS * int(nt, int64)) then
            dbg_sort_refine_runs = dbg_sort_refine_runs + 1_int64
            !$omp parallel do num_threads(nt) default(shared) private(b, i, sub_lo, sub_min, sub_max, ln) &
            !$omp schedule(dynamic)
            do b = 0_int64, 255_int64
                if (cnt(b) > blo(b)) then
                    sub_lo = blo(b)
                    sub_min = huge(0_int64)
                    sub_max = 0_int64
                    do i = sub_lo, cnt(b) - 1_int64
                        ln = keys(kx)%offsets(perm(i) + 1_int64) - keys(kx)%offsets(perm(i))
                        if (ln < sub_min) sub_min = ln
                        if (ln > sub_max) sub_max = ln
                    end do
                    call sort_radix_refine_run(keys, kx, sub_lo, cnt(b) - 1_int64, at + 1_int64, &
                        sub_min, sub_max, perm, buf, one_key, 1)
                end if
            end do
            !$omp end parallel do
            return
        end if
#endif
        do b = 0_int64, 255_int64
            if (cnt(b) > blo(b)) then
                sub_lo = blo(b)
                sub_min = huge(0_int64)
                sub_max = 0_int64
                do i = sub_lo, cnt(b) - 1_int64
                    ln = keys(kx)%offsets(perm(i) + 1_int64) - keys(kx)%offsets(perm(i))
                    if (ln < sub_min) sub_min = ln
                    if (ln > sub_max) sub_max = ln
                end do
                call sort_radix_refine_run(keys, kx, sub_lo, cnt(b) - 1_int64, at + 1_int64, &
                    sub_min, sub_max, perm, buf, one_key, nt)
            end if
        end do
    end subroutine sort_radix_refine_run

    !> Finishes one run the radix gave up on. Split out so that the ways it gives up read as one
    !! decision rather than several copies of the same call.
    !!
    !! **Which sort is correct here depends on how many keys there are, and that is the whole
    !! reason this takes `one_key`.** With a single key, "order by that key, then by row index" IS
    !! the whole ordering, so the ordinary introsort over `sort_row_less` is exactly right and is
    !! also the fastest thing available. Inside a multi-key chain it would be flatly wrong: it would
    !! order the run by key 1 and the row index, destroying the order the keys AFTER `kx` have
    !! already established, which is the order this pass is obliged to preserve. There the run must
    !! be sorted by key `kx` alone, **stably** — and `sort_row_less` cannot express that, since it
    !! compares every key and then the index.
    subroutine sort_radix_refine_tail(keys, kx, lo, hi, perm, one_key)
        type(sort_key_buf), intent(in) :: keys(:) !! the keys, in precedence order.
        integer, intent(in) :: kx                 !! which key to order by, when `one_key` is false.
        integer(int64), intent(in) :: lo, hi      !! the run, as absolute `perm` positions.
        integer(int64), intent(inout) :: perm(:)  !! the permutation, refined in place.
        logical, intent(in) :: one_key            !! .true. when the whole ordering may be used.
        !
        integer(int64) :: i, j, v !! insertion cursor, shift cursor, the element being placed.
        !
        if (hi <= lo) return
        if (one_key) then
            call sort_introsort_loop(keys, perm, lo, hi, 2 * sort_ilog2(hi - lo + 1_int64))
            call sort_insertion(keys, perm, lo, hi)
            return
        end if
        !
        ! Stable insertion by key `kx` alone. Insertion sort is stable precisely because the shift
        ! condition is STRICT: an element equal to the one being placed stops the walk, so equal
        ! rows keep the order they arrived in. Weakening it to `>=` would silently reverse ties and
        ! break the whole multi-key composition -- and every fixture without ties would still pass.
        !
        ! O(m^2), which is only sound because m is small: `sort_radix_candidate` refuses a multi-key
        ! sort whose string key is longer than `SORT_RADIX_MAX_BYTE`, so the recursion above always
        ! ends by running out of BYTES rather than by hitting its depth cap, and a run reaching here
        ! is therefore under `SORT_INSERTION_CUTOFF`.
        do i = lo + 1_int64, hi
            v = perm(i)
            j = i - 1_int64
            do while (j >= lo)
                if (compare_bytes(keys(kx), perm(j), v) * sort_radix_dir(keys(kx)) <= 0) exit
                perm(j + 1_int64) = perm(j)
                j = j - 1_int64
            end do
            perm(j + 1_int64) = v
        end do
    end subroutine sort_radix_refine_tail

    !> +1 for an ascending key, -1 for a descending one, so a raw `compare_bytes` result can be
    !! turned into this key's own order by one multiplication rather than a branch per comparison.
    function sort_radix_dir(key) result(d)
        type(sort_key_buf), intent(in) :: key !! the bound key.
        integer :: d                          !! +1 ascending, -1 descending.
        !
        d = 1
        if (key%descending) d = -1
    end function sort_radix_dir

    !> Byte `at` of one string row, 0-based, as an unsigned 0..255 -- or the PAD when the row has
    !! ended, which is what makes a shorter string sort before a longer one that extends it.
    !!
    !! `descending` complements the byte, exactly as `sort_radix_images_range` complements the whole
    !! 64-bit image and for the same reason: complementing reverses the value order while leaving
    !! stability intact, where reversing the output would put ties backwards.
    function sort_radix_byte_at(key, i, at) result(b)
        type(sort_key_buf), intent(in) :: key !! the bound key, of family SK_STR.
        integer(int64), intent(in) :: i       !! row, 1-based.
        integer(int64), intent(in) :: at      !! byte position, 0-based.
        integer(int64) :: b                   !! the bucket, 0..255.
        !
        integer(int64) :: ln !! byte length of row `i`.
        !
        ln = key%offsets(i + 1_int64) - key%offsets(i)
        if (at >= ln) then
            b = 0_int64
        else
            b = int(iand(iachar(key%data(key%offsets(i) + 1_int64 + at)), 255), int64)
        end if
        if (key%descending) b = 255_int64 - b
    end function sort_radix_byte_at

    ! ---- The serial sort: introsort over sort_row_less ------------------------------------------
    !
    ! The same algorithm `std::sort` is, deliberately: quicksort with median-of-three pivoting, a
    ! depth-limited heapsort fallback, and one final insertion pass over the whole range. Keeping the
    ! shape identical is what makes "same answer, different language" a fair comparison and what keeps
    ! the adversarial-input behaviour equivalent -- a plain quicksort here would be an O(n^2) trapdoor
    ! reachable from ordinary user data (an already-sorted column, an all-equal one, an organ pipe).
    !
    ! **Three-way (Bentley-McIlroy) partitioning is NOT wanted here, and a reviewer will suggest it**
    ! the moment they see how tie-heavy real key data is. Under `sort_row_less` there are no equal
    ! elements to collect -- the row-index tiebreaker makes it a total order -- so a middle partition
    ! would always be exactly one element wide and the extra branch would be pure cost.
    !
    ! Every comparison goes through `sort_row_less` and there is no second ordering anywhere in this
    ! section. That is Risk-34 again: an "optimised" inline comparison here would be a third copy of
    ! the decision, and the conformance tests compare permutations, so a drifting copy would show up
    ! only on the fixtures that happen to exercise it.
    !
    ! ---- THE FINAL INSERTION PASS MASKS EVERY ORDERING DEFECT ABOVE IT --------------------------
    !
    ! `sort_insertion` is a complete sort, so whatever `sort_introsort_loop` leaves behind -- however
    ! wrong -- comes out correctly ordered. That makes the sort robust and makes it hard to test: a
    ! defect above the insertion pass is a PERFORMANCE defect, not a wrong answer, and no assertion
    ! on the permutation can see it. Mutation testing confirmed this rather than predicting it, and
    ! the two survivors are worth knowing apart:
    !
    !   * **A sift-down with its comparison inverted** -- i.e. a completely broken heapsort -- passed
    !     every conformance test. It is now caught, by the invariant below.
    !   * **A partition returning `cut = i + 1`** passes everything and is expected to keep passing.
    !     It is not detectable by any correctness-shaped test, because it is correct: it leaves one
    !     element per partition slightly too far RIGHT, and an insertion pass repairs that by
    !     shifting each smaller successor left by exactly one. Total extra work is O(1) amortised per
    !     partition, so it is a near-no-op rather than a lurking quadratic. Do not add a contorted
    !     test for it.
    !
    ! What a broken heapsort cannot fake is the invariant the quicksort exists to establish -- **no
    ! element more than SORT_INSERTION_CUTOFF positions LEFT of where it belongs** -- so that is what
    ! is asserted, through `dbg_sort_track_shift`/`dbg_sort_max_shift` and
    ! `test_fortran_engine_presort_invariant`. Note the one-sidedness: an insertion pass only ever
    ! moves an element leftward, so this sees an element left too far right only when some later
    ! element has to travel back past it. Anything added to this section that changes where elements
    ! end up before the insertion pass must be mutation-tested against BOTH the conformance tests and
    ! that invariant; neither alone is sufficient.

    module procedure sort_comparison_permutation
        integer(int64) :: k    !! fill index.
        integer :: depth       !! remaining quicksort depth before the heapsort fallback.
        !
        do k = 1_int64, n
            perm(k) = k
        end do
        if (n < 2_int64) return
        !
        ! 2*floor(log2(n)) is std::sort's own limit. It is a PERFORMANCE guard, not a correctness
        ! one -- the fallback sorts just as correctly -- so a mutation to this expression cannot be
        ! caught by asserting the answer, which is why `dbg_sort_depth_limit` exists.
        depth = 2 * sort_ilog2(n)
        if (dbg_sort_depth_limit >= 0) depth = dbg_sort_depth_limit
        call sort_introsort_loop(keys, perm, 1_int64, n, depth)
        !
        ! ONE insertion pass over the whole range, not one per chunk: `sort_introsort_loop` leaves
        ! every element within SORT_INSERTION_CUTOFF of its final position and leaves the chunks in
        ! order relative to each other, so this is O(n * cutoff) rather than O(n^2).
        call sort_insertion(keys, perm, 1_int64, n)
    end procedure sort_comparison_permutation

    !> `floor(log2(n))` for `n >= 1`, by shifting — there is no integer-log intrinsic.
    pure function sort_ilog2(n) result(k)
        integer(int64), intent(in) :: n !! the value; must be positive.
        integer :: k                    !! floor(log2(n)); 0 for n = 1.
        !
        integer(int64) :: m !! the value being shifted down.
        !
        k = 0
        m = n
        do while (m > 1_int64)
            m = ishft(m, -1)
            k = k + 1
        end do
    end function sort_ilog2

    !> Quicksorts `perm(lo:hi)` down to `SORT_INSERTION_CUTOFF`-sized chunks, or heapsorts on depth 0.
    !!
    !! Recurses on the RIGHT partition and loops on the left, which is what `std::sort` does. Depth is
    !! bounded by the limit the caller passed, so the recursion cannot outgrow the stack.
    recursive subroutine sort_introsort_loop(keys, perm, lo_in, hi_in, depth_in)
        type(sort_key_buf), intent(in) :: keys(:) !! the keys, in precedence order.
        integer(int64), intent(inout) :: perm(:)  !! the permutation being ordered, in place.
        integer(int64), intent(in) :: lo_in       !! first index of the range, 1-based.
        integer(int64), intent(in) :: hi_in       !! last index of the range, inclusive.
        integer, intent(in) :: depth_in           !! remaining depth before the heapsort fallback.
        !
        integer(int64) :: lo, hi !! the range currently being worked on.
        integer(int64) :: cut    !! first index of the right partition.
        integer :: depth         !! remaining depth.
        !
        lo = lo_in
        hi = hi_in
        depth = depth_in
        do while (hi - lo + 1_int64 > SORT_INSERTION_CUTOFF)
            if (depth == 0) then
                call sort_heapsort(keys, perm, lo, hi)
                return
            end if
            depth = depth - 1
            cut = sort_partition(keys, perm, lo, hi)
            call sort_introsort_loop(keys, perm, cut, hi, depth)
            hi = cut - 1_int64
        end do
    end subroutine sort_introsort_loop

    !> Partitions `perm(lo:hi)` about a median-of-three pivot; returns the right partition's first index.
    !!
    !! The pivot is moved to `perm(lo)` and **stays there** — it is not placed at its final position,
    !! which is why the caller's left partition is `[lo, cut-1]` (the pivot included) rather than
    !! `[lo, cut-2]`. That is `std::sort`'s own arrangement and the following insertion pass fixes the
    !! pivot's position along with everything else.
    !!
    !! The two inner scans are deliberately **unguarded** — neither tests its index against the range
    !! bound. What stops them running off the end is the median-of-three: after it, `perm(lo)` is
    !! neither the largest nor the smallest of `{perm(lo+1), perm(mid), perm(hi)}`, so an element that
    !! stops each scan is guaranteed to exist. Change the pivot selection and this stops being true.
    !!
    !! `i >= j` and `i > j` are the same test here, and only because the comparator is a total order:
    !! `i == j` would mean `perm(i)` is neither less than nor greater than the pivot, i.e. equal to
    !! it, which no row other than the pivot itself can be — and the pivot sits at `lo`, outside the
    !! scanned range. `>=` is kept because that is what `std::sort` writes.
    function sort_partition(keys, perm, lo, hi) result(cut)
        type(sort_key_buf), intent(in) :: keys(:) !! the keys, in precedence order.
        integer(int64), intent(inout) :: perm(:)  !! the permutation being ordered, in place.
        integer(int64), intent(in) :: lo          !! first index of the range, 1-based.
        integer(int64), intent(in) :: hi          !! last index of the range, inclusive.
        integer(int64) :: cut                     !! first index of the right partition.
        !
        integer(int64) :: i, j   !! the two scan cursors.
        integer(int64) :: pivot  !! the pivot ROW, cached: `perm(lo)` is never swapped below.
        integer(int64) :: tmp    !! swap temporary.
        !
        call sort_median_to_first(keys, perm, lo, hi)
        pivot = perm(lo)
        i = lo + 1_int64
        j = hi + 1_int64
        do
            do while (sort_row_less(keys, perm(i), pivot))
                i = i + 1_int64
            end do
            j = j - 1_int64
            do while (sort_row_less(keys, pivot, perm(j)))
                j = j - 1_int64
            end do
            if (i >= j) exit
            tmp = perm(i)
            perm(i) = perm(j)
            perm(j) = tmp
            i = i + 1_int64
        end do
        cut = i
    end function sort_partition

    !> Puts the median of `perm(lo+1)`, `perm(mid)` and `perm(hi)` into `perm(lo)`.
    !!
    !! `std::sort`'s `__move_median_to_first`, branch for branch. Only the ordering of the three
    !! candidates matters, so this costs at most three comparisons and no allocation.
    subroutine sort_median_to_first(keys, perm, lo, hi)
        type(sort_key_buf), intent(in) :: keys(:) !! the keys, in precedence order.
        integer(int64), intent(inout) :: perm(:)  !! the permutation being ordered, in place.
        integer(int64), intent(in) :: lo          !! first index of the range, 1-based.
        integer(int64), intent(in) :: hi          !! last index of the range, inclusive.
        !
        integer(int64) :: a, b, c !! the three candidate positions.
        !
        a = lo + 1_int64
        b = lo + (hi - lo + 1_int64) / 2_int64
        c = hi
        if (sort_row_less(keys, perm(a), perm(b))) then
            if (sort_row_less(keys, perm(b), perm(c))) then
                call sort_swap(perm, lo, b)
            else if (sort_row_less(keys, perm(a), perm(c))) then
                call sort_swap(perm, lo, c)
            else
                call sort_swap(perm, lo, a)
            end if
        else if (sort_row_less(keys, perm(a), perm(c))) then
            call sort_swap(perm, lo, a)
        else if (sort_row_less(keys, perm(b), perm(c))) then
            call sort_swap(perm, lo, c)
        else
            call sort_swap(perm, lo, b)
        end if
    end subroutine sort_median_to_first

    !> Exchanges two entries of the permutation.
    subroutine sort_swap(perm, i, j)
        integer(int64), intent(inout) :: perm(:) !! the permutation.
        integer(int64), intent(in) :: i          !! first position.
        integer(int64), intent(in) :: j          !! second position.
        !
        integer(int64) :: tmp !! swap temporary.
        !
        tmp = perm(i)
        perm(i) = perm(j)
        perm(j) = tmp
    end subroutine sort_swap

    !> Insertion-sorts `perm(lo:hi)`.
    !!
    !! Guarded (it tests `j >= lo`) rather than `std::sort`'s unguarded variant, which relies on the
    !! first chunk already being sorted to act as a sentinel. The guard costs one comparison per
    !! shifted element and removes a precondition that a future change to the loop above could break
    !! silently.
    subroutine sort_insertion(keys, perm, lo, hi)
        type(sort_key_buf), intent(in) :: keys(:) !! the keys, in precedence order.
        integer(int64), intent(inout) :: perm(:)  !! the permutation being ordered, in place.
        integer(int64), intent(in) :: lo          !! first index of the range, 1-based.
        integer(int64), intent(in) :: hi          !! last index of the range, inclusive.
        !
        integer(int64) :: i, j !! the outer and inner cursors.
        integer(int64) :: v    !! the row being placed.
        !
        do i = lo + 1_int64, hi
            v = perm(i)
            j = i - 1_int64
            do while (j >= lo)
                if (.not. sort_row_less(keys, v, perm(j))) exit
                perm(j + 1_int64) = perm(j)
                j = j - 1_int64
            end do
            perm(j + 1_int64) = v
            ! Armed only by a test -- see `dbg_sort_track_shift` in src/parquet_sorting.f90 for what
            ! it is for. Per ELEMENT, not per shift, so it cannot show up in a profile.
            if (dbg_sort_track_shift) then
                if (i - 1_int64 - j > dbg_sort_max_shift) dbg_sort_max_shift = i - 1_int64 - j
            end if
        end do
    end subroutine sort_insertion

    !> Heapsorts `perm(lo:hi)` — the introsort's depth-limit fallback.
    !!
    !! Reached only when the quicksort recursion hits its depth limit, i.e. on input adversarial
    !! enough to have degenerated. `std::sort` uses `partial_sort` (make_heap + sort_heap) here; this
    !! is the same algorithm written out. It is O(n log n) unconditionally, which is the whole point.
    subroutine sort_heapsort(keys, perm, lo, hi)
        type(sort_key_buf), intent(in) :: keys(:) !! the keys, in precedence order.
        integer(int64), intent(inout) :: perm(:)  !! the permutation being ordered, in place.
        integer(int64), intent(in) :: lo          !! first index of the range, 1-based.
        integer(int64), intent(in) :: hi          !! last index of the range, inclusive.
        !
        integer(int64) :: n !! elements in the range.
        integer(int64) :: k !! heap-building index, then the shrinking heap size.
        !
        n = hi - lo + 1_int64
        if (n < 2_int64) return
        ! Test-only, and the ONLY observable that separates this path from the quicksort one -- they
        ! answer identically by construction. See `dbg_sort_heapsort_calls` in src/parquet_sorting.f90.
        dbg_sort_heapsort_calls = dbg_sort_heapsort_calls + 1_int64
        do k = n / 2_int64, 1_int64, -1_int64
            call sort_sift_down(keys, perm, lo, k, n)
        end do
        do k = n, 2_int64, -1_int64
            call sort_swap(perm, lo, lo + k - 1_int64)
            call sort_sift_down(keys, perm, lo, 1_int64, k - 1_int64)
        end do
    end subroutine sort_heapsort

    !> Restores the max-heap property at heap position `start`, within a heap of `count` elements.
    !!
    !! Heap positions are 1-based within the range, so heap position `p` is `perm(lo + p - 1)`.
    subroutine sort_sift_down(keys, perm, lo, start, count)
        type(sort_key_buf), intent(in) :: keys(:) !! the keys, in precedence order.
        integer(int64), intent(inout) :: perm(:)  !! the permutation being ordered, in place.
        integer(int64), intent(in) :: lo          !! first index of the range, 1-based.
        integer(int64), intent(in) :: start       !! heap position to sift from, 1-based.
        integer(int64), intent(in) :: count       !! heap size, in elements.
        !
        integer(int64) :: root, child !! heap positions, 1-based.
        !
        root = start
        do
            child = 2_int64 * root
            if (child > count) exit
            if (child < count) then
                if (sort_row_less(keys, perm(lo + child - 1_int64), perm(lo + child))) child = child + 1_int64
            end if
            if (.not. sort_row_less(keys, perm(lo + root - 1_int64), perm(lo + child - 1_int64))) exit
            call sort_swap(perm, lo + root - 1_int64, lo + child - 1_int64)
            root = child
        end do
    end subroutine sort_sift_down

    ! ---- The operations that are not a full sort (feature_sort.md Stage 5) ---------------------
    !
    ! Six operations that answer something other than "order every row". Not one of them writes a
    ! comparison out again: each routes through `sort_row_less` or `sort_keys_compare`, so the
    ! ordering rule keeps appearing exactly once (feature_risks.md Risk-34).
    !
    ! **Which of the two comparators an operation takes is a correctness decision, not a style one.**
    ! `sort_row_less` carries the row-index tiebreaker and is for ORDERING -- partial selection and
    ! nth. `sort_keys_compare` does not, and is for everything that has to recognise "these two rows
    ! are EQUAL": is_sorted, the run flags, binary search and merge. Under the tiebreaker no two
    ! distinct rows are ever equal, so using it in those four would silently answer a different
    ! question -- run detection would report every row as its own group, and a search would never
    ! find a match. The C++ engine draws the same line for the same reason.

    !> The first `count` entries of the sorted permutation, by heap selection.
    !!
    !! `std::partial_sort`'s algorithm, which is what the C++ engine calls: build a max-heap of the
    !! first `count` rows, then walk the rest replacing the root whenever a row beats it, then sort
    !! the heap. **Not a full sort truncated** -- that distinction is asserted by a test counting
    !! comparisons, so a "simplification" to sort-then-take would be caught.
    !!
    !! The heap phases are written out rather than reusing `sort_heapsort`, which builds AND sorts in
    !! one call and would also bump `dbg_sort_heapsort_calls` -- an observable that belongs to the
    !! introsort's depth fallback and would stop meaning that.
    module procedure sort_partial_permutation
        integer(int64), allocatable :: heap(:) !! the `count` best rows so far, as a max-heap.
        integer(int64) :: i, k                 !! scan cursor, then heap index.
        !
        if (count < 1_int64 .or. n < 1_int64) return
        allocate(heap(count))
        do k = 1_int64, count
            heap(k) = k
        end do
        do k = count / 2_int64, 1_int64, -1_int64
            call sort_sift_down(keys, heap, 1_int64, k, count)
        end do
        ! The root is the WORST of the current best `count`, so one comparison per remaining row is
        ! enough to reject it. That is what makes this O(n + count log count) rather than O(n log n).
        do i = count + 1_int64, n
            if (sort_row_less(keys, i, heap(1))) then
                heap(1) = i
                call sort_sift_down(keys, heap, 1_int64, 1_int64, count)
            end if
        end do
        do k = count, 2_int64, -1_int64
            call sort_swap(heap, 1_int64, k)
            call sort_sift_down(keys, heap, 1_int64, 1_int64, k - 1_int64)
        end do
        perm(1:count) = heap(1:count)
    end procedure sort_partial_permutation

    !> The row index a full sort would place at 1-based rank `nth`, by ORDERING or by quickselect.
    !!
    !! **Two routes, chosen by size at `SORT_NTH_ORDER_MIN`, answering identically.** At or above the
    !! floor the array is ordered and the rank read off, which is asymptotically worse and measurably
    !! faster -- see that constant's own comment for the table and the two reasons. Below it, the
    !! quickselect here.
    !!
    !! The quickselect is `std::nth_element`'s introselect, over the same `sort_partition` the
    !! introsort uses: partition, keep only the side holding `nth`, and stop at the insertion cutoff.
    !! Everything outside the surviving range is already separated correctly by the partitions, so
    !! insertion-sorting just that range puts rank `nth` in its place.
    !!
    !! The depth fallback is the shared `sort_heapsort`, which is the right call here -- it really is
    !! the introsort's fallback doing its job -- and it does bump `dbg_sort_heapsort_calls`.
    module procedure sort_nth_index
        integer(int64), allocatable :: work(:) !! the permutation being narrowed.
        integer(int64) :: lo, hi, cut, k       !! the surviving range, the partition point, a cursor.
        integer :: depth                       !! remaining depth before the heapsort fallback.
        integer(int64) :: floor_rows, nt       !! the ordering floor in force, and the thread count.
        !
        idx = 0_int64
        if (n < 1_int64 .or. nth < 1_int64 .or. nth > n) return
        floor_rows = SORT_NTH_ORDER_MIN
        if (dbg_sort_nth_order_min >= 0_int64) floor_rows = dbg_sort_nth_order_min
        if (n >= floor_rows) then
            ! **The ORDERING route.** Not a fallback and not a heuristic about the data: it is
            ! faster than quickselect at every size above the floor, for the reasons set out on
            ! SORT_NTH_ORDER_MIN, and it answers identically by construction. It also picks up the
            ! radix path and the thread team, neither of which the quickselect below can reach.
            allocate(work(n))
            call resolve_thread_count(threads, n, nt)
            call sort_build_permutation_threaded(keys, n, nt, work)
            idx = work(nth)
            return
        end if
        allocate(work(n))
        do k = 1_int64, n
            work(k) = k
        end do
        lo = 1_int64
        hi = n
        depth = 2 * sort_ilog2(n)
        if (dbg_sort_depth_limit >= 0) depth = dbg_sort_depth_limit
        do while (hi - lo + 1_int64 > SORT_INSERTION_CUTOFF)
            if (depth == 0) then
                call sort_heapsort(keys, work, lo, hi)
                exit
            end if
            depth = depth - 1
            cut = sort_partition(keys, work, lo, hi)
            if (nth >= cut) then
                lo = cut
            else
                hi = cut - 1_int64
            end if
        end do
        call sort_insertion(keys, work, lo, hi)
        idx = work(nth)
    end procedure sort_nth_index

    !> Are rows `1 .. n` already in order under every key?
    !!
    !! `sort_keys_compare`, so two adjacent rows that compare EQUAL are in order.
    !!
    !! **`sort_row_less` would happen to give the same answer here, and that is a coincidence worth
    !! not relying on.** Its tiebreaker orders by ascending row index, and this scan walks rows in
    !! ascending index order, so on an equal-comparing pair it reports "in order" too — a mutation
    !! swapping one for the other survives every fixture. The agreement is a property of the scan
    !! direction, not of the operation: anything walking a permutation instead of the rows
    !! themselves (which is what run detection does, one procedure below) gets a different answer
    !! from the two. Keep the comparator that matches the question being asked.
    module procedure sort_is_sorted
        integer(int64) :: i !! the row being compared against its predecessor.
        !
        answer = .true.
        do i = 2_int64, n
            if (sort_keys_compare(keys, i - 1_int64, i, size(keys)) > 0) then
                answer = .false.
                return
            end if
        end do
    end procedure sort_is_sorted

    !> Sorts, then flags where the runs of EQUAL rows begin.
    !!
    !! One pass rather than a sort followed by a separate comparison pass, because the callers
    !! (`pf_unique`, `pf_rank`) need both and building the permutation twice would double the cost.
    !!
    !! `group_keys` is how many LEADING keys decide a tie, and it is the only thing here that does not
    !! use every key -- the sort always orders by all of them. That asymmetry is the point: it
    !! produces "grouped by field, ordered by magnitude within each group" from a single pass.
    !! `tie(1)` is always 0; the first row starts a run by definition.
    module procedure sort_build_runs_permutation
        integer(int64) :: k !! output position.
        integer :: team     !! the tie pass's team; 1 is the serial loop.
        !
        ! **Threaded, on the same count and the same floor as every ungrouped sort.** `nthreads` is
        ! already resolved -- `resolve_thread_count` has applied the caller's `threads=`, the
        ! automatic policy, `parquet_set_sort_threads`, the nested-region rule and the processor
        ! clamp -- so this hands it straight on and `sort_build_permutation_threaded` applies the
        ! two clauses that need the DATA: the row floor, and one thread meaning the serial path.
        ! Calling the serial `sort_build_permutation` here is what made `threads=` a no-op on the
        ! whole grouped family; feature_risks.md Risk-189 records the shape.
        call sort_build_permutation_threaded(keys, n, nthreads, perm)
        dbg_sort_tie_threads_used = 1_int64
        if (n < 1_int64) return
        ! **The tie pass is threaded on the same count**, as one static `parallel do` over the
        ! output positions: each flag is one comparison of the two rows `perm` names at `k - 1`
        ! and `k`, a pure function of the finished permutation, so a chunk reads its lower
        ! neighbour's last row across the boundary and writes nothing but its own flags. That is
        ! what makes the answer the serial one exactly, and it is what the engine A/B in
        ! test/test_sorting_cpp.f90 rests on now: `parquet_sort_builder_build_runs`
        ! (src/parquet_wrapper.cpp) still walks its flags serially after its threaded build, and
        ! the A/B compares that walk against this pass on a team, rather than the two being
        ! statement-for-statement identical as they were while this loop stayed serial. The
        ! measurement the old "stays SERIAL, deliberately" note asked for came from the join:
        ! this loop was the largest single serial item in a join on an integer key -- 54% of a
        ! 10M-row lookup join at one thread (feature_join.md section 5.2), scattered loads
        ! through `perm` -- and it is what a `threads=` on `pf_unique`, `pf_rank`, `pf_match` or
        ! a sort-engine `%join` was otherwise buying nothing on. `tail_team` applies the tail
        ! floor; the team is recorded because no answer can show it (feature_risks.md Risk-189).
        team = tail_team(nthreads, n)
        dbg_sort_tie_threads_used = int(team, int64)
        tie(1) = 0_c_int8_t
        !$omp parallel do num_threads(team) default(shared) private(k) schedule(static) if (team > 1)
        do k = 2_int64, n
            ! `int()` because `sort_keys_compare` takes a default-kind `nkeys` and the drivers carry
            ! the group width as int64. A key count cannot overflow int32, and the callee clamps it
            ! to `size(keys)` anyway.
            if (sort_keys_compare(keys, perm(k - 1_int64), perm(k), int(group_keys)) == 0) then
                tie(k) = 1_c_int8_t
            else
                tie(k) = 0_c_int8_t
            end if
        end do
        !$omp end parallel do
    end procedure sort_build_runs_permutation

    !> Binary search for the target row, which the caller has APPENDED as row `n_search + 1`.
    !!
    !! **That appending is the whole design and must survive any rewrite.** The target is compared by
    !! the very same `sort_keys_compare` over the very same key layout, so there is no
    !! compare-a-row-against-a-value arm to keep in step with the sort -- which is what makes drift
    !! structurally impossible rather than merely tested for (`feature_risks.md` Risk-34).
    !!
    !! `upper` selects the first position the target is ordered before; otherwise the first position
    !! not ordered before the target. The result is a 1-based insertion point in `1 .. n_search+1`.
    module procedure sort_search_position
        integer(int64) :: lo, hi, mid !! the half-open search window, 0-based, and its midpoint.
        integer(int64) :: target      !! 1-based row index of the appended target.
        integer :: c                  !! comparison of the midpoint row against the target.
        logical :: before             !! .true. when the midpoint is ordered before the answer.
        !
        ! One extracted key can carry many appended targets, so which one to search for is the
        ! caller's to name. Absent, it is the row just past the searched range -- the single-target
        ! case every scalar search passes, and the shape this procedure originally had.
        target = n_search + 1_int64
        if (present(target_row)) target = target_row
        lo = 0_int64
        hi = max(n_search, 0_int64)
        do while (lo < hi)
            mid = lo + (hi - lo) / 2_int64
            c = sort_keys_compare(keys, mid + 1_int64, target, size(keys))
            if (upper) then
                before = c <= 0
            else
                before = c < 0
            end if
            if (before) then
                lo = mid + 1_int64
            else
                hi = mid
            end if
        end do
        pos = lo + 1_int64
    end procedure sort_search_position

    !> Merges two already-ordered ranges -- rows `1 .. na` and `na+1 .. n` -- into one permutation.
    !!
    !! `<= 0` takes from the FIRST range on a tie, which is `std::merge`'s own stability guarantee and
    !! what makes `pf_merge` agree with `pf_sort` of the concatenation element for element. Note this
    !! is one of the places where the two comparators would happen to agree -- every row of the first
    !! range has a lower index than every row of the second, so the tiebreaker would break the tie the
    !! same way -- but `sort_keys_compare` is used anyway, because the agreement is a property of how
    !! the caller happens to lay the ranges out and not of the operation.
    module procedure sort_merge_permutation
        integer(int64) :: i, j, k !! cursors into the first range, the second, and the output.
        integer(int64) :: na_c    !! `na` clamped into 0..n.
        !
        ! Clamped exactly as the C++ entry point clamps it, and for a sharper reason here: an `na`
        ! above `n` would send the first drain loop past the end of `perm`, which Fortran will not
        ! catch without bounds checking.
        na_c = max(0_int64, min(na, n))
        i = 1_int64
        j = na_c + 1_int64
        k = 0_int64
        do while (i <= na_c .and. j <= n)
            k = k + 1_int64
            if (sort_keys_compare(keys, i, j, size(keys)) <= 0) then
                perm(k) = i
                i = i + 1_int64
            else
                perm(k) = j
                j = j + 1_int64
            end if
        end do
        do while (i <= na_c)
            k = k + 1_int64
            perm(k) = i
            i = i + 1_int64
        end do
        do while (j <= n)
            k = k + 1_int64
            perm(k) = j
            j = j + 1_int64
        end do
    end procedure sort_merge_permutation

    ! ---- Test-only access to the two comparators -----------------------------------------------
    !
    ! These exist so `test/test_sorting.f90` can ask the Fortran engine what it thinks, one row pair
    ! at a time, and compare that against what the C++ engine thinks. The test reaches the C++ side
    ! on its own, through locally declared bind(C) interfaces to `parquet_debug_sort_row_less` and
    ! `parquet_debug_sort_keys_compare` in src/parquet_wrapper.cpp -- which is why NOTHING here
    ! crosses the bind(C) boundary and why feature_sort.md section 4's "the C++ boundary for
    ! parquet_sorting is confined to ONE file" still holds with these in place. Keep it that way: a
    ! crossing added here would have to be unpicked again at Stage 6.



    ! The two sweeps below must stay loop-for-loop identical to their C++ twins
    ! (`parquet_debug_sort_sweep_less_cpp` / `_compare_cpp`, src/parquet_wrapper.cpp). They exist so
    ! bench/benchmark_sort_comparator.f90 can time the COMPARATOR rather than the cost of reaching it,
    ! and their agreeing checksums are what prove the two arms did the same work. Two details are
    ! load-bearing and neither is obvious:
    !
    !   * `stride` is computed once per REP. Inside the inner loop it would be a `mod` on a runtime
    !     divisor, i.e. an integer division -- around 6 ns on x86-64, against a comparator costing a
    !     few. CLAUDE.md records a benchmark whose entire reported floor turned out to be exactly
    !     this mistake.
    !   * The wrapping step is `j = i + stride` with one conditional subtraction, which is why
    !     `stride` is kept in `[1, nrows-1]`: a larger stride would need a loop, not a subtraction.


    ! The three below are Stage 2 scaffolding over the module state declared in
    ! src/parquet_sorting.f90 -- see `dbg_fortran_engine` there for why an engine selector is a debug
    ! hook and not a `parquet_settings` knob. All three go away at the Stage 6 cutover.


    module procedure parquet_debug_using_fortran_sort_engine
        on = dbg_fortran_engine
    end procedure parquet_debug_using_fortran_sort_engine

    module procedure parquet_debug_set_sort_depth_limit
        dbg_sort_depth_limit = n
        dbg_sort_heapsort_calls = 0_int64
    end procedure parquet_debug_set_sort_depth_limit

    module procedure parquet_debug_sort_heapsort_calls
        n = dbg_sort_heapsort_calls
    end procedure parquet_debug_sort_heapsort_calls

    module procedure parquet_debug_set_sort_track_shift
        dbg_sort_track_shift = on
        dbg_sort_max_shift = 0_int64
    end procedure parquet_debug_set_sort_track_shift

    module procedure parquet_debug_sort_max_insertion_shift
        n = dbg_sort_max_shift
    end procedure parquet_debug_sort_max_insertion_shift

    module procedure parquet_debug_set_sort_radix_min_rows
        dbg_sort_radix_min_rows = n
    end procedure parquet_debug_set_sort_radix_min_rows

    module procedure parquet_debug_set_sort_nth_order_min
        dbg_sort_nth_order_min = n
    end procedure parquet_debug_set_sort_nth_order_min

    module procedure parquet_debug_set_sort_task_floor
        dbg_sort_task_floor = n
    end procedure parquet_debug_set_sort_task_floor

    module procedure parquet_debug_sort_refine_runs
        n = dbg_sort_refine_runs
    end procedure parquet_debug_sort_refine_runs

    module procedure parquet_debug_set_sort_counting_max_threads
        dbg_sort_counting_max_threads = n
    end procedure parquet_debug_set_sort_counting_max_threads

    module procedure parquet_debug_set_sort_engine_min_rows
        dbg_sort_engine_min_rows = n
    end procedure parquet_debug_set_sort_engine_min_rows

    module procedure parquet_debug_set_sort_tail_min_rows
        dbg_sort_tail_min_rows = n
    end procedure parquet_debug_set_sort_tail_min_rows

    module procedure parquet_debug_set_sort_split_min_card
        dbg_sort_split_min_card = n
    end procedure parquet_debug_set_sort_split_min_card

    module procedure parquet_debug_set_sort_radix_fail_alloc
        dbg_sort_radix_fail_alloc = which
    end procedure parquet_debug_set_sort_radix_fail_alloc

    module procedure parquet_debug_reset_sort_radix_passes
        dbg_sort_radix_passes = 0_int64
    end procedure parquet_debug_reset_sort_radix_passes

    module procedure parquet_debug_sort_radix_passes
        n = dbg_sort_radix_passes
    end procedure parquet_debug_sort_radix_passes

    module procedure parquet_debug_sort_threads_used
        n = dbg_sort_threads_used
    end procedure parquet_debug_sort_threads_used

    module procedure parquet_debug_sort_tie_threads_used
        n = dbg_sort_tie_threads_used
    end procedure parquet_debug_sort_tie_threads_used

    module procedure parquet_debug_sort_offsets_threads_used
        n = dbg_sort_offsets_threads_used
    end procedure parquet_debug_sort_offsets_threads_used

    module procedure parquet_debug_sort_split_buckets
        n = dbg_sort_split_buckets
    end procedure parquet_debug_sort_split_buckets

    module procedure parquet_debug_sort_design
        n = dbg_sort_design
    end procedure parquet_debug_sort_design


end submodule parquet_argsort_engine ! GCOVR_EXCL_LINE
