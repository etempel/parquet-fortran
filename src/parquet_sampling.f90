!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
! DRAWING FROM A POPULATION, as opposed to drawing a NUMBER.
!
! `parquet_random` answers "what is value `draw` of stream `i` under `seed`" and nothing else. That
! keeps it a leaf module -- `iso_fortran_env` and no more -- which is what lets the route (e) fork's
! two arms be compiled and compared standalone by `tools/check_random_kernels.sh`. Everything here
! needs strictly more than that: the permutation kernel needs a thread-count rule from
! `parquet_settings_base`, and the weighted permutation needs a sort.
!
! **The sort is why this module carries the Arrow link edge and `parquet_random` does not.**
! `pf_weighted_permutation` orders `n` exponential keys, the project's one sort is `parquet_sorting`,
! and that module imports `parquet_bindings`. No C++ actually RUNS for `pf_argsort` -- the engine has
! been the Fortran one in `parquet_argsort_engine.f90` since the cutover -- so the cost is a link
! edge, not a call. The alternative was a second copy of a sorting algorithm, which is a far worse
! thing to own. Splitting the two modules is what confines that edge to the callers who need it.
!
! **This module reaches `parquet_random` through its PUBLIC API only**, which is not a style
! preference: a sibling module cannot see another's private names at all, so `perm_at_impl`'s draw
! goes through `pf_random_int_at` rather than the `int_at_impl` it used to call directly, and the
! bulk resample fills through `pf_random_fill_draws` rather than `fill_draws_i32`/`_i64`. Each of
! those public generics is a pass-through to exactly the worker it replaced, so no value changed --
! confirmed by the golden vectors, which are byte-identical across the split.

!> Random selection from a population: permutations, subsets, resampling, and weighted draws.
!!
!! Everything here is built on `parquet_random`'s coordinate-addressed generator, so it inherits
!! that module's central property: **the answer depends on the coordinates, never on the schedule.**
!! A permutation of `10**12` elements is addressable one element at a time without materialising
!! it, a subset of size `k` is a prefix of one of size `2k`, and a weighted draw taken on eight
!! threads equals the one taken on one.
!!
!! Three families, in increasing order of what they cost:
!!
!! - **Uniform, coordinate-addressed.** `pf_random_perm_at` answers a single element of a
!!   permutation in `O(1)` without building it; `pf_random_permutation` and `pf_random_subset`
!!   are the bulk forms over the same bijection, and `pf_random_resample` draws with replacement.
!! - **Weighted, sequential.** `pf_weighted_draw` is successive sampling without replacement:
!!   each draw picks an item with probability proportional to its remaining weight. `O(log n)`
!!   per draw off a segment tree, with an undo journal so `%reset` costs `O(k log n)` rather
!!   than a rebuild. `pf_weighted_subset` is the bulk `k`-item form.
!! - **Weighted, whole-population.** `pf_weighted_permutation` orders every item at once by the
!!   exponential race, `key(i) = -log(u_i)/w_i` sorted ascending, which is the same
!!   Plackett-Luce order the sequential draw produces and is far cheaper when `k` approaches `n`.
!!
!! **The weighted forms give the Plackett-Luce (successive sampling) order, NOT
!! inclusion-probability-proportional-to-size.** An item's chance of being drawn *first* is exactly
!! `w(i)/sum(w)`; its chance of appearing anywhere in a subset of size `k` is not `k*w(i)/sum(w)`
!! and is not available in closed form. If a design calls for probability-proportional-to-size
!! inclusion probabilities, this is not it.
module parquet_sampling

    use iso_fortran_env, only: int32, int64, real64
    use, intrinsic :: ieee_arithmetic, only: ieee_is_nan, ieee_is_finite
    ! The generator, through its public surface only -- see the file header for why.
    use parquet_random, only: pf_random_at, pf_random_key, pf_random_int_at, pf_random_fill_draws
    ! The sort behind `pf_weighted_permutation`, and the only reason this module is not a leaf.
    ! **The ARGSORT TIER, not `parquet_sorting`, and that is the whole point of the tier.**
    ! `pf_weighted_permutation` sorts its exponential race keys, and one `pf_argsort(real64 array,
    ! int64 perm)` is the entire dependency. Taking it from `parquet_sorting` dragged in
    ! `parquet_columns`, `parquet_strings`, `parquet_temporal` and -- before the C++ engine moved
    ! behind a procedure pointer -- `parquet_bindings` and with it Arrow, none of which a weighted
    ! draw has any use for. See feature_modules.md section 4.
    use parquet_argsort, only: pf_argsort
    ! The frozen `-log(u)` transform, in its own leaf module so the standalone fingerprint check
    ! can compile it without dragging this one in.
    use parquet_expkey, only: exp_key, exp_key_contract_ok
    ! parquet_settings_base, NOT parquet_settings: that one imports parquet_bindings to mirror the
    ! C++-side knobs, and nothing here needs a mirrored knob.
    use parquet_settings_base

    implicit none
    private

    public :: parquet_debug_random_bulk_threads
    !
    ! ---- Re-exported from parquet_settings_base ----
    !
    ! **A module re-exports, get and set, every knob its own code reads.** A program that imports
    ! this module for its capability must be able to configure that capability from the same import;
    ! otherwise the only route is `use parquet_settings`, which reaches `parquet_bindings` and drags
    ! the whole Arrow stack back into a build this module exists to keep clear of it. The output
    ! pair comes too wherever the module can emit or suppress output.
    public :: parquet_set_random_threads, parquet_get_random_threads
    public :: parquet_set_random_parallel_min_elements
    public :: parquet_get_random_parallel_min_elements
    !
    public :: parquet_debug_set_perm_rounds
    public :: parquet_debug_set_perm_parity
    public :: parquet_debug_set_perm_force_feistel
    public :: parquet_debug_perm_config
    public :: parquet_debug_set_weighted_int32_limit
    public :: pf_random_perm_algorithm
    public :: pf_random_perm_at
    public :: pf_random_permutation
    public :: pf_random_subset
    public :: pf_random_resample
    public :: pf_weighted_subset
    public :: pf_weighted_permutation

    ! ---- The permutation bijection (see `pf_random_perm_at`) ----

    !> Identifies the PERMUTATION contract, separately from `pf_random_algorithm`.
    !!
    !! Two identifiers rather than one, deliberately. `pf_random_algorithm` names the draw grid's
    !! mappings; the permutation is a different construction with its own kernel, so folding it into
    !! that string would tell every program which recorded it that its stored *draws* had changed
    !! when only the permutation had. This one covers exactly five things and nothing else: the
    !! construction (a Feistel network with cycle-walking), the width rule (`Z_a x Z_b` with
    !! `a = ceil(sqrt(m))`), the round count, the round function, and the round-key derivation.
    !!
    !! **The construction is piecewise and the identifier says so.** `exact20` records that
    !! `m <= perm_exact_max` does not use the Feistel at all -- it is ranked and unranked, and is
    !! therefore exactly uniform rather than approximately so -- while `16p` records the round count
    !! and the parity correction the Feistel carries above that threshold. All four facts fix the
    !! answer, so all four belong in the string; a program that recorded `feistel-mix2-4/zaxzb/v1`
    !! must see a different string, because every value it stored has changed.
    !!
    !! **`/v2`'s values changed once, after this string existed, and the string deliberately did not
    !! move -- read this before concluding the identifier is unreliable.** Folding `m` into the
    !! parity key (`perm_parity_flip`) changed what `/v2` answers for the populations where the
    !! network is parity-locked. The maintainer's decision was to apply that fix under `/v2` rather
    !! than bump to `/v3`, on the grounds that `/v2` was found and corrected inside internal testing
    !! and never reached a released version or an outside user, so no stored value anywhere
    !! disagrees with the current one. **That reasoning expires the moment `/v2` ships**: from the
    !! first release carrying it, any change to what it answers requires a new string, and the
    !! exception recorded here is not a precedent for one taken afterwards.
    !!
    !! **Never replace the construction with a shuffle** (Fisher-Yates, or key-and-argsort): a shuffle
    !! is order-dependent, so element `k` would stop being a function of `(seed, m, k)` alone, and
    !! single-element access, prefix consistency and bit-identity across `threads=` all go with it.
    character(len=*), parameter :: pf_random_perm_algorithm = "feistel-mix2-16p/zaxzb/exact20/v3"

    !> Feistel rounds. **Sixteen, and it must stay even.**
    !!
    !! Even because the two factors swap places every round, so an odd count leaves the state in
    !! `Z_b x Z_a` and the output encoded against transposed factors -- the value would depend on the
    !! parity of the round count in a way nothing else in this module does.
    !!
    !! **Sixteen because four was measured wrong, and because the sizes that would have forced a
    !! higher count no longer reach this kernel at all.** Four was chosen against a *marginal*
    !! distinguisher -- pairs of inputs sharing a component, asking whether their outputs share one
    !! -- and a marginal statistic cannot see the deficit that matters. An all-cells chi-square over
    !! every one of the `m!` permutations scores **z = 27729 at m = 5** against four rounds: the
    !! construction is a long way from uniform at small `m`, and nothing that looks at one position
    !! at a time can tell.
    !!
    !! The requirement is set by the *narrower* Feistel half, not by `m`: exhaustively, half-width 2
    !! needs 20 rounds, **half-width 3 needs 16**, and half-width 5 and above is clean at 8. Because
    !! `m <= perm_exact_max` is answered exactly (see `perm_exact`), every `m` that reaches this
    !! kernel has `min(a, b) >= 5` -- so 16 gives the network the count a strictly *harder* class
    !! than any it can meet requires, two half-width classes of margin, anchored on an exhaustive
    !! measurement rather than on an extrapolation.
    !!
    !! **This is correctness, not tuning, and it may not become a setting** -- see `perm_parity_bit`
    !! for the other half of the same statement, and `feature_risks.md` for what fails silently if
    !! either is lowered. `parquet_debug_set_perm_rounds` exists so a test can weaken it deliberately
    !! and show that the gates have power; nothing in normal operation may vary it.
    integer, parameter :: perm_rounds = 16

    !> Largest `m` answered EXACTLY, by ranking rather than by enciphering. See `perm_exact`.
    !!
    !! `20! = 2432902008176640000` fits an `integer(int64)` and `21!` does not, so this is arithmetic
    !! rather than policy: it is the largest population whose permutations can be addressed one-to-one
    !! by a 64-bit rank. **It is not a tuning knob and must never become one** -- it fixes what the
    !! library answers, which is why it appears in `pf_random_perm_algorithm`.
    integer, parameter :: perm_exact_max = 20

    !> `0!` through `perm_exact_max!`, for the ranking. Every entry is below `huge(int64)`.
    integer(int64), parameter :: perm_fact(0:perm_exact_max) = [ &
        1_int64, 1_int64, 2_int64, 6_int64, &
        24_int64, 120_int64, 720_int64, 5040_int64, &
        40320_int64, 362880_int64, 3628800_int64, 39916800_int64, &
        479001600_int64, 6227020800_int64, 87178291200_int64, 1307674368000_int64, &
        20922789888000_int64, 355687428096000_int64, 6402373705728000_int64, 121645100408832000_int64, &
        2432902008176640000_int64]

    !> `pf_random_key` label separating the permutation family's rank from every draw family.
    !!
    !! The exact path spends one `pf_random_int_at` draw on its rank. Taken at the caller's own seed
    !! it would be the *same* value that caller gets from `pf_random_int_at(seed, 1, ...)`, so a
    !! program using both would find its permutation locked to its first integer draw -- the same
    !! class of defect as a shared grid, and invisible to every test that looks at one family alone.
    integer(int64), parameter :: perm_family_label = 6813122891117395759_int64

    !> Elements enciphered together in the bulk fill's transposed loop. **Speed only.**
    !!
    !! The round loop runs OUTSIDE this block and the element loop inside it, so each round is a
    !! straight-line pass over `perm_block` independent values instead of one value's 16-round
    !! dependency chain. That is the standard counter-mode shape and it is worth 2.07x on machine B
    !! at plain `-O3` with **no vector instruction at all** -- pure instruction-level parallelism,
    !! so the gain is not an ISA assumption -- and 10.77x once the compiler is allowed AVX-512.
    !!
    !! **It changes no value**, which is the property that lets it be chosen freely: the arithmetic
    !! is identical and only its order is not. 64 is comfortably past the point where the chain is
    !! hidden and still a 1.5 KB stack footprint.
    integer, parameter :: perm_block = 64

    !> Key-schedule index the parity bit is drawn from. **Fixed, and deliberately not `perm_rounds`.**
    !!
    !! Keying it on the effective round count would make `parquet_debug_set_perm_rounds` change two
    !! things at once, so a round-count negative control could no longer say which of them it had
    !! detected. A constant index one past the largest round keeps each hook to one variable.
    integer, parameter :: perm_parity_key = perm_rounds + 1

    !> First `mix2` multiplier: the odd 32-bit golden-ratio constant, as used by xxHash and others.
    integer(int64), parameter :: perm_c1 = 2654435761_int64
    !> Second `mix2` multiplier: xxHash's PRIME32_2. Odd, below `2**32`, and well studied.
    integer(int64), parameter :: perm_c2 = 2246822519_int64
    !> Low 31 bits set. **Load-bearing, not decoration** -- see `perm_mix2`.
    integer(int64), parameter :: perm_m31 = 2147483647_int64
    !> Largest `a` whose square is representable, so `a * a` in `perm_factors` cannot overflow.
    integer(int64), parameter :: perm_a_max = 3037000499_int64

    !> Low 32 bits set, for the key schedule and the parity bit.
    !!
    !! A local copy of `parquet_random`'s own `M32`, which is private to that module and stays
    !! that way: publishing it would put a bare `M32` into the namespace `use parquet` hands a
    !! user, for a constant that is `2**32 - 1` and can no more drift than the word size can.
    !! That is the test CLAUDE.md's publish-do-not-copy rule turns on -- it governs an internal
    !! LAYOUT fact two modules could change independently, which this is not.
    integer(int64), parameter :: perm_m32 = 4294967295_int64

    ! ---- Test-only overrides of the permutation kernel ----
    !
    ! **These are the module's only other mutable state, they are process-global, and no library
    ! code reads them outside the three accessors below.** They exist so a test can weaken the
    ! kernel deliberately and demonstrate that a gate has power: a uniformity test asserting only
    ! that the shipped configuration is clean cannot distinguish a correct kernel from a loose
    ! test, which is exactly the error that let four rounds ship. See `parquet_debug_set_perm_rounds`.
    !
    ! They are read from `pure` procedures, which the standard permits -- purity forbids *defining*
    ! a host- or use-associated variable, not referencing one. The consequence to keep in mind is
    ! that a compiler may legally common up two calls with identical arguments across a change of
    ! these values; every test that sets them therefore asserts the two configurations DISAGREE, so
    ! a hoisted call fails loudly instead of quietly reporting a pass.

    !> Forced Feistel round count; `0` means the compiled-in `perm_rounds`.
    integer, save :: perm_dbg_rounds = 0
    !> `.true.` suppresses the parity correction. See `perm_parity_bit`.
    logical, save :: perm_dbg_no_parity = .false.
    !> `.true.` sends `m <= perm_exact_max` through the Feistel instead of the exact path.
    logical, save :: perm_dbg_force_feistel = .false.
    !> Forced ceiling for the three "population must fit in an int32 index" refusals; `< 0` means
    !! the real one, `huge(int32)`.
    !!
    !! Those guards cannot otherwise be reached from a test: entering one needs 2**31 weights, which
    !! is 17 GB of `real64` before anything is drawn -- a fixture this repository cannot build, so
    !! all three would ship with every mutation to them surviving. This is the shape CLAUDE.md's
    !! "Guarding a hard Arrow int32-only ceiling" prescribes, with the override on the Fortran side
    !! rather than in C++ because this module reaches no `bind(C)` surface (see
    !! "A Fortran-side debug hook has to be PUBLIC, so prefer a C++ one").
    !!
    !! ONE knob for all three, deliberately: they answer the same question about the same quantity,
    !! and three separate overrides would let a test lower one while asserting against another.
    integer(int64), save :: wd_dbg_int32_limit = -1_int64

    !> Element `k` of a uniform-looking permutation of `1 .. m`, addressed by its coordinates.
    !!
    !! The permutation counterpart of `pf_random_at`: a pure function of `(seed, m, k)`, computed in
    !! constant time and constant memory, touching no array. Element `k` depends on no other element,
    !! so a loop over `k` may be run in any order, on any number of threads, and gives the same
    !! answer -- which is the property the whole module exists for, extended from draws to
    !! permutations.
    !!
    !! ```fortran
    !! !$omp parallel do
    !! do k = 1, m
    !!     perm(k) = pf_random_perm_at(seed, m, k)      ! same result at any thread count
    !! end do
    !! ```
    !!
    !! **The first `n` values are a uniform random `n`-subset of `1 .. m`**, so a subset needs no
    !! separate machinery and no memory: ask for `k = 1 .. n`. Prefix consistency follows for free --
    !! a size-3 subset is a prefix of a size-6 one from the same `(seed, m)` -- and a subset at
    !! `n == m` **is** the permutation, rather than merely agreeing with it.
    !!
    !! **`m` and `k` share their kind** (`integer(int32)` or `integer(int64)`) and the result follows
    !! them; `seed` is always `integer(int64)`. `k` outside `[1, m]` is clamped rather than reported,
    !! the same convention `draw_or_1` uses and for the same reason: this is `pure elemental` and has
    !! no way to abort.
    !!
    !! **Uniformity comes in two grades, split at `m = 20`, and the split is visible in
    !! `pf_random_perm_algorithm`.**
    !!
    !! For `m <= 20` the result is **exactly uniform over all `m!` permutations** -- one exactly
    !! uniform rank in `[0, m!)` composed with a bijection onto `S_m`, so this is a property of the
    !! construction rather than a measurement. 20 is where it stops because `20!` is the last
    !! factorial an `integer(int64)` holds.
    !!
    !! For `m >= 21` a 64-bit seed cannot address `m!` permutations at all, so exact uniformity is
    !! not available to *any* construction with this signature. What is measured instead is that the
    !! result is indistinguishable from a uniform permutation under an all-cells chi-square, an
    !! order-4 tuple statistic, a parity test over the alternating group, and fixed-point,
    !! cycle-structure, position-uniformity, subset-membership and structural tests.
    !!
    !! **Consecutive `m` are independent, in both regimes.** Asking for a permutation of 5 and one of
    !! 6 under the same seed gives two unrelated answers rather than two views of one draw.
    interface pf_random_perm_at
        module procedure pf_random_perm_at_i32
        module procedure pf_random_perm_at_i64
    end interface pf_random_perm_at

    !> Fills `perm` with the whole permutation of `1 .. size(perm)` under `seed`.
    !!
    !! The bulk form of `pf_random_perm_at`, and an **identity rather than a second algorithm**:
    !! `perm(k)` equals `pf_random_perm_at(seed, size(perm), k)` for every `k`, so the two may be
    !! mixed freely and a prefix of one is a prefix of the other. That is the same relationship
    !! `pf_random_at` and `pf_random_fill_draws` already have, and it is what keeps the two forms
    !! from drifting apart under a later optimisation.
    !!
    !! `perm` is a rank-1 `integer(int32)` or `integer(int64)` array, `intent(out)`; `seed` is
    !! `integer(int64)`. A zero-sized `perm` is a defined no-op.
    !!
    !! **`threads` is optional and changes only how fast the array is filled, never what is in
    !! it** -- and that is provable here rather than merely intended, which is unusual for a
    !! threading argument. `perm(k)` is a pure function of `(seed, size(perm), k)` and reads
    !! nothing another element writes, so the 1-thread and N-thread results are bit-identical and
    !! a program may switch thread counts while debugging without its permutation changing under
    !! it. Absent means automatic: as many threads as OpenMP offers, capped by
    !! `parquet_set_random_threads`, and **1 inside an OpenMP parallel region**, since a nested
    !! region is the caller's business. An explicit `threads=` is honoured there too.
    !!
    !! **A work floor applies to both, and it is not a tuning detail.** Below
    !! `parquet_set_random_parallel_min_elements` (default 1000) elements per thread the call runs
    !! serially, because threading a small permutation is measurably *slower* than not threading
    !! it -- so `threads=8` on a 100-element array is honoured by being declined.
    !!
    !! **It is about 2.4x cheaper per element than calling the elemental form `size(perm)` times**
    !! (machine B, gfortran 14.2.1, `--profile release`: 9.9 ns against 24.1 ns at `m = 10**8`), for
    !! two structural reasons rather than any tuning. Enumerating the domain in order hands the loop
    !! the `(l, r)` split that the scalar form has to recover with an integer division; and, the
    !! larger of the two, the width rule and the key schedule are derived once per call instead of
    !! once per element, which a `pure elemental` function has no way to avoid. The values are
    !! unchanged by either -- only the route to them is.
    interface pf_random_permutation
        module procedure pf_random_permutation_i32
        module procedure pf_random_permutation_i64
    end interface pf_random_permutation

    !> Fills `idx` with the first `size(idx)` elements of the permutation of `1 .. m` under `seed`.
    !!
    !! A uniform random subset of size `size(idx)` drawn without replacement, in uniform random
    !! order -- and it costs `O(size(idx))` rather than `O(m)`, which is the reason this permutation
    !! is coordinate-addressed at all: 1000 rows out of `10**12` reads 1000 elements and never
    !! materialises the population. `pf_random_subset(idx, size(idx), seed)` **is**
    !! `pf_random_permutation(idx, seed)`, and a subset of size `n` is a prefix of one of size `2n`.
    !!
    !! `idx` is a rank-1 `integer(int32)` or `integer(int64)` array, `intent(out)`; `m` is
    !! `integer(int32)` or `integer(int64)`; `seed` is `integer(int64)`. A zero-sized `idx` is a
    !! defined no-op and is not validated -- it asks for nothing, so no precondition applies to it.
    !! `threads` behaves exactly as it does on `pf_random_permutation`, including the work floor;
    !! see there. Note the floor is measured in elements PRODUCED, `size(idx)`, not in `m` -- a
    !! 100-element subset of a population of `10**12` is 100 elements of work.
    !!
    !! **Three preconditions, all of which abort rather than truncate or wrap** when `idx` is
    !! non-empty: `m >= 1`; `size(idx) <= m`, since a subset drawn without replacement cannot be
    !! larger than its population; and -- for an `integer(int32)` `idx` only -- `m <= huge(int32)`,
    !! since an element may be any value in `[1, m]` and one above `huge(int32)` has nowhere to go.
    !! The last of those is why the `integer(int32)`-array/`integer(int64)`-`m` pairing is accepted
    !! at compile time and rejected at run time: refusing it in the interface would make a perfectly
    !! ordinary `integer :: m` fail to compile against an `integer(int64)` array.
    interface pf_random_subset
        module procedure pf_random_subset_i32_i32
        module procedure pf_random_subset_i32_i64
        module procedure pf_random_subset_i64_i32
        module procedure pf_random_subset_i64_i64
    end interface pf_random_subset

    !> Fills `idx` with `size(idx)` values drawn from `1 .. m` **with replacement**.
    !!
    !! The third member of the resampling trio, and the one whose construction is not a construction
    !! at all: drawing with replacement means `size(idx)` independent uniform integers in `[1, m]`,
    !! with no dedup structure, no permutation and no sort. So this is the draw-axis integer bulk
    !! fill under another name, and the identity is exact and is asserted by the suite:
    !!
    !! ```fortran
    !! call pf_random_resample(idx, m, seed, stream)
    !! call pf_random_fill_draws(seed, stream, idx, 1_int64, m)   ! the SAME values
    !! ```
    !!
    !! **What the name buys is the 1.4x-1.6x a caller loses by writing the obvious loop.** Without
    !! it the natural code is `do k = 1, n; idx(k) = pf_random_int_at(seed, i, 1, m, k); end do`,
    !! which re-enciphers a Philox block for every value where the bulk form serves two draws from
    !! each one -- measured at 25.7 against 15.7 ns per value on machine B (gfortran 15.2.1,
    !! `--profile release`, 10M values) and 16.1 against 10.1 on machine A. The values are identical
    !! either way; only the route to them differs.
    !!
    !! `idx` is a rank-1 `integer(int32)` or `integer(int64)` array, `intent(out)`; `m` is
    !! `integer(int32)` or `integer(int64)`; `seed` is `integer(int64)`. `stream` is optional,
    !! `integer(int32)` or `integer(int64)`, and defaults to 1 -- it selects which replicate this is,
    !! so replicate `b` is reproducible from `(seed, b)` alone, independent of how many replicates
    !! were asked for or in what order they ran. A zero-sized `idx` is a defined no-op and is not
    !! validated: it asks for nothing, so no precondition applies to it.
    !!
    !! **Two preconditions, both aborting rather than truncating or wrapping**, and only when `idx`
    !! is non-empty: `m >= 1`; and -- for an `integer(int32)` `idx` only -- `m <= huge(int32)`, since
    !! an element may be any value in `[1, m]` and one above `huge(int32)` has nowhere to go. The
    !! second is why the `integer(int32)`-array/`integer(int64)`-`m` pairing is accepted at compile
    !! time and rejected at run time, exactly as on `pf_random_subset`.
    !!
    !! **There is deliberately NO `size(idx) <= m` precondition**, and its absence is the clearest
    !! statement of how this differs from its sibling. That bound belongs to a subset drawn *without*
    !! replacement; here `size(idx) == m` is the most ordinary bootstrap there is, and `size(idx)`
    !! far beyond `m` is perfectly meaningful. A guard copied across from `pf_random_subset` would
    !! refuse the procedure's main use.
    !!
    !! **`stream` is this procedure's replicate axis, and the siblings do not have one.**
    !! `pf_random_permutation` and `pf_random_subset` are keyed by `(seed, m)` alone, so independent
    !! replicates of those come from `pf_random_key(seed, b)` instead. A resample is built on the
    !! draw axis, which carries a stream coordinate already, so it costs a caller one integer rather
    !! than a key derivation. Both routes are available here: `stream = b` and
    !! `seed = pf_random_key(seed, b)` are equally independent.
    !!
    !! **`threads` behaves exactly as it does on `pf_random_permutation` and `pf_random_subset`** --
    !! same `parquet_set_random_threads` default, same `parquet_set_random_parallel_min_elements`
    !! work floor, and the floor applies to an explicit request too, so a small resample stays serial
    !! however many workers are asked for. The result is **bit-identical at every thread count**, and
    !! by construction rather than by care: element `k` is a pure function of `(seed, stream, k)`, so
    !! a thread filling elements `lo .. hi` is the serial fill started at draw `lo`. That is asserted
    !! rather than argued.
    !!
    !! **`threads` requires an explicit `stream`, and that is a language constraint rather than a
    !! choice.** `threads` and `stream` are both integers in argument position 4, so a generic
    !! offering `(idx, m, seed [, threads])` beside `(idx, m, seed, stream)` is **rejected by the
    !! compiler** -- "Ambiguous interfaces in generic interface 'pf_random_resample'" -- and giving
    !! the two dummies different keyword names does not rescue it, because keyword names do not make
    !! specifics distinguishable. Both spellings were compiled to confirm it. So write
    !! `call pf_random_resample(idx, m, seed, 1, threads=8)` for the default replicate; omitting
    !! `stream` produces a "no specific subroutine matches" error that does not explain itself. This
    !! is the same constraint the `parquet_open_reader` split answers one level along, where an
    !! optional argument differing only by *kind* could not disambiguate either.
    interface pf_random_resample
        module procedure pf_random_resample_i32_i32
        module procedure pf_random_resample_i32_i64
        module procedure pf_random_resample_i64_i32
        module procedure pf_random_resample_i64_i64
        module procedure pf_random_resample_i32_i32_s32
        module procedure pf_random_resample_i32_i64_s32
        module procedure pf_random_resample_i64_i32_s32
        module procedure pf_random_resample_i64_i64_s32
        module procedure pf_random_resample_i32_i32_s64
        module procedure pf_random_resample_i32_i64_s64
        module procedure pf_random_resample_i64_i32_s64
        module procedure pf_random_resample_i64_i64_s64
    end interface pf_random_resample

    ! ================================================================================
    ! Tier 2 -- the weighted sequential draw
    ! ================================================================================

    !> Label deriving the zero-weight tail's own permutation seed. Any fixed value would do.
    integer(int64), parameter :: wd_zero_label = 7965600847521931_int64

    !> Labels separating the two weighted families' uniforms from each other, and from the caller's.
    !!
    !! **The two families are DIFFERENT REALIZATIONS, so they must not share a uniform**, and
    !! without these labels they shared the first one exactly: the race reads draw 1 of
    !! `(seed, stream)` as item 1's key while the sequential descent reads the same draw and scales
    !! it into `[0, total)`. A key near zero and a descent landing on the leftmost leaf are then the
    !! same event, so both families chose item 1 together. Measured over 500 000 seeds with weights
    !! `1 .. 50`: the race chose item 1 on 399 seeds and the tree agreed on **264 of them (66 %)**,
    !! against an independent expectation of **0.078 %** -- an 844x enrichment, in that one cell and
    !! nowhere else. Every marginal was clean (2.72 % overall agreement against 2.64 % by chance),
    !! and so was the different-stream control, which is why nothing caught it: it is invisible to
    !! any test of one family, and the suite's own cross-family test asserts only that the two
    !! DIFFER. See `feature_risks.md` Risk-113 for the same class found in the stride axis.
    !!
    !! Deriving each family's seed through `pf_random_key` costs one mix per call and separates
    !! three things at once -- the two families from each other, and both from a caller drawing
    !! `pf_random_at(seed, stream, ...)` at coordinates it chose itself. This is what
    !! `perm_family_label` already does for the exact permutation path's rank draw.
    integer(int64), parameter :: wd_family_label = 3168215062744167237_int64
    !> The race's own label. See `wd_family_label`; the two values need only differ.
    integer(int64), parameter :: wperm_family_label = 8402337155096104909_int64

    !> Journal entries allocated on first use; it doubles from there.
    integer(int64), parameter :: wd_journal_min = 64_int64

    !> The smallest weight that takes part in a draw; below this an item is treated as zero-weight.
    !!
    !! **Two problems have one answer here, and the threshold is set by the second.**
    !!
    !! **1. The classification must not depend on the floating-point model.** A build with
    !! `-ffast-math`, or ifx at its own defaults, runs with denormals flushed to zero, so
    !! `w > 0.0` is `.false.` for a denormal weight while a `-fp-model=precise` build says
    !! `.true.` -- measured on ifx 2026.1.1, where `1.0e-320 > 0.0` differs between `-O2` and
    !! `-O2 -fp-model=precise`. That put the SAME item in the race under one build and in the
    !! zero-weight tail under another. Comparing against a normal constant agrees under both,
    !! because a flushed denormal reads as `0.0`.
    !!
    !! **2. A key must not be able to overflow, and that is what fixes the VALUE.** The key is
    !! `-log(u)/w` and the numerator reaches `53*log(2) = 36.7368` (the domain is `u >= 2**-53`),
    !! so `w` must satisfy `36.7368/w <= huge(1.0_real64)`, i.e. `w >= 2.043552e-307`. This
    !! constant is that bound rounded up, giving a maximum key of `1.7920e308` against a `huge` of
    !! `1.7977e308`. **It is 9.2x `tiny`, and `tiny` would NOT have been enough**: weights in
    !! `[tiny, 2.0436e-307)` are ordinary normal numbers that no model flushes, and their keys
    !! overflow for a small but nonzero fraction of draws -- which would have made the abort
    !! DRAW-DEPENDENT, firing for some seeds and not others on identical weights.
    !!
    !! **Nothing observable is lost by calling such an item zero-weight.** Against weights of order
    !! one, a weight of `2e-307` has a chance of order `1e-307` of being drawn first; no finite
    !! sample can distinguish that from zero, and the item is still returned -- last, in uniform
    !! random order, like every other zero-weight item.
    !!
    !! Keep this in step with `exp_key`'s domain: if that transform's range ever grows, this bound
    !! grows with it, and the overflow guard in `wperm_impl` stops being unreachable.
    real(real64), parameter :: wd_min_weight = 2.05e-307_real64

    !> Successive sampling without replacement: draw one item with probability proportional to its
    !> weight, remove it, renormalise over the survivors, repeat.
    !>
    !> **What this distribution is, and one thing it is not.** Drawn to exhaustion it is the
    !> weighted shuffle, also called the Plackett-Luce order. The FIRST draw is exactly
    !> proportional to weight; later draws are proportional among the survivors. It is **not**
    !> inclusion-probability-proportional-to-size: an item with twice the weight is not twice as
    !> likely to appear somewhere in the first `k`, and no construction with this interface can
    !> make it so. Most callers who reach for "weighted sampling without replacement" want this
    !> one, but the distinction is worth knowing before relying on it.
    !>
    !> ```fortran
    !> type(pf_weighted_draw) :: d
    !> integer :: item
    !> logical :: ok
    !>
    !> call d%init(weights, seed)
    !> do
    !>     call d%next(item, ok)
    !>     if (.not. ok) exit          ! the population is exhausted
    !>     ! ... judge item; exit on acceptance ...
    !> end do
    !> ```
    !>
    !> **The tree is seed-independent, and that is the whole reason this type exists.** It is built
    !> from the weights alone; the seed only steers the descent. So a second sequence over the same
    !> weights costs `O(k log n)` to restore rather than an `O(n)` rebuild, which is what makes an
    !> outer loop of many short sequences -- the shape this type was designed for -- nearly free.
    !> Use `%reseed` for that, holding the seed fixed and passing the outer iteration as `stream`.
    !>
    !> **Cost**: `%init` is `O(n)`, `%next` is `O(log n)`, `%reset` and `%reseed` are `O(k log n)`
    !> in the draws already taken.
    !>
    !> **Serial by nature, and there is no `threads=`.** Draw `k+1` cannot be produced until draw
    !> `k` has been removed, so there is no parallelism inside one sequence to expose; an argument
    !> that was accepted and ignored would be worse than its absence. Independent sequences ARE
    !> parallel, and that is where the threads go -- see the note below.
    !>
    !> **To run many sequences in parallel, give each thread its own sampler, built inside the
    !> parallel region.** `%next` mutates the tree, so one sampler cannot serve two threads.
    !>
    !> ```fortran
    !> type(pf_weighted_draw), allocatable :: dd(:)
    !> integer :: nt, tid, j
    !>
    !> nt = 1
    !> !$ nt = omp_get_max_threads()
    !> allocate(dd(nt))
    !> !$omp parallel do default(shared) private(tid)
    !> do tid = 1, nt
    !>     call dd(tid)%init(weights, seed)     ! nt builds, wall-clock of ONE
    !> end do
    !>
    !> !$omp parallel do default(shared) private(tid, item, ok) schedule(dynamic)
    !> do j = 1, n_outer
    !>     tid = 1
    !>     !$ tid = omp_get_thread_num() + 1
    !>     call dd(tid)%reseed(seed, stream=int(j, int64))
    !>     do
    !>         call dd(tid)%next(item, ok)
    !>         if (.not. ok) exit
    !>     end do
    !> end do
    !> ```
    !>
    !> **Never put this type in an OpenMP `private()` clause.** A private copy is a FRESH object,
    !> not a copy of yours: measured on gfortran 15.2.1 and ifx 2026.1.1, the tree comes back
    !> allocated to the right shape with UNINITIALISED contents and every scalar reset to its
    !> default. Nothing aborts and every drawn item still looks valid, so the failure is a wrong
    !> answer with no symptom. The shared per-thread array above avoids the privatisation machinery
    !> entirely; `firstprivate()` also copies correctly on both compilers, if you prefer it.
    !>
    !> **Because each sequence is named by its coordinates, the result does not depend on the
    !> schedule or the thread count.** `schedule(dynamic)` above costs nothing in reproducibility.
    !>
    !> **"Zero-weight" means a weight below `wd_min_weight` (2.05e-307), not exactly zero.** Below
    !> that bound `-log(u)/w` would overflow, so such a weight cannot be ordered at all; and against
    !> weights of order one its draw chance is of order `1e-307`, which no finite sample can tell
    !> from zero. Thresholding rather than testing `> 0` also makes the classification independent
    !> of the floating-point model, since a build that flushes denormals reads them as `0.0`.
    !>
    !> **Zero-weight items are kept out of the tree and handed out last**, in uniform random order,
    !> so draining a population is always a genuine permutation of every item. Keeping them out is
    !> not an optimisation: a zero leaf would be indistinguishable from a spent one, and the
    !> descent's liveness test reads exactly that.
    type, public :: pf_weighted_draw
        private
        !> The segment tree: `2*p2` nodes, leaf `j` at `st(p2 + j - 1)`, every internal node the
        !! sum of its two children. Node `1` is unused padding's parent -- the root is `st(1)`.
        real(real64), allocatable :: st(:)
        !> Leaf `j` holds the weight of item `leaf_item(j)`. **Unallocated when no weight is
        !! zero**, which is the common case; leaf `j` is then item `j` and the map is skipped.
        integer(int64), allocatable :: leaf_item(:)
        !> The zero-weight items, in input order. Unallocated when there are none.
        integer(int64), allocatable :: zero_item(:)
        !> Undo journal: the leaf node zeroed by each draw so far, in order.
        !!
        !! **One entry per DRAW, not one per tree write.** Undoing a draw restores its leaf and
        !! then recomputes that leaf's ancestors from their children, exactly as the forward
        !! direction does -- so the `O(log n)` ancestor writes need not be journalled at all. That
        !! is 1 entry where the obvious design has `1 + log2(n)`, which at `n = 10**6` is a 21x
        !! difference in what a fully drained sampler holds, and it is why no capacity policy or
        !! rebuild fallback is needed. It is exact rather than approximate: every node is a pure
        !! function of its two children, so replaying the leaves reproduces the built tree bit for
        !! bit -- `test_reset_restores_tree_exactly` asserts precisely that.
        integer(int64), allocatable :: jr_pos(:)
        !> Undo journal: the weight each of those leaves held.
        real(real64), allocatable :: jr_val(:)
        integer(int64) :: p2 = 0        !! leaves in the tree; the least power of two `>= npos`
        integer(int64) :: npos = 0      !! items with a strictly positive weight
        integer(int64) :: nzero = 0     !! items with weight exactly zero
        integer(int64) :: live = 0      !! positive-weight items not yet drawn
        integer(int64) :: ndrawn = 0    !! draws taken in this sequence, zero-weight tail included
        integer(int64) :: jr_n = 0      !! journal entries in use
        integer(int64) :: wseed = 0     !! the seed steering the descent
        integer(int64) :: wstream = 0   !! which sequence of that seed
        integer(int64) :: zkey = 0      !! derived seed ordering the zero-weight tail
        logical :: ready = .false.      !! `%init` has run; guards every other entry point
    contains
        ! No FINAL binding, and none may be added: the allocatable components free themselves, and a
        ! finalizer would add gfortran's finalizable-type `private()` hazard (`test_weighted_per_thread`).
        procedure, private :: init_base => wd_init_base  !! `%init` with no stream
        procedure, private :: init_s32 => wd_init_s32    !! `%init` with an `int32` stream
        procedure, private :: init_s64 => wd_init_s64    !! `%init` with an `int64` stream
        !> Prepares the sampler over `weights`. `O(n)`. `stream` defaults to 0.
        generic :: init => init_base, init_s32, init_s64
        procedure, private :: next_i32 => wd_next_i32    !! `%next` into an `int32` item
        procedure, private :: next_i64 => wd_next_i64    !! `%next` into an `int64` item
        !> Draws the next item. `O(log n)`. `ok` is `.false.` once every item has been drawn.
        generic :: next => next_i32, next_i64
        procedure :: reset => wd_reset                   !! Restarts the SAME sequence. `O(k log n)`.
        procedure, private :: reseed_base => wd_reseed_base !! `%reseed` with no stream
        procedure, private :: reseed_s32 => wd_reseed_s32   !! `%reseed` with an `int32` stream
        procedure, private :: reseed_s64 => wd_reseed_s64   !! `%reseed` with an `int64` stream
        !> Starts a DIFFERENT sequence over the same weights. `O(k log n)`; `stream` defaults to 0.
        generic :: reseed => reseed_base, reseed_s32, reseed_s64
        procedure :: remaining => wd_remaining           !! Items not yet drawn; exact, `O(1)`.
    end type pf_weighted_draw

    !> Fills `perm` with a weighted random permutation of `1 .. size(weights)`.
    !!
    !! **The order family.** `perm(k)` is the item drawn `k`-th by successive sampling: the first
    !! is proportional to weight, each later one proportional among the survivors. Drawn to
    !! exhaustion, that is the weighted shuffle. Same distribution as `pf_weighted_draw` -- see the
    !! note on realizations below.
    !!
    !! `perm` is a rank-1 `integer(int32)` or `integer(int64)` array, `intent(out)`, whose size
    !! must equal `size(weights)`; `weights` is `real(real64)`; `seed` is `integer(int64)`;
    !! `stream` is optional and `integer(int64)` -- note the asymmetry with `pf_weighted_draw`,
    !! whose `%init`/`%reseed` take either kind. It is forced rather than chosen: an optional
    !! `threads` and an `integer(int32)` `stream` are not distinguishable as a positional fourth
    !! argument, so one generic cannot carry both, and `threads=` is worth more here than saving a
    !! cast. Write `stream=int(j, int64)` in a loop. `threads` behaves as it does elsewhere in this
    !! module, including the work floor.
    !!
    !! **It is a DIFFERENT REALIZATION from the sequential family, not a different distribution.**
    !! `pf_weighted_permutation(perm, w, seed)` and a `pf_weighted_draw` drained under the same
    !! seed both draw from successive sampling, and for one seed they give different draws from it
    !! -- in the same way two different seeds would. **They are INDEPENDENT at matched coordinates,
    !! not merely different**, which is a stronger claim and one the two families had to be
    !! domain-separated to earn: each derives its own seed through `pf_random_key`, so neither
    !! shares a uniform with the other or with a caller drawing at coordinates it chose itself.
    !! Before that separation both read draw 1 of `(seed, stream)` and chose the lowest-weight item
    !! together on 66 % of the seeds where either did, against 0.078 % by chance -- with every
    !! marginal clean. `test_families_independent` is what holds this now; see `wd_family_label`.
    !! There is no prefix identity across the two families.
    !! Within the sequential family the prefix identity does hold; see `pf_weighted_subset`.
    !!
    !! **Construction: the exponential race.** Give item `i` the key `-log(u_i)/w_i` for
    !! independent uniforms, and sort ascending. That is exactly successive sampling, and unlike
    !! the sequential form it is embarrassingly parallel and coordinate-addressed, so the answer is
    !! **bit-identical at every thread count**. Use this when you want many whole shuffles; use
    !! `pf_weighted_draw` when you want a few draws, or many short sequences over the same weights,
    !! where it is thousands of times cheaper.
    !!
    !! **To produce many shuffles, parallelise ACROSS them rather than within one.** The key loop
    !! is memory-bound and scales sub-linearly, whereas independent shuffles are perfectly
    !! parallel. An OpenMP loop over sequences, each calling this with `threads=1`, beats a serial
    !! loop over threaded calls; the module's own auto-threading already resolves to serial inside
    !! an active parallel region, so this needs no special handling.
    !!
    !! **Zero-weight items come last, in uniform random order** -- they can never be drawn while a
    !! positive weight remains, so a full shuffle is still a genuine permutation of every item.
    !!
    !! **Preconditions, all of which abort**: `size(perm) == size(weights)`; every weight finite
    !! and `>= 0`; at least one weight `> 0`; no weight so small that `-log(u)/w` overflows (below
    !! about `2e-307`, which no real weighting reaches and which would otherwise tie several items
    !! at infinity); and, for an `integer(int32)` `perm`, `size(weights) <= huge(1_int32)`.
    interface pf_weighted_permutation
        module procedure wperm_i32_base
        module procedure wperm_i32_s64
        module procedure wperm_i64_base
        module procedure wperm_i64_s64
    end interface pf_weighted_permutation

    !> Fills `idx` with the first `size(idx)` items of a weighted sequential draw over `weights`.
    !!
    !! **Defined as `size(idx)` calls to `pf_weighted_draw%next`, not as a second algorithm**, so a
    !! subset of size `k` is a prefix of one of size `2k`, and stopping a `%next` loop after `k`
    !! draws gives exactly this. That identity is asserted by the suite rather than intended.
    !!
    !! `idx` is a rank-1 `integer(int32)` or `integer(int64)` array, `intent(out)`; `weights` is
    !! `real(real64)`, one per item; `seed` is `integer(int64)`; `stream` is optional and is
    !! `integer(int32)` or `integer(int64)`. The population is `size(weights)` and the number drawn
    !! is `size(idx)` -- note this differs from `pf_random_subset`, whose second argument is the
    !! POPULATION, because here the population is carried by the weights themselves.
    !!
    !! **Preconditions, all of which abort** when `idx` is non-empty: `size(idx) <= size(weights)`;
    !! every weight `>= 0`; at least one weight `> 0`; and, for an `integer(int32)` `idx`,
    !! `size(weights) <= huge(1_int32)`. A zero-sized `idx` asks for nothing and is a defined no-op.
    !!
    !! **`O(n + k log n)`**, so it is the cheaper form whenever `k` is small against `n`; past
    !! roughly `k = n/2` a caller wanting most of the population is better served by asking for the
    !! whole weighted permutation.
    interface pf_weighted_subset
        module procedure wsub_i32_base
        module procedure wsub_i32_s32
        module procedure wsub_i32_s64
        module procedure wsub_i64_base
        module procedure wsub_i64_s32
        module procedure wsub_i64_s64
    end interface pf_weighted_subset

contains

    ! ================================================================================
    ! The permutation bijection
    ! ================================================================================

    !> `pf_random_perm_at` for `integer(int32)` population size and index.
    !!
    !! The result is inside `[1, m]` by construction, so narrowing the `int64` worker's answer is
    !! exact -- the same argument `pf_random_int_at_i32` rests on.
    pure elemental function pf_random_perm_at_i32(seed, m, k) result(r)
        integer(int64), intent(in) :: seed          !! the permutation family's seed
        integer(int32), intent(in) :: m             !! population size; the permutation is of `1 .. m`
        integer(int32), intent(in) :: k             !! 1-based position; clamped into `[1, m]`
        integer(int32) :: r                         !! element `k` of that permutation
        r = int(perm_at_impl(seed, int(m, int64), int(k, int64)), int32)
    end function pf_random_perm_at_i32

    !> `pf_random_perm_at` for `integer(int64)` population size and index.
    pure elemental function pf_random_perm_at_i64(seed, m, k) result(r)
        integer(int64), intent(in) :: seed          !! the permutation family's seed
        integer(int64), intent(in) :: m             !! population size; the permutation is of `1 .. m`
        integer(int64), intent(in) :: k             !! 1-based position; clamped into `[1, m]`
        integer(int64) :: r                         !! element `k` of that permutation
        r = perm_at_impl(seed, m, k)
    end function pf_random_perm_at_i64

    !> The permutation itself. **Two constructions, split at `perm_exact_max`.**
    !!
    !! For `m <= perm_exact_max` the answer is *ranked*, not enciphered: one exactly-uniform draw in
    !! `[0, m!)` and a Lehmer unranking, which is a bijection onto `S_m`. That composition is exactly
    !! uniform over all `m!` permutations -- by construction, with no mixing question, no round count
    !! and nothing to validate by measurement. See `perm_exact`.
    !!
    !! For `m >= perm_exact_max + 1` it is the Feistel network: split, encipher, cycle-walk, rejoin,
    !! then the parity correction. The walk is what turns a bijection on `[0, a*b)` into one on
    !! `[0, m)`: re-apply the network while the value is out of range. It terminates because the
    !! network is a bijection, so every orbit closes, and the orbit of a value below `m` must return
    !! to it. With `a*b` sitting on top of `m` it is entered essentially never -- measured at 1.0000
    !! applications per element at m = 10**6, 10**7 and 10**8.
    !!
    !! **The boundary costs nothing in consistency, which is the reason it is affordable at all.**
    !! Both sides are reached from here and from `perm_fill_i32`/`perm_fill_i64` through the *same*
    !! two procedures, so `perm(k) == pf_random_perm_at(seed, m, k)` holds because there is one
    !! computation rather than two that have to be kept equal by testing. And the boundary is a
    !! compile-time constant fixed by `20! < huge(int64)`, so it cannot drift.
    pure function perm_at_impl(seed, m, k) result(r)
        integer(int64), intent(in) :: seed          !! the permutation family's seed
        integer(int64), intent(in) :: m             !! population size
        integer(int64), intent(in) :: k             !! 1-based position, not yet clamped
        integer(int64) :: r                         !! element `k`, in `[1, m]`
        integer(int64) :: a, b, rk(perm_rounds), x, n, small(perm_exact_max)
        integer :: nr
        n = m
        if (n < 1_int64) n = 1_int64                ! a degenerate population is one element
        x = k - 1_int64
        if (x < 0_int64) x = 0_int64                ! clamp rather than report; see the interface doc
        if (x >= n) x = n - 1_int64
        if (n == 1_int64) then
            r = 1_int64
            return
        end if
        if (perm_use_exact(n)) then
            call perm_exact(seed, n, small)
            r = small(x + 1_int64)
            return
        end if
        call perm_factors(n, a, b)
        nr = perm_round_count()
        call perm_round_keys(seed, nr, rk)
        do
            x = perm_feistel(rk, nr, a, b, x)
            if (x < n) exit
        end do
        r = perm_swap01(perm_parity_flip(seed, n), x) + 1_int64
    end function perm_at_impl

    !> Whether population `m` takes the exact path. See `perm_exact` and `perm_exact_max`.
    pure function perm_use_exact(m) result(yes)
        integer(int64), intent(in) :: m             !! population size, at least 2
        logical :: yes                              !! `.true.` when `m` is ranked rather than enciphered
        yes = (m <= int(perm_exact_max, int64)) .and. .not. perm_dbg_force_feistel
    end function perm_use_exact

    !> Rounds the Feistel will actually run: `perm_rounds`, or whatever a test has forced.
    pure function perm_round_count() result(nr)
        integer :: nr                               !! effective round count, always at least 1
        nr = perm_rounds
        if (perm_dbg_rounds > 0) nr = perm_dbg_rounds
    end function perm_round_count

    !> The whole permutation of `1 .. m` for `m <= perm_exact_max`, EXACTLY uniform.
    !!
    !! **This is the only part of the module whose uniformity is a theorem rather than a
    !! measurement, and it covers precisely the sizes where the Feistel was worst.** `pf_random_int_at`
    !! is exactly uniform over any closed range -- that is what its rejection step buys -- and Lehmer
    !! unranking is a bijection from `[0, m!)` onto `S_m`. The composition of an exactly uniform draw
    !! with a bijection is exactly uniform, so there is nothing here for an ensemble test to find.
    !!
    !! **`m` is the STREAM index, not part of the key.** Reducing one candidate into `[0, 5!)` and
    !! into `[0, 6!)` would give two ranks that are both monotone in the same 64-bit word, so a
    !! program asking for a permutation of 5 and one of 6 under the same seed would get two strongly
    !! correlated answers -- structure a uniform construction does not have. Separate streams are
    !! separate Philox counters, so the ranks are independent.
    !!
    !! **`perm_family_label` is not optional either**: without it the rank is literally the value
    !! `pf_random_int_at(seed, m, ...)` hands the same caller, so a program using both families would
    !! find its permutation locked to its own integer draws.
    !!
    !! Cost is one draw plus `m-1` divisions and an `O(m**2)/2` selection scan, all bounded by
    !! `perm_exact_max`; the pool lives on the stack and nothing is allocated.
    pure subroutine perm_exact(seed, m, p)
        integer(int64), intent(in) :: seed          !! the permutation family's seed
        integer(int64), intent(in) :: m             !! population size, `2 <= m <= perm_exact_max`
        integer(int64), intent(out) :: p(perm_exact_max) !! `p(1:m)` is the permutation of `1 .. m`
        integer(int64) :: rank, rem, d, pool(perm_exact_max)
        integer :: i, j, mm, dd
        mm = int(m)
        rank = pf_random_int_at(pf_random_key(seed, perm_family_label), m, 0_int64, perm_fact(mm) - 1_int64, 1_int64)
        do i = 1, mm
            pool(i) = int(i, int64)
        end do
        rem = rank
        do i = 1, mm
            ! `rem < (mm-i+1)!` on entry, so `d < mm-i+1` and the pool index is always in range.
            d = rem / perm_fact(mm - i)
            rem = rem - d * perm_fact(mm - i)
            dd = int(d)
            p(i) = pool(dd + 1)
            do j = dd + 1, mm - i
                pool(j) = pool(j + 1)
            end do
        end do
        do i = mm + 1, perm_exact_max
            p(i) = 0_int64                          ! never read; defined so nothing can read it
        end do
    end subroutine perm_exact

    !> Whether this seed's permutation is composed with the transposition `(0 1)`.
    !!
    !! **This is the parity correction, and it is correctness rather than mixing.** A Feistel over
    !! `Z_a x Z_b` cannot reach an odd permutation when `a == b` and `a` is odd: every round is a
    !! product of `a` cyclic shifts of `Z_a` (or of `Z_b`), a cyclic shift of `Z_q` by `t` has parity
    !! `(-1)**(q - gcd(q, t))` which is always even for odd `q`, and the half-swap between rounds is
    !! likewise even. So at `m = 25, 49, 81, 121, ...` -- `q**2` with `q` odd -- **exactly half of
    !! `S_m` is unreachable at any round count**, and the fraction of odd permutations reads 0.0 %.
    !! No amount of mixing closes that; only composing with an odd permutation does.
    !!
    !! One transposition is enough because it flips the parity of whatever the network produced, and
    !! the bit deciding it is unbiased: 0.4997 ones over 2 000 000 seeds, `z = -0.82`. Applying it
    !! after the cycle walk keeps it a permutation of `[0, m)` for every `m >= 2`.
    !!
    !! **`m` IS FOLDED INTO THE KEY, and dropping it re-creates a defect no single-`m` test can
    !! see.** Where the network is parity-locked its own contribution is *constant*, so the answer's
    !! parity is exactly this bit -- and if the bit depended on the seed alone, every locked size
    !! would share one parity under a given seed. Measured before the fold: pairwise agreement
    !! **1.0000** across m = 25, 49, 81, 121, 169, where a uniform pair agrees half the time; after
    !! it, 0.4984-0.5046. Each size on its own looked perfectly uniform in both cases, which is why
    !! `test_perm_parity` cannot see this and `test_perm_parity_cross_m` exists.
    !!
    !! The locked set is wider than the exact squares, which is worth knowing before deciding this
    !! is a corner case: sweeping raw network parity over m = 21..400 finds a band below each odd
    !! square -- 80-81, 119-121, 166-169, 221-225, 284-289, 354-361 -- exactly locked at `q**2` and
    !! 98-99 % determined just below it, where the cycle walk perturbs it without freeing it. That
    !! band grows as `sqrt(m)`.
    !!
    !! **Do not "simplify" this away because every structural test still passes without it** -- they
    !! all do. A whole-coset loss is invisible to fixed points, cycle counts, position marginals and
    !! inversions alike; only a parity test sees it. See `feature_risks.md` Risk-116.
    pure function perm_parity_flip(seed, m) result(f)
        integer(int64), intent(in) :: seed          !! the permutation family's seed
        integer(int64), intent(in) :: m             !! population size; see the note above
        logical :: f                                !! `.true.` when outputs 0 and 1 are swapped
        integer(int64) :: pk
        f = .false.
        if (perm_dbg_no_parity) return
        ! `m` rides in on the key material of the two evaluations that were already happening, so
        ! this costs two `ieor`s and no extra `perm_mix2`. Both halves of `m` are folded, or two
        ! populations differing only above bit 32 would share a bit.
        pk = perm_mix2(ieor(int(perm_parity_key, int64) * perm_c1, m), iand(seed, perm_m32))
        pk = ieor(pk, perm_mix2(ieor(int(perm_parity_key, int64), ishft(m, -32)), &
                                iand(ishft(seed, -32), perm_m32)))
        f = iand(pk, 1_int64) == 1_int64
    end function perm_parity_flip

    !> Applies the parity correction to one already-walked output in `[0, m)`.
    pure function perm_swap01(flip, y) result(z)
        logical, intent(in) :: flip                 !! from `perm_parity_flip`
        integer(int64), intent(in) :: y             !! an output in `[0, m)`, `m >= 2`
        integer(int64) :: z                         !! `y`, with 0 and 1 exchanged when `flip`
        z = y
        if (flip .and. y <= 1_int64) z = 1_int64 - y
    end function perm_swap01

    !> The width rule: `a = ceil(sqrt(m))`, `b = ceil(m/a)`, so `a*b` sits just above `m`.
    !!
    !! **This is contract, not tuning** -- it decides the cycle-walk orbit and therefore every value.
    !! It was chosen by measurement over two power-of-two alternatives: a balanced rule (`b` even)
    !! walks up to 2.68 applications per element at m = 10**8, an unbalanced one 1.34 and only when
    !! `ceil(log2(m))` happens to be odd, and this one 1.0000 at every size measured. The price is a
    !! division per application instead of a mask, which measured 22 % -- worth paying whenever the
    !! walk ratio exceeds about 1.22, which is most of its range.
    !!
    !! `a` is capped at `perm_a_max` so that `a * a` cannot overflow while the loops correct it.
    pure subroutine perm_factors(m, a, b)
        integer(int64), intent(in) :: m             !! population size, at least 2
        integer(int64), intent(out) :: a            !! left factor, `ceil(sqrt(m))`
        integer(int64), intent(out) :: b            !! right factor, `ceil(m/a)`
        a = int(sqrt(real(m, real64)), int64)
        if (a < 1_int64) a = 1_int64
        if (a > perm_a_max) a = perm_a_max
        do while (a < perm_a_max .and. a * a < m)
            a = a + 1_int64
        end do
        ! **The shrink loop cannot fire with an IEEE `sqrt`, and is kept anyway.** Reaching it needs
        ! `sqrt(real(m))` to overshoot the true square root by a whole integer, i.e. a relative error
        ! of `1 / sqrt(m)` -- about 2**-31 at the largest `m` this walks, where real64 gives 2**-53.
        ! Correctness here is contract rather than tuning (the factors decide the orbit and therefore
        ! every value), so the rule is written to be exact for ANY `sqrt` rather than for the one
        ! this machine has, and the loop is what makes the grow loop above safe to start below the
        ! answer. Excluded rather than deleted: no fixture can enter it, so it would otherwise report
        ! as uncovered for good.
        do while (a > 1_int64 .and. (a - 1_int64) * (a - 1_int64) >= m)
            a = a - 1_int64 ! GCOVR_EXCL_LINE
        end do
        b = (m + a - 1_int64) / a
    end subroutine perm_factors

    !> One Feistel round-function evaluation: two multiplies, and **no overflow is possible**.
    !!
    !! **Every operand is masked to 31 bits before each multiply, and that is a correctness
    !! requirement rather than tidiness.** The multipliers are below `2**32`, so each product is
    !! bounded by `(2**31 - 1) * (2**32 - 1) < 2**63` and no signed overflow can occur.
    !! `feature_risks.md` Risk-94 records this repository being caught with a wrapping multiply that
    !! *measured* correct while the optimiser used the overflow's undefinedness to delete a branch
    !! two functions away, so a new kernel must be free of it by construction. **Do not remove the
    !! masks**; the alternative, routing through `mul64_lo_strict`, is correct but builds the product
    !! from 16-bit limbs on the wrapping arm and is far too expensive for a per-round path.
    !!
    !! A Feistel network is a bijection for **any** round function, so the mask costs a little
    !! mixing and can cost nothing else. What it costs was measured: nothing on gfortran, and it is
    !! slightly *faster* than the unmasked form on ifx.
    pure function perm_mix2(rk, x) result(w)
        integer(int64), intent(in) :: rk            !! this round's key
        integer(int64), intent(in) :: x             !! the half being mixed, below `2**32`
        integer(int64) :: w                         !! a mixed word, below `2**32`
        integer(int64) :: z
        z = ieor(x, rk)
        z = iand(z, perm_m31) * perm_c1
        z = ieor(z, ishft(z, -29))
        z = iand(z, perm_m31) * perm_c2
        z = ieor(z, ishft(z, -31))
        w = iand(z, perm_m32)
    end function perm_mix2

    !> The round keys, derived from the seed once per call.
    !!
    !! Only the first `nr` are derived; the tail is zeroed rather than left undefined, so that a
    !! forced round count cannot leave a sanitiser reading uninitialised stack. Two `perm_mix2`
    !! evaluations per key, which is why the SCALAR path's cost is dominated by this schedule rather
    !! than by the network -- 16 rounds is 32 evaluations of setup against 16 of work. The bulk forms
    !! derive it once per call and so do not care.
    pure subroutine perm_round_keys(seed, nr, rk)
        integer(int64), intent(in) :: seed          !! the permutation family's seed
        integer, intent(in) :: nr                   !! rounds actually to be run
        integer(int64), intent(out) :: rk(perm_rounds)  !! one key per round; `rk(nr+1:)` is zero
        integer :: j
        do j = 1, nr
            rk(j) = perm_mix2(int(j, int64) * perm_c1, iand(seed, perm_m32))
            rk(j) = ieor(rk(j), perm_mix2(int(j, int64), iand(ishft(seed, -32), perm_m32)))
        end do
        do j = nr + 1, perm_rounds
            rk(j) = 0_int64
        end do
    end subroutine perm_round_keys

    !> One application of the network on `[0, a*b)`; a bijection for any round function.
    !!
    !! Splits `x` into `(l, r)` -- the one integer division on the scalar path -- and hands off to
    !! `perm_feistel_lr`, which is where the rounds actually are. The bulk fill skips this split
    !! entirely because its loop indices already are `(l, r)`; see that function's own note.
    pure function perm_feistel(rk, nr, a, b, x) result(y)
        integer(int64), intent(in) :: rk(perm_rounds)   !! the round keys
        integer, intent(in) :: nr                   !! rounds to run
        integer(int64), intent(in) :: a             !! left factor
        integer(int64), intent(in) :: b             !! right factor
        integer(int64), intent(in) :: x             !! input in `[0, a*b)`
        integer(int64) :: y                         !! output in `[0, a*b)`
        integer(int64) :: l, r
        l = x / b
        r = x - l * b
        y = perm_feistel_lr(rk, nr, a, b, l, r)
    end function perm_feistel

    !> The rounds themselves, from an ALREADY-SPLIT input -- the shape the bulk fill uses.
    !!
    !! One element, expressed as a block of one, so that `perm_feistel_block` stays the module's
    !! only copy of the round loop. The two `(1)`-shaped locals cost 24 bytes of stack against a
    !! scalar path that already derives a 16-key schedule per call.
    pure function perm_feistel_lr(rk, nr, a, b, l0, r0) result(y)
        integer(int64), intent(in) :: rk(perm_rounds)   !! the round keys
        integer, intent(in) :: nr                   !! rounds to run
        integer(int64), intent(in) :: a             !! left factor
        integer(int64), intent(in) :: b             !! right factor
        integer(int64), intent(in) :: l0            !! left half of the input, in `[0, a)`
        integer(int64), intent(in) :: r0            !! right half of the input, in `[0, b)`
        integer(int64) :: y                         !! output in `[0, a*b)`
        integer(int64) :: l(1), r(1), yy(1)
        l(1) = l0
        r(1) = r0
        call perm_feistel_block(rk, nr, a, b, 1, l, r, yy)
        y = yy(1)
    end function perm_feistel_lr

    !> `nb` independent elements through `nr` rounds. **The module's only copy of the round loop.**
    !!
    !! **The scalar and bulk entry points differ only in how they obtain `(l, r)`**, and both reach
    !! the rounds through here, so their agreement is structural rather than something two
    !! implementations have to be kept equal by testing. `feature_risks.md` Risk-109 is the rule that
    !! it stays that way, and it survived the transposition below intact.
    !!
    !! **The loops are ROUNDS OUTSIDE, ELEMENTS INSIDE, and that order is the whole point.** Run the
    !! other way round -- all `nr` rounds for one element, then the next element -- each element is a
    !! sequential dependency chain `nr` long and the loop runs at the latency of that chain instead
    !! of at the throughput of the machine. This way every round is a straight-line pass over `nb`
    !! independent values. Measured on machine B at `m = 10**6` and 16 rounds: **2.07x** at plain
    !! `-O3`, in a build containing no vector instruction at all -- the gain is pure instruction-level
    !! parallelism and needs no ISA assumption -- and **10.77x** with `-march=native`, which lands the
    !! 16-round kernel below the cost of the old four-round one. Do not reorder these loops.
    !!
    !! Three arithmetic details are worth keeping. The factors swap every round, which is why
    !! `perm_rounds` must be even -- an odd count leaves the output encoded against transposed
    !! factors. `t = l + F` needs at most **one** conditional subtraction because `l < p` and `F < p`,
    !! so no division is needed to reduce it; that subtraction is also what makes the round a
    !! bijection at all, and removing it makes the caller's cycle-walk **non-terminating** rather than
    !! merely wrong. And the multiply-shift takes the mixed word down to 31 bits before multiplying by
    !! `p`, which bounds that product below `2**63` for every `m` a caller can name -- shifting a full
    !! 32-bit word instead would overflow above `m ~ 2**62`.
    pure subroutine perm_feistel_block(rk, nr, a, b, nb, l, r, y)
        integer(int64), intent(in) :: rk(perm_rounds)   !! the round keys
        integer, intent(in) :: nr                   !! rounds to run
        integer(int64), intent(in) :: a             !! left factor
        integer(int64), intent(in) :: b             !! right factor
        integer, intent(in) :: nb                   !! elements in this block, at least 1
        integer(int64), intent(inout) :: l(nb)      !! in: left halves in `[0, a)`; out: enciphered
        integer(int64), intent(inout) :: r(nb)      !! in: right halves in `[0, b)`; out: enciphered
        integer(int64), intent(out) :: y(nb)        !! the rejoined outputs, in `[0, a*b)`
        integer(int64) :: t, p, q, sw
        integer :: e, j
        p = a
        q = b
        do j = 1, nr
            do e = 1, nb
                t = l(e) + ishft(iand(perm_mix2(rk(j), r(e)), perm_m31) * p, -31)
                if (t >= p) t = t - p
                l(e) = r(e)
                r(e) = t
            end do
            sw = p
            p = q
            q = sw
        end do
        do e = 1, nb
            y(e) = l(e) * q + r(e)
        end do
    end subroutine perm_feistel_block

    ! ---- The bulk forms ----

    !> `pf_random_permutation` for an `integer(int32)` result array.
    subroutine pf_random_permutation_i32(perm, seed, threads)
        integer(int32), intent(out) :: perm(:)      !! filled with the permutation of `1 .. size(perm)`
        integer(int64), intent(in) :: seed          !! the permutation family's seed
        integer, intent(in), optional :: threads    !! thread request; absent means automatic
        call perm_fill_i32(seed, size(perm, kind=int64), size(perm, kind=int64), perm, threads)
    end subroutine pf_random_permutation_i32

    !> `pf_random_permutation` for an `integer(int64)` result array.
    subroutine pf_random_permutation_i64(perm, seed, threads)
        integer(int64), intent(out) :: perm(:)      !! filled with the permutation of `1 .. size(perm)`
        integer(int64), intent(in) :: seed          !! the permutation family's seed
        integer, intent(in), optional :: threads    !! thread request; absent means automatic
        call perm_fill_i64(seed, size(perm, kind=int64), size(perm, kind=int64), perm, threads)
    end subroutine pf_random_permutation_i64

    !> `pf_random_subset` for an `integer(int32)` result and an `integer(int32)` population size.
    subroutine pf_random_subset_i32_i32(idx, m, seed, threads)
        integer(int32), intent(out) :: idx(:)       !! filled with the first `size(idx)` elements
        integer(int32), intent(in) :: m             !! population size; the permutation is of `1 .. m`
        integer(int64), intent(in) :: seed          !! the permutation family's seed
        integer, intent(in), optional :: threads    !! thread request; absent means automatic
        call subset_check(size(idx, kind=int64), int(m, int64), .true.)
        call perm_fill_i32(seed, int(m, int64), size(idx, kind=int64), idx, threads)
    end subroutine pf_random_subset_i32_i32

    !> `pf_random_subset` for an `integer(int32)` result and an `integer(int64)` population size.
    subroutine pf_random_subset_i32_i64(idx, m, seed, threads)
        integer(int32), intent(out) :: idx(:)       !! filled with the first `size(idx)` elements
        integer(int64), intent(in) :: m             !! population size; must not exceed `huge(int32)`
        integer(int64), intent(in) :: seed          !! the permutation family's seed
        integer, intent(in), optional :: threads    !! thread request; absent means automatic
        call subset_check(size(idx, kind=int64), m, .true.)
        call perm_fill_i32(seed, m, size(idx, kind=int64), idx, threads)
    end subroutine pf_random_subset_i32_i64

    !> `pf_random_subset` for an `integer(int64)` result and an `integer(int32)` population size.
    subroutine pf_random_subset_i64_i32(idx, m, seed, threads)
        integer(int64), intent(out) :: idx(:)       !! filled with the first `size(idx)` elements
        integer(int32), intent(in) :: m             !! population size; the permutation is of `1 .. m`
        integer(int64), intent(in) :: seed          !! the permutation family's seed
        integer, intent(in), optional :: threads    !! thread request; absent means automatic
        call subset_check(size(idx, kind=int64), int(m, int64), .false.)
        call perm_fill_i64(seed, int(m, int64), size(idx, kind=int64), idx, threads)
    end subroutine pf_random_subset_i64_i32

    !> `pf_random_subset` for an `integer(int64)` result and an `integer(int64)` population size.
    subroutine pf_random_subset_i64_i64(idx, m, seed, threads)
        integer(int64), intent(out) :: idx(:)       !! filled with the first `size(idx)` elements
        integer(int64), intent(in) :: m             !! population size; the permutation is of `1 .. m`
        integer(int64), intent(in) :: seed          !! the permutation family's seed
        integer, intent(in), optional :: threads    !! thread request; absent means automatic
        call subset_check(size(idx, kind=int64), m, .false.)
        call perm_fill_i64(seed, m, size(idx, kind=int64), idx, threads)
    end subroutine pf_random_subset_i64_i64

    !> The `pf_random_subset` preconditions, in one place so all four specifics state them alike.
    !!
    !! A zero-sized request returns without checking anything: it asks for no element, so neither
    !! `m` nor the representability of an element is a question about it.
    subroutine subset_check(n, m, narrow)
        integer(int64), intent(in) :: n             !! requested subset size, `size(idx)`
        integer(int64), intent(in) :: m             !! population size, widened
        logical, intent(in) :: narrow               !! `.true.` when `idx` is `integer(int32)`
        character(len=32) :: t1, t2
        if (n <= 0_int64) return
        if (m < 1_int64) then
            write (t1, '(i0)') m
            error stop "pf_random_subset: population size m must be at least 1, got " // trim(t1)
        end if
        if (n > m) then
            write (t1, '(i0)') n
            write (t2, '(i0)') m
            error stop "pf_random_subset: subset size " // trim(t1) // &
                       " exceeds population size " // trim(t2)
        end if
        if (narrow .and. m > int(huge(1_int32), int64)) then
            write (t1, '(i0)') m
            error stop "pf_random_subset: population size " // trim(t1) // &
                       " exceeds huge(int32); the elements need an integer(int64) array"
        end if
    end subroutine subset_check

    ! ---- pf_random_resample: draw with replacement -------------------------------------------

    !> The `pf_random_resample` preconditions, in one place so all twelve specifics state them alike.
    !!
    !! A zero-sized request returns without checking anything: it asks for no element, so neither `m`
    !! nor the representability of an element is a question about it.
    !!
    !! **The absent `n > m` test is the one line separating this from `subset_check`**, and it is
    !! absent on purpose: drawing with replacement has no such bound, and `n == m` is the most
    !! ordinary bootstrap there is. See the `pf_random_resample` interface.
    subroutine resample_check(n, m, narrow)
        integer(int64), intent(in) :: n             !! requested sample size, `size(idx)`
        integer(int64), intent(in) :: m             !! population size, widened
        logical, intent(in) :: narrow               !! `.true.` when `idx` is `integer(int32)`
        character(len=32) :: t1
        if (n <= 0_int64) return
        if (m < 1_int64) then
            write (t1, '(i0)') m
            error stop "pf_random_resample: population size m must be at least 1, got " // trim(t1)
        end if
        if (narrow .and. m > int(huge(1_int32), int64)) then
            write (t1, '(i0)') m
            error stop "pf_random_resample: population size " // trim(t1) // &
                       " exceeds huge(int32); the elements need an integer(int64) array"
        end if
    end subroutine resample_check

    !> The resample worker, `integer(int32)` result. `resample_i64` carries the design note.
    !!
    !! The early return after the check keeps `int(m, int32)` from being evaluated for a zero-sized
    !! request, which is not validated at all and so may still carry an `m` above `huge(int32)` --
    !! an out-of-range conversion, and a standard violation whatever a given compiler does with it.
    !!
    !! **It is defensive, not load-bearing, and that was established by mutation rather than
    !! assumed**: deleting it changes no observable behaviour, because `fill_draws_i32` returns on a
    !! zero-sized `v` before it reads `hi` at all. The whole suite passes without it, including under
    !! `--profile debug`. Keep it anyway -- it costs one comparison on a path that then returns --
    !! but do not expect a test to defend it, and do not add one that pins a wrapped value.
    subroutine resample_i32(idx, m, seed, stream, threads)
        integer(int32), intent(out) :: idx(:)       !! filled with `size(idx)` draws from `1 .. m`
        integer(int64), intent(in) :: m             !! population size, widened
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int64), intent(in) :: stream        !! replicate index, already defaulted
        integer, intent(in), optional :: threads    !! caller's request; absent means automatic
        integer(int64) :: n, chunk, lo, hi
        integer :: nth, t
        n = size(idx, kind=int64)
        call resample_check(n, m, .true.)
        if (n <= 0_int64) return
        nth = random_threads(n, threads)
        if (nth <= 1) then
            call pf_random_fill_draws(seed, stream, idx, 1_int32, int(m, int32), 1_int64)
            return
        end if
        chunk = (n + int(nth, int64) - 1_int64) / int(nth, int64)
#ifdef _OPENMP
        !$omp parallel do default(shared) private(t, lo, hi) schedule(static) num_threads(nth)
#endif
        do t = 0, nth - 1
            lo = int(t, int64) * chunk + 1_int64
            hi = min(n, lo + chunk - 1_int64)
            if (lo <= hi) call pf_random_fill_draws(seed, stream, idx(lo:hi), 1_int32, int(m, int32), lo)
        end do
#ifdef _OPENMP
        !$omp end parallel do
#endif
    end subroutine resample_i32

    !> The resample worker, `integer(int64)` result.
    !!
    !! **There is no resample-specific arithmetic here, and that is the design.** `pf_random_resample`
    !! is `pf_random_fill_draws` over `1 .. m` starting at draw 1, so it delegates rather than
    !! reimplementing: one integer rule, one rejection test, one grid. Every value it can return is
    !! already frozen by `pf_random_algorithm`, which is why this procedure needs no golden vectors of
    !! its own -- what the suite asserts instead is the identity with the fill and with the scalar
    !! draw, which is what would break if a future optimisation moved one of the three.
    !!
    !! **Threading splits the DRAW axis, and that is what makes it bit-identical rather than merely
    !! equivalent.** Element `k` is a pure function of `(seed, stream, k)`, so a chunk covering
    !! elements `lo .. hi` is exactly `fill_draws_i64` started at draw `lo` -- no per-thread state, no
    !! reduction, nothing to order. Chunk boundaries land anywhere, including in the middle of a
    !! block, which the fill's alignment head already handles; before the stage-2 restructure gave it
    !! one, an arbitrary starting draw would have needed a special case here. `num_threads(nth)` is
    !! load-bearing for the reason `perm_fill_i32` records at length: without it OpenMP opens the
    !! default team, 192 on machine B, on every call whatever the caller asked for.
    !!
    !! The work floor inside `random_threads` applies to an explicit `threads=` as well as to the
    !! automatic answer, so a small resample stays serial even when threading is requested -- a
    !! property of the work rather than of the caller's intent.
    subroutine resample_i64(idx, m, seed, stream, threads)
        integer(int64), intent(out) :: idx(:)       !! filled with `size(idx)` draws from `1 .. m`
        integer(int64), intent(in) :: m             !! population size
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int64), intent(in) :: stream        !! replicate index, already defaulted
        integer, intent(in), optional :: threads    !! caller's request; absent means automatic
        integer(int64) :: n, chunk, lo, hi
        integer :: nth, t
        n = size(idx, kind=int64)
        call resample_check(n, m, .false.)
        if (n <= 0_int64) return
        nth = random_threads(n, threads)
        if (nth <= 1) then
            call pf_random_fill_draws(seed, stream, idx, 1_int64, m, 1_int64)
            return
        end if
        chunk = (n + int(nth, int64) - 1_int64) / int(nth, int64)
#ifdef _OPENMP
        !$omp parallel do default(shared) private(t, lo, hi) schedule(static) num_threads(nth)
#endif
        do t = 0, nth - 1
            lo = int(t, int64) * chunk + 1_int64
            hi = min(n, lo + chunk - 1_int64)
            if (lo <= hi) call pf_random_fill_draws(seed, stream, idx(lo:hi), 1_int64, m, lo)
        end do
#ifdef _OPENMP
        !$omp end parallel do
#endif
    end subroutine resample_i64

    !> `pf_random_resample`, `integer(int32)` result and `integer(int32)` population, stream 1.
    subroutine pf_random_resample_i32_i32(idx, m, seed)
        integer(int32), intent(out) :: idx(:)       !! filled with `size(idx)` draws from `1 .. m`
        integer(int32), intent(in) :: m             !! population size
        integer(int64), intent(in) :: seed          !! the stream family's seed
        call resample_i32(idx, int(m, int64), seed, 1_int64)
    end subroutine pf_random_resample_i32_i32

    !> `pf_random_resample`, `integer(int32)` result and `integer(int64)` population, stream 1.
    subroutine pf_random_resample_i32_i64(idx, m, seed)
        integer(int32), intent(out) :: idx(:)       !! filled with `size(idx)` draws from `1 .. m`
        integer(int64), intent(in) :: m             !! population size
        integer(int64), intent(in) :: seed          !! the stream family's seed
        call resample_i32(idx, m, seed, 1_int64)
    end subroutine pf_random_resample_i32_i64

    !> `pf_random_resample`, `integer(int64)` result and `integer(int32)` population, stream 1.
    subroutine pf_random_resample_i64_i32(idx, m, seed)
        integer(int64), intent(out) :: idx(:)       !! filled with `size(idx)` draws from `1 .. m`
        integer(int32), intent(in) :: m             !! population size
        integer(int64), intent(in) :: seed          !! the stream family's seed
        call resample_i64(idx, int(m, int64), seed, 1_int64)
    end subroutine pf_random_resample_i64_i32

    !> `pf_random_resample`, `integer(int64)` result and `integer(int64)` population, stream 1.
    subroutine pf_random_resample_i64_i64(idx, m, seed)
        integer(int64), intent(out) :: idx(:)       !! filled with `size(idx)` draws from `1 .. m`
        integer(int64), intent(in) :: m             !! population size
        integer(int64), intent(in) :: seed          !! the stream family's seed
        call resample_i64(idx, m, seed, 1_int64)
    end subroutine pf_random_resample_i64_i64

    !> `pf_random_resample` with an `integer(int32)` stream; `integer(int32)` result and population.
    !!
    !! **The stream-carrying specifics are separate procedures rather than one with an optional
    !! dummy**, because an optional argument that differs only by kind cannot be the sole
    !! disambiguator in a generic interface -- a call omitting it would match both. This is the split
    !! `.claude/rules/api-conventions.md`'s "Numeric arguments: int32 and int64" note prescribes and `parquet_open_reader`
    !! already uses.
    subroutine pf_random_resample_i32_i32_s32(idx, m, seed, stream, threads)
        integer(int32), intent(out) :: idx(:)       !! filled with `size(idx)` draws from `1 .. m`
        integer(int32), intent(in) :: m             !! population size
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int32), intent(in) :: stream        !! replicate index
        integer, intent(in), optional :: threads    !! worker count; absent means automatic
        call resample_i32(idx, int(m, int64), seed, int(stream, int64), threads)
    end subroutine pf_random_resample_i32_i32_s32

    !> `pf_random_resample` with an `integer(int32)` stream; `int32` result, `int64` population.
    subroutine pf_random_resample_i32_i64_s32(idx, m, seed, stream, threads)
        integer(int32), intent(out) :: idx(:)       !! filled with `size(idx)` draws from `1 .. m`
        integer(int64), intent(in) :: m             !! population size
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int32), intent(in) :: stream        !! replicate index
        integer, intent(in), optional :: threads    !! worker count; absent means automatic
        call resample_i32(idx, m, seed, int(stream, int64), threads)
    end subroutine pf_random_resample_i32_i64_s32

    !> `pf_random_resample` with an `integer(int32)` stream; `int64` result, `int32` population.
    subroutine pf_random_resample_i64_i32_s32(idx, m, seed, stream, threads)
        integer(int64), intent(out) :: idx(:)       !! filled with `size(idx)` draws from `1 .. m`
        integer(int32), intent(in) :: m             !! population size
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int32), intent(in) :: stream        !! replicate index
        integer, intent(in), optional :: threads    !! worker count; absent means automatic
        call resample_i64(idx, int(m, int64), seed, int(stream, int64), threads)
    end subroutine pf_random_resample_i64_i32_s32

    !> `pf_random_resample` with an `integer(int32)` stream; `int64` result and population.
    subroutine pf_random_resample_i64_i64_s32(idx, m, seed, stream, threads)
        integer(int64), intent(out) :: idx(:)       !! filled with `size(idx)` draws from `1 .. m`
        integer(int64), intent(in) :: m             !! population size
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int32), intent(in) :: stream        !! replicate index
        integer, intent(in), optional :: threads    !! worker count; absent means automatic
        call resample_i64(idx, m, seed, int(stream, int64), threads)
    end subroutine pf_random_resample_i64_i64_s32

    !> `pf_random_resample` with an `integer(int64)` stream; `int32` result and population.
    subroutine pf_random_resample_i32_i32_s64(idx, m, seed, stream, threads)
        integer(int32), intent(out) :: idx(:)       !! filled with `size(idx)` draws from `1 .. m`
        integer(int32), intent(in) :: m             !! population size
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int64), intent(in) :: stream        !! replicate index
        integer, intent(in), optional :: threads    !! worker count; absent means automatic
        call resample_i32(idx, int(m, int64), seed, stream, threads)
    end subroutine pf_random_resample_i32_i32_s64

    !> `pf_random_resample` with an `integer(int64)` stream; `int32` result, `int64` population.
    subroutine pf_random_resample_i32_i64_s64(idx, m, seed, stream, threads)
        integer(int32), intent(out) :: idx(:)       !! filled with `size(idx)` draws from `1 .. m`
        integer(int64), intent(in) :: m             !! population size
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int64), intent(in) :: stream        !! replicate index
        integer, intent(in), optional :: threads    !! worker count; absent means automatic
        call resample_i32(idx, m, seed, stream, threads)
    end subroutine pf_random_resample_i32_i64_s64

    !> `pf_random_resample` with an `integer(int64)` stream; `int64` result, `int32` population.
    subroutine pf_random_resample_i64_i32_s64(idx, m, seed, stream, threads)
        integer(int64), intent(out) :: idx(:)       !! filled with `size(idx)` draws from `1 .. m`
        integer(int32), intent(in) :: m             !! population size
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int64), intent(in) :: stream        !! replicate index
        integer, intent(in), optional :: threads    !! worker count; absent means automatic
        call resample_i64(idx, int(m, int64), seed, stream, threads)
    end subroutine pf_random_resample_i64_i32_s64

    !> `pf_random_resample` with an `integer(int64)` stream; `int64` result and population.
    subroutine pf_random_resample_i64_i64_s64(idx, m, seed, stream, threads)
        integer(int64), intent(out) :: idx(:)       !! filled with `size(idx)` draws from `1 .. m`
        integer(int64), intent(in) :: m             !! population size
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int64), intent(in) :: stream        !! replicate index
        integer, intent(in), optional :: threads    !! worker count; absent means automatic
        call resample_i64(idx, m, seed, stream, threads)
    end subroutine pf_random_resample_i64_i64_s64

    !> How many threads a bulk permutation of `n` elements should use.
    !!
    !! **Both OpenMP rules come from `parquet_settings_base`** -- `parquet_auto_thread_count` for
    !! the automatic answer and `parquet_nested_team_unsafe` for the Risk-104 deadlock guard on an
    !! explicit request. This module deliberately holds no copy of either; CLAUDE.md's auto-threading
    !! note names a further copy as the mistake, and `pf_sort_threads` asks the identical questions.
    !!
    !! What is specific to this module is the **work floor**. It applies to an explicit `threads=`
    !! as well as to the automatic answer, because it asks whether the array is worth splitting at
    !! all -- a property of the work, not of the caller's intent -- and threading a small
    !! permutation is measurably *worse* than not threading it.
    integer function random_threads(n, threads) result(nth)
        integer(int64), intent(in) :: n             !! elements to produce
        integer, intent(in), optional :: threads    !! caller's request; absent means automatic
        integer(int64) :: want, floor_per, by_work
        if (present(threads)) then
            want = max(1_int64, int(threads, int64))
            if (parquet_nested_team_unsafe()) want = 1_int64
        else
            want = int(parquet_auto_thread_count(parquet_get_random_threads(), "random draws"), int64)
        end if
        floor_per = parquet_get_random_parallel_min_elements()
        if (floor_per > 0_int64) then
            by_work = n / floor_per                 ! how many threads this much work can feed
            if (want > by_work) want = by_work
        end if
        if (want > n) want = n                      ! never more threads than elements
        if (want < 1_int64) want = 1_int64
        nth = int(want, int32)
    end function random_threads

    !> Exposes `random_threads` for testing. **Test-only**; no library code calls it.
    !!
    !! Public because the rule it reports lives behind a private procedure in a module with no
    !! `bind(C)` surface, so the C++-side debug-hook convention this library prefers is unavailable
    !! here -- the same position `parquet_debug_string_bulk_threads` is in, and this mirrors it
    !! deliberately rather than inventing a second shape. See CLAUDE.md, "A Fortran-side debug hook
    !! has to be PUBLIC, so prefer a C++ one".
    !!
    !! **It RE-COMPUTES the rule rather than reporting what a call did**, which is the same
    !! limitation the string-column hook has and is worth stating: it can assert that
    !! `parquet_set_random_parallel_min_elements` changes the answer, and it cannot assert that the
    !! fill went on to honour that answer. The bit-identity of the 1-thread and N-thread results is
    !! what the suite checks instead, and `bench/probe_random_perm.f90 --mode=floor` is what shows the
    !! threading actually happens. See `feature_risks.md` Risk-111.
    integer function parquet_debug_random_bulk_threads(n, threads) result(nth)
        integer(int64), intent(in) :: n             !! elements a bulk call would produce
        integer, intent(in), optional :: threads    !! the caller's request, if any
        nth = random_threads(n, threads)
    end function parquet_debug_random_bulk_threads

    !> Forces the Feistel round count. **Test-only.** `n <= 0` restores the compiled-in value.
    !!
    !! Public because it has to be: the value lives in Fortran, and this module reaches no `bind(C)`
    !! surface, so the C++-side debug-hook convention the rest of the library uses is unavailable
    !! here. See `.claude/rules/testing.md`, "Debug hooks".
    !!
    !! **It is load-bearing rather than convenient, and that is a consequence of the exact path.**
    !! Every `m` small enough to enumerate all `m!` permutations of is also small enough to be
    !! answered exactly, so the shipped kernel is uniform by construction at exactly the sizes an
    !! exhaustive test can reach -- and an exhaustive test that can only ever confirm a construction
    !! proves nothing about the round count. Forcing the count (with
    !! `parquet_debug_set_perm_force_feistel`, which sends those sizes back through the network) is
    !! the only way a test can still show that 16 rounds is clean where 4, 8 and 12 are not.
    !!
    !! It is excluded from README.md's API overview, no library code calls it, and it has no
    !! counterpart in the public contract: `pf_random_perm_algorithm` names the compiled-in count,
    !! and this does not change that string.
    subroutine parquet_debug_set_perm_rounds(n)
        integer, intent(in) :: n                    !! rounds to force; `<= 0` restores the default
        if (n <= 0) then
            perm_dbg_rounds = 0
        else
            perm_dbg_rounds = min(n, perm_rounds)
        end if
    end subroutine parquet_debug_set_perm_rounds

    !> Enables or disables the parity correction. **Test-only**; see `parquet_debug_set_perm_rounds`.
    subroutine parquet_debug_set_perm_parity(on)
        logical, intent(in) :: on                   !! `.false.` suppresses the correction
        perm_dbg_no_parity = .not. on
    end subroutine parquet_debug_set_perm_parity

    !> Lowers the population ceiling the three int32-index refusals test against. **Test-only.**
    !!
    !! Public for the reason `parquet_debug_set_perm_rounds` gives, and it exists because the real
    !! ceiling is `huge(int32)` items -- see `wd_dbg_int32_limit`, which records why no fixture can
    !! reach it. `n < 1` restores the real one.
    subroutine parquet_debug_set_weighted_int32_limit(n)
        integer(int64), intent(in) :: n             !! forced ceiling in items; `< 1` restores the real one
        if (n < 1_int64) then
            wd_dbg_int32_limit = -1_int64
        else
            wd_dbg_int32_limit = n
        end if
    end subroutine parquet_debug_set_weighted_int32_limit

    !> The population ceiling an `integer(int32)` item index imposes, after any debug override.
    pure function wd_int32_limit() result(n)
        integer(int64) :: n                         !! largest population an int32 index can name
        n = int(huge(1_int32), int64)
        if (wd_dbg_int32_limit >= 1_int64) n = wd_dbg_int32_limit
    end function wd_int32_limit

    !> Sends `m <= perm_exact_max` through the Feistel. **Test-only**; see the two hooks above.
    subroutine parquet_debug_set_perm_force_feistel(on)
        logical, intent(in) :: on                   !! `.true.` bypasses the exact path
        perm_dbg_force_feistel = on
    end subroutine parquet_debug_set_perm_force_feistel

    !> Reports the permutation kernel's effective configuration. **Test-only.**
    !!
    !! An observation hook rather than a fourth override, and it exists because the three setters
    !! above have no readback: a test that restores the defaults cannot otherwise assert it really
    !! did, and one suite leaking a forced round count into another would show up as an unrelated
    !! golden-vector failure in whichever test happened to run next.
    subroutine parquet_debug_perm_config(rounds, parity, exact_max)
        integer, intent(out) :: rounds              !! rounds the kernel will actually run
        logical, intent(out) :: parity              !! whether the parity correction is applied
        integer, intent(out) :: exact_max           !! largest `m` answered exactly; 0 when forced off
        rounds = perm_round_count()
        parity = .not. perm_dbg_no_parity
        exact_max = 0
        if (.not. perm_dbg_force_feistel) exact_max = perm_exact_max
    end subroutine parquet_debug_perm_config

    !> The bulk permutation fill, `integer(int32)` result. `perm_fill_i64` carries the design note.
    subroutine perm_fill_i32(seed, m, n, v, threads)
        integer(int64), intent(in) :: seed          !! the permutation family's seed
        integer(int64), intent(in) :: m             !! population size, at least 1 when `n > 0`
        integer(int64), intent(in) :: n             !! how many elements to produce; `0 <= n <= m`
        integer(int32), intent(out) :: v(:)         !! filled with elements `1 .. n`
        integer, intent(in), optional :: threads    !! caller's request; absent means automatic
        integer(int64) :: a, b, rk(perm_rounds), lo, hi, chunk, small(perm_exact_max)
        integer :: nth, t, nr, i
        logical :: flip
        if (n <= 0_int64) return
        if (m <= 1_int64) then
            v(1) = 1_int32                          ! `n <= m` leaves only n == m == 1 here
            return
        end if
        if (perm_use_exact(m)) then
            ! `n <= m <= perm_exact_max`, so this is at most 20 elements and never worth threading.
            call perm_exact(seed, m, small)
            do i = 1, int(n)
                v(i) = int(small(i), int32)
            end do
            return
        end if
        call perm_factors(m, a, b)
        nr = perm_round_count()
        flip = perm_parity_flip(seed, m)
        call perm_round_keys(seed, nr, rk)
        nth = random_threads(n, threads)
        if (nth <= 1) then
            call perm_range_i32(m, a, b, rk, nr, flip, 1_int64, n, v)
            return
        end if
        chunk = (n + int(nth, int64) - 1_int64) / int(nth, int64)
#ifdef _OPENMP
        ! **`num_threads(nth)` is load-bearing, not decoration.** Without it OpenMP opens the
        ! DEFAULT team -- 384 threads on machine B -- and then runs a loop with `nth`
        ! iterations, so every call pays to create and destroy a full team whatever the
        ! caller asked for. Measured before the clause was added: `threads=2` on a
        ! 1000-element permutation cost 11.3 ms against 10 us serial, a fixed ~6-11 ms on
        ! every call at every size, which reads as an absurdly expensive work floor rather
        ! than as a missing clause.
        !$omp parallel do default(shared) private(t, lo, hi) schedule(static) num_threads(nth)
#endif
        do t = 0, nth - 1
            lo = int(t, int64) * chunk + 1_int64
            hi = min(n, lo + chunk - 1_int64)
            if (lo <= hi) call perm_range_i32(m, a, b, rk, nr, flip, lo, hi, v)
        end do
#ifdef _OPENMP
        !$omp end parallel do
#endif
    end subroutine perm_fill_i32

    !> Fills `v(lo:hi)` with elements `lo .. hi` of the permutation. `perm_range_i64` has the note.
    pure subroutine perm_range_i32(m, a, b, rk, nr, flip, lo, hi, v)
        integer(int64), intent(in) :: m             !! population size, at least 2
        integer(int64), intent(in) :: a             !! left factor
        integer(int64), intent(in) :: b             !! right factor
        integer(int64), intent(in) :: rk(perm_rounds)   !! the round keys
        integer, intent(in) :: nr                   !! rounds to run
        logical, intent(in) :: flip                 !! the seed's parity correction
        integer(int64), intent(in) :: lo            !! first element index, 1-based
        integer(int64), intent(in) :: hi            !! last element index, 1-based
        integer(int32), intent(inout) :: v(:)       !! only `v(lo:hi)` is written
        integer(int64) :: lb(perm_block), rb(perm_block), yb(perm_block)
        integer(int64) :: l, r, y, k
        integer :: e, nb
        k = lo
        l = (lo - 1_int64) / b
        r = (lo - 1_int64) - l * b
        do while (k <= hi)
            nb = int(min(int(perm_block, int64), hi - k + 1_int64))
            ! (1) The block's inputs. One branch per element, in a prologue rather than in the
            !     rounds -- `l` advances only when `r` wraps at `b`.
            do e = 1, nb
                lb(e) = l
                rb(e) = r
                r = r + 1_int64
                if (r >= b) then
                    r = 0_int64
                    l = l + 1_int64
                end if
            end do
            ! (2) The rounds, transposed: see `perm_feistel_block`.
            call perm_feistel_block(rk, nr, a, b, nb, lb, rb, yb)
            ! (3) The cycle walk, the parity correction and the store. The walk is a FIXUP pass
            !     rather than part of the main loop, which is what lets (2) stay branch-free; it
            !     touches only the `a*b - m` values that land outside `[0, m)`, about `sqrt(m)` of
            !     `m` -- 0.1 % at m = 10**6 -- which is why that split is nearly free here.
            do e = 1, nb
                y = yb(e)
                do while (y >= m)
                    y = perm_feistel(rk, nr, a, b, y)
                end do
                v(k + int(e - 1, int64)) = int(perm_swap01(flip, y) + 1_int64, int32)
            end do
            k = k + int(nb, int64)
        end do
    end subroutine perm_range_i32

    !> The bulk permutation fill, `integer(int64)` result -- and the division-free `(l, r)` shape.
    !!
    !! **Three things move out of the per-element path here, and the division is the smallest.**
    !!
    !! The division first, since it names the shape: `perm_at_impl` must compute `l = x/b` and
    !! `r = x - l*b` for each `k`, whereas walking `l` in the outer loop and `r` in the inner one
    !! enumerates `x = l*b + r` as `0, 1, 2, ...` -- exactly the sequence of `k - 1` the caller
    !! asked for, with the split already in hand.
    !!
    !! **The SETUP is the larger saving, and it is easy to miss because it is not in this loop.**
    !! `pf_random_perm_at` is `pure elemental` and stateless, so every single call re-derives the
    !! width rule (`perm_factors`: a `sqrt` and two correction loops) and the whole key schedule
    !! (`perm_round_keys`: **32** `perm_mix2` evaluations at 16 rounds, against the 16 the network
    !! itself performs). Here both happen once per call, whatever `n` is.
    !!
    !! **And the round loop is TRANSPOSED here, which the scalar form cannot do** -- one element has
    !! nothing to interleave with. See `perm_feistel_block`; it is worth 2.07x on machine B with no
    !! vector instruction involved, so the bulk-versus-scalar gap is wider than the 2.4x measured
    !! before either change (machine B, gfortran 14.2.1, `--profile release`, four rounds: 24.1 ns
    !! per element scalar against 9.9 ns bulk at `m = 10**8`).
    !!
    !! **The cycle-walk keeps the division and that is fine**, because it is entered essentially
    !! never -- 1.0000 applications per element at every size measured -- so it reuses the scalar
    !! `perm_feistel` rather than duplicating a split-aware walk.
    !!
    !! **The loop bounds are the highest-risk edit in this file.** An off-by-one produces an array
    !! that looks entirely plausible and is not a permutation. `feature_risks.md` Risk-109.
    !!
    !! **Threading is answer-invariant here by construction, not by care.** Element `k` is a pure
    !! function of `(seed, m, k)` and reads nothing any other element writes, so splitting `1 .. n`
    !! into contiguous chunks cannot change a value -- which is why `threads=` is admissible on this
    !! call at all, and why the 1-thread and N-thread outputs are asserted bit-identical rather than
    !! merely statistically similar. The factors and the round keys are derived **before** the
    !! region and shared read-only, so each thread pays one division at entry and none per element.
    subroutine perm_fill_i64(seed, m, n, v, threads)
        integer(int64), intent(in) :: seed          !! the permutation family's seed
        integer(int64), intent(in) :: m             !! population size, at least 1 when `n > 0`
        integer(int64), intent(in) :: n             !! how many elements to produce; `0 <= n <= m`
        integer(int64), intent(out) :: v(:)         !! filled with elements `1 .. n`
        integer, intent(in), optional :: threads    !! caller's request; absent means automatic
        integer(int64) :: a, b, rk(perm_rounds), lo, hi, chunk, small(perm_exact_max)
        integer :: nth, t, nr, i
        logical :: flip
        if (n <= 0_int64) return
        if (m <= 1_int64) then
            v(1) = 1_int64                          ! `n <= m` leaves only n == m == 1 here
            return
        end if
        if (perm_use_exact(m)) then
            ! `n <= m <= perm_exact_max`, so this is at most 20 elements and never worth threading.
            call perm_exact(seed, m, small)
            do i = 1, int(n)
                v(i) = small(i)
            end do
            return
        end if
        call perm_factors(m, a, b)
        nr = perm_round_count()
        flip = perm_parity_flip(seed, m)
        call perm_round_keys(seed, nr, rk)
        nth = random_threads(n, threads)
        if (nth <= 1) then
            call perm_range_i64(m, a, b, rk, nr, flip, 1_int64, n, v)
            return
        end if
        chunk = (n + int(nth, int64) - 1_int64) / int(nth, int64)
#ifdef _OPENMP
        ! **`num_threads(nth)` is load-bearing, not decoration.** Without it OpenMP opens the
        ! DEFAULT team -- 384 threads on machine B -- and then runs a loop with `nth`
        ! iterations, so every call pays to create and destroy a full team whatever the
        ! caller asked for. Measured before the clause was added: `threads=2` on a
        ! 1000-element permutation cost 11.3 ms against 10 us serial, a fixed ~6-11 ms on
        ! every call at every size, which reads as an absurdly expensive work floor rather
        ! than as a missing clause.
        !$omp parallel do default(shared) private(t, lo, hi) schedule(static) num_threads(nth)
#endif
        do t = 0, nth - 1
            lo = int(t, int64) * chunk + 1_int64
            hi = min(n, lo + chunk - 1_int64)
            if (lo <= hi) call perm_range_i64(m, a, b, rk, nr, flip, lo, hi, v)
        end do
#ifdef _OPENMP
        !$omp end parallel do
#endif
    end subroutine perm_fill_i64

    !> Fills `v(lo:hi)` with elements `lo .. hi` of the permutation, from an arbitrary start.
    !!
    !! **This is the only place elements are produced**, serial and threaded alike -- a thread's
    !! chunk and the whole array are the same call with different bounds. That is deliberate: two
    !! shapes for one computation is how a threaded path comes to disagree with its serial twin
    !! over some boundary nobody tested.
    !!
    !! **One division per CALL, not per element.** Entering at `lo` needs `(l, r)` for
    !! `x = lo - 1`, which costs a division once; from there the pair advances by the loop
    !! structure exactly as the whole-array walk does.
    !!
    !! **Three passes per block, and the split into three is what buys the speed.** The inputs are
    !! gathered first, so the `r`-wraps-at-`b` branch sits in a prologue; then `perm_feistel_block`
    !! runs every round over the whole block with no branch and no carried dependency; then the
    !! cycle walk, the parity correction and the store happen in a fixup pass. Folding the walk
    !! back into the middle pass would undo the transposition, because it is a `do while` whose
    !! trip count differs per element -- and it can be a fixup precisely because it is entered
    !! essentially never (1.0000 applications per element at every size measured).
    pure subroutine perm_range_i64(m, a, b, rk, nr, flip, lo, hi, v)
        integer(int64), intent(in) :: m             !! population size, at least 2
        integer(int64), intent(in) :: a             !! left factor
        integer(int64), intent(in) :: b             !! right factor
        integer(int64), intent(in) :: rk(perm_rounds)   !! the round keys
        integer, intent(in) :: nr                   !! rounds to run
        logical, intent(in) :: flip                 !! the seed's parity correction
        integer(int64), intent(in) :: lo            !! first element index, 1-based
        integer(int64), intent(in) :: hi            !! last element index, 1-based
        integer(int64), intent(inout) :: v(:)       !! only `v(lo:hi)` is written
        integer(int64) :: lb(perm_block), rb(perm_block), yb(perm_block)
        integer(int64) :: l, r, y, k
        integer :: e, nb
        k = lo
        l = (lo - 1_int64) / b
        r = (lo - 1_int64) - l * b
        do while (k <= hi)
            nb = int(min(int(perm_block, int64), hi - k + 1_int64))
            ! (1) The block's inputs. One branch per element, in a prologue rather than in the
            !     rounds -- `l` advances only when `r` wraps at `b`.
            do e = 1, nb
                lb(e) = l
                rb(e) = r
                r = r + 1_int64
                if (r >= b) then
                    r = 0_int64
                    l = l + 1_int64
                end if
            end do
            ! (2) The rounds, transposed: see `perm_feistel_block`.
            call perm_feistel_block(rk, nr, a, b, nb, lb, rb, yb)
            ! (3) The cycle walk, the parity correction and the store. The walk is a FIXUP pass
            !     rather than part of the main loop, which is what lets (2) stay branch-free; it
            !     touches only the `a*b - m` values that land outside `[0, m)`, about `sqrt(m)` of
            !     `m` -- 0.1 % at m = 10**6 -- which is why that split is nearly free here.
            do e = 1, nb
                y = yb(e)
                do while (y >= m)
                    y = perm_feistel(rk, nr, a, b, y)
                end do
                v(k + int(e - 1, int64)) = perm_swap01(flip, y) + 1_int64
            end do
            k = k + int(nb, int64)
        end do
    end subroutine perm_range_i64

    ! ================================================================================
    ! Tier 2 -- the weighted sequential draw
    ! ================================================================================
    !
    ! Three properties of the descent are load-bearing, and each fails SILENTLY if it is dropped --
    ! a wrong answer with nothing to report it, which is why all three are stated here rather than
    ! left to be inferred from the code.
    !
    !  1. **An ancestor is RECOMPUTED from its two children, never decremented.** Subtracting the
    !     drawn weight folds another rounding error into a running value at every draw, so the root
    !     drifts away from the true remaining weight and the descent starts landing on spent
    !     leaves. Measured during design at 16% duplicate draws over 18 decades of dynamic range --
    !     and, less comfortably, at 100 duplicates with plain uniform(0,1) weights, so this is not
    !     an exotic-input problem. Recomputing costs exactly the same and is exact to one ulp.
    !
    !  2. **Exhaustion is tested on a live integer COUNT, never on the root's weight.** This one is
    !     REDUNDANT GIVEN (1) and is kept anyway, in the same way `column_has_nulls_from_footer`'s
    !     two guards are individually redundant and jointly load-bearing: because ancestors are
    !     recomputed, a drawn leaf is exactly `0.0` and any node above only-drawn leaves is an
    !     exact sum of exact zeros, so `st(1) > 0` and `live > 0` are the same predicate. Mutating
    !     the test to the root's weight duly survives the whole suite. It stops being redundant the
    !     moment (1) is weakened, which is exactly when nothing else would notice -- and the count
    !     is what makes `%remaining` an exact `O(1)` answer rather than a question about rounding.
    !
    !  3. **The descent may never enter a subtree whose sum is zero.** `st(p)` is the rounded sum
    !     of its children and can exceed their exact total, so a `target` just below `st(p)` can
    !     exceed `st(left) + st(right)`; a root-only clamp does not close that, because the excess
    !     reappears at every level. `wd_draw` tests each child for liveness before descending.
    !
    !     **This is a DEFENSIVE branch and its mutation survives the suite -- deliberately, not for
    !     want of trying.** The obvious route into a dead subtree is already closed by (1): a dead
    !     subtree sums to EXACTLY zero, and `x + 0.0` is exact, so a node with one live child
    !     equals that child exactly and `target < st(p)` implies `target < st(live child)`. What
    !     remains is the case of two live children where the parent's rounding lets `target`
    !     overshoot into the right subtree by up to an ulp, and that overshoot then cascades onto a
    !     dead leaf further down. That needs `u` within about `2**-52` of 1 at a specific node, so
    !     no fixture this repository can build will reach it. The guard costs two comparisons and
    !     turns "vanishingly unlikely" into "cannot happen"; do not delete it on the strength of a
    !     coverage report or a surviving mutant.
    !
    !     A zero-weight leaf would defeat this test -- it is indistinguishable from a spent one --
    !     which is the real reason such items are held outside the tree rather than given a leaf.

    !> Validates `weights` and builds the tree. The shared worker behind all three `%init` forms.
    subroutine wd_build(self, weights)
        class(pf_weighted_draw), intent(inout) :: self  !! the sampler, freshly guarded
        real(real64), intent(in) :: weights(:)          !! one weight per item; all finite, all `>= 0`
        integer(int64) :: n, i, j, z, p, p2

        n = size(weights, kind=int64)
        if (n < 1_int64) error stop "pf_weighted_draw%init: weights must have at least one element"
        self%npos = 0_int64
        self%nzero = 0_int64
        do i = 1_int64, n
            if (ieee_is_nan(weights(i))) &
                error stop "pf_weighted_draw%init: a weight is NaN; a NaN compares false against " // &
                           "every bound and would silently be treated as zero"
            if (.not. ieee_is_finite(weights(i))) &
                error stop "pf_weighted_draw%init: a weight is infinite; the total weight would be " // &
                           "infinite and every draw degenerate"
            if (weights(i) < 0.0_real64) &
                error stop "pf_weighted_draw%init: a weight is negative; a weight is a relative " // &
                           "frequency and cannot be below zero"
            if (weights(i) >= wd_min_weight) then
                self%npos = self%npos + 1_int64
            else
                self%nzero = self%nzero + 1_int64
            end if
        end do
        if (self%npos == 0_int64) &
            error stop "pf_weighted_draw%init: every weight is zero; there is no distribution to " // &
                       "draw from"

        p2 = 1_int64
        do while (p2 < self%npos)
            p2 = 2_int64 * p2
        end do
        self%p2 = p2
        if (allocated(self%st)) deallocate(self%st)
        allocate(self%st(2_int64 * p2 - 1_int64))
        ! Only the PADDING leaves need clearing: every internal node is written by the sweep below
        ! and every real leaf by the fill. Blanking the whole array first would double the build's
        ! memory traffic, which is what the build costs.
        self%st(p2 + self%npos : 2_int64 * p2 - 1_int64) = 0.0_real64

        ! The item map exists only when a zero weight is present. Without one, leaf j IS item j,
        ! and skipping the map saves an int64 per item -- worth having, since a parallel caller
        ! holds one sampler per thread.
        if (allocated(self%leaf_item)) deallocate(self%leaf_item)
        if (allocated(self%zero_item)) deallocate(self%zero_item)
        if (self%nzero > 0_int64) then
            allocate(self%leaf_item(self%npos))
            allocate(self%zero_item(self%nzero))
        end if
        j = 0_int64
        z = 0_int64
        do i = 1_int64, n
            if (weights(i) >= wd_min_weight) then
                j = j + 1_int64
                self%st(p2 + j - 1_int64) = weights(i)
                if (self%nzero > 0_int64) self%leaf_item(j) = i
            else
                z = z + 1_int64
                self%zero_item(z) = i
            end if
        end do
        do p = p2 - 1_int64, 1_int64, -1_int64
            self%st(p) = self%st(2_int64 * p) + self%st(2_int64 * p + 1_int64)
        end do

        self%live = self%npos
        self%ndrawn = 0_int64
        self%jr_n = 0_int64
        self%ready = .true.
    end subroutine wd_build

    !> Installs the coordinates and derives the zero-weight tail's own seed from them.
    pure subroutine wd_set_coords(self, seed, stream)
        class(pf_weighted_draw), intent(inout) :: self  !! the sampler
        integer(int64), intent(in) :: seed              !! the seed steering the descent
        integer(int64), intent(in) :: stream            !! which sequence of that seed

        ! Domain separation, not decoration: without it this family and the race read the SAME
        ! draw 1 of (seed, stream) and chose the lowest-weight item together. See `wd_family_label`.
        self%wseed = pf_random_key(seed, wd_family_label)
        self%wstream = stream
        ! Two derivations rather than one: the tail's order must vary with BOTH coordinates, and
        ! folding the stream into a label instead could collide with an ordinary stream index. It
        ! starts from the family-derived seed, so the two families' tails are separated too -- they
        ! were previously identical, which is the same coupling in its most extreme form.
        self%zkey = pf_random_key(pf_random_key(self%wseed, stream), wd_zero_label)
    end subroutine wd_set_coords

    !> Refuses a call on a sampler `%init` has not prepared.
    subroutine wd_require_ready(self, proc)
        class(pf_weighted_draw), intent(in) :: self     !! the sampler
        character(len=*), intent(in) :: proc            !! the calling binding, for the message

        if (.not. self%ready) &
            error stop "pf_weighted_draw%" // proc // ": this sampler has not been initialised; " // &
                       "call %init(weights, seed) first"
    end subroutine wd_require_ready

    !> Records one drawn leaf so `%reset`/`%reseed` can put it back. Doubles on overflow.
    subroutine wd_push(self, pos, val)
        class(pf_weighted_draw), intent(inout) :: self  !! the sampler
        integer(int64), intent(in) :: pos               !! the leaf node just zeroed
        real(real64), intent(in) :: val                 !! the weight it held
        integer(int64), allocatable :: np(:)
        real(real64), allocatable :: nv(:)
        integer(int64) :: cap

        if (.not. allocated(self%jr_pos)) then
            allocate(self%jr_pos(wd_journal_min))
            allocate(self%jr_val(wd_journal_min))
        else
            cap = size(self%jr_pos, kind=int64)
            if (self%jr_n >= cap) then
                allocate(np(2_int64 * cap))
                allocate(nv(2_int64 * cap))
                np(1:cap) = self%jr_pos
                nv(1:cap) = self%jr_val
                call move_alloc(np, self%jr_pos)
                call move_alloc(nv, self%jr_val)
            end if
        end if
        self%jr_n = self%jr_n + 1_int64
        self%jr_pos(self%jr_n) = pos
        self%jr_val(self%jr_n) = val
    end subroutine wd_push

    !> Puts every drawn leaf back and recomputes the paths above them.
    !!
    !! **The result is bit-identical to a fresh `%init`, and that is provable rather than hoped
    !! for.** Every internal node is a pure function of its two children, and the last restore that
    !! writes a given node necessarily writes it after both children have reached their final
    !! values -- any restore touching a child touches that node too, so none can come later. So the
    !! order entries are replayed in does not matter either.
    subroutine wd_restore(self)
        class(pf_weighted_draw), intent(inout) :: self  !! the sampler
        integer(int64) :: t, p

        do t = self%jr_n, 1_int64, -1_int64
            p = self%jr_pos(t)
            self%st(p) = self%jr_val(t)
            p = p / 2_int64
            do while (p >= 1_int64)
                self%st(p) = self%st(2_int64 * p) + self%st(2_int64 * p + 1_int64)
                p = p / 2_int64
            end do
        end do
        self%jr_n = 0_int64
        self%live = self%npos
        self%ndrawn = 0_int64
    end subroutine wd_restore

    !> One draw: descend the tree by weight, remove the item, journal what was removed.
    subroutine wd_draw(self, item, ok)
        class(pf_weighted_draw), intent(inout) :: self  !! the sampler
        integer(int64), intent(out) :: item             !! the item drawn, or 0 when exhausted
        logical, intent(out) :: ok                      !! `.false.` once every item has been drawn
        integer(int64) :: p, l, r, leaf
        real(real64) :: u, target

        item = 0_int64
        ok = .false.
        if (self%live > 0_int64) then
            self%ndrawn = self%ndrawn + 1_int64
            u = pf_random_at(self%wseed, self%wstream, draw=self%ndrawn)
            target = u * self%st(1_int64)
            ! `u` reaches 1 - 2**-53 and the product is rounded, so `target` can land exactly on
            ! the root. Written as a negated `<` so a NaN -- which cannot arise here, the weights
            ! having been validated, but which would otherwise walk the tree silently -- also lands
            ! on the safe branch.
            if (.not. (target < self%st(1_int64))) target = 0.0_real64
            p = 1_int64
            do while (p < self%p2)
                l = 2_int64 * p
                r = l + 1_int64
                if (self%st(l) > 0.0_real64 .and. &
                    (self%st(r) <= 0.0_real64 .or. target < self%st(l))) then
                    if (target >= self%st(l)) target = 0.0_real64
                    p = l
                else
                    target = target - self%st(l)
                    if (target < 0.0_real64) target = 0.0_real64
                    p = r
                end if
            end do
            leaf = p - self%p2 + 1_int64
            call wd_push(self, p, self%st(p))
            self%st(p) = 0.0_real64
            p = p / 2_int64
            do while (p >= 1_int64)
                self%st(p) = self%st(2_int64 * p) + self%st(2_int64 * p + 1_int64)
                p = p / 2_int64
            end do
            self%live = self%live - 1_int64
            if (allocated(self%leaf_item)) then
                item = self%leaf_item(leaf)
            else
                item = leaf
            end if
            ok = .true.
        else if (self%ndrawn < self%npos + self%nzero) then
            ! The zero-weight tail. These can never be drawn while any positive weight remains, so
            ! they necessarily land here; handing them out through the uniform permutation gets
            ! Q5's "uniform random order" without a second construction and without state.
            self%ndrawn = self%ndrawn + 1_int64
            item = self%zero_item(pf_random_perm_at(self%zkey, self%nzero, self%ndrawn - self%npos))
            ok = .true.
        end if
    end subroutine wd_draw

    ! ---- pf_weighted_draw bindings ----

    !> `%init` with no stream; the stream is 0.
    subroutine wd_init_base(self, weights, seed)
        class(pf_weighted_draw), intent(inout) :: self  !! the sampler
        real(real64), intent(in) :: weights(:)          !! one weight per item
        integer(int64), intent(in) :: seed              !! the seed

        call wd_init_s64(self, weights, seed, 0_int64)
    end subroutine wd_init_base

    !> `%init` with an `integer(int32)` stream.
    subroutine wd_init_s32(self, weights, seed, stream)
        class(pf_weighted_draw), intent(inout) :: self  !! the sampler
        real(real64), intent(in) :: weights(:)          !! one weight per item
        integer(int64), intent(in) :: seed              !! the seed
        integer(int32), intent(in) :: stream            !! which sequence; sign-extends

        call wd_init_s64(self, weights, seed, int(stream, int64))
    end subroutine wd_init_s32

    !> `%init` with an `integer(int64)` stream. The form the other two delegate to.
    !!
    !! **Deliberately `intent(inout)` with an explicit called-twice guard, not `intent(out)`.**
    !! `intent(out)` would reset the object on entry and make a second `%init` silently succeed,
    !! quietly discarding a sequence in progress; `%reseed` is the supported way to reuse a
    !! sampler and is `O(k log n)` where a rebuild is `O(n)`, so a caller reaching for `%init`
    !! twice is nearly always reaching for the wrong one.
    subroutine wd_init_s64(self, weights, seed, stream)
        class(pf_weighted_draw), intent(inout) :: self  !! the sampler
        real(real64), intent(in) :: weights(:)          !! one weight per item
        integer(int64), intent(in) :: seed              !! the seed
        integer(int64), intent(in) :: stream            !! which sequence

        if (self%ready) &
            error stop "pf_weighted_draw%init: already called for this sampler; use %reseed to " // &
                       "start another sequence over the same weights"
        call wd_build(self, weights)
        call wd_set_coords(self, seed, stream)
    end subroutine wd_init_s64

    !> `%next` into an `integer(int32)` item index.
    subroutine wd_next_i32(self, item, ok)
        class(pf_weighted_draw), intent(inout) :: self  !! the sampler
        integer(int32), intent(out) :: item             !! the item drawn, or 0 when exhausted
        logical, intent(out), optional :: ok            !! `.false.` once exhausted
        integer(int64) :: item64
        logical :: got

        call wd_require_ready(self, "next")
        if (self%npos + self%nzero > wd_int32_limit()) &
            error stop "pf_weighted_draw%next: the population exceeds huge(int32) and cannot be " // &
                       "reported into an integer(int32) item; declare it integer(int64)"
        call wd_draw(self, item64, got)
        item = int(item64, int32)
        if (present(ok)) ok = got
    end subroutine wd_next_i32

    !> `%next` into an `integer(int64)` item index.
    subroutine wd_next_i64(self, item, ok)
        class(pf_weighted_draw), intent(inout) :: self  !! the sampler
        integer(int64), intent(out) :: item             !! the item drawn, or 0 when exhausted
        logical, intent(out), optional :: ok            !! `.false.` once exhausted
        logical :: got

        call wd_require_ready(self, "next")
        call wd_draw(self, item, got)
        if (present(ok)) ok = got
    end subroutine wd_next_i64

    !> Restarts the SAME sequence: the next `%next` returns what the first one did.
    subroutine wd_reset(self)
        class(pf_weighted_draw), intent(inout) :: self  !! the sampler

        call wd_require_ready(self, "reset")
        call wd_restore(self)
    end subroutine wd_reset

    !> `%reseed` with no stream; the stream is 0.
    subroutine wd_reseed_base(self, seed)
        class(pf_weighted_draw), intent(inout) :: self  !! the sampler
        integer(int64), intent(in) :: seed              !! the new seed

        call wd_reseed_s64(self, seed, 0_int64)
    end subroutine wd_reseed_base

    !> `%reseed` with an `integer(int32)` stream.
    subroutine wd_reseed_s32(self, seed, stream)
        class(pf_weighted_draw), intent(inout) :: self  !! the sampler
        integer(int64), intent(in) :: seed              !! the new seed
        integer(int32), intent(in) :: stream            !! the new stream; sign-extends

        call wd_reseed_s64(self, seed, int(stream, int64))
    end subroutine wd_reseed_s32

    !> `%reseed` with an `integer(int64)` stream. The form the other two delegate to.
    !!
    !! **Works on a pristine sampler**, one that has never drawn: the journal is empty, the replay
    !! is a no-op and only the coordinates change. That is not an edge case to tolerate but the
    !! normal path for the per-thread-array idiom, where every sampler is freshly built and then
    !! immediately reseeded on its first outer iteration.
    subroutine wd_reseed_s64(self, seed, stream)
        class(pf_weighted_draw), intent(inout) :: self  !! the sampler
        integer(int64), intent(in) :: seed              !! the new seed
        integer(int64), intent(in) :: stream            !! the new stream

        call wd_require_ready(self, "reseed")
        call wd_restore(self)
        call wd_set_coords(self, seed, stream)
    end subroutine wd_reseed_s64

    !> Items not yet drawn, zero-weight ones included. Exact and `O(1)` -- never a weight sum.
    function wd_remaining(self) result(r)
        class(pf_weighted_draw), intent(in) :: self     !! the sampler
        integer(int64) :: r                             !! items still available

        call wd_require_ready(self, "remaining")
        r = self%npos + self%nzero - self%ndrawn
    end function wd_remaining

    ! ---- pf_weighted_subset ----

    !> The shared worker: `size(idx)` calls to `%next`, and nothing else.
    subroutine wsub_impl(idx, weights, seed, stream)
        integer(int64), intent(out) :: idx(:)   !! the items drawn, in order
        real(real64), intent(in) :: weights(:)  !! one weight per item
        integer(int64), intent(in) :: seed      !! the seed
        integer(int64), intent(in) :: stream    !! which sequence
        type(pf_weighted_draw) :: d
        integer(int64) :: k
        logical :: ok

        if (size(idx, kind=int64) == 0_int64) return
        if (size(idx, kind=int64) > size(weights, kind=int64)) &
            error stop "pf_weighted_subset: more items requested than there are weights; a subset " // &
                       "drawn without replacement cannot be larger than its population"
        call d%init(weights, seed, stream)
        do k = 1_int64, size(idx, kind=int64)
            call d%next(idx(k), ok)
            if (.not. ok) &
                error stop "pf_weighted_subset: the sampler was exhausted early; this cannot " // &
                           "happen once the population check has passed, and is a library defect"
        end do
    end subroutine wsub_impl

    !> Refuses an `integer(int32)` result array for a population that cannot fit in one.
    subroutine wsub_check_i32(weights)
        real(real64), intent(in) :: weights(:)  !! the weights, whose size bounds every item index

        if (size(weights, kind=int64) > wd_int32_limit()) &
            error stop "pf_weighted_subset: the population exceeds huge(int32) and an item index " // &
                       "has nowhere to go; declare idx as integer(int64)"
    end subroutine wsub_check_i32

    !> `pf_weighted_subset` into an `int32` array, no stream.
    subroutine wsub_i32_base(idx, weights, seed)
        integer(int32), intent(out) :: idx(:)   !! the items drawn, in order
        real(real64), intent(in) :: weights(:)  !! one weight per item
        integer(int64), intent(in) :: seed      !! the seed

        call wsub_i32_s64(idx, weights, seed, 0_int64)
    end subroutine wsub_i32_base

    !> `pf_weighted_subset` into an `int32` array, `int32` stream.
    subroutine wsub_i32_s32(idx, weights, seed, stream)
        integer(int32), intent(out) :: idx(:)   !! the items drawn, in order
        real(real64), intent(in) :: weights(:)  !! one weight per item
        integer(int64), intent(in) :: seed      !! the seed
        integer(int32), intent(in) :: stream    !! which sequence; sign-extends

        call wsub_i32_s64(idx, weights, seed, int(stream, int64))
    end subroutine wsub_i32_s32

    !> `pf_weighted_subset` into an `int32` array, `int64` stream.
    subroutine wsub_i32_s64(idx, weights, seed, stream)
        integer(int32), intent(out) :: idx(:)   !! the items drawn, in order
        real(real64), intent(in) :: weights(:)  !! one weight per item
        integer(int64), intent(in) :: seed      !! the seed
        integer(int64), intent(in) :: stream    !! which sequence
        integer(int64), allocatable :: tmp(:)

        if (size(idx, kind=int64) == 0_int64) return
        call wsub_check_i32(weights)
        allocate(tmp(size(idx, kind=int64)))
        call wsub_impl(tmp, weights, seed, stream)
        idx = int(tmp, int32)
    end subroutine wsub_i32_s64

    !> `pf_weighted_subset` into an `int64` array, no stream.
    subroutine wsub_i64_base(idx, weights, seed)
        integer(int64), intent(out) :: idx(:)   !! the items drawn, in order
        real(real64), intent(in) :: weights(:)  !! one weight per item
        integer(int64), intent(in) :: seed      !! the seed

        call wsub_impl(idx, weights, seed, 0_int64)
    end subroutine wsub_i64_base

    !> `pf_weighted_subset` into an `int64` array, `int32` stream.
    subroutine wsub_i64_s32(idx, weights, seed, stream)
        integer(int64), intent(out) :: idx(:)   !! the items drawn, in order
        real(real64), intent(in) :: weights(:)  !! one weight per item
        integer(int64), intent(in) :: seed      !! the seed
        integer(int32), intent(in) :: stream    !! which sequence; sign-extends

        call wsub_impl(idx, weights, seed, int(stream, int64))
    end subroutine wsub_i64_s32

    !> `pf_weighted_subset` into an `int64` array, `int64` stream.
    subroutine wsub_i64_s64(idx, weights, seed, stream)
        integer(int64), intent(out) :: idx(:)   !! the items drawn, in order
        real(real64), intent(in) :: weights(:)  !! one weight per item
        integer(int64), intent(in) :: seed      !! the seed
        integer(int64), intent(in) :: stream    !! which sequence

        call wsub_impl(idx, weights, seed, stream)
    end subroutine wsub_i64_s64

    ! ---- pf_weighted_permutation: the exponential race ----

    !> The shared worker: build the keys, sort them, then place the zero-weight tail.
    subroutine wperm_impl(perm, weights, seed, stream, threads)
        integer(int64), intent(out) :: perm(:)  !! the weighted permutation of `1 .. size(weights)`
        real(real64), intent(in) :: weights(:)  !! one weight per item
        integer(int64), intent(in) :: seed      !! the seed
        integer(int64), intent(in) :: stream    !! which sequence
        integer, intent(in), optional :: threads !! thread request; absent means auto
        real(real64), allocatable :: u(:), key(:)
        integer(int64), allocatable :: ord(:), pos_item(:), zero_item(:)
        integer(int64) :: rseed                 !! this family's own seed; see `wperm_family_label`
        integer(int64) :: n, npos, nzero, i, j, z, zkey
        real(real64) :: uu, k1

        n = size(weights, kind=int64)
        if (size(perm, kind=int64) /= n) &
            error stop "pf_weighted_permutation: perm and weights must have the same size; a " // &
                       "weighted permutation returns every item exactly once"
        if (n == 0_int64) return
        if (.not. exp_key_contract_ok()) &
            error stop "pf_weighted_permutation: this build does not reproduce the frozen -log(u) " // &
                       "transform, so its permutations would disagree with every other build. " // &
                       "Every configuration tools/check_exp_key.sh sweeps -- gfortran and ifx, " // &
                       "-O0 to -Ofast, fast-math included -- reproduces it, so this is a compiler, " // &
                       "version or flag combination nobody has swept. Run that script with FC set " // &
                       "to this compiler; it prints which configurations differ."

        npos = 0_int64
        nzero = 0_int64
        do i = 1_int64, n
            if (ieee_is_nan(weights(i))) &
                error stop "pf_weighted_permutation: a weight is NaN; a NaN compares false against " // &
                           "every bound and would silently be treated as zero"
            if (.not. ieee_is_finite(weights(i))) &
                error stop "pf_weighted_permutation: a weight is infinite; the key would be zero " // &
                           "and that item drawn first every time"
            if (weights(i) < 0.0_real64) &
                error stop "pf_weighted_permutation: a weight is negative; the key would be " // &
                           "negative and that item drawn FIRST, the opposite of any sane reading"
            if (weights(i) >= wd_min_weight) then
                npos = npos + 1_int64
            else
                nzero = nzero + 1_int64
            end if
        end do
        if (npos == 0_int64) &
            error stop "pf_weighted_permutation: every weight is zero; there is no distribution to " // &
                       "draw from"

        ! Zero-weight items are held OUT of the race rather than given a sentinel key. A sentinel
        ! would have to sort after every real key, and there is no such value: a denormal weight
        ! makes `-log(u)/w` overflow to infinity, which would then sort after the sentinel and put
        ! zero-weight items in the middle. Keeping them out removes the question, and it is also
        ! what gets them into UNIFORM RANDOM order -- tied sentinel keys would come back in index
        ! order, since every sort here is stable.
        ! Domain separation, exactly as `wd_set_coords` does for the sequential family and for the
        ! same reason: sharing draw 1 with it made both choose the lowest-weight item together.
        rseed = pf_random_key(seed, wperm_family_label)
        allocate(u(n), key(npos), pos_item(npos))
        call pf_random_fill_draws(rseed, stream, u)
        j = 0_int64
        do i = 1_int64, n
            if (weights(i) >= wd_min_weight) then
                j = j + 1_int64
                ! u is in [0,1); the race needs (0,1], and u = 0 would give the key exactly 0 and
                ! draw that item first every time.
                uu = 1.0_real64 - u(i)
                k1 = exp_key(uu) / weights(i)
                ! DEFENSIVE AND UNREACHABLE: `wd_min_weight` is the overflow bound rounded up, so
                ! every weight reaching this loop gives a key of at most 1.7920e308 against a
                ! `huge` of 1.7977e308. It is kept because the bound is derived from `exp_key`'s
                ! range, and a change there would make it reachable again with nothing else to
                ! notice. See wd_min_weight; no fixture this repository can build reaches it.
                ! gcov attribution artifact: the test itself runs once per weight (over a million
                ! times in a full suite run) while the error stop below never does, so this
                ! excluded line is expected to report a positive hit count.
                if (.not. ieee_is_finite(k1)) &                                  ! GCOVR_EXCL_LINE
                    error stop "pf_weighted_permutation: a weight is so small that its key " // &
                               "overflows; this cannot happen for a weight at or above " // &
                               "wd_min_weight, so exp_key's range has changed"    ! GCOVR_EXCL_LINE
                key(j) = k1
                pos_item(j) = i
            end if
        end do
        deallocate(u)

        ! Ties break by item index for free: every sort in `parquet_sorting` is stable, so equal
        ! keys keep their original order and `(key, index)` is a strict total order without a
        ! second sort key. `test_race_tie_rule` asserts that rather than trusting it.
        call pf_argsort(key, ord, threads=threads)
        do i = 1_int64, npos
            perm(i) = pos_item(ord(i))
        end do

        if (nzero > 0_int64) then
            allocate(zero_item(nzero))
            z = 0_int64
            do i = 1_int64, n
                if (.not. (weights(i) >= wd_min_weight)) then
                    z = z + 1_int64
                    zero_item(z) = i
                end if
            end do
            ! The same two derivations the sequential family uses, from this family's own seed --
            ! so the tail is ordered the same WAY and not in the same ORDER.
            zkey = pf_random_key(pf_random_key(rseed, stream), wd_zero_label)
            do i = 1_int64, nzero
                perm(npos + i) = zero_item(pf_random_perm_at(zkey, nzero, i))
            end do
        end if
    end subroutine wperm_impl

    !> Refuses an `integer(int32)` result array for a population that cannot fit in one.
    subroutine wperm_check_i32(weights)
        real(real64), intent(in) :: weights(:)  !! the weights, whose size bounds every item index

        if (size(weights, kind=int64) > wd_int32_limit()) &
            error stop "pf_weighted_permutation: the population exceeds huge(int32) and an item " // &
                       "index has nowhere to go; declare perm as integer(int64)"
    end subroutine wperm_check_i32

    !> `pf_weighted_permutation` into an `int32` array, no stream.
    subroutine wperm_i32_base(perm, weights, seed, threads)
        integer(int32), intent(out) :: perm(:)  !! the weighted permutation
        real(real64), intent(in) :: weights(:)  !! one weight per item
        integer(int64), intent(in) :: seed      !! the seed
        integer, intent(in), optional :: threads !! thread request; absent means auto

        call wperm_i32_s64(perm, weights, seed, 0_int64, threads)
    end subroutine wperm_i32_base

    !> `pf_weighted_permutation` into an `int32` array, `int64` stream.
    subroutine wperm_i32_s64(perm, weights, seed, stream, threads)
        integer(int32), intent(out) :: perm(:)  !! the weighted permutation
        real(real64), intent(in) :: weights(:)  !! one weight per item
        integer(int64), intent(in) :: seed      !! the seed
        integer(int64), intent(in) :: stream    !! which sequence
        integer, intent(in), optional :: threads !! thread request; absent means auto
        integer(int64), allocatable :: tmp(:)

        if (size(perm, kind=int64) == 0_int64 .and. size(weights, kind=int64) == 0_int64) return
        call wperm_check_i32(weights)
        allocate(tmp(size(perm, kind=int64)))
        call wperm_impl(tmp, weights, seed, stream, threads)
        perm = int(tmp, int32)
    end subroutine wperm_i32_s64

    !> `pf_weighted_permutation` into an `int64` array, no stream.
    subroutine wperm_i64_base(perm, weights, seed, threads)
        integer(int64), intent(out) :: perm(:)  !! the weighted permutation
        real(real64), intent(in) :: weights(:)  !! one weight per item
        integer(int64), intent(in) :: seed      !! the seed
        integer, intent(in), optional :: threads !! thread request; absent means auto

        call wperm_impl(perm, weights, seed, 0_int64, threads)
    end subroutine wperm_i64_base

    !> `pf_weighted_permutation` into an `int64` array, `int64` stream.
    subroutine wperm_i64_s64(perm, weights, seed, stream, threads)
        integer(int64), intent(out) :: perm(:)  !! the weighted permutation
        real(real64), intent(in) :: weights(:)  !! one weight per item
        integer(int64), intent(in) :: seed      !! the seed
        integer(int64), intent(in) :: stream    !! which sequence
        integer, intent(in), optional :: threads !! thread request; absent means auto

        call wperm_impl(perm, weights, seed, stream, threads)
    end subroutine wperm_i64_s64

end module parquet_sampling ! GCOVR_EXCL_LINE
