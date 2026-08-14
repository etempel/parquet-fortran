!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
! NOT a generated file -- unlike every other src/parquet_sorting*.f90, which
! tools/generate_parquet_sorting.py emits. Edit THIS file directly.
! Its four procedures' INTERFACES do live in the generated src/parquet_sorting.f90;
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
submodule (parquet_sorting) parquet_sorting_engine
    implicit none
    !
    !> Ranges of this size or smaller are left to the final insertion pass, as `std::sort` does.
    integer(int64), parameter :: SORT_INSERTION_CUTOFF = 16_int64
    !
    ! ---- NaN detection: `x /= x`, deliberately NOT ieee_is_nan -------------------------------
    !
    ! CLAUDE.md's "Compiler & language gotchas" says to test for NaN with `ieee_is_nan` rather than
    ! this idiom, because this one trips gfortran's -Wcompare-reals. **That convention is knowingly
    ! broken here, and only here**, for one measured reason:
    !
    ! **Under ifx, `ieee_is_nan` is a call into the Intel Fortran runtime, not an instruction.** The
    ! disassembly shows two `ieee_arithmetic_mp_for_ieee_is_nan_k8_` PLT calls per comparison, fired
    ! only on real keys -- exactly the shape of the f64-specific penalty machine B measured (+3.45 ns
    ! over its own i64 arm). gfortran inlines the same intrinsic to a native compare, so machine A
    ! never saw it. `app/probe_isnan.f90` on machine B, ifx 2026.1.1, ns per test above a bare `<`:
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
        logical :: is_null !! row `i` is null under this key.
        logical :: is_nan  !! row `i` is a NaN under this key (real keys only).
        !
        ! An UNALLOCATED `valid` means "this key has no nulls at all" -- the fast path, and the
        ! first thing a port of the C++ side gets wrong, because there the same state is an empty
        ! vector. Every caller must be safe against it.
        is_null = .false.
        if (allocated(key%valid)) is_null = (key%valid(i) == 0_c_int8_t)
        !
        is_nan = .false.
        ! `x /= x` rather than `ieee_is_nan` -- see this submodule's header. A deliberate, measured
        ! exception to a project-wide convention, not an oversight.
        if (.not. is_null .and. key%family == SK_REAL) is_nan = (key%reals(i) /= key%reals(i))
        !
        ! `descending` deliberately does not appear here. A descending sort still puts nulls last by
        ! default; it does not flip them to the front.
        if (key%nulls_first) then
            if (is_null) then
                tier = 0
            else if (is_nan) then
                tier = 1
            else
                tier = 2
            end if
        else
            if (is_null) then
                tier = 2
            else if (is_nan) then
                tier = 1
            else
                tier = 0
            end if
        end if
    end procedure sort_tier_of

    module procedure sort_compare_key
        integer :: ta, tb       !! tiers of `a` and `b`.
        integer :: value_tier   !! which tier number the VALUES occupy under this key's placement.
        integer(int64) :: ia, ib !! integer key values.
        real(real64) :: ra, rb   !! real key values.
        !
        ta = sort_tier_of(key, a)
        tb = sort_tier_of(key, b)
        if (ta /= tb) then
            c = -1
            if (ta > tb) c = 1
            return
        end if
        !
        ! Same tier. If it is not the VALUE tier then both rows are null, or both are NaN, and the
        ! answer is EQUAL -- which is what leaves them in file order once the caller's index
        ! tiebreaker runs. Note the value tier is 2 under `nulls_first` and 0 otherwise.
        value_tier = 0
        if (key%nulls_first) value_tier = 2
        if (ta /= value_tier) then
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
        b = lo + (hi - lo) / 2_int64
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

    ! ---- Test-only access to the two comparators -----------------------------------------------
    !
    ! These exist so `test/test_sorting.f90` can ask the Fortran engine what it thinks, one row pair
    ! at a time, and compare that against what the C++ engine thinks. The test reaches the C++ side
    ! on its own, through locally declared bind(C) interfaces to `parquet_debug_sort_row_less` and
    ! `parquet_debug_sort_keys_compare` in src/parquet_wrapper.cpp -- which is why NOTHING here
    ! crosses the bind(C) boundary and why feature_sort.md section 4's "the C++ boundary for
    ! parquet_sorting is confined to ONE file" still holds with these in place. Keep it that way: a
    ! crossing added here would have to be unpicked again at Stage 6.

    module procedure parquet_debug_sort_row_less
        less = .false.
        if (.not. allocated(keys%keys)) return
        less = sort_row_less(keys%keys, a, b)
    end procedure parquet_debug_sort_row_less

    module procedure parquet_debug_sort_keys_compare
        c = 0
        if (.not. allocated(keys%keys)) return
        c = sort_keys_compare(keys%keys, a, b, nkeys)
    end procedure parquet_debug_sort_keys_compare

    ! The two sweeps below must stay loop-for-loop identical to their C++ twins
    ! (`parquet_debug_sort_sweep_less_cpp` / `_compare_cpp`, src/parquet_wrapper.cpp). They exist so
    ! app/benchmark_sort_comparator.f90 can time the COMPARATOR rather than the cost of reaching it,
    ! and their agreeing checksums are what prove the two arms did the same work. Two details are
    ! load-bearing and neither is obvious:
    !
    !   * `stride` is computed once per REP. Inside the inner loop it would be a `mod` on a runtime
    !     divisor, i.e. an integer division -- around 6 ns on x86-64, against a comparator costing a
    !     few. CLAUDE.md records a benchmark whose entire reported floor turned out to be exactly
    !     this mistake.
    !   * The wrapping step is `j = i + stride` with one conditional subtraction, which is why
    !     `stride` is kept in `[1, nrows-1]`: a larger stride would need a loop, not a subtraction.

    module procedure parquet_debug_sort_sweep_less
        integer(int64) :: rep, i, j, stride
        !
        count = -1_int64
        if (.not. allocated(keys%keys)) return
        if (nrows < 2_int64) return
        count = 0_int64
        do rep = 0_int64, nreps - 1_int64
            stride = 1_int64 + mod(rep, nrows - 1_int64)
            do i = 1_int64, nrows
                j = i + stride
                if (j > nrows) j = j - nrows
                if (sort_row_less(keys%keys, i, j)) count = count + 1_int64
            end do
        end do
    end procedure parquet_debug_sort_sweep_less

    ! The three below are Stage 2 scaffolding over the module state declared in
    ! src/parquet_sorting.f90 -- see `dbg_fortran_engine` there for why an engine selector is a debug
    ! hook and not a `parquet_settings` knob. All three go away at the Stage 6 cutover.

    module procedure parquet_debug_use_fortran_sort_engine
        dbg_fortran_engine = on
    end procedure parquet_debug_use_fortran_sort_engine

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

    module procedure parquet_debug_sort_sweep_compare
        integer(int64) :: rep, i, j, stride
        !
        total = -1_int64
        if (.not. allocated(keys%keys)) return
        if (nrows < 2_int64) return
        total = 0_int64
        do rep = 0_int64, nreps - 1_int64
            stride = 1_int64 + mod(rep, nrows - 1_int64)
            do i = 1_int64, nrows
                j = i + stride
                if (j > nrows) j = j - nrows
                total = total + int(sort_keys_compare(keys%keys, i, j, nkeys), int64)
            end do
        end do
    end procedure parquet_debug_sort_sweep_compare

end submodule parquet_sorting_engine ! GCOVR_EXCL_LINE
