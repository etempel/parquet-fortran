!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
! NOT a generated file -- unlike every other src/parquet_sorting*.f90, which
! tools/generate_parquet_sorting.py emits. Edit THIS file directly.
! Its four procedures' INTERFACES do live in the generated src/parquet_sorting.f90;
! change those in the generator's emit_engine_interfaces(), never in the generated output.
!
!> The comparator core of the pure-Fortran sort engine: the ordering, and nothing else.
!!
!! Stage 1 of `feature_sort.md`. This submodule decides **what order rows go in**; it does not sort,
!! and at this stage nothing in the library calls it — the shipped path still crosses `bind(C)` into
!! the C++ engine. What proves this code correct is `test/test_sorting.f90`'s conformance oracle,
!! which asks the C++ comparators the same questions through two test-only hooks and requires the
!! same answers.
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
!! Stage 2 is where the value arrays get hoisted: an introsort holds one key set across millions of
!! comparisons, so it should resolve `keys(k)%ints`/`%reals` into local `contiguous` pointers once
!! rather than re-reaching through the derived type per comparison. Doing that *here* would buy
!! nothing — each call reaches a key exactly once — and would obscure the ordering, which is the only
!! thing this file is for.
submodule (parquet_sorting) parquet_sorting_engine
    implicit none
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
        if (.not. is_null .and. key%family == SK_REAL) is_nan = ieee_is_nan(key%reals(i))
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

end submodule parquet_sorting_engine ! GCOVR_EXCL_LINE
