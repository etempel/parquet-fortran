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
!! exactly — treats a prefix as *less*, so `"ab" < "ab "`. `compare_bytes` below does it byte by
!! byte and then by length, and reads each byte as **unsigned**, because
!! `char_traits<char>::compare` is `memcmp` and a byte ≥ 128 must sort high.
!!
!! ## Where the ordering actually lives: `parquet_sorting_key_compare.inc`
!!
!! Every procedure in this file is a thin shell around `tier_of`/`cmp_key`, which are
!! **internal procedures** textually included from `src/parquet_sorting_key_compare.inc` into each
!! host's `contains` section. Read that file's header before changing anything here — it explains why
!! the ordering cannot live in a shared module procedure.
!!
!! In one sentence: under ELF with `-fPIC`, which fpm's release profile passes to every file, a
!! gfortran **module procedure is a global symbol and therefore interposable**, so GCC refuses to
!! inline it — measured at four out-of-line calls per comparison and 1.3x–2.2x against the C++
!! engine, whose helpers are `static inline`. An internal procedure gets **local** linkage and is
!! immune. `compare_bytes` is the deliberate exception, kept out of line: it is a byte loop that can
!! never execute on a numeric key, so inlining it into every unrolled iteration of the numeric path
!! is pure bloat. Measured on machine A: keeping it out of line took the `str` arm from 7.64 to 4.95
!! ns and left the numeric arms unchanged within noise.
!!
!! **`feature_risks.md` Risk-34 still holds, by a changed mechanism.** `sort_row_less` and
!! `sort_keys_compare` remain one decision expressed twice, and they still cannot drift — but the
!! guarantee is now "both include one shared source" rather than "both call one shared body". The
!! way to break it is to reimplement the comparison in a host instead of including the file.
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
!! **One global call per comparison remains, and it is deliberate**: a caller reaching
!! `sort_row_less` still pays an out-of-line call, because that one must stay a module procedure for
!! `parquet_sorting_keys` to reach it at Stage 6. Stage 2 can remove even that, by having the
!! introsort include the same file as its own internal comparator.
!!
!! Stage 2 is also where the value arrays get hoisted: an introsort holds one key set across millions
!! of comparisons, so it should resolve `keys(k)%ints`/`%reals` into local `contiguous` pointers once
!! rather than re-reaching through the derived type per comparison. Doing that *here* would buy
!! nothing — each call reaches a key exactly once — and would obscure the ordering, which is the only
!! thing this file is for.
submodule (parquet_sorting) parquet_sorting_engine
    implicit none
    !
contains

    ! ---- The two comparators. Adjacent on purpose -- Risk-34. ----------------------------------

    module procedure sort_tier_of
        tier = tier_of(key, i)
    contains
        include 'parquet_sorting_key_compare.inc'
    end procedure sort_tier_of

    module procedure sort_compare_key
        c = cmp_key(key, a, b)
    contains
        include 'parquet_sorting_key_compare.inc'
    end procedure sort_compare_key

    module procedure sort_row_less
        integer :: k !! key index.
        integer :: c !! this key's three-way answer.
        !
        do k = 1, size(keys)
            c = cmp_key(keys(k), a, b)
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
    contains
        include 'parquet_sorting_key_compare.inc'
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
            c = cmp_key(keys(k), a, b)
            if (c /= 0) return
        end do
    contains
        include 'parquet_sorting_key_compare.inc'
    end procedure sort_keys_compare


    ! ---- String keys: deliberately NOT in the include -------------------------------------
    !
    ! Kept out of line, unlike tier_of/cmp_key, because inlining a byte loop into every unrolled
    ! iteration of the NUMERIC path is pure code bloat -- it can never execute there. On a string
    ! key the extra call is negligible beside the loop it guards.
    !> Bytewise comparison of two string-key rows: `memcmp` semantics, deliberately NOT Fortran's.
    !!
    !! Row `k` occupies `key%data(key%offsets(k) + 1 : key%offsets(k + 1))` -- `offsets` holds
    !! 0-based byte positions in a 1-based array, which is the layout the C++ side indexes.
    !!
    !! Two departures from Fortran's own `character` comparison, both required to match
    !! `std::string_view::compare`: the shorter string is LESS when it is a prefix of the longer
    !! (Fortran would blank-pad and call them equal), and bytes are read as UNSIGNED so a byte
    !! >= 128 sorts above every ASCII one. `iand(..., 255)` guarantees the second whatever the
    !! processor's `iachar` hands back.
    integer function compare_bytes(key, a, b)
        type(sort_key_buf), intent(in) :: key !! the bound string key.
        integer(int64), intent(in) :: a       !! first row, 1-based.
        integer(int64), intent(in) :: b       !! second row, 1-based.
        integer(int64) :: pa, pb              !! first byte of each row, 1-based into `data`.
        integer(int64) :: na, nb              !! byte length of each row.
        integer(int64) :: k, m                !! loop index, and the common prefix length.
        integer :: ba, bb                     !! one byte from each row, unsigned 0..255.
        !
        pa = key%offsets(a) + 1_int64
        na = key%offsets(a + 1_int64) - key%offsets(a)
        pb = key%offsets(b) + 1_int64
        nb = key%offsets(b + 1_int64) - key%offsets(b)
        m = min(na, nb)
        !
        compare_bytes = 0
        do k = 0_int64, m - 1_int64
            ba = iand(iachar(key%data(pa + k)), 255)
            bb = iand(iachar(key%data(pb + k)), 255)
            if (ba /= bb) then
                compare_bytes = -1
                if (ba > bb) compare_bytes = 1
                return
            end if
        end do
        !
        ! Equal over the common prefix: the shorter one is less.
        if (na < nb) then
            compare_bytes = -1
        else if (na > nb) then
            compare_bytes = 1
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
