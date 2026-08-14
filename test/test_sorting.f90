!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> Unit tests for `parquet_sorting`: `pf_argsort`, `pf_sort`, `pf_permute`, `pf_is_sorted` and
!> the `pf_sort_keys` multi-key builder, over all eleven supported element types.
!>
!> Five things shape this suite:
!>
!> * **The ordering contract is the same one `test_sort.f90` asserts for a read-time `sort_by=`,
!>   and `test_oracle_matches_read_time_sort` is what proves the two really do share an engine.**
!>   It writes a file, reads it back sorted by the reader, and separately `pf_argsort`s the same
!>   values in memory -- if either path ever grew its own comparator, that test fails and no
!>   other one would.
!> * **Every type is exercised, because each reaches the engine differently**: `logical` becomes
!>   0/1, `parquet_date`/`parquet_time` become their raw integers, a `parquet_timestamp` becomes
!>   TWO integer keys, `character(len=*)` and `parquet_string_column` become packed
!>   (offsets, data) pairs, and `parquet_column` resolves its kind at runtime. A test passing for
!>   `int32` says nothing about any of them.
!> * **Null and NaN placement is absolute** -- `descending` reverses the values, never the tiers.
!>   Both directions are asserted for both, since flipping the tiers is the obvious way to get
!>   this wrong and it still looks right for a null-free array.
!> * **Stability is a contract, not an accident.** The all-tied test is what fails if the
!>   comparator loses its row-index tiebreaker or the counting fast path stops placing in order.
!> * **Abort paths live elsewhere.** `error stop` kills the process, so every rejection (a bad
!>   permutation, a mismatched mask, a vector column, an empty key list, an int32 overflow) is a
!>   `sorting_*` scenario in test/error_scenarios.f90, driven from test_errors.f90.
!>
!> Tests here allocate their own arrays and share no state, so nothing needs a per-test fixture
!> filename -- except the two that write a parquet file, which follow the usual rule.
module test_sorting
    use parquet
    use iso_fortran_env, only : int32, int64, real32, real64
    use ieee_arithmetic, only : ieee_value, ieee_quiet_nan, ieee_is_nan
    use testdrive, only : new_unittest, unittest_type, error_type, check
    ! For the Stage 1 conformance oracle only: it builds a C++ key set of its own so that both
    ! engines can be asked about the same rows. These are ordinary library bindings, not debug
    ! hooks -- the two debug hooks are declared locally in `sweep_pairs`, per convention.
    use iso_c_binding, only : c_ptr, c_loc, c_null_ptr, c_int8_t, c_char, c_long_long
    use parquet_bindings, only : parquet_sort_builder_new, parquet_sort_builder_free, &
        parquet_sort_builder_add_key_int64, parquet_sort_builder_add_key_double, &
        parquet_sort_builder_add_key_string
    !
    implicit none
    private
    public :: collect_tests_parquet_sorting
    !
    ! ==================================================================================
    ! The shared sweep fixture
    ! ==================================================================================
    !
    !> Every "one call per specific" sweep below builds its values from these parameters, so an
    !> expectation is written once as a RANK and never as a per-type literal. That is what keeps a
    !> sixty-call sweep readable, and it keeps the oracle independent of the engine under test: the
    !> answer is read off `SWRANK`/`SWASC` by hand, not obtained from a second call into `pf_sort`.
    !>
    !> The order is scrambled deliberately. No element is already in place, the smallest and the
    !> largest both sit in the interior, and the ascending permutation is neither the identity nor a
    !> reversal -- so a specific wired to the wrong extractor, or one that transposes a value against
    !> an index, cannot pass by coincidence.
    integer, parameter :: SWN = 6
    !> `SWRANK(k)` is element k's 1-based position in ascending order; no two elements tie.
    integer, parameter :: SWRANK(SWN) = [3, 1, 5, 2, 6, 4]
    !> `SWASC(r)` is the element index holding rank r -- the permutation a full ascending sort gives.
    integer, parameter :: SWASC(SWN) = [2, 4, 1, 6, 3, 5]
    !> The `logical` fixture, the one type that cannot hold six distinct values: `.false.` at
    !> elements 2, 5 and 6, so a stable ascending sort must begin with exactly those three, in that
    !> order. Weaker than the others, which is why it is spelled out here rather than left to look
    !> equivalent.
    logical, parameter :: SWBOOL(SWN) = [.true., .false., .true., .true., .false., .false.]
    !> The DUPLICATE fixture, for the questions that only mean something with ties: six elements
    !> over three classes, each class appearing twice, neither contiguously nor in class order. A
    !> body that reported one entry per RUN rather than per VALUE would answer four rather than
    !> three, and one that lost the tie in ranking would never produce a repeated rank.
    integer, parameter :: SWDUP(SWN) = [3, 1, 3, 2, 1, 2]
    !> The SORTED duplicate fixture, for the search questions, which require sorted input: one
    !> element of class 1, two of class 2, three of class 3. Class 2 is what the sweeps look for --
    !> it is neither at the start nor at the end, and its run is neither of length one nor the
    !> longest, so its lower bound (2), upper bound (4) and equal range (2, 3) are four different
    !> numbers and no two of the three operations can be confused for each other.
    integer, parameter :: SWSRT(SWN) = [1, 2, 2, 3, 3, 3]
    !> The `logical` search fixture: the same shape as far as a two-valued type allows.
    logical, parameter :: SWSRTB(SWN) = [.false., .false., .false., .true., .true., .true.]
    !
contains
    !
    !> The sweep fixture's string value for rank `r`: "v1" through "v6", which order
    !> lexicographically by rank. A fixed-length result, never a deferred-length one -- see
    !> CLAUDE.md on why this module returns no `character(len=:), allocatable` function results.
    pure function swchr(r) result(res)
        integer, intent(in) :: r !! the 1-based rank wanted.
        character(len=2) :: res  !! "v" followed by the rank digit.
        res = "v"//achar(iachar("0") + r)
    end function swchr
    !
    subroutine collect_tests_parquet_sorting(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)
        testsuite = [ &
            new_unittest("argsort returns the permutation that sorts", test_argsort_basic), &
            new_unittest("descending reverses the values", test_descending), &
            new_unittest("ties keep their original order", test_stability), &
            new_unittest("nulls sort last by default", test_nulls_last), &
            new_unittest("nulls_first moves them to the front", test_nulls_first), &
            new_unittest("descending does not flip the null tier", test_descending_nulls_absolute), &
            new_unittest("NaNs sort between values and nulls", test_nan_tier), &
            new_unittest("pf_sort copies without touching the input", test_sort_out_of_place), &
            new_unittest("pf_permute applies a permutation in place", test_permute_round_trip), &
            new_unittest("pf_permute accepts assume_valid", test_permute_assume_valid), &
            new_unittest("pf_permute: every type x both index kinds x five shapes", test_permute_all_specifics), &
            new_unittest("pf_is_sorted agrees with pf_sort", test_is_sorted_agrees), &
            new_unittest("is_sorted is direction- and null-aware", test_is_sorted_options), &
            new_unittest("an int32 permutation equals the int64 one", test_perm_kinds_agree), &
            new_unittest("every scalar type sorts", test_all_scalar_types), &
            new_unittest("pf_argsort: every type x both index kinds, with and without groups", &
                test_argsort_every_specific), &
            new_unittest("pf_sort: every type", test_sort_every_specific), &
            new_unittest("logical sorts false before true", test_logical), &
            new_unittest("character sorts on the full padded length", test_character_padding), &
            new_unittest("date and time sort by their raw values", test_date_time), &
            new_unittest("a timestamp breaks ties on nanoseconds", test_timestamp_two_keys), &
            new_unittest("a string column sorts and reindexes", test_string_column), &
            new_unittest("a parquet_column sorts by its runtime kind", test_parquet_column), &
            new_unittest("a second key breaks the first key's ties", test_multi_key), &
            new_unittest("keys of mixed types combine", test_multi_key_mixed), &
            new_unittest("per-key direction is independent", test_multi_key_directions), &
            new_unittest("%clear makes a key list reusable", test_keys_clear), &
            new_unittest("empty and single-element arrays are handled", test_degenerate_sizes), &
            new_unittest("pf_argsort matches a read-time sort_by=", test_oracle_matches_read_time_sort), &
            new_unittest("the counting path matches the comparator", test_counting_path_agrees), &
            new_unittest("the counting path matches the comparator on a NULL-BEARING key", &
                test_counting_path_nulls_agree), &
            new_unittest("partial_sort equals a truncated full sort", test_partial_matches_full), &
            new_unittest("n is clamped, not refused", test_partial_clamps), &
            new_unittest("partial_argsort agrees with argsort", test_partial_argsort), &
            new_unittest("partial descending gives the last N", test_partial_descending), &
            new_unittest("partial_sort really is partial", test_partial_is_partial), &
            new_unittest("sorted_valid tracks the sorted order", test_sorted_valid), &
            new_unittest("nth_element agrees with a full sort", test_nth_matches_full), &
            new_unittest("nth_element is stable on duplicates", test_nth_stable_index), &
            new_unittest("nth_element takes direction and nulls", test_nth_options), &
            new_unittest("quantile endpoints are exact", test_quantile_endpoints), &
            new_unittest("quantile excludes nulls and counts them", test_quantile_nulls), &
            new_unittest("each rounding token resolves differently", test_quantile_rounding), &
            new_unittest("rounding tokens are case-insensitive", test_quantile_rounding_case), &
            new_unittest("pf_partial_sort: every type", test_partial_sort_every_specific), &
            new_unittest("pf_partial_argsort: every type x both index kinds", &
                test_partial_argsort_every_specific), &
            new_unittest("pf_nth_element: every type x both rank kinds x three index forms", &
                test_nth_every_specific), &
            new_unittest("pf_nth_quantile: every type x three index forms", test_quantile_every_specific), &
            new_unittest("searches agree with a counting oracle", test_search_linear_oracle), &
            new_unittest("equal_range brackets duplicates", test_equal_range_duplicates), &
            new_unittest("searches follow a descending order", test_search_descending), &
            new_unittest("searches respect the null tier", test_search_nulls), &
            new_unittest("assume_sorted changes no answer", test_search_assume_sorted), &
            new_unittest("int32 and int64 searches agree", test_search_index_kinds), &
            new_unittest("strings and dates are searchable", test_search_other_types), &
            new_unittest("an empty array still gives an insertion point", test_search_degenerate), &
            new_unittest("searches: every type x three operations x both index kinds", &
                test_search_every_specific), &
            new_unittest("unique reports distinct values in order", test_unique_basic), &
            new_unittest("unique excludes and counts nulls", test_unique_nulls), &
            new_unittest("unique agrees on both sort paths", test_unique_both_paths), &
            new_unittest("float distinctness is exact", test_unique_float_exact), &
            new_unittest("a string column's distinct values", test_unique_string_column), &
            new_unittest("the three rank methods differ on ties", test_rank_methods), &
            new_unittest("a null ranks 0", test_rank_nulls_zero), &
            new_unittest("ordinal ranks invert argsort", test_rank_inverts_argsort), &
            new_unittest("descending ranks from the top", test_rank_descending), &
            new_unittest("rank agrees on both sort paths", test_rank_both_paths), &
            new_unittest("pf_unique/pf_unique_count: every type x both count kinds", &
                test_unique_every_specific), &
            new_unittest("pf_rank: every type x both rank kinds", test_rank_every_specific), &
            new_unittest("minmax agrees with minval/maxval", test_minmax_basic), &
            new_unittest("minmax skips nulls and NaNs", test_minmax_skips), &
            new_unittest("argminmax reports the first of a tie", test_argminmax_ties), &
            new_unittest("argminmax accepts a parquet_column", test_argminmax_column), &
            new_unittest("merge equals a sort of the concatenation", test_merge_matches_sort), &
            new_unittest("merge tracks validity", test_merge_validity), &
            new_unittest("merge follows a descending order", test_merge_descending), &
            new_unittest("merge handles an empty input", test_merge_empty), &
            new_unittest("merge widens two string lengths", test_merge_string_widths), &
            new_unittest("pf_minmax: every type", test_minmax_every_specific), &
            new_unittest("pf_argminmax: every type x both index kinds", test_argminmax_every_specific), &
            new_unittest("pf_merge: every type", test_merge_every_specific), &
            new_unittest("a threaded sort equals the serial one", test_threads_identical), &
            new_unittest("threads are really created", test_threads_really_used), &
            new_unittest("auto is serial inside a parallel region", test_threads_auto_in_parallel), &
            new_unittest("threads=1 forces serial", test_threads_one_is_serial), &
            new_unittest("unique and rank take threads too", test_threads_on_derived), &
            new_unittest("every size from 2 to 400 threads identically", test_merge_size_sweep), &
            new_unittest("a threaded sort is still a permutation at every size", test_merge_sweep_is_permutation), &
            new_unittest("the final merge round is really co-ranked", test_merge_round_threads_used), &
            new_unittest("co-ranking survives its extreme inputs", test_merge_corank_extremes), &
            new_unittest("strings and multi-key merge identically", test_merge_key_families), &
            new_unittest("nulls and NaNs merge identically", test_merge_tiers), &
            new_unittest("assume_valid really skips the scan for a column", test_permute_column_assume_valid), &
            new_unittest("pf_is_sorted: every type, sorted and scrambled", test_is_sorted_every_specific), &
            new_unittest("pf_sort_keys%add: every type", test_keys_add_every_specific), &
            new_unittest("a parquet_column key of every runtime kind", test_argsort_column_every_kind), &
            new_unittest("group_offsets marks every run of equal rows", test_group_offsets_basic), &
            new_unittest("group_offsets handles empty, single and all-tied", test_group_offsets_edges), &
            new_unittest("all nulls form one group and all NaNs form one", test_group_offsets_tiers), &
            new_unittest("group_offsets does not change the permutation", test_group_offsets_same_perm), &
            new_unittest("group_offsets comes back in both kinds", test_group_offsets_kinds), &
            new_unittest("group_nkeys groups on a prefix of the keys", test_group_nkeys_prefix), &
            new_unittest("group_nkeys counts caller keys, not engine keys", test_group_nkeys_timestamp), &
            new_unittest("nkeys_added counts %add calls, not engine keys", test_nkeys_added_counts_adds), &
            new_unittest("is_sorted accepts a pf_sort_keys", test_is_sorted_keys), &
            new_unittest("partial_argsort accepts a pf_sort_keys", test_partial_argsort_keys), &
            new_unittest("engine: integer keys match the C++ comparators", test_engine_conf_int), &
            new_unittest("engine: real keys with NaNs match the C++ comparators", test_engine_conf_real), &
            new_unittest("engine: string keys match the C++ comparators", test_engine_conf_str), &
            new_unittest("engine: variable-length strings compare like memcmp", test_engine_conf_varstr), &
            new_unittest("engine: multi-key and the nkeys prefix match the C++ comparators", &
                test_engine_conf_multi), &
            new_unittest("engine: the Fortran/C++ engine selector really switches engines", &
                test_fortran_engine_switches), &
            new_unittest("engine: the Fortran sort matches the C++ one on every family and size", &
                test_fortran_engine_ab_families), &
            new_unittest("engine: the Fortran sort matches on degenerate input shapes", &
                test_fortran_engine_adversarial), &
            new_unittest("engine: the forced heapsort fallback matches the C++ sort", &
                test_fortran_engine_heapsort_fallback), &
            new_unittest("engine: the depth-limit hook really reaches the heapsort fallback", &
                test_fortran_engine_depth_limit_bites), &
            new_unittest("engine: the quicksort leaves every element within the insertion cutoff", &
                test_fortran_engine_presort_invariant) &
            ]
    end subroutine collect_tests_parquet_sorting
    !
    !> `perm(k)` must name the element belonging at position k, and gathering by it must sort.
    subroutine test_argsort_basic(error)
        type(error_type), allocatable, intent(out) :: error
        integer(int32) :: v(6) = [30, 10, 50, 20, 60, 40]
        integer(int32), allocatable :: perm(:)
        integer :: k

        call pf_argsort(v, perm)
        call check(error, all(perm == [2, 4, 1, 6, 3, 5]), "argsort must order v as rows 2,4,1,6,3,5")
        if (allocated(error)) return
        call check(error, all([(v(perm(k)), k = 1, 6)] == [10, 20, 30, 40, 50, 60]), &
            "gathering v by its own argsort must produce a sorted array")
        if (allocated(error)) return
        call check(error, all(v == [30, 10, 50, 20, 60, 40]), "pf_argsort must not modify its input")
    end subroutine test_argsort_basic
    !
    !> With no ties and no nulls, descending is the exact reverse of ascending.
    subroutine test_descending(error)
        type(error_type), allocatable, intent(out) :: error
        integer(int32) :: v(6) = [30, 10, 50, 20, 60, 40]
        integer(int32), allocatable :: up(:), down(:)

        call pf_argsort(v, up)
        call pf_argsort(v, down, descending=.true.)
        call check(error, all(down == up(6:1:-1)), "descending must be the exact reverse of ascending here")
    end subroutine test_descending
    !
    !> Every value equal: the only correct answer is the identity, and anything unstable will
    !> produce something else. This is the test that fails if the comparator's row-index
    !> tiebreaker is dropped or the counting path stops placing in order.
    subroutine test_stability(error)
        type(error_type), allocatable, intent(out) :: error
        integer(int32) :: v(8) = [7, 7, 7, 7, 7, 7, 7, 7]
        integer(int32), allocatable :: perm(:)
        integer :: k

        call pf_argsort(v, perm)
        call check(error, all(perm == [(k, k = 1, 8)]), "an all-tied array must sort to the identity permutation")
        if (allocated(error)) return
        call pf_argsort(v, perm, descending=.true.)
        call check(error, all(perm == [(k, k = 1, 8)]), &
            "an all-tied array must sort to the identity permutation when descending too")
    end subroutine test_stability
    !
    !> Nulls go last by default, and the values before them are in order.
    subroutine test_nulls_last(error)
        type(error_type), allocatable, intent(out) :: error
        integer(int32) :: v(5) = [30, 10, 50, 20, 40]
        logical :: ok(5) = [.true., .false., .true., .false., .true.]
        integer(int32), allocatable :: perm(:)

        call pf_argsort(v, perm, is_valid=ok)
        call check(error, all(perm(1:3) == [1, 5, 3]), "the three valid rows must come first, in value order")
        if (allocated(error)) return
        call check(error, all(perm(4:5) == [2, 4]), "the two null rows must come last, in original order")
    end subroutine test_nulls_last
    !
    !> `nulls_first` moves the null tier to the front without reordering anything within a tier.
    subroutine test_nulls_first(error)
        type(error_type), allocatable, intent(out) :: error
        integer(int32) :: v(5) = [30, 10, 50, 20, 40]
        logical :: ok(5) = [.true., .false., .true., .false., .true.]
        integer(int32), allocatable :: perm(:)

        call pf_argsort(v, perm, is_valid=ok, nulls_first=.true.)
        call check(error, all(perm(1:2) == [2, 4]), "the null rows must come first, in original order")
        if (allocated(error)) return
        call check(error, all(perm(3:5) == [1, 5, 3]), "the valid rows must follow, in value order")
    end subroutine test_nulls_first
    !
    !> Null placement is ABSOLUTE: `descending` reverses the values and leaves the tiers alone.
    !> A null-free array cannot tell the difference, which is why this needs its own test.
    subroutine test_descending_nulls_absolute(error)
        type(error_type), allocatable, intent(out) :: error
        integer(int32) :: v(5) = [30, 10, 50, 20, 40]
        logical :: ok(5) = [.true., .false., .true., .false., .true.]
        integer(int32), allocatable :: perm(:)

        call pf_argsort(v, perm, is_valid=ok, descending=.true.)
        call check(error, all(perm(1:3) == [3, 5, 1]), "descending must reverse the values")
        if (allocated(error)) return
        call check(error, all(perm(4:5) == [2, 4]), "descending must still leave the nulls last")
    end subroutine test_descending_nulls_absolute
    !
    !> A NaN is an ordinary value that sits in its own tier: values, then NaNs, then nulls.
    subroutine test_nan_tier(error)
        type(error_type), allocatable, intent(out) :: error
        real(real64) :: v(5)
        logical :: ok(5) = [.true., .true., .true., .false., .true.]
        integer(int32), allocatable :: perm(:)

        v = [3.0_real64, ieee_value(1.0_real64, ieee_quiet_nan), 1.0_real64, 9.0_real64, 2.0_real64]
        call pf_argsort(v, perm, is_valid=ok)
        call check(error, all(perm == [3, 5, 1, 2, 4]), &
            "ascending must give values 1,2,3 then the NaN then the null")
        if (allocated(error)) return
        call pf_argsort(v, perm, is_valid=ok, descending=.true.)
        call check(error, all(perm == [1, 5, 3, 2, 4]), &
            "descending must reverse only the values, leaving the NaN then the null last")
    end subroutine test_nan_tier
    !
    !> `pf_sort` produces an independent copy and never writes through to its input.
    subroutine test_sort_out_of_place(error)
        type(error_type), allocatable, intent(out) :: error
        integer(int32) :: v(6) = [30, 10, 50, 20, 60, 40]
        integer(int32), allocatable :: sorted(:)

        call pf_sort(v, sorted)
        call check(error, all(sorted == [10, 20, 30, 40, 50, 60]), "pf_sort must return the values in order")
        if (allocated(error)) return
        call check(error, all(v == [30, 10, 50, 20, 60, 40]), "pf_sort must leave its input untouched")
        if (allocated(error)) return
        sorted(1) = -1
        call check(error, v(2) == 10, "the sorted copy must be independent of the input")
    end subroutine test_sort_out_of_place
    !
    !> `pf_permute` by an array's own argsort must equal `pf_sort` of it -- the identity that ties
    !> the three operations together.
    subroutine test_permute_round_trip(error)
        type(error_type), allocatable, intent(out) :: error
        integer(int32) :: v(6) = [30, 10, 50, 20, 60, 40]
        integer(int32) :: w(6) = [30, 10, 50, 20, 60, 40]
        integer(int32), allocatable :: perm(:), sorted(:)

        call pf_argsort(v, perm)
        call pf_permute(v, perm)
        call pf_sort(w, sorted)
        call check(error, all(v == sorted), "permuting by an array's own argsort must equal sorting it")
        if (allocated(error)) return
        call check(error, all(perm == [2, 4, 1, 6, 3, 5]), "pf_permute must not modify perm")
    end subroutine test_permute_round_trip
    !
    !> `assume_valid=.true.` skips the check and must produce the identical result for a
    !> permutation that really is valid. (The rejection path is an error scenario.)
    subroutine test_permute_assume_valid(error)
        type(error_type), allocatable, intent(out) :: error
        integer(int32) :: a(5) = [5, 4, 3, 2, 1]
        integer(int32) :: b(5) = [5, 4, 3, 2, 1]
        integer(int32) :: perm(5) = [5, 4, 3, 2, 1]

        call pf_permute(a, perm)
        call pf_permute(b, perm, assume_valid=.true.)
        call check(error, all(a == b), "assume_valid must not change the result for a valid permutation")
        if (allocated(error)) return
        call check(error, all(a == [1, 2, 3, 4, 5]), "reversing a reversed array must sort it")
    end subroutine test_permute_assume_valid
    !
    !> `pf_permute` has 22 specifics -- eleven value types times two index kinds -- all emitted from
    !> ONE template in `tools/generate_parquet_sorting.py`. That is the risk this test exists for: a
    !> template slip is invisible in the generator's own `--check` (which compares the committed
    !> output against a fresh generation, so a wrong template matches perfectly) and a wrong
    !> permutation is a silently wrong answer, not a crash. So the sweep is exhaustive by
    !> construction rather than representative.
    !>
    !> **The `_i64` half is the half that was thin.** The specifics differ only in the index kind,
    !> and until this test existed eight of ten `pf_permute` call sites in this file passed an
    !> `integer(int32)` permutation -- which is what `pf_argsort` hands back into an `int32`
    !> variable, so it is the shape a test reaches for by default. Every case below runs both kinds
    !> over the same data and requires the same answer, so a specific that diverges from its twin
    !> fails here even if both are individually self-consistent.
    !>
    !> Five permutation shapes, chosen so that a plausible slip cannot hide behind all of them:
    !> identity (catches a body that permutes when it should not), reversal (catches an inverted
    !> permutation -- it is its own inverse, so it is the one shape that would NOT catch that, and
    !> is included for the boundary rather than for that property), scattered (catches an inverted
    !> or transposed index -- `perm` and its inverse differ here), a two-element swap (catches an
    !> off-by-one that leaves most elements in place) and a single element (the degenerate bound).
    !>
    !> Values are distinct per row wherever the type allows it, so any misplacement shows. `logical`
    !> is the one type that cannot manage six distinct values; its pattern is asymmetric and swept
    !> across all shapes instead, which is weaker and is why it is called out here rather than left
    !> to look equivalent.
    subroutine test_permute_all_specifics(error)
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: N = 6, NSH = 4
        !> Column `s` is one permutation shape: identity, reversal, scattered, two-element swap.
        integer(int32), parameter :: SH(N, NSH) = reshape([ &
            1_int32, 2_int32, 3_int32, 4_int32, 5_int32, 6_int32, &
            6_int32, 5_int32, 4_int32, 3_int32, 2_int32, 1_int32, &
            4_int32, 1_int32, 6_int32, 2_int32, 5_int32, 3_int32, &
            2_int32, 1_int32, 3_int32, 4_int32, 5_int32, 6_int32], [N, NSH])
        character(len=*), parameter :: SHNAME(NSH) = &
            [character(len=8) :: "identity", "reversal", "scatter", "swap"]
        integer :: s, ik
        integer(int32) :: p32(N)
        integer(int64) :: p64(N)
        character(len=32) :: ctx

        do s = 1, NSH
            do ik = 1, 2
                p32 = SH(:, s)
                p64 = int(SH(:, s), int64)
                write(ctx, "(A,A,A,I0)") "[", trim(SHNAME(s)), "/i", 32*ik
                ! ---- the six plain intrinsic types ----
                block
                    integer(int32) :: v(N), src(N) = [11_int32, 22_int32, 33_int32, 44_int32, 55_int32, 66_int32]
                    v = src
                    if (ik == 1) then
                        call pf_permute(v, p32)
                    else
                        call pf_permute(v, p64)
                    end if
                    call check(error, all(v == src(SH(:, s))), trim(ctx)//"] int32 permute")
                end block
                if (allocated(error)) return
                block
                    integer(int64) :: v(N), src(N) = [11_int64, 22_int64, 33_int64, 44_int64, 55_int64, 66_int64]
                    v = src
                    if (ik == 1) then
                        call pf_permute(v, p32)
                    else
                        call pf_permute(v, p64)
                    end if
                    call check(error, all(v == src(SH(:, s))), trim(ctx)//"] int64 permute")
                end block
                if (allocated(error)) return
                block
                    real(real32) :: v(N), src(N) = [1.5_real32, 2.5_real32, 3.5_real32, 4.5_real32, &
                        5.5_real32, 6.5_real32]
                    v = src
                    if (ik == 1) then
                        call pf_permute(v, p32)
                    else
                        call pf_permute(v, p64)
                    end if
                    call check(error, all(v == src(SH(:, s))), trim(ctx)//"] real32 permute")
                end block
                if (allocated(error)) return
                block
                    real(real64) :: v(N), src(N) = [1.5_real64, 2.5_real64, 3.5_real64, 4.5_real64, &
                        5.5_real64, 6.5_real64]
                    v = src
                    if (ik == 1) then
                        call pf_permute(v, p32)
                    else
                        call pf_permute(v, p64)
                    end if
                    call check(error, all(v == src(SH(:, s))), trim(ctx)//"] real64 permute")
                end block
                if (allocated(error)) return
                block
                    logical :: v(N), src(N) = [.true., .false., .true., .true., .false., .false.]
                    v = src
                    if (ik == 1) then
                        call pf_permute(v, p32)
                    else
                        call pf_permute(v, p64)
                    end if
                    call check(error, all(v .eqv. src(SH(:, s))), trim(ctx)//"] logical permute")
                end block
                if (allocated(error)) return
                block
                    character(len=4) :: v(N), src(N) = [character(len=4) :: "aa1", "bb2", "cc3", "dd4", "ee5", "ff6"]
                    v = src
                    if (ik == 1) then
                        call pf_permute(v, p32)
                    else
                        call pf_permute(v, p64)
                    end if
                    call check(error, all(v == src(SH(:, s))), trim(ctx)//"] character permute")
                end block
                if (allocated(error)) return
                ! ---- the three temporal element types, compared through their raw accessors ----
                block
                    type(parquet_date) :: v(N)
                    integer(int32) :: raws(N) = [101_int32, 202_int32, 303_int32, 404_int32, 505_int32, 606_int32]
                    integer :: k
                    call v%set_raw(raws)
                    if (ik == 1) then
                        call pf_permute(v, p32)
                    else
                        call pf_permute(v, p64)
                    end if
                    call check(error, all([(v(k)%raw(), k = 1, N)] == raws(SH(:, s))), &
                        trim(ctx)//"] parquet_date permute")
                end block
                if (allocated(error)) return
                block
                    type(parquet_time) :: v(N)
                    integer(int64) :: raws(N) = [101_int64, 202_int64, 303_int64, 404_int64, 505_int64, 606_int64]
                    integer :: k
                    call v%set_raw(raws)
                    if (ik == 1) then
                        call pf_permute(v, p32)
                    else
                        call pf_permute(v, p64)
                    end if
                    call check(error, all([(v(k)%raw(), k = 1, N)] == raws(SH(:, s))), &
                        trim(ctx)//"] parquet_time permute")
                end block
                if (allocated(error)) return
                block
                    type(parquet_timestamp) :: v(N)
                    integer(int64) :: secs(N) = [101_int64, 202_int64, 303_int64, 404_int64, 505_int64, 606_int64]
                    integer(int64) :: gs(N)
                    integer(int32) :: gn(N)
                    integer :: k
                    call v%set_raw(secs, 7_int32)
                    if (ik == 1) then
                        call pf_permute(v, p32)
                    else
                        call pf_permute(v, p64)
                    end if
                    do k = 1, N
                        call v(k)%get_raw(gs(k), gn(k))
                    end do
                    call check(error, all(gs == secs(SH(:, s))) .and. all(gn == 7_int32), &
                        trim(ctx)//"] parquet_timestamp permute")
                end block
                if (allocated(error)) return
                ! ---- the two container types, which delegate to their own %reindex ----
                block
                    type(parquet_string_column) :: sc
                    character(len=:), allocatable :: got
                    character(len=*), parameter :: SRC(N) = [character(len=4) :: "aa1", "bb2", "cc3", &
                        "dd4", "ee5", "ff6"]
                    integer :: k
                    logical :: ok
                    do k = 1, N
                        call sc%append_string(trim(SRC(k)))
                    end do
                    if (ik == 1) then
                        call pf_permute(sc, p32)
                    else
                        call pf_permute(sc, p64)
                    end if
                    ok = .true.
                    do k = 1, N
                        call sc%get(int(k, int64), got)
                        if (got /= trim(SRC(SH(k, s)))) ok = .false.
                    end do
                    call check(error, ok, trim(ctx)//"] parquet_string_column permute")
                end block
                if (allocated(error)) return
                block
                    type(parquet_column) :: c
                    integer(int32) :: src(N) = [11_int32, 22_int32, 33_int32, 44_int32, 55_int32, 66_int32]
                    integer(int32) :: got
                    integer :: k
                    logical :: ok
                    call c%init(PK_INT32, int(N, int64))
                    call c%set_all(src)
                    call c%set_null(2_int64)      ! a null must travel with its row, not stay put
                    if (ik == 1) then
                        call pf_permute(c, p32)
                    else
                        call pf_permute(c, p64)
                    end if
                    ok = .true.
                    do k = 1, N
                        if (SH(k, s) == 2) then
                            if (.not. c%is_null(int(k, int64))) ok = .false.
                        else
                            if (c%is_null(int(k, int64))) ok = .false.
                            call c%get_at(int(k, int64), got)
                            if (got /= src(SH(k, s))) ok = .false.
                        end if
                    end do
                    call check(error, ok, trim(ctx)//"] parquet_column permute (value and null)")
                end block
                if (allocated(error)) return
            end do
        end do

        ! ---- the degenerate bound: a single element, every type reached through the same path ----
        block
            integer(int32) :: a(1) = [7_int32], q32(1) = [1_int32]
            integer(int64) :: b(1) = [7_int64], q64(1) = [1_int64]
            call pf_permute(a, q32)
            call pf_permute(b, q64)
            call check(error, a(1) == 7_int32 .and. b(1) == 7_int64, &
                "a single-element permutation must be a no-op in both index kinds")
        end block
    end subroutine test_permute_all_specifics
    !
    !> `pf_is_sorted` must answer .false. before and .true. after sorting the same array -- the
    !> two share a comparator, so they cannot be allowed to disagree.
    subroutine test_is_sorted_agrees(error)
        type(error_type), allocatable, intent(out) :: error
        integer(int32) :: v(6) = [30, 10, 50, 20, 60, 40]
        integer(int32), allocatable :: perm(:)
        logical :: before, after

        call pf_is_sorted(v, before)
        call pf_argsort(v, perm)
        call pf_permute(v, perm)
        call pf_is_sorted(v, after)
        call check(error, .not. before, "an unsorted array must not report itself sorted")
        if (allocated(error)) return
        call check(error, after, "an array must report itself sorted once sorted")
    end subroutine test_is_sorted_agrees
    !
    !> `is_sorted` uses the same `descending`/`nulls_first` options the sort does, and a run of
    !> equal values counts as sorted (no index tiebreaker in the adjacent-pair comparison).
    subroutine test_is_sorted_options(error)
        type(error_type), allocatable, intent(out) :: error
        integer(int32) :: down(4) = [9, 7, 5, 1]
        integer(int32) :: tied(4) = [3, 3, 3, 3]
        integer(int32) :: withnull(3) = [1, 5, 9]
        logical :: ok(3) = [.true., .false., .true.]
        logical :: a1, a2, a3, a4

        call pf_is_sorted(down, a1, descending=.true.)
        call check(error, a1, "a descending array must report sorted under descending=.true.")
        if (allocated(error)) return
        call pf_is_sorted(down, a2)
        call check(error, .not. a2, "a descending array must not report sorted under the default")
        if (allocated(error)) return
        call pf_is_sorted(tied, a3)
        call check(error, a3, "a run of equal values must count as sorted")
        if (allocated(error)) return
        ! [1, null, 9] is NOT sorted with nulls last, but IS with nulls first... no: nulls first
        ! wants the null at position 1. Neither holds, which is what makes this a useful check.
        call pf_is_sorted(withnull, a4, is_valid=ok)
        call check(error, .not. a4, "a null in the middle must not report sorted with nulls last")
    end subroutine test_is_sorted_options
    !
    !> The int32 and int64 permutation forms are the same computation, narrowed at the end.
    subroutine test_perm_kinds_agree(error)
        type(error_type), allocatable, intent(out) :: error
        integer(int32) :: v(6) = [30, 10, 50, 20, 60, 40]
        integer(int32), allocatable :: p32(:)
        integer(int64), allocatable :: p64(:)

        call pf_argsort(v, p32)
        call pf_argsort(v, p64)
        call check(error, all(int(p32, int64) == p64), &
            "the int32 and int64 permutation forms must agree element for element")
    end subroutine test_perm_kinds_agree
    !
    !> One shared expectation across the four numeric kinds: each must sort to the same order,
    !> since the values correspond. Widening to int64/real64 is exact for all four.
    subroutine test_all_scalar_types(error)
        type(error_type), allocatable, intent(out) :: error
        integer(int32) :: a(5) = [30, 10, 50, 20, 40]
        integer(int64) :: b(5) = [30, 10, 50, 20, 40]
        real(real32) :: c(5) = [30.0, 10.0, 50.0, 20.0, 40.0]
        real(real64) :: d(5) = [30.0_real64, 10.0_real64, 50.0_real64, 20.0_real64, 40.0_real64]
        integer(int32), allocatable :: pa(:), pb(:), pc(:), pd(:)

        call pf_argsort(a, pa)
        call pf_argsort(b, pb)
        call pf_argsort(c, pc)
        call pf_argsort(d, pd)
        call check(error, all(pa == [2, 4, 1, 5, 3]), "int32 must sort to 2,4,1,5,3")
        if (allocated(error)) return
        call check(error, all(pb == pa), "int64 must sort exactly as int32 does on the same values")
        if (allocated(error)) return
        call check(error, all(pc == pa), "real32 must sort exactly as int32 does on the same values")
        if (allocated(error)) return
        call check(error, all(pd == pa), "real64 must sort exactly as int32 does on the same values")
    end subroutine test_all_scalar_types
    !
    !> **Every `pf_argsort` specific, once each, in both of its shapes** -- twelve value families
    !> times the two index kinds, each called with and without `group_offsets`. The int32 bodies
    !> branch on that argument (they must narrow a second array when it is asked for), so a call
    !> without it reaches only half of one, and every `group_offsets` test in this suite before
    !> this one used an integer key.
    !>
    !> `SWDUP` is the fixture because groups need ties: three classes of two, so the offsets are
    !> `[1, 3, 5, 7]` -- a body that emitted one group per ROW would give seven entries, and one
    !> that emitted a single group would give two.
    subroutine test_argsort_every_specific(error)
        type(error_type), allocatable, intent(out) :: error
        !> Ascending, ties in original order: class 1 is rows 2 and 5, class 2 rows 4 and 6, class
        !> 3 rows 1 and 3.
        integer, parameter :: PEXP(SWN) = [2, 5, 4, 6, 1, 3]
        !> Three groups of two, plus the one-past-the-end sentinel.
        integer, parameter :: OEXP(4) = [1, 3, 5, 7]
        !> The same two for the logical fixture: three .false. then three .true.
        integer, parameter :: PBOOL(SWN) = [2, 5, 6, 1, 3, 4]
        integer, parameter :: OBOOL(3) = [1, 4, 7]
        integer(int32), allocatable :: p32(:), o32(:)
        integer(int64), allocatable :: p64(:), o64(:)
        integer :: k

        block
            integer(int32) :: v(SWN)
            v = 10_int32 * int(SWDUP, int32)
            call pf_argsort(v, p32)
            call pf_argsort(v, p64)
            call check(error, sweep_group_ok(p32, o32, p64, o64, PEXP), "int32 pf_argsort")
            if (allocated(error)) return
            call pf_argsort(v, p32, group_offsets=o32)
            call pf_argsort(v, p64, group_offsets=o64)
            call check(error, sweep_group_ok(p32, o32, p64, o64, PEXP, OEXP), &
                "int32 pf_argsort must report three groups of two")
        end block
        if (allocated(error)) return
        block
            integer(int64) :: v(SWN)
            v = 100_int64 * int(SWDUP, int64)
            call pf_argsort(v, p32, group_offsets=o32)
            call pf_argsort(v, p64, group_offsets=o64)
            call check(error, sweep_group_ok(p32, o32, p64, o64, PEXP, OEXP), &
                "int64 pf_argsort must report three groups of two")
        end block
        if (allocated(error)) return
        block
            real(real32) :: v(SWN)
            v = real(SWDUP, real32) + 0.5_real32
            call pf_argsort(v, p32, group_offsets=o32)
            call pf_argsort(v, p64, group_offsets=o64)
            call check(error, sweep_group_ok(p32, o32, p64, o64, PEXP, OEXP), &
                "real32 pf_argsort must report three groups of two")
        end block
        if (allocated(error)) return
        block
            real(real64) :: v(SWN)
            v = real(SWDUP, real64) + 0.25_real64
            call pf_argsort(v, p32, group_offsets=o32)
            call pf_argsort(v, p64, group_offsets=o64)
            call check(error, sweep_group_ok(p32, o32, p64, o64, PEXP, OEXP), &
                "real64 pf_argsort must report three groups of two")
        end block
        if (allocated(error)) return
        block
            call pf_argsort(SWBOOL, p32, group_offsets=o32)
            call pf_argsort(SWBOOL, p64, group_offsets=o64)
            call check(error, sweep_group_ok(p32, o32, p64, o64, PBOOL, OBOOL), &
                "logical pf_argsort must report two groups of three")
        end block
        if (allocated(error)) return
        block
            character(len=2) :: v(SWN)
            do k = 1, SWN
                v(k) = swchr(SWDUP(k))
            end do
            call pf_argsort(v, p32, group_offsets=o32)
            call pf_argsort(v, p64, group_offsets=o64)
            call check(error, sweep_group_ok(p32, o32, p64, o64, PEXP, OEXP), &
                "character pf_argsort must report three groups of two")
        end block
        if (allocated(error)) return
        block
            type(parquet_date) :: v(SWN)
            call v%set_raw(1000_int32 + int(SWDUP, int32))
            call pf_argsort(v, p32, group_offsets=o32)
            call pf_argsort(v, p64, group_offsets=o64)
            call check(error, sweep_group_ok(p32, o32, p64, o64, PEXP, OEXP), &
                "parquet_date pf_argsort must report three groups of two")
        end block
        if (allocated(error)) return
        block
            type(parquet_time) :: v(SWN)
            call v%set_raw(2000_int64 + int(SWDUP, int64))
            call pf_argsort(v, p32, group_offsets=o32)
            call pf_argsort(v, p64, group_offsets=o64)
            call check(error, sweep_group_ok(p32, o32, p64, o64, PEXP, OEXP), &
                "parquet_time pf_argsort must report three groups of two")
        end block
        if (allocated(error)) return
        block
            type(parquet_timestamp) :: v(SWN)
            call v%set_raw(3000_int64 + int(SWDUP, int64), 7_int32)
            call pf_argsort(v, p32, group_offsets=o32)
            call pf_argsort(v, p64, group_offsets=o64)
            call check(error, sweep_group_ok(p32, o32, p64, o64, PEXP, OEXP), &
                "parquet_timestamp pf_argsort must report three groups of two")
        end block
        if (allocated(error)) return
        block
            type(parquet_string_column) :: sc
            do k = 1, SWN
                call sc%append_string(swchr(SWDUP(k)))
            end do
            call pf_argsort(sc, p32, group_offsets=o32)
            call pf_argsort(sc, p64, group_offsets=o64)
            call check(error, sweep_group_ok(p32, o32, p64, o64, PEXP, OEXP), &
                "parquet_string_column pf_argsort must report three groups of two")
        end block
        if (allocated(error)) return
        block
            type(parquet_column) :: c
            call c%init(PK_INT32, int(SWN, int64))
            call c%set_all(10_int32 * int(SWDUP, int32))
            call pf_argsort(c, p32, group_offsets=o32)
            call pf_argsort(c, p64, group_offsets=o64)
            call check(error, sweep_group_ok(p32, o32, p64, o64, PEXP, OEXP), &
                "parquet_column pf_argsort must report three groups of two")
        end block
        if (allocated(error)) return
        block
            type(pf_sort_keys) :: keys
            integer(int32) :: v(SWN)
            v = 10_int32 * int(SWDUP, int32)
            call keys%add(v)
            call pf_argsort(keys, p32)
            call pf_argsort(keys, p64)
            call check(error, sweep_group_ok(p32, o32, p64, o64, PEXP), "pf_sort_keys pf_argsort")
            if (allocated(error)) return
            call pf_argsort(keys, p32, group_offsets=o32)
            call pf_argsort(keys, p64, group_offsets=o64)
            call check(error, sweep_group_ok(p32, o32, p64, o64, PEXP, OEXP), &
                "pf_sort_keys pf_argsort must report three groups of two")
        end block
    end subroutine test_argsort_every_specific
    !
    !> **Every `pf_sort` specific, once each** -- the nine families that copy their values out.
    !> The six taking a mask are called a second time with `is_valid` AND `sorted_valid`, which is
    !> the only shape reaching a body's validity-gathering branch.
    subroutine test_sort_every_specific(error)
        type(error_type), allocatable, intent(out) :: error
        !> Five valid rows in rank order, then the one null, which sorts last.
        logical, parameter :: SVEXP(SWN) = [.true., .true., .true., .true., .true., .false.]
        logical :: mask(SWN)
        integer :: k

        mask = .true.
        mask(SWASC(1)) = .false.        ! the SMALLEST value is the null, so it has to travel
        block
            integer(int32) :: v(SWN)
            integer(int32), allocatable :: s(:)
            logical, allocatable :: sv(:)
            v = 10_int32 * int(SWRANK, int32)
            call pf_sort(v, s)
            call check(error, all(s == [(10_int32 * int(k, int32), k = 1, SWN)]), &
                "int32 pf_sort must order the whole fixture")
            if (allocated(error)) return
            call pf_sort(v, s, is_valid=mask, sorted_valid=sv)
            call check(error, all(sv .eqv. SVEXP) .and. s(1) == 20_int32 .and. s(SWN) == 10_int32, &
                "int32 pf_sort must carry validity into sorted_valid, the null last")
        end block
        if (allocated(error)) return
        block
            integer(int64) :: v(SWN)
            integer(int64), allocatable :: s(:)
            logical, allocatable :: sv(:)
            v = 100_int64 * int(SWRANK, int64)
            call pf_sort(v, s)
            call check(error, all(s == [(100_int64 * int(k, int64), k = 1, SWN)]), &
                "int64 pf_sort must order the whole fixture")
            if (allocated(error)) return
            call pf_sort(v, s, is_valid=mask, sorted_valid=sv)
            call check(error, all(sv .eqv. SVEXP) .and. s(1) == 200_int64 .and. s(SWN) == 100_int64, &
                "int64 pf_sort must carry validity into sorted_valid, the null last")
        end block
        if (allocated(error)) return
        block
            real(real32) :: v(SWN)
            real(real32), allocatable :: s(:)
            logical, allocatable :: sv(:)
            v = real(SWRANK, real32) + 0.5_real32
            call pf_sort(v, s)
            call check(error, all(s == [(real(k, real32) + 0.5_real32, k = 1, SWN)]), &
                "real32 pf_sort must order the whole fixture")
            if (allocated(error)) return
            call pf_sort(v, s, is_valid=mask, sorted_valid=sv)
            call check(error, all(sv .eqv. SVEXP) .and. s(1) == 2.5_real32 .and. s(SWN) == 1.5_real32, &
                "real32 pf_sort must carry validity into sorted_valid, the null last")
        end block
        if (allocated(error)) return
        block
            real(real64) :: v(SWN)
            real(real64), allocatable :: s(:)
            logical, allocatable :: sv(:)
            v = real(SWRANK, real64) + 0.25_real64
            call pf_sort(v, s)
            call check(error, all(s == [(real(k, real64) + 0.25_real64, k = 1, SWN)]), &
                "real64 pf_sort must order the whole fixture")
            if (allocated(error)) return
            call pf_sort(v, s, is_valid=mask, sorted_valid=sv)
            call check(error, all(sv .eqv. SVEXP) .and. s(1) == 2.25_real64 .and. s(SWN) == 1.25_real64, &
                "real64 pf_sort must carry validity into sorted_valid, the null last")
        end block
        if (allocated(error)) return
        block
            logical, allocatable :: s(:), sv(:)
            logical :: bmask(SWN)
            call pf_sort(SWBOOL, s)
            call check(error, .not. any(s(1:3)) .and. all(s(4:SWN)), &
                "logical pf_sort must put all three .false. before all three .true.")
            if (allocated(error)) return
            bmask = .true.
            bmask(2) = .false.          ! the first .false., i.e. the smallest, is the null
            call pf_sort(SWBOOL, s, is_valid=bmask, sorted_valid=sv)
            call check(error, all(sv .eqv. SVEXP) .and. .not. s(1) .and. s(3), &
                "logical pf_sort must carry validity into sorted_valid, the null last")
        end block
        if (allocated(error)) return
        block
            character(len=2) :: v(SWN)
            character(len=2), allocatable :: s(:)
            logical, allocatable :: sv(:)
            do k = 1, SWN
                v(k) = swchr(SWRANK(k))
            end do
            call pf_sort(v, s)
            call check(error, all(s == [(swchr(k), k = 1, SWN)]), &
                "character pf_sort must order the whole fixture")
            if (allocated(error)) return
            call pf_sort(v, s, is_valid=mask, sorted_valid=sv)
            call check(error, all(sv .eqv. SVEXP) .and. s(1) == swchr(2) .and. s(SWN) == swchr(1), &
                "character pf_sort must carry validity into sorted_valid, the null last")
        end block
        if (allocated(error)) return
        block
            type(parquet_date) :: v(SWN)
            type(parquet_date), allocatable :: s(:)
            call v%set_raw(1000_int32 + int(SWRANK, int32))
            call pf_sort(v, s)
            call check(error, all([(s(k)%raw(), k = 1, SWN)] == [(1000 + k, k = 1, SWN)]), &
                "parquet_date pf_sort must order the whole fixture")
        end block
        if (allocated(error)) return
        block
            type(parquet_time) :: v(SWN)
            type(parquet_time), allocatable :: s(:)
            call v%set_raw(2000_int64 + int(SWRANK, int64))
            call pf_sort(v, s)
            call check(error, all([(s(k)%raw(), k = 1, SWN)] == [(2000_int64 + int(k, int64), k = 1, SWN)]), &
                "parquet_time pf_sort must order the whole fixture")
        end block
        if (allocated(error)) return
        block
            type(parquet_timestamp) :: v(SWN)
            type(parquet_timestamp), allocatable :: s(:)
            integer(int64) :: gs(SWN)
            integer(int32) :: gn(SWN)
            call v%set_raw(3000_int64 + int(SWRANK, int64), 7_int32)
            call pf_sort(v, s, descending=.true.)
            do k = 1, SWN
                call s(k)%get_raw(gs(k), gn(k))
            end do
            call check(error, all(gs == [(3000_int64 + int(SWN - k + 1, int64), k = 1, SWN)]) .and. &
                all(gn == 7_int32), &
                "parquet_timestamp pf_sort must order the whole fixture, reversed under descending")
        end block
    end subroutine test_sort_every_specific
    !
    !> A permutation in both index kinds, and optionally the group offsets in both, against the
    !> expected lists. Omitting `wanto` says the offsets were not asked for on this call and must
    !> not be examined -- an unallocated array, not an empty one.
    pure function sweep_group_ok(p32, o32, p64, o64, wantp, wanto) result(ok)
        integer(int32), allocatable, intent(in) :: p32(:) !! the int32 permutation.
        integer(int32), allocatable, intent(in) :: o32(:) !! the int32 group offsets, if asked for.
        integer(int64), allocatable, intent(in) :: p64(:) !! the int64 permutation.
        integer(int64), allocatable, intent(in) :: o64(:) !! the int64 group offsets, if asked for.
        integer, intent(in) :: wantp(:)                   !! the expected permutation.
        integer, intent(in), optional :: wanto(:)         !! the expected offsets, when asked for.
        logical :: ok                                     !! .true. when every requested list matches.
        ok = allocated(p32) .and. allocated(p64)
        if (ok) ok = size(p32) == size(wantp) .and. size(p64) == size(wantp)
        if (ok) ok = all(p32 == int(wantp, int32)) .and. all(p64 == int(wantp, int64))
        if (ok .and. present(wanto)) then
            ok = allocated(o32) .and. allocated(o64)
            if (ok) ok = size(o32) == size(wanto) .and. size(o64) == size(wanto)
            if (ok) ok = all(o32 == int(wanto, int32)) .and. all(o64 == int(wanto, int64))
        end if
    end function sweep_group_ok
    !
    !> A `logical` binds as 0/1, so .false. sorts before .true. and equal values stay stable.
    subroutine test_logical(error)
        type(error_type), allocatable, intent(out) :: error
        logical :: v(5) = [.true., .false., .true., .false., .true.]
        integer(int32), allocatable :: perm(:)
        logical, allocatable :: sorted(:)

        call pf_argsort(v, perm)
        call check(error, all(perm == [2, 4, 1, 3, 5]), ".false. rows must come first, each group in original order")
        if (allocated(error)) return
        call pf_sort(v, sorted, descending=.true.)
        call check(error, all(sorted .eqv. [.true., .true., .true., .false., .false.]), &
            "descending must put .true. first")
    end subroutine test_logical
    !
    !> `character(len=*)` is compared over its FULL declared length, trailing blanks included --
    !> which is exactly Fortran's own `<` for equal-length strings, so pf_is_sorted must agree
    !> with a hand-written comparison loop rather than quietly trimming behind it.
    subroutine test_character_padding(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=4) :: v(4) = ["bb  ", "aa  ", "b   ", "aaa "]
        character(len=4), allocatable :: sorted(:)
        logical :: ok
        integer :: k

        call pf_sort(v, sorted)
        call check(error, all(sorted == ["aa  ", "aaa ", "b   ", "bb  "]), &
            "strings must sort as Fortran compares them, blanks included")
        if (allocated(error)) return
        call pf_is_sorted(sorted, ok)
        call check(error, ok, "the sorted result must report itself sorted")
        if (allocated(error)) return
        ok = .true.
        do k = 1, 3
            if (sorted(k) > sorted(k + 1)) ok = .false.
        end do
        call check(error, ok, "pf_sort's order must agree with Fortran's own < operator")
    end subroutine test_character_padding
    !
    !> `parquet_date`/`parquet_time` carry their own null state and sort by their raw values.
    subroutine test_date_time(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_date) :: d(4)
        type(parquet_time) :: t(3)
        integer(int32), allocatable :: pd(:), pt(:)

        call d(1)%set(2024, 5, 1)
        call d(2)%set(1999, 1, 2)
        call d(4)%set(2024, 4, 30)     ! d(3) left null
        call pf_argsort(d, pd)
        call check(error, all(pd == [2, 4, 1, 3]), "dates must sort chronologically with the null last")
        if (allocated(error)) return
        call t(1)%set(12, 0, 0)
        call t(2)%set(6, 30, 0)
        call t(3)%set(23, 59, 59)
        call pf_argsort(t, pt)
        call check(error, all(pt == [2, 1, 3]), "times must sort by time of day")
    end subroutine test_date_time
    !
    !> A `parquet_timestamp` binds as TWO integer keys: seconds lead, nanoseconds break their
    !> ties. Folding the pair into one int64 would overflow outside roughly 1678-2262, so this
    !> asserts the second key is really consulted -- every row here shares a second.
    subroutine test_timestamp_two_keys(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_timestamp) :: ts(4)
        integer(int32), allocatable :: perm(:)
        logical :: ok

        call ts(1)%set_raw(100_int64, 500)
        call ts(2)%set_raw(100_int64, 100)
        call ts(3)%set_raw(99_int64, 900)
        call ts(4)%set_raw(100_int64, 300)
        call pf_argsort(ts, perm)
        call check(error, all(perm == [3, 2, 4, 1]), &
            "timestamps must order by seconds then by nanoseconds within a second")
        if (allocated(error)) return
        call pf_is_sorted(ts, ok)
        call check(error, .not. ok, "the unsorted timestamps must not report themselves sorted")
        if (allocated(error)) return
        call pf_permute(ts, perm)
        call pf_is_sorted(ts, ok)
        call check(error, ok, "the permuted timestamps must report themselves sorted")
    end subroutine test_timestamp_two_keys
    !
    !> A `parquet_string_column` carries its own nulls, and `pf_permute` delegates to its
    !> `%reindex` -- so the null must travel with its row rather than staying put.
    subroutine test_string_column(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_string_column) :: sc
        integer(int32), allocatable :: perm(:)
        character(len=:), allocatable :: s

        call sc%append_string("pear")
        call sc%append_null()
        call sc%append_string("aaaa")
        call sc%append_string("mint")
        call pf_argsort(sc, perm)
        call check(error, all(perm == [3, 4, 1, 2]), "the string column must sort with its null last")
        if (allocated(error)) return
        call pf_permute(sc, perm)
        call sc%get(1, s)
        call check(error, s == "aaaa", "after permuting, element 1 must be 'aaaa'")
        if (allocated(error)) return
        call check(error, sc%is_null(4), "after permuting, the null must have moved to element 4")
        if (allocated(error)) return
        call check(error, .not. sc%is_null(1), "after permuting, element 1 must not be null")
    end subroutine test_string_column
    !
    !> A `parquet_column` resolves its element kind at runtime, so the same call has to reach a
    !> different extractor per kind. Two kinds are checked here, plus its own validity.
    subroutine test_parquet_column(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_column) :: col, scol
        integer(int32), allocatable :: perm(:)
        real(real64) :: got
        logical :: ok

        call col%init(PK_FLOAT64, 5_int64)
        call col%set_at(1_int64, 3.5_real64)
        call col%set_at(2_int64, 1.5_real64)
        call col%set_at(3_int64, 9.5_real64)
        call col%set_at(4_int64, 0.5_real64)
        call col%set_at(5_int64, 2.5_real64)
        call col%set_null(3_int64)
        call pf_argsort(col, perm)
        call check(error, all(perm == [4, 2, 5, 1, 3]), "a float64 column must sort by value with its null last")
        if (allocated(error)) return
        call pf_permute(col, perm)
        call col%get_at(1_int64, got)
        call check(error, got == 0.5_real64, "after permuting, row 1 must hold the smallest value")
        if (allocated(error)) return
        call check(error, col%is_null(5_int64), "after permuting, the null must have moved to row 5")
        if (allocated(error)) return
        call scol%init(PK_STRING, 3_int64)
        call scol%set_at(1_int64, "pear")
        call scol%set_at(2_int64, "aaaa")
        call scol%set_at(3_int64, "mint")
        call pf_argsort(scol, perm)
        call check(error, all(perm == [2, 3, 1]), "a string column must sort lexicographically")
        if (allocated(error)) return
        call pf_is_sorted(scol, ok)
        call check(error, .not. ok, "the unsorted string column must not report itself sorted")
    end subroutine test_parquet_column
    !
    !> The second key must decide only where the first key ties, and nowhere else.
    subroutine test_multi_key(error)
        type(error_type), allocatable, intent(out) :: error
        type(pf_sort_keys) :: k
        integer(int32) :: primary(6) = [2, 1, 2, 1, 2, 1]
        integer(int32) :: secondary(6) = [30, 60, 10, 40, 20, 50]
        integer(int32), allocatable :: perm(:)

        call k%add(primary)
        call k%add(secondary)
        call check(error, k%nkeys_added() == 2, "two %add calls must report two keys")
        if (allocated(error)) return
        call pf_argsort(k, perm)
        call check(error, all(perm == [4, 6, 2, 3, 5, 1]), &
            "rows must group by the primary key and order by the secondary within each group")
    end subroutine test_multi_key
    !
    !> Keys of different types combine, which is the whole reason the builder exists -- a generic
    !> could not offer "an optional second array of any type".
    subroutine test_multi_key_mixed(error)
        type(error_type), allocatable, intent(out) :: error
        type(pf_sort_keys) :: k
        character(len=3) :: name(4) = ["bb ", "aa ", "bb ", "aa "]
        real(real64) :: val(4) = [1.0_real64, 4.0_real64, 2.0_real64, 3.0_real64]
        integer(int32), allocatable :: perm(:)

        call k%add(name)
        call k%add(val, descending=.true.)
        call pf_argsort(k, perm)
        call check(error, all(perm == [2, 4, 3, 1]), &
            "a string primary key with a descending real tiebreak must give rows 2,4,3,1")
    end subroutine test_multi_key_mixed
    !
    !> Each key carries its own direction: the first ascending, the second descending, and the
    !> result must differ from making both ascending.
    subroutine test_multi_key_directions(error)
        type(error_type), allocatable, intent(out) :: error
        type(pf_sort_keys) :: same, mixed
        integer(int32) :: primary(4) = [1, 1, 2, 2]
        integer(int32) :: secondary(4) = [10, 20, 10, 20]
        integer(int32), allocatable :: p_same(:), p_mixed(:)

        call same%add(primary)
        call same%add(secondary)
        call pf_argsort(same, p_same)
        call mixed%add(primary)
        call mixed%add(secondary, descending=.true.)
        call pf_argsort(mixed, p_mixed)
        call check(error, all(p_same == [1, 2, 3, 4]), "both keys ascending must give 1,2,3,4")
        if (allocated(error)) return
        call check(error, all(p_mixed == [2, 1, 4, 3]), &
            "a descending second key must reverse within each primary group only")
    end subroutine test_multi_key_directions
    !
    !> `%clear` drops every key, so the same object can be reused for an unrelated sort.
    subroutine test_keys_clear(error)
        type(error_type), allocatable, intent(out) :: error
        type(pf_sort_keys) :: k
        integer(int32) :: a(3) = [3, 1, 2]
        integer(int32) :: b(4) = [4, 3, 2, 1]
        integer(int32), allocatable :: perm(:)

        call k%add(a)
        call k%clear()
        call check(error, k%nkeys_added() == 0, "%clear must leave no keys")
        if (allocated(error)) return
        ! A different LENGTH proves the row count was reset too, not just the key list.
        call k%add(b)
        call pf_argsort(k, perm)
        call check(error, all(perm == [4, 3, 2, 1]), "a cleared key list must accept a differently sized key")
    end subroutine test_keys_clear
    !
    !> Zero and one element must not need a special case at any call site.
    subroutine test_degenerate_sizes(error)
        type(error_type), allocatable, intent(out) :: error
        integer(int32) :: empty(0), one(1) = [42]
        integer(int32), allocatable :: perm(:), sorted(:)
        logical :: ok

        call pf_argsort(empty, perm)
        call check(error, size(perm) == 0, "an empty array must give an empty permutation")
        if (allocated(error)) return
        call pf_is_sorted(empty, ok)
        call check(error, ok, "an empty array must report itself sorted")
        if (allocated(error)) return
        call pf_argsort(one, perm)
        call check(error, size(perm) == 1 .and. perm(1) == 1, "a one-element array must give the identity")
        if (allocated(error)) return
        call pf_sort(one, sorted)
        call check(error, size(sorted) == 1 .and. sorted(1) == 42, "sorting one element must return it")
        if (allocated(error)) return
        call pf_permute(one, perm)
        call check(error, one(1) == 42, "permuting one element by the identity must leave it alone")
    end subroutine test_degenerate_sizes
    !
    !> **The test that justifies sharing an engine.** The same values are ordered two ways -- by
    !> the reader's `sort_by=` on the way out of a file, and by `pf_argsort` in memory -- and the
    !> two must agree row for row. If either path ever grew its own comparator, null placement,
    !> NaN placement or tie order could drift apart and no other test in either suite would
    !> notice, because each one only ever checks its own side.
    subroutine test_oracle_matches_read_time_sort(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), parameter :: file = "test_run/pf_sorting_oracle.parquet"
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        type(parquet_sortkey) :: srt
        integer(int32) :: id(8) = [1, 2, 3, 4, 5, 6, 7, 8]
        real(real64) :: v(8) = [3.5_real64, -1.0_real64, 3.5_real64, 9.25_real64, &
            0.0_real64, -7.5_real64, 2.0_real64, 3.5_real64]
        integer(int32), allocatable :: from_reader(:), perm(:)
        integer(int64) :: nrows
        integer :: k

        call parquet_open_writer(writer, file)
        call parquet_write_column(writer, "id", id)
        call parquet_write_column(writer, "v", v)
        call parquet_close_writer(writer)

        call srt%add("v asc")
        call parquet_open_reader(reader, file, sort_by=srt)
        call parquet_get_nrows(reader, nrows)
        allocate(from_reader(nrows))
        call parquet_read_column(reader, "id", from_reader)
        call parquet_close_reader(reader)

        call pf_argsort(v, perm)
        call check(error, size(from_reader) == 8, "the fixture must read back eight rows")
        if (allocated(error)) return
        ! `id(k) == k`, so the reader's id column IS the permutation it applied.
        call check(error, all(from_reader == perm), &
            "pf_argsort must produce exactly the row order a read-time sort_by= produces")
        if (allocated(error)) return
        ! The three tied 3.5 values are what make this more than a smoke test: both paths must
        ! break the tie the same way (original file order), not merely sort the distinct values.
        call check(error, all([(v(perm(k)), k = 1, 8)] == [-7.5_real64, -1.0_real64, 0.0_real64, &
            2.0_real64, 3.5_real64, 3.5_real64, 3.5_real64, 9.25_real64]), &
            "gathering by the permutation must give the values in ascending order")
    end subroutine test_oracle_matches_read_time_sort
    !
    !> The integer counting fast path is a SECOND code path producing the same answer, so it is
    !> compared against the comparator path rather than trusted. Low-cardinality integers are what
    !> select it; the debug hook forces the comparator path for the same input.
    !>
    !> The hook is process-global, which is why this suite is excluded from test/run_tester.f90's
    !> per-test parallelism.
    subroutine test_counting_path_agrees(error)
        type(error_type), allocatable, intent(out) :: error
        integer(int32) :: v(40)
        integer(int32), allocatable :: fast(:), slow(:)
        integer(int64) :: cmp_fast, cmp_slow
        integer :: k

        do k = 1, 40
            v(k) = int(mod(k * 7, 5), int32)   ! only five distinct values: the counting path's case
        end do
        call arm_sort_comparisons()
        call pf_argsort(v, fast)
        cmp_fast = sort_comparisons()
        call parquet_set_sort_counting_path(.false.)
        call arm_sort_comparisons()
        call pf_argsort(v, slow)
        cmp_slow = sort_comparisons()
        call parquet_set_sort_counting_path(.true.)
        call check(error, cmp_fast == 0_int64 .and. cmp_slow > 0_int64, &
            "the two halves must reach DIFFERENT engines, or the agreement below is vacuous")
        if (allocated(error)) return
        call check(error, all(fast == slow), &
            "the counting fast path must produce exactly the comparator path's permutation")
        if (allocated(error)) return
        call check(error, all([(v(fast(k)), k = 1, 40)] == [(v(slow(k)), k = 1, 40)]), &
            "both paths must gather the same values in the same order")
    end subroutine test_counting_path_agrees
    !
    !> The counting fast path with NULLS, which it used to decline outright.
    !!
    !! Declining cost the whole fast path to a single null anywhere in the column -- a step
    !! function of WHETHER a null exists, not how many, measured as a 3.1x end-to-end loss on a
    !! 4M-row column at 0.1% null density. Nulls are a TIER in this engine, never a value, so they
    !! form one contiguous block the permutation places directly.
    !!
    !! **Every case sweeps `descending` x `nulls_first`, and that is the point rather than
    !! thoroughness for its own sake.** The null block's position must depend on `nulls_first` and
    !! must NOT depend on `descending` -- Arrow's rule, and the one property a partition-then-count
    !! implementation gets wrong by default. An ascending nulls-last test cannot see either.
    !!
    !! Each case asserts BOTH halves: that the two engines really diverged (zero comparisons on the
    !! fast half, nonzero on the slow one) and that they agree. Without the first, a fast path that
    !! silently declined would pass the equality against itself -- feature_risks.md Risk-35/Risk-52.
    subroutine test_counting_path_nulls_agree(error)
        type(error_type), allocatable, intent(out) :: error
        integer(int32) :: v(40)
        logical :: ok(40), none_null(40), all_null(40)
        integer :: k

        do k = 1, 40
            v(k) = int(mod(k * 7, 5), int32)     ! five distinct values: the counting path's case
            ok(k) = mod(k, 7) /= 0               ! a scattered handful of nulls
        end do
        none_null = .true.
        all_null = .false.
        !
        call one_null_case(error, v, ok, .false., .false., "asc/nulls-last")
        if (allocated(error)) return
        call one_null_case(error, v, ok, .true., .false., "desc/nulls-last")
        if (allocated(error)) return
        call one_null_case(error, v, ok, .false., .true., "asc/nulls-first")
        if (allocated(error)) return
        call one_null_case(error, v, ok, .true., .true., "desc/nulls-first")
        if (allocated(error)) return
        ! The two ends. All-null has no value range at all, so the candidate has nothing to bound
        ! and must still answer file order; no-null must reach the same result as before the mask
        ! argument existed.
        call one_null_case(error, v, all_null, .false., .false., "all-null asc")
        if (allocated(error)) return
        call one_null_case(error, v, all_null, .true., .true., "all-null desc/nulls-first")
        if (allocated(error)) return
        call one_null_case(error, v, none_null, .false., .false., "no-null asc")
        if (allocated(error)) return
        call one_null_case(error, v, none_null, .true., .false., "no-null desc")
        if (allocated(error)) return
        !
        ! A NULL ROW'S KEY SLOT IS NOT A VALUE, and this is the case that proves the candidate
        ! knows it. Arrow promises nothing about the bytes behind a null, so a null row can carry
        ! anything -- here a value four orders of magnitude outside the valid rows' 0..4 range, and
        ! far past the bucket limit. Counting the null rows into the range scan would size the
        ! bucket domain from that value, blow the limit, and silently decline the fast path; the
        ! `cmp_fast == 0` assertion in one_null_case is what catches it. Every other fixture in
        ! this test holds an in-range value behind its nulls, so none of them can see this.
        do k = 1, 40
            if (mod(k, 7) == 0) v(k) = 2000000000_int32
        end do
        call one_null_case(error, v, ok, .false., .false., "null slot holds a huge value")
        if (allocated(error)) return
        call one_null_case(error, v, ok, .true., .true., "huge null slot, desc/nulls-first")
    end subroutine test_counting_path_nulls_agree
    !
    !> One (descending, nulls_first) case of the test above: both engines, both halves asserted.
    subroutine one_null_case(error, v, ok, desc, nfirst, label)
        type(error_type), allocatable, intent(out) :: error
        integer(int32), intent(in) :: v(:)      !! key values.
        logical, intent(in) :: ok(:)            !! validity mask; .false. marks a null.
        logical, intent(in) :: desc             !! sort direction.
        logical, intent(in) :: nfirst           !! null placement.
        character(len=*), intent(in) :: label   !! names the case in a failure message.
        integer(int32), allocatable :: fast(:), slow(:)
        integer(int64) :: cmp_fast, cmp_slow
        integer :: k, n

        n = size(v)
        call arm_sort_comparisons()
        call pf_argsort(v, fast, is_valid=ok, descending=desc, nulls_first=nfirst)
        cmp_fast = sort_comparisons()
        call parquet_set_sort_counting_path(.false.)
        call arm_sort_comparisons()
        call pf_argsort(v, slow, is_valid=ok, descending=desc, nulls_first=nfirst)
        cmp_slow = sort_comparisons()
        call parquet_set_sort_counting_path(.true.)
        !
        call check(error, cmp_fast == 0_int64, &
            label // ": the fast half must reach the COUNTING path (zero comparisons); a nonzero " // &
            "count means the candidate declined and the agreement below is vacuous")
        if (allocated(error)) return
        call check(error, cmp_slow > 0_int64, &
            label // ": the slow half must reach the COMPARATOR path")
        if (allocated(error)) return
        call check(error, all(fast == slow), &
            label // ": the counting path must produce exactly the comparator path's permutation")
        if (allocated(error)) return
        ! Not implied by the permutation check: it pins WHERE the nulls landed, which is the
        ! property `descending` must not disturb.
        call check(error, all([(ok(fast(k)), k = 1, n)] .eqv. [(ok(slow(k)), k = 1, n)]), &
            label // ": the null block must land in the same place under both engines")
    end subroutine one_null_case
    !
    !> **The partial-sort oracle.** Its first `n` must equal `pf_sort`'s first `n`, for every
    !> boundary value of `n` -- which is nearly free, because `pf_sort` is already trusted by every
    !> test above it.
    subroutine test_partial_matches_full(error)
        type(error_type), allocatable, intent(out) :: error
        integer(int32) :: v(10) = [30, 10, 50, 20, 60, 40, 15, 55, 25, 45]
        integer(int32), allocatable :: full(:), part(:)
        integer :: n

        call pf_sort(v, full)
        do n = 0, 10
            call pf_partial_sort(v, part, n)
            call check(error, size(part) == n, "pf_partial_sort must return exactly n elements")
            if (allocated(error)) return
            if (n > 0) then
                call check(error, all(part == full(1:n)), &
                    "pf_partial_sort's result must equal the first n of a full sort")
                if (allocated(error)) return
            end if
        end do
    end subroutine test_partial_matches_full
    !
    !> `n` past the end returns everything rather than aborting, and `n = 0` returns nothing.
    subroutine test_partial_clamps(error)
        type(error_type), allocatable, intent(out) :: error
        integer(int32) :: v(5) = [3, 1, 4, 1, 5]
        integer(int32), allocatable :: part(:)
        integer(int32), allocatable :: perm(:)

        call pf_partial_sort(v, part, 100)
        call check(error, size(part) == 5, "n past the end must clamp to the array size")
        if (allocated(error)) return
        call check(error, all(part == [1, 1, 3, 4, 5]), "a clamped partial sort must sort everything")
        if (allocated(error)) return
        call pf_partial_sort(v, part, 0)
        call check(error, size(part) == 0, "n = 0 must return an empty array")
        if (allocated(error)) return
        call pf_partial_argsort(v, perm, 100)
        call check(error, size(perm) == 5, "pf_partial_argsort must clamp the same way")
    end subroutine test_partial_clamps
    !
    !> The permutation form must agree with the full argsort's prefix, element for element.
    subroutine test_partial_argsort(error)
        type(error_type), allocatable, intent(out) :: error
        integer(int32) :: v(8) = [30, 10, 50, 20, 60, 40, 15, 55]
        integer(int32), allocatable :: full(:), part(:)
        integer(int64), allocatable :: p64(:)

        call pf_argsort(v, full)
        call pf_partial_argsort(v, part, 4)
        call check(error, all(part == full(1:4)), &
            "pf_partial_argsort must equal the first n of a full argsort")
        if (allocated(error)) return
        call pf_partial_argsort(v, p64, 4)
        call check(error, all(int(part, int64) == p64), &
            "the int32 and int64 partial permutation forms must agree")
    end subroutine test_partial_argsort
    !
    !> "The last N" is `descending=.true.`, not a separate procedure -- so it must equal the tail
    !> of an ascending full sort, reversed.
    subroutine test_partial_descending(error)
        type(error_type), allocatable, intent(out) :: error
        integer(int32) :: v(8) = [30, 10, 50, 20, 60, 40, 15, 55]
        integer(int32), allocatable :: full(:), part(:)

        call pf_sort(v, full)
        call pf_partial_sort(v, part, 3, descending=.true.)
        call check(error, all(part == full(8:6:-1)), &
            "a descending partial sort must give the largest n, in descending order")
    end subroutine test_partial_descending
    !
    !> **A partial sort that is not actually partial is invisible to every test above.** Returning
    !> the first n of a FULL sort is correct and merely slower, so only a comparison count
    !> distinguishes them -- a wall-clock benchmark would be flaky and needs warm-up.
    !>
    !> Uses a REAL key on purpose: the integer counting fast path performs zero comparisons, so a
    !> low-cardinality integer key would report 0 on both paths and the test would pass vacuously.
    subroutine test_partial_is_partial(error)
        type(error_type), allocatable, intent(out) :: error
        real(real64) :: v(4000)
        real(real64), allocatable :: out(:)
        integer(int64) :: n_partial, n_full
        integer :: k
        interface
            subroutine count_cmp(enable) bind(C, name="parquet_debug_set_count_sort_comparisons")
                use iso_c_binding, only : c_int
                integer(c_int), value :: enable !! nonzero arms and zeroes the counter.
            end subroutine count_cmp
            function got_cmp() bind(C, name="parquet_debug_get_sort_comparisons") result(n)
                use iso_c_binding, only : c_long_long
                integer(c_long_long) :: n !! comparisons since the counter was armed.
            end function got_cmp
        end interface

        do k = 1, 4000
            v(k) = real(mod(k * 7919, 4001), real64) * 0.5_real64
        end do
        call count_cmp(1)
        call pf_partial_sort(v, out, 10)
        n_partial = got_cmp()
        call count_cmp(1)
        call pf_sort(v, out)
        n_full = got_cmp()
        call count_cmp(0)
        call check(error, n_partial > 0 .and. n_full > 0, &
            "both sorts must reach the comparator path for this comparison to mean anything")
        if (allocated(error)) return
        call check(error, n_partial < n_full, &
            "a partial sort of 10 of 4000 must do fewer comparisons than a full sort")
    end subroutine test_partial_is_partial
    !
    !> `sorted_valid` must describe `sorted`, in ITS order -- so it equals the input mask gathered
    !> by the same permutation `pf_argsort` produces. Always allocated, even with no `is_valid`.
    subroutine test_sorted_valid(error)
        type(error_type), allocatable, intent(out) :: error
        integer(int32) :: v(5) = [30, 10, 50, 20, 40]
        logical :: mask(5) = [.true., .false., .true., .false., .true.]
        integer(int32), allocatable :: sorted(:), perm(:)
        logical, allocatable :: sv(:)
        integer :: k

        call pf_argsort(v, perm, is_valid=mask)
        call pf_sort(v, sorted, is_valid=mask, sorted_valid=sv)
        call check(error, allocated(sv), "sorted_valid must be allocated when asked for")
        if (allocated(error)) return
        call check(error, all(sv .eqv. [(mask(perm(k)), k = 1, 5)]), &
            "sorted_valid must be the input mask gathered by the sort's own permutation")
        if (allocated(error)) return
        call check(error, all(sv(4:5) .eqv. [.false., .false.]), &
            "the two null rows must be marked invalid, at the end")
        if (allocated(error)) return
        ! No is_valid at all: the answer is a direct question, so it is all .true., not unallocated.
        call pf_sort(v, sorted, sorted_valid=sv)
        call check(error, allocated(sv) .and. all(sv), &
            "with no is_valid, sorted_valid must be allocated and all .true.")
    end subroutine test_sorted_valid
    !
    !> `pf_nth_element` must agree with `pf_sort` at every rank, value and index alike.
    subroutine test_nth_matches_full(error)
        type(error_type), allocatable, intent(out) :: error
        integer(int32) :: v(9) = [30, 10, 50, 20, 60, 40, 15, 55, 25]
        integer(int32), allocatable :: full(:)
        integer(int32) :: val
        integer(int64) :: idx
        integer :: k

        call pf_sort(v, full)
        do k = 1, 9
            call pf_nth_element(v, k, val, idx)
            call check(error, val == full(k), "pf_nth_element must return the value a full sort puts at that rank")
            if (allocated(error)) return
            call check(error, v(idx) == full(k), "pf_nth_element's index must point at that same value")
            if (allocated(error)) return
        end do
    end subroutine test_nth_matches_full
    !
    !> **The test that fails if the comparator's row-index tiebreaker is ever dropped.**
    !> `std::nth_element` normally leaves an arbitrary member of an equal-comparing run at the
    !> requested position; only a total order makes the reported index deterministic. Without
    !> duplicates this proves nothing at all.
    subroutine test_nth_stable_index(error)
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: n = 300, ngroup = 10
        integer(int32) :: v(n)
        integer(int32), allocatable :: perm(:)
        integer(int64) :: expect(n)
        integer(int32) :: val
        integer(int64) :: idx
        integer :: k, g, m, pass

        ! An INDEPENDENT oracle, not a second call into the engine. v(k) = mod(k-1, ngroup), so the
        ! stable order is: every index with value 0 in increasing order, then every index with
        ! value 1, and so on -- which the nested loop below constructs without sorting anything.
        !
        ! Asserting against pf_argsort instead would be circular: both it and pf_nth_element go
        ! through the same comparator, so removing its tiebreaker breaks the two identically and
        ! the comparison still holds. That mutation survived an earlier version of this test.
        !
        ! The array is deliberately LARGE. libstdc++ falls back to insertion sort below ~16
        ! elements, which is stable even with no tiebreaker at all, so a six-element fixture cannot
        ! distinguish a total order from an accidentally-stable one either.
        do k = 1, n
            v(k) = int(mod(k - 1, ngroup), int32)
        end do
        m = 0
        do g = 0, ngroup - 1
            do k = 1, n
                if (v(k) == g) then
                    m = m + 1
                    expect(m) = int(k, int64)
                end if
            end do
        end do

        ! BOTH code paths, because they are stable for entirely different reasons and only one of
        ! them can lose it: the counting path places in order by construction and never calls the
        ! comparator at all, so a key it accepts -- like this low-cardinality integer one -- cannot
        ! test the comparator's tiebreaker. An earlier version of this test ran only pass 1 and a
        ! mutation removing that tiebreaker survived it.
        do pass = 1, 2
            call parquet_set_sort_counting_path(pass == 1)
            call pf_argsort(v, perm)
            call check(error, all(int(perm, int64) == expect), &
                "pf_argsort must be stable: equal values must keep their original index order")
            if (allocated(error)) exit
            do k = 1, n
                call pf_nth_element(v, k, val, idx)
                call check(error, idx == expect(k), &
                    "pf_nth_element's index must be the one a full STABLE sort gives, duplicates included")
                if (allocated(error)) exit
                call check(error, val == v(expect(k)), "pf_nth_element's value must match its own index")
                if (allocated(error)) exit
            end do
            if (allocated(error)) exit
        end do
        call parquet_set_sort_counting_path(.true.)
    end subroutine test_nth_stable_index
    !
    !> `descending` and `nulls_first` mean here exactly what they mean for the sort, and `nth`
    !> counts nulls as ranked elements rather than skipping them.
    subroutine test_nth_options(error)
        type(error_type), allocatable, intent(out) :: error
        integer(int32) :: v(5) = [30, 10, 50, 20, 40]
        logical :: mask(5) = [.true., .false., .true., .true., .true.]
        integer(int32) :: val
        integer(int64) :: idx

        call pf_nth_element(v, 1, val, descending=.true.)
        call check(error, val == 50, "rank 1 descending must be the largest value")
        if (allocated(error)) return
        ! v(2) is null; with nulls last it occupies rank 5.
        call pf_nth_element(v, 5, val, idx, is_valid=mask)
        call check(error, idx == 2_int64, "with nulls last, the final rank must be the null row")
        if (allocated(error)) return
        call pf_nth_element(v, 1, val, idx, is_valid=mask, nulls_first=.true.)
        call check(error, idx == 2_int64, "with nulls_first, rank 1 must be the null row")
    end subroutine test_nth_options
    !
    !> quantile 0 and 1 must land exactly on the smallest and largest value, with no rounding
    !> involved at either end, and 0.5 on the true median for an odd count.
    subroutine test_quantile_endpoints(error)
        type(error_type), allocatable, intent(out) :: error
        real(real64) :: v(5) = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64, 10.0_real64]
        real(real64) :: q

        call pf_nth_quantile(v, 0.0_real64, q)
        call check(error, q == 1.0_real64, "quantile 0 must be the smallest value")
        if (allocated(error)) return
        call pf_nth_quantile(v, 1.0_real64, q)
        call check(error, q == 10.0_real64, "quantile 1 must be the largest value")
        if (allocated(error)) return
        call pf_nth_quantile(v, 0.5_real64, q)
        call check(error, q == 3.0_real64, "quantile 0.5 of five values must be the middle one")
    end subroutine test_quantile_endpoints
    !
    !> Nulls are EXCLUDED from the population rather than placed in it -- the one operation in this
    !> module where that is true -- and `n_null` reports how many were dropped.
    subroutine test_quantile_nulls(error)
        type(error_type), allocatable, intent(out) :: error
        real(real64) :: v(5) = [1.0_real64, 2.0_real64, 999.0_real64, 4.0_real64, 10.0_real64]
        logical :: mask(5) = [.true., .true., .false., .true., .true.]
        real(real64) :: q
        integer(int64) :: nn

        call pf_nth_quantile(v, 0.0_real64, q, is_valid=mask, n_null=nn)
        call check(error, nn == 1_int64, "n_null must report the one excluded value")
        if (allocated(error)) return
        call check(error, q == 1.0_real64, "the excluded value must not become the population")
        if (allocated(error)) return
        call pf_nth_quantile(v, 1.0_real64, q, is_valid=mask)
        call check(error, q == 10.0_real64, &
            "the largest NON-NULL value must be quantile 1, not the null row's 999")
    end subroutine test_quantile_nulls
    !
    !> On a fractional position the three tokens must give three different answers, or they are
    !> untested. Four values, quantile 0.5 -> position 1.5 on the 0-based scale.
    subroutine test_quantile_rounding(error)
        type(error_type), allocatable, intent(out) :: error
        real(real64) :: v(4) = [1.0_real64, 2.0_real64, 4.0_real64, 10.0_real64]
        real(real64) :: down, up, near

        call pf_nth_quantile(v, 0.5_real64, down, rounding="down")
        call pf_nth_quantile(v, 0.5_real64, up, rounding="up")
        call pf_nth_quantile(v, 0.5_real64, near)
        call check(error, down == 2.0_real64, "rounding down must take the lower of the two middle values")
        if (allocated(error)) return
        call check(error, up == 4.0_real64, "rounding up must take the higher")
        if (allocated(error)) return
        call check(error, near == up, "the default rounding must be nearest, which rounds 1.5 up")
    end subroutine test_quantile_rounding
    !
    !> Tokens match case-insensitively, like parquet_sortkey's own direction words.
    subroutine test_quantile_rounding_case(error)
        type(error_type), allocatable, intent(out) :: error
        real(real64) :: v(4) = [1.0_real64, 2.0_real64, 4.0_real64, 10.0_real64]
        real(real64) :: lower, upper

        call pf_nth_quantile(v, 0.5_real64, lower, rounding="down")
        call pf_nth_quantile(v, 0.5_real64, upper, rounding="DoWn")
        call check(error, lower == upper, "a rounding token must match regardless of case")
        if (allocated(error)) return
        ! "nearest" NAMED is a different path from "nearest" by omission: the token resolver
        ! returns early when the argument is absent, so its own arm for this token is reached
        ! only when a caller spells it out.
        call pf_nth_quantile(v, 0.5_real64, lower)
        call pf_nth_quantile(v, 0.5_real64, upper, rounding="nearest")
        call check(error, lower == upper, &
            "naming the default rounding token must give what omitting it gives")
    end subroutine test_quantile_rounding_case
    !
    ! ----------------------------------------------------------------------------------
    ! M2 sweeps: one call per generated specific
    ! ----------------------------------------------------------------------------------
    !
    !> **Every `pf_partial_sort` specific, once each.** The tests above prove the OPERATION is
    !> right; they prove it for `int32` and `real64` only, so each of the other seven specifics --
    !> separately generated bodies, each naming its own extractor -- was reached by nothing at all.
    !> A body wired to the wrong extractor, or one that gathered by the wrong array, would pass the
    !> whole suite.
    !>
    !> The six types taking a mask are called twice: once plain, and once with `is_valid` AND
    !> `sorted_valid` together, which is the only call shape that reaches a specific's
    !> validity-gathering branch at all.
    subroutine test_partial_sort_every_specific(error)
        type(error_type), allocatable, intent(out) :: error
        !> The mask's expected image: five valid rows, then the one null, which sorts last.
        logical, parameter :: SVEXP(SWN) = [.true., .true., .true., .true., .true., .false.]
        logical :: mask(SWN)
        integer :: k

        mask = .true.
        mask(SWASC(1)) = .false.        ! the SMALLEST value is the null, so it has to travel
        block
            integer(int32) :: v(SWN)
            integer(int32), allocatable :: s(:)
            logical, allocatable :: sv(:)
            v = 10_int32 * int(SWRANK, int32)
            call pf_partial_sort(v, s, 3)
            call check(error, size(s) == 3 .and. all(s == [10_int32, 20_int32, 30_int32]), &
                "int32 pf_partial_sort must return the three smallest values, in order")
            if (allocated(error)) return
            call pf_partial_sort(v, s, SWN, is_valid=mask, sorted_valid=sv)
            call check(error, all(sv .eqv. SVEXP) .and. s(1) == 20_int32 .and. s(SWN) == 10_int32, &
                "int32 pf_partial_sort must carry validity into sorted_valid, the null last")
        end block
        if (allocated(error)) return
        block
            integer(int64) :: v(SWN)
            integer(int64), allocatable :: s(:)
            logical, allocatable :: sv(:)
            v = 100_int64 * int(SWRANK, int64)
            call pf_partial_sort(v, s, 3)
            call check(error, size(s) == 3 .and. all(s == [100_int64, 200_int64, 300_int64]), &
                "int64 pf_partial_sort must return the three smallest values, in order")
            if (allocated(error)) return
            call pf_partial_sort(v, s, SWN, is_valid=mask, sorted_valid=sv)
            call check(error, all(sv .eqv. SVEXP) .and. s(1) == 200_int64 .and. s(SWN) == 100_int64, &
                "int64 pf_partial_sort must carry validity into sorted_valid, the null last")
        end block
        if (allocated(error)) return
        block
            real(real32) :: v(SWN)
            real(real32), allocatable :: s(:)
            logical, allocatable :: sv(:)
            v = real(SWRANK, real32) + 0.5_real32
            call pf_partial_sort(v, s, 3)
            call check(error, size(s) == 3 .and. all(s == [1.5_real32, 2.5_real32, 3.5_real32]), &
                "real32 pf_partial_sort must return the three smallest values, in order")
            if (allocated(error)) return
            call pf_partial_sort(v, s, SWN, is_valid=mask, sorted_valid=sv)
            call check(error, all(sv .eqv. SVEXP) .and. s(1) == 2.5_real32 .and. s(SWN) == 1.5_real32, &
                "real32 pf_partial_sort must carry validity into sorted_valid, the null last")
        end block
        if (allocated(error)) return
        block
            real(real64) :: v(SWN)
            real(real64), allocatable :: s(:)
            logical, allocatable :: sv(:)
            v = real(SWRANK, real64) + 0.25_real64
            call pf_partial_sort(v, s, 3)
            call check(error, size(s) == 3 .and. all(s == [1.25_real64, 2.25_real64, 3.25_real64]), &
                "real64 pf_partial_sort must return the three smallest values, in order")
            if (allocated(error)) return
            call pf_partial_sort(v, s, SWN, is_valid=mask, sorted_valid=sv)
            call check(error, all(sv .eqv. SVEXP) .and. s(1) == 2.25_real64 .and. s(SWN) == 1.25_real64, &
                "real64 pf_partial_sort must carry validity into sorted_valid, the null last")
        end block
        if (allocated(error)) return
        block
            logical, allocatable :: s(:), sv(:)
            logical :: bmask(SWN)
            call pf_partial_sort(SWBOOL, s, 3)
            call check(error, size(s) == 3 .and. .not. any(s), &
                "logical pf_partial_sort must return the three .false. elements first")
            if (allocated(error)) return
            bmask = .true.
            bmask(2) = .false.          ! the first .false., i.e. the smallest, is the null
            call pf_partial_sort(SWBOOL, s, SWN, is_valid=bmask, sorted_valid=sv)
            call check(error, all(sv .eqv. SVEXP) .and. .not. s(1) .and. s(3), &
                "logical pf_partial_sort must carry validity into sorted_valid, the null last")
        end block
        if (allocated(error)) return
        block
            character(len=2) :: v(SWN)
            character(len=2), allocatable :: s(:)
            logical, allocatable :: sv(:)
            do k = 1, SWN
                v(k) = swchr(SWRANK(k))
            end do
            call pf_partial_sort(v, s, 3)
            call check(error, size(s) == 3 .and. all(s == [swchr(1), swchr(2), swchr(3)]), &
                "character pf_partial_sort must return the three smallest values, in order")
            if (allocated(error)) return
            call pf_partial_sort(v, s, SWN, is_valid=mask, sorted_valid=sv)
            call check(error, all(sv .eqv. SVEXP) .and. s(1) == swchr(2) .and. s(SWN) == swchr(1), &
                "character pf_partial_sort must carry validity into sorted_valid, the null last")
        end block
        if (allocated(error)) return
        ! ---- the three temporal types, which carry their own nulls and so take no mask ----
        block
            type(parquet_date) :: v(SWN)
            type(parquet_date), allocatable :: s(:)
            call v%set_raw(1000_int32 + int(SWRANK, int32))
            call pf_partial_sort(v, s, 3)
            call check(error, size(s) == 3 .and. all([(s(k)%raw(), k = 1, 3)] == [1001, 1002, 1003]), &
                "parquet_date pf_partial_sort must return the three earliest dates, in order")
        end block
        if (allocated(error)) return
        block
            type(parquet_time) :: v(SWN)
            type(parquet_time), allocatable :: s(:)
            call v%set_raw(2000_int64 + int(SWRANK, int64))
            call pf_partial_sort(v, s, 3)
            call check(error, size(s) == 3 .and. &
                all([(s(k)%raw(), k = 1, 3)] == [2001_int64, 2002_int64, 2003_int64]), &
                "parquet_time pf_partial_sort must return the three earliest times, in order")
        end block
        if (allocated(error)) return
        block
            type(parquet_timestamp) :: v(SWN)
            type(parquet_timestamp), allocatable :: s(:)
            integer(int64) :: gs(3)
            integer(int32) :: gn(3)
            call v%set_raw(3000_int64 + int(SWRANK, int64), 7_int32)
            call pf_partial_sort(v, s, 3)
            do k = 1, 3
                call s(k)%get_raw(gs(k), gn(k))
            end do
            call check(error, size(s) == 3 .and. all(gs == [3001_int64, 3002_int64, 3003_int64]) .and. &
                all(gn == 7_int32), &
                "parquet_timestamp pf_partial_sort must return the three earliest timestamps, in order")
        end block
    end subroutine test_partial_sort_every_specific
    !
    !> **Every `pf_partial_argsort` specific, once each** -- twelve value families times the two
    !> index kinds. The int32 and int64 forms are separate bodies (one narrows the engine's int64
    !> permutation, the other moves it), so an int64 form that silently truncated, or an int32 form
    !> that narrowed the wrong array, is invisible until each is called.
    !>
    !> The expected answer is `SWASC(1:3)` read straight off the fixture -- the elements holding
    !> ranks 1, 2 and 3 -- so nothing here is compared against a second call into the engine.
    subroutine test_partial_argsort_every_specific(error)
        type(error_type), allocatable, intent(out) :: error
        !> The three elements a partial argsort of the fixture must name, in order.
        integer, parameter :: TOP3(3) = [SWASC(1), SWASC(2), SWASC(3)]
        integer(int32), allocatable :: p32(:)
        integer(int64), allocatable :: p64(:)
        integer :: ik, k
        character(len=16) :: ctx

        do ik = 1, 2
            write(ctx, "(A,I0,A)") "[i", 32*ik, "] "
            block
                integer(int32) :: v(SWN)
                v = 10_int32 * int(SWRANK, int32)
                if (ik == 1) then
                    call pf_partial_argsort(v, p32, 3)
                else
                    call pf_partial_argsort(v, p64, 3)
                end if
                call check(error, sweep_perm_ok(p32, p64, ik, TOP3), trim(ctx)//"int32 pf_partial_argsort")
            end block
            if (allocated(error)) return
            block
                integer(int64) :: v(SWN)
                v = 100_int64 * int(SWRANK, int64)
                if (ik == 1) then
                    call pf_partial_argsort(v, p32, 3)
                else
                    call pf_partial_argsort(v, p64, 3)
                end if
                call check(error, sweep_perm_ok(p32, p64, ik, TOP3), trim(ctx)//"int64 pf_partial_argsort")
            end block
            if (allocated(error)) return
            block
                real(real32) :: v(SWN)
                v = real(SWRANK, real32) + 0.5_real32
                if (ik == 1) then
                    call pf_partial_argsort(v, p32, 3)
                else
                    call pf_partial_argsort(v, p64, 3)
                end if
                call check(error, sweep_perm_ok(p32, p64, ik, TOP3), trim(ctx)//"real32 pf_partial_argsort")
            end block
            if (allocated(error)) return
            block
                real(real64) :: v(SWN)
                v = real(SWRANK, real64) + 0.25_real64
                if (ik == 1) then
                    call pf_partial_argsort(v, p32, 3)
                else
                    call pf_partial_argsort(v, p64, 3)
                end if
                call check(error, sweep_perm_ok(p32, p64, ik, TOP3), trim(ctx)//"real64 pf_partial_argsort")
            end block
            if (allocated(error)) return
            block
                ! logical has only two values, so its expectation is the three .false. elements in
                ! their original order -- which is a stability assertion as much as an order one.
                if (ik == 1) then
                    call pf_partial_argsort(SWBOOL, p32, 3)
                else
                    call pf_partial_argsort(SWBOOL, p64, 3)
                end if
                call check(error, sweep_perm_ok(p32, p64, ik, [2, 5, 6]), &
                    trim(ctx)//"logical pf_partial_argsort must name the .false. elements in order")
            end block
            if (allocated(error)) return
            block
                character(len=2) :: v(SWN)
                do k = 1, SWN
                    v(k) = swchr(SWRANK(k))
                end do
                if (ik == 1) then
                    call pf_partial_argsort(v, p32, 3)
                else
                    call pf_partial_argsort(v, p64, 3)
                end if
                call check(error, sweep_perm_ok(p32, p64, ik, TOP3), trim(ctx)//"character pf_partial_argsort")
            end block
            if (allocated(error)) return
            block
                type(parquet_date) :: v(SWN)
                call v%set_raw(1000_int32 + int(SWRANK, int32))
                if (ik == 1) then
                    call pf_partial_argsort(v, p32, 3)
                else
                    call pf_partial_argsort(v, p64, 3)
                end if
                call check(error, sweep_perm_ok(p32, p64, ik, TOP3), trim(ctx)//"parquet_date pf_partial_argsort")
            end block
            if (allocated(error)) return
            block
                type(parquet_time) :: v(SWN)
                call v%set_raw(2000_int64 + int(SWRANK, int64))
                if (ik == 1) then
                    call pf_partial_argsort(v, p32, 3)
                else
                    call pf_partial_argsort(v, p64, 3)
                end if
                call check(error, sweep_perm_ok(p32, p64, ik, TOP3), trim(ctx)//"parquet_time pf_partial_argsort")
            end block
            if (allocated(error)) return
            block
                type(parquet_timestamp) :: v(SWN)
                call v%set_raw(3000_int64 + int(SWRANK, int64), 7_int32)
                if (ik == 1) then
                    call pf_partial_argsort(v, p32, 3)
                else
                    call pf_partial_argsort(v, p64, 3)
                end if
                call check(error, sweep_perm_ok(p32, p64, ik, TOP3), &
                    trim(ctx)//"parquet_timestamp pf_partial_argsort")
            end block
            if (allocated(error)) return
            block
                type(parquet_string_column) :: sc
                do k = 1, SWN
                    call sc%append_string(swchr(SWRANK(k)))
                end do
                if (ik == 1) then
                    call pf_partial_argsort(sc, p32, 3)
                else
                    call pf_partial_argsort(sc, p64, 3)
                end if
                call check(error, sweep_perm_ok(p32, p64, ik, TOP3), &
                    trim(ctx)//"parquet_string_column pf_partial_argsort")
            end block
            if (allocated(error)) return
            block
                type(parquet_column) :: c
                call c%init(PK_INT32, int(SWN, int64))
                call c%set_all(10_int32 * int(SWRANK, int32))
                if (ik == 1) then
                    call pf_partial_argsort(c, p32, 3)
                else
                    call pf_partial_argsort(c, p64, 3)
                end if
                call check(error, sweep_perm_ok(p32, p64, ik, TOP3), trim(ctx)//"parquet_column pf_partial_argsort")
            end block
            if (allocated(error)) return
            block
                type(pf_sort_keys) :: keys
                integer(int32) :: v(SWN)
                v = 10_int32 * int(SWRANK, int32)
                call keys%add(v)
                if (ik == 1) then
                    call pf_partial_argsort(keys, p32, 3)
                else
                    call pf_partial_argsort(keys, p64, 3)
                end if
                call check(error, sweep_perm_ok(p32, p64, ik, TOP3), trim(ctx)//"pf_sort_keys pf_partial_argsort")
            end block
            if (allocated(error)) return
        end do
    end subroutine test_partial_argsort_every_specific
    !
    !> **Every `pf_nth_element` specific, once each** -- ten value families times two rank kinds
    !> times {no index, int32 index, int64 index}, which is sixty separate generated bodies. Rank 2
    !> is asked for throughout, because it is in the interior of the fixture: a body that reported
    !> the minimum, the maximum, or the element already sitting at position 2 would each give a
    !> different wrong answer, and all three are excluded by the same assertion.
    subroutine test_nth_every_specific(error)
        type(error_type), allocatable, intent(out) :: error
        !> The element holding rank 2, which every call below must name.
        integer, parameter :: WANT = SWASC(2)
        integer(int32) :: i32
        integer(int64) :: i64
        integer :: rk, k
        character(len=16) :: ctx

        do rk = 1, 2                    ! the rank argument's own kind: int32, then int64
            write(ctx, "(A,I0,A)") "[rank i", 32*rk, "] "
            block
                integer(int32) :: v(SWN), pv
                logical :: ok
                v = 10_int32 * int(SWRANK, int32)
                ok = .true.
                if (rk == 1) then
                    call pf_nth_element(v, 2_int32, pv)
                    if (pv /= 20_int32) ok = .false.
                    call pf_nth_element(v, 2_int32, pv, i32)
                    call pf_nth_element(v, 2_int32, pv, i64)
                else
                    call pf_nth_element(v, 2_int64, pv)
                    if (pv /= 20_int32) ok = .false.
                    call pf_nth_element(v, 2_int64, pv, i32)
                    call pf_nth_element(v, 2_int64, pv, i64)
                end if
                call check(error, ok .and. pv == 20_int32 .and. i32 == WANT .and. i64 == int(WANT, int64), &
                    trim(ctx)//"int32 pf_nth_element must report rank 2's value and element")
            end block
            if (allocated(error)) return
            block
                integer(int64) :: v(SWN), pv
                logical :: ok
                v = 100_int64 * int(SWRANK, int64)
                ok = .true.
                if (rk == 1) then
                    call pf_nth_element(v, 2_int32, pv)
                    if (pv /= 200_int64) ok = .false.
                    call pf_nth_element(v, 2_int32, pv, i32)
                    call pf_nth_element(v, 2_int32, pv, i64)
                else
                    call pf_nth_element(v, 2_int64, pv)
                    if (pv /= 200_int64) ok = .false.
                    call pf_nth_element(v, 2_int64, pv, i32)
                    call pf_nth_element(v, 2_int64, pv, i64)
                end if
                call check(error, ok .and. pv == 200_int64 .and. i32 == WANT .and. i64 == int(WANT, int64), &
                    trim(ctx)//"int64 pf_nth_element must report rank 2's value and element")
            end block
            if (allocated(error)) return
            block
                real(real32) :: v(SWN), pv
                logical :: ok
                v = real(SWRANK, real32) + 0.5_real32
                ok = .true.
                if (rk == 1) then
                    call pf_nth_element(v, 2_int32, pv)
                    if (pv /= 2.5_real32) ok = .false.
                    call pf_nth_element(v, 2_int32, pv, i32)
                    call pf_nth_element(v, 2_int32, pv, i64)
                else
                    call pf_nth_element(v, 2_int64, pv)
                    if (pv /= 2.5_real32) ok = .false.
                    call pf_nth_element(v, 2_int64, pv, i32)
                    call pf_nth_element(v, 2_int64, pv, i64)
                end if
                call check(error, ok .and. pv == 2.5_real32 .and. i32 == WANT .and. i64 == int(WANT, int64), &
                    trim(ctx)//"real32 pf_nth_element must report rank 2's value and element")
            end block
            if (allocated(error)) return
            block
                real(real64) :: v(SWN), pv
                logical :: ok
                v = real(SWRANK, real64) + 0.25_real64
                ok = .true.
                if (rk == 1) then
                    call pf_nth_element(v, 2_int32, pv)
                    if (pv /= 2.25_real64) ok = .false.
                    call pf_nth_element(v, 2_int32, pv, i32)
                    call pf_nth_element(v, 2_int32, pv, i64)
                else
                    call pf_nth_element(v, 2_int64, pv)
                    if (pv /= 2.25_real64) ok = .false.
                    call pf_nth_element(v, 2_int64, pv, i32)
                    call pf_nth_element(v, 2_int64, pv, i64)
                end if
                call check(error, ok .and. pv == 2.25_real64 .and. i32 == WANT .and. i64 == int(WANT, int64), &
                    trim(ctx)//"real64 pf_nth_element must report rank 2's value and element")
            end block
            if (allocated(error)) return
            block
                ! Rank 2 of the logical fixture is the SECOND .false., i.e. element 5 -- an answer
                ! only a stable ordering gives, so this is the type's stability check too.
                logical :: pv, ok
                ok = .true.
                if (rk == 1) then
                    call pf_nth_element(SWBOOL, 2_int32, pv)
                    if (pv) ok = .false.
                    call pf_nth_element(SWBOOL, 2_int32, pv, i32)
                    call pf_nth_element(SWBOOL, 2_int32, pv, i64)
                else
                    call pf_nth_element(SWBOOL, 2_int64, pv)
                    if (pv) ok = .false.
                    call pf_nth_element(SWBOOL, 2_int64, pv, i32)
                    call pf_nth_element(SWBOOL, 2_int64, pv, i64)
                end if
                call check(error, ok .and. .not. pv .and. i32 == 5 .and. i64 == 5_int64, &
                    trim(ctx)//"logical pf_nth_element must report the second .false. element")
            end block
            if (allocated(error)) return
            block
                character(len=2) :: v(SWN)
                character(len=:), allocatable :: pv
                logical :: ok
                do k = 1, SWN
                    v(k) = swchr(SWRANK(k))
                end do
                ok = .true.
                if (rk == 1) then
                    call pf_nth_element(v, 2_int32, pv)
                    if (pv /= swchr(2)) ok = .false.
                    call pf_nth_element(v, 2_int32, pv, i32)
                    call pf_nth_element(v, 2_int32, pv, i64)
                else
                    call pf_nth_element(v, 2_int64, pv)
                    if (pv /= swchr(2)) ok = .false.
                    call pf_nth_element(v, 2_int64, pv, i32)
                    call pf_nth_element(v, 2_int64, pv, i64)
                end if
                call check(error, ok .and. pv == swchr(2) .and. i32 == WANT .and. i64 == int(WANT, int64), &
                    trim(ctx)//"character pf_nth_element must report rank 2's value and element")
            end block
            if (allocated(error)) return
            block
                type(parquet_date) :: v(SWN), pv
                logical :: ok
                call v%set_raw(1000_int32 + int(SWRANK, int32))
                ok = .true.
                if (rk == 1) then
                    call pf_nth_element(v, 2_int32, pv)
                    if (pv%raw() /= 1002_int32) ok = .false.
                    call pf_nth_element(v, 2_int32, pv, i32)
                    call pf_nth_element(v, 2_int32, pv, i64)
                else
                    call pf_nth_element(v, 2_int64, pv)
                    if (pv%raw() /= 1002_int32) ok = .false.
                    call pf_nth_element(v, 2_int64, pv, i32)
                    call pf_nth_element(v, 2_int64, pv, i64)
                end if
                call check(error, ok .and. pv%raw() == 1002_int32 .and. i32 == WANT .and. &
                    i64 == int(WANT, int64), &
                    trim(ctx)//"parquet_date pf_nth_element must report rank 2's value and element")
            end block
            if (allocated(error)) return
            block
                type(parquet_time) :: v(SWN), pv
                logical :: ok
                call v%set_raw(2000_int64 + int(SWRANK, int64))
                ok = .true.
                if (rk == 1) then
                    call pf_nth_element(v, 2_int32, pv)
                    if (pv%raw() /= 2002_int64) ok = .false.
                    call pf_nth_element(v, 2_int32, pv, i32)
                    call pf_nth_element(v, 2_int32, pv, i64)
                else
                    call pf_nth_element(v, 2_int64, pv)
                    if (pv%raw() /= 2002_int64) ok = .false.
                    call pf_nth_element(v, 2_int64, pv, i32)
                    call pf_nth_element(v, 2_int64, pv, i64)
                end if
                call check(error, ok .and. pv%raw() == 2002_int64 .and. i32 == WANT .and. &
                    i64 == int(WANT, int64), &
                    trim(ctx)//"parquet_time pf_nth_element must report rank 2's value and element")
            end block
            if (allocated(error)) return
            block
                type(parquet_timestamp) :: v(SWN), pv
                integer(int64) :: gs
                integer(int32) :: gn
                logical :: ok
                call v%set_raw(3000_int64 + int(SWRANK, int64), 7_int32)
                ok = .true.
                if (rk == 1) then
                    call pf_nth_element(v, 2_int32, pv)
                    call pv%get_raw(gs, gn)
                    if (gs /= 3002_int64 .or. gn /= 7_int32) ok = .false.
                    call pf_nth_element(v, 2_int32, pv, i32)
                    call pf_nth_element(v, 2_int32, pv, i64)
                else
                    call pf_nth_element(v, 2_int64, pv)
                    call pv%get_raw(gs, gn)
                    if (gs /= 3002_int64 .or. gn /= 7_int32) ok = .false.
                    call pf_nth_element(v, 2_int64, pv, i32)
                    call pf_nth_element(v, 2_int64, pv, i64)
                end if
                call check(error, ok .and. i32 == WANT .and. i64 == int(WANT, int64), &
                    trim(ctx)//"parquet_timestamp pf_nth_element must report rank 2's value and element")
            end block
            if (allocated(error)) return
            block
                type(parquet_string_column) :: sc
                character(len=:), allocatable :: pv
                logical :: ok
                do k = 1, SWN
                    call sc%append_string(swchr(SWRANK(k)))
                end do
                ok = .true.
                if (rk == 1) then
                    call pf_nth_element(sc, 2_int32, pv)
                    if (pv /= swchr(2)) ok = .false.
                    call pf_nth_element(sc, 2_int32, pv, i32)
                    call pf_nth_element(sc, 2_int32, pv, i64)
                else
                    call pf_nth_element(sc, 2_int64, pv)
                    if (pv /= swchr(2)) ok = .false.
                    call pf_nth_element(sc, 2_int64, pv, i32)
                    call pf_nth_element(sc, 2_int64, pv, i64)
                end if
                call check(error, ok .and. pv == swchr(2) .and. i32 == WANT .and. i64 == int(WANT, int64), &
                    trim(ctx)//"parquet_string_column pf_nth_element must report rank 2's value and element")
            end block
            if (allocated(error)) return
        end do
    end subroutine test_nth_every_specific
    !
    !> **Every `pf_nth_quantile` specific, once each** -- ten value families times {no index, int32
    !> index, int64 index}. Quantile 1.0 is asked for because its answer is exact under every
    !> rounding token, so the sweep tests the per-type wiring and nothing else; the rounding rules
    !> themselves are `test_quantile_rounding`'s job.
    !>
    !> `n_null` is requested on the value-only call of each family, since it is an optional
    !> out-argument whose assignment is a separate line in every one of these bodies.
    subroutine test_quantile_every_specific(error)
        type(error_type), allocatable, intent(out) :: error
        !> The element holding rank 6, which quantile 1.0 must name for every non-logical family.
        integer, parameter :: WANT = SWASC(SWN)
        integer(int32) :: i32
        integer(int64) :: i64, nn
        integer :: k

        block
            integer(int32) :: v(SWN), pv
            v = 10_int32 * int(SWRANK, int32)
            call pf_nth_quantile(v, 1.0_real64, pv, n_null=nn)
            call pf_nth_quantile(v, 1.0_real64, pv, i32)
            call pf_nth_quantile(v, 1.0_real64, pv, i64)
            call check(error, pv == 60_int32 .and. i32 == WANT .and. i64 == int(WANT, int64) .and. &
                nn == 0_int64, "int32 pf_nth_quantile at 1.0 must be the maximum, with no nulls")
        end block
        if (allocated(error)) return
        block
            integer(int64) :: v(SWN), pv
            v = 100_int64 * int(SWRANK, int64)
            call pf_nth_quantile(v, 1.0_real64, pv, n_null=nn)
            call pf_nth_quantile(v, 1.0_real64, pv, i32)
            call pf_nth_quantile(v, 1.0_real64, pv, i64)
            call check(error, pv == 600_int64 .and. i32 == WANT .and. i64 == int(WANT, int64) .and. &
                nn == 0_int64, "int64 pf_nth_quantile at 1.0 must be the maximum, with no nulls")
        end block
        if (allocated(error)) return
        block
            real(real32) :: v(SWN), pv
            v = real(SWRANK, real32) + 0.5_real32
            call pf_nth_quantile(v, 1.0_real64, pv, n_null=nn)
            call pf_nth_quantile(v, 1.0_real64, pv, i32)
            call pf_nth_quantile(v, 1.0_real64, pv, i64)
            call check(error, pv == 6.5_real32 .and. i32 == WANT .and. i64 == int(WANT, int64) .and. &
                nn == 0_int64, "real32 pf_nth_quantile at 1.0 must be the maximum, with no nulls")
        end block
        if (allocated(error)) return
        block
            real(real64) :: v(SWN), pv
            logical :: mask(SWN)
            v = real(SWRANK, real64) + 0.25_real64
            mask = .true.
            mask(SWASC(SWN)) = .false.      ! the largest value is a null, so the answer steps down
            call pf_nth_quantile(v, 1.0_real64, pv, is_valid=mask, n_null=nn)
            call check(error, pv == 5.25_real64 .and. nn == 1_int64, &
                "real64 pf_nth_quantile must exclude a null from the population and count it")
            if (allocated(error)) return
            call pf_nth_quantile(v, 1.0_real64, pv, i32)
            call pf_nth_quantile(v, 1.0_real64, pv, i64)
            call check(error, pv == 6.25_real64 .and. i32 == WANT .and. i64 == int(WANT, int64), &
                "real64 pf_nth_quantile at 1.0 must be the maximum")
        end block
        if (allocated(error)) return
        block
            logical :: pv
            call pf_nth_quantile(SWBOOL, 1.0_real64, pv, n_null=nn)
            call pf_nth_quantile(SWBOOL, 1.0_real64, pv, i32)
            call pf_nth_quantile(SWBOOL, 1.0_real64, pv, i64)
            call check(error, pv .and. i32 == 4 .and. i64 == 4_int64 .and. nn == 0_int64, &
                "logical pf_nth_quantile at 1.0 must be the LAST .true. element, which is 4")
        end block
        if (allocated(error)) return
        block
            character(len=2) :: v(SWN)
            character(len=:), allocatable :: pv
            do k = 1, SWN
                v(k) = swchr(SWRANK(k))
            end do
            call pf_nth_quantile(v, 1.0_real64, pv, n_null=nn)
            call pf_nth_quantile(v, 1.0_real64, pv, i32)
            call pf_nth_quantile(v, 1.0_real64, pv, i64)
            call check(error, pv == swchr(6) .and. i32 == WANT .and. i64 == int(WANT, int64) .and. &
                nn == 0_int64, "character pf_nth_quantile at 1.0 must be the maximum, with no nulls")
        end block
        if (allocated(error)) return
        block
            type(parquet_date) :: v(SWN), pv
            call v%set_raw(1000_int32 + int(SWRANK, int32))
            call pf_nth_quantile(v, 1.0_real64, pv, n_null=nn)
            call pf_nth_quantile(v, 1.0_real64, pv, i32)
            call pf_nth_quantile(v, 1.0_real64, pv, i64)
            call check(error, pv%raw() == 1006_int32 .and. i32 == WANT .and. i64 == int(WANT, int64) .and. &
                nn == 0_int64, "parquet_date pf_nth_quantile at 1.0 must be the latest date")
        end block
        if (allocated(error)) return
        block
            type(parquet_time) :: v(SWN), pv
            call v%set_raw(2000_int64 + int(SWRANK, int64))
            call pf_nth_quantile(v, 1.0_real64, pv, n_null=nn)
            call pf_nth_quantile(v, 1.0_real64, pv, i32)
            call pf_nth_quantile(v, 1.0_real64, pv, i64)
            call check(error, pv%raw() == 2006_int64 .and. i32 == WANT .and. i64 == int(WANT, int64) .and. &
                nn == 0_int64, "parquet_time pf_nth_quantile at 1.0 must be the latest time")
        end block
        if (allocated(error)) return
        block
            type(parquet_timestamp) :: v(SWN), pv
            integer(int64) :: gs
            integer(int32) :: gn
            call v%set_raw(3000_int64 + int(SWRANK, int64), 7_int32)
            call pf_nth_quantile(v, 1.0_real64, pv, n_null=nn)
            call pf_nth_quantile(v, 1.0_real64, pv, i32)
            call pf_nth_quantile(v, 1.0_real64, pv, i64)
            call pv%get_raw(gs, gn)
            call check(error, gs == 3006_int64 .and. gn == 7_int32 .and. i32 == WANT .and. &
                i64 == int(WANT, int64) .and. nn == 0_int64, &
                "parquet_timestamp pf_nth_quantile at 1.0 must be the latest timestamp")
        end block
        if (allocated(error)) return
        block
            type(parquet_string_column) :: sc
            character(len=:), allocatable :: pv
            do k = 1, SWN
                call sc%append_string(swchr(SWRANK(k)))
            end do
            call pf_nth_quantile(sc, 1.0_real64, pv, n_null=nn)
            call pf_nth_quantile(sc, 1.0_real64, pv, i32)
            call pf_nth_quantile(sc, 1.0_real64, pv, i64)
            call check(error, pv == swchr(6) .and. i32 == WANT .and. i64 == int(WANT, int64) .and. &
                nn == 0_int64, "parquet_string_column pf_nth_quantile at 1.0 must be the maximum")
        end block
    end subroutine test_quantile_every_specific
    !
    !> Whichever index kind a sweep just asked for, compared against the expected element list.
    !> Both permutation variables are passed every time because only one of them is defined on any
    !> given pass, and `ik` says which -- reading the other would be reading an unset allocatable.
    pure function sweep_perm_ok(p32, p64, ik, want) result(ok)
        integer(int32), allocatable, intent(in) :: p32(:) !! the int32 result, defined when ik == 1.
        integer(int64), allocatable, intent(in) :: p64(:) !! the int64 result, defined when ik == 2.
        integer, intent(in) :: ik      !! 1 for the int32 form, 2 for the int64 form.
        integer, intent(in) :: want(:) !! the element indices expected, in order.
        logical :: ok                  !! .true. when the requested form matches `want` exactly.
        if (ik == 1) then
            ok = allocated(p32)
            if (ok) ok = size(p32) == size(want)
            if (ok) ok = all(p32 == int(want, int32))
        else
            ok = allocated(p64)
            if (ok) ok = size(p64) == size(want)
            if (ok) ok = all(p64 == int(want, int64))
        end if
    end function sweep_perm_ok
    !
    ! ==================================================================================
    ! M3: searching a sorted array
    ! ==================================================================================
    !
    !> **The oracle is `count`, not a second engine call.** For an ascending array the lower bound
    !> of `t` is `1 + count(v < t)` and the upper bound `1 + count(v <= t)` -- arithmetic this
    !> module has no part in, so it catches an off-by-one that comparing two searches against each
    !> other never could. Every target from below the smallest to above the largest is swept,
    !> including the gaps between elements and the duplicated value.
    subroutine test_search_linear_oracle(error)
        type(error_type), allocatable, intent(out) :: error
        integer(int32) :: v(9) = [10, 20, 20, 20, 30, 40, 40, 50, 60]
        integer(int32) :: t
        integer :: lo, hi

        do t = 5_int32, 65_int32, 5_int32
            call pf_lower_bound(v, t, lo)
            call pf_upper_bound(v, t, hi)
            call check(error, lo == 1 + count(v < t), &
                "pf_lower_bound must equal 1 + count(v < target)")
            if (allocated(error)) return
            call check(error, hi == 1 + count(v <= t), &
                "pf_upper_bound must equal 1 + count(v <= target)")
            if (allocated(error)) return
        end do
    end subroutine test_search_linear_oracle
    !
    !> Both halves of the contract: a value with duplicates comes back as a non-empty inclusive
    !> range, and an ABSENT value as `last == first - 1` -- the empty-range convention a caller
    !> must check before reading `values(first)`.
    subroutine test_equal_range_duplicates(error)
        type(error_type), allocatable, intent(out) :: error
        integer(int32) :: v(9) = [10, 20, 20, 20, 30, 40, 40, 50, 60]
        integer :: first, last

        call pf_equal_range(v, 20_int32, first, last)
        call check(error, first == 2 .and. last == 4, &
            "equal_range must bracket every element equal to the target")
        if (allocated(error)) return
        call check(error, last - first + 1 == count(v == 20_int32), &
            "the range width must equal how many elements match")
        if (allocated(error)) return
        call pf_equal_range(v, 25_int32, first, last)
        call check(error, last == first - 1, "an absent target must give an empty range")
        if (allocated(error)) return
        call check(error, first == 5, "an absent target's range must start at its insertion point")
    end subroutine test_equal_range_duplicates
    !
    !> `descending` selects the comparison, it does not reorder anything -- so on a descending
    !> array the oracle flips to `count(v > t)`. Getting this wrong still passes on an ascending
    !> fixture, which is why it has its own test.
    subroutine test_search_descending(error)
        type(error_type), allocatable, intent(out) :: error
        integer(int32) :: v(7) = [60, 50, 40, 30, 30, 20, 10]
        integer(int32) :: t
        integer :: lo, hi

        do t = 5_int32, 65_int32, 5_int32
            call pf_lower_bound(v, t, lo, descending=.true.)
            call pf_upper_bound(v, t, hi, descending=.true.)
            call check(error, lo == 1 + count(v > t), &
                "a descending lower bound must equal 1 + count(v > target)")
            if (allocated(error)) return
            call check(error, hi == 1 + count(v >= t), &
                "a descending upper bound must equal 1 + count(v >= target)")
            if (allocated(error)) return
        end do
    end subroutine test_search_descending
    !
    !> A null is ordered AFTER every value by default, so a target larger than every value lands
    !> before the nulls rather than at the end of the array.
    subroutine test_search_nulls(error)
        type(error_type), allocatable, intent(out) :: error
        integer(int32) :: v(5) = [10, 20, 30, 999, 888]
        logical :: mask(5) = [.true., .true., .true., .false., .false.]
        integer :: pos

        call pf_lower_bound(v, 20_int32, pos, is_valid=mask)
        call check(error, pos == 2, "a present target must be found before the null tier")
        if (allocated(error)) return
        call pf_upper_bound(v, 30_int32, pos, is_valid=mask)
        call check(error, pos == 4, &
            "a target past every value must land before the nulls, not at the end")
        if (allocated(error)) return
        call pf_lower_bound(v, 25_int32, pos, is_valid=mask)
        call check(error, pos == 3, "an absent target must land between the values it falls between")
    end subroutine test_search_nulls
    !
    !> `assume_sorted` is a promise about the input, not a change of behaviour: on input that IS
    !> sorted, every answer must be identical with and without it.
    subroutine test_search_assume_sorted(error)
        type(error_type), allocatable, intent(out) :: error
        integer(int32) :: v(6) = [1, 3, 3, 7, 9, 11]
        integer(int32) :: t
        integer :: checked, promised

        do t = 0_int32, 12_int32
            call pf_lower_bound(v, t, checked)
            call pf_lower_bound(v, t, promised, assume_sorted=.true.)
            call check(error, checked == promised, &
                "assume_sorted must not change the answer for genuinely sorted input")
            if (allocated(error)) return
        end do
    end subroutine test_search_assume_sorted
    !
    !> The int32 and int64 forms are separate specifics, so each needs proving against the other.
    subroutine test_search_index_kinds(error)
        type(error_type), allocatable, intent(out) :: error
        integer(int32) :: v(5) = [2, 4, 6, 8, 10]
        integer(int32) :: small, f32, l32
        integer(int64) :: big, f64, l64

        call pf_lower_bound(v, 6_int32, small)
        call pf_lower_bound(v, 6_int32, big)
        call check(error, int(small, int64) == big, &
            "the int32 and int64 lower-bound forms must agree")
        if (allocated(error)) return
        call pf_equal_range(v, 8_int32, f32, l32)
        call pf_equal_range(v, 8_int32, f64, l64)
        call check(error, int(f32, int64) == f64 .and. int(l32, int64) == l64, &
            "the int32 and int64 equal_range forms must agree")
    end subroutine test_search_index_kinds
    !
    !> The three non-numeric shapes a search has to cope with: a blank-padded `character` array
    !> (where the target is padded to the array's own width), a `parquet_string_column` (stored
    !> verbatim, no width to pad to) and a temporal type reaching the engine as a raw integer.
    subroutine test_search_other_types(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=4) :: names(4) = ["ada ", "bob ", "bob ", "cid "]
        type(parquet_string_column) :: sc
        type(parquet_date) :: days(3), want
        integer :: lo, hi

        call pf_lower_bound(names, "bob", lo)
        call pf_upper_bound(names, "bob", hi)
        call check(error, lo == 2 .and. hi == 4, &
            "a shorter target must be blank-padded to the array's element length")
        if (allocated(error)) return
        call sc%append_string("ada")
        call sc%append_string("bob")
        call sc%append_string("bob")
        call sc%append_string("cid")
        call pf_lower_bound(sc, "bob", lo)
        call pf_upper_bound(sc, "bob", hi)
        call check(error, lo == 2 .and. hi == 4, "a string column must bracket its duplicates too")
        if (allocated(error)) return
        call days(1)%set(2020, 1, 1)
        call days(2)%set(2021, 6, 15)
        call days(3)%set(2022, 12, 31)
        call want%set(2021, 6, 15)
        call pf_lower_bound(days, want, lo)
        call check(error, lo == 2, "a date target must be located by its raw day count")
    end subroutine test_search_other_types
    !
    !> An empty array has exactly one insertion point, and a one-element array two. Both are easy
    !> to get wrong by clamping the range to `size(values)`.
    subroutine test_search_degenerate(error)
        type(error_type), allocatable, intent(out) :: error
        integer(int32), allocatable :: empty(:)
        integer(int32) :: one(1) = [5]
        integer :: pos

        allocate(empty(0))
        call pf_lower_bound(empty, 3_int32, pos)
        call check(error, pos == 1, "an empty array's only insertion point is 1")
        if (allocated(error)) return
        call pf_lower_bound(one, 3_int32, pos)
        call check(error, pos == 1, "a target below the single element must insert before it")
        if (allocated(error)) return
        call pf_upper_bound(one, 9_int32, pos)
        call check(error, pos == 2, "a target above the single element must insert after it")
    end subroutine test_search_degenerate
    !
    !> **Every `pf_lower_bound`, `pf_upper_bound` and `pf_equal_range` specific, once each** -- ten
    !> value families times three operations times the two index kinds, which is sixty separate
    !> generated bodies. `parquet_column` is absent because a search needs a target of the
    !> element's own type, which a type-erased column cannot supply.
    !>
    !> All four expected numbers differ (2, 4, 2 and 3), so a body wired to the wrong operation,
    !> or one that reported `last` where `first` belongs, fails rather than coinciding.
    subroutine test_search_every_specific(error)
        type(error_type), allocatable, intent(out) :: error
        !> Class 2's lower bound, upper bound, and the two ends of its equal range.
        integer, parameter :: WANT(4) = [2, 4, 2, 3]
        !> The same four for the logical fixture, searching for `.true.`.
        integer, parameter :: WANTB(4) = [4, 7, 4, 6]
        integer(int32) :: lo32, up32, fi32, la32
        integer(int64) :: lo64, up64, fi64, la64
        integer :: k

        block
            integer(int32) :: v(SWN), t
            v = 10_int32 * int(SWSRT, int32)
            t = 20_int32
            call pf_lower_bound(v, t, lo32)
            call pf_lower_bound(v, t, lo64)
            call pf_upper_bound(v, t, up32)
            call pf_upper_bound(v, t, up64)
            call pf_equal_range(v, t, fi32, la32)
            call pf_equal_range(v, t, fi64, la64)
            call check(error, sweep_search_ok([lo32, up32, fi32, la32], [lo64, up64, fi64, la64], WANT), &
                "int32 searches must bracket the duplicated class in both index kinds")
        end block
        if (allocated(error)) return
        block
            integer(int64) :: v(SWN), t
            v = 100_int64 * int(SWSRT, int64)
            t = 200_int64
            call pf_lower_bound(v, t, lo32)
            call pf_lower_bound(v, t, lo64)
            call pf_upper_bound(v, t, up32)
            call pf_upper_bound(v, t, up64)
            call pf_equal_range(v, t, fi32, la32)
            call pf_equal_range(v, t, fi64, la64)
            call check(error, sweep_search_ok([lo32, up32, fi32, la32], [lo64, up64, fi64, la64], WANT), &
                "int64 searches must bracket the duplicated class in both index kinds")
        end block
        if (allocated(error)) return
        block
            real(real32) :: v(SWN), t
            v = real(SWSRT, real32) + 0.5_real32
            t = 2.5_real32
            call pf_lower_bound(v, t, lo32)
            call pf_lower_bound(v, t, lo64)
            call pf_upper_bound(v, t, up32)
            call pf_upper_bound(v, t, up64)
            call pf_equal_range(v, t, fi32, la32)
            call pf_equal_range(v, t, fi64, la64)
            call check(error, sweep_search_ok([lo32, up32, fi32, la32], [lo64, up64, fi64, la64], WANT), &
                "real32 searches must bracket the duplicated class in both index kinds")
        end block
        if (allocated(error)) return
        block
            real(real64) :: v(SWN), t
            logical :: mask(SWN)
            v = real(SWSRT, real64) + 0.25_real64
            t = 2.25_real64
            call pf_lower_bound(v, t, lo32)
            call pf_lower_bound(v, t, lo64)
            call pf_upper_bound(v, t, up32)
            call pf_upper_bound(v, t, up64)
            call pf_equal_range(v, t, fi32, la32)
            call pf_equal_range(v, t, fi64, la64)
            call check(error, sweep_search_ok([lo32, up32, fi32, la32], [lo64, up64, fi64, la64], WANT), &
                "real64 searches must bracket the duplicated class in both index kinds")
            if (allocated(error)) return
            ! A null sorts last, so it is outside the searched region entirely: marking the final
            ! element null must not move any of the four answers.
            mask = .true.
            mask(SWN) = .false.
            call pf_equal_range(v, t, fi32, la32, is_valid=mask, assume_sorted=.true.)
            call check(error, fi32 == 2_int32 .and. la32 == 3_int32, &
                "a trailing null must leave a range that ends before it untouched")
        end block
        if (allocated(error)) return
        block
            call pf_lower_bound(SWSRTB, .true., lo32)
            call pf_lower_bound(SWSRTB, .true., lo64)
            call pf_upper_bound(SWSRTB, .true., up32)
            call pf_upper_bound(SWSRTB, .true., up64)
            call pf_equal_range(SWSRTB, .true., fi32, la32)
            call pf_equal_range(SWSRTB, .true., fi64, la64)
            call check(error, sweep_search_ok([lo32, up32, fi32, la32], [lo64, up64, fi64, la64], WANTB), &
                "logical searches must bracket the .true. run in both index kinds")
        end block
        if (allocated(error)) return
        block
            character(len=2) :: v(SWN)
            do k = 1, SWN
                v(k) = swchr(SWSRT(k))
            end do
            call pf_lower_bound(v, swchr(2), lo32)
            call pf_lower_bound(v, swchr(2), lo64)
            call pf_upper_bound(v, swchr(2), up32)
            call pf_upper_bound(v, swchr(2), up64)
            call pf_equal_range(v, swchr(2), fi32, la32)
            call pf_equal_range(v, swchr(2), fi64, la64)
            call check(error, sweep_search_ok([lo32, up32, fi32, la32], [lo64, up64, fi64, la64], WANT), &
                "character searches must bracket the duplicated class in both index kinds")
        end block
        if (allocated(error)) return
        block
            type(parquet_date) :: v(SWN), t
            call v%set_raw(1000_int32 + int(SWSRT, int32))
            call t%set_raw(1002_int32)
            call pf_lower_bound(v, t, lo32)
            call pf_lower_bound(v, t, lo64)
            call pf_upper_bound(v, t, up32)
            call pf_upper_bound(v, t, up64)
            call pf_equal_range(v, t, fi32, la32)
            call pf_equal_range(v, t, fi64, la64)
            call check(error, sweep_search_ok([lo32, up32, fi32, la32], [lo64, up64, fi64, la64], WANT), &
                "parquet_date searches must bracket the duplicated class in both index kinds")
        end block
        if (allocated(error)) return
        block
            type(parquet_time) :: v(SWN), t
            call v%set_raw(2000_int64 + int(SWSRT, int64))
            call t%set_raw(2002_int64)
            call pf_lower_bound(v, t, lo32)
            call pf_lower_bound(v, t, lo64)
            call pf_upper_bound(v, t, up32)
            call pf_upper_bound(v, t, up64)
            call pf_equal_range(v, t, fi32, la32)
            call pf_equal_range(v, t, fi64, la64)
            call check(error, sweep_search_ok([lo32, up32, fi32, la32], [lo64, up64, fi64, la64], WANT), &
                "parquet_time searches must bracket the duplicated class in both index kinds")
        end block
        if (allocated(error)) return
        block
            type(parquet_timestamp) :: v(SWN), t
            call v%set_raw(3000_int64 + int(SWSRT, int64), 7_int32)
            call t%set_raw(3002_int64, 7_int32)
            call pf_lower_bound(v, t, lo32)
            call pf_lower_bound(v, t, lo64)
            call pf_upper_bound(v, t, up32)
            call pf_upper_bound(v, t, up64)
            call pf_equal_range(v, t, fi32, la32)
            call pf_equal_range(v, t, fi64, la64)
            call check(error, sweep_search_ok([lo32, up32, fi32, la32], [lo64, up64, fi64, la64], WANT), &
                "parquet_timestamp searches must bracket the duplicated class in both index kinds")
        end block
        if (allocated(error)) return
        block
            type(parquet_string_column) :: sc
            do k = 1, SWN
                call sc%append_string(swchr(SWSRT(k)))
            end do
            call pf_lower_bound(sc, swchr(2), lo32)
            call pf_lower_bound(sc, swchr(2), lo64)
            call pf_upper_bound(sc, swchr(2), up32)
            call pf_upper_bound(sc, swchr(2), up64)
            call pf_equal_range(sc, swchr(2), fi32, la32)
            call pf_equal_range(sc, swchr(2), fi64, la64)
            call check(error, sweep_search_ok([lo32, up32, fi32, la32], [lo64, up64, fi64, la64], WANT), &
                "parquet_string_column searches must bracket the duplicated class in both index kinds")
            if (allocated(error)) return
            block
                ! A descending search needs descending input -- the same fixture reversed, where
                ! class 2's run sits at 4..5 rather than at 2..3.
                type(parquet_string_column) :: rev
                do k = SWN, 1, -1
                    call rev%append_string(swchr(SWSRT(k)))
                end do
                call pf_lower_bound(rev, swchr(2), lo32, descending=.true.)
                call pf_upper_bound(rev, swchr(2), up32, descending=.true.)
                call pf_equal_range(rev, swchr(2), fi32, la32, descending=.true.)
                call check(error, lo32 == 4_int32 .and. up32 == 6_int32 .and. fi32 == 4_int32 .and. &
                    la32 == 5_int32, &
                    "a descending parquet_string_column search must bracket the reversed run")
            end block
        end block
    end subroutine test_search_every_specific
    !
    !> The four search answers in both index kinds against one expected list. Taking all four
    !> together is what makes a body wired to the wrong operation visible: each of the three
    !> operations has a different expected number, so a mix-up cannot coincide.
    pure function sweep_search_ok(g32, g64, want) result(ok)
        integer(int32), intent(in) :: g32(4) !! lower, upper, first, last -- the int32 forms.
        integer(int64), intent(in) :: g64(4) !! the same four, the int64 forms.
        integer, intent(in) :: want(4)       !! the expected lower, upper, first and last.
        logical :: ok                        !! .true. when all eight answers match.
        ok = all(g32 == int(want, int32)) .and. all(g64 == int(want, int64))
    end function sweep_search_ok
    !
    ! ==================================================================================
    ! M3: distinct values and ranks
    ! ==================================================================================
    !
    !> The distinct values come back SORTED, and `descending` reverses that order without changing
    !> which values they are.
    subroutine test_unique_basic(error)
        type(error_type), allocatable, intent(out) :: error
        integer(int32) :: v(8) = [30, 10, 20, 10, 30, 30, 40, 20]
        integer(int32), allocatable :: d(:)
        integer :: c

        call pf_unique_count(v, c)
        call check(error, c == 4, "pf_unique_count must count each repeated value once")
        if (allocated(error)) return
        call pf_unique(v, d)
        call check(error, size(d) == 4, "pf_unique must return exactly the distinct values")
        if (allocated(error)) return
        call check(error, all(d == [10, 20, 30, 40]), "the distinct values must come back in order")
        if (allocated(error)) return
        call pf_unique(v, d, descending=.true.)
        call check(error, all(d == [40, 30, 20, 10]), "descending must reverse the distinct values")
    end subroutine test_unique_basic
    !
    !> Nulls are outside the population: excluded from the count, absent from the distinct values,
    !> and reported separately -- and a null's stored value must not sneak in as a value of its own.
    subroutine test_unique_nulls(error)
        type(error_type), allocatable, intent(out) :: error
        integer(int32) :: v(6) = [10, 20, 99, 10, 99, 30]
        logical :: mask(6) = [.true., .true., .false., .true., .false., .true.]
        integer(int32), allocatable :: d(:)
        integer :: c
        integer(int64) :: nn

        call pf_unique_count(v, c, is_valid=mask, n_null=nn)
        call check(error, c == 3, "nulls must not count as distinct values")
        if (allocated(error)) return
        call check(error, nn == 2_int64, "n_null must report how many values were null")
        if (allocated(error)) return
        call pf_unique(v, d, is_valid=mask)
        call check(error, all(d == [10, 20, 30]), &
            "a null's stored value must not appear among the distinct values")
    end subroutine test_unique_nulls
    !
    !> **Risk-35 discipline.** A low-cardinality integer array takes the counting fast path, which
    !> performs zero comparisons -- so a test that only ever runs it proves nothing about the
    !> comparator's run detection. Both paths are forced and required to agree.
    subroutine test_unique_both_paths(error)
        type(error_type), allocatable, intent(out) :: error
        integer(int32) :: v(60)
        integer(int32), allocatable :: fast(:), slow(:)
        integer(int64) :: cmp_fast, cmp_slow
        integer :: k, c_fast, c_slow

        do k = 1, 60
            v(k) = int(mod(k * 7, 6), int32)   ! six distinct values: the counting path's case
        end do
        call arm_sort_comparisons()
        call pf_unique_count(v, c_fast)
        call pf_unique(v, fast)
        cmp_fast = sort_comparisons()
        call parquet_set_sort_counting_path(.false.)
        call arm_sort_comparisons()
        call pf_unique_count(v, c_slow)
        call pf_unique(v, slow)
        cmp_slow = sort_comparisons()
        call parquet_set_sort_counting_path(.true.)
        call check(error, cmp_fast == 0_int64 .and. cmp_slow > 0_int64, &
            "the two halves must reach DIFFERENT engines, or the agreement below is vacuous")
        if (allocated(error)) return
        call check(error, c_fast == 6 .and. c_slow == 6, &
            "both sort paths must find the same six distinct values")
        if (allocated(error)) return
        call check(error, size(fast) == size(slow), "both paths must return the same number of values")
        if (allocated(error)) return
        call check(error, all(fast == slow), "both paths must return the same distinct values")
    end subroutine test_unique_both_paths
    !
    !> Distinctness is the sort comparator's equality, which on reals is EXACT -- and separately,
    !> every NaN is one value collectively, even though `==` reports every NaN pair as unequal.
    subroutine test_unique_float_exact(error)
        type(error_type), allocatable, intent(out) :: error
        real(real64) :: v(3) = [0.1_real64 + 0.2_real64, 0.3_real64, 0.3_real64]
        real(real64) :: w(4)
        integer :: c

        call pf_unique_count(v, c)
        call check(error, c == 2, "0.1 + 0.2 and 0.3 must count as two distinct values")
        if (allocated(error)) return
        w(1) = 1.0_real64
        w(2) = ieee_value(1.0_real64, ieee_quiet_nan)
        w(3) = ieee_value(1.0_real64, ieee_quiet_nan)
        w(4) = 2.0_real64
        call pf_unique_count(w, c)
        call check(error, c == 3, "every NaN must collapse into one distinct value")
    end subroutine test_unique_float_exact
    !
    !> A `parquet_string_column` answers with a column of its own, so its distinct values keep the
    !> variable-length storage rather than being flattened to a padded array.
    subroutine test_unique_string_column(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_string_column) :: sc, d
        character(len=:), allocatable :: s
        integer :: c

        call sc%append_string("pear")
        call sc%append_string("fig")
        call sc%append_string("pear")
        call sc%append_string("apple")
        call sc%append_string("fig")
        call pf_unique_count(sc, c)
        call check(error, c == 3, "a string column's repeated values must count once")
        if (allocated(error)) return
        call pf_unique(sc, d)
        call check(error, d%size() == 3_int64, "the distinct column must hold three elements")
        if (allocated(error)) return
        call d%get(1_int64, s)
        call check(error, s == "apple", "the distinct values must come back in sorted order")
        if (allocated(error)) return
        call d%get(3_int64, s)
        call check(error, s == "pear", "the last distinct value must be the largest")
    end subroutine test_unique_string_column
    !
    !> **The three methods must genuinely differ on the same fixture**, or the selector is untested:
    !> a fixture without duplicates gives 1,2,3,4 for all three.
    subroutine test_rank_methods(error)
        type(error_type), allocatable, intent(out) :: error
        integer(int32) :: v(4) = [10, 20, 20, 30]
        integer, allocatable :: r(:)

        call pf_rank(v, r)
        call check(error, all(r == [1, 2, 2, 4]), "the default method must be competition ranking")
        if (allocated(error)) return
        call pf_rank(v, r, method="dense")
        call check(error, all(r == [1, 2, 2, 3]), "dense ranking must skip no rank")
        if (allocated(error)) return
        call pf_rank(v, r, method="ordinal")
        call check(error, all(r == [1, 2, 3, 4]), "ordinal ranking must give every element its own rank")
        if (allocated(error)) return
        call pf_rank(v, r, method="DeNsE")
        call check(error, all(r == [1, 2, 2, 3]), "a method token must match regardless of case")
    end subroutine test_rank_methods
    !
    !> Rank 0 means "no rank", which is why `pf_rank` takes no `nulls_first`: a null is outside the
    !> ranking rather than placed at one end of it.
    subroutine test_rank_nulls_zero(error)
        type(error_type), allocatable, intent(out) :: error
        integer(int32) :: v(4) = [10, 99, 20, 30]
        logical :: mask(4) = [.true., .false., .true., .true.]
        integer, allocatable :: r(:)

        call pf_rank(v, r, is_valid=mask)
        call check(error, r(2) == 0, "a null must get rank 0")
        if (allocated(error)) return
        call check(error, all(r == [1, 0, 2, 3]), &
            "the remaining ranks must be consecutive, as if the null were absent")
    end subroutine test_rank_nulls_zero
    !
    !> A free cross-check between two independently generated families: ordinal rank is exactly the
    !> inverse permutation of `pf_argsort`, so `r(perm(k)) == k` for every k.
    subroutine test_rank_inverts_argsort(error)
        type(error_type), allocatable, intent(out) :: error
        integer(int32) :: v(8) = [30, 10, 50, 10, 60, 40, 15, 55]
        integer, allocatable :: r(:), perm(:)
        integer :: k
        logical :: ok

        call pf_argsort(v, perm)
        call pf_rank(v, r, method="ordinal")
        ok = .true.
        do k = 1, 8
            if (r(perm(k)) /= k) ok = .false.
        end do
        call check(error, ok, "ordinal ranks must be the inverse of pf_argsort's permutation")
    end subroutine test_rank_inverts_argsort
    !
    !> `descending` ranks from the largest value down, without moving nulls or changing tie rules.
    subroutine test_rank_descending(error)
        type(error_type), allocatable, intent(out) :: error
        integer(int32) :: v(4) = [10, 20, 20, 30]
        integer, allocatable :: r(:)

        call pf_rank(v, r, descending=.true.)
        call check(error, all(r == [4, 2, 2, 1]), &
            "descending competition ranking must rank the largest value 1")
        if (allocated(error)) return
        call pf_rank(v, r, method="dense", descending=.true.)
        call check(error, all(r == [3, 2, 2, 1]), "descending dense ranking must skip no rank")
    end subroutine test_rank_descending
    !
    !> **Risk-35 discipline**, for the same reason as `test_unique_both_paths`: ranking turns on
    !> run detection, and a counting-path fixture never invokes the comparator that finds the runs.
    subroutine test_rank_both_paths(error)
        type(error_type), allocatable, intent(out) :: error
        integer(int32) :: v(50)
        integer, allocatable :: fast(:), slow(:)
        integer(int64) :: cmp_fast, cmp_slow
        integer :: k

        do k = 1, 50
            v(k) = int(mod(k * 3, 4), int32)   ! four distinct values, heavily tied
        end do
        call arm_sort_comparisons()
        call pf_rank(v, fast)
        cmp_fast = sort_comparisons()
        call parquet_set_sort_counting_path(.false.)
        call arm_sort_comparisons()
        call pf_rank(v, slow)
        cmp_slow = sort_comparisons()
        call parquet_set_sort_counting_path(.true.)
        call check(error, cmp_fast == 0_int64 .and. cmp_slow > 0_int64, &
            "the two halves must reach DIFFERENT engines, or the agreement below is vacuous")
        if (allocated(error)) return
        call check(error, all(fast == slow), &
            "both sort paths must produce the same competition ranks")
        if (allocated(error)) return
        call check(error, maxval(fast) == 50 - count(v == maxval(v)) + 1, &
            "the largest competition rank must be one past the count below the top run")
    end subroutine test_rank_both_paths
    !
    ! ----------------------------------------------------------------------------------
    ! Distinct-value sweeps: one call per generated specific
    ! ----------------------------------------------------------------------------------
    !
    !> **Every `pf_unique_count` and `pf_unique` specific, once each** -- eleven value families
    !> times two count kinds for the counter, and ten families for the values (`parquet_column`
    !> has no `pf_unique`, since a distinct VALUE needs a compile-time element type where a count
    !> does not).
    !>
    !> `SWDUP` is the fixture rather than `SWRANK`, because a distinct-value question needs ties to
    !> mean anything: six elements over three classes, each class appearing twice and neither
    !> contiguously nor in class order, so a body that reported the first occurrence of each RUN
    !> rather than of each VALUE would give four and not three.
    subroutine test_unique_every_specific(error)
        type(error_type), allocatable, intent(out) :: error
        !> Element 1 is null. It belongs to class 3, whose OTHER element (3) is not -- so the
        !> distinct count must not move, and only the null count may. A body that dropped the
        !> whole class, or that stopped counting nulls at the first one, fails on one or the other.
        logical, parameter :: NMASK(SWN) = [.false., .true., .true., .true., .true., .true.]
        integer :: k
        integer(int32) :: c32
        integer(int64) :: c64, nn

        block
            integer(int32) :: v(SWN)
            integer(int32), allocatable :: d(:)
            v = 10_int32 * int(SWDUP, int32)
            call pf_unique_count(v, c32, n_null=nn)
            call pf_unique_count(v, c64)
            call pf_unique(v, d, n_null=nn)
            call check(error, c32 == 3_int32 .and. c64 == 3_int64 .and. nn == 0_int64 .and. &
                size(d) == 3 .and. all(d == [10_int32, 20_int32, 30_int32]), &
                "int32 pf_unique must report the three distinct values, ascending, and count them")
            if (allocated(error)) return
            call pf_unique_count(v, c32, is_valid=NMASK, n_null=nn)
            call check(error, c32 == 3_int32 .and. nn == 1_int64, &
                "int32 pf_unique_count must count the null and keep its surviving class-mate")
        end block
        if (allocated(error)) return
        block
            integer(int64) :: v(SWN)
            integer(int64), allocatable :: d(:)
            v = 100_int64 * int(SWDUP, int64)
            call pf_unique_count(v, c32, n_null=nn)
            call pf_unique_count(v, c64)
            call pf_unique(v, d, n_null=nn)
            call check(error, c32 == 3_int32 .and. c64 == 3_int64 .and. nn == 0_int64 .and. &
                size(d) == 3 .and. all(d == [100_int64, 200_int64, 300_int64]), &
                "int64 pf_unique must report the three distinct values, ascending, and count them")
            if (allocated(error)) return
            call pf_unique_count(v, c32, is_valid=NMASK, n_null=nn)
            call check(error, c32 == 3_int32 .and. nn == 1_int64, &
                "int64 pf_unique_count must count the null and keep its surviving class-mate")
        end block
        if (allocated(error)) return
        block
            real(real32) :: v(SWN)
            real(real32), allocatable :: d(:)
            v = real(SWDUP, real32) + 0.5_real32
            call pf_unique_count(v, c32, n_null=nn)
            call pf_unique_count(v, c64)
            call pf_unique(v, d, n_null=nn)
            call check(error, c32 == 3_int32 .and. c64 == 3_int64 .and. nn == 0_int64 .and. &
                size(d) == 3 .and. all(d == [1.5_real32, 2.5_real32, 3.5_real32]), &
                "real32 pf_unique must report the three distinct values, ascending, and count them")
            if (allocated(error)) return
            call pf_unique_count(v, c32, is_valid=NMASK, n_null=nn)
            call check(error, c32 == 3_int32 .and. nn == 1_int64, &
                "real32 pf_unique_count must count the null and keep its surviving class-mate")
        end block
        if (allocated(error)) return
        block
            real(real64) :: v(SWN)
            real(real64), allocatable :: d(:)
            logical :: mask(SWN)
            v = real(SWDUP, real64) + 0.25_real64
            mask = .true.
            mask(2) = .false.       ! one of the two elements of class 1: the CLASS must survive
            mask(5) = .false.       ! now the whole of class 1 is null, so it must disappear
            call pf_unique_count(v, c32, is_valid=mask, n_null=nn)
            call pf_unique(v, d, is_valid=mask)
            call check(error, c32 == 2_int32 .and. nn == 2_int64 .and. size(d) == 2 .and. &
                all(d == [2.25_real64, 3.25_real64]), &
                "real64 pf_unique must drop a wholly-null class and count both its nulls")
            if (allocated(error)) return
            call pf_unique_count(v, c64)
            call pf_unique(v, d, descending=.true.)
            call check(error, c64 == 3_int64 .and. size(d) == 3 .and. &
                all(d == [3.25_real64, 2.25_real64, 1.25_real64]), &
                "real64 pf_unique must reverse its distinct values under descending")
        end block
        if (allocated(error)) return
        block
            logical, allocatable :: d(:)
            call pf_unique_count(SWBOOL, c32, n_null=nn)
            call pf_unique_count(SWBOOL, c64)
            call pf_unique(SWBOOL, d, n_null=nn)
            call check(error, c32 == 2_int32 .and. c64 == 2_int64 .and. nn == 0_int64 .and. &
                size(d) == 2 .and. (d(1) .eqv. .false.) .and. (d(2) .eqv. .true.), &
                "logical pf_unique must report .false. then .true., and count two distinct values")
            if (allocated(error)) return
            call pf_unique_count(SWBOOL, c32, is_valid=NMASK, n_null=nn)
            call check(error, c32 == 2_int32 .and. nn == 1_int64, &
                "logical pf_unique_count must count the null and keep both values")
        end block
        if (allocated(error)) return
        block
            character(len=2) :: v(SWN)
            character(len=2), allocatable :: d(:)
            do k = 1, SWN
                v(k) = swchr(SWDUP(k))
            end do
            call pf_unique_count(v, c32, n_null=nn)
            call pf_unique_count(v, c64)
            call pf_unique(v, d, n_null=nn)
            call check(error, c32 == 3_int32 .and. c64 == 3_int64 .and. nn == 0_int64 .and. &
                size(d) == 3 .and. all(d == [swchr(1), swchr(2), swchr(3)]), &
                "character pf_unique must report the three distinct values, ascending, and count them")
            if (allocated(error)) return
            call pf_unique_count(v, c32, is_valid=NMASK, n_null=nn)
            call check(error, c32 == 3_int32 .and. nn == 1_int64, &
                "character pf_unique_count must count the null and keep its surviving class-mate")
        end block
        if (allocated(error)) return
        block
            type(parquet_date) :: v(SWN)
            type(parquet_date), allocatable :: d(:)
            call v%set_raw(1000_int32 + int(SWDUP, int32))
            call pf_unique_count(v, c32, n_null=nn)
            call pf_unique_count(v, c64)
            call pf_unique(v, d, n_null=nn)
            call check(error, c32 == 3_int32 .and. c64 == 3_int64 .and. nn == 0_int64 .and. &
                size(d) == 3 .and. all([(d(k)%raw(), k = 1, 3)] == [1001, 1002, 1003]), &
                "parquet_date pf_unique must report the three distinct dates, ascending")
            if (allocated(error)) return
            call v(1)%set_null()        ! a temporal element carries its own null, so there is no mask
            call pf_unique_count(v, c32, n_null=nn)
            call check(error, c32 == 3_int32 .and. nn == 1_int64, &
                "parquet_date pf_unique_count must count an element that marked itself null")
        end block
        if (allocated(error)) return
        block
            type(parquet_time) :: v(SWN)
            type(parquet_time), allocatable :: d(:)
            call v%set_raw(2000_int64 + int(SWDUP, int64))
            call pf_unique_count(v, c32, n_null=nn)
            call pf_unique_count(v, c64)
            call pf_unique(v, d, n_null=nn)
            call check(error, c32 == 3_int32 .and. c64 == 3_int64 .and. nn == 0_int64 .and. &
                size(d) == 3 .and. &
                all([(d(k)%raw(), k = 1, 3)] == [2001_int64, 2002_int64, 2003_int64]), &
                "parquet_time pf_unique must report the three distinct times, ascending")
            if (allocated(error)) return
            call v(1)%set_null()
            call pf_unique_count(v, c32, n_null=nn)
            call check(error, c32 == 3_int32 .and. nn == 1_int64, &
                "parquet_time pf_unique_count must count an element that marked itself null")
        end block
        if (allocated(error)) return
        block
            type(parquet_timestamp) :: v(SWN)
            type(parquet_timestamp), allocatable :: d(:)
            integer(int64) :: gs(3)
            integer(int32) :: gn(3)
            call v%set_raw(3000_int64 + int(SWDUP, int64), 7_int32)
            call pf_unique_count(v, c32, n_null=nn)
            call pf_unique_count(v, c64)
            call pf_unique(v, d, n_null=nn)
            do k = 1, min(3, size(d))
                call d(k)%get_raw(gs(k), gn(k))
            end do
            call check(error, c32 == 3_int32 .and. c64 == 3_int64 .and. nn == 0_int64 .and. &
                size(d) == 3 .and. all(gs == [3001_int64, 3002_int64, 3003_int64]) .and. &
                all(gn == 7_int32), &
                "parquet_timestamp pf_unique must report the three distinct timestamps, ascending")
            if (allocated(error)) return
            call v(1)%set_null()
            call pf_unique_count(v, c32, n_null=nn)
            call check(error, c32 == 3_int32 .and. nn == 1_int64, &
                "parquet_timestamp pf_unique_count must count an element that marked itself null")
        end block
        if (allocated(error)) return
        block
            type(parquet_string_column) :: sc, d
            character(len=:), allocatable :: got
            logical :: ok
            do k = 1, SWN
                call sc%append_string(swchr(SWDUP(k)))
            end do
            call pf_unique_count(sc, c32, n_null=nn)
            call pf_unique_count(sc, c64)
            call pf_unique(sc, d, n_null=nn)
            ok = c32 == 3_int32 .and. c64 == 3_int64 .and. nn == 0_int64
            if (ok) ok = d%size() == 3_int64
            if (ok) then
                do k = 1, 3
                    call d%get(int(k, int64), got)
                    if (got /= swchr(k)) ok = .false.
                end do
            end if
            call check(error, ok, &
                "parquet_string_column pf_unique must report the three distinct values, ascending")
            if (allocated(error)) return
            block
                type(parquet_string_column) :: withnull
                call withnull%append_null()     ! the store owns its validity, so there is no mask
                do k = 2, SWN
                    call withnull%append_string(swchr(SWDUP(k)))
                end do
                call pf_unique_count(withnull, c32, n_null=nn)
                call check(error, c32 == 3_int32 .and. nn == 1_int64, &
                    "parquet_string_column pf_unique_count must count its own null element")
            end block
        end block
        if (allocated(error)) return
        block
            ! parquet_column counts distinct values but cannot return them: an out-argument would
            ! need an element type the type-erased column only knows at runtime.
            type(parquet_column) :: c
            call c%init(PK_INT32, int(SWN, int64))
            call c%set_all(10_int32 * int(SWDUP, int32))
            call c%set_null(2_int64)
            call pf_unique_count(c, c32, n_null=nn)
            call pf_unique_count(c, c64)
            call check(error, c32 == 3_int32 .and. c64 == 3_int64 .and. nn == 1_int64, &
                "parquet_column pf_unique_count must count by runtime kind and report its null")
        end block
    end subroutine test_unique_every_specific
    !
    !> **Every `pf_rank` specific, once each** -- eleven value families times the two rank-array
    !> kinds. Competition ranking over `SWDUP` is the assertion: each of the three classes appears
    !> twice, so the ranks must be 1, 3 and 5 and never 1, 2 and 3 -- which is what separates the
    !> default method from `dense` without either being compared against the other.
    subroutine test_rank_every_specific(error)
        type(error_type), allocatable, intent(out) :: error
        !> Competition ranks of `SWDUP` ascending: class 1 ranks 1, class 2 ranks 3, class 3 ranks 5.
        integer, parameter :: RCOMP(SWN) = [5, 1, 5, 3, 1, 3]
        !> The same for `SWBOOL`: three .false. tie at 1, three .true. tie at 4.
        integer, parameter :: RBOOL(SWN) = [4, 1, 4, 4, 1, 1]
        !> DENSE ranks number the classes rather than the rows, so they are `SWDUP` itself -- the
        !> one method whose answer is the fixture's own class labels.
        integer, parameter :: RDENSE(SWN) = SWDUP
        !> ORDINAL ranks break every tie on the original row order, so no two rows share a rank.
        integer, parameter :: RORD(SWN) = [5, 1, 6, 3, 2, 4]
        !> The same two for the logical fixture.
        integer, parameter :: RBDENSE(SWN) = [2, 1, 2, 2, 1, 1]
        integer, parameter :: RBORD(SWN) = [4, 1, 5, 6, 2, 3]
        !> Competition ranks with the order reversed: class 3 leads at 1, class 1 trails at 5.
        integer, parameter :: RDESC(SWN) = [1, 5, 1, 3, 5, 3]
        integer(int32), allocatable :: r32(:)
        integer(int64), allocatable :: r64(:)
        integer :: k

        block
            integer(int32) :: v(SWN)
            v = 10_int32 * int(SWDUP, int32)
            call pf_rank(v, r32)
            call pf_rank(v, r64)
            call check(error, sweep_rank_ok(r32, r64, RCOMP), "int32 pf_rank must give competition ranks")
            if (allocated(error)) return
            call pf_rank(v, r32, method="dense")
            call pf_rank(v, r64, method="ordinal")
            call check(error, all(r32 == int(RDENSE, int32)) .and. all(r64 == int(RORD, int64)), &
                "int32 pf_rank must number classes under dense and every row under ordinal")
        end block
        if (allocated(error)) return
        block
            integer(int64) :: v(SWN)
            v = 100_int64 * int(SWDUP, int64)
            call pf_rank(v, r32)
            call pf_rank(v, r64)
            call check(error, sweep_rank_ok(r32, r64, RCOMP), "int64 pf_rank must give competition ranks")
            if (allocated(error)) return
            call pf_rank(v, r32, method="dense")
            call pf_rank(v, r64, method="ordinal")
            call check(error, all(r32 == int(RDENSE, int32)) .and. all(r64 == int(RORD, int64)), &
                "int64 pf_rank must number classes under dense and every row under ordinal")
        end block
        if (allocated(error)) return
        block
            real(real32) :: v(SWN)
            v = real(SWDUP, real32) + 0.5_real32
            call pf_rank(v, r32)
            call pf_rank(v, r64)
            call check(error, sweep_rank_ok(r32, r64, RCOMP), "real32 pf_rank must give competition ranks")
            if (allocated(error)) return
            call pf_rank(v, r32, method="dense")
            call pf_rank(v, r64, method="ordinal")
            call check(error, all(r32 == int(RDENSE, int32)) .and. all(r64 == int(RORD, int64)), &
                "real32 pf_rank must number classes under dense and every row under ordinal")
        end block
        if (allocated(error)) return
        block
            real(real64) :: v(SWN)
            logical :: mask(SWN)
            v = real(SWDUP, real64) + 0.25_real64
            call pf_rank(v, r32)
            call pf_rank(v, r64)
            call check(error, sweep_rank_ok(r32, r64, RCOMP), "real64 pf_rank must give competition ranks")
            if (allocated(error)) return
            call pf_rank(v, r32, method="dense")
            call pf_rank(v, r64, method="ordinal")
            call check(error, all(r32 == int(RDENSE, int32)) .and. all(r64 == int(RORD, int64)), &
                "real64 pf_rank must number classes under dense and every row under ordinal")
            if (allocated(error)) return
            mask = .true.
            mask(1) = .false.       ! a null ranks 0 and vacates rank 5 for its class-mate
            call pf_rank(v, r32, is_valid=mask)
            call check(error, r32(1) == 0_int32 .and. r32(3) == 5_int32, &
                "real64 pf_rank must rank a null 0 without shifting the ranks below it")
        end block
        if (allocated(error)) return
        block
            call pf_rank(SWBOOL, r32)
            call pf_rank(SWBOOL, r64)
            call check(error, sweep_rank_ok(r32, r64, RBOOL), &
                "logical pf_rank must tie the three .false. at 1 and the three .true. at 4")
            if (allocated(error)) return
            call pf_rank(SWBOOL, r32, method="dense")
            call pf_rank(SWBOOL, r64, method="ordinal")
            call check(error, all(r32 == int(RBDENSE, int32)) .and. all(r64 == int(RBORD, int64)), &
                "logical pf_rank must number classes under dense and every row under ordinal")
        end block
        if (allocated(error)) return
        block
            character(len=2) :: v(SWN)
            do k = 1, SWN
                v(k) = swchr(SWDUP(k))
            end do
            call pf_rank(v, r32)
            call pf_rank(v, r64)
            call check(error, sweep_rank_ok(r32, r64, RCOMP), "character pf_rank must give competition ranks")
            if (allocated(error)) return
            call pf_rank(v, r32, method="dense")
            call pf_rank(v, r64, method="ordinal")
            call check(error, all(r32 == int(RDENSE, int32)) .and. all(r64 == int(RORD, int64)), &
                "character pf_rank must number classes under dense and every row under ordinal")
        end block
        if (allocated(error)) return
        block
            type(parquet_date) :: v(SWN)
            call v%set_raw(1000_int32 + int(SWDUP, int32))
            call pf_rank(v, r32)
            call pf_rank(v, r64)
            call check(error, sweep_rank_ok(r32, r64, RCOMP), "parquet_date pf_rank must give competition ranks")
            if (allocated(error)) return
            call pf_rank(v, r32, method="dense")
            call pf_rank(v, r64, method="ordinal")
            call check(error, all(r32 == int(RDENSE, int32)) .and. all(r64 == int(RORD, int64)), &
                "parquet_date pf_rank must number classes under dense and every row under ordinal")
        end block
        if (allocated(error)) return
        block
            type(parquet_time) :: v(SWN)
            call v%set_raw(2000_int64 + int(SWDUP, int64))
            call pf_rank(v, r32)
            call pf_rank(v, r64)
            call check(error, sweep_rank_ok(r32, r64, RCOMP), "parquet_time pf_rank must give competition ranks")
            if (allocated(error)) return
            call pf_rank(v, r32, method="dense")
            call pf_rank(v, r64, method="ordinal")
            call check(error, all(r32 == int(RDENSE, int32)) .and. all(r64 == int(RORD, int64)), &
                "parquet_time pf_rank must number classes under dense and every row under ordinal")
        end block
        if (allocated(error)) return
        block
            type(parquet_timestamp) :: v(SWN)
            call v%set_raw(3000_int64 + int(SWDUP, int64), 7_int32)
            call pf_rank(v, r32)
            call pf_rank(v, r64)
            call check(error, sweep_rank_ok(r32, r64, RCOMP), "parquet_timestamp pf_rank must give competition ranks")
            if (allocated(error)) return
            call pf_rank(v, r32, method="dense")
            call pf_rank(v, r64, method="ordinal")
            call check(error, all(r32 == int(RDENSE, int32)) .and. all(r64 == int(RORD, int64)), &
                "parquet_timestamp pf_rank must number classes under dense and every row under ordinal")
        end block
        if (allocated(error)) return
        block
            type(parquet_string_column) :: sc
            do k = 1, SWN
                call sc%append_string(swchr(SWDUP(k)))
            end do
            call pf_rank(sc, r32)
            call pf_rank(sc, r64)
            call check(error, sweep_rank_ok(r32, r64, RCOMP), "parquet_string_column pf_rank must give competition ranks")
            if (allocated(error)) return
            call pf_rank(sc, r32, method="dense")
            call pf_rank(sc, r64, method="ordinal")
            call check(error, all(r32 == int(RDENSE, int32)) .and. all(r64 == int(RORD, int64)), &
                "parquet_string_column pf_rank must number classes under dense and every row under ordinal")
            if (allocated(error)) return
            call pf_rank(sc, r32, descending=.true.)
            call pf_rank(sc, r64, descending=.true.)
            call check(error, sweep_rank_ok(r32, r64, RDESC), &
                "parquet_string_column pf_rank must rank from the top under descending")
        end block
        if (allocated(error)) return
        block
            type(parquet_column) :: c
            call c%init(PK_INT32, int(SWN, int64))
            call c%set_all(10_int32 * int(SWDUP, int32))
            call pf_rank(c, r32)
            call pf_rank(c, r64)
            call check(error, sweep_rank_ok(r32, r64, RCOMP), "parquet_column pf_rank must give competition ranks")
            if (allocated(error)) return
            call pf_rank(c, r32, method="dense")
            call pf_rank(c, r64, method="ordinal")
            call check(error, all(r32 == int(RDENSE, int32)) .and. all(r64 == int(RORD, int64)), &
                "parquet_column pf_rank must number classes under dense and every row under ordinal")
            if (allocated(error)) return
            call pf_rank(c, r32, method="dense", descending=.true.)
            call check(error, all(r32 == [1_int32, 3_int32, 1_int32, 2_int32, 3_int32, 2_int32]), &
                "parquet_column pf_rank must take method and descending together")
            if (allocated(error)) return
            call pf_rank(c, r64, descending=.true.)
            call check(error, all(r64 == int(RDESC, int64)), &
                "an int64 parquet_column rank must take descending too")
            if (allocated(error)) return
            ! "competition" NAMED is a different path from "competition" by omission: the token
            ! resolver returns early when the argument is absent, so its own arm for this token is
            ! reached only when a caller spells it out.
            call pf_rank(c, r32, method="competition")
            call check(error, all(r32 == int(RCOMP, int32)), &
                "naming the default rank method must give what omitting it gives")
        end block
    end subroutine test_rank_every_specific
    !
    !> Both rank arrays against one expected list -- the two kinds are separate specifics, so each
    !> must be checked, and neither may be inferred from the other.
    pure function sweep_rank_ok(r32, r64, want) result(ok)
        integer(int32), allocatable, intent(in) :: r32(:) !! the int32 result.
        integer(int64), allocatable, intent(in) :: r64(:) !! the int64 result.
        integer, intent(in) :: want(:)                    !! the expected rank of each element.
        logical :: ok                                     !! .true. when both match `want` exactly.
        ok = allocated(r32) .and. allocated(r64)
        if (ok) ok = size(r32) == size(want) .and. size(r64) == size(want)
        if (ok) ok = all(r32 == int(want, int32)) .and. all(r64 == int(want, int64))
    end function sweep_rank_ok
    !
    ! ==================================================================================
    ! M3: extremes and merging
    ! ==================================================================================
    !
    !> The oracle is `minval`/`maxval`, which share nothing with the sort engine.
    subroutine test_minmax_basic(error)
        type(error_type), allocatable, intent(out) :: error
        integer(int32) :: v(6) = [30, 10, 50, 20, 60, 40]
        character(len=3) :: s(4) = ["cat", "ant", "dog", "bee"]
        integer(int32) :: lo, hi
        character(len=:), allocatable :: slo, shi

        call pf_minmax(v, lo, hi)
        call check(error, lo == minval(v), "pf_minmax must agree with minval")
        if (allocated(error)) return
        call check(error, hi == maxval(v), "pf_minmax must agree with maxval")
        if (allocated(error)) return
        call pf_minmax(s, slo, shi)
        call check(error, slo == "ant" .and. shi == "dog", &
            "a character array's extremes must be its lexicographic ends")
    end subroutine test_minmax_basic
    !
    !> A NaN is an ordinary value everywhere else in this module, but it is not the minimum or the
    !> maximum of anything -- so both it and every null are skipped here.
    subroutine test_minmax_skips(error)
        type(error_type), allocatable, intent(out) :: error
        real(real64) :: v(5)
        logical :: mask(5) = [.true., .true., .true., .false., .true.]
        real(real64) :: lo, hi

        v(1) = 3.0_real64
        v(2) = ieee_value(1.0_real64, ieee_quiet_nan)
        v(3) = 1.0_real64
        v(4) = 999.0_real64
        v(5) = 5.0_real64
        call pf_minmax(v, lo, hi, is_valid=mask)
        call check(error, lo == 1.0_real64, "a NaN must not be reported as the minimum")
        if (allocated(error)) return
        call check(error, hi == 5.0_real64, &
            "neither a NaN nor a null's stored value may be reported as the maximum")
    end subroutine test_minmax_skips
    !
    !> Ties report the FIRST occurrence at both ends -- the element a full stable sort would place
    !> there, which is what makes the index deterministic rather than whichever one was seen last.
    subroutine test_argminmax_ties(error)
        type(error_type), allocatable, intent(out) :: error
        integer(int32) :: v(6) = [10, 5, 20, 5, 20, 7]
        integer :: i1, i2
        integer(int64) :: j1, j2

        call pf_argminmax(v, i1, i2)
        call check(error, i1 == 2, "argmin must report the first of the tied smallest values")
        if (allocated(error)) return
        call check(error, i2 == 3, "argmax must report the first of the tied largest values")
        if (allocated(error)) return
        call pf_argminmax(v, j1, j2)
        call check(error, int(i1, int64) == j1 .and. int(i2, int64) == j2, &
            "the int32 and int64 argminmax forms must agree")
    end subroutine test_argminmax_ties
    !
    !> `parquet_column` is in `pf_argminmax` but not `pf_minmax`: an index needs no compile-time
    !> element type, a value does. This is the only place that asymmetry is exercised.
    subroutine test_argminmax_column(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_column) :: col
        integer :: i1, i2

        call col%init(PK_FLOAT64, 5_int64)
        call col%set_all([2.5_real64, 9.5_real64, 0.5_real64, 9.5_real64, 4.5_real64])
        call pf_argminmax(col, i1, i2)
        call check(error, i1 == 3, "a column's argmin must be found by its runtime kind")
        if (allocated(error)) return
        call check(error, i2 == 2, "a column's argmax must report the first of its tied maxima")
    end subroutine test_argminmax_column
    !
    !> **The merge oracle**: merging two sorted halves must equal sorting their concatenation, value
    !> for value. That is what a stable merge means, and it needs no hand-written expectation.
    subroutine test_merge_matches_sort(error)
        type(error_type), allocatable, intent(out) :: error
        integer(int32) :: a(4) = [1, 4, 6, 9]
        integer(int32) :: b(5) = [2, 3, 6, 7, 10]
        integer(int32) :: cat(9)
        integer(int32), allocatable :: m(:), s(:)

        cat(1:4) = a
        cat(5:9) = b
        call pf_merge(a, b, m)
        call pf_sort(cat, s)
        call check(error, size(m) == 9, "a merge must return every element of both inputs")
        if (allocated(error)) return
        call check(error, all(m == s), "a merge must equal a sort of the concatenation")
    end subroutine test_merge_matches_sort
    !
    !> Nulls sit last in each already-sorted input, so they end up last in the result -- and
    !> `merged_valid` is allocated even when neither input supplied a mask.
    subroutine test_merge_validity(error)
        type(error_type), allocatable, intent(out) :: error
        integer(int32) :: a(3) = [1, 3, 99]
        integer(int32) :: b(2) = [2, 88]
        logical :: ma(3) = [.true., .true., .false.]
        logical :: mb(2) = [.true., .false.]
        integer(int32), allocatable :: m(:)
        logical, allocatable :: mv(:)

        call pf_merge(a, b, m, is_valid_a=ma, is_valid_b=mb, merged_valid=mv)
        call check(error, all(m(1:3) == [1, 2, 3]), "the non-null values must merge in order")
        if (allocated(error)) return
        ! The two nulls compare EQUAL to each other, so which of them lands first is decided purely
        ! by the tie rule -- and taking from `b` instead would produce a result whose values are
        ! still "sorted" and whose validity mask is still identical. This is the only assertion in
        ! the suite that pins stability for the merge; a mutation of that rule survives without it.
        call check(error, all(m == [1, 2, 3, 99, 88]), &
            "a tie must take from `a` first, so a's null precedes b's")
        if (allocated(error)) return
        call check(error, all(mv .eqv. [.true., .true., .true., .false., .false.]), &
            "merged_valid must mark exactly the rows that came from a null")
        if (allocated(error)) return
        call pf_merge(a, b, m, merged_valid=mv)
        call check(error, allocated(mv) .and. all(mv), &
            "merged_valid must be allocated and all .true. when no input mask was supplied")
    end subroutine test_merge_validity
    !
    !> `descending` selects the comparison, exactly as in the searches -- the inputs must already
    !> be in that order for the result to be.
    subroutine test_merge_descending(error)
        type(error_type), allocatable, intent(out) :: error
        integer(int32) :: a(3) = [9, 5, 1]
        integer(int32) :: b(3) = [8, 4, 2]
        integer(int32), allocatable :: m(:)

        call pf_merge(a, b, m, descending=.true.)
        call check(error, all(m == [9, 8, 5, 4, 2, 1]), &
            "a descending merge must interleave two descending inputs")
    end subroutine test_merge_descending
    !
    !> An empty input contributes nothing and must not shift the other one.
    subroutine test_merge_empty(error)
        type(error_type), allocatable, intent(out) :: error
        integer(int32), allocatable :: empty(:)
        integer(int32) :: b(3) = [2, 4, 6]
        integer(int32), allocatable :: m(:)

        allocate(empty(0))
        call pf_merge(empty, b, m)
        call check(error, size(m) == 3 .and. all(m == b), &
            "merging an empty array must return the other one unchanged")
        if (allocated(error)) return
        call pf_merge(b, empty, m)
        call check(error, size(m) == 3 .and. all(m == b), &
            "an empty second input must be handled the same way")
        if (allocated(error)) return
        call pf_merge(empty, empty, m)
        call check(error, size(m) == 0, "merging two empty arrays must give an empty result")
    end subroutine test_merge_empty
    !
    !> Two `character` arrays of different declared lengths merge into the wider one. Comparing the
    !> packed keys at two different widths would misorder them, so both halves are widened first.
    subroutine test_merge_string_widths(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=3) :: a(2) = ["abc", "xyz"]
        character(len=5) :: b(2) = ["aaaaa", "mmmmm"]
        character(len=:), allocatable :: m(:)

        call pf_merge(a, b, m)
        call check(error, len(m) == 5, "the merged array must be as wide as the wider input")
        if (allocated(error)) return
        call check(error, size(m) == 4, "the merged array must hold every element of both inputs")
        if (allocated(error)) return
        call check(error, all(m == ["aaaaa", "abc  ", "mmmmm", "xyz  "]), &
            "a shorter element must order as if blank-padded to the merged width")
    end subroutine test_merge_string_widths
    !
    ! ----------------------------------------------------------------------------------
    ! Extremes and merge sweeps: one call per generated specific
    ! ----------------------------------------------------------------------------------
    !
    !> **Every `pf_minmax` specific, once each** -- the nine families that have one. `logical` and
    !> `parquet_column` are absent by design and not by omission: a value out-argument needs a
    !> compile-time element type, which the type-erased column has not got, and the extremes of a
    !> `logical` array are not a question worth an API.
    !>
    !> The fixture puts neither extreme at an end, so a body that reported `values(1)` and
    !> `values(SWN)` -- the single most likely way to get this wrong -- fails everywhere.
    subroutine test_minmax_every_specific(error)
        type(error_type), allocatable, intent(out) :: error
        integer :: k

        block
            integer(int32) :: v(SWN), lo, hi
            v = 10_int32 * int(SWRANK, int32)
            call pf_minmax(v, lo, hi)
            call check(error, lo == 10_int32 .and. hi == 60_int32, &
                "int32 pf_minmax must report the interior extremes, not the end elements")
        end block
        if (allocated(error)) return
        block
            integer(int64) :: v(SWN), lo, hi
            v = 100_int64 * int(SWRANK, int64)
            call pf_minmax(v, lo, hi)
            call check(error, lo == 100_int64 .and. hi == 600_int64, &
                "int64 pf_minmax must report the interior extremes, not the end elements")
        end block
        if (allocated(error)) return
        block
            real(real32) :: v(SWN), lo, hi
            v = real(SWRANK, real32) + 0.5_real32
            call pf_minmax(v, lo, hi)
            call check(error, lo == 1.5_real32 .and. hi == 6.5_real32, &
                "real32 pf_minmax must report the interior extremes, not the end elements")
        end block
        if (allocated(error)) return
        block
            real(real64) :: v(SWN), lo, hi
            logical :: mask(SWN)
            v = real(SWRANK, real64) + 0.25_real64
            call pf_minmax(v, lo, hi)
            call check(error, lo == 1.25_real64 .and. hi == 6.25_real64, &
                "real64 pf_minmax must report the interior extremes, not the end elements")
            if (allocated(error)) return
            mask = .true.
            mask(SWASC(1)) = .false.    ! the smallest is a null, so the answer must step up
            mask(SWASC(SWN)) = .false.  ! and the largest too, so it must step down
            call pf_minmax(v, lo, hi, is_valid=mask)
            call check(error, lo == 2.25_real64 .and. hi == 5.25_real64, &
                "real64 pf_minmax must skip a null at either end rather than reporting its value")
        end block
        if (allocated(error)) return
        block
            character(len=2) :: v(SWN)
            character(len=:), allocatable :: lo, hi
            do k = 1, SWN
                v(k) = swchr(SWRANK(k))
            end do
            call pf_minmax(v, lo, hi)
            call check(error, lo == swchr(1) .and. hi == swchr(6), &
                "character pf_minmax must report the lexicographic ends")
        end block
        if (allocated(error)) return
        block
            type(parquet_date) :: v(SWN), lo, hi
            call v%set_raw(1000_int32 + int(SWRANK, int32))
            call pf_minmax(v, lo, hi)
            call check(error, lo%raw() == 1001_int32 .and. hi%raw() == 1006_int32, &
                "parquet_date pf_minmax must report the earliest and latest dates")
        end block
        if (allocated(error)) return
        block
            type(parquet_time) :: v(SWN), lo, hi
            call v%set_raw(2000_int64 + int(SWRANK, int64))
            call pf_minmax(v, lo, hi)
            call check(error, lo%raw() == 2001_int64 .and. hi%raw() == 2006_int64, &
                "parquet_time pf_minmax must report the earliest and latest times")
        end block
        if (allocated(error)) return
        block
            type(parquet_timestamp) :: v(SWN), lo, hi
            integer(int64) :: gs1, gs2
            integer(int32) :: gn1, gn2
            call v%set_raw(3000_int64 + int(SWRANK, int64), 7_int32)
            call pf_minmax(v, lo, hi)
            call lo%get_raw(gs1, gn1)
            call hi%get_raw(gs2, gn2)
            call check(error, gs1 == 3001_int64 .and. gs2 == 3006_int64 .and. gn1 == 7_int32 .and. &
                gn2 == 7_int32, &
                "parquet_timestamp pf_minmax must report the earliest and latest timestamps")
        end block
        if (allocated(error)) return
        block
            type(parquet_string_column) :: sc
            character(len=:), allocatable :: lo, hi
            do k = 1, SWN
                call sc%append_string(swchr(SWRANK(k)))
            end do
            call pf_minmax(sc, lo, hi)
            call check(error, lo == swchr(1) .and. hi == swchr(6), &
                "parquet_string_column pf_minmax must report the lexicographic ends")
        end block
    end subroutine test_minmax_every_specific
    !
    !> **Every `pf_argminmax` specific, once each** -- ten families times the two index kinds.
    !> `parquet_column` is here although `pf_minmax` excludes it, for the reason given on that
    !> generic: an index needs no compile-time element type where a value does.
    !>
    !> The expected answer is `SWASC(1)` and `SWASC(SWN)` read off the fixture, so a body that
    !> returned the extremes' VALUES where indices were asked for, or transposed the two, fails.
    subroutine test_argminmax_every_specific(error)
        type(error_type), allocatable, intent(out) :: error
        integer(int32) :: a32, b32
        integer(int64) :: a64, b64
        integer :: k
        logical :: ok

        block
            integer(int32) :: v(SWN)
            v = 10_int32 * int(SWRANK, int32)
            call pf_argminmax(v, a32, b32)
            call pf_argminmax(v, a64, b64)
            ok = sweep_argmm_ok(a32, b32, a64, b64)
            call check(error, ok, "int32 pf_argminmax must name the extreme elements in both kinds")
        end block
        if (allocated(error)) return
        block
            integer(int64) :: v(SWN)
            v = 100_int64 * int(SWRANK, int64)
            call pf_argminmax(v, a32, b32)
            call pf_argminmax(v, a64, b64)
            call check(error, sweep_argmm_ok(a32, b32, a64, b64), &
                "int64 pf_argminmax must name the extreme elements in both kinds")
        end block
        if (allocated(error)) return
        block
            real(real32) :: v(SWN)
            v = real(SWRANK, real32) + 0.5_real32
            call pf_argminmax(v, a32, b32)
            call pf_argminmax(v, a64, b64)
            call check(error, sweep_argmm_ok(a32, b32, a64, b64), &
                "real32 pf_argminmax must name the extreme elements in both kinds")
        end block
        if (allocated(error)) return
        block
            real(real64) :: v(SWN)
            logical :: mask(SWN)
            v = real(SWRANK, real64) + 0.25_real64
            call pf_argminmax(v, a32, b32)
            call pf_argminmax(v, a64, b64)
            call check(error, sweep_argmm_ok(a32, b32, a64, b64), &
                "real64 pf_argminmax must name the extreme elements in both kinds")
            if (allocated(error)) return
            mask = .true.
            mask(SWASC(1)) = .false.
            call pf_argminmax(v, a32, b32, is_valid=mask)
            call check(error, a32 == int(SWASC(2), int32) .and. b32 == int(SWASC(SWN), int32), &
                "real64 pf_argminmax must skip a null rather than naming the element holding it")
        end block
        if (allocated(error)) return
        block
            character(len=2) :: v(SWN)
            do k = 1, SWN
                v(k) = swchr(SWRANK(k))
            end do
            call pf_argminmax(v, a32, b32)
            call pf_argminmax(v, a64, b64)
            call check(error, sweep_argmm_ok(a32, b32, a64, b64), &
                "character pf_argminmax must name the extreme elements in both kinds")
        end block
        if (allocated(error)) return
        block
            type(parquet_date) :: v(SWN)
            call v%set_raw(1000_int32 + int(SWRANK, int32))
            call pf_argminmax(v, a32, b32)
            call pf_argminmax(v, a64, b64)
            call check(error, sweep_argmm_ok(a32, b32, a64, b64), &
                "parquet_date pf_argminmax must name the extreme elements in both kinds")
        end block
        if (allocated(error)) return
        block
            type(parquet_time) :: v(SWN)
            call v%set_raw(2000_int64 + int(SWRANK, int64))
            call pf_argminmax(v, a32, b32)
            call pf_argminmax(v, a64, b64)
            call check(error, sweep_argmm_ok(a32, b32, a64, b64), &
                "parquet_time pf_argminmax must name the extreme elements in both kinds")
        end block
        if (allocated(error)) return
        block
            type(parquet_timestamp) :: v(SWN)
            call v%set_raw(3000_int64 + int(SWRANK, int64), 7_int32)
            call pf_argminmax(v, a32, b32)
            call pf_argminmax(v, a64, b64)
            call check(error, sweep_argmm_ok(a32, b32, a64, b64), &
                "parquet_timestamp pf_argminmax must name the extreme elements in both kinds")
        end block
        if (allocated(error)) return
        block
            type(parquet_string_column) :: sc
            do k = 1, SWN
                call sc%append_string(swchr(SWRANK(k)))
            end do
            call pf_argminmax(sc, a32, b32)
            call pf_argminmax(sc, a64, b64)
            call check(error, sweep_argmm_ok(a32, b32, a64, b64), &
                "parquet_string_column pf_argminmax must name the extreme elements in both kinds")
        end block
        if (allocated(error)) return
        block
            type(parquet_column) :: c
            call c%init(PK_INT32, int(SWN, int64))
            call c%set_all(10_int32 * int(SWRANK, int32))
            call pf_argminmax(c, a32, b32)
            call pf_argminmax(c, a64, b64)
            call check(error, sweep_argmm_ok(a32, b32, a64, b64), &
                "parquet_column pf_argminmax must name the extreme elements in both kinds")
        end block
    end subroutine test_argminmax_every_specific
    !
    !> **Every `pf_merge` specific, once each** -- the nine families defined on plain arrays.
    !> `parquet_string_column` and `parquet_column` are out because merging is defined on arrays.
    !>
    !> The two inputs strictly INTERLEAVE (odd ranks in `a`, even ranks in `b`), which is what
    !> makes both arms of each body's gather loop run: a merge that took everything from one side
    !> before looking at the other would still be sorted, and would still pass a test whose inputs
    !> did not overlap. The six families taking masks are additionally called with both input masks
    !> and `merged_valid` together, the only shape that reaches their validity-gathering loop.
    subroutine test_merge_every_specific(error)
        type(error_type), allocatable, intent(out) :: error
        !> Each input's LAST element is its null. `pf_merge` merges, it does not sort, so an input
        !> must already be in the order asked for -- including its null tier, which is why a null
        !> cannot sit in the middle of one of these arrays.
        logical, parameter :: MA(3) = [.true., .true., .false.]
        !> `b`'s mask, the same shape, so each of the two null rows comes from a DIFFERENT input --
        !> a body that read one input's mask for both would still produce this pattern from one.
        logical, parameter :: MB(3) = [.true., .true., .false.]
        !> With nulls last, the four valid ranks in order, then a's null, then b's.
        logical, parameter :: MVEXP(SWN) = [.true., .true., .true., .true., .false., .false.]
        integer :: k

        block
            integer(int32) :: a(3), b(3)
            integer(int32), allocatable :: m(:)
            logical, allocatable :: mv(:)
            a = [10_int32, 30_int32, 50_int32]
            b = [20_int32, 40_int32, 60_int32]
            call pf_merge(a, b, m)
            call check(error, all(m == [10_int32, 20_int32, 30_int32, 40_int32, 50_int32, 60_int32]), &
                "int32 pf_merge must interleave two sorted inputs")
            if (allocated(error)) return
            ! The null rows carry ranks 1 and 6, i.e. values that would sort at the two ENDS were
            ! they not null -- so the answer differs from the null-free merge above at every
            ! position, and a body that ignored the masks cannot produce it.
            call pf_merge([30_int32, 50_int32, 10_int32], [20_int32, 40_int32, 60_int32], m, &
                is_valid_a=MA, is_valid_b=MB, merged_valid=mv)
            call check(error, all(mv .eqv. MVEXP) .and. &
                all(m == [20_int32, 30_int32, 40_int32, 50_int32, 10_int32, 60_int32]), &
                "int32 pf_merge must take each null from the input that declared it")
        end block
        if (allocated(error)) return
        block
            integer(int64) :: a(3), b(3)
            integer(int64), allocatable :: m(:)
            logical, allocatable :: mv(:)
            a = [100_int64, 300_int64, 500_int64]
            b = [200_int64, 400_int64, 600_int64]
            call pf_merge(a, b, m)
            call check(error, all(m == [100_int64, 200_int64, 300_int64, 400_int64, 500_int64, 600_int64]), &
                "int64 pf_merge must interleave two sorted inputs")
            if (allocated(error)) return
            call pf_merge([300_int64, 500_int64, 100_int64], [200_int64, 400_int64, 600_int64], m, &
                is_valid_a=MA, is_valid_b=MB, merged_valid=mv)
            call check(error, all(mv .eqv. MVEXP) .and. &
                all(m == [200_int64, 300_int64, 400_int64, 500_int64, 100_int64, 600_int64]), &
                "int64 pf_merge must take each null from the input that declared it")
        end block
        if (allocated(error)) return
        block
            real(real32) :: a(3), b(3)
            real(real32), allocatable :: m(:)
            logical, allocatable :: mv(:)
            a = [1.5_real32, 3.5_real32, 5.5_real32]
            b = [2.5_real32, 4.5_real32, 6.5_real32]
            call pf_merge(a, b, m)
            call check(error, all(m == [1.5_real32, 2.5_real32, 3.5_real32, 4.5_real32, 5.5_real32, 6.5_real32]), &
                "real32 pf_merge must interleave two sorted inputs")
            if (allocated(error)) return
            call pf_merge([3.5_real32, 5.5_real32, 1.5_real32], [2.5_real32, 4.5_real32, 6.5_real32], m, &
                is_valid_a=MA, is_valid_b=MB, merged_valid=mv)
            call check(error, all(mv .eqv. MVEXP) .and. &
                all(m == [2.5_real32, 3.5_real32, 4.5_real32, 5.5_real32, 1.5_real32, 6.5_real32]), &
                "real32 pf_merge must take each null from the input that declared it")
        end block
        if (allocated(error)) return
        block
            real(real64) :: a(3), b(3)
            real(real64), allocatable :: m(:)
            logical, allocatable :: mv(:)
            a = [1.25_real64, 3.25_real64, 5.25_real64]
            b = [2.25_real64, 4.25_real64, 6.25_real64]
            ! descending needs inputs ALREADY in that order -- pf_merge merges, it does not sort.
            call pf_merge(a(3:1:-1), b(3:1:-1), m, descending=.true.)
            call check(error, all(m == [6.25_real64, 5.25_real64, 4.25_real64, 3.25_real64, 2.25_real64, &
                1.25_real64]), "real64 pf_merge must reverse the merged order under descending")
            if (allocated(error)) return
            call pf_merge([3.25_real64, 5.25_real64, 1.25_real64], [2.25_real64, 4.25_real64, 6.25_real64], &
                m, is_valid_a=MA, is_valid_b=MB, merged_valid=mv, assume_sorted=.true.)
            call check(error, all(mv .eqv. MVEXP) .and. &
                all(m == [2.25_real64, 3.25_real64, 4.25_real64, 5.25_real64, 1.25_real64, 6.25_real64]), &
                "real64 pf_merge must give the same answer with the sortedness check skipped")
        end block
        if (allocated(error)) return
        block
            logical :: a(3), b(3)
            logical, allocatable :: m(:), mv(:)
            a = [.false., .false., .true.]
            b = [.false., .true., .true.]
            call pf_merge(a, b, m)
            call check(error, .not. any(m(1:3)) .and. all(m(4:6)), &
                "logical pf_merge must put all three .false. before all three .true.")
            if (allocated(error)) return
            call pf_merge([.false., .true., .false.], [.false., .true., .true.], m, &
                is_valid_a=MA, is_valid_b=MB, merged_valid=mv)
            call check(error, all(mv .eqv. MVEXP) .and. .not. any(m(1:2)) .and. all(m(3:4)), &
                "logical pf_merge must take each null from the input that declared it")
        end block
        if (allocated(error)) return
        block
            character(len=2) :: a(3), b(3)
            character(len=:), allocatable :: m(:)
            logical, allocatable :: mv(:)
            a = [swchr(1), swchr(3), swchr(5)]
            b = [swchr(2), swchr(4), swchr(6)]
            call pf_merge(a, b, m)
            call check(error, all(m == [(swchr(k), k = 1, SWN)]), &
                "character pf_merge must interleave two sorted inputs")
            if (allocated(error)) return
            call pf_merge([swchr(3), swchr(5), swchr(1)], [swchr(2), swchr(4), swchr(6)], m, &
                is_valid_a=MA, is_valid_b=MB, merged_valid=mv)
            call check(error, all(mv .eqv. MVEXP) .and. &
                all(m == [swchr(2), swchr(3), swchr(4), swchr(5), swchr(1), swchr(6)]), &
                "character pf_merge must take each null from the input that declared it")
        end block
        if (allocated(error)) return
        ! ---- the three temporal families, which carry their own nulls and so take no masks ----
        block
            type(parquet_date) :: a(3), b(3)
            type(parquet_date), allocatable :: m(:)
            call a%set_raw([1001_int32, 1003_int32, 1005_int32])
            call b%set_raw([1002_int32, 1004_int32, 1006_int32])
            call pf_merge(a, b, m)
            call check(error, all([(m(k)%raw(), k = 1, SWN)] == [(1000 + k, k = 1, SWN)]), &
                "parquet_date pf_merge must interleave two sorted inputs")
        end block
        if (allocated(error)) return
        block
            type(parquet_time) :: a(3), b(3)
            type(parquet_time), allocatable :: m(:)
            call a%set_raw([2001_int64, 2003_int64, 2005_int64])
            call b%set_raw([2002_int64, 2004_int64, 2006_int64])
            call pf_merge(a, b, m)
            call check(error, all([(m(k)%raw(), k = 1, SWN)] == [(2000_int64 + int(k, int64), k = 1, SWN)]), &
                "parquet_time pf_merge must interleave two sorted inputs")
        end block
        if (allocated(error)) return
        block
            type(parquet_timestamp) :: a(3), b(3)
            type(parquet_timestamp), allocatable :: m(:)
            integer(int64) :: gs(SWN)
            integer(int32) :: gn(SWN)
            call a%set_raw([3001_int64, 3003_int64, 3005_int64], 7_int32)
            call b%set_raw([3002_int64, 3004_int64, 3006_int64], 7_int32)
            call pf_merge(a, b, m)
            do k = 1, SWN
                call m(k)%get_raw(gs(k), gn(k))
            end do
            call check(error, all(gs == [(3000_int64 + int(k, int64), k = 1, SWN)]) .and. all(gn == 7_int32), &
                "parquet_timestamp pf_merge must interleave two sorted inputs")
        end block
    end subroutine test_merge_every_specific
    !
    !> The four argminmax out-arguments against the fixture's own extremes, in both index kinds.
    pure function sweep_argmm_ok(a32, b32, a64, b64) result(ok)
        integer(int32), intent(in) :: a32 !! the int32 argmin.
        integer(int32), intent(in) :: b32 !! the int32 argmax.
        integer(int64), intent(in) :: a64 !! the int64 argmin.
        integer(int64), intent(in) :: b64 !! the int64 argmax.
        logical :: ok                     !! .true. when all four name the fixture's extremes.
        ok = a32 == int(SWASC(1), int32) .and. b32 == int(SWASC(SWN), int32) .and. &
             a64 == int(SWASC(1), int64) .and. b64 == int(SWASC(SWN), int64)
    end function sweep_argmm_ok
    !
    ! ==================================================================================
    ! M4: parallel sorting
    ! ==================================================================================
    !
    !> Arms and zeroes the sort's comparison counter.
    !>
    !> **This is what makes every counting-path A/B test in this file non-vacuous.** Those tests run
    !> one fixture down both sort engines and assert the two agree -- but "both engines" is a claim
    !> about which code ran, and nothing in an equality assertion can see it. Turn the setting the
    !> wrong way round, or stop it reaching C++, and both halves take the SAME path: the comparison
    !> holds trivially and the test passes while testing nothing (feature_risks.md Risk-35).
    !>
    !> The counting path performs exactly zero comparisons by construction, so `0` on one half and
    !> nonzero on the other proves the two halves really diverged.
    subroutine arm_sort_comparisons()
        interface
            subroutine count_cmp(enable) bind(C, name="parquet_debug_set_count_sort_comparisons")
                use iso_c_binding, only : c_int
                integer(c_int), value :: enable !! nonzero arms and zeroes the counter.
            end subroutine count_cmp
        end interface
        call count_cmp(1)
    end subroutine arm_sort_comparisons

    !> Comparisons counted since the last arm_sort_comparisons, then disarms the counter.
    integer(int64) function sort_comparisons() result(n)
        interface
            function got_cmp() bind(C, name="parquet_debug_get_sort_comparisons") result(k)
                use iso_c_binding, only : c_long_long
                integer(c_long_long) :: k !! comparisons since the counter was armed.
            end function got_cmp
            subroutine count_cmp(enable) bind(C, name="parquet_debug_set_count_sort_comparisons")
                use iso_c_binding, only : c_int
                integer(c_int), value :: enable
            end subroutine count_cmp
        end interface
        n = int(got_cmp(), int64)
        call count_cmp(0)
    end function sort_comparisons

    !> Lowers the row count below which threading is refused, so a test-sized array can reach the
    !> parallel path at all. Every fixture here is orders of magnitude below the real threshold.
    subroutine force_parallel_threshold(rows)
        integer(int64), intent(in) :: rows !! new threshold; 0 restores the built-in one.

        call parquet_set_sort_parallel_min_rows(rows)
    end subroutine force_parallel_threshold
    !
    !> How many threads the last sort actually put to work, the calling thread included.
    function threads_used() result(n)
        integer(int64) :: n !! 1 means the sort ran serially.
        interface
            function get_used() bind(C, name="parquet_debug_get_sort_threads_used") result(k)
                use iso_c_binding, only : c_int64_t
                integer(c_int64_t) :: k !! threads used by the last threaded build.
            end function get_used
        end interface
        n = int(get_used(), int64)
    end function threads_used
    !
    !> Builds the fixture every test below shares: **heavy ties in the FULL key, and a key family
    !> the counting fast path declines.** Both halves were arrived at by mutation.
    !>
    !> Ties matter because a merge defect is only visible where two rows compare equal -- with the
    !> tiebreaker-free comparator substituted into the merge, an all-distinct fixture and a
    !> near-unique multi-key fixture both still passed while three tied ones failed.
    !>
    !> The key family matters because `sort_counting_candidate` keys on the value RANGE, not on
    !> cardinality: 300 distinct integers under 4M still take the counting path, which performs zero
    !> comparisons and spawns nothing. A real key declines it outright.
    subroutine ties_fixture(v)
        real(real64), intent(out) :: v(:) !! heavily tied real values.
        integer :: k
        do k = 1, size(v)
            v(k) = real(mod(k * 7919, 97), real64)
        end do
    end subroutine ties_fixture
    !
    !> **The identity oracle, and the reason `threads=` is safe at all.** `SortRowLess` ends with a
    !> tiebreaker on the row index, making it a total order with no ties, so every correct sorting
    !> algorithm -- serial, threaded, or both -- must produce the SAME permutation. Any thread count
    !> that disagreed with the serial answer would be a defect, not a variation.
    subroutine test_threads_identical(error)
        type(error_type), allocatable, intent(out) :: error
        real(real64) :: v(2000)
        logical :: mask(2000)
        integer(int32), allocatable :: ser(:), par(:)
        integer :: t, k

        call ties_fixture(v)
        do k = 1, 2000
            mask(k) = mod(k, 13) /= 0
        end do
        call force_parallel_threshold(1000000000_int64)   ! above the size -> serial reference
        call pf_argsort(v, ser, is_valid=mask)
        call force_parallel_threshold(4_int64)            ! below the size -> parallel
        do t = 2, 8
            call pf_argsort(v, par, is_valid=mask, threads=t)
            call check(error, size(par) == size(ser), "a threaded argsort must return every index")
            if (allocated(error)) exit
            call check(error, all(par == ser), &
                "a threaded permutation must be identical to the serial one, at every thread count")
            if (allocated(error)) exit
        end do
        call force_parallel_threshold(0_int64)
    end subroutine test_threads_identical
    !
    !> **Without this the whole feature is untestable.** A `threads=` that is silently ignored
    !> returns the serial permutation, which is CORRECT -- so every assertion above passes just as
    !> happily against an implementation that never spawns anything. Zero parallelism is a passing
    !> test, exactly as zero comparisons was for the partial sort (`feature_risks.md` Risk-35).
    subroutine test_threads_really_used(error)
        type(error_type), allocatable, intent(out) :: error
        real(real64) :: v(2000)
        integer(int32), allocatable :: perm(:)

        call ties_fixture(v)
        call force_parallel_threshold(4_int64)
        call pf_argsort(v, perm, threads=4)
        call check(error, threads_used() == 4_int64, &
            "asking for 4 threads must actually put 4 threads to work")
        if (allocated(error)) then
            call force_parallel_threshold(0_int64)
            return
        end if
        ! The negative control: below the threshold nothing threads, however many were asked for.
        call force_parallel_threshold(1000000000_int64)
        call pf_argsort(v, perm, threads=4)
        call check(error, threads_used() == 1_int64, &
            "an array below the minimum-work threshold must sort serially whatever was asked for")
        call force_parallel_threshold(0_int64)
    end subroutine test_threads_really_used
    !
    !> **The rule nothing else observes**, and the one that keeps the rest of this test suite from
    !> oversubscribing: with `threads=` absent, auto takes the machine in a serial region and stays
    !> SERIAL inside a parallel one, because T OpenMP threads each asking for T more would be T*T
    !> threads. An explicit `threads=` is still honoured there -- the caller has said what they want.
    subroutine test_threads_auto_in_parallel(error)
        type(error_type), allocatable, intent(out) :: error
        real(real64) :: v(2000)
        integer(int32), allocatable :: perm(:)
        integer(int64) :: auto_outside, auto_inside, explicit_inside

        call ties_fixture(v)
        call force_parallel_threshold(4_int64)
        call pf_argsort(v, perm)
        auto_outside = threads_used()
        auto_inside = -1_int64
        explicit_inside = -1_int64
        !$omp parallel
        !$omp single
        block
            integer(int32), allocatable :: p2(:)
            call pf_argsort(v, p2)
            auto_inside = threads_used()
            call pf_argsort(v, p2, threads=3)
            explicit_inside = threads_used()
        end block
        !$omp end single
        !$omp end parallel
        call force_parallel_threshold(0_int64)
        call check(error, auto_inside == 1_int64, &
            "auto must resolve to serial inside an OpenMP parallel region")
        if (allocated(error)) return
        call check(error, explicit_inside == 3_int64, &
            "an explicit threads= must still be honoured inside a parallel region")
        if (allocated(error)) return
        ! Guards the test itself: if auto were serial everywhere, the assertion above would pass
        ! while proving nothing about the parallel-region rule.
        call check(error, auto_outside >= 1_int64, "auto outside a parallel region must resolve")
    end subroutine test_threads_auto_in_parallel
    !
    !> `threads=1` is the documented way to turn parallelism off, now that absence means auto.
    subroutine test_threads_one_is_serial(error)
        type(error_type), allocatable, intent(out) :: error
        real(real64) :: v(2000)
        integer(int32), allocatable :: perm(:), ref(:)

        call ties_fixture(v)
        call force_parallel_threshold(4_int64)
        call pf_argsort(v, perm, threads=1)
        call check(error, threads_used() == 1_int64, "threads=1 must sort serially")
        if (allocated(error)) then
            call force_parallel_threshold(0_int64)
            return
        end if
        call pf_argsort(v, ref, threads=8)
        call force_parallel_threshold(0_int64)
        call check(error, all(perm == ref), "threads=1 and threads=8 must agree element for element")
    end subroutine test_threads_one_is_serial
    !
    !> `threads=` reaches the three operations that sort internally, not just the two that are a
    !> sort -- and their answers must not change either.
    subroutine test_threads_on_derived(error)
        type(error_type), allocatable, intent(out) :: error
        real(real64) :: v(2000)
        real(real64), allocatable :: d1(:), d2(:)
        integer, allocatable :: r1(:), r2(:)
        integer :: c1, c2

        call ties_fixture(v)
        call force_parallel_threshold(1000000000_int64)
        call pf_unique_count(v, c1)
        call pf_unique(v, d1)
        call pf_rank(v, r1)
        call force_parallel_threshold(4_int64)
        call pf_unique_count(v, c2, threads=8)
        call pf_unique(v, d2, threads=8)
        call pf_rank(v, r2, threads=8)
        call force_parallel_threshold(0_int64)
        call check(error, c1 == c2 .and. c1 == 97, "a threaded pf_unique_count must count the same")
        if (allocated(error)) return
        call check(error, size(d1) == size(d2) .and. all(d1 == d2), &
            "a threaded pf_unique must return the same distinct values")
        if (allocated(error)) return
        call check(error, all(r1 == r2), "a threaded pf_rank must produce the same ranks")
    end subroutine test_threads_on_derived
    !
    !> Shrinks the smallest output range the co-ranked merge gives its own thread, so a test-sized
    !! array reaches the co-rank at all; 0 restores the real floor.
    !!
    !! **Every test below that sorts fewer than ~32000 elements is worthless without this**, and
    !! silently so. The real floor is 16384, and a pair shorter than twice that is merged in one
    !! piece — which is exactly the old, correct, single-threaded merge. So a dense sweep over small
    !! arrays exercises the path this feature *replaced*, calls the co-rank zero times, and passes.
    !! Found by mutation, not by reasoning: two deliberate co-rank defects survived the entire suite
    !! until the sweeps started calling this. `feature_risks.md` Risk-49.
    subroutine force_merge_segments(min_segment)
        integer(int64), intent(in) :: min_segment !! new floor in elements; 0 restores the built-in one.
        interface
            subroutine set_min_seg(n) bind(C, name="parquet_debug_set_sort_merge_min_segment")
                use iso_c_binding, only : c_int64_t
                integer(c_int64_t), value :: n !! elements; <= 0 restores the real floor.
            end subroutine set_min_seg
        end interface
        call set_min_seg(int(min_segment, int64))
    end subroutine force_merge_segments
    !
    !> How many threads worked the last sort's FINAL merge round, the calling thread included.
    function merge_threads_used() result(n)
        integer(int64) :: n !! 1 means that round ran on one thread.
        interface
            function get_used() bind(C, name="parquet_debug_get_sort_merge_threads_used") result(k)
                use iso_c_binding, only : c_int64_t
                integer(c_int64_t) :: k !! threads that worked the final merge round.
            end function get_used
        end interface
        n = int(get_used(), int64)
    end function merge_threads_used
    !
    !> **The single highest-value test of the co-ranked merge**, and the reason it is a *dense* sweep
    !! rather than a handful of round numbers.
    !!
    !! Co-ranking splits each merge by binary search, and the classic defect it invites is an
    !! off-by-one that shows up at exactly one array size and passes at every neighbouring one --
    !! a boundary that lands one element early only matters when some run happens to end there. So
    !! this asserts the identity oracle at EVERY size from 2 to 400 and at every thread count from 2
    !! to 8, then at a scattering of larger and deliberately awkward sizes (primes, powers of two and
    !! their neighbours, exact multiples of the thread count) that reach merge rounds the small sizes
    !! never do.
    !!
    !! The oracle itself is the one the whole feature rests on: `SortRowLess` ends with a tiebreaker
    !! on the row index, so it is a total order in which no two rows compare equal, and every correct
    !! sorting algorithm must therefore produce the identical permutation. A threaded answer that
    !! differs from the serial one at any size or thread count is a defect, never a variation.
    !!
    !! ~2800 sorts of trivially small arrays, well under a second, and a decided permanent cost: the
    !! density IS the test, so do not narrow the range to make it faster.
    subroutine test_merge_size_sweep(error)
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: nbig = 12
        !> Sizes past the dense range, chosen to be awkward: primes, powers of two and their
        !! neighbours, and exact multiples of a thread count.
        integer, parameter :: big(nbig) = [401, 511, 512, 513, 1021, 1024, 1025, 2048, 3000, 4093, 5000, 8192]
        real(real64), allocatable :: v(:)
        integer(int32), allocatable :: ser(:), par(:)
        integer :: n, t, k

        do n = 2, 400
            allocate(v(n))
            call ties_fixture(v)
            call force_merge_segments(1_int64)   ! every pair segments, however small
        call force_parallel_threshold(1000000000_int64)  ! above the size -> serial reference
            call pf_argsort(v, ser)
            call force_parallel_threshold(2_int64)           ! below the size -> parallel
            do t = 2, 8
                call pf_argsort(v, par, threads=t)
                call check(error, size(par) == n .and. all(par == ser), &
                    "a co-ranked merge must equal the serial permutation at n="//itoa(n)// &
                    " threads="//itoa(t))
                if (allocated(error)) exit
            end do
            deallocate(v)
            if (allocated(error)) exit
        end do
        if (allocated(error)) then
            call force_merge_segments(0_int64)
        call force_parallel_threshold(0_int64)
            return
        end if
        do k = 1, nbig
            allocate(v(big(k)))
            call ties_fixture(v)
            call force_parallel_threshold(1000000000_int64)
            call pf_argsort(v, ser)
            call force_parallel_threshold(2_int64)
            do t = 2, 8
                call pf_argsort(v, par, threads=t)
                call check(error, size(par) == big(k) .and. all(par == ser), &
                    "a co-ranked merge must equal the serial permutation at n="//itoa(big(k))// &
                    " threads="//itoa(t))
                if (allocated(error)) exit
            end do
            deallocate(v)
            if (allocated(error)) exit
        end do
        call force_merge_segments(0_int64)
        call force_parallel_threshold(0_int64)
    end subroutine test_merge_size_sweep
    !
    !> **Not redundant with the sweep above**, and the sharper of the two where a segment goes wrong.
    !!
    !! The co-ranked segments are a partition of both input runs only because each boundary satisfies
    !! `i + j == k`. A boundary that drifts makes two segments overlap or leave a gap, so some row
    !! index is written twice and another not at all -- and the result stops being a permutation.
    !! **Nothing on the raw path would notice**: `pf_argsort` hands its answer straight to the caller,
    !! and `pf_permute(..., assume_valid=.true.)` is documented as the way to skip validation for
    !! exactly such a permutation. (`%sort_by` would be caught, by the one remaining `%reindex`
    !! validation -- `feature_risks.md` Risk-46 -- but that is the other path.)
    !!
    !! So this asserts the property directly rather than through the oracle: every index in 1..n
    !! appears exactly once.
    subroutine test_merge_sweep_is_permutation(error)
        type(error_type), allocatable, intent(out) :: error
        real(real64), allocatable :: v(:)
        integer(int32), allocatable :: perm(:)
        logical, allocatable :: seen(:)
        integer :: n, t, k

        call force_merge_segments(1_int64)   ! every pair segments, however small
        call force_parallel_threshold(2_int64)
        do n = 2, 400
            allocate(v(n), seen(n))
            call ties_fixture(v)
            do t = 2, 8
                call pf_argsort(v, perm, threads=t)
                seen = .false.
                do k = 1, n
                    if (perm(k) < 1 .or. perm(k) > n) exit
                    if (seen(perm(k))) exit
                    seen(perm(k)) = .true.
                end do
                call check(error, all(seen), &
                    "a co-ranked merge must return each index exactly once at n="//itoa(n)// &
                    " threads="//itoa(t))
                if (allocated(error)) exit
            end do
            deallocate(v, seen)
            if (allocated(error)) exit
        end do
        call force_merge_segments(0_int64)
        call force_parallel_threshold(0_int64)
    end subroutine test_merge_sweep_is_permutation
    !
    !> **Without this the co-ranked merge is untestable, exactly as `threads=` itself was.** A merge
    !! that quietly stopped splitting its final round would still return the identical permutation --
    !! that identity is what makes every thread count safe -- so every assertion above passes just as
    !! happily against the old single-threaded tail. `feature_risks.md` Risk-49.
    !!
    !! The final round is asked about specifically, not the maximum over rounds: a merge that
    !! co-ranked only its first round would report a high maximum while leaving the whole O(n) tail
    !! in place, which is the thing this work exists to remove.
    !!
    !! `merge_threads_used` is a different counter from `threads_used`, which still means phase 1's
    !! chunk-sort thread count. Both are asserted here, so a future change that collapsed them into
    !! one would fail rather than silently answer the wrong question.
    subroutine test_merge_round_threads_used(error)
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: n = 200000
        real(real64), allocatable :: v(:)
        integer(int32), allocatable :: perm(:)

        allocate(v(n))
        call ties_fixture(v)
        ! Segments have a minimum size, so the array has to be big enough for the final round to be
        ! worth splitting at all -- a 2000-element fixture would legitimately report one thread.
        call force_parallel_threshold(4_int64)
        call pf_argsort(v, perm, threads=4)
        call check(error, threads_used() == 4_int64, "phase 1 must still put 4 threads to work")
        if (allocated(error)) then
            call force_parallel_threshold(0_int64)
            return
        end if
        call check(error, merge_threads_used() > 1_int64, &
            "the final merge round must be co-ranked across more than one thread")
        if (allocated(error)) then
            call force_parallel_threshold(0_int64)
            return
        end if
        ! Two negative controls, because a hook that always answered "4" would pass the assertion
        ! above. Below the minimum-work threshold nothing threads at all...
        call force_parallel_threshold(1000000000_int64)
        call pf_argsort(v, perm, threads=4)
        call check(error, merge_threads_used() == 1_int64, &
            "an array below the minimum-work threshold must report no co-ranked merge")
        if (allocated(error)) then
            call force_parallel_threshold(0_int64)
            return
        end if
        ! ...and threads=1 is serial however large the array is.
        call force_parallel_threshold(4_int64)
        call pf_argsort(v, perm, threads=1)
        call check(error, merge_threads_used() == 1_int64, &
            "threads=1 must report no co-ranked merge")
        call force_parallel_threshold(0_int64)
    end subroutine test_merge_round_threads_used
    !
    !> The inputs that drive co-ranking to its ends, where a binary search that is one step out
    !! stops being harmless.
    !!
    !! * **Already sorted**: every merge consumes its whole left run before touching the right, so
    !!   each boundary sits at `j == 0` or `i == nA`.
    !! * **Reverse sorted**: the mirror image, every boundary at the other extreme.
    !! * **All values equal**: every comparison is a tie in the user's key, resolved only by the row
    !!   index. This is the fixture that pins the left-wins-ties rule inside the co-rank predicate --
    !!   with ties everywhere, taking from the wrong side reorders equal rows and the permutation
    !!   stops matching the serial one.
    !! * **One extreme at each end**: a single value that must travel the whole way across.
    subroutine test_merge_corank_extremes(error)
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: n = 5000
        real(real64) :: v(n)
        integer(int32), allocatable :: ser(:), par(:)
        integer :: shape_id, k, t

        do shape_id = 1, 4
            select case (shape_id)
            case (1)
                do k = 1, n
                    v(k) = real(k, real64)                      ! already sorted
                end do
            case (2)
                do k = 1, n
                    v(k) = real(n - k, real64)                  ! reverse sorted
                end do
            case (3)
                v = 1.0_real64                                  ! every comparison a tie
            case (4)
                v = 5.0_real64
                v(1) = 9.0_real64                               ! must travel to the end
                v(n) = -9.0_real64                              ! must travel to the front
            end select
            call force_merge_segments(1_int64)   ! every pair segments, however small
        call force_parallel_threshold(1000000000_int64)
            call pf_argsort(v, ser)
            call force_parallel_threshold(2_int64)
            do t = 2, 8
                call pf_argsort(v, par, threads=t)
                call check(error, all(par == ser), &
                    "co-ranking must equal the serial permutation on extreme shape "// &
                    itoa(shape_id)//" at threads="//itoa(t))
                if (allocated(error)) exit
            end do
            if (allocated(error)) exit
        end do
        call force_merge_segments(0_int64)
        call force_parallel_threshold(0_int64)
    end subroutine test_merge_corank_extremes
    !
    !> The merge is generic over key family, but only one of the three is arithmetic. A `character`
    !! key compares through `std::string_view::compare` and a multi-key walks several keys per
    !! comparison, so both reach `sort_compare_key` differently from a `real(real64)` -- and the
    !! co-rank predicate calls the very same comparator, so a family that broke it would break here
    !! and nowhere in the sweep above.
    subroutine test_merge_key_families(error)
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: n = 4000
        character(len=8) :: s(n)
        real(real64) :: a(n)
        integer(int32) :: b(n)
        type(pf_sort_keys) :: keys
        integer(int32), allocatable :: ser(:), par(:)
        integer :: k, t

        do k = 1, n
            write(s(k), '(i8.8)') mod(k * 7919, 137)   ! heavy ties, lexicographic order
            a(k) = real(mod(k * 7919, 53), real64)
            b(k) = int(mod(k * 104729, 17), int32)
        end do
        call force_merge_segments(1_int64)   ! every pair segments, however small
        call force_parallel_threshold(1000000000_int64)
        call pf_argsort(s, ser)
        call force_parallel_threshold(2_int64)
        do t = 2, 8
            call pf_argsort(s, par, threads=t)
            call check(error, all(par == ser), &
                "a co-ranked merge over a character key must equal the serial one at threads="//itoa(t))
            if (allocated(error)) exit
        end do
        if (allocated(error)) then
            call force_merge_segments(0_int64)
        call force_parallel_threshold(0_int64)
            return
        end if
        call keys%add(a)
        call keys%add(b, descending=.true.)
        call force_parallel_threshold(1000000000_int64)
        call pf_argsort(keys, ser)
        call force_parallel_threshold(2_int64)
        do t = 2, 8
            call pf_argsort(keys, par, threads=t)
            call check(error, all(par == ser), &
                "a co-ranked merge over two keys must equal the serial one at threads="//itoa(t))
            if (allocated(error)) exit
        end do
        call force_merge_segments(0_int64)
        call force_parallel_threshold(0_int64)
    end subroutine test_merge_key_families
    !
    !> Nulls and NaNs sit in tiers of their own, and a tier boundary is exactly where two merged runs
    !! meet: the left run may end in values while the right begins in NaNs, so a co-rank probe lands
    !! on a comparison between tiers rather than between numbers. Both null placements are covered,
    !! because `nulls_first` moves the tier the probe straddles.
    subroutine test_merge_tiers(error)
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: n = 6000
        real(real64) :: v(n)
        logical :: mask(n)
        integer(int32), allocatable :: ser(:), par(:)
        integer :: k, t, variant
        logical :: nf

        do k = 1, n
            v(k) = real(mod(k * 7919, 41), real64)
            if (mod(k, 7) == 0) v(k) = ieee_value(1.0_real64, ieee_quiet_nan)
            mask(k) = mod(k, 11) /= 0
        end do
        do variant = 1, 2
            nf = (variant == 2)
            call force_merge_segments(1_int64)   ! every pair segments, however small
        call force_parallel_threshold(1000000000_int64)
            call pf_argsort(v, ser, is_valid=mask, nulls_first=nf)
            call force_parallel_threshold(2_int64)
            do t = 2, 8
                call pf_argsort(v, par, is_valid=mask, nulls_first=nf, threads=t)
                call check(error, all(par == ser), &
                    "a co-ranked merge must equal the serial one across null/NaN tiers, "// &
                    "nulls_first variant "//itoa(variant)//" at threads="//itoa(t))
                if (allocated(error)) exit
            end do
            if (allocated(error)) exit
        end do
        call force_merge_segments(0_int64)
        call force_parallel_threshold(0_int64)
    end subroutine test_merge_tiers
    !
    !> Decimal text for a small integer, so a sweep's failure message can name the size and thread
    !! count that actually failed -- without one, a dense sweep reports a line number and nothing else.
    function itoa(k) result(s)
        integer, intent(in) :: k           !! value to render.
        character(len=:), allocatable :: s !! decimal text.
        character(len=16) :: buf
        write(buf, '(i0)') k
        s = trim(buf)
    end function itoa
    !
    !> `pf_permute` over a `parquet_column` HONOURS `assume_valid`, routing to `%reindex_trusted`.
    !!
    !! **The proof has to be a permutation that is not one.** A valid permutation behaves
    !! identically whether the check runs or not, so asserting "same result" would pass just as
    !! happily against an implementation that still validates — which is what this generic did
    !! until the trusted path existed. Handing it a duplicate-bearing index array is the only
    !! observation that separates the two: with the check it aborts, without it the gather is
    !! defined and simply repeats an element.
    !!
    !! The `parquet_string_column` half is checked too, because it has its own second validation
    !! one level down and would otherwise keep scanning while the outer one skipped.
    subroutine test_permute_column_assume_valid(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_column) :: c
        type(parquet_string_column) :: sc
        integer(int64) :: dup(3) = [2_int64, 2_int64, 3_int64]
        integer(int32) :: got
        character(len=:), allocatable :: s
        !
        ! Not a permutation: index 2 twice, index 1 never. With the scan this aborts the process;
        ! trusted, it gathers rows 2, 2, 3.
        call c%init(PK_INT32, 3_int64)
        call c%set_all([10_int32, 20_int32, 30_int32])
        call pf_permute(c, dup, assume_valid=.true.)
        call c%get_at(1_int64, got)
        call check(error, got == 20_int32, "a trusted permute must gather row 2 into row 1")
        if (allocated(error)) return
        call c%get_at(2_int64, got)
        call check(error, got == 20_int32, "a trusted permute must repeat the duplicated index rather than abort")
        if (allocated(error)) return
        !
        call sc%append_string("aa")
        call sc%append_string("bb")
        call sc%append_string("cc")
        call pf_permute(sc, dup, assume_valid=.true.)
        call sc%get(1_int64, s)
        call check(error, s == "bb", "the string store must skip its own scan under assume_valid too")
        if (allocated(error)) return
        call sc%get(2_int64, s)
        call check(error, s == "bb", "a trusted string permute must repeat the duplicated index")
        if (allocated(error)) return
        !
        ! Both container types again through their INT32 permutation specifics, which are separate
        ! bodies: the int64 calls above say nothing about whether those two forward assume_valid.
        block
            type(parquet_column) :: c32
            type(parquet_string_column) :: sc32
            integer(int32) :: dup32(3) = [2_int32, 2_int32, 3_int32]
            call c32%init(PK_INT32, 3_int64)
            call c32%set_all([10_int32, 20_int32, 30_int32])
            call pf_permute(c32, dup32, assume_valid=.true.)
            call c32%get_at(1_int64, got)
            call check(error, got == 20_int32, &
                "an int32 trusted permute must gather row 2 into row 1 without its own scan")
            if (allocated(error)) return
            call sc32%append_string("aa")
            call sc32%append_string("bb")
            call sc32%append_string("cc")
            call pf_permute(sc32, dup32, assume_valid=.true.)
            call sc32%get(2_int64, s)
            call check(error, s == "bb", &
                "an int32 trusted string permute must repeat the duplicated index")
        end block
    end subroutine test_permute_column_assume_valid
    !
    !> **Every `pf_is_sorted` specific, once each** -- eleven value families plus `pf_sort_keys`.
    !> Each is asked twice, about a sorted fixture and about a scrambled one, because a body that
    !> answered a constant would pass a one-sided test: `.true.` always passes "is this sorted
    !> input sorted?", and `.false.` always passes the converse.
    subroutine test_is_sorted_every_specific(error)
        type(error_type), allocatable, intent(out) :: error
        logical :: yes, no
        integer :: k

        block
            integer(int32) :: srt(SWN), scr(SWN)
            srt = 10_int32 * int(SWSRT, int32)
            scr = 10_int32 * int(SWRANK, int32)
            call pf_is_sorted(srt, yes)
            call pf_is_sorted(scr, no)
            call check(error, yes .and. .not. no, "int32 pf_is_sorted must tell the two apart")
        end block
        if (allocated(error)) return
        block
            integer(int64) :: srt(SWN), scr(SWN)
            srt = 100_int64 * int(SWSRT, int64)
            scr = 100_int64 * int(SWRANK, int64)
            call pf_is_sorted(srt, yes)
            call pf_is_sorted(scr, no)
            call check(error, yes .and. .not. no, "int64 pf_is_sorted must tell the two apart")
        end block
        if (allocated(error)) return
        block
            real(real32) :: srt(SWN), scr(SWN)
            srt = real(SWSRT, real32) + 0.5_real32
            scr = real(SWRANK, real32) + 0.5_real32
            call pf_is_sorted(srt, yes)
            call pf_is_sorted(scr, no)
            call check(error, yes .and. .not. no, "real32 pf_is_sorted must tell the two apart")
        end block
        if (allocated(error)) return
        block
            real(real64) :: srt(SWN), scr(SWN)
            logical :: mask(SWN)
            srt = real(SWSRT, real64) + 0.25_real64
            scr = real(SWRANK, real64) + 0.25_real64
            call pf_is_sorted(srt, yes)
            call pf_is_sorted(scr, no)
            call check(error, yes .and. .not. no, "real64 pf_is_sorted must tell the two apart")
            if (allocated(error)) return
            ! A null belongs at the end by default, so marking the FIRST element null makes an
            ! otherwise sorted array unsorted -- the tier, not the value, decides.
            mask = .true.
            mask(1) = .false.
            call pf_is_sorted(srt, no, is_valid=mask)
            call pf_is_sorted(srt, yes, is_valid=mask, nulls_first=.true.)
            call check(error, yes .and. .not. no, &
                "real64 pf_is_sorted must place the null tier by nulls_first, not by its value")
        end block
        if (allocated(error)) return
        block
            call pf_is_sorted(SWSRTB, yes)
            call pf_is_sorted(SWBOOL, no)
            call check(error, yes .and. .not. no, "logical pf_is_sorted must tell the two apart")
        end block
        if (allocated(error)) return
        block
            character(len=2) :: srt(SWN), scr(SWN)
            do k = 1, SWN
                srt(k) = swchr(SWSRT(k))
                scr(k) = swchr(SWRANK(k))
            end do
            call pf_is_sorted(srt, yes)
            call pf_is_sorted(scr, no)
            call check(error, yes .and. .not. no, "character pf_is_sorted must tell the two apart")
        end block
        if (allocated(error)) return
        block
            type(parquet_date) :: srt(SWN), scr(SWN)
            call srt%set_raw(1000_int32 + int(SWSRT, int32))
            call scr%set_raw(1000_int32 + int(SWRANK, int32))
            call pf_is_sorted(srt, yes)
            call pf_is_sorted(scr, no)
            call check(error, yes .and. .not. no, "parquet_date pf_is_sorted must tell the two apart")
        end block
        if (allocated(error)) return
        block
            type(parquet_time) :: srt(SWN), scr(SWN)
            call srt%set_raw(2000_int64 + int(SWSRT, int64))
            call scr%set_raw(2000_int64 + int(SWRANK, int64))
            call pf_is_sorted(srt, yes)
            call pf_is_sorted(scr, no)
            call check(error, yes .and. .not. no, "parquet_time pf_is_sorted must tell the two apart")
        end block
        if (allocated(error)) return
        block
            type(parquet_timestamp) :: srt(SWN), scr(SWN)
            call srt%set_raw(3000_int64 + int(SWSRT, int64), 7_int32)
            call scr%set_raw(3000_int64 + int(SWRANK, int64), 7_int32)
            call pf_is_sorted(srt, yes)
            call pf_is_sorted(scr, no)
            call check(error, yes .and. .not. no, &
                "parquet_timestamp pf_is_sorted must tell the two apart")
        end block
        if (allocated(error)) return
        block
            type(parquet_string_column) :: srt, scr
            do k = 1, SWN
                call srt%append_string(swchr(SWSRT(k)))
                call scr%append_string(swchr(SWRANK(k)))
            end do
            call pf_is_sorted(srt, yes)
            call pf_is_sorted(scr, no)
            call check(error, yes .and. .not. no, &
                "parquet_string_column pf_is_sorted must tell the two apart")
        end block
        if (allocated(error)) return
        block
            type(parquet_column) :: srt, scr
            call srt%init(PK_INT32, int(SWN, int64))
            call srt%set_all(10_int32 * int(SWSRT, int32))
            call scr%init(PK_INT32, int(SWN, int64))
            call scr%set_all(10_int32 * int(SWRANK, int32))
            call pf_is_sorted(srt, yes)
            call pf_is_sorted(scr, no)
            call check(error, yes .and. .not. no, "parquet_column pf_is_sorted must tell the two apart")
        end block
        if (allocated(error)) return
        block
            type(pf_sort_keys) :: ksrt, kscr
            call ksrt%add(10_int32 * int(SWSRT, int32))
            call kscr%add(10_int32 * int(SWRANK, int32))
            call pf_is_sorted(ksrt, yes)
            call pf_is_sorted(kscr, no)
            call check(error, yes .and. .not. no, "pf_sort_keys pf_is_sorted must tell the two apart")
        end block
    end subroutine test_is_sorted_every_specific
    !
    !> **Every `pf_sort_keys%add` specific, once each** -- all eleven value families. The multi-key
    !> tests elsewhere in this suite build their keys from two or three families only, so most of
    !> these bodies (each naming its own extractor, exactly as the one-shot entry points do) were
    !> reached by nothing.
    !>
    !> One key per builder, so the expected permutation is the fixture's own `SWASC` and a body
    !> that extracted the wrong array shows up as a wrong order rather than as a tie.
    subroutine test_keys_add_every_specific(error)
        type(error_type), allocatable, intent(out) :: error
        integer(int32), allocatable :: perm(:)
        integer :: k

        block
            type(pf_sort_keys) :: keys
            call keys%add(10_int32 * int(SWRANK, int32))
            call pf_argsort(keys, perm)
            call check(error, all(perm == int(SWASC, int32)), "pf_sort_keys%add must take an int32 key")
        end block
        if (allocated(error)) return
        block
            type(pf_sort_keys) :: keys
            call keys%add(100_int64 * int(SWRANK, int64))
            call pf_argsort(keys, perm)
            call check(error, all(perm == int(SWASC, int32)), "pf_sort_keys%add must take an int64 key")
        end block
        if (allocated(error)) return
        block
            type(pf_sort_keys) :: keys
            call keys%add(real(SWRANK, real32) + 0.5_real32)
            call pf_argsort(keys, perm)
            call check(error, all(perm == int(SWASC, int32)), "pf_sort_keys%add must take a real32 key")
        end block
        if (allocated(error)) return
        block
            type(pf_sort_keys) :: keys
            logical :: mask(SWN)
            call keys%add(real(SWRANK, real64) + 0.25_real64)
            call pf_argsort(keys, perm)
            call check(error, all(perm == int(SWASC, int32)), "pf_sort_keys%add must take a real64 key")
            if (allocated(error)) return
            ! %add's own descending= and is_valid= are per-key, not per-sort: the one place a
            ! builder differs from the one-shot entry points, so it is asserted rather than assumed.
            call keys%clear()
            mask = .true.
            mask(SWASC(SWN)) = .false.
            call keys%add(real(SWRANK, real64) + 0.25_real64, descending=.true., is_valid=mask)
            call pf_argsort(keys, perm)
            call check(error, perm(1) == int(SWASC(SWN - 1), int32) .and. &
                perm(SWN) == int(SWASC(SWN), int32), &
                "a descending key must lead with the largest non-null and leave the null last")
        end block
        if (allocated(error)) return
        block
            type(pf_sort_keys) :: keys
            call keys%add(SWBOOL)
            call pf_argsort(keys, perm)
            call check(error, all(perm == [2_int32, 5_int32, 6_int32, 1_int32, 3_int32, 4_int32]), &
                "pf_sort_keys%add must take a logical key, stably")
        end block
        if (allocated(error)) return
        block
            type(pf_sort_keys) :: keys
            character(len=2) :: v(SWN)
            do k = 1, SWN
                v(k) = swchr(SWRANK(k))
            end do
            call keys%add(v)
            call pf_argsort(keys, perm)
            call check(error, all(perm == int(SWASC, int32)), "pf_sort_keys%add must take a character key")
        end block
        if (allocated(error)) return
        block
            type(pf_sort_keys) :: keys
            type(parquet_date) :: v(SWN)
            call v%set_raw(1000_int32 + int(SWRANK, int32))
            call keys%add(v)
            call pf_argsort(keys, perm)
            call check(error, all(perm == int(SWASC, int32)), "pf_sort_keys%add must take a date key")
        end block
        if (allocated(error)) return
        block
            type(pf_sort_keys) :: keys
            type(parquet_time) :: v(SWN)
            call v%set_raw(2000_int64 + int(SWRANK, int64))
            call keys%add(v)
            call pf_argsort(keys, perm)
            call check(error, all(perm == int(SWASC, int32)), "pf_sort_keys%add must take a time key")
        end block
        if (allocated(error)) return
        block
            type(pf_sort_keys) :: keys
            type(parquet_timestamp) :: v(SWN)
            call v%set_raw(3000_int64 + int(SWRANK, int64), 7_int32)
            call keys%add(v)
            call pf_argsort(keys, perm)
            call check(error, all(perm == int(SWASC, int32)), "pf_sort_keys%add must take a timestamp key")
        end block
        if (allocated(error)) return
        block
            type(pf_sort_keys) :: keys
            type(parquet_string_column) :: sc
            do k = 1, SWN
                call sc%append_string(swchr(SWRANK(k)))
            end do
            call keys%add(sc)
            call pf_argsort(keys, perm)
            call check(error, all(perm == int(SWASC, int32)), &
                "pf_sort_keys%add must take a parquet_string_column key")
        end block
        if (allocated(error)) return
        block
            type(pf_sort_keys) :: keys
            type(parquet_column) :: c
            call c%init(PK_INT32, int(SWN, int64))
            call c%set_all(10_int32 * int(SWRANK, int32))
            call keys%add(c)
            call pf_argsort(keys, perm)
            call check(error, all(perm == int(SWASC, int32)), &
                "pf_sort_keys%add must take a parquet_column key")
        end block
    end subroutine test_keys_add_every_specific
    !
    !> **A `parquet_column` sort key of every runtime kind.** The column extractor switches on
    !> `%kindof()` ABOVE its copy loop, so each kind is a separate arm reading through a
    !> differently-typed pointer -- and one column type, `PK_INT32`, is what every other test in
    !> this suite hands it. An arm reading the wrong pointer would give a wrong order, not a
    !> type error, because the buffer it fills is `integer(int64)` for five of the nine kinds.
    !>
    !> A timestamp column is the one that becomes TWO engine keys rather than one, so it is asked
    !> a second question its single-key siblings cannot answer: ties on the seconds half must be
    !> broken by the nanoseconds half.
    subroutine test_argsort_column_every_kind(error)
        type(error_type), allocatable, intent(out) :: error
        integer(int32), allocatable :: perm(:)
        integer :: k

        block
            type(parquet_column) :: c
            call c%init(PK_INT64, int(SWN, int64))
            call c%set_all(100_int64 * int(SWRANK, int64))
            call pf_argsort(c, perm)
            call check(error, all(perm == int(SWASC, int32)), "an int64 column key must sort by its values")
        end block
        if (allocated(error)) return
        block
            type(parquet_column) :: c
            call c%init(PK_FLOAT32, int(SWN, int64))
            call c%set_all(real(SWRANK, real32) + 0.5_real32)
            call pf_argsort(c, perm)
            call check(error, all(perm == int(SWASC, int32)), "a real32 column key must sort by its values")
        end block
        if (allocated(error)) return
        block
            type(parquet_column) :: c
            call c%init(PK_FLOAT64, int(SWN, int64))
            call c%set_all(real(SWRANK, real64) + 0.25_real64)
            call pf_argsort(c, perm)
            call check(error, all(perm == int(SWASC, int32)), "a real64 column key must sort by its values")
        end block
        if (allocated(error)) return
        block
            type(parquet_column) :: c
            call c%init(PK_LOGICAL, int(SWN, int64))
            call c%set_all(SWBOOL)
            call pf_argsort(c, perm)
            call check(error, all(perm == [2_int32, 5_int32, 6_int32, 1_int32, 3_int32, 4_int32]), &
                "a logical column key must sort .false. before .true., stably")
        end block
        if (allocated(error)) return
        block
            type(parquet_column) :: c
            character(len=2) :: v(SWN)
            do k = 1, SWN
                v(k) = swchr(SWRANK(k))
            end do
            call c%init(PK_STRING, int(SWN, int64))
            call c%set_all(v)
            call pf_argsort(c, perm)
            call check(error, all(perm == int(SWASC, int32)), "a string column key must sort by its values")
        end block
        if (allocated(error)) return
        block
            type(parquet_column) :: c
            type(parquet_date) :: v(SWN)
            call v%set_raw(1000_int32 + int(SWRANK, int32))
            call c%init(PK_DATE, int(SWN, int64))
            call c%set_all(v)
            call pf_argsort(c, perm)
            call check(error, all(perm == int(SWASC, int32)), "a date column key must sort by its values")
        end block
        if (allocated(error)) return
        block
            type(parquet_column) :: c
            type(parquet_time) :: v(SWN)
            call v%set_raw(2000_int64 + int(SWRANK, int64))
            call c%init(PK_TIME, int(SWN, int64))
            call c%set_all(v)
            call pf_argsort(c, perm)
            call check(error, all(perm == int(SWASC, int32)), "a time column key must sort by its values")
        end block
        if (allocated(error)) return
        block
            type(parquet_column) :: c
            type(parquet_timestamp) :: v(SWN)
            call v%set_raw(3000_int64 + int(SWRANK, int64), 7_int32)
            call c%init(PK_TIMESTAMP, int(SWN, int64))
            call c%set_all(v)
            call pf_argsort(c, perm)
            call check(error, all(perm == int(SWASC, int32)), &
                "a timestamp column key must sort by its values")
            if (allocated(error)) return
            ! All six seconds equal: only the nanoseconds half of the pair can order these, so a
            ! body that built one key from the seconds alone leaves them in file order.
            call v%set_raw(spread(3000_int64, 1, SWN), int(SWRANK, int32))
            call c%set_all(v)
            call pf_argsort(c, perm)
            call check(error, all(perm == int(SWASC, int32)), &
                "a timestamp column key must break a seconds tie on its nanoseconds")
        end block
        if (allocated(error)) return
        ! An EMPTY column of each extractor family. Each extractor allocates one padding element
        ! so its buffer is never zero-length, and initialises it on a separate early-return path
        ! that no non-empty column reaches -- a path that, left unwritten, would hand the engine
        ! an uninitialised key.
        block
            type(parquet_column) :: c
            call c%init(PK_INT64, 0_int64)
            call pf_argsort(c, perm)
            call check(error, size(perm) == 0, "an empty int64 column key must give an empty permutation")
            if (allocated(error)) return
            call c%init(PK_FLOAT64, 0_int64)
            call pf_argsort(c, perm)
            call check(error, size(perm) == 0, "an empty real64 column key must give an empty permutation")
            if (allocated(error)) return
            call c%init(PK_TIMESTAMP, 0_int64)
            call pf_argsort(c, perm)
            call check(error, size(perm) == 0, &
                "an empty timestamp column key must give an empty permutation from both its keys")
        end block
    end subroutine test_argsort_column_every_kind
    !
    !> Builds the expected group offsets INDEPENDENTLY -- by walking the sorted values and asking
    !> where the value changes -- rather than by a second call into the sorting module.
    !>
    !> That independence is the whole point: comparing `group_offsets` against anything that shares
    !> the engine's comparator would break identically under a comparator mutation and still pass.
    subroutine expected_offsets_i32(values, perm, offsets)
        integer(int32), intent(in) :: values(:)    !! the unsorted values.
        integer(int64), intent(in) :: perm(:)      !! the permutation sorting them.
        integer(int64), allocatable, intent(out) :: offsets(:) !! where each run of equal values starts.
        integer(int64) :: k, n, ngroups, pos
        !
        n = size(perm, kind=int64)
        ngroups = 0_int64
        do k = 1_int64, n
            if (k == 1_int64) then
                ngroups = ngroups + 1_int64
            else if (values(perm(k)) /= values(perm(k - 1_int64))) then
                ngroups = ngroups + 1_int64
            end if
        end do
        allocate(offsets(ngroups + 1_int64))
        pos = 0_int64
        do k = 1_int64, n
            if (k == 1_int64) then
                pos = pos + 1_int64
                offsets(pos) = k
            else if (values(perm(k)) /= values(perm(k - 1_int64))) then
                pos = pos + 1_int64
                offsets(pos) = k
            end if
        end do
        offsets(ngroups + 1_int64) = n + 1_int64
    end subroutine expected_offsets_i32
    !
    !> Every group must be a maximal run of equal values, and the offsets must slice `perm` with no
    !> special case for the last group.
    subroutine test_group_offsets_basic(error)
        type(error_type), allocatable, intent(out) :: error
        integer(int32) :: v(9) = [5, 1, 5, 3, 1, 1, 3, 5, 9]
        integer(int64), allocatable :: perm(:), go(:), want(:)
        integer(int64) :: g
        logical :: uniform
        !
        call pf_argsort(v, perm, group_offsets=go)
        call expected_offsets_i32(v, perm, want)
        call check(error, size(go) == size(want), "the group count must match the independent oracle")
        if (allocated(error)) return
        call check(error, all(go == want), "every group boundary must match the independent oracle")
        if (allocated(error)) return
        call check(error, go(size(go)) == 10_int64, "the last entry must be the sentinel n + 1")
        if (allocated(error)) return
        call check(error, size(go) - 1 == 4, "1, 3, 5 and 9 must give four groups")
        if (allocated(error)) return
        ! Slice every group the documented way -- including the last, which is exactly what the
        ! sentinel exists to make unremarkable.
        uniform = .true.
        do g = 1_int64, size(go, kind=int64) - 1_int64
            if (any(v(perm(go(g):go(g + 1_int64) - 1_int64)) /= v(perm(go(g))))) uniform = .false.
        end do
        call check(error, uniform, "every row inside a group must hold the same value")
    end subroutine test_group_offsets_basic
    !
    !> The three shapes where an off-by-one hides: no rows, one row, and one single group.
    subroutine test_group_offsets_edges(error)
        type(error_type), allocatable, intent(out) :: error
        integer(int32) :: empty(0), one(1) = [7], tied(4) = [2, 2, 2, 2]
        integer(int64), allocatable :: perm(:), go(:)
        !
        call pf_argsort(empty, perm, group_offsets=go)
        call check(error, allocated(go), "group_offsets must be allocated even for an empty array")
        if (allocated(error)) return
        call check(error, size(go) == 1 .and. go(1) == 1_int64, &
            "an empty array must give the sentinel alone, [1], meaning no groups")
        if (allocated(error)) return
        !
        call pf_argsort(one, perm, group_offsets=go)
        call check(error, size(go) == 2, "one row must give exactly one group")
        if (allocated(error)) return
        call check(error, all(go == [1_int64, 2_int64]), "one row must give [1, 2]")
        if (allocated(error)) return
        !
        call pf_argsort(tied, perm, group_offsets=go)
        call check(error, all(go == [1_int64, 5_int64]), "four equal rows must give one group, [1, 5]")
    end subroutine test_group_offsets_edges
    !
    !> Nulls form ONE group and NaNs form ONE group, because rows in the same non-value tier
    !> compare equal. Deliberately unlike `pf_unique`, which drops nulls entirely -- a group list
    !> has to account for every row, so this difference is a contract rather than an accident.
    subroutine test_group_offsets_tiers(error)
        type(error_type), allocatable, intent(out) :: error
        integer(int32) :: v(6) = [3, 0, 1, 0, 3, 0]
        logical :: valid(6) = [.true., .false., .true., .false., .true., .false.]
        real(real64) :: f(6)
        integer(int64), allocatable :: perm(:), go(:)
        !
        ! 1 | 3, 3 | three nulls -- three groups, and the nulls are one of them however many rows
        ! carry a null.
        call pf_argsort(v, perm, is_valid=valid, group_offsets=go)
        call check(error, size(go) - 1 == 3, "two values plus three nulls must give three groups")
        if (allocated(error)) return
        call check(error, all(go == [1_int64, 2_int64, 4_int64, 7_int64]), &
            "the three nulls must occupy one trailing group")
        if (allocated(error)) return
        !
        f = [2.0_real64, ieee_value(1.0_real64, ieee_quiet_nan), 1.0_real64, &
            ieee_value(1.0_real64, ieee_quiet_nan), 2.0_real64, &
            ieee_value(1.0_real64, ieee_quiet_nan)]
        call pf_argsort(f, perm, group_offsets=go)
        call check(error, size(go) - 1 == 3, "two values plus three NaNs must give three groups")
        if (allocated(error)) return
        call check(error, all(go == [1_int64, 2_int64, 4_int64, 7_int64]), &
            "the three NaNs must occupy one trailing group")
    end subroutine test_group_offsets_tiers
    !
    !> Asking for boundaries switches the engine from the one-shot borrow to the builder, so the
    !> permutation is produced by a different code path. It must not differ by a single element --
    !> and no group-boundary assertion would notice if it did.
    subroutine test_group_offsets_same_perm(error)
        type(error_type), allocatable, intent(out) :: error
        integer(int32) :: v(11) = [4, 9, 1, 9, 2, 7, 4, 0, 7, 7, 3]
        integer(int64), allocatable :: plain(:), grouped(:), go(:)
        type(pf_sort_keys) :: k
        integer(int64), allocatable :: kplain(:), kgrouped(:)
        !
        call pf_argsort(v, plain)
        call pf_argsort(v, grouped, group_offsets=go)
        call check(error, all(plain == grouped), &
            "the builder path must produce the same permutation as the one-shot path")
        if (allocated(error)) return
        !
        call k%add(v)
        call pf_argsort(k, kplain)
        call pf_argsort(k, kgrouped, group_offsets=go)
        call check(error, all(kplain == kgrouped), &
            "a pf_sort_keys sort must agree with itself whether or not boundaries are asked for")
    end subroutine test_group_offsets_same_perm
    !
    !> `group_offsets` follows `perm`'s kind, so both forms exist and must agree.
    subroutine test_group_offsets_kinds(error)
        type(error_type), allocatable, intent(out) :: error
        integer(int32) :: v(5) = [2, 1, 2, 3, 1]
        integer(int32), allocatable :: perm32(:), go32(:)
        integer(int64), allocatable :: perm64(:), go64(:)
        !
        call pf_argsort(v, perm32, group_offsets=go32)
        call pf_argsort(v, perm64, group_offsets=go64)
        call check(error, size(go32) == size(go64), "both kinds must report the same group count")
        if (allocated(error)) return
        call check(error, all(int(go32, int64) == go64), "both kinds must report the same boundaries")
        if (allocated(error)) return
        call check(error, all(go64 == [1_int64, 3_int64, 5_int64, 6_int64]), &
            "1,1 | 2,2 | 3 must give three groups")
    end subroutine test_group_offsets_kinds
    !
    !> The motivating shape: sort by both keys, group by the first alone. Both halves are asserted,
    !> because an implementation that dropped the second key from the SORT would still produce the
    !> right group boundaries.
    subroutine test_group_nkeys_prefix(error)
        type(error_type), allocatable, intent(out) :: error
        type(pf_sort_keys) :: k
        integer(int32) :: field(6) = [2, 1, 2, 1, 2, 1]
        integer(int32) :: mag(6) = [30, 60, 10, 40, 20, 50]
        integer(int64), allocatable :: perm(:), go(:), go_all(:)
        !
        call k%add(field)
        call k%add(mag)
        call pf_argsort(k, perm, group_offsets=go, group_nkeys=1)
        call check(error, size(go) - 1 == 2, "grouping on field alone must give two groups")
        if (allocated(error)) return
        call check(error, all(go == [1_int64, 4_int64, 7_int64]), &
            "each field value must own three consecutive rows")
        if (allocated(error)) return
        ! The second key still orders: inside field 1 the magnitudes must climb 40, 50, 60.
        call check(error, all(mag(perm(1:3)) == [40, 50, 60]), &
            "rows inside a group must still be ordered by the second key")
        if (allocated(error)) return
        call check(error, all(mag(perm(4:6)) == [10, 20, 30]), &
            "the second group must be ordered by the second key too")
        if (allocated(error)) return
        ! The default is every key, which here means every row is its own group.
        call pf_argsort(k, perm, group_offsets=go_all)
        call check(error, size(go_all) - 1 == 6, &
            "the default must group on ALL keys, giving six singleton groups here")
    end subroutine test_group_nkeys_prefix
    !
    !> `group_nkeys` counts the keys the CALLER added, and a `parquet_timestamp` is one of those
    !> while being two engine keys. The fixture is built so a translation that forgot this cannot
    !> pass: two rows share a second and differ only in nanoseconds, so grouping on engine key 1
    !> alone (the seconds) would merge them, while grouping on caller key 1 must not.
    subroutine test_group_nkeys_timestamp(error)
        type(error_type), allocatable, intent(out) :: error
        type(pf_sort_keys) :: k
        type(parquet_timestamp) :: ts(4)
        integer(int32) :: mag(4) = [10, 20, 30, 40]
        integer(int64), allocatable :: perm(:), go(:)
        !
        ! Two rows at second 100 differing in nanoseconds, and two genuinely equal at second 200.
        call ts(1)%set_unix(100_int64, parquet_unit_seconds)
        call ts(1)%set_raw(100_int64, 500_int32)
        call ts(2)%set_raw(100_int64, 900_int32)
        call ts(3)%set_raw(200_int64, 0_int32)
        call ts(4)%set_raw(200_int64, 0_int32)
        call k%add(ts)
        call k%add(mag)
        call check(error, k%nkeys_added() == 2, &
            "a timestamp key plus an integer key must report TWO keys, not three")
        if (allocated(error)) return
        !
        call pf_argsort(k, perm, group_offsets=go, group_nkeys=1)
        call check(error, size(go) - 1 == 3, &
            "grouping on the timestamp alone must give three groups; merging 100.500 with " // &
            "100.900 would mean the prefix was counted in engine keys")
        if (allocated(error)) return
        call check(error, all(go == [1_int64, 2_int64, 3_int64, 5_int64]), &
            "the two identical timestamps must share a group and the two others must not")
    end subroutine test_group_nkeys_timestamp
    !
    !> `%nkeys_added` reports `%add` calls. It used to report the ENGINE key count, so a lone
    !> timestamp answered 2 -- which no non-temporal fixture can detect.
    subroutine test_nkeys_added_counts_adds(error)
        type(error_type), allocatable, intent(out) :: error
        type(pf_sort_keys) :: k
        type(parquet_timestamp) :: ts(3)
        integer(int32) :: v(3) = [1, 2, 3]
        integer :: i
        !
        do i = 1, 3
            call ts(i)%set_raw(int(i, int64), 0_int32)
        end do
        call check(error, k%nkeys_added() == 0, "a fresh key list must report no keys")
        if (allocated(error)) return
        call k%add(ts)
        call check(error, k%nkeys_added() == 1, &
            "one timestamp %add must report ONE key, though it binds as two engine keys")
        if (allocated(error)) return
        call k%add(v)
        call check(error, k%nkeys_added() == 2, "a second %add must report two keys")
        if (allocated(error)) return
        call k%clear()
        call check(error, k%nkeys_added() == 0, "%clear must leave no keys")
    end subroutine test_nkeys_added_counts_adds
    !
    !> A multi-key order could be built and nothing else asked about it. Both answers are
    !> asserted -- a check that always says .true. passes every positive test ever written for it.
    subroutine test_is_sorted_keys(error)
        type(error_type), allocatable, intent(out) :: error
        type(pf_sort_keys) :: k
        integer(int32) :: field(6) = [1, 1, 1, 2, 2, 2]
        integer(int32) :: mag(6) = [10, 20, 30, 5, 15, 25]
        integer(int32) :: jumbled(6) = [10, 20, 30, 5, 15, 24]
        logical :: ok
        !
        call k%add(field)
        call k%add(mag)
        call pf_is_sorted(k, ok)
        call check(error, ok, "a table already ordered by both keys must report sorted")
        if (allocated(error)) return
        !
        ! Ordered by the FIRST key but not within its groups: the second key is what must catch it,
        ! so this fails if only the primary key is consulted.
        call k%clear()
        call k%add(field)
        call k%add([30, 20, 10, 5, 15, 25])
        call pf_is_sorted(k, ok)
        call check(error, .not. ok, "a break in the SECOND key must report unsorted")
        if (allocated(error)) return
        !
        ! Descending is a per-key property carried from %add, not an argument here.
        call k%clear()
        call k%add(field, descending=.true.)
        call pf_is_sorted(k, ok)
        call check(error, .not. ok, "an ascending array must not report sorted under descending=")
        if (allocated(error)) return
        !
        call k%clear()
        call k%add(jumbled)
        call pf_is_sorted(k, ok)
        call check(error, .not. ok, "an unsorted single key must report unsorted")
    end subroutine test_is_sorted_keys
    !
    !> The multi-key top-N. Its oracle is a full `pf_argsort` of the same keys: the first `n`
    !> entries must agree exactly, since a partial sort orders that prefix and nothing else.
    subroutine test_partial_argsort_keys(error)
        type(error_type), allocatable, intent(out) :: error
        type(pf_sort_keys) :: k
        integer(int32) :: field(8) = [2, 1, 2, 1, 2, 1, 3, 3]
        integer(int32) :: mag(8) = [30, 60, 10, 40, 20, 50, 5, 70]
        integer(int64), allocatable :: full(:), part(:)
        integer(int32), allocatable :: part32(:)
        !
        call k%add(field)
        call k%add(mag)
        call pf_argsort(k, full)
        call pf_partial_argsort(k, part, 3)
        call check(error, size(part) == 3, "n = 3 must return exactly three indices")
        if (allocated(error)) return
        call check(error, all(part == full(1:3)), &
            "the first n of a partial sort must equal the first n of a full sort")
        if (allocated(error)) return
        !
        ! Clamped, not checked -- no min(n, nrows) is needed at any call site.
        call pf_partial_argsort(k, part, 99)
        call check(error, size(part) == 8, "n above the row count must clamp rather than abort")
        if (allocated(error)) return
        call check(error, all(part == full), "a fully clamped partial sort must equal a full sort")
        if (allocated(error)) return
        !
        call pf_partial_argsort(k, part, 0)
        call check(error, size(part) == 0, "n = 0 must return an empty permutation")
        if (allocated(error)) return
        !
        call pf_partial_argsort(k, part32, 3)
        call check(error, all(int(part32, int64) == full(1:3)), &
            "the int32 form must agree with the int64 one")
    end subroutine test_partial_argsort_keys
    !
    ! ============================================================================================
    ! Stage 1 conformance oracle -- feature_sort.md section 6 Stage 1
    ! ============================================================================================
    !
    ! The Fortran comparator core must answer EXACTLY as the C++ one does, because after the Stage 6
    ! cutover the two decide the order of the same data by different routes. Asserting that a sort
    ! comes out sorted does not test this: both engines sort correctly and can still disagree about
    ! which of two EQUAL rows comes first, which is the maintainer's stated requirement ("the two
    ! sorting algorithms provide the same ordering, even for duplicated values").
    !
    ! So these tests compare ANSWERS, not orderings, over every ordered pair of a small tie-rich
    ! fixture. Every fixture is deliberately duplicate-heavy: a random real64 column of any size
    ! contains essentially no ties at all, which is exactly why the motivating benchmark in
    ! feature_sort.md section 2 could not have caught a tie-order defect.
    !
    ! Both engines are fed from ONE source in each test -- the same Fortran arrays go into a
    ! `pf_sort_keys` and into a C++ builder -- so a disagreement is a comparator disagreement and
    ! not a data one.
    !
    !> Asks BOTH engines about every ordered pair, and checks the total-order properties.
    !!
    !! The C++ side is reached through two test-only hooks in src/parquet_wrapper.cpp, declared
    !! locally here because that is how every `parquet_debug_*` hook is reached and it keeps them
    !! out of src/parquet_bindings.f90. Their rows are 0-BASED (the C++ internal convention), so
    !! each index is passed as `i - 1`; the Fortran side is 1-based throughout.
    !!
    !! The three property checks are what the C++ side cannot be asked about, and they are the
    !! reason this is not merely a two-way diff: irreflexivity, antisymmetry, and that `less` agrees
    !! with the sign of the tie-free answer or -- on a full tie -- falls back to row order, which is
    !! the index tiebreaker doing its job.
    subroutine sweep_pairs(error, label, keys, builder, n, nkeys, ntotal)
        type(error_type), allocatable, intent(inout) :: error !! set on the first disagreement.
        character(len=*), intent(in) :: label                 !! names the fixture in every message.
        type(pf_sort_keys), intent(in) :: keys                !! the Fortran key set.
        type(c_ptr), intent(in) :: builder                    !! the C++ builder over the same data.
        integer(int64), intent(in) :: n                       !! rows.
        integer, intent(in) :: nkeys                          !! prefix for the tie-free comparator.
        integer, intent(in) :: ntotal                         !! engine keys the set actually holds.
        interface
            function c_dbg_row_less(handle, a, b) bind(C, name="parquet_debug_sort_row_less") result(r)
                import :: c_ptr, c_long_long
                type(c_ptr), value :: handle           !! the C++ builder handle.
                integer(c_long_long), value :: a       !! first row, 0-based.
                integer(c_long_long), value :: b       !! second row, 0-based.
                integer(c_long_long) :: r              !! 1 = less, 0 = not, -1 = no key added.
            end function c_dbg_row_less
            function c_dbg_keys_compare(handle, a, b, nk) &
                    bind(C, name="parquet_debug_sort_keys_compare") result(r)
                import :: c_ptr, c_long_long
                type(c_ptr), value :: handle           !! the C++ builder handle.
                integer(c_long_long), value :: a       !! first row, 0-based.
                integer(c_long_long), value :: b       !! second row, 0-based.
                integer(c_long_long), value :: nk      !! leading keys taking part.
                integer(c_long_long) :: r              !! -1/0/+1, or -2 when no key was added.
            end function c_dbg_keys_compare
        end interface
        integer(int64) :: a, b
        logical :: fl, cl, fl_rev
        integer :: fc, cc
        !
        do a = 1_int64, n
            do b = 1_int64, n
                fl = parquet_debug_sort_row_less(keys, a, b)
                cl = c_dbg_row_less(builder, int(a - 1_int64, c_long_long), &
                    int(b - 1_int64, c_long_long)) == 1_c_long_long
                call check(error, fl .eqv. cl, label // ": sort_row_less disagrees with the C++ engine")
                if (allocated(error)) return
                !
                fc = parquet_debug_sort_keys_compare(keys, a, b, nkeys)
                cc = int(c_dbg_keys_compare(builder, int(a - 1_int64, c_long_long), &
                    int(b - 1_int64, c_long_long), int(nkeys, c_long_long)))
                call check(error, fc == cc, label // ": sort_keys_compare disagrees with the C++ engine")
                if (allocated(error)) return
                !
                if (a == b) then
                    call check(error, .not. fl, label // ": sort_row_less(i, i) must be .false.")
                    if (allocated(error)) return
                else
                    fl_rev = parquet_debug_sort_row_less(keys, b, a)
                    call check(error, fl .neqv. fl_rev, &
                        label // ": exactly one of less(a,b) and less(b,a) must hold")
                    if (allocated(error)) return
                    if (fc /= 0) then
                        ! Safe for any prefix: if the leading `nkeys` keys already decide, the full
                        ! comparator decides the same way on the same key.
                        call check(error, fl .eqv. (fc < 0), &
                            label // ": less must follow the sign of the tie-free comparator")
                    else if (nkeys >= ntotal) then
                        ! Only meaningful when the prefix covers EVERY key. On a shorter prefix a
                        ! zero says "tied so far", and a later key -- not the row index -- is what
                        ! `less` used to decide. Asserting row order here would be asserting that
                        ! the prefix is the whole key list.
                        call check(error, fl .eqv. (a < b), &
                            label // ": on a full tie, less must fall back to row order")
                    end if
                    if (allocated(error)) return
                end if
            end do
        end do
    end subroutine sweep_pairs
    !
    !> Integer keys: three-way ties, two nulls, every direction and null placement.
    subroutine test_engine_conf_int(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first disagreement.
        integer(int64), parameter :: n = 8_int64
        integer(int64), target :: vals(n)
        integer(c_int8_t), target :: cvalid(n)
        logical :: fvalid(n)
        type(pf_sort_keys) :: keys
        type(c_ptr) :: builder
        integer :: id, inf
        logical :: desc, nf
        !
        ! Three 5s and two 3s, so tie order is exercised on most pairs; rows 3 and 5 are null, and
        ! their value slots deliberately hold ordinary values -- a comparator that forgets the tier
        ! test would order them by those and still look plausible.
        vals = [5_int64, 3_int64, 5_int64, 1_int64, 3_int64, 9_int64, 0_int64, 5_int64]
        fvalid = [.true., .true., .false., .true., .false., .true., .true., .true.]
        cvalid = merge(1_c_int8_t, 0_c_int8_t, fvalid)
        do id = 0, 1
            do inf = 0, 1
                desc = (id == 1)
                nf = (inf == 1)
                call keys%clear()
                call keys%add(vals, descending=desc, nulls_first=nf, is_valid=fvalid)
                builder = parquet_sort_builder_new(int(n, c_long_long))
                call parquet_sort_builder_add_key_int64(builder, vals, c_loc(cvalid), &
                    merge(1_c_int8_t, 0_c_int8_t, desc), merge(1_c_int8_t, 0_c_int8_t, nf))
                call sweep_pairs(error, "int desc=" // merge("T", "F", desc) // " nf=" // &
                    merge("T", "F", nf), keys, builder, n, 1, 1)
                call parquet_sort_builder_free(builder)
                if (allocated(error)) return
            end do
        end do
    end subroutine test_engine_conf_int
    !
    !> Real keys: all three tiers at once -- values, NaNs and nulls -- with ties inside each.
    subroutine test_engine_conf_real(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first disagreement.
        integer(int64), parameter :: n = 8_int64
        real(real64), target :: vals(n)
        integer(c_int8_t), target :: cvalid(n)
        logical :: fvalid(n)
        real(real64) :: qnan
        type(pf_sort_keys) :: keys
        type(c_ptr) :: builder
        integer :: id, inf
        logical :: desc, nf
        !
        qnan = ieee_value(1.0_real64, ieee_quiet_nan)
        ! Two NaNs and two nulls, plus repeated values: this is the only fixture that can catch a
        ! comparator putting NaNs on the wrong side of the null block, which no ascending,
        ! null-free test can see (feature_sort.md section 5.1).
        vals = [2.0_real64, qnan, -1.0_real64, 2.0_real64, qnan, 0.0_real64, 3.0_real64, -1.0_real64]
        fvalid = [.true., .true., .true., .false., .true., .false., .true., .true.]
        cvalid = merge(1_c_int8_t, 0_c_int8_t, fvalid)
        do id = 0, 1
            do inf = 0, 1
                desc = (id == 1)
                nf = (inf == 1)
                call keys%clear()
                call keys%add(vals, descending=desc, nulls_first=nf, is_valid=fvalid)
                builder = parquet_sort_builder_new(int(n, c_long_long))
                call parquet_sort_builder_add_key_double(builder, vals, c_loc(cvalid), &
                    merge(1_c_int8_t, 0_c_int8_t, desc), merge(1_c_int8_t, 0_c_int8_t, nf))
                call sweep_pairs(error, "real desc=" // merge("T", "F", desc) // " nf=" // &
                    merge("T", "F", nf), keys, builder, n, 1, 1)
                call parquet_sort_builder_free(builder)
                if (allocated(error)) return
            end do
        end do
    end subroutine test_engine_conf_real
    !
    !> Fixed-width string keys, including a byte above 127.
    !!
    !! `%add` on a `character(len=*)` array sorts on the FULL declared width, blanks included, so
    !! every row here is the same length and the length tail of the comparison is not reached — that
    !! is `test_engine_conf_varstr`'s job. What this fixture does cover is the **unsigned** byte
    !! rule: row 6 carries `achar(200)`, which must sort ABOVE every ASCII row. A comparator reading
    !! bytes as signed puts it below them, and nothing else in the suite would notice.
    subroutine test_engine_conf_str(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first disagreement.
        integer(int64), parameter :: n = 7_int64
        integer(int64), parameter :: w = 3_int64
        character(len=3), target :: vals(n)
        integer(c_long_long), target :: offs(n + 1)
        character(kind=c_char), target :: bytes(n * w)
        integer(c_int8_t), target :: cvalid(n)
        logical :: fvalid(n)
        type(pf_sort_keys) :: keys
        type(c_ptr) :: builder
        integer :: id, inf, j
        integer(int64) :: k
        logical :: desc, nf
        !
        vals = ["abc", "ab ", "abc", "b  ", "a  ", "z  ", "abc"]
        vals(6)(1:1) = achar(200)
        fvalid = [.true., .true., .false., .true., .true., .true., .true.]
        cvalid = merge(1_c_int8_t, 0_c_int8_t, fvalid)
        ! The same bytes the Fortran side will pack, laid out for the C++ builder: fixed width,
        ! 0-based offsets, no trimming.
        do k = 1_int64, n + 1_int64
            offs(k) = int((k - 1_int64) * w, c_long_long)
        end do
        do k = 1_int64, n
            do j = 1, int(w)
                bytes((k - 1_int64) * w + j) = vals(k)(j:j)
            end do
        end do
        do id = 0, 1
            do inf = 0, 1
                desc = (id == 1)
                nf = (inf == 1)
                call keys%clear()
                call keys%add(vals, descending=desc, nulls_first=nf, is_valid=fvalid)
                builder = parquet_sort_builder_new(int(n, c_long_long))
                call parquet_sort_builder_add_key_string(builder, offs, bytes, c_loc(cvalid), &
                    merge(1_c_int8_t, 0_c_int8_t, desc), merge(1_c_int8_t, 0_c_int8_t, nf))
                call sweep_pairs(error, "str desc=" // merge("T", "F", desc) // " nf=" // &
                    merge("T", "F", nf), keys, builder, n, 1, 1)
                call parquet_sort_builder_free(builder)
                if (allocated(error)) return
            end do
        end do
    end subroutine test_engine_conf_str
    !
    !> Variable-length string keys, where a prefix must sort BEFORE the string it is a prefix of.
    !!
    !! This is the one fixture that separates `memcmp` semantics from Fortran's own `<`, which
    !! blank-pads the shorter operand and would call "ab" and "ab " equal. A `parquet_string_column`
    !! is the only route to genuinely ragged rows, since the `character(len=*)` form is fixed width.
    subroutine test_engine_conf_varstr(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first disagreement.
        integer(int64), parameter :: n = 6_int64
        character(len=*), parameter :: raw(n) = ["ab ", "a  ", "abc", "ab ", "   ", "b  "]
        integer(int64), parameter :: lens(n) = [2_int64, 1_int64, 3_int64, 2_int64, 0_int64, 1_int64]
        type(parquet_string_column) :: col
        integer(c_long_long), target :: offs(n + 1)
        character(kind=c_char), target :: bytes(9) !! sum(lens) -- sized exactly, not guessed.
        type(pf_sort_keys) :: keys
        type(c_ptr) :: builder
        integer :: id, inf, j
        integer(int64) :: k, pos
        logical :: desc, nf
        !
        ! "a" is a prefix of "ab", which is a prefix of "abc", and "" is a prefix of everything;
        ! rows 1 and 4 are an exact tie. Fortran's own comparison would order several of these
        ! differently from memcmp, which is precisely the point.
        call col%clear()
        do k = 1_int64, n
            call col%append_string(raw(k)(1:lens(k)))
        end do
        pos = 0_int64
        do k = 1_int64, n
            offs(k) = int(pos, c_long_long)
            do j = 1, int(lens(k))
                bytes(pos + j) = raw(k)(j:j)
            end do
            pos = pos + lens(k)
        end do
        offs(n + 1) = int(pos, c_long_long)
        do id = 0, 1
            do inf = 0, 1
                desc = (id == 1)
                nf = (inf == 1)
                call keys%clear()
                call keys%add(col, descending=desc, nulls_first=nf)
                builder = parquet_sort_builder_new(int(n, c_long_long))
                call parquet_sort_builder_add_key_string(builder, offs, bytes, c_null_ptr, &
                    merge(1_c_int8_t, 0_c_int8_t, desc), merge(1_c_int8_t, 0_c_int8_t, nf))
                call sweep_pairs(error, "varstr desc=" // merge("T", "F", desc) // " nf=" // &
                    merge("T", "F", nf), keys, builder, n, 1, 1)
                call parquet_sort_builder_free(builder)
                if (allocated(error)) return
            end do
        end do
    end subroutine test_engine_conf_varstr
    !
    !> Three keys of mixed family, swept over every `nkeys` prefix including an over-long one.
    !!
    !! The prefix is what run detection uses -- "sort by field then magnitude, but group by field
    !! alone" -- so an off-by-one there silently changes what `pf_unique`/`pf_rank` treat as one
    !! group while leaving every ordering correct. The primary key repeats heavily so that the
    !! second and third keys actually decide most pairs.
    subroutine test_engine_conf_multi(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first disagreement.
        integer(int64), parameter :: n = 8_int64
        integer(int64), parameter :: w = 2_int64
        integer(int64), target :: k1(n)
        real(real64), target :: k2(n)
        character(len=2), target :: k3(n)
        integer(c_long_long), target :: offs(n + 1)
        character(kind=c_char), target :: bytes(n * w)
        integer(c_int8_t), target :: cvalid(n)
        logical :: fvalid(n)
        type(pf_sort_keys) :: keys
        type(c_ptr) :: builder
        integer :: nk, j
        integer(int64) :: k
        !
        k1 = [1_int64, 1_int64, 1_int64, 2_int64, 2_int64, 2_int64, 1_int64, 2_int64]
        k2 = [7.0_real64, 7.0_real64, 5.0_real64, 1.0_real64, 1.0_real64, 9.0_real64, 7.0_real64, 1.0_real64]
        k3 = ["bb", "aa", "cc", "aa", "zz", "mm", "bb", "aa"]
        ! Nulls on the SECOND key only, so the tier rule has to be applied per key rather than per
        ! row -- a comparator that hoisted the null test out of the key loop would pass every
        ! single-key fixture above and fail here.
        fvalid = [.true., .true., .false., .true., .true., .true., .true., .false.]
        cvalid = merge(1_c_int8_t, 0_c_int8_t, fvalid)
        do k = 1_int64, n + 1_int64
            offs(k) = int((k - 1_int64) * w, c_long_long)
        end do
        do k = 1_int64, n
            do j = 1, int(w)
                bytes((k - 1_int64) * w + j) = k3(k)(j:j)
            end do
        end do
        !
        call keys%add(k1)
        call keys%add(k2, descending=.true., is_valid=fvalid)
        call keys%add(k3, nulls_first=.true.)
        builder = parquet_sort_builder_new(int(n, c_long_long))
        call parquet_sort_builder_add_key_int64(builder, k1, c_null_ptr, 0_c_int8_t, 0_c_int8_t)
        call parquet_sort_builder_add_key_double(builder, k2, c_loc(cvalid), 1_c_int8_t, 0_c_int8_t)
        call parquet_sort_builder_add_key_string(builder, offs, bytes, c_null_ptr, 0_c_int8_t, 1_c_int8_t)
        ! nkeys = 4 is deliberately one more than exists: both engines must clamp, not read past.
        do nk = 1, 4
            call sweep_pairs(error, "multi nkeys=" // achar(iachar("0") + nk), keys, builder, n, nk, 3)
            if (allocated(error)) then
                call parquet_sort_builder_free(builder)
                return
            end if
        end do
        call parquet_sort_builder_free(builder)
    end subroutine test_engine_conf_multi
    !
    ! ============================================================================================
    ! Stage 2 conformance -- feature_sort.md section 6 Stage 2
    ! ============================================================================================
    !
    ! Stage 1 proved the two comparators agree pair by pair. These prove the two SORTS agree
    ! permutation by permutation, which is a different claim: a correct comparator driven by a
    ! defective sort still returns a sorted answer whenever the defect only reorders equal elements
    ! -- and under `sort_row_less` there are no equal elements, so the two engines' permutations
    ! must be identical element for element or one of them is wrong.
    !
    ! Both engines are reached through the ordinary public entry point (`pf_argsort`), switched by
    ! `parquet_debug_use_fortran_sort_engine`. That is deliberate: it tests the wiring in
    ! `drive_engine` as well as the algorithm, which a direct call into the engine would not.
    !
    !> Arms the C++ comparison counter; see `arm_sort_comparisons` in test_sort.f90 for the full why.
    subroutine engine_arm_comparisons()
        interface
            subroutine count_cmp(enable) bind(C, name="parquet_debug_set_count_sort_comparisons")
                use iso_c_binding, only : c_int
                integer(c_int), value :: enable !! nonzero arms and zeroes the counter.
            end subroutine count_cmp
        end interface
        call count_cmp(1)
    end subroutine engine_arm_comparisons
    !
    !> C++ comparisons counted since the last `engine_arm_comparisons`, then disarms the counter.
    integer(int64) function engine_comparisons() result(n)
        interface
            function got_cmp() bind(C, name="parquet_debug_get_sort_comparisons") result(k)
                use iso_c_binding, only : c_long_long
                integer(c_long_long) :: k !! comparisons since the counter was armed.
            end function got_cmp
            subroutine count_cmp(enable) bind(C, name="parquet_debug_set_count_sort_comparisons")
                use iso_c_binding, only : c_int
                integer(c_int), value :: enable !! nonzero arms and zeroes the counter.
            end subroutine count_cmp
        end interface
        n = int(got_cmp(), int64)
        call count_cmp(0)
    end function engine_comparisons
    !
    !> Argsorts one key set down BOTH engines and requires the two permutations to be identical.
    !!
    !! The permutation-validity check afterwards is not redundant with the equality: two engines
    !! broken in the same way would agree with each other, and only "names every row exactly once"
    !! notices. It is the cheapest independent oracle available here.
    subroutine engine_ab(error, label, keys, n)
        type(error_type), allocatable, intent(inout) :: error !! set on the first disagreement.
        character(len=*), intent(in) :: label                 !! names the fixture in every message.
        class(pf_sort_keys), intent(in) :: keys               !! the key set to sort by.
        integer(int64), intent(in) :: n                       !! rows.
        integer(int64), allocatable :: pc(:) !! the C++ engine's permutation.
        integer(int64), allocatable :: pf(:) !! the Fortran engine's permutation.
        logical, allocatable :: seen(:)      !! which rows the Fortran permutation named.
        integer(int64) :: k                  !! walk index.
        !
        call parquet_debug_use_fortran_sort_engine(.false.)
        call pf_argsort(keys, pc)
        call parquet_debug_use_fortran_sort_engine(.true.)
        call pf_argsort(keys, pf)
        call parquet_debug_use_fortran_sort_engine(.false.)
        !
        call check(error, size(pf, kind=int64) == n, label // ": the Fortran permutation is the wrong length")
        if (allocated(error)) return
        call check(error, size(pc, kind=int64) == n, label // ": the C++ permutation is the wrong length")
        if (allocated(error)) return
        call check(error, all(pf == pc), label // ": the Fortran permutation differs from the C++ one")
        if (allocated(error)) return
        !
        allocate(seen(max(n, 1_int64)))
        seen = .false.
        do k = 1_int64, n
            if (pf(k) < 1_int64 .or. pf(k) > n) then
                call check(error, .false., label // ": the Fortran permutation holds an out-of-range row")
                return
            end if
            seen(pf(k)) = .true.
        end do
        call check(error, all(seen(1:n)), label // ": the Fortran permutation does not name every row once")
    end subroutine engine_ab
    !
    !> The switch really switches: the C++ engine counts comparisons, the Fortran one cannot.
    !!
    !! **Without this every other Stage 2 test is potentially vacuous.** They assert that two
    !! permutations agree, and if `parquet_debug_use_fortran_sort_engine` did nothing at all -- a
    !! flag never read, a branch placed after the return, a regenerated file that lost the wiring --
    !! both halves would be the C++ engine and every one of them would pass while testing nothing.
    !! That is `feature_risks.md` Risk-35's failure mode exactly.
    !!
    !! The counter lives inside the C++ comparator, so it can only move when the C++ comparator
    !! runs. The key is real-valued with distinct values so that the integer counting fast path --
    !! which performs zero comparisons by construction -- declines it and the C++ arm must count.
    subroutine test_fortran_engine_switches(error)
        type(error_type), allocatable, intent(out) :: error !! set on failure.
        integer(int64), parameter :: n = 64_int64
        real(real64) :: v(n)
        type(pf_sort_keys) :: keys
        integer(int64), allocatable :: perm(:)
        integer(int64) :: cmp_cpp, cmp_fortran, k
        !
        do k = 1_int64, n
            v(k) = real(mod(k * 37_int64, n), real64) + 0.5_real64
        end do
        call keys%add(v)
        !
        call parquet_debug_use_fortran_sort_engine(.false.)
        call check(error, .not. parquet_debug_using_fortran_sort_engine(), &
            "the engine selector must report the C++ engine after being cleared")
        if (allocated(error)) return
        call engine_arm_comparisons()
        call pf_argsort(keys, perm)
        cmp_cpp = engine_comparisons()
        !
        call parquet_debug_use_fortran_sort_engine(.true.)
        call check(error, parquet_debug_using_fortran_sort_engine(), &
            "the engine selector must report the Fortran engine after being set")
        if (allocated(error)) then
            call parquet_debug_use_fortran_sort_engine(.false.)
            return
        end if
        call engine_arm_comparisons()
        call pf_argsort(keys, perm)
        cmp_fortran = engine_comparisons()
        call parquet_debug_use_fortran_sort_engine(.false.)
        !
        call check(error, cmp_cpp > 0_int64, &
            "the C++ arm counted no comparisons, so the counter is not measuring what it should")
        if (allocated(error)) return
        call check(error, cmp_fortran == 0_int64, &
            "the Fortran arm reached the C++ comparator, so the engine selector did not switch")
    end subroutine test_fortran_engine_switches
    !
    !> Both engines, every key family, over sizes spanning the insertion cutoff and the recursion.
    !!
    !! The sizes are chosen against the algorithm rather than at random: 2 and 5 never leave the
    !! final insertion pass, 16 is exactly `SORT_INSERTION_CUTOFF`, 17 is the first size that
    !! partitions at all, and 257/1000 recurse several levels deep. A sweep that used only round
    !! numbers would miss the cutoff boundary, which is where an off-by-one in the loop condition
    !! lives.
    !!
    !! The integer key is deliberately LOW-CARDINALITY, which sends the C++ arm down its integer
    !! counting fast path while the Fortran arm compares its way there. The two engines then reach
    !! the same permutation by genuinely different routes, which is a stronger check than two
    !! comparison sorts agreeing.
    subroutine test_fortran_engine_ab_families(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first disagreement.
        integer(int64), parameter :: sizes(6) = [2_int64, 5_int64, 16_int64, 17_int64, 257_int64, 1000_int64]
        integer(int64), allocatable :: vi(:)
        real(real64), allocatable :: vr(:)
        character(len=4), allocatable :: vs(:)
        logical, allocatable :: valid(:)
        type(pf_sort_keys) :: keys
        integer(int64) :: n, k
        integer :: is, id, inf
        logical :: desc, nf
        character(len=32) :: nstr
        character(len=:), allocatable :: tag
        !
        do is = 1, size(sizes)
            n = sizes(is)
            allocate(vi(n), vr(n), vs(n), valid(n))
            do k = 1_int64, n
                vi(k) = mod(k * 5_int64, 8_int64)
                vr(k) = real(mod(k * 7_int64, 11_int64), real64)
                ! Every thirteenth row is a NaN, so the middle tier is populated at every size from
                ! 17 upward -- and absent below it, which is itself worth covering.
                if (mod(k, 13_int64) == 0_int64) vr(k) = ieee_value(1.0_real64, ieee_quiet_nan)
                write (nstr, "(i0)") mod(k * 3_int64, 17_int64)
                vs(k) = trim(nstr)
                valid(k) = (mod(k, 5_int64) /= 0_int64)
            end do
            write (nstr, "(i0)") n
            do id = 0, 1
                do inf = 0, 1
                    desc = (id == 1)
                    nf = (inf == 1)
                    tag = " n=" // trim(nstr) // " desc=" // merge("T", "F", desc) // &
                        " nf=" // merge("T", "F", nf)
                    !
                    call keys%clear()
                    call keys%add(vi, descending=desc, nulls_first=nf, is_valid=valid)
                    call engine_ab(error, "int" // tag, keys, n)
                    if (allocated(error)) return
                    !
                    call keys%clear()
                    call keys%add(vr, descending=desc, nulls_first=nf, is_valid=valid)
                    call engine_ab(error, "real" // tag, keys, n)
                    if (allocated(error)) return
                    !
                    call keys%clear()
                    call keys%add(vs, descending=desc, nulls_first=nf, is_valid=valid)
                    call engine_ab(error, "str" // tag, keys, n)
                    if (allocated(error)) return
                    !
                    ! Multi-key, with the null-bearing key SECOND: the tier test has to be applied
                    ! per key rather than per row, and a sort that hoisted it would still agree with
                    ! the C++ engine on every single-key fixture above.
                    call keys%clear()
                    call keys%add(vi, descending=desc)
                    call keys%add(vr, nulls_first=nf, is_valid=valid)
                    call keys%add(vs, descending=.not. desc)
                    call engine_ab(error, "multi" // tag, keys, n)
                    if (allocated(error)) return
                end do
            end do
            deallocate(vi, vr, vs, valid)
        end do
    end subroutine test_fortran_engine_ab_families
    !
    !> The input shapes a quicksort degenerates on, at a size where degenerating would be visible.
    !!
    !! Already-sorted, reverse-sorted, all-equal and organ-pipe are not exotic -- they are what real
    !! column data looks like -- and each is a classic O(n^2) trapdoor for a naive pivot choice. This
    !! asserts the ANSWER rather than the running time, because a correctness test cannot see a
    !! quadratic sort; what it does catch is a median-of-three or a partition that mishandles a run
    !! of equal elements, which is the same code the degenerate shapes exercise.
    subroutine test_fortran_engine_adversarial(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first disagreement.
        integer(int64), parameter :: n = 2000_int64
        integer(int64) :: v(n)
        type(pf_sort_keys) :: keys
        integer(int64) :: k
        integer :: shape_id
        character(len=16) :: label
        !
        do shape_id = 1, 4
            select case (shape_id)
            case (1)
                label = "sorted"
                do k = 1_int64, n
                    v(k) = k
                end do
            case (2)
                label = "reversed"
                do k = 1_int64, n
                    v(k) = n - k + 1_int64
                end do
            case (3)
                label = "all-equal"
                v = 7_int64
            case default
                label = "organ-pipe"
                do k = 1_int64, n
                    v(k) = min(k, n - k + 1_int64)
                end do
            end select
            ! The counting fast path would take the C++ arm for every one of these -- they are all
            ! dense small ranges -- so it is turned off, leaving both arms comparison sorts over the
            ! same degenerate shape. That is the comparison this test is for.
            call parquet_set_sort_counting_path(.false.)
            call keys%clear()
            call keys%add(v)
            call engine_ab(error, trim(label), keys, n)
            call parquet_set_sort_counting_path(.true.)
            if (allocated(error)) return
        end do
    end subroutine test_fortran_engine_adversarial
    !
    !> The heapsort fallback, forced, must produce the same permutation as the quicksort path.
    !!
    !! **It is unreachable without the hook.** Median-of-three pivoting plus a limit of
    !! `2*floor(log2(n))` means ordinary data never approaches the depth at which the fallback fires,
    !! so every mutation to `sort_heapsort`/`sort_sift_down` would survive the whole suite. Forcing
    !! the limit to zero makes the very first oversized range heapsort instead.
    !!
    !! Both halves of the A/B run under the forced limit, and the C++ engine ignores it entirely --
    !! so the C++ arm is an unchanged reference and any difference is the fallback's.
    subroutine test_fortran_engine_heapsort_fallback(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first disagreement.
        integer(int64), parameter :: n = 500_int64
        real(real64) :: v(n)
        logical :: valid(n)
        type(pf_sort_keys) :: keys
        integer(int64) :: k
        integer :: id
        logical :: desc
        !
        do k = 1_int64, n
            v(k) = real(mod(k * 31_int64, 97_int64), real64)
            if (mod(k, 29_int64) == 0_int64) v(k) = ieee_value(1.0_real64, ieee_quiet_nan)
            valid(k) = (mod(k, 11_int64) /= 0_int64)
        end do
        !
        ! The negative control: the same fixture through the ordinary quicksort path first. If the
        ! forced-limit run below were silently taking that same path, this pair would still pass --
        ! but `test_fortran_engine_depth_limit_bites` is what rules that out.
        call keys%clear()
        call keys%add(v, is_valid=valid)
        call engine_ab(error, "heap control", keys, n)
        if (allocated(error)) return
        !
        call parquet_debug_set_sort_depth_limit(0)
        do id = 0, 1
            desc = (id == 1)
            call keys%clear()
            call keys%add(v, descending=desc, is_valid=valid)
            call engine_ab(error, "heapsort desc=" // merge("T", "F", desc), keys, n)
            if (allocated(error)) then
                call parquet_debug_set_sort_depth_limit(-1)
                return
            end if
        end do
        call parquet_debug_set_sort_depth_limit(-1)
    end subroutine test_fortran_engine_heapsort_fallback
    !
    !> The quicksort must leave every element within the cutoff of its place — not merely sortable.
    !!
    !! **This is the only test in the suite that can see a broken heapsort.** The final insertion
    !! pass is a complete sort, so it repairs whatever `sort_introsort_loop` leaves behind and the
    !! permutation comes out correct either way — mutation testing confirmed it, with a sift-down
    !! whose comparison was inverted surviving every conformance test above. What that cannot fake is
    !! the invariant the quicksort exists to establish, and the largest shift the insertion pass
    !! performs is that invariant made visible.
    !!
    !! It is one-sided, and deliberately not more: an insertion pass only moves elements leftward, so
    !! a defect that leaves an element slightly too far RIGHT is invisible here. That case was
    !! measured (a partition returning `cut + 1`) and found to be genuinely correct at O(1) amortised
    !! extra cost, so there is nothing to catch — see the engine's own notes.
    !!
    !! Asserted on both paths — the ordinary quicksort and the forced heapsort fallback — because
    !! they establish it by entirely different means, and the fallback establishes it exactly (it
    !! leaves the range fully ordered, so a correct heapsort shifts nothing at all).
    subroutine test_fortran_engine_presort_invariant(error)
        type(error_type), allocatable, intent(out) :: error !! set on failure.
        integer(int64), parameter :: n = 1500_int64
        integer(int64), parameter :: cutoff = 16_int64 !! SORT_INSERTION_CUTOFF; private to the engine.
        real(real64) :: v(n)
        type(pf_sort_keys) :: keys
        integer(int64), allocatable :: perm(:)
        integer(int64) :: k, shift_quick, shift_heap
        !
        do k = 1_int64, n
            v(k) = real(mod(k * 7919_int64, 4001_int64), real64)
        end do
        call keys%add(v)
        call parquet_debug_use_fortran_sort_engine(.true.)
        !
        call parquet_debug_set_sort_track_shift(.true.)
        call pf_argsort(keys, perm)
        shift_quick = parquet_debug_sort_max_insertion_shift()
        !
        call parquet_debug_set_sort_depth_limit(0)
        call parquet_debug_set_sort_track_shift(.true.)
        call pf_argsort(keys, perm)
        shift_heap = parquet_debug_sort_max_insertion_shift()
        !
        call parquet_debug_set_sort_track_shift(.false.)
        call parquet_debug_set_sort_depth_limit(-1)
        call parquet_debug_use_fortran_sort_engine(.false.)
        !
        ! Nonzero on the way up as well as bounded on the way down: a tracker that never fired would
        ! satisfy the bound trivially, and this fixture is scrambled enough that the insertion pass
        ! must have something to do.
        call check(error, shift_quick > 0_int64, &
            "the largest-shift tracker recorded nothing, so it is not measuring the insertion pass")
        if (allocated(error)) return
        call check(error, shift_quick <= cutoff, &
            "the quicksort left an element further than the insertion cutoff from its place")
        if (allocated(error)) return
        call check(error, shift_heap == 0_int64, &
            "the forced heapsort fallback left the range unordered for the insertion pass to repair")
    end subroutine test_fortran_engine_presort_invariant
    !
    !> The depth-limit hook must actually change which code runs, or the test above proves nothing.
    !!
    !! **Both paths answer identically -- that is the point of them -- so no assertion on the
    !! permutation can tell them apart.** A hook that forced nothing would leave
    !! `test_fortran_engine_heapsort_fallback` comparing the quicksort path against itself, passing
    !! while covering none of `sort_heapsort`/`sort_sift_down`. The heapsort call counter is the only
    !! observable that separates them, which is why it exists.
    !!
    !! Both directions are asserted, because a counter that only ever went up would satisfy the
    !! forced half on its own.
    subroutine test_fortran_engine_depth_limit_bites(error)
        type(error_type), allocatable, intent(out) :: error !! set on failure.
        integer(int64), parameter :: n = 300_int64
        integer(int64) :: v(n)
        type(pf_sort_keys) :: keys
        integer(int64), allocatable :: perm(:)
        integer(int64) :: k, heap_normal, heap_forced
        !
        ! High-cardinality, so the C++ side would decline the counting path anyway; the Fortran side
        ! has no counting path at all yet, so this only has to be a size that partitions.
        do k = 1_int64, n
            v(k) = mod(k * 17_int64, 251_int64)
        end do
        call keys%add(v)
        call parquet_debug_use_fortran_sort_engine(.true.)
        !
        ! The computed limit, i.e. what ships: 2*floor(log2(300)) = 16, which 300 random-ish rows
        ! come nowhere near. Setting -1 both restores the computed limit and zeroes the counter.
        call parquet_debug_set_sort_depth_limit(-1)
        call pf_argsort(keys, perm)
        heap_normal = parquet_debug_sort_heapsort_calls()
        !
        call parquet_debug_set_sort_depth_limit(0)
        call pf_argsort(keys, perm)
        heap_forced = parquet_debug_sort_heapsort_calls()
        call parquet_debug_set_sort_depth_limit(-1)
        call parquet_debug_use_fortran_sort_engine(.false.)
        !
        call check(error, heap_normal == 0_int64, &
            "the ordinary path entered the heapsort fallback, so the depth limit is far too small")
        if (allocated(error)) return
        call check(error, heap_forced > 0_int64, &
            "forcing the depth limit to zero did not reach the heapsort fallback")
    end subroutine test_fortran_engine_depth_limit_bites
    !
end module test_sorting
