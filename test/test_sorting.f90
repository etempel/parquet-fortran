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
    ! NARROW imports, not `use parquet`. Every test in this file is Arrow-free by construction --
    ! the 55 that were not now live in test_sorting_cpp.f90 -- and naming the tiers is what keeps
    ! it that way: a future test reaching a reader or writer is a build error here rather than a
    ! silent loss of this file's place in an undef-safe runner. See feature_tests.md §8.1.
    use parquet_sorting
    use parquet_columns
    use parquet_strings
    use parquet_temporal
    ! The C++ sort engine is TEST-ONLY and is not re-exported by the `parquet` facade: reaching it
    ! needs this import, which is what keeps it out of every other program's dependency graph.
    ! Importing it is also what BINDS it -- `parquet_debug_use_fortran_sort_engine` lives here and
    ! registers the engine's entry points as a side effect of being called.
    use iso_fortran_env, only : int32, int64, real32, real64
    use ieee_arithmetic, only : ieee_value, ieee_quiet_nan, ieee_is_nan, ieee_positive_inf, ieee_negative_inf
    use testdrive, only : new_unittest, unittest_type, error_type, check, skip_test
#ifdef _OPENMP
    ! Used by every test that states its own precondition: a guard, or a threaded design, that is
    ! about teams has nothing to prove on a build or a machine that can never open one.
    ! `omp_get_num_procs` rather than `omp_get_max_threads` for the engine tests below, because that
    ! is the quantity the engine clamps an explicit `threads=` against (see
    ! `sort_build_permutation_threaded`, src/parquet_argsort_engine.f90): on a one-processor machine
    ! `threads=4` resolves to 1 and no threaded design is entered.
    use omp_lib, only : omp_get_max_threads, omp_in_parallel, omp_get_num_procs
#endif
    !
    implicit none
    private
    public :: collect_tests_parquet_sorting
    ! Shared with test_sorting_cpp, which holds the tests that reach the C++ layer and so had to
    ! move to their own file (see that file's header). Public only for that reason: these five are
    ! pure fixture builders, used by both halves, and duplicating them would be two copies of a
    ! fixture that has to agree for the A/B tests on the other side to mean anything.
    public :: itoa, negative_zero, radix_tag, str_prefix_column, ties_fixture
    ! Also shared: engine_only_introsort is pure Fortran, and test_sorting_cpp's own threshold
    ! helper layers the C++ floor on top of force_fortran_parallel_threshold rather than
    ! duplicating the Fortran half. force_fortran_bucket_split goes with it -- the C++ sweep there
    ! restores the two engines' floors together, and the two engines decompose differently.
    public :: engine_only_introsort, force_fortran_parallel_threshold, force_fortran_bucket_split
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
            new_unittest("partial_sort equals a truncated full sort", test_partial_matches_full), &
            new_unittest("n is clamped, not refused", test_partial_clamps), &
            new_unittest("partial_argsort agrees with argsort", test_partial_argsort), &
            new_unittest("partial descending gives the last N", test_partial_descending), &
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
            new_unittest("the bulk searches agree with the scalar ones, target for target", &
                test_search_many_matches_scalar), &
            new_unittest("the bulk searches handle strings, nulls and a descending order", &
                test_search_many_strings_and_nulls), &
            new_unittest("a bulk search with no targets returns without touching the results", &
                test_search_many_empty), &
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
            new_unittest("float distinctness is exact", test_unique_float_exact), &
            new_unittest("a string column's distinct values", test_unique_string_column), &
            new_unittest("the three rank methods differ on ties", test_rank_methods), &
            new_unittest("a null ranks 0", test_rank_nulls_zero), &
            new_unittest("ordinal ranks invert argsort", test_rank_inverts_argsort), &
            new_unittest("descending ranks from the top", test_rank_descending), &
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
            new_unittest("pf_minmax ok= reports an empty population, every type", &
                test_minmax_ok_every_specific), &
            new_unittest("pf_nth_quantile ok= reports an empty population, every type", &
                test_quantile_ok_every_specific), &
            new_unittest("pf_argminmax: every type x both index kinds", test_argminmax_every_specific), &
            new_unittest("pf_merge: every type", test_merge_every_specific), &
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
            new_unittest("partial_argsort: threads= never changes the answer", &
                test_partial_argsort_threads_answer), &
            new_unittest("selection: threads= never changes what partial_sort/nth/quantile answer", &
                test_selection_threads_answer), &
            new_unittest("a selection's two routes agree, and both are reachable", &
                test_selection_routes_agree), &
            new_unittest("a selection's ordering route really opens a team", &
                test_selection_ordering_threads), &
            new_unittest("a threaded sort equals the serial one", test_threads_identical), &
            new_unittest("no team is opened one level down, on either arm", test_nested_team_guard), &
            new_unittest("unique and rank take threads too", test_threads_on_derived), &
            new_unittest("the grouped path really opens the team threads= asked for", &
                test_group_offsets_threads), &
            new_unittest("a threaded grouped sort equals the serial one", &
                test_group_offsets_threads_identical), &
            new_unittest("every size from 2 to 400 threads identically", test_split_size_sweep), &
            new_unittest("a threaded sort is still a permutation at every size", test_split_sweep_is_permutation), &
            new_unittest("the threaded split survives its extreme inputs", test_split_boundary_extremes), &
            new_unittest("strings and multi-key split identically", test_split_key_families), &
            new_unittest("the LSD chain splits identically at every size", test_split_lsd_size_sweep), &
            new_unittest("nulls and NaNs split identically", test_split_tiers), &
            new_unittest("partial_argsort: the threaded tail passes agree with the serial ones", &
                test_partial_argsort_threads_tail), &
            new_unittest("selection: the threaded extraction agrees with the serial one", &
                test_selection_threads_extraction), &
            new_unittest("engine: the Fortran engine threads without changing its answer", &
                test_fortran_engine_threading), &
            new_unittest("engine: the radix path really runs, and only above its floor", &
                test_radix_path_runs), &
            new_unittest("engine: the executed-pass counter reports the passes the skip removed", &
                test_radix_pass_counter), &
            new_unittest("engine: the depth-limit hook really reaches the heapsort fallback", &
                test_fortran_engine_depth_limit_bites), &
            new_unittest("engine: the quicksort leaves every element within the insertion cutoff", &
                test_fortran_engine_presort_invariant), &
            new_unittest("engine: the refinement floor scales with the team and declines small tasks", &
                test_engine_refine_floor), &
            new_unittest("engine: the threading floor scales with the team and declines small columns", &
                test_engine_thread_floor), &
            new_unittest("engine: a small team still takes the counting path on a narrow range", &
                test_counting_small_team), &
            new_unittest("engine: every team-only allocation fallback declines without changing " // &
                "the answer", test_radix_alloc_fallback_selectors), &
            new_unittest("engine: Design B refines a sub-bucket that is itself oversized", &
                test_engine_refine_two_levels), &
            new_unittest("the threaded real32 and logical extractions agree with the serial ones", &
                test_tail_extraction_real32_logical), &
            new_unittest("the threaded date and time extractions agree with the serial ones", &
                test_tail_extraction_date_time), &
            new_unittest("pf_match/pf_match_all/pf_in agree with a brute-force oracle", &
                test_match_basic), &
            new_unittest("a null matches nothing, on either side, including another null", &
                test_match_nulls), &
            new_unittest("a NaN matches a NaN, and -0.0 matches +0.0", &
                test_match_nan_and_zero), &
            new_unittest("pf_match answers the same shape for every element type", &
                test_match_all_types), &
            new_unittest("pf_match and pf_match_all handle every empty-input combination", &
                test_match_empty), &
            new_unittest("two character arrays of different widths match on their content", &
                test_match_string_widths), &
            new_unittest("a pf_sort_keys built from an EMPTY array reports zero rows", &
                test_keys_empty_row_count) &
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
    !> `threads=` on `pf_partial_argsort` is a performance control and nothing else -- the same
    !! contract `pf_argsort` carries, asserted across the whole generic.
    !!
    !! **What this one asserts is API SURFACE, and it deliberately does not skip without OpenMP.**
    !! Every specific of the generic must accept the argument -- the four shapes below are the four
    !! that exist (array/`pf_sort_keys` x int32/int64 permutation) -- and none may answer
    !! differently for it. That claim is meaningful on a serial build too, where it says the
    !! argument is accepted and ignored correctly.
    !!
    !! **It is NOT evidence that anything ran in parallel**, and cannot be: at this size the tail
    !! floor (`tail_team`, src/parquet_argsort_kernel.f90) declines a team whatever is asked for.
    !! `test_partial_argsort_threads_tail` is the one that forces the threaded passes to execute.
    subroutine test_partial_argsort_threads_answer(error)
        type(error_type), allocatable, intent(out) :: error
        integer(int32) :: v(8) = [30, 10, 50, 20, 60, 40, 15, 55]
        integer(int32) :: mag(8) = [3, 1, 3, 1, 3, 1, 2, 2]
        type(pf_sort_keys) :: k
        integer(int32), allocatable :: ref32(:), got32(:)
        integer(int64), allocatable :: ref64(:), got64(:)
        !
        call pf_partial_argsort(v, ref32, 4, threads=1)
        call pf_partial_argsort(v, got32, 4, threads=4)
        call check(error, all(got32 == ref32), &
            "array form, int32 perm: threads= must not change the answer")
        if (allocated(error)) return
        call pf_partial_argsort(v, got32, 4)
        call check(error, all(got32 == ref32), &
            "array form, int32 perm: omitting threads= must not change it either")
        if (allocated(error)) return
        !
        call pf_partial_argsort(v, ref64, 4, threads=1)
        call pf_partial_argsort(v, got64, 4, threads=4)
        call check(error, all(got64 == ref64), &
            "array form, int64 perm: threads= must not change the answer")
        if (allocated(error)) return
        call check(error, all(int(ref32, int64) == ref64), &
            "the int32 and int64 array forms must still agree under threads=")
        if (allocated(error)) return
        !
        call k%add(mag)
        call k%add(v)
        call pf_partial_argsort(k, ref64, 3, threads=1)
        call pf_partial_argsort(k, got64, 3, threads=4)
        call check(error, all(got64 == ref64), &
            "pf_sort_keys form, int64 perm: threads= must not change the answer")
        if (allocated(error)) return
        call pf_partial_argsort(k, got32, 3, threads=4)
        call check(error, all(int(got32, int64) == ref64), &
            "pf_sort_keys form, int32 perm: threads= must not change the answer")
    end subroutine test_partial_argsort_threads_answer
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
    !> The bulk forms exist to make m searches cost O(n + m log n) instead of O(m*n) -- every
    !> `search_impl_*` re-extracts the whole array, so a LOOP of scalar searches is quadratic.
    !>
    !> **The scalar form is the oracle**, which is the strongest one available: the two run the same
    !> comparator through the same binary search and differ only in where the target row sits, so any
    !> disagreement is an indexing defect in the bulk path. Every target class is swept together --
    !> below the array, above it, in a gap, on a duplicated value, and on the endpoints.
    subroutine test_search_many_matches_scalar(error)
        type(error_type), allocatable, intent(out) :: error
        integer(int32) :: v(9) = [10, 20, 20, 20, 30, 40, 40, 50, 60]
        integer(int32) :: tg(13)
        integer(int64) :: lo_m(13), hi_m(13), f_m(13), l_m(13)
        integer(int64) :: lo1, hi1, f1, l1
        integer :: k

        do k = 1, 13
            tg(k) = int(5*k, int32)
        end do
        call pf_lower_bound(v, tg, lo_m)
        call pf_upper_bound(v, tg, hi_m)
        call pf_equal_range(v, tg, f_m, l_m)
        do k = 1, 13
            call pf_lower_bound(v, tg(k), lo1)
            call pf_upper_bound(v, tg(k), hi1)
            call pf_equal_range(v, tg(k), f1, l1)
            call check(error, lo_m(k) == lo1, "bulk pf_lower_bound must equal the scalar form")
            if (allocated(error)) return
            call check(error, hi_m(k) == hi1, "bulk pf_upper_bound must equal the scalar form")
            if (allocated(error)) return
            call check(error, f_m(k) == f1 .and. l_m(k) == l1, &
                "bulk pf_equal_range must equal the scalar form")
            if (allocated(error)) return
        end do
        ! An independent oracle as well, so a shared defect in both forms cannot hide here.
        do k = 1, 13
            call check(error, lo_m(k) == 1 + count(v < tg(k)), &
                "bulk pf_lower_bound must equal 1 + count(v < target)")
            if (allocated(error)) return
        end do
    end subroutine test_search_many_matches_scalar
    !
    !> The three cases the numeric sweep above cannot reach: a `character` array (whose targets are
    !> padded to the ARRAY's element length, per target), a null-bearing array, and a descending
    !> order. All three are checked against the scalar form, which is the same oracle.
    subroutine test_search_many_strings_and_nulls(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=4) :: s(5) = ["aa  ", "bb  ", "bb  ", "cc  ", "dd  "]
        character(len=2) :: st(3) = ["bb", "zz", "aa"]
        integer(int32) :: d(5) = [50, 40, 30, 20, 10]
        integer(int32) :: dt(2) = [35, 50]
        ! The null sits LAST because nulls_first defaults to .false., so that is the order this
        ! array has to already be in -- a null in the middle is genuinely unsorted and is refused.
        integer(int32) :: nv(5) = [10, 20, 40, 50, 30]
        logical :: ok(5) = [.true., .true., .true., .true., .false.]
        integer(int32) :: nt(2) = [30, 40]
        integer(int64) :: fm(3), lm(3), f1, l1, pm(2), p1
        integer :: k

        ! A shorter target than the array's element length: each is blank-padded to len(values).
        call pf_equal_range(s, st, fm, lm)
        do k = 1, 3
            call pf_equal_range(s, st(k), f1, l1)
            call check(error, fm(k) == f1 .and. lm(k) == l1, &
                "bulk pf_equal_range on a character array must equal the scalar form")
            if (allocated(error)) return
        end do
        call check(error, fm(1) == 2 .and. lm(1) == 3, "'bb' must bracket both of its elements")
        if (allocated(error)) return
        ! descending=.true. selects the comparison; it does not reorder anything.
        call pf_lower_bound(d, dt, pm, descending=.true.)
        do k = 1, 2
            call pf_lower_bound(d, dt(k), p1, descending=.true.)
            call check(error, pm(k) == p1, "bulk pf_lower_bound must equal the scalar form when descending")
            if (allocated(error)) return
        end do
        ! A null is outside the value tier, so it must not shift a value's insertion point.
        call pf_lower_bound(nv, nt, pm, is_valid=ok)
        do k = 1, 2
            call pf_lower_bound(nv, nt(k), p1, is_valid=ok)
            call check(error, pm(k) == p1, "bulk pf_lower_bound must equal the scalar form with nulls")
            if (allocated(error)) return
        end do
    end subroutine test_search_many_strings_and_nulls
    !
    !> Zero targets is a legitimate call, not an edge case to abort on -- a caller filtering a target
    !> list can legitimately filter it empty. It must also not extract or order-check anything.
    subroutine test_search_many_empty(error)
        type(error_type), allocatable, intent(out) :: error
        integer(int32) :: v(3) = [1, 2, 3]
        integer(int32) :: none(0)
        integer(int64) :: pos(0), f(0), l(0)

        call pf_lower_bound(v, none, pos)
        call pf_equal_range(v, none, f, l)
        call check(error, size(pos) == 0 .and. size(f) == 0, &
            "a bulk search with no targets must return no results and not abort")
    end subroutine test_search_many_empty
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
        !> Lower bound, upper bound, and the two ends of the equal range -- held as one array of
        !> four rather than four scalars so the call below needs no `[lo32, up32, ...]`
        !> constructor. `sweep_search_ok` takes them explicit-shape, and an array CONSTRUCTOR
        !> passed to such a dummy is argument-associated through a compiler-created temporary,
        !> which ifx reports as `forrtl: warning (406)` -- with a traceback, on every one of these
        !> twenty calls -- under the debug profile's `-check arg_temp_created`.
        integer(int32) :: g32(4)
        integer(int64) :: g64(4)
        integer :: k

        block
            integer(int32) :: v(SWN), t
            v = 10_int32 * int(SWSRT, int32)
            t = 20_int32
            call pf_lower_bound(v, t, g32(1))
            call pf_lower_bound(v, t, g64(1))
            call pf_upper_bound(v, t, g32(2))
            call pf_upper_bound(v, t, g64(2))
            call pf_equal_range(v, t, g32(3), g32(4))
            call pf_equal_range(v, t, g64(3), g64(4))
            call check(error, sweep_search_ok(g32, g64, WANT), &
                "int32 searches must bracket the duplicated class in both index kinds")
        end block
        if (allocated(error)) return
        block
            integer(int64) :: v(SWN), t
            v = 100_int64 * int(SWSRT, int64)
            t = 200_int64
            call pf_lower_bound(v, t, g32(1))
            call pf_lower_bound(v, t, g64(1))
            call pf_upper_bound(v, t, g32(2))
            call pf_upper_bound(v, t, g64(2))
            call pf_equal_range(v, t, g32(3), g32(4))
            call pf_equal_range(v, t, g64(3), g64(4))
            call check(error, sweep_search_ok(g32, g64, WANT), &
                "int64 searches must bracket the duplicated class in both index kinds")
        end block
        if (allocated(error)) return
        block
            real(real32) :: v(SWN), t
            v = real(SWSRT, real32) + 0.5_real32
            t = 2.5_real32
            call pf_lower_bound(v, t, g32(1))
            call pf_lower_bound(v, t, g64(1))
            call pf_upper_bound(v, t, g32(2))
            call pf_upper_bound(v, t, g64(2))
            call pf_equal_range(v, t, g32(3), g32(4))
            call pf_equal_range(v, t, g64(3), g64(4))
            call check(error, sweep_search_ok(g32, g64, WANT), &
                "real32 searches must bracket the duplicated class in both index kinds")
        end block
        if (allocated(error)) return
        block
            real(real64) :: v(SWN), t
            logical :: mask(SWN)
            v = real(SWSRT, real64) + 0.25_real64
            t = 2.25_real64
            call pf_lower_bound(v, t, g32(1))
            call pf_lower_bound(v, t, g64(1))
            call pf_upper_bound(v, t, g32(2))
            call pf_upper_bound(v, t, g64(2))
            call pf_equal_range(v, t, g32(3), g32(4))
            call pf_equal_range(v, t, g64(3), g64(4))
            call check(error, sweep_search_ok(g32, g64, WANT), &
                "real64 searches must bracket the duplicated class in both index kinds")
            if (allocated(error)) return
            ! A null sorts last, so it is outside the searched region entirely: marking the final
            ! element null must not move any of the four answers.
            mask = .true.
            mask(SWN) = .false.
            call pf_equal_range(v, t, g32(3), g32(4), is_valid=mask, assume_sorted=.true.)
            call check(error, g32(3) == 2_int32 .and. g32(4) == 3_int32, &
                "a trailing null must leave a range that ends before it untouched")
        end block
        if (allocated(error)) return
        block
            call pf_lower_bound(SWSRTB, .true., g32(1))
            call pf_lower_bound(SWSRTB, .true., g64(1))
            call pf_upper_bound(SWSRTB, .true., g32(2))
            call pf_upper_bound(SWSRTB, .true., g64(2))
            call pf_equal_range(SWSRTB, .true., g32(3), g32(4))
            call pf_equal_range(SWSRTB, .true., g64(3), g64(4))
            call check(error, sweep_search_ok(g32, g64, WANTB), &
                "logical searches must bracket the .true. run in both index kinds")
        end block
        if (allocated(error)) return
        block
            character(len=2) :: v(SWN)
            do k = 1, SWN
                v(k) = swchr(SWSRT(k))
            end do
            call pf_lower_bound(v, swchr(2), g32(1))
            call pf_lower_bound(v, swchr(2), g64(1))
            call pf_upper_bound(v, swchr(2), g32(2))
            call pf_upper_bound(v, swchr(2), g64(2))
            call pf_equal_range(v, swchr(2), g32(3), g32(4))
            call pf_equal_range(v, swchr(2), g64(3), g64(4))
            call check(error, sweep_search_ok(g32, g64, WANT), &
                "character searches must bracket the duplicated class in both index kinds")
        end block
        if (allocated(error)) return
        block
            type(parquet_date) :: v(SWN), t
            call v%set_raw(1000_int32 + int(SWSRT, int32))
            call t%set_raw(1002_int32)
            call pf_lower_bound(v, t, g32(1))
            call pf_lower_bound(v, t, g64(1))
            call pf_upper_bound(v, t, g32(2))
            call pf_upper_bound(v, t, g64(2))
            call pf_equal_range(v, t, g32(3), g32(4))
            call pf_equal_range(v, t, g64(3), g64(4))
            call check(error, sweep_search_ok(g32, g64, WANT), &
                "parquet_date searches must bracket the duplicated class in both index kinds")
        end block
        if (allocated(error)) return
        block
            type(parquet_time) :: v(SWN), t
            call v%set_raw(2000_int64 + int(SWSRT, int64))
            call t%set_raw(2002_int64)
            call pf_lower_bound(v, t, g32(1))
            call pf_lower_bound(v, t, g64(1))
            call pf_upper_bound(v, t, g32(2))
            call pf_upper_bound(v, t, g64(2))
            call pf_equal_range(v, t, g32(3), g32(4))
            call pf_equal_range(v, t, g64(3), g64(4))
            call check(error, sweep_search_ok(g32, g64, WANT), &
                "parquet_time searches must bracket the duplicated class in both index kinds")
        end block
        if (allocated(error)) return
        block
            type(parquet_timestamp) :: v(SWN), t
            call v%set_raw(3000_int64 + int(SWSRT, int64), 7_int32)
            call t%set_raw(3002_int64, 7_int32)
            call pf_lower_bound(v, t, g32(1))
            call pf_lower_bound(v, t, g64(1))
            call pf_upper_bound(v, t, g32(2))
            call pf_upper_bound(v, t, g64(2))
            call pf_equal_range(v, t, g32(3), g32(4))
            call pf_equal_range(v, t, g64(3), g64(4))
            call check(error, sweep_search_ok(g32, g64, WANT), &
                "parquet_timestamp searches must bracket the duplicated class in both index kinds")
        end block
        if (allocated(error)) return
        block
            type(parquet_string_column) :: sc
            do k = 1, SWN
                call sc%append_string(swchr(SWSRT(k)))
            end do
            call pf_lower_bound(sc, swchr(2), g32(1))
            call pf_lower_bound(sc, swchr(2), g64(1))
            call pf_upper_bound(sc, swchr(2), g32(2))
            call pf_upper_bound(sc, swchr(2), g64(2))
            call pf_equal_range(sc, swchr(2), g32(3), g32(4))
            call pf_equal_range(sc, swchr(2), g64(3), g64(4))
            call check(error, sweep_search_ok(g32, g64, WANT), &
                "parquet_string_column searches must bracket the duplicated class in both index kinds")
            if (allocated(error)) return
            block
                ! A descending search needs descending input -- the same fixture reversed, where
                ! class 2's run sits at 4..5 rather than at 2..3.
                type(parquet_string_column) :: rev
                do k = SWN, 1, -1
                    call rev%append_string(swchr(SWSRT(k)))
                end do
                call pf_lower_bound(rev, swchr(2), g32(1), descending=.true.)
                call pf_upper_bound(rev, swchr(2), g32(2), descending=.true.)
                call pf_equal_range(rev, swchr(2), g32(3), g32(4), descending=.true.)
                call check(error, g32(1) == 4_int32 .and. g32(2) == 6_int32 .and. g32(3) == 4_int32 .and. &
                    g32(4) == 5_int32, &
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
    !> **`pf_minmax`'s `ok=` on every specific, in BOTH directions.** The all-null arm asserts the
    !> report; the one-value arm is the negative control, without which a guard that answered
    !> `.false.` unconditionally would pass every assertion above it.
    !>
    !> The abort direction -- `ok` absent, same input, message unchanged -- is not here and cannot
    !> be: it kills the process. It is the `sorting_minmax_all_null*` error scenarios, which
    !> predate this argument and are what makes "omitting `ok` restores the abort" a tested claim
    !> rather than a stated one.
    !>
    !> Nothing reads `lo`/`hi` after a `.false.`: the contract says they were not written.
    subroutine test_minmax_ok_every_specific(error)
        type(error_type), allocatable, intent(out) :: error
        logical :: ok
        logical :: mask(2)

        mask = [.false., .false.]
        block
            integer(int32) :: v(2), lo, hi
            v = [7_int32, 9_int32]
            call pf_minmax(v, lo, hi, is_valid=mask, ok=ok)
            call check(error, .not. ok, "int32 pf_minmax must report ok=.false. for an all-null array")
            if (allocated(error)) return
            call pf_minmax(v, lo, hi, ok=ok)
            call check(error, ok .and. lo == 7_int32 .and. hi == 9_int32, &
                "int32 pf_minmax must report ok=.true. and the extremes when a value exists")
        end block
        if (allocated(error)) return
        block
            integer(int64) :: v(2), lo, hi
            v = [7_int64, 9_int64]
            call pf_minmax(v, lo, hi, is_valid=mask, ok=ok)
            call check(error, .not. ok, "int64 pf_minmax must report ok=.false. for an all-null array")
            if (allocated(error)) return
            call pf_minmax(v, lo, hi, ok=ok)
            call check(error, ok .and. lo == 7_int64 .and. hi == 9_int64, &
                "int64 pf_minmax must report ok=.true. and the extremes when a value exists")
        end block
        if (allocated(error)) return
        block
            real(real32) :: v(2), lo, hi
            ! A NaN is excluded from this population exactly as a null is, so an all-NaN array
            ! with no mask at all must report .false. too -- the other half of "null or NaN".
            v = ieee_value(1.0_real32, ieee_quiet_nan)
            call pf_minmax(v, lo, hi, ok=ok)
            call check(error, .not. ok, "real32 pf_minmax must report ok=.false. for an all-NaN array")
            if (allocated(error)) return
            v = [7.5_real32, 9.5_real32]
            call pf_minmax(v, lo, hi, ok=ok)
            call check(error, ok .and. lo == 7.5_real32 .and. hi == 9.5_real32, &
                "real32 pf_minmax must report ok=.true. and the extremes when a value exists")
        end block
        if (allocated(error)) return
        block
            real(real64) :: v(2), lo, hi
            v = ieee_value(1.0_real64, ieee_quiet_nan)
            call pf_minmax(v, lo, hi, ok=ok)
            call check(error, .not. ok, "real64 pf_minmax must report ok=.false. for an all-NaN array")
            if (allocated(error)) return
            v = [7.5_real64, 9.5_real64]
            call pf_minmax(v, lo, hi, is_valid=[.true., .false.], ok=ok)
            call check(error, ok .and. lo == 7.5_real64 .and. hi == 7.5_real64, &
                "real64 pf_minmax must report ok=.true. when only PART of the array is null")
        end block
        if (allocated(error)) return
        block
            character(len=2) :: v(2)
            character(len=:), allocatable :: lo, hi
            v = ["aa", "bb"]
            call pf_minmax(v, lo, hi, is_valid=mask, ok=ok)
            call check(error, .not. ok, "character pf_minmax must report ok=.false. for an all-null array")
            if (allocated(error)) return
            call check(error, .not. allocated(lo) .and. .not. allocated(hi), &
                "character pf_minmax must leave its results unallocated when ok is .false.")
            if (allocated(error)) return
            call pf_minmax(v, lo, hi, ok=ok)
            call check(error, ok .and. lo == "aa" .and. hi == "bb", &
                "character pf_minmax must report ok=.true. and the ends when a value exists")
        end block
        if (allocated(error)) return
        block
            type(parquet_date) :: v(2), lo, hi, nulls(2)
            ! A temporal element carries its own nullness, so there is no `is_valid` to pass:
            ! a default-initialised array IS the all-null population.
            call v%set_raw([1001_int32, 1006_int32])
            call pf_minmax(nulls, lo, hi, ok=ok)
            call check(error, .not. ok, "parquet_date pf_minmax must report ok=.false. for an all-null array")
            if (allocated(error)) return
            call check(error, lo%is_null() .and. hi%is_null(), &
                "parquet_date pf_minmax must leave its results default-initialised when ok is .false.")
            if (allocated(error)) return
            call pf_minmax(v, lo, hi, ok=ok)
            call check(error, ok .and. lo%raw() == 1001_int32 .and. hi%raw() == 1006_int32, &
                "parquet_date pf_minmax must report ok=.true. and the ends when a value exists")
        end block
        if (allocated(error)) return
        block
            type(parquet_time) :: v(2), lo, hi, nulls(2)
            call v%set_raw([2001_int64, 2006_int64])
            call pf_minmax(nulls, lo, hi, ok=ok)
            call check(error, .not. ok, "parquet_time pf_minmax must report ok=.false. for an all-null array")
            if (allocated(error)) return
            call pf_minmax(v, lo, hi, ok=ok)
            call check(error, ok .and. lo%raw() == 2001_int64 .and. hi%raw() == 2006_int64, &
                "parquet_time pf_minmax must report ok=.true. and the ends when a value exists")
        end block
        if (allocated(error)) return
        block
            type(parquet_timestamp) :: v(2), lo, hi, nulls(2)
            integer(int64) :: gs1, gs2
            integer(int32) :: gn1, gn2
            call v%set_raw([3001_int64, 3006_int64], 7_int32)
            call pf_minmax(nulls, lo, hi, ok=ok)
            call check(error, .not. ok, &
                "parquet_timestamp pf_minmax must report ok=.false. for an all-null array")
            if (allocated(error)) return
            call pf_minmax(v, lo, hi, ok=ok)
            call lo%get_raw(gs1, gn1)
            call hi%get_raw(gs2, gn2)
            call check(error, ok .and. gs1 == 3001_int64 .and. gs2 == 3006_int64, &
                "parquet_timestamp pf_minmax must report ok=.true. and the ends when a value exists")
        end block
        if (allocated(error)) return
        block
            type(parquet_string_column) :: sc
            character(len=:), allocatable :: lo, hi
            ! The packed store owns its own validity, so the nulls are appended rather than masked.
            call sc%append_null()
            call sc%append_null()
            call pf_minmax(sc, lo, hi, ok=ok)
            call check(error, .not. ok, &
                "parquet_string_column pf_minmax must report ok=.false. for an all-null column")
            if (allocated(error)) return
            call sc%append_string("aa")
            call pf_minmax(sc, lo, hi, ok=ok)
            call check(error, ok .and. lo == "aa" .and. hi == "aa", &
                "parquet_string_column pf_minmax must report ok=.true. once one value is present")
        end block
    end subroutine test_minmax_ok_every_specific
    !
    !> **`pf_nth_quantile`'s `ok=` on every specific, in BOTH directions**, plus the two things the
    !> minmax twin has no counterpart for: `n_null` is set even when `ok` is `.false.` (so a caller
    !> taking the reporting route still learns why), and the index forms return without touching
    !> `index`, which is what the `i32`/`i64` specifics would otherwise read from an unset local.
    !>
    !> The abort direction is `sorting_quantile_all_null`, as above.
    subroutine test_quantile_ok_every_specific(error)
        type(error_type), allocatable, intent(out) :: error
        logical :: ok
        logical :: mask(2)
        integer(int64) :: nn

        mask = [.false., .false.]
        block
            integer(int32) :: v(2), q
            integer(int32) :: idx32
            v = [7_int32, 9_int32]
            call pf_nth_quantile(v, 0.5_real64, q, is_valid=mask, n_null=nn, ok=ok)
            call check(error, .not. ok, "int32 pf_nth_quantile must report ok=.false. for an all-null array")
            if (allocated(error)) return
            call check(error, nn == 2_int64, &
                "pf_nth_quantile must still report n_null on the ok=.false. path")
            if (allocated(error)) return
            call pf_nth_quantile(v, 0.0_real64, q, idx32, is_valid=mask, ok=ok)
            call check(error, .not. ok, &
                "the int32-index pf_nth_quantile specific must report ok=.false. too")
            if (allocated(error)) return
            call pf_nth_quantile(v, 0.0_real64, q, ok=ok)
            call check(error, ok .and. q == 7_int32, &
                "int32 pf_nth_quantile must report ok=.true. and the value when one exists")
        end block
        if (allocated(error)) return
        block
            integer(int64) :: v(2), q, idx64
            v = [7_int64, 9_int64]
            call pf_nth_quantile(v, 0.5_real64, q, is_valid=mask, ok=ok)
            call check(error, .not. ok, "int64 pf_nth_quantile must report ok=.false. for an all-null array")
            if (allocated(error)) return
            call pf_nth_quantile(v, 1.0_real64, q, idx64, is_valid=mask, ok=ok)
            call check(error, .not. ok, &
                "the int64-index pf_nth_quantile specific must report ok=.false. too")
            if (allocated(error)) return
            call pf_nth_quantile(v, 1.0_real64, q, idx64, ok=ok)
            call check(error, ok .and. q == 9_int64 .and. idx64 == 2_int64, &
                "int64 pf_nth_quantile must report ok=.true., the value and its index when one exists")
        end block
        if (allocated(error)) return
        block
            real(real32) :: v(2), q
            v = [7.5_real32, 9.5_real32]
            call pf_nth_quantile(v, 0.5_real64, q, is_valid=mask, ok=ok)
            call check(error, .not. ok, "real32 pf_nth_quantile must report ok=.false. for an all-null array")
            if (allocated(error)) return
            call pf_nth_quantile(v, 0.0_real64, q, ok=ok)
            call check(error, ok .and. q == 7.5_real32, &
                "real32 pf_nth_quantile must report ok=.true. and the value when one exists")
        end block
        if (allocated(error)) return
        block
            real(real64) :: v(2), q
            v = [7.5_real64, 9.5_real64]
            call pf_nth_quantile(v, 0.5_real64, q, is_valid=mask, ok=ok)
            call check(error, .not. ok, "real64 pf_nth_quantile must report ok=.false. for an all-null array")
            if (allocated(error)) return
            ! A NaN is a VALUE here, unlike in pf_minmax: pf_nth_quantile excludes nulls only.
            call pf_nth_quantile(v, 0.0_real64, q, is_valid=[.true., .false.], n_null=nn, ok=ok)
            call check(error, ok .and. q == 7.5_real64 .and. nn == 1_int64, &
                "real64 pf_nth_quantile must report ok=.true. when only PART of the array is null")
        end block
        if (allocated(error)) return
        block
            logical :: v(2), q
            v = [.true., .true.]
            call pf_nth_quantile(v, 0.5_real64, q, is_valid=mask, ok=ok)
            call check(error, .not. ok, "logical pf_nth_quantile must report ok=.false. for an all-null array")
            if (allocated(error)) return
            call pf_nth_quantile(v, 0.5_real64, q, ok=ok)
            call check(error, ok .and. q, &
                "logical pf_nth_quantile must report ok=.true. and the value when one exists")
        end block
        if (allocated(error)) return
        block
            character(len=2) :: v(2)
            character(len=:), allocatable :: q
            v = ["aa", "bb"]
            call pf_nth_quantile(v, 0.5_real64, q, is_valid=mask, ok=ok)
            call check(error, .not. ok, &
                "character pf_nth_quantile must report ok=.false. for an all-null array")
            if (allocated(error)) return
            call check(error, .not. allocated(q), &
                "character pf_nth_quantile must leave its result unallocated when ok is .false.")
            if (allocated(error)) return
            call pf_nth_quantile(v, 0.0_real64, q, ok=ok)
            call check(error, ok .and. q == "aa", &
                "character pf_nth_quantile must report ok=.true. and the value when one exists")
        end block
        if (allocated(error)) return
        block
            type(parquet_date) :: v(2), q, nulls(2)
            call v%set_raw([1001_int32, 1006_int32])
            call pf_nth_quantile(nulls, 0.5_real64, q, ok=ok)
            call check(error, .not. ok, &
                "parquet_date pf_nth_quantile must report ok=.false. for an all-null array")
            if (allocated(error)) return
            call check(error, q%is_null(), &
                "parquet_date pf_nth_quantile must leave its result default-initialised when ok is .false.")
            if (allocated(error)) return
            call pf_nth_quantile(v, 0.0_real64, q, ok=ok)
            call check(error, ok .and. q%raw() == 1001_int32, &
                "parquet_date pf_nth_quantile must report ok=.true. and the value when one exists")
        end block
        if (allocated(error)) return
        block
            type(parquet_time) :: v(2), q, nulls(2)
            call v%set_raw([2001_int64, 2006_int64])
            call pf_nth_quantile(nulls, 0.5_real64, q, ok=ok)
            call check(error, .not. ok, &
                "parquet_time pf_nth_quantile must report ok=.false. for an all-null array")
            if (allocated(error)) return
            call pf_nth_quantile(v, 0.0_real64, q, ok=ok)
            call check(error, ok .and. q%raw() == 2001_int64, &
                "parquet_time pf_nth_quantile must report ok=.true. and the value when one exists")
        end block
        if (allocated(error)) return
        block
            type(parquet_timestamp) :: v(2), q, nulls(2)
            integer(int64) :: gs
            integer(int32) :: gn
            call v%set_raw([3001_int64, 3006_int64], 7_int32)
            call pf_nth_quantile(nulls, 0.5_real64, q, ok=ok)
            call check(error, .not. ok, &
                "parquet_timestamp pf_nth_quantile must report ok=.false. for an all-null array")
            if (allocated(error)) return
            call pf_nth_quantile(v, 0.0_real64, q, ok=ok)
            call q%get_raw(gs, gn)
            call check(error, ok .and. gs == 3001_int64, &
                "parquet_timestamp pf_nth_quantile must report ok=.true. and the value when one exists")
        end block
        if (allocated(error)) return
        block
            type(parquet_string_column) :: sc
            character(len=:), allocatable :: q
            call sc%append_null()
            call sc%append_null()
            call pf_nth_quantile(sc, 0.5_real64, q, n_null=nn, ok=ok)
            call check(error, .not. ok, &
                "parquet_string_column pf_nth_quantile must report ok=.false. for an all-null column")
            if (allocated(error)) return
            call check(error, nn == 2_int64, &
                "parquet_string_column pf_nth_quantile must still report n_null on the ok=.false. path")
            if (allocated(error)) return
            call sc%append_string("aa")
            call pf_nth_quantile(sc, 0.0_real64, q, ok=ok)
            call check(error, ok .and. q == "aa", &
                "parquet_string_column pf_nth_quantile must report ok=.true. once one value is present")
        end block
        ! **Every INDEX specific's own `ok` path, all twenty of them.** The blocks above cover the
        ! no-index form for all ten element types and two index forms besides; the remaining
        ! eighteen `if (.not. ok) return` lines are in specifics nothing else calls with `ok`, and
        ! a coverage run is what found them. Each one guards a read of an index the worker never
        ! set, so an omitted guard is a use of an undefined value rather than a wrong answer --
        ! which is exactly the class no assertion elsewhere would notice.
        block
            integer(int32) :: xi(2), qi, i32
            integer(int64) :: xl(2), ql, i64
            real(real32) :: xs(2), qs
            real(real64) :: xd(2), qd
            logical :: xb(2), qb
            character(len=2) :: xc(2)
            character(len=:), allocatable :: qc
            type(parquet_date) :: dnull(2), qdt
            type(parquet_time) :: tnull(2), qtm
            type(parquet_timestamp) :: snull(2), qts
            type(parquet_string_column) :: sc

            xi = [7_int32, 9_int32]
            xl = [7_int64, 9_int64]
            xs = [7.5_real32, 9.5_real32]
            xd = [7.5_real64, 9.5_real64]
            xb = [.true., .true.]
            xc = ["aa", "bb"]
            call sc%append_null()
            call sc%append_null()

            call pf_nth_quantile(xi, 0.5_real64, qi, i32, is_valid=mask, ok=ok)
            call check(error, .not. ok, "quantile_i32_i32 must report ok=.false.")
            if (allocated(error)) return
            call pf_nth_quantile(xi, 0.5_real64, qi, i64, is_valid=mask, ok=ok)
            call check(error, .not. ok, "quantile_i32_i64 must report ok=.false.")
            if (allocated(error)) return
            call pf_nth_quantile(xl, 0.5_real64, ql, i32, is_valid=mask, ok=ok)
            call check(error, .not. ok, "quantile_i64_i32 must report ok=.false.")
            if (allocated(error)) return
            call pf_nth_quantile(xl, 0.5_real64, ql, i64, is_valid=mask, ok=ok)
            call check(error, .not. ok, "quantile_i64_i64 must report ok=.false.")
            if (allocated(error)) return
            call pf_nth_quantile(xs, 0.5_real64, qs, i32, is_valid=mask, ok=ok)
            call check(error, .not. ok, "quantile_f32_i32 must report ok=.false.")
            if (allocated(error)) return
            call pf_nth_quantile(xs, 0.5_real64, qs, i64, is_valid=mask, ok=ok)
            call check(error, .not. ok, "quantile_f32_i64 must report ok=.false.")
            if (allocated(error)) return
            call pf_nth_quantile(xd, 0.5_real64, qd, i32, is_valid=mask, ok=ok)
            call check(error, .not. ok, "quantile_f64_i32 must report ok=.false.")
            if (allocated(error)) return
            call pf_nth_quantile(xd, 0.5_real64, qd, i64, is_valid=mask, ok=ok)
            call check(error, .not. ok, "quantile_f64_i64 must report ok=.false.")
            if (allocated(error)) return
            call pf_nth_quantile(xb, 0.5_real64, qb, i32, is_valid=mask, ok=ok)
            call check(error, .not. ok, "quantile_bool_i32 must report ok=.false.")
            if (allocated(error)) return
            call pf_nth_quantile(xb, 0.5_real64, qb, i64, is_valid=mask, ok=ok)
            call check(error, .not. ok, "quantile_bool_i64 must report ok=.false.")
            if (allocated(error)) return
            call pf_nth_quantile(xc, 0.5_real64, qc, i32, is_valid=mask, ok=ok)
            call check(error, .not. ok, "quantile_chr_i32 must report ok=.false.")
            if (allocated(error)) return
            call pf_nth_quantile(xc, 0.5_real64, qc, i64, is_valid=mask, ok=ok)
            call check(error, .not. ok, "quantile_chr_i64 must report ok=.false.")
            if (allocated(error)) return
            call pf_nth_quantile(dnull, 0.5_real64, qdt, i32, ok=ok)
            call check(error, .not. ok, "quantile_date_i32 must report ok=.false.")
            if (allocated(error)) return
            call pf_nth_quantile(dnull, 0.5_real64, qdt, i64, ok=ok)
            call check(error, .not. ok, "quantile_date_i64 must report ok=.false.")
            if (allocated(error)) return
            call pf_nth_quantile(tnull, 0.5_real64, qtm, i32, ok=ok)
            call check(error, .not. ok, "quantile_time_i32 must report ok=.false.")
            if (allocated(error)) return
            call pf_nth_quantile(tnull, 0.5_real64, qtm, i64, ok=ok)
            call check(error, .not. ok, "quantile_time_i64 must report ok=.false.")
            if (allocated(error)) return
            call pf_nth_quantile(snull, 0.5_real64, qts, i32, ok=ok)
            call check(error, .not. ok, "quantile_ts_i32 must report ok=.false.")
            if (allocated(error)) return
            call pf_nth_quantile(snull, 0.5_real64, qts, i64, ok=ok)
            call check(error, .not. ok, "quantile_ts_i64 must report ok=.false.")
            if (allocated(error)) return
            call pf_nth_quantile(sc, 0.5_real64, qc, i32, ok=ok)
            call check(error, .not. ok, "quantile_strcol_i32 must report ok=.false.")
            if (allocated(error)) return
            call pf_nth_quantile(sc, 0.5_real64, qc, i64, ok=ok)
            call check(error, .not. ok, "quantile_strcol_i64 must report ok=.false.")
            if (allocated(error)) return

            ! The control for the whole block: with a value present, an index form must report
            ! `.true.` AND set the index. Without this, a guard that returned unconditionally
            ! would satisfy every assertion above.
            call pf_nth_quantile(xd, 0.0_real64, qd, i64, ok=ok)
            call check(error, ok .and. i64 == 1_int64 .and. qd == 7.5_real64, &
                "an index form must still report ok=.true., the value and the index when one exists")
        end block
    end subroutine test_quantile_ok_every_specific
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
    !> Whether `x` is a NEGATIVE zero, decided by its sign bit rather than by `sign()`.
    !>
    !> **`sign(1.0_real64, x) < 0.0` is NOT a portable test for this**, which is what it took an ifx
    !> failure to notice. F2018 16.9.180 makes `SIGN(A, B)` with a zero `B` processor-dependent: a
    !> processor that does not distinguish negative zero returns `|A|`, so the idiom is entitled to
    !> answer `+1.0` for a genuine `-0.0`. gfortran and flang both distinguish it; ifx does not, so
    !> the old form reported a correct merge as broken on one compiler out of three.
    !>
    !> Reading the sign bit has no such licence — it is the definition of the thing being asked.
    !> Note this deliberately does not use `ieee_is_negative`, which would be equally correct but
    !> would add an import for one call and answers a slightly wider question.
    logical function negative_zero(x) result(neg)
        real(real64), intent(in) :: x !! the value to probe.
        neg = (x == 0.0_real64) .and. transfer(x, 0_int64) < 0_int64
    end function negative_zero
    !
    !> A string column whose values share a prefix past the radix window and then split — the shape
    !! that makes the deep refine allocate its scatter buffer at all.
    subroutine str_prefix_column(col, n)
        type(parquet_string_column), intent(inout) :: col !! receives `n` values.
        integer(int64), intent(in) :: n                   !! rows to build.
        integer(int64) :: k
        character(len=8) :: num
        !
        call col%clear()
        do k = 1_int64, n
            write (num, "(i8.8)") int(mod(k * 7_int64, 211_int64))
            call col%append_string("sharedprefix" // num)
        end do
    end subroutine str_prefix_column
    !
    !> Names the flag combination in a radix test's failure message.
    function radix_tag(desc, nf) result(tag)
        logical, intent(in) :: desc !! the key's `descending` flag.
        logical, intent(in) :: nf   !! the key's `nulls_first` flag.
        character(len=16) :: tag    !! " desc=T nf=F" and the like.
        !
        tag = " desc=" // merge("T", "F", desc) // " nf=" // merge("T", "F", nf)
    end function radix_tag
    !
    ! ---- C++-engine pins ------------------------------------------------------------------
    !
    ! Every test wrapped below observes a counter the C++ engine populates and the Fortran one
    ! does not -- `engine_comparisons` and `parquet_debug_sort_merge_threads_used`. Stage 6 made
    ! the Fortran engine the default, so each of these went from testing something to testing
    ! nothing -- and every one of them FAILED loudly rather than passing vacuously, because each
    ! carries the "this arm must really reach the path" control this project requires. That is the
    ! controls working exactly as intended.
    !
    ! **`parquet_debug_sort_threads_used` is NOT in that class and must not be added to it.**
    ! `sort_build_permutation_impl` (src/parquet_argsort_engine.f90) sets `dbg_sort_threads_used`
    ! on every build, so the Fortran engine populates it too -- and its own doc-comment
    ! (src/parquet_argsort.f90) calls it "the only way a test can see either" of the engine's two
    ! team decisions. Listing it here once read as though no Fortran-side threading assertion were
    ! possible, which is the opposite of the truth: `test_selection_ordering_threads` below relies
    ! on exactly that. See feature_doc_sorting.md's S1.
    !
    ! Pinning is the right fix rather than re-pointing them at Fortran observables, because the
    ! C++ engine still ships and is still user-reachable: `parquet_open_reader(..., sort_by=)`
    ! and `parquet_reader_set_sort` call `sort_build_permutation_threaded` directly, with no
    ! engine selector anywhere in that path. These are that engine's only tests.
    !
    ! The wrapper shape (rather than a pin at the top of each body) is deliberate: these tests
    ! have up to five early `return`s, and a selector leaked on one of them would not fail the
    ! test that leaked it -- it would silently change which engine a LATER test measures.

    !
    !> `threads=` on `pf_partial_sort`, `pf_nth_element` and `pf_nth_quantile` must never change
    !! the answer, whatever it is set to. The cheap half of the pair below.
    !!
    !! Every value is distinct, so each of the three has exactly one right answer and a
    !! thread-count-dependent one would be visible immediately. `threads=1` is included as the
    !! forced-serial arm rather than assuming the default resolves to it.
    subroutine test_selection_threads_answer(error)
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: n = 512
        real(real64) :: v(n)
        real(real64), allocatable :: s_auto(:), s_one(:), s_many(:)
        real(real64) :: nth_auto, nth_one, nth_many, q_auto, q_one, q_many
        integer :: i

        do i = 1, n
            v(i) = real(mod(i * 7919, 5003), real64) * 0.25_real64
        end do

        call pf_partial_sort(v, s_auto, 16)
        call pf_partial_sort(v, s_one, 16, threads=1)
        call pf_partial_sort(v, s_many, 16, threads=4)
        call check(error, all(s_auto == s_one) .and. all(s_auto == s_many), &
            "pf_partial_sort must give the same first n whatever threads= is set to")
        if (allocated(error)) return

        call pf_nth_element(v, 37, nth_auto)
        call pf_nth_element(v, 37, nth_one, threads=1)
        call pf_nth_element(v, 37, nth_many, threads=4)
        call check(error, nth_auto == nth_one .and. nth_auto == nth_many, &
            "pf_nth_element must give the same value whatever threads= is set to")
        if (allocated(error)) return

        call pf_nth_quantile(v, 0.75_real64, q_auto)
        call pf_nth_quantile(v, 0.75_real64, q_one, threads=1)
        call pf_nth_quantile(v, 0.75_real64, q_many, threads=4)
        call check(error, q_auto == q_one .and. q_auto == q_many, &
            "pf_nth_quantile must give the same value whatever threads= is set to")
    end subroutine test_selection_threads_answer
    !
    !
    !> **A selection's ORDERING route really opens a thread team.** That is what `threads=` means on
    !! `pf_nth_element` and `pf_nth_quantile` once the array is past `SORT_NTH_ORDER_MIN` and is
    !! answered by ordering rather than by quickselect -- the claim
    !! doc/pages/utilities/sorting.md makes under "What threads= reaches in a selection".
    !!
    !! **Nothing else covers it, and the obvious candidate cannot.** `test_selection_threads_answer`
    !! asserts only that the answer is the same at `threads=1`, `4` and auto -- which
    !! `sort_row_less`'s row-index tiebreaker guarantees whatever the team size, so it would pass
    !! just as happily against a selection that had stopped threading altogether.
    !! `test_selection_routes_agree` proves the ordering ROUTE is taken, via the radix pass count,
    !! and says nothing about a TEAM. `parquet_debug_sort_threads_used` is the only observable that
    !! can: `sort_build_permutation_impl` (src/parquet_argsort_engine.f90) sets it on every build,
    !! the Fortran engine included -- see the "C++-engine pins" banner further down, which must not
    !! be read as saying otherwise.
    !!
    !! **The engine team floor has to be forced, and this is the Risk-49 trap.** A team is declined
    !! below `max(32768, 1024*nt)` rows, which no fixture here reaches, so without
    !! `parquet_debug_set_sort_engine_min_rows` every arm below runs the same serial code and every
    !! assertion holds for the wrong reason. Measured while writing this: at 4096 elements
    !! `threads=4` resolves to **1** until that floor is lowered. `SORT_NTH_ORDER_MIN` is 256, well
    !! below this fixture, so the ordering route is taken without forcing anything.
    !!
    !! **Two controls, and the second deliberately reads a DIFFERENT counter.** `threads=1` on the
    !! same array must report 1, so the observable tracks the request rather than being a constant.
    !! The route control asserts the quickselect arm opens no team -- and `dbg_sort_threads_used` is
    !! NOT zeroed per call, so on that arm it still reports whatever the previous ordering left
    !! (measured 8). Only the radix pass count has a reset, so that is what the route control reads,
    !! exactly as `test_selection_routes_agree` does.
    subroutine test_selection_ordering_threads(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first disagreement.
        integer, parameter :: n = 4096 !! above SORT_NTH_ORDER_MIN, so the ordering route is taken.
        real(real64) :: v(n)
        real(real64) :: val                        !! the rank's value; not what this test is about.
        integer(int64) :: seen_four, seen_one, seen_q !! teams resolved on the three ordering arms.
        integer(int64) :: npass                    !! radix passes the quickselect arm reached.
        integer :: i
        !
        ! Preconditions, declared rather than assumed: with the team preprocessed out, or on a
        ! machine where an explicit threads= clamps back to 1, every arm below is the same serial
        ! code and each assertion passes without testing anything.
#ifndef _OPENMP
        call skip_test(error, "needs OpenMP: sort_build_permutation_threaded opens its team " // &
            "inside #ifdef _OPENMP, so every arm below would resolve to one thread and the " // &
            "assertions would hold for the wrong reason")
        return
#else
        if (omp_get_num_procs() < 2) then
            call skip_test(error, "needs at least two processors: resolve_thread_count clamps " // &
                "an explicit threads= to omp_get_num_procs(), so threads=4 resolves to 1 here")
            return
        end if
#endif
        do i = 1, n
            v(i) = real(mod(i * 7919, 5003), real64) * 0.25_real64
        end do
        !
        ! Lowered and restored around the three calls, BEFORE the first assertion: every `check`
        ! can return early, and a leaked floor would silently rethread every later test here.
        call parquet_debug_set_sort_engine_min_rows(1_int64)
        call pf_nth_element(v, 37, val, threads=4)
        seen_four = parquet_debug_sort_threads_used()
        call pf_nth_element(v, 37, val, threads=1)
        seen_one = parquet_debug_sort_threads_used()
        call pf_nth_quantile(v, 0.5_real64, val, threads=4)
        seen_q = parquet_debug_sort_threads_used()
        call parquet_debug_set_sort_engine_min_rows(-1_int64)
        !
        call check(error, seen_four == 4_int64, &
            "pf_nth_element's ordering route must open the team threads= asked for")
        if (allocated(error)) return
        ! The control: without it a policy that ignored threads= entirely would pass the line above
        ! on any machine with four processors.
        call check(error, seen_one == 1_int64, &
            "pf_nth_element with threads=1 must report one thread, not whatever the machine has")
        if (allocated(error)) return
        call check(error, seen_q == 4_int64, &
            "pf_nth_quantile's ordering route must open the team threads= asked for")
        if (allocated(error)) return
        !
        ! Route control: forced BELOW the ordering floor the quickselect arm runs, which drives no
        ! engine build and so no team. It cannot be observed through the thread counter -- that one
        ! is not zeroed per call and would still read 4 from the arms above -- so the radix pass
        ! count, which does have a reset, is what says the arm was really reached.
        call parquet_debug_set_sort_nth_order_min(huge(0_int64))
        call parquet_debug_set_sort_engine_min_rows(1_int64)
        call parquet_debug_set_sort_radix_min_rows(2_int64)
        call parquet_debug_reset_sort_radix_passes()
        call pf_nth_element(v, 37, val, threads=4)
        npass = parquet_debug_sort_radix_passes()
        call parquet_debug_set_sort_radix_min_rows(-1_int64)
        call parquet_debug_set_sort_engine_min_rows(-1_int64)
        call parquet_debug_set_sort_nth_order_min(-1_int64)
        call check(error, npass == 0_int64, &
            "below the ordering floor a selection must quickselect, reaching no radix pass at all")
    end subroutine test_selection_ordering_threads
    !
    !
    !> The refinement floor must scale with the team, and must decline a task too small to thread.
    !!
    !! **What this protects.** A refinement pass is THREADED, so `sort_radix_count_range_par` and
    !! `sort_radix_scatter_range_par` dispatch the whole team over one task's range. Refining a range
    !! that gives each thread only a few hundred elements pays a full barrier for almost no work, and
    !! machine B measured that costing **2.0x at 16 threads, 6.1x at 32 and 10.6x at 64** on a 32768-row
    !! column -- the damage growing with the team, which is exactly why the floor is
    !! `SORT_REFINE_ELEMS_PER_THREAD * nt` and not the flat 4096 it replaced.
    !!
    !! **The fixture has to collide in the top byte and differ lower down.** Refinement subdivides; it
    !! cannot manufacture distinctions the key does not have, so a key whose distinct values each own
    !! their own top byte leaves every post-split bucket internally constant and NOTHING to refine --
    !! the loop would drop `tdmax` through the constant digits and never scatter, and both arms below
    !! would report the same bucket count for the wrong reason. Three top-byte groups each holding
    !! thirteen values that differ in digit 6 is what gives refinement something to do.
    !!
    !! **Both arms are asserted, and the forced one is the vacuity control**: with the floor forced to
    !! 1 the same key must refine, or the shipped-floor assertion is passing against a build that
    !! never refines anything.
    subroutine test_engine_refine_floor(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        integer(int64), parameter :: n = 16384_int64
        integer(int64) :: v(n)
        integer(int64), allocatable :: ref(:), got_shipped(:), got_forced(:)
        integer(int64) :: shipped_buckets, forced_buckets, shipped_design
        integer(int64) :: i, g
        ! **Preconditions, declared rather than assumed.** The floor under test is measured against the team size
        ! (`SORT_REFINE_ELEMS_PER_THREAD * nt`), and both arms below compare bucket counts that only a
        ! team produces.
        ! Where no team can be opened the assertions are not merely untestable but VACUOUS:
        ! they would pass just as happily against a library that had stopped threading
        ! altogether. Skipping says so out loud, which a silent pass would not. Same reasoning
        ! and same shape as `test_nested_team_guard`.
#ifndef _OPENMP
        call skip_test(error, "needs OpenMP: without it the threaded sort designs are " // &
            "preprocessed out entirely -- the Design A/B dispatch in " // &
            "src/parquet_argsort_engine.f90 sits inside #ifdef _OPENMP -- so no team is " // &
            "ever opened and every assertion below would be vacuous")
        return
#else
        if (omp_get_num_procs() < 2) then
            call skip_test(error, "needs at least two processors: the engine clamps an " // &
                "explicit threads= to omp_get_num_procs(), so it resolves to 1 here and no " // &
                "threaded design is entered")
            return
        end if
#endif
        !
        ! Group 0 takes 6000 rows, groups 1 and 2 the rest. With four threads the shipped floor is
        ! 2048*4 = 8192 and `nv/team` is 4096, so the target is 8192 and the largest bucket (6000)
        ! sits UNDER it -- no refinement. Forcing the floor to 1 drops the target to 4096, which
        ! every one of the three buckets exceeds.
        do i = 1_int64, n
            if (i <= 6000_int64) then
                g = 0_int64
            else
                g = 1_int64 + mod(i, 2_int64)
            end if
            v(i) = ishft(g, 56) + ishft(mod(i, 13_int64), 48)
        end do
        !
        ! **Every piece of global state is captured and RESTORED before the first assertion.** Each
        ! `check` below can `return`, and this suite shares one process with the settings suite -- a
        ! leaked engine selection was measured breaking two settings tests that never mention sorting
        ! designs at all, which is the kind of failure that gets debugged in the wrong file.
        call force_fortran_parallel_threshold(1_int64)
        call pf_argsort(v, ref, threads=1)
        !
        call pf_argsort(v, got_shipped, threads=4)
        shipped_buckets = parquet_debug_sort_split_buckets()
        shipped_design = parquet_debug_sort_design()
        !
        call parquet_debug_set_sort_task_floor(1_int64)
        call pf_argsort(v, got_forced, threads=4)
        forced_buckets = parquet_debug_sort_split_buckets()
        !
        call parquet_debug_set_sort_task_floor(-1_int64)
        call force_fortran_parallel_threshold(0_int64)
        !
        call check(error, shipped_design == 2_int64, &
            "the fixture must reach Design B, or this test says nothing about its floor")
        if (allocated(error)) return
        call check(error, all(got_shipped == ref), "the shipped floor must not change the answer")
        if (allocated(error)) return
        call check(error, all(got_forced == ref), "refining must not change the answer either")
        if (allocated(error)) return
        call check(error, forced_buckets > shipped_buckets, &
            "forcing the floor to 1 must actually refine, or the shipped-floor assertion is vacuous")
        if (allocated(error)) return
        call check(error, shipped_buckets == 3_int64, &
            "at the shipped floor the three top-byte buckets must survive unrefined")
    end subroutine test_engine_refine_floor


    !
    !> The engine's threading floor must scale with the team, and decline a column too small for it.
    !!
    !! **What this protects.** The floor is `max(32768, 2048 * nt)`, replacing a flat 8192 that was
    !! measured wrong by **4.83x under ifx and 20.11x under gfortran** at their worst points -- the
    !! damage concentrated exactly where a flat number must fail, at small `n` with a large team,
    !! where 8192 rows over 64 threads is 128 rows each and a full barrier to pay for them.
    !!
    !! **The fixture straddles the ABSOLUTE term, not the per-thread one.** At four threads the rule
    !! is `max(32768, 8192)` = 32768, so 20000 rows must run serial and 40000 must thread. A fixture
    !! chosen to straddle `2048 * nt` instead would sit at 8192 rows, where the absolute term decides
    !! and the per-thread term is invisible -- the test would then pass against a rule that had lost
    !! its team scaling entirely.
    !!
    !! **The forced arm is the vacuity control**: with the floor forced to 1 the same 20000-row
    !! column must thread, or the serial assertion above is passing against an engine that never
    !! threads at this size for some other reason.
    subroutine test_engine_thread_floor(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        integer(int64), allocatable :: small(:), big(:), perm(:)
        integer(int64) :: used_small, used_big, used_forced, st, i
        !
        allocate(small(20000), big(40000))
        st = 88172645463325252_int64
        do i = 1_int64, 40000_int64
            st = ieor(st, ishft(st, 13)); st = ieor(st, ishft(st, -7)); st = ieor(st, ishft(st, 17))
            if (i <= 20000_int64) small(i) = st
            big(i) = st
        end do
        !
        call pf_argsort(small, perm, threads=4)
        used_small = fortran_threads_used()
        call pf_argsort(big, perm, threads=4)
        used_big = fortran_threads_used()
        !
        call parquet_debug_set_sort_engine_min_rows(1_int64)
        call pf_argsort(small, perm, threads=4)
        used_forced = fortran_threads_used()
        !
        ! Restored BEFORE the assertions: each `check` can `return`, and this suite shares a process
        ! with the settings suite, where a leaked floor reads as an unrelated failure.
        call parquet_debug_set_sort_engine_min_rows(-1_int64)
        !
        call check(error, used_small == 1_int64, &
            "20000 rows is under max(32768, 2048*4) and must run serial, not open a team")
        if (allocated(error)) return
        call check(error, used_big >= 2_int64, &
            "40000 rows is over the floor and must open a team")
        if (allocated(error)) return
        call check(error, used_forced >= 2_int64, &
            "forcing the floor to 1 must make the small column thread, or the serial check is vacuous")
    end subroutine test_engine_thread_floor


    !
    !> A small team must still take the SERIAL counting sort when the value range is narrow enough.
    !!
    !! **The defect this closes.** The counting path used to be admitted only at `nt <= 1`, so the
    !! moment a team existed a low-cardinality integer key fell counting -> radix -> Design B
    !! declines -> Design A, and landed slower than the serial sort it had just refused. Machine A
    !! measured 2.0x at two threads on gfortran; machine B reproduced 1.46x under ifx, which is what
    !! established it as a real defect rather than an instance of the gfortran radix gap that
    !! `feature_sort_report.md` section 11.3 tracks.
    !!
    !! **The `nt = 4` arm is not decoration.** The ceiling is 2 because that is where the compilers
    !! stop agreeing -- from four threads ifx's radix wins and gfortran's does not -- so a test that
    !! only proved counting is reachable with a team would pass just as happily against a ceiling of
    !! 64, which would be a large regression under ifx.
    !!
    !! **The forced-ceiling arm is the vacuity control.** With the ceiling at 1 the same key at the
    !! same team must reach the radix; without that, the first assertion would pass against an engine
    !! that never reaches the radix here for some unrelated reason.
    subroutine test_counting_small_team(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        integer(int64) :: v(40000)
        integer(int64), allocatable :: ref(:), got2(:), got4(:), gotold(:)
        integer(int64) :: d2, d4, dold, i
        ! **Preconditions, declared rather than assumed.** The question here is which design a SMALL team reaches;
        ! without a team, no design is entered at all.
        ! Where no team can be opened the assertions are not merely untestable but VACUOUS:
        ! they would pass just as happily against a library that had stopped threading
        ! altogether. Skipping says so out loud, which a silent pass would not. Same reasoning
        ! and same shape as `test_nested_team_guard`.
#ifndef _OPENMP
        call skip_test(error, "needs OpenMP: without it the threaded sort designs are " // &
            "preprocessed out entirely -- the Design A/B dispatch in " // &
            "src/parquet_argsort_engine.f90 sits inside #ifdef _OPENMP -- so no team is " // &
            "ever opened and every assertion below would be vacuous")
        return
#else
        if (omp_get_num_procs() < 2) then
            call skip_test(error, "needs at least two processors: the engine clamps an " // &
                "explicit threads= to omp_get_num_procs(), so it resolves to 1 here and no " // &
                "threaded design is entered")
            return
        end if
#endif
        !
        ! 40000 rows clears the engine's own floor (max(32768, 2048*nt)) so a team really opens, and
        ! ten distinct values put the range six orders of magnitude below the n/100 the rule allows
        ! at two threads.
        do i = 1_int64, 40000_int64
            v(i) = mod(i * 7_int64, 10_int64)
        end do
        !
        call pf_argsort(v, ref, threads=1)
        call pf_argsort(v, got2, threads=2)
        d2 = parquet_debug_sort_design()
        call pf_argsort(v, got4, threads=4)
        d4 = parquet_debug_sort_design()
        call parquet_debug_set_sort_counting_max_threads(1_int64)
        call pf_argsort(v, gotold, threads=2)
        dold = parquet_debug_sort_design()
        !
        ! Restored before the assertions: each `check` can `return`, and this suite shares a process
        ! with the settings suite.
        call parquet_debug_set_sort_counting_max_threads(-1_int64)
        !
        call check(error, d2 == 0_int64, &
            "two threads on a narrow range must take the counting path, not fall through to a design")
        if (allocated(error)) return
        call check(error, dold /= 0_int64, &
            "forcing the ceiling to 1 must reach a radix design, or the check above is vacuous")
        if (allocated(error)) return
        call check(error, d4 /= 0_int64, &
            "four threads must DECLINE counting: the ceiling is 2 because ifx's radix wins above it")
        if (allocated(error)) return
        call check(error, all(got2 == ref), "the counting path must give the serial permutation")
        if (allocated(error)) return
        call check(error, all(got4 == ref), "declining counting must not change the answer")
        if (allocated(error)) return
        call check(error, all(gotold == ref), "the forced-radix arm must not change the answer either")
    end subroutine test_counting_small_team


    !
    !> The threaded halves of `pf_partial_argsort` -- the key extraction and the int32 narrowing --
    !! must agree with the serial ones. This is the test that makes `threads=` mean something.
    !!
    !! **Forcing the floor is the whole point.** `tail_team` declines a team below
    !! `max(32768, 1024*nt)` elements, which no test fixture reaches, so without
    !! `parquet_debug_set_sort_tail_min_rows` both arms would run the SAME serial code and the
    !! equality below would hold for the wrong reason -- CLAUDE.md's "a threshold no test-sized
    !! fixture can reach is a threshold no test exercises".
    !!
    !! **What it can and cannot see.** With the floor at 1 and `threads=4`, `extract_i32` and
    !! `narrow_perm` take their `!$omp parallel do` branches, so a plumbing error in either is a
    !! wrong answer here. It cannot assert *that* a team was opened: the tail passes report no
    !! team size, and the engine's own `parquet_debug_sort_threads_used` describes the full sort's
    !! radix, which a partial sort never enters. The selection stays serial by design, so there is
    !! nothing further to observe -- verified instead by mutation (dropping either `threads=` from
    !! the generated body must fail this test).
    subroutine test_partial_argsort_threads_tail(error)
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: n = 4096
        integer(int32) :: v(n)
        logical :: mask(n)
        integer(int32), allocatable :: ref32(:), got32(:)
        integer(int64), allocatable :: ref64(:), got64(:)
        integer(int32), allocatable :: refnull(:), gotnull(:)
        integer :: i
        !
        ! Preconditions, declared rather than assumed: with the threaded branches preprocessed out,
        ! or on a machine where an explicit threads= clamps back to 1, both arms below are the same
        ! serial code and every assertion passes without testing anything.
#ifndef _OPENMP
        call skip_test(error, "needs OpenMP: the threaded extraction and narrowing " // &
            "(extract_* and narrow_perm, src/parquet_argsort_kernel.f90) sit inside " // &
            "#ifdef _OPENMP, so both arms below would run the same serial code and the " // &
            "equality would hold for the wrong reason")
        return
#else
        if (omp_get_num_procs() < 2) then
            call skip_test(error, "needs at least two processors: resolve_thread_count clamps " // &
                "an explicit threads= to omp_get_num_procs(), so threads=4 resolves to 1 here " // &
                "and no tail pass is threaded")
            return
        end if
#endif
        !
        ! Deliberately not sorted, not reverse-sorted, and carrying duplicates, so a narrowing that
        ! dropped or reordered entries cannot coincide with the right answer.
        do i = 1, n
            v(i) = int(mod(i * 7919, 1021), int32)
            mask(i) = (mod(i, 17) /= 0)
        end do
        !
        ! Captured and restored BEFORE the first assertion: every `check` can return early, and a
        ! leaked floor would silently rethread every later test in this suite.
        call parquet_debug_set_sort_tail_min_rows(1_int64)
        call pf_partial_argsort(v, got32, 64, threads=4)
        call pf_partial_argsort(v, got64, 64, threads=4)
        call pf_partial_argsort(v, gotnull, 64, is_valid=mask, threads=4)
        call parquet_debug_set_sort_tail_min_rows(-1_int64)
        !
        call pf_partial_argsort(v, ref32, 64, threads=1)
        call pf_partial_argsort(v, ref64, 64, threads=1)
        call pf_partial_argsort(v, refnull, 64, is_valid=mask, threads=1)
        !
        call check(error, size(got32) == 64, "the forced-floor run must still return n indices")
        if (allocated(error)) return
        call check(error, all(got32 == ref32), &
            "a threaded extraction and narrowing must give the serial int32 answer")
        if (allocated(error)) return
        call check(error, all(got64 == ref64), &
            "a threaded extraction must give the serial int64 answer")
        if (allocated(error)) return
        call check(error, all(int(got32, int64) == got64), &
            "the threaded int32 and int64 forms must agree with each other")
        if (allocated(error)) return
        ! The mask travels through the same threaded extraction as the values, so a null placed by
        ! the wrong thread's chunk is a wrong answer this arm sees and the unmasked ones cannot.
        call check(error, all(gotnull == refnull), &
            "a threaded extraction must place nulls exactly as the serial one does")
    end subroutine test_partial_argsort_threads_tail


    !
    !> **A selection answers by quickselecting or by ordering, and the two must agree.**
    !> `SORT_NTH_ORDER_MIN` is 256 rows, far above every fixture in this suite, so without the
    !> debug override only the quickselect arm would ever run — and the arm that SHIPS for any
    !> array worth selecting from would be untested. That is `feature_risks.md` Risk-49's shape
    !> exactly: a size threshold hiding a whole code path from the tests written for everything
    !> else.
    !>
    !> Both arms are forced on the same fixture and their answers compared **element for element,
    !> at every rank** — not at one probe, because a routing bug that returned a neighbouring rank
    !> would survive a single-probe check. The comparison is against the full sort's own
    !> permutation, so it is an independent expectation rather than one route vouching for the
    !> other.
    !>
    !> The negative control is the last block: with the floor forced high the quickselect arm must
    !> still answer correctly, so a test that had accidentally forced ordering in both arms fails.
    subroutine test_selection_routes_agree(error)
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: NSEL = 64
        real(real64) :: v(NSEL), q_sel, q_ord
        integer(int64), allocatable :: perm(:)
        integer(int64) :: i_sel, i_ord, r
        integer :: k

        ! Deliberately not sorted, with a duplicated value at ranks that differ, so a route that
        ! broke ties differently would report a different INDEX for the same VALUE.
        do k = 1, NSEL
            v(k) = real(mod(k * 37, NSEL), real64)
        end do
        v(7) = v(19)
        call pf_argsort(v, perm)

        do r = 1_int64, int(NSEL, int64)
            call parquet_debug_set_sort_nth_order_min(huge(0_int64))   ! force quickselect
            call pf_nth_element(v, r, q_sel, i_sel)
            call parquet_debug_set_sort_nth_order_min(1_int64)         ! force the ordering route
            call pf_nth_element(v, r, q_ord, i_ord)
            call parquet_debug_set_sort_nth_order_min(-1_int64)
            call check(error, i_sel == perm(r) .and. i_ord == perm(r), &
                "both selection routes must report the index the full sort puts at that rank")
            if (allocated(error)) return
            call check(error, q_sel == v(perm(r)) .and. q_ord == v(perm(r)), &
                "both selection routes must report the value at that rank")
            if (allocated(error)) return
        end do

        ! **The answers alone cannot say WHICH route ran** -- both are correct by construction, so
        ! a change that disabled the routing entirely would pass every assertion above. Confirmed
        ! by mutation: forcing the branch to `.false.` leaves this test green up to here. The radix
        ! pass count is the observable that separates them: the ordering route drives the engine's
        ! radix path, quickselect never touches it.
        call parquet_debug_set_sort_nth_order_min(1_int64)             ! force the ordering route
        ! The radix path has a floor of its own (SORT_RADIX_MIN_ROWS, 128 rows), well above this
        ! 64-row fixture, so it has to be lowered too or the observable reads 0 for the wrong
        ! reason -- which is the same Risk-49 trap one level down.
        call parquet_debug_set_sort_radix_min_rows(2_int64)
        call parquet_debug_reset_sort_radix_passes()
        call pf_nth_element(v, 1_int64, q_ord, i_ord)
        call parquet_debug_set_sort_radix_min_rows(-1_int64)
        call check(error, parquet_debug_sort_radix_passes() > 0_int64, &
            "the ordering route must reach the radix path, which is how it is told from quickselect")
        if (allocated(error)) return

        ! Negative control, in both senses: the quickselect arm really is reachable, it is what
        ! runs below the floor, and it answers correctly there.
        call parquet_debug_set_sort_nth_order_min(huge(0_int64))
        call parquet_debug_set_sort_radix_min_rows(2_int64)
        call parquet_debug_reset_sort_radix_passes()
        call pf_nth_element(v, 1_int64, q_sel, i_sel)
        call parquet_debug_set_sort_radix_min_rows(-1_int64)
        call check(error, parquet_debug_sort_radix_passes() == 0_int64, &
            "quickselect must reach no radix pass at all")
        if (allocated(error)) return
        call parquet_debug_set_sort_nth_order_min(-1_int64)
        call check(error, i_sel == perm(1) .and. q_sel == minval(v), &
            "the quickselect arm must still answer correctly when the ordering floor is out of reach")
    end subroutine test_selection_routes_agree


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
        call force_fortran_parallel_threshold(1000000000_int64)   ! above the size -> serial reference
        call pf_argsort(v, ser, is_valid=mask)
        call force_fortran_parallel_threshold(4_int64)            ! below the size -> parallel
        do t = 2, 8
            call pf_argsort(v, par, is_valid=mask, threads=t)
            call check(error, size(par) == size(ser), "a threaded argsort must return every index")
            if (allocated(error)) exit
            call check(error, all(par == ser), &
                "a threaded permutation must be identical to the serial one, at every thread count")
            if (allocated(error)) exit
        end do
        call force_fortran_parallel_threshold(0_int64)
    end subroutine test_threads_identical


    !
    !> **The rule nothing else observes**, and the one that keeps the rest of this test suite from
    !> oversubscribing: with `threads=` absent, auto takes the machine in a serial region and stays
    !> SERIAL inside a parallel one, because T OpenMP threads each asking for T more would be T*T
    !> threads. An explicit `threads=` is still honoured there -- the caller has said what they want.
    !> Neither half of the nested-team guard may open a team one level down. See Risk-104.
    !!
    !! **A deadlock cannot be asserted directly**, so this asserts the DECISION that leads to one
    !! instead: what the library resolves as its thread count, in a region that exists but runs on
    !! a single thread. That is the state libgomp hangs in, and it is reachable in ordinary user
    !! code -- `!$omp parallel if(cond)` with `cond` false, or any region under
    !! `OMP_NUM_THREADS=1`. `omp_in_parallel()` reads `.false.` there while `omp_get_level()` is 1,
    !! which is exactly why the old predicate missed it.
    !!
    !! Both arms carry a level-0 negative control taken first. Without them the test passes on a
    !! single-core machine, or against a library that had stopped threading altogether, while
    !! proving nothing about the guard.
    subroutine test_nested_team_guard(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed assertion.
        integer(int64), allocatable :: v(:), perm(:)
        integer(int64) :: auto_top, auto_inactive, used_top, used_inactive, st, i
        !
        ! **Preconditions, declared rather than assumed.** This test is about a team that must not
        ! be opened; where no team can be opened at all the property holds trivially and, worse,
        ! the level-0 negative controls below -- the only thing keeping the assertions honest --
        ! cannot be established. Skipping says so out loud, which a silent pass would not.
#ifndef _OPENMP
        call skip_test(error, "needs OpenMP: without it no sort ever opens a team, so the guard " // &
            "under test is satisfied trivially and its negative controls cannot be established")
        return
#else
        if (omp_get_max_threads() < 2) then
            call skip_test(error, "needs at least two OpenMP threads: with one available no sort " // &
                "opens a team anywhere, so the negative controls below cannot be established")
            return
        end if
#endif
        !
        allocate(v(40000))
        st = 88172645463325252_int64
        do i = 1_int64, 40000_int64
            st = ieor(st, ishft(st, 13)); st = ieor(st, ishft(st, -7)); st = ieor(st, ishft(st, 17))
            v(i) = st
        end do
        !
        ! Arm 1: the AUTOMATIC answer, read straight from the public policy function.
        auto_top = int(pf_sort_threads(), int64)
        auto_inactive = -1_int64
        !$omp parallel if(.false.)
        auto_inactive = int(pf_sort_threads(), int64)
        !$omp end parallel
        !
        ! Arm 2: an EXPLICIT threads= reaching the Fortran engine, which is what `resolve_thread_count`
        ! clamps. The C++ engine threads with std::thread rather than OpenMP and is deliberately not
        ! clamped, so this arm has to pin the Fortran one to observe the rule at all.
        call pf_argsort(v, perm, threads=4)
        used_top = fortran_threads_used()
        used_inactive = -1_int64
        !$omp parallel if(.false.)
        call pf_argsort(v, perm, threads=4)
        used_inactive = fortran_threads_used()
        !$omp end parallel
        !
        call check(error, auto_top >= 2_int64, &
            "negative control: the automatic answer must exceed 1 at the top level, or the guard below is vacuous")
        if (allocated(error)) return
        call check(error, auto_inactive == 1_int64, &
            "pf_sort_threads must resolve to 1 inside an inactive parallel region: omp_get_level() is 1 there even " // &
            "though omp_in_parallel() is .false., and a team opened one level down deadlocks libgomp (Risk-104)")
        if (allocated(error)) return
        call check(error, used_top >= 2_int64, &
            "negative control: an explicit threads=4 must open a team at the top level, or the guard below is vacuous")
        if (allocated(error)) return
        call check(error, used_inactive == 1_int64, &
            "an explicit threads= must be clamped to serial inside an INACTIVE parallel region -- the one shape " // &
            "measured deadlocking; an enclosing team of two or more still honours it")
    end subroutine test_nested_team_guard


    !
    !> `threads=` reaches the three operations that sort internally, not just the two that are a
    !> sort -- and their answers must not change either.
    subroutine test_threads_on_derived(error)
        type(error_type), allocatable, intent(out) :: error
        real(real64) :: v(2000)
        real(real64), allocatable :: d1(:), d2(:)
        integer, allocatable :: r1(:), r2(:)
        integer :: c1, c2
        integer(int64) :: seen_ser, seen_par !! teams the two arms actually opened.

        call ties_fixture(v)
        call force_fortran_parallel_threshold(1000000000_int64)
        call pf_unique_count(v, c1)
        call pf_unique(v, d1)
        call pf_rank(v, r1)
        seen_ser = parquet_debug_sort_threads_used()
        call force_fortran_parallel_threshold(4_int64)
        call pf_unique_count(v, c2, threads=8)
        call pf_unique(v, d2, threads=8)
        call pf_rank(v, r2, threads=8)
        seen_par = parquet_debug_sort_threads_used()
        call force_fortran_parallel_threshold(0_int64)
        call check(error, c1 == c2 .and. c1 == 97, "a threaded pf_unique_count must count the same")
        if (allocated(error)) return
        call check(error, size(d1) == size(d2) .and. all(d1 == d2), &
            "a threaded pf_unique must return the same distinct values")
        if (allocated(error)) return
        call check(error, all(r1 == r2), "a threaded pf_rank must produce the same ranks")
        if (allocated(error)) return
        ! **The team assertions are what make the three above mean anything.** Every assertion in
        ! this test compares an answer, and a grouped sort's answer does not depend on the team
        ! size -- so for as long as `engine_build_runs` dropped its resolved count, the `threads=8`
        ! arm ran the identical serial code as the default arm and all three held for the wrong
        ! reason. `seen_par > 1` is the one line that could tell those apart, and `seen_ser == 1`
        ! is its negative control: without it, an implementation that ignored `threads=` and always
        ! opened a full team would pass the first. See feature_risks.md Risk-189.
#ifdef _OPENMP
        if (omp_get_num_procs() >= 2) then
            call check(error, seen_par > 1, &
                "pf_rank(threads=8) must really open a team: the grouped builder has to be " // &
                "handed the resolved thread count, not just have one resolved for it")
            if (allocated(error)) return
            call check(error, seen_ser == 1, &
                "the control: the default arm here has the engine floor raised out of reach, " // &
                "so it must sort serially -- if this reports a team the arms are not an A/B")
        end if
#endif
    end subroutine test_threads_on_derived
    !
    !> The grouped path must open the team `threads=` asked for, exactly as the ungrouped path does.
    !!
    !! **This is the assertion that failed before `engine_build_runs` passed its resolved count on.**
    !! It resolved `nthreads`, handed it to the C++ oracle, and dropped it on the branch that ships
    !! -- so `pf_argsort(..., group_offsets=)`, and with it `pf_match`, `pf_unique`, `pf_rank` and
    !! `parquet_table%join`, sorted serially whatever the caller passed. Nothing else could see it:
    !! the permutation is identical at every team size, so no correctness test can distinguish the
    !! two and `parquet_debug_sort_threads_used` is the only observable. feature_risks.md Risk-189.
    !!
    !! **Three arms.** The grouped path opens what was asked for; `threads=1` still means serial
    !! (without which a policy that ignored `threads=` entirely would pass the first); and the
    !! ungrouped path resolves to the same number on the same data, which is the property the whole
    !! fix is about -- the two drivers must not have separate thread policies.
    subroutine test_group_offsets_threads(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first disagreement.
        integer, parameter :: n = 4096                      !! above the lowered floor, below any cap.
        real(real64) :: v(n)
        integer(int64), allocatable :: perm(:), go(:)
        integer(int64) :: seen_four, seen_one, seen_plain
        integer :: i
        !
        ! Preconditions, declared rather than assumed -- the same pair `test_selection_ordering_threads`
        ! carries, and for the same reason: with the team preprocessed out, or on a one-processor
        ! machine where an explicit threads= clamps back to 1, every arm below is the same serial
        ! code and each assertion passes without testing anything.
#ifndef _OPENMP
        call skip_test(error, "needs OpenMP: sort_build_permutation_threaded opens its team " // &
            "inside #ifdef _OPENMP, so every arm below would resolve to one thread and the " // &
            "assertions would hold for the wrong reason")
        return
#else
        if (omp_get_num_procs() < 2) then
            call skip_test(error, "needs at least two processors: resolve_thread_count clamps " // &
                "an explicit threads= to omp_get_num_procs(), so threads=4 resolves to 1 here")
            return
        end if
#endif
        do i = 1, n
            v(i) = real(mod(i * 7919, 5003), real64) * 0.25_real64
        end do
        !
        ! Lowered and restored around all three calls, BEFORE the first assertion: every `check`
        ! can return early, and a leaked floor would silently rethread every later test here.
        call force_fortran_parallel_threshold(1_int64)
        call pf_argsort(v, perm, group_offsets=go, threads=4)
        seen_four = parquet_debug_sort_threads_used()
        call pf_argsort(v, perm, group_offsets=go, threads=1)
        seen_one = parquet_debug_sort_threads_used()
        call pf_argsort(v, perm, threads=4)
        seen_plain = parquet_debug_sort_threads_used()
        call force_fortran_parallel_threshold(0_int64)
        !
        call check(error, seen_four == 4_int64, &
            "the grouped path must open the team threads= asked for, as the ungrouped one does")
        if (allocated(error)) return
        ! The control: without it a policy that ignored threads= and always opened
        ! omp_get_max_threads() would pass the line above.
        call check(error, seen_one == 1_int64, &
            "threads=1 must still sort the grouped path serially")
        if (allocated(error)) return
        call check(error, seen_plain == seen_four, &
            "grouped and ungrouped must resolve the same team on the same data: one policy, " // &
            "one resolve_thread_count, two drivers")
    end subroutine test_group_offsets_threads
    !
    !> A threaded grouped sort must return the identical permutation AND the identical group offsets.
    !!
    !! The oracle is the one the whole feature rests on: every comparator ends in a row-index
    !! tiebreaker, so no two distinct rows compare equal, exactly one permutation is correct, and a
    !! threaded answer that differs from a serial one is WRONG rather than merely different. The
    !! offsets follow, being a pure function of that permutation.
    !!
    !! **The team is asserted on both arms, and that is not decoration.** An A/B that only compares
    !! answers passes just as happily when both arms ran the same serial code -- which is exactly
    !! what happened here for as long as the grouped builder discarded its thread count, and is the
    !! vacuity trap feature_risks.md Risk-49 describes. Ties are dense on purpose: `group_offsets`
    !! is about where the runs are, so a fixture of distinct values would exercise one group per row.
    subroutine test_group_offsets_threads_identical(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first disagreement.
        integer, parameter :: n = 4096                      !! enough rows for a team to be worth it.
        real(real64) :: v(n)
        integer(int64), allocatable :: p_ser(:), p_par(:), g_ser(:), g_par(:)
        integer(int64) :: seen_ser, seen_par
        integer :: i
        !
#ifndef _OPENMP
        call skip_test(error, "needs OpenMP: both arms would run the same serial code, so the " // &
            "equality below would hold for the wrong reason")
        return
#else
        if (omp_get_num_procs() < 2) then
            call skip_test(error, "needs at least two processors: an explicit threads= is " // &
                "clamped to omp_get_num_procs(), so the parallel arm would resolve to 1")
            return
        end if
#endif
        ! ~40 distinct values over 4096 rows, so every run is long and the offsets are the point.
        do i = 1, n
            v(i) = real(mod(i * 7919, 41), real64)
        end do
        !
        call force_fortran_parallel_threshold(1_int64)
        call pf_argsort(v, p_ser, group_offsets=g_ser, threads=1)
        seen_ser = parquet_debug_sort_threads_used()
        call pf_argsort(v, p_par, group_offsets=g_par, threads=8)
        seen_par = parquet_debug_sort_threads_used()
        call force_fortran_parallel_threshold(0_int64)
        !
        call check(error, seen_ser == 1_int64, &
            "precondition: the serial arm must really be serial")
        if (allocated(error)) return
        call check(error, seen_par > 1_int64, &
            "precondition: the parallel arm must really open a team, or this compares serial " // &
            "against serial and asserts nothing")
        if (allocated(error)) return
        call check(error, size(p_ser) == size(p_par), &
            "a threaded grouped sort must return a permutation of the same length")
        if (allocated(error)) return
        call check(error, all(p_ser == p_par), &
            "a threaded grouped sort must return the identical permutation: the comparator is a " // &
            "total order, so a different answer is a wrong one")
        if (allocated(error)) return
        call check(error, size(g_ser) == size(g_par), &
            "a threaded grouped sort must find the same number of groups")
        if (allocated(error)) return
        call check(error, all(g_ser == g_par), &
            "a threaded grouped sort must report the identical group offsets")
    end subroutine test_group_offsets_threads_identical


    !
    !> **The single highest-value test of the threaded decomposition**, and the reason it is a
    !! *dense* sweep rather than a handful of round numbers.
    !!
    !! A decomposition splits the array at computed boundaries, and the classic defect that invites
    !! is an off-by-one that shows up at exactly one array size and passes at every neighbouring one
    !! -- a boundary that lands one element early only matters when some run happens to end there.
    !! So this asserts the identity oracle at EVERY size from 2 to 400 and at every thread count
    !! from 2 to 8, then at a scattering of larger and deliberately awkward sizes (primes, powers of
    !! two and their neighbours, exact multiples of the thread count) that reach bucket counts the
    !! small sizes never do.
    !!
    !! **`force_fortran_bucket_split`, not `force_fortran_parallel_threshold`**, and the difference
    !! is the whole small end of this sweep: without the radix floor lowered, `n = 2..127` opened a
    !! team and sorted serially inside it, so 882 of these 2793 points asserted a serial answer
    !! against itself. `threaded_split_ran()` is what stops that returning. See both helpers.
    !!
    !! The oracle itself is the one the whole feature rests on: `SortRowLess` ends with a tiebreaker
    !! on the row index, so it is a total order in which no two rows compare equal, and every correct
    !! sorting algorithm must therefore produce the identical permutation. A threaded answer that
    !! differs from the serial one at any size or thread count is a defect, never a variation.
    !!
    !! ~2800 sorts of trivially small arrays, well under a second, and a decided permanent cost: the
    !! density IS the test, so do not narrow the range to make it faster.
    subroutine test_split_size_sweep(error)
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: nbig = 12
        !> Sizes past the dense range, chosen to be awkward: primes, powers of two and their
        !! neighbours, and exact multiples of a thread count.
        integer, parameter :: big(nbig) = [401, 511, 512, 513, 1021, 1024, 1025, 2048, 3000, 4093, 5000, 8192]
        real(real64), allocatable :: v(:)
        integer(int32), allocatable :: ser(:), par(:)
        integer :: n, t, k

        ! **Precondition, declared rather than assumed.** Without a team this test's
        ! positive control (`threaded_split_ran`/`threaded_design_was`) cannot be
        ! satisfied, and the identity oracle beneath it would compare the serial answer
        ! with itself -- passing against a library that had stopped threading entirely.
        ! Skipping says so out loud; a silent pass would not. Same shape as the engine
        ! tests above. It was flang that found this: it builds with no OpenMP at all
        ! (MacPorts flang-mp-22 ships no omp_lib.mod), so every one of these failed there
        ! the moment the control was added -- which is the control working, not a defect.
#ifndef _OPENMP
        call skip_test(error, "needs OpenMP: without it the bucket split is preprocessed out " // &
            "entirely, so no team is ever opened, the threaded design is never entered " // &
            "and every assertion below would be vacuous")
        return
#else
        if (omp_get_num_procs() < 2) then
            call skip_test(error, "needs at least two processors: the engine clamps an " // &
                "explicit threads= to omp_get_num_procs(), so it resolves to 1 here and " // &
                "no threaded design is entered")
            return
        end if
#endif
        do n = 2, 400
            allocate(v(n))
            call ties_fixture(v)
            call force_fortran_bucket_split(1000000000_int64)   ! above the size -> serial reference
            call pf_argsort(v, ser)
            call force_fortran_bucket_split(2_int64)            ! below the size -> decomposed
            do t = 2, 8
                call pf_argsort(v, par, threads=t)
                call check(error, threaded_split_ran(), &
                    "the threaded radix must actually decompose at n="//itoa(n)// &
                    " threads="//itoa(t)//"; a serial fallback would satisfy the oracle below")
                if (allocated(error)) exit
                call check(error, size(par) == n .and. all(par == ser), &
                    "a threaded decomposition must equal the serial permutation at n="//itoa(n)// &
                    " threads="//itoa(t))
                if (allocated(error)) exit
            end do
            deallocate(v)
            if (allocated(error)) exit
        end do
        if (allocated(error)) then
            call force_fortran_bucket_split(0_int64)
            return
        end if
        do k = 1, nbig
            allocate(v(big(k)))
            call ties_fixture(v)
            call force_fortran_bucket_split(1000000000_int64)
            call pf_argsort(v, ser)
            call force_fortran_bucket_split(2_int64)
            do t = 2, 8
                call pf_argsort(v, par, threads=t)
                call check(error, threaded_split_ran(), &
                    "the threaded radix must actually decompose at n="//itoa(big(k))// &
                    " threads="//itoa(t))
                if (allocated(error)) exit
                call check(error, size(par) == big(k) .and. all(par == ser), &
                    "a threaded decomposition must equal the serial permutation at n="//itoa(big(k))// &
                    " threads="//itoa(t))
                if (allocated(error)) exit
            end do
            deallocate(v)
            if (allocated(error)) exit
        end do
        call force_fortran_bucket_split(0_int64)
    end subroutine test_split_size_sweep


    !
    !> **Not redundant with the sweep above**, and the sharper of the two where a boundary goes
    !! wrong.
    !!
    !! The buckets are a partition of the input only because each one's cursor starts exactly where
    !! the previous one ends. A boundary that drifts makes two buckets overlap or leave a gap, so
    !! some row index is written twice and another not at all -- and the result stops being a
    !! permutation.
    !! **Nothing on the raw path would notice**: `pf_argsort` hands its answer straight to the caller,
    !! and `pf_permute(..., assume_valid=.true.)` is documented as the way to skip validation for
    !! exactly such a permutation. (`%sort_by` would be caught, by the one remaining `%reindex`
    !! validation -- `feature_risks.md` Risk-46 -- but that is the other path.)
    !!
    !! So this asserts the property directly rather than through the oracle: every index in 1..n
    !! appears exactly once.
    subroutine test_split_sweep_is_permutation(error)
        type(error_type), allocatable, intent(out) :: error
        real(real64), allocatable :: v(:)
        integer(int32), allocatable :: perm(:)
        logical, allocatable :: seen(:)
        integer :: n, t, k

        ! **Precondition, declared rather than assumed.** Without a team this test's
        ! positive control (`threaded_split_ran`/`threaded_design_was`) cannot be
        ! satisfied, and the identity oracle beneath it would compare the serial answer
        ! with itself -- passing against a library that had stopped threading entirely.
        ! Skipping says so out loud; a silent pass would not. Same shape as the engine
        ! tests above. It was flang that found this: it builds with no OpenMP at all
        ! (MacPorts flang-mp-22 ships no omp_lib.mod), so every one of these failed there
        ! the moment the control was added -- which is the control working, not a defect.
#ifndef _OPENMP
        call skip_test(error, "needs OpenMP: without it the bucket split is preprocessed out " // &
            "entirely, so no team is ever opened, the threaded design is never entered " // &
            "and every assertion below would be vacuous")
        return
#else
        if (omp_get_num_procs() < 2) then
            call skip_test(error, "needs at least two processors: the engine clamps an " // &
                "explicit threads= to omp_get_num_procs(), so it resolves to 1 here and " // &
                "no threaded design is entered")
            return
        end if
#endif
        call force_fortran_bucket_split(2_int64)
        do n = 2, 400
            allocate(v(n), seen(n))
            call ties_fixture(v)
            do t = 2, 8
                call pf_argsort(v, perm, threads=t)
                call check(error, threaded_split_ran(), &
                    "the threaded radix must actually decompose at n="//itoa(n)// &
                    " threads="//itoa(t)//"; a serial fallback is a permutation too")
                if (allocated(error)) exit
                seen = .false.
                do k = 1, n
                    if (perm(k) < 1 .or. perm(k) > n) exit
                    if (seen(perm(k))) exit
                    seen(perm(k)) = .true.
                end do
                call check(error, all(seen), &
                    "a threaded decomposition must return each index exactly once at n="//itoa(n)// &
                    " threads="//itoa(t))
                if (allocated(error)) exit
            end do
            deallocate(v, seen)
            if (allocated(error)) exit
        end do
        call force_fortran_bucket_split(0_int64)
    end subroutine test_split_sweep_is_permutation


    !
    !> The inputs that drive the split to its ends, where a boundary that is one step out stops
    !! being harmless.
    !!
    !! * **Already sorted**: the rows arrive in key order, so each bucket's range is a contiguous
    !!   prefix of the input and every boundary sits at an extreme of its own bucket.
    !! * **Reverse sorted**: the mirror image, every boundary at the other extreme.
    !! * **All values equal**: every comparison is a tie in the user's key, resolved only by the row
    !!   index. This is the fixture that pins stability -- one bucket holds every row, and a scatter
    !!   that reorders equal rows makes the permutation stop matching the serial one.
    !! * **One extreme at each end**: a single value that must travel the whole way across.
    subroutine test_split_boundary_extremes(error)
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: n = 5000
        real(real64) :: v(n)
        integer(int32), allocatable :: ser(:), par(:)
        integer :: shape_id, k, t

        ! **Precondition, declared rather than assumed.** Without a team this test's
        ! positive control (`threaded_split_ran`/`threaded_design_was`) cannot be
        ! satisfied, and the identity oracle beneath it would compare the serial answer
        ! with itself -- passing against a library that had stopped threading entirely.
        ! Skipping says so out loud; a silent pass would not. Same shape as the engine
        ! tests above. It was flang that found this: it builds with no OpenMP at all
        ! (MacPorts flang-mp-22 ships no omp_lib.mod), so every one of these failed there
        ! the moment the control was added -- which is the control working, not a defect.
#ifndef _OPENMP
        call skip_test(error, "needs OpenMP: without it the bucket split is preprocessed out " // &
            "entirely, so no team is ever opened, the threaded design is never entered " // &
            "and every assertion below would be vacuous")
        return
#else
        if (omp_get_num_procs() < 2) then
            call skip_test(error, "needs at least two processors: the engine clamps an " // &
                "explicit threads= to omp_get_num_procs(), so it resolves to 1 here and " // &
                "no threaded design is entered")
            return
        end if
#endif
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
            call force_fortran_bucket_split(1000000000_int64)
            call pf_argsort(v, ser)
            call force_fortran_bucket_split(2_int64)
            do t = 2, 8
                call pf_argsort(v, par, threads=t)
                call check(error, threaded_split_ran(), &
                    "the threaded radix must actually decompose on extreme shape "// &
                    itoa(shape_id)//" at threads="//itoa(t))
                if (allocated(error)) exit
                call check(error, all(par == ser), &
                    "the threaded split must equal the serial permutation on extreme shape "// &
                    itoa(shape_id)//" at threads="//itoa(t))
                if (allocated(error)) exit
            end do
            if (allocated(error)) exit
        end do
        call force_fortran_bucket_split(0_int64)
    end subroutine test_split_boundary_extremes


    !
    !> The threaded radix is generic over key family, but only one of the three is arithmetic. A
    !! `character` key is ordered byte by byte and a multi-key walks several keys per comparison, so
    !! both reach a different design from a `real(real64)`: measured at `n = 4000`, `real64` reports
    !! design 2 (the bucket split) while a `character` key and a two-key set report design 1 (the
    !! LSD chain). The sweeps above are all `real64`, so design 1 is threaded HERE and nowhere else
    !! -- which is what makes this test irreplaceable rather than a variation on the sweep.
    subroutine test_split_key_families(error)
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: n = 4000
        character(len=8) :: s(n)
        real(real64) :: a(n)
        integer(int32) :: b(n)
        type(pf_sort_keys) :: keys
        integer(int32), allocatable :: ser(:), par(:)
        integer :: k, t

        ! **Precondition, declared rather than assumed.** Without a team this test's
        ! positive control (`threaded_split_ran`/`threaded_design_was`) cannot be
        ! satisfied, and the identity oracle beneath it would compare the serial answer
        ! with itself -- passing against a library that had stopped threading entirely.
        ! Skipping says so out loud; a silent pass would not. Same shape as the engine
        ! tests above. It was flang that found this: it builds with no OpenMP at all
        ! (MacPorts flang-mp-22 ships no omp_lib.mod), so every one of these failed there
        ! the moment the control was added -- which is the control working, not a defect.
#ifndef _OPENMP
        call skip_test(error, "needs OpenMP: without it the threaded LSD chain and the bucket split is preprocessed out " // &
            "entirely, so no team is ever opened, the threaded design is never entered " // &
            "and every assertion below would be vacuous")
        return
#else
        if (omp_get_num_procs() < 2) then
            call skip_test(error, "needs at least two processors: the engine clamps an " // &
                "explicit threads= to omp_get_num_procs(), so it resolves to 1 here and " // &
                "no threaded design is entered")
            return
        end if
#endif
        do k = 1, n
            write(s(k), '(i8.8)') mod(k * 7919, 137)   ! heavy ties, lexicographic order
            a(k) = real(mod(k * 7919, 53), real64)
            b(k) = int(mod(k * 104729, 17), int32)
        end do
        call force_fortran_bucket_split(1000000000_int64)
        call pf_argsort(s, ser)
        call force_fortran_bucket_split(2_int64)
        do t = 2, 8
            call pf_argsort(s, par, threads=t)
            call check(error, threaded_design_was(1_int64), &
                "a character key must reach the THREADED LSD CHAIN (design 1) at threads="//itoa(t)// &
                "; this test is design 1's only threaded coverage")
            if (allocated(error)) exit
            call check(error, all(par == ser), &
                "a threaded split over a character key must equal the serial one at threads="//itoa(t))
            if (allocated(error)) exit
        end do
        if (allocated(error)) then
            call force_fortran_bucket_split(0_int64)
            return
        end if
        call keys%add(a)
        call keys%add(b, descending=.true.)
        call force_fortran_bucket_split(1000000000_int64)
        call pf_argsort(keys, ser)
        call force_fortran_bucket_split(2_int64)
        do t = 2, 8
            call pf_argsort(keys, par, threads=t)
            call check(error, threaded_design_was(1_int64), &
                "a two-key set must reach the THREADED LSD CHAIN (design 1) at threads="//itoa(t)// &
                "; this test is design 1's only threaded coverage")
            if (allocated(error)) exit
            call check(error, all(par == ser), &
                "a threaded split over two keys must equal the serial one at threads="//itoa(t))
            if (allocated(error)) exit
        end do
        call force_fortran_bucket_split(0_int64)
    end subroutine test_split_key_families

    !
    !> **The dense sweep for design 1, which `test_split_key_families` covers at ONE size.**
    !!
    !! The two threaded designs decompose differently and only one of them was being swept. The
    !! sweeps above are all `real64` and reach design 2 (the bucket split, a count-prefix-scatter
    !! into disjoint ranges); a `character` key reaches design 1 (the LSD chain, a pass per byte
    !! position), whose boundaries are computed by different code. Testing it at `n = 4000` alone
    !! left exactly the off-by-one-at-one-size defect the `real64` sweep exists to catch,
    !! uncovered for the other half of the engine.
    !!
    !! Same shape and same two assertions as the `real64` sweep, and the same reason for each: the
    !! oracle catches a boundary that reorders rows, the each-index-once walk catches one that
    !! makes two ranges overlap or leave a gap, and `threaded_design_was(1)` catches the case where
    !! neither can fail because nothing threaded.
    subroutine test_split_lsd_size_sweep(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=8), allocatable :: s(:)
        integer(int32), allocatable :: ser(:), par(:)
        logical, allocatable :: seen(:)
        integer :: n, t, k

        ! **Precondition, declared rather than assumed.** Without a team this test's
        ! positive control (`threaded_split_ran`/`threaded_design_was`) cannot be
        ! satisfied, and the identity oracle beneath it would compare the serial answer
        ! with itself -- passing against a library that had stopped threading entirely.
        ! Skipping says so out loud; a silent pass would not. Same shape as the engine
        ! tests above. It was flang that found this: it builds with no OpenMP at all
        ! (MacPorts flang-mp-22 ships no omp_lib.mod), so every one of these failed there
        ! the moment the control was added -- which is the control working, not a defect.
#ifndef _OPENMP
        call skip_test(error, "needs OpenMP: without it the threaded LSD chain is preprocessed out " // &
            "entirely, so no team is ever opened, the threaded design is never entered " // &
            "and every assertion below would be vacuous")
        return
#else
        if (omp_get_num_procs() < 2) then
            call skip_test(error, "needs at least two processors: the engine clamps an " // &
                "explicit threads= to omp_get_num_procs(), so it resolves to 1 here and " // &
                "no threaded design is entered")
            return
        end if
#endif
        do n = 2, 400
            allocate(s(n), seen(n))
            do k = 1, n
                ! Heavy ties in lexicographic order, so the chain has to separate rows on later
                ! bytes rather than on the first -- which is where its own boundaries are computed.
                write(s(k), '(i8.8)') mod(k * 7919, 137)
            end do
            call force_fortran_bucket_split(1000000000_int64)   ! above the size -> serial reference
            call pf_argsort(s, ser)
            call force_fortran_bucket_split(2_int64)            ! below the size -> decomposed
            do t = 2, 8
                call pf_argsort(s, par, threads=t)
                call check(error, threaded_design_was(1_int64), &
                    "a character key must reach the threaded LSD chain at n="//itoa(n)// &
                    " threads="//itoa(t))
                if (allocated(error)) exit
                call check(error, size(par) == n .and. all(par == ser), &
                    "the threaded LSD chain must equal the serial permutation at n="//itoa(n)// &
                    " threads="//itoa(t))
                if (allocated(error)) exit
                seen = .false.
                do k = 1, n
                    if (par(k) < 1 .or. par(k) > n) exit
                    if (seen(par(k))) exit
                    seen(par(k)) = .true.
                end do
                call check(error, all(seen), &
                    "the threaded LSD chain must return each index exactly once at n="//itoa(n)// &
                    " threads="//itoa(t))
                if (allocated(error)) exit
            end do
            deallocate(s, seen)
            if (allocated(error)) exit
        end do
        call force_fortran_bucket_split(0_int64)
    end subroutine test_split_lsd_size_sweep


    !
    !> Nulls and NaNs sit in tiers of their own, and the tier split runs BEFORE the radix sees a
    !! single value: `sort_tier_split_par` partitions the rows into value, NaN and null tiers across
    !! the whole team, and only the value tier is imaged and bucketed. So a threaded tier split that
    !! miscounts puts a row in the wrong tier, which no amount of correct bucketing recovers. Both
    !! null placements are covered, because `nulls_first` moves where the tiers land in the output.
    subroutine test_split_tiers(error)
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: n = 6000
        real(real64) :: v(n)
        logical :: mask(n)
        integer(int32), allocatable :: ser(:), par(:)
        integer :: k, t, variant
        logical :: nf

        ! **Precondition, declared rather than assumed.** Without a team this test's
        ! positive control (`threaded_split_ran`/`threaded_design_was`) cannot be
        ! satisfied, and the identity oracle beneath it would compare the serial answer
        ! with itself -- passing against a library that had stopped threading entirely.
        ! Skipping says so out loud; a silent pass would not. Same shape as the engine
        ! tests above. It was flang that found this: it builds with no OpenMP at all
        ! (MacPorts flang-mp-22 ships no omp_lib.mod), so every one of these failed there
        ! the moment the control was added -- which is the control working, not a defect.
#ifndef _OPENMP
        call skip_test(error, "needs OpenMP: without it the bucket split is preprocessed out " // &
            "entirely, so no team is ever opened, the threaded design is never entered " // &
            "and every assertion below would be vacuous")
        return
#else
        if (omp_get_num_procs() < 2) then
            call skip_test(error, "needs at least two processors: the engine clamps an " // &
                "explicit threads= to omp_get_num_procs(), so it resolves to 1 here and " // &
                "no threaded design is entered")
            return
        end if
#endif
        do k = 1, n
            v(k) = real(mod(k * 7919, 41), real64)
            if (mod(k, 7) == 0) v(k) = ieee_value(1.0_real64, ieee_quiet_nan)
            mask(k) = mod(k, 11) /= 0
        end do
        do variant = 1, 2
            nf = (variant == 2)
            call force_fortran_bucket_split(1000000000_int64)
            call pf_argsort(v, ser, is_valid=mask, nulls_first=nf)
            call force_fortran_bucket_split(2_int64)
            do t = 2, 8
                call pf_argsort(v, par, is_valid=mask, nulls_first=nf, threads=t)
                call check(error, threaded_split_ran(), &
                    "the threaded radix must actually decompose a null/NaN-bearing key, "// &
                    "nulls_first variant "//itoa(variant)//" at threads="//itoa(t))
                if (allocated(error)) exit
                call check(error, all(par == ser), &
                    "a threaded split must equal the serial one across null/NaN tiers, "// &
                    "nulls_first variant "//itoa(variant)//" at threads="//itoa(t))
                if (allocated(error)) exit
            end do
            if (allocated(error)) exit
        end do
        call force_fortran_bucket_split(0_int64)
    end subroutine test_split_tiers


    !
    !> **Stage 5: every operation that is NOT a full sort, C++ engine against Fortran engine.**
    !!
    !! `engine_ab` above covers `pf_argsort`. These six reach engine procedures of their own, and
    !! every one of them was unreachable from the Fortran engine before Stage 5 — so without this
    !! test the six new procedures would be dead code that the suite reports as passing.
    !!
    !! **The fixtures carry DUPLICATES deliberately.** Four of the six — is_sorted, the run flags,
    !! search and merge — turn on rows comparing EQUAL, which is the one relation the sort
    !! comparator's index tiebreaker destroys. A fixture of distinct values passes just as happily
    !! against an implementation that reached for the wrong comparator, so it would test nothing
    !! about the decision this stage's engine procedures actually have to make.
    !> Stage 4: the Fortran engine's threaded permutation equals its serial one and the C++ one.
    !>
    !> **The equality assertions are the whole correctness gate, and they are strong for a reason
    !> that is about the ORDERING rather than about this test.** `sort_row_less` ends in a row-index
    !> tiebreaker, so no two distinct rows compare equal, exactly one permutation is correct, and
    !> every correct algorithm must produce it. A threading defect therefore cannot hide as a
    !> differently-ordered-but-valid answer: any disagreement here is a wrong answer.
    !>
    !> **Without the `threads_seen` control the whole test would be vacuous**, and vacuous in the
    !> direction that passes: if the policy silently refused to thread, every arm would run the same
    !> serial code and every equality would hold. The permutation cannot reveal the team size —
    !> that is precisely what the tiebreaker guarantees — so `parquet_debug_sort_threads_used` is the
    !> only observable that can, and it is asserted in BOTH directions (a team was really opened, and
    !> the two refusal clauses really refuse).
    subroutine test_fortran_engine_threading(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first disagreement.
        !> Large enough that every arm's chunk split is non-trivial: at 8 threads a 4000-row fixture
        !! gives 500 rows per chunk, so an off-by-one in `sort_chunk_bounds` moves real rows rather
        !! than falling in an empty tail. Real values, because `sort_counting_candidate` accepts an
        !! integer key on value RANGE and the counting path is serial and untouched by Stage 4 —
        !! an integer fixture would test nothing here.
        real(real64) :: v(4000)
        real(real64) :: lowcard(4000) !! three distinct values: the shape that must DECLINE the split.
        !> A FULL-RANGE int64 key, because Design B needs its top varying byte to be well spread and
        !! `v` above is not: a bounded-range real's high bytes are its exponent, which takes only a
        !! handful of values, so the split there is skewed and the balance test declines it. That is
        !! correct behaviour and is why this arm needs a different key rather than a looser threshold.
        integer(int64) :: wide(4000)
        integer(int64) :: spread_vals(200)            !! the distinct values `wide` cycles through.
        character(len=10) :: sv(4000)                 !! a string key with a SHARED STEM.
        logical :: nmask(4000)                        !! a validity mask, so the null tier is exercised.
        integer(int64) :: narrowv(4000)               !! a NARROW-range integer key, for the pass count.
        integer(int64) :: npass_ser, npass_par        !! radix passes, serial and threaded.
        integer(int64) :: mk1(4000), mk2(4000)        !! a two-key chain; the first repeats heavily.
        type(pf_sort_keys) :: mkeys                   !! the multi-key set.
        integer(int64), allocatable :: sref(:), sgot(:) !! the string key's serial and threaded answers.
        integer(int64) :: xs                          !! xorshift state, so the spread is reproducible.
        integer(int64), allocatable :: wref(:)        !! the wide key's serial answer.
        integer(int64), allocatable :: lref(:)        !! the low-cardinality key's serial answer.
        integer(int64), allocatable :: ref(:), got(:) !! serial reference, and one threaded arm.
        integer(int64), allocatable :: cpp(:)         !! the C++ engine's answer, as a cross-check.
        integer, parameter :: arms(4) = [2, 3, 4, 8]  !! 3 is deliberate: not a divisor of 4000.
        integer(int64) :: threads_seen(size(arms))    !! what the policy resolved on each arm.
        integer :: k
        character(len=96) :: kstr !! long enough for the longest message below, plus the arm number.
        ! **Preconditions, declared rather than assumed.** Every assertion here is about a team: the threaded designs,
        ! the split bucket counts, and the threaded-equals-serial comparisons -- which without a team
        ! compare serial with serial, and so hold for the wrong reason. That is why this is skipped
        ! whole rather than split: nothing in it keeps its meaning serially.
        ! Where no team can be opened the assertions are not merely untestable but VACUOUS:
        ! they would pass just as happily against a library that had stopped threading
        ! altogether. Skipping says so out loud, which a silent pass would not. Same reasoning
        ! and same shape as `test_nested_team_guard`.
#ifndef _OPENMP
        call skip_test(error, "needs OpenMP: without it the threaded sort designs are " // &
            "preprocessed out entirely -- the Design A/B dispatch in " // &
            "src/parquet_argsort_engine.f90 sits inside #ifdef _OPENMP -- so no team is " // &
            "ever opened and every assertion below would be vacuous")
        return
#else
        if (omp_get_num_procs() < 2) then
            call skip_test(error, "needs at least two processors: the engine clamps an " // &
                "explicit threads= to omp_get_num_procs(), so it resolves to 1 here and no " // &
                "threaded design is entered")
            return
        end if
#endif
        !
        ! Ties every seventh row, and no pre-existing order: a chunk boundary falling inside a run
        ! of equal keys is where a split that loses stability would show, and the tiebreaker is what
        ! must keep the answer unique there.
        do k = 1, size(v)
            v(k) = real(mod(k * 37, 571), real64) + real(mod(k, 7), real64) * 0.5_real64
        end do
        ! The C++ engine first, while it is still the default, so the cross-check is taken against
        ! an engine this test has not touched the settings of.
        call pf_argsort(v, cpp, threads=4)
        !
        ! Both floors have to come down together or the fixture reaches neither path: the radix floor
        ! gates the only phase Stage 4 threads, and the parallel floor gates threading itself. A
        ! fixture big enough to clear the production values of both would be far too slow for a unit
        ! test — `feature_risks.md` Risk-49 in its usual form.
        call parquet_debug_set_sort_radix_min_rows(2_int64)
        ! **1, not 0.** Every floor hook treats a value <= 0 as "restore the built-in", so a floor
        ! of 0 is the DEFAULT floor and would refuse this 4000-row fixture outright. The
        ! positive assertions below caught exactly that; the negative control could not have, because
        ! it would have been asserting a refusal that was already happening for the wrong reason.
        call force_fortran_parallel_threshold(1_int64)
        !
        call pf_argsort(v, ref, threads=1)
        call check(error, fortran_threads_used() == 1_int64, &
            "threads=1 must resolve to the serial path, not a team of one")
        if (allocated(error)) return
        !
        do k = 1, size(arms)
            call pf_argsort(v, got, threads=arms(k))
            threads_seen(k) = fortran_threads_used()
            write (kstr, '(a,i0,a)') "the Fortran engine at threads=", arms(k), &
                " must give the serial permutation exactly"
            call check(error, size(got) == size(ref), trim(kstr) // " (size)")
            if (allocated(error)) return
            call check(error, all(got == ref), trim(kstr))
            if (allocated(error)) return
            ! A RANGE, not an equality, because the engine clamps to `omp_get_num_procs()` — asking
            ! for more threads than the machine has is what dragged a parallel sort back to serial
            ! speed on machine A. The lower bound is what keeps this a vacuity control: a policy that
            ! silently refuses to thread reports 1 and fails here.
            write (kstr, '(a,i0)') "a team must actually be opened at threads=", arms(k)
            call check(error, threads_seen(k) >= 2_int64 .and. threads_seen(k) <= int(arms(k), int64), &
                trim(kstr))
            if (allocated(error)) return
        end do
        !
        ! The cross-engine check. Kept separate from the loop above because a disagreement here and
        ! a disagreement there mean different things: this one says the two ENGINES differ, which is
        ! a Stage 1 conformance failure, not a Stage 4 threading one.
        call check(error, size(cpp) == size(ref) .and. all(cpp == ref), &
            "the Fortran engine's threaded answer must equal the C++ engine's")
        if (allocated(error)) return
        !
        ! **The negative control for the row floor.** Raising it above the fixture must send the
        ! same call back to the serial path — without this, a policy that ignored the floor entirely
        ! would pass every assertion above.
        call force_fortran_parallel_threshold(int(size(v), int64) + 1_int64)
        call pf_argsort(v, got, threads=8)
        call check(error, fortran_threads_used() == 1_int64, &
            "a row floor above the fixture must refuse the team")
        if (allocated(error)) return
        call check(error, all(got == ref), "refusing the team must not change the answer")
        if (allocated(error)) return
        !
        !
        ! **Design B, the MSD split.** It answers identically to the serial LSD loop by construction,
        ! so `parquet_debug_sort_split_buckets` is the only thing that can say which path ran — and
        ! every assertion above would hold just as well against an engine that never split at all.
        ! **200 distinct values, each repeated 20 times — the repetition is load-bearing.** A plain
        ! xorshift fill makes every key distinct, and then the final order is fully determined by the
        ! key alone: the split's STABILITY becomes unobservable, and a mutation reversing the order
        ! threads contribute within a bucket survives the whole test. It did, on the first version of
        ! this fixture. Ties are what make the row-index tiebreaker the only correct answer, and
        ! therefore what makes an unstable split a WRONG one rather than merely a different one.
        !
        ! Spread over the full int64 range so the top byte still takes ~200 of its 256 values, which
        ! is what Design B's balance test needs in order to accept the key at all.
        xs = 88172645463325252_int64
        do k = 1, 200
            xs = ieor(xs, ishft(xs, 13))
            xs = ieor(xs, ishft(xs, -7))
            xs = ieor(xs, ishft(xs, 17))
            spread_vals(k) = xs
        end do
        do k = 1, size(wide)
            wide(k) = spread_vals(mod(k - 1, 200) + 1)
        end do
        call force_fortran_parallel_threshold(1_int64)
        call pf_argsort(wide, wref, threads=1)
        call check(error, parquet_debug_sort_split_buckets() == 0_int64, &
            "one thread must not split: Design B needs a team, and T=1 is the serial path")
        if (allocated(error)) return
        call pf_argsort(wide, got, threads=4)
        call check(error, all(got == wref), "Design B must give the serial permutation exactly")
        if (allocated(error)) return
        call check(error, parquet_debug_sort_split_buckets() > 1_int64, &
            "Design B must actually split this key, or every assertion here is vacuous")
        if (allocated(error)) return
        call check(error, parquet_debug_sort_design() == 2_int64, &
            "a well-spread key must take Design B")
        if (allocated(error)) return
        !
        ! **The decline, forced through the hook rather than through data — and it has to be.** The
        ! floor is `max(2, nt / 8)`, so at the 4 threads this test uses it is 2, and a 3-distinct-
        ! value key is now ACCEPTED. That is deliberate: machine B measured Design B 3.44x FASTER
        ! than Design A on exactly this shape (cardinality 3, 4 threads, values spread across the
        ! int64 range, which `real64` 0.0/1.0/2.0 bit patterns are). The 2.10-2.57x loss this test
        ! was written around was measured on the PRE-REFINEMENT Design B, which could not subdivide
        ! an oversized bucket; refinement is what made low-cardinality keys viable, and the flat
        ! floor of 16 outlived the problem it was guarding.
        !
        ! So the decline is reached by forcing the floor above the fixture's cardinality. The
        ! assertions below are unchanged in meaning: a declining key must fall back to Design A and
        ! must not change the answer. **If the floor is ever removed entirely, delete the forcing
        ! and this whole block with it — do not weaken it into a decline-on-data test at a larger
        ! team, which would make it a test of the thread count rather than of the fallback.**
        do k = 1, size(v)
            lowcard(k) = real(mod(k, 3), real64)
        end do
        call parquet_debug_set_sort_split_min_card(16_int64)
        call pf_argsort(lowcard, lref, threads=1)
        call pf_argsort(lowcard, got, threads=4)
        ! Reset BEFORE asserting. The hook is process-global and every `check` below can `return`,
        ! which would leak a forced floor into every later test in this suite -- and the leak would
        ! show up as an unrelated test asserting the wrong design, far from here.
        call parquet_debug_set_sort_split_min_card(-1_int64)
        call check(error, parquet_debug_sort_split_buckets() == 0_int64, &
            "a low-cardinality key must decline Design B's split")
        if (allocated(error)) return
        ! **The fallback, and the assertion that makes it worth having.** Before Design A existed a
        ! declining key fell all the way back to the SERIAL loop, which is what machine B measured
        ! costing 2.10-2.57x at 64 threads. Asserting only the decline above would pass just as well
        ! against that.
        call check(error, parquet_debug_sort_design() == 1_int64, &
            "declining Design B must fall back to Design A, not to the serial loop")
        if (allocated(error)) return
        call check(error, all(got == lref), "declining the split must not change the answer")
        if (allocated(error)) return
        !
        ! **The other half of the same fixture: with the shipped floor, this key is ACCEPTED.**
        ! Without this the block above would pass just as well against a floor that declines
        ! everything, which is precisely the defect the flat 16 turned out to be. `max(2, nt/8)` is
        ! 2 at four threads, and this key has three distinct values, so the split must run — and
        ! must still give the serial permutation exactly.
        call pf_argsort(lowcard, got, threads=4)
        call check(error, parquet_debug_sort_design() == 2_int64, &
            "at the shipped floor a 3-value key must now TAKE the split: max(2, nt/8) is 2 here")
        if (allocated(error)) return
        call check(error, all(got == lref), "accepting the split must not change the answer either")
        if (allocated(error)) return
        !
        !
        ! **Strings, which reach Design A and must NOT reach Design B.** The string path radixes a
        ! packed 8-byte prefix and then refines runs that share one, reading the sorted `ka`/`ra` the
        ! radix leaves behind — so a design that writes into `perm` and abandons the images cannot be
        ! used here. Design A can, because it ping-pongs with `move_alloc` and finishes with the
        ! answer in place. A shared stem is deliberate: it is the shape real string columns have, and
        ! the one machine B measured Design B losing 2.10× on.
        do k = 1, size(v)
            write (sv(k), '(a,i5.5)') "stem/", mod(k * 37, 700)
        end do
        call pf_argsort(sv, sref, threads=1)
        call pf_argsort(sv, sgot, threads=4)
        call check(error, size(sgot) == size(sref) .and. all(sgot == sref), &
            "a threaded string sort must give the serial permutation exactly")
        if (allocated(error)) return
        call check(error, parquet_debug_sort_design() == 1_int64, &
            "a string key must take Design A: B abandons the images its refine has to read")
        if (allocated(error)) return
        call check(error, parquet_debug_sort_split_buckets() == 0_int64, &
            "a string key must never reach Design B's split")
        if (allocated(error)) return
        !
        ! **A NULL-carrying key, which the threaded tier split is otherwise never asked about.** Both
        ! of this test's earlier keys are null-free, so pass 2 of `sort_tier_split_par` never takes
        ! its skip branch — and inverting that branch, so nulls are kept and values dropped, survived
        ! every assertion above. A compaction that mis-classifies rows is about as bad as this engine
        ! gets, and nothing here could see it.
        do k = 1, size(v)
            nmask(k) = mod(k, 5) /= 0
        end do
        call pf_argsort(v, ref, is_valid=nmask, threads=1)
        call pf_argsort(v, got, is_valid=nmask, threads=4)
        call check(error, size(got) == size(ref) .and. all(got == ref), &
            "a threaded sort of a key WITH NULLS must give the serial permutation exactly")
        if (allocated(error)) return
        !
        ! **A narrow-range integer key far from zero**, which is the shape the value-range bias exists
        ! for: imaged as `v - vmin` its top bytes go constant and the radix skips those passes.
        !
        ! **What this does NOT assert, and why — read before "strengthening" it.** The obvious check
        ! is that the threaded run does the same number of radix passes as the serial one, and it
        ! cannot be written: `parquet_debug_sort_radix_passes` is DESIGN-dependent. Design B counts a
        ! single pass for its whole split and counts nothing for the per-bucket sorts, where the
        ! serial loop counts every digit — so serial and threaded pass counts differ legitimately and
        ! comparing them fails against correct code. A mutation that drops the per-thread range
        ! reduction in `sort_tier_split_par` therefore SURVIVES this test; it is recorded as an open
        ! gap in `feature_sort.md` §6 Stage 4, 4e rather than papered over here. Closing it needs an
        ! observable the designs share — the resolved `vmin`/`vmax` themselves.
        call parquet_set_sort_counting_path(.false.)
        do k = 1, size(narrowv)
            narrowv(k) = 1000000000000_int64 + int(mod(k * 37, 5000), int64)
        end do
        call parquet_debug_reset_sort_radix_passes()
        call pf_argsort(narrowv, ref, threads=1)
        npass_ser = parquet_debug_sort_radix_passes()
        call pf_argsort(narrowv, got, threads=4)
        npass_par = parquet_debug_sort_radix_passes()
        call parquet_set_sort_counting_path(.true.)
        call check(error, all(got == ref), &
            "a narrow-range integer key far from zero must sort the same threaded as serial")
        if (allocated(error)) return
        call check(error, npass_ser > 0_int64 .and. npass_par > 0_int64, &
            "the narrow-range fixture must actually reach the radix on both arms")
        if (allocated(error)) return
        !
        ! **A MULTI-KEY chain, which runs a different routine entirely.** Single-key work goes through
        ! `sort_radix_permutation` and Designs A/B; two or more keys go through
        ! `sort_radix_multi_permutation`, one stable pass per key from the last key to the first. Its
        ! scatter is threaded with the same count-prefix-scatter helpers, and nothing above touches it
        ! — every assertion so far would hold with the multi-key path still fully serial.
        !
        ! The first key repeats heavily so the second one actually decides order for most rows: with
        ! a near-unique first key the later passes are no-ops and a broken chain still looks right.
        do k = 1, size(v)
            mk1(k) = int(mod(k, 40), int64)
            mk2(k) = int(mod(k * 37, 977), int64)
        end do
        call mkeys%add(mk1)
        call mkeys%add(mk2)
        call pf_argsort(mkeys, ref, threads=1)
        call pf_argsort(mkeys, got, threads=4)
        call check(error, size(got) == size(ref) .and. all(got == ref), &
            "a threaded multi-key sort must give the serial permutation exactly")
        if (allocated(error)) return
        call check(error, parquet_debug_sort_design() == 1_int64, &
            "the multi-key chain must actually thread its scatter, not fall back to the serial arms")
        if (allocated(error)) return
        !
        call force_fortran_parallel_threshold(0_int64)
        call parquet_debug_set_sort_radix_min_rows(-1_int64)
    end subroutine test_fortran_engine_threading


    !
    !> How many threads the **Fortran** engine's last permutation build resolved; 1 means serial.
    !>
    !> The twin of `threads_used` above, which answers for the C++ engine. The two counters cannot
    !> see each other — see `parquet_debug_sort_threads_used`'s own doc-comment for why that is
    !> deliberate, and for which tests the Stage 6 cutover has to repoint from one to the other.
    function fortran_threads_used() result(n)
        integer(int64) :: n !! threads resolved for the last Fortran-engine build.
        n = parquet_debug_sort_threads_used()
    end function fortran_threads_used


    !
    !> The radix path must actually RUN above its row floor, must NOT below it, and the OVERRIDE must
    !! move that floor in both directions.
    !!
    !! Every other radix test compares permutations, and the radix path and the introsort answer
    !! identically by construction -- so all of them would pass just as happily against a radix path
    !! that never ran, which is the whole trap `SORT_RADIX_MIN_ROWS` sets.
    !!
    !! The observable is the insertion tracker. The introsort always ends with one insertion pass
    !! over the whole range, so on random input it records a positive shift; the radix path never
    !! calls `sort_insertion` at all for a non-string key. A REAL key is used because the string
    !! refine pass does call it, and an integer one could be taken by the counting path instead.
    !!
    !! **The last two assertions are what six other tests rest on.** Since the floor was lowered,
    !! `engine_only_introsort` is the only thing keeping the introsort's and the counting path's own
    !! negative controls non-vacuous, and it works by raising this floor to `huge`. A hook that
    !! silently did nothing would leave all six passing while testing a fast path instead -- the
    !! `had_index` shape from `feature_risks.md` Risk-75, where a hook that forces a state has to
    !! prove the state took effect.
    subroutine test_radix_path_runs(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first disagreement.
        integer(int64), parameter :: above = 4096_int64 !! comfortably over the shipped floor.
        integer(int64), parameter :: below = 64_int64
        !! Comfortably under it -- but only 2x under, since the floor moved to 128. Lowering
        !! `SORT_RADIX_MIN_ROWS` further means lowering this too, or the shipped-floor half of this
        !! test silently stops testing the floor and starts testing the radix path twice.
        real(real64), allocatable :: v(:)
        integer(int64), allocatable :: perm(:)
        integer(int64) :: k, shift_above, shift_below, shift_declined, shift_forced
        !
        allocate(v(above))
        do k = 1_int64, above
            v(k) = real(mod(k * 2654435761_int64, 100003_int64), real64)
        end do
        !
        !
        ! The shipped floor, both sides of it.
        call parquet_debug_set_sort_track_shift(.true.)
        call pf_argsort(v(1:above), perm)
        shift_above = parquet_debug_sort_max_insertion_shift()
        call parquet_debug_set_sort_track_shift(.true.)
        call pf_argsort(v(1:below), perm)
        shift_below = parquet_debug_sort_max_insertion_shift()
        !
        ! The override, both directions, over the sizes that just took the OTHER path.
        call parquet_debug_set_sort_radix_min_rows(huge(0_int64))
        call parquet_debug_set_sort_track_shift(.true.)
        call pf_argsort(v(1:above), perm)
        shift_declined = parquet_debug_sort_max_insertion_shift()
        call parquet_debug_set_sort_radix_min_rows(2_int64)
        call parquet_debug_set_sort_track_shift(.true.)
        call pf_argsort(v(1:below), perm)
        shift_forced = parquet_debug_sort_max_insertion_shift()
        !
        call parquet_debug_set_sort_radix_min_rows(-1_int64)
        call parquet_debug_set_sort_track_shift(.false.)
        !
        ! The negative control, and the half that fails if the floor ever stops being consulted.
        call check(error, shift_below > 0_int64, &
            "below the floor the introsort should have run, but the insertion tracker recorded nothing")
        if (allocated(error)) return
        call check(error, shift_above == 0_int64, &
            "above the floor the insertion pass still ran, so the radix path did not take the sort")
        if (allocated(error)) return
        call check(error, shift_declined > 0_int64, &
            "raising the radix floor to huge did not decline the radix path, so engine_only_introsort is a no-op")
        if (allocated(error)) return
        call check(error, shift_forced == 0_int64, &
            "lowering the radix floor to 2 did not reach the radix path, so the override is one-directional")
    end subroutine test_radix_path_runs


    !
    !> The executed-pass counter must count passes, decline to count what the skip removed, and read
    !! zero when the radix path did not run at all.
    !!
    !! **This test exists so that other tests can be non-vacuous, and it has no other purpose.** Every
    !! optimisation in this engine that changes how many radix passes run -- the constant-digit skip,
    !! and the narrow-integer bias built on top of it -- leaves the permutation bit-identical by
    !! construction. So a test asserting an answer passes just as happily against a build where the
    !! optimisation never fires, and the counter is the only observable that can tell the two apart.
    !! Something has to establish that the counter itself is not the thing that is broken, or every
    !! test resting on it inherits the doubt. That is what this is.
    !!
    !! Three properties, and the third is the one that makes it an observable rather than a number:
    !!
    !! * a full-width key runs all eight passes -- so the counter is counting passes and not sorts;
    !! * a `real32` key runs strictly FEWER, because widening it to `real64` zeroes its low mantissa
    !!   bytes and the skip drops those passes. This is the `f32`-is-the-fastest-arm observation from
    !!   the benchmark, asserted rather than assumed;
    !! * a sort the radix path declined counts ZERO. Without this the counter could be reporting
    !!   something else entirely and every reading above it would still look plausible.
    subroutine test_radix_pass_counter(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first disagreement.
        integer(int64), parameter :: n = 4096_int64 !! comfortably over the shipped floor.
        real(real64), allocatable :: wide(:)
        real(real32), allocatable :: narrow(:)
        integer(int64), allocatable :: perm(:)
        integer(int64) :: k, passes_wide, passes_narrow, passes_declined, passes_reset
        !
        allocate(wide(n), narrow(n))
        do k = 1_int64, n
            ! Divided rather than whole: a small integer held as a real64 has trailing zero mantissa
            ! bytes, so a whole-number fixture would itself skip passes and this arm would not be the
            ! full-width control it is supposed to be.
            wide(k) = real(mod(k * 2654435761_int64, 100003_int64), real64) / 7.0_real64
            narrow(k) = real(wide(k), real32)
        end do
        !
        !
        call parquet_debug_reset_sort_radix_passes()
        call pf_argsort(wide, perm)
        passes_wide = parquet_debug_sort_radix_passes()
        !
        call parquet_debug_reset_sort_radix_passes()
        call pf_argsort(narrow, perm)
        passes_narrow = parquet_debug_sort_radix_passes()
        !
        ! The negative control: the same data, the same call, the radix path declined by the floor.
        call parquet_debug_set_sort_radix_min_rows(huge(0_int64))
        call parquet_debug_reset_sort_radix_passes()
        call pf_argsort(wide, perm)
        passes_declined = parquet_debug_sort_radix_passes()
        call parquet_debug_set_sort_radix_min_rows(-1_int64)
        !
        call parquet_debug_reset_sort_radix_passes()
        passes_reset = parquet_debug_sort_radix_passes()
        !
        call check(error, passes_wide == 8_int64, &
            "a full-width real64 key should have run all eight radix passes")
        if (allocated(error)) return
        call check(error, passes_narrow < passes_wide, &
            "a real32 key should skip the passes its zeroed low mantissa bytes make constant")
        if (allocated(error)) return
        call check(error, passes_narrow > 0_int64, &
            "a real32 key should still have run some radix passes")
        if (allocated(error)) return
        call check(error, passes_declined == 0_int64, &
            "the counter recorded passes for a sort the radix path declined, so it counts something else")
        if (allocated(error)) return
        call check(error, passes_reset == 0_int64, &
            "the reset did not zero the pass counter")
    end subroutine test_radix_pass_counter


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
        ! The invariant belongs to the introsort, and both fast paths leave the tracker at zero --
        ! which would satisfy the bound below while proving nothing. Declining them is what keeps
        ! the `shift_quick > 0` control meaningful.
        call engine_only_introsort(.true.)
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
        call engine_only_introsort(.false.)
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
    !!
    !! **`sort_nth_index` has its own copy of the fallback and needs its own arm.** Quickselect
    !! narrows to one side of each partition instead of recursing into both, so it is a separate
    !! `do while` loop with a separate `depth == 0` test calling the same `sort_heapsort` -- and
    !! `pf_argsort` cannot reach it. Its answer is a VALUE rather than a permutation, which is the
    !! sharper assertion of the two: the introsort's own fallback is followed by a full insertion
    !! pass that would repair a broken heapsort, whereas here the surviving range is insertion-sorted
    !! only after the heapsort has decided which rows are in it.
    subroutine test_fortran_engine_depth_limit_bites(error)
        type(error_type), allocatable, intent(out) :: error !! set on failure.
        integer(int64), parameter :: n = 300_int64
        real(real64) :: v(n)
        type(pf_sort_keys) :: keys
        integer(int64), allocatable :: perm(:)
        integer(int64) :: k, heap_normal, heap_forced, heap_nth_normal, heap_nth_forced
        real(real64) :: nth_normal, nth_forced
        !
        ! **A REAL key, and that is load-bearing.** The counting fast path accepts integer keys only,
        ! so a real one cannot reach it whatever `parquet_set_sort_counting_path` says — which keeps
        ! this test independent of a process-global setting a sibling could disturb. It used an
        ! integer key until Stage 3 landed, at which point the counting path silently took over and
        ! the introsort stopped running here at all. The RADIX path accepts every family, so a key
        ! type is no defence against that one and it has to be declined explicitly.
        do k = 1_int64, n
            v(k) = real(mod(k * 17_int64, 251_int64), real64)
        end do
        call keys%add(v)
        call engine_only_introsort(.true.)
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
        !
        ! The quickselect arm. Rank 137 of 300 is well inside the array, so the range stays above
        ! the insertion cutoff for several partitions and the forced limit bites on the first one.
        call parquet_debug_set_sort_depth_limit(-1)
        call pf_nth_element(v, 137_int64, nth_normal)
        heap_nth_normal = parquet_debug_sort_heapsort_calls()
        call parquet_debug_set_sort_depth_limit(0)
        call pf_nth_element(v, 137_int64, nth_forced)
        heap_nth_forced = parquet_debug_sort_heapsort_calls()
        !
        call parquet_debug_set_sort_depth_limit(-1)
        call engine_only_introsort(.false.)
        !
        call check(error, heap_normal == 0_int64, &
            "the ordinary path entered the heapsort fallback, so the depth limit is far too small")
        if (allocated(error)) return
        call check(error, heap_forced > 0_int64, &
            "forcing the depth limit to zero did not reach the heapsort fallback")
        if (allocated(error)) return
        call check(error, heap_nth_normal == 0_int64, &
            "quickselect entered the heapsort fallback unforced, so its depth limit is far too small")
        if (allocated(error)) return
        call check(error, heap_nth_forced > 0_int64, &
            "forcing the depth limit to zero did not reach quickselect's own heapsort fallback")
        if (allocated(error)) return
        call check(error, nth_forced == nth_normal, &
            "quickselect's heapsort fallback answered a different element than its partition loop")
    end subroutine test_fortran_engine_depth_limit_bites


    !
    !> The half that makes `threads=` mean something on the three selection procedures: with the
    !! tail floor forced down, the key extraction really does take its `!$omp parallel do` branch,
    !! and its answer must equal the serial one.
    !!
    !! **Forcing the floor is the whole point**, exactly as in `test_partial_argsort_threads_tail`
    !! above: `tail_team` declines a team below `max(32768, 1024*nt)` elements, which no fixture
    !! here reaches, so without `parquet_debug_set_sort_tail_min_rows` both arms would run the same
    !! serial code and every equality below would hold for the wrong reason.
    !!
    !! **What a team does and does not reach here** is narrower than on `pf_argsort`, and the
    !! doc-comments on these three say so: only the extraction is threaded. The selection,
    !! `pf_partial_sort`'s gather and `pf_nth_quantile`'s `key_valid_count` are all serial by
    !! design, so this test asserts the extraction and nothing further. The `is_valid` arm is the
    !! sharp one -- the mask travels through the same threaded extraction as the values, so a null
    !! placed by the wrong thread's chunk is a wrong answer only that arm can see.
    subroutine test_selection_threads_extraction(error)
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: n = 4096
        integer(int32) :: v(n)
        logical :: mask(n)
        integer(int32), allocatable :: ser(:), par(:), sernull(:), parnull(:)
        integer(int32) :: nth_ser, nth_par, nthnull_ser, nthnull_par
        ! p_value takes the ARRAY's type, not real64 -- `v` is int32, so a quantile of it is too.
        integer(int32) :: q_ser, q_par
        integer(int64) :: nnull_ser, nnull_par
        integer :: i
        !
        ! Preconditions, declared rather than assumed: with the threaded branch preprocessed out,
        ! or on a machine where an explicit threads= clamps back to 1, both arms below are the same
        ! serial code and every assertion passes without testing anything.
#ifndef _OPENMP
        call skip_test(error, "needs OpenMP: the threaded extraction (extract_*, " // &
            "src/parquet_argsort_kernel.f90) sits inside #ifdef _OPENMP, so both arms below " // &
            "would run the same serial code and the equality would hold for the wrong reason")
        return
#else
        if (omp_get_num_procs() < 2) then
            call skip_test(error, "needs at least two processors: resolve_thread_count clamps " // &
                "an explicit threads= to omp_get_num_procs(), so threads=4 resolves to 1 here " // &
                "and the extraction is not threaded")
            return
        end if
#endif
        !
        do i = 1, n
            v(i) = int(mod(i * 7919, 65537), int32)
            mask(i) = mod(i, 7) /= 0
        end do
        !
        ! The floor is restored BEFORE the first assertion: every `check` can return early, and a
        ! leaked floor would silently rethread every later test in this suite.
        call parquet_debug_set_sort_tail_min_rows(1_int64)
        call pf_partial_sort(v, par, 64, threads=4)
        call pf_partial_sort(v, parnull, 64, is_valid=mask, threads=4)
        call pf_nth_element(v, 37, nth_par, threads=4)
        call pf_nth_element(v, 37, nthnull_par, is_valid=mask, threads=4)
        call pf_nth_quantile(v, 0.6_real64, q_par, is_valid=mask, n_null=nnull_par, threads=4)
        call parquet_debug_set_sort_tail_min_rows(-1_int64)
        !
        call pf_partial_sort(v, ser, 64, threads=1)
        call pf_partial_sort(v, sernull, 64, is_valid=mask, threads=1)
        call pf_nth_element(v, 37, nth_ser, threads=1)
        call pf_nth_element(v, 37, nthnull_ser, is_valid=mask, threads=1)
        call pf_nth_quantile(v, 0.6_real64, q_ser, is_valid=mask, n_null=nnull_ser, threads=1)
        !
        call check(error, size(par) == 64, "the forced-floor run must still return n values")
        if (allocated(error)) return
        call check(error, all(par == ser), &
            "a threaded extraction must give pf_partial_sort's serial answer")
        if (allocated(error)) return
        call check(error, all(parnull == sernull), &
            "a threaded extraction must place pf_partial_sort's nulls exactly as the serial one does")
        if (allocated(error)) return
        call check(error, nth_par == nth_ser, &
            "a threaded extraction must give pf_nth_element's serial answer")
        if (allocated(error)) return
        call check(error, nthnull_par == nthnull_ser, &
            "a threaded extraction must give pf_nth_element's serial answer with a mask")
        if (allocated(error)) return
        call check(error, q_par == q_ser .and. nnull_par == nnull_ser, &
            "a threaded extraction must give pf_nth_quantile's serial value and null count")
    end subroutine test_selection_threads_extraction


    !> Every allocation the radix path can fail must decline to a slower route, never abort, and
    !! must still answer identically.
    !!
    !! **Six of `parquet_debug_set_sort_radix_fail_alloc`'s eight selectors are only reachable with a
    !! TEAM**, which is why they live here rather than beside selector 1 in
    !! `test_radix_path_alloc_fallback`: the threaded tier split (3), Design B's task arrays (4) and
    !! the four allocations `grow_run_list` makes in turn (5 to 8) all sit inside `#ifdef _OPENMP`
    !! behind an `nt > 1` test, and selector 2's run-level arm needs `nrun >= 2 * nt` runs on top of
    !! that -- which `str_prefix_column`'s single run can never supply.
    !!
    !! **What each forced arm is asserted on, because "the answer is unchanged" is most of what a
    !! fallback guarantees and is therefore nearly vacuous on its own:**
    !!
    !! * **4** declines Design B, so `parquet_debug_sort_design()` must report 1 (Design A) where the
    !!   control reports 2. That is a sharp negative control: a selector that did nothing would leave
    !!   it at 2.
    !! * **2, 5, 6, 7 and 8** all end with the string runs refined one at a time rather than by the
    !!   team, so `parquet_debug_sort_refine_runs()` must be 0 where the control is positive. For 2
    !!   that counter is doubly pinned -- the run-level dispatch is skipped AND every run's own
    !!   sub-bucket loop declines, because selector 2 also fails the buffer `sort_radix_refine_run`
    !!   allocates lazily.
    !! * **3** has no observable at all, and this says so rather than implying otherwise: the
    !!   threaded tier split and the serial one produce the same `ra`, `nv`, `nnan`, `nnull` and
    !!   value range by construction, and nothing counts which ran. Its arm asserts the permutation
    !!   and is kept because the fallback would otherwise ship with every mutation to it surviving.
    subroutine test_radix_alloc_fallback_selectors(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first disagreement.
        integer(int64), parameter :: n = 16384_int64  !! integer rows, enough to reach Design B.
        integer(int64), parameter :: ns = 40000_int64 !! string rows, enough for many refine runs.
        integer, parameter :: str_sel(5) = [2, 5, 6, 7, 8] !! the selectors a string fixture reaches.
        integer(int64) :: v(n)
        character(len=11) :: sv(ns)
        integer(int64), allocatable :: ref(:), got(:), sref(:), sgot(:)
        integer(int64) :: i, g, q, design_ctl, design_4, runs_ctl
        integer(int64) :: runs_forced(size(str_sel))
        logical :: same_int(3:4), same_str(size(str_sel))
        integer :: k
        ! **Preconditions, declared rather than assumed.** Every selector below guards an allocation
        ! made only when a team exists, so without one the forced arms run exactly the code the
        ! control arm runs and every equality holds for the wrong reason -- the vacuous pass this
        ! file's other threading tests skip for.
#ifndef _OPENMP
        call skip_test(error, "needs OpenMP: the threaded tier split, Design B and the " // &
            "run-level string refine all sit inside #ifdef _OPENMP in " // &
            "src/parquet_argsort_engine.f90, so none of the allocations these selectors " // &
            "fail is ever made and every assertion below would be vacuous")
        return
#else
        if (omp_get_num_procs() < 2) then
            call skip_test(error, "needs at least two processors: the engine clamps an " // &
                "explicit threads= to omp_get_num_procs(), so it resolves to 1 here and no " // &
                "threaded design is entered")
            return
        end if
#endif
        !
        ! The integer fixture is `test_engine_refine_floor`'s, which is known to reach Design B: a
        ! heavily skewed top byte with a second varying byte below it.
        do i = 1_int64, n
            if (i <= 6000_int64) then
                g = 0_int64
            else
                g = 1_int64 + mod(i, 2_int64)
            end if
            v(i) = ishft(g, 56) + ishft(mod(i, 13_int64), 48)
        end do
        ! The string fixture shares four leading characters, so the eight-byte radix window still
        ! splits the column into ~100 runs -- enough to clear the run-level loop's `2 * nt` floor --
        ! and every value outruns the window, so every run really does need refining.
        do i = 1_int64, ns
            q = mod(i * 2654435761_int64, 100000_int64)
            write (sv(i), '(a,i7.7)') "aaaa", q
        end do
        !
        ! Every piece of global state is restored BEFORE the first assertion: each `check` can
        ! return early, and a leaked forced-failure selector would silently disable the radix path
        ! for every later test in this suite.
        call force_fortran_parallel_threshold(1_int64)
        !
        call pf_argsort(v, ref, threads=4)
        design_ctl = parquet_debug_sort_design()
        do k = 3, 4
            call parquet_debug_set_sort_radix_fail_alloc(k)
            call pf_argsort(v, got, threads=4)
            if (k == 4) design_4 = parquet_debug_sort_design()
            same_int(k) = (size(got) == size(ref))
            if (same_int(k)) same_int(k) = all(got == ref)
        end do
        call parquet_debug_set_sort_radix_fail_alloc(0)
        !
        call pf_argsort(sv, sref, threads=4)
        runs_ctl = parquet_debug_sort_refine_runs()
        do k = 1, size(str_sel)
            call parquet_debug_set_sort_radix_fail_alloc(str_sel(k))
            call pf_argsort(sv, sgot, threads=4)
            runs_forced(k) = parquet_debug_sort_refine_runs()
            same_str(k) = (size(sgot) == size(sref))
            if (same_str(k)) same_str(k) = all(sgot == sref)
        end do
        call parquet_debug_set_sort_radix_fail_alloc(0)
        !
        call force_fortran_parallel_threshold(0_int64)
        !
        call check(error, design_ctl == 2_int64, &
            "the integer fixture must reach Design B, or selector 4's decline proves nothing")
        if (allocated(error)) return
        call check(error, runs_ctl > 0_int64, &
            "the string fixture must reach the run-level refine dispatch, or selectors 2 and 5-8 " // &
            "have nothing to decline")
        if (allocated(error)) return
        call check(error, same_int(3), &
            "failing the threaded tier split's counters must give the serial permutation exactly")
        if (allocated(error)) return
        call check(error, same_int(4), &
            "failing Design B's task arrays must give the same permutation as Design B")
        if (allocated(error)) return
        call check(error, design_4 == 1_int64, &
            "failing Design B's task arrays must fall through to Design A, not run Design B anyway")
        if (allocated(error)) return
        do k = 1, size(str_sel)
            call check(error, same_str(k), &
                "a forced allocation failure changed the string permutation, selector " // &
                itoa(str_sel(k)))
            if (allocated(error)) return
            call check(error, runs_forced(k) == 0_int64, &
                "the forced arm still dispatched runs to the team, so the failure was not " // &
                "engaged, selector " // itoa(str_sel(k)))
            if (allocated(error)) return
        end do
    end subroutine test_radix_alloc_fallback_selectors


    !
    !> Design B must refine a SUB-bucket that is itself oversized, not only a top-level one.
    !!
    !! **The second level is what the ping-pong buffer choice exists for and nothing else reaches
    !! it.** Every initial task sits in the split's output buffer, so a first refinement always
    !! scatters that buffer back into the other one; its children then sit in the opposite buffer,
    !! and refining one of THOSE takes the other arm of the same test. `test_engine_refine_floor`'s
    !! fixture refines once and stops -- its sub-buckets are ~460 rows against a 4096 target -- so
    !! that arm had never run, and a mutation swapping its source and destination would have been
    !! invisible: the answer only changes when a task is refined twice.
    !!
    !! **The fixture is a skew at each of three digits, which is the only shape that gets there.**
    !! A task is refinable only while it owes at least one digit below the one it was split on
    !! (`tdmax >= 1`), so two rounds need three varying bytes: byte 7 splits, byte 6 refines, byte 5
    !! refines again. At four threads the target is `nv / 4` = 4096, so byte 7 must leave a bucket
    !! above that whose byte-6 majority is also above it.
    !!
    !! **The bucket count is what pins the two rounds**, since the permutation is identical either
    !! way: three initial buckets, plus two from the first refinement, plus eleven from the second.
    !! Five is what one round alone would leave -- and the negative control is exactly that, a task
    !! floor set between the two sub-bucket sizes so the walk refines once and stops. Without it,
    !! "more than five buckets" would be satisfied by any split at all.
    subroutine test_engine_refine_two_levels(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        integer(int64), parameter :: n = 16384_int64
        integer(int64), parameter :: one_round_floor = 13000_int64
        !! Above the 12000-row sub-bucket and below the 14000-row bucket that produced it, so the
        !! first refinement happens and the second cannot.
        integer(int64) :: v(n)
        integer(int64), allocatable :: ref(:), got_one(:), got_two(:)
        integer(int64) :: one_buckets, two_buckets, two_design
        integer(int64) :: i, g, h
        ! **Preconditions, declared rather than assumed.** Design B is compiled out without OpenMP
        ! and declines without a team, so both arms would be the serial LSD loop and the bucket
        ! counts below would both be zero -- passing against an engine that never split anything.
#ifndef _OPENMP
        call skip_test(error, "needs OpenMP: the Design A/B dispatch in " // &
            "src/parquet_argsort_engine.f90 sits inside #ifdef _OPENMP, so no split happens " // &
            "and every assertion below would be vacuous")
        return
#else
        if (omp_get_num_procs() < 2) then
            call skip_test(error, "needs at least two processors: the engine clamps an " // &
                "explicit threads= to omp_get_num_procs(), so it resolves to 1 here and " // &
                "Design B is never entered")
            return
        end if
#endif
        !
        ! Byte 7 leaves 14000 rows in one bucket, byte 6 leaves 12000 of those in one sub-bucket,
        ! and byte 5 finally spreads them over eleven. Both of the first two are above the 4096
        ! target, which is what makes the walk refine twice.
        do i = 1_int64, n
            if (i <= 14000_int64) then
                g = 0_int64
            else
                g = 1_int64 + mod(i, 2_int64)
            end if
            if (i <= 12000_int64) then
                h = 0_int64
            else
                h = 1_int64
            end if
            v(i) = ishft(g, 56) + ishft(h, 48) + ishft(mod(i, 11_int64), 40)
        end do
        !
        ! Restored before the first assertion, for the reason `test_engine_refine_floor` gives: a
        ! leaked task floor or engine selection breaks tests that never mention sorting designs.
        call force_fortran_parallel_threshold(1_int64)
        call pf_argsort(v, ref, threads=1)
        !
        call parquet_debug_set_sort_task_floor(one_round_floor)
        call pf_argsort(v, got_one, threads=4)
        one_buckets = parquet_debug_sort_split_buckets()
        !
        call parquet_debug_set_sort_task_floor(1_int64)
        call pf_argsort(v, got_two, threads=4)
        two_buckets = parquet_debug_sort_split_buckets()
        two_design = parquet_debug_sort_design()
        !
        call parquet_debug_set_sort_task_floor(-1_int64)
        call force_fortran_parallel_threshold(0_int64)
        !
        call check(error, two_design == 2_int64, &
            "the fixture must reach Design B, or this test says nothing about its refinement")
        if (allocated(error)) return
        call check(error, all(got_one == ref), "one refinement round must not change the answer")
        if (allocated(error)) return
        call check(error, all(got_two == ref), "refining twice must not change the answer either")
        if (allocated(error)) return
        call check(error, one_buckets == 5_int64, &
            "the control must refine exactly once: 3 top-byte buckets, one of them replaced by 2")
        if (allocated(error)) return
        call check(error, two_buckets > one_buckets, &
            "only one refinement round ran, so a sub-bucket was never itself refined and the " // &
            "second-level scatter -- the one that reads the OTHER buffer pair -- never executed")
    end subroutine test_engine_refine_two_levels


    !
    !> The threaded key extraction must agree with the serial one for `real32` and `logical` too.
    !!
    !! **One extraction per element family, and three of the six were covered by nothing.** Each
    !! `extract_*` has its own threaded twin -- a separate subroutine, deliberately, because writing
    !! the `!$omp parallel do` inline measured the SERIAL arm 2.4x slower -- so a plumbing error in
    !! one says nothing about the others. `test_partial_argsort_threads_tail` and
    !! `test_selection_threads_extraction` both drive `int32` only.
    !!
    !! **Forcing the floor is the whole point**, exactly as in those two: `tail_team` declines a team
    !! below `max(32768, 1024*nt)` elements, which no fixture here reaches, so without
    !! `parquet_debug_set_sort_tail_min_rows` both arms would run the same serial code.
    !!
    !! The masked arm is the sharp one for `logical`: values and mask travel through the same
    !! threaded pre-fill-and-extract pair, so a null placed by the wrong thread's chunk is a wrong
    !! answer that only a null-carrying fixture can see.
    subroutine test_tail_extraction_real32_logical(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first disagreement.
        integer, parameter :: n = 4096
        real(real32) :: v32(n)
        logical :: lv(n), mask(n)
        integer(int64), allocatable :: ref32(:), got32(:), refl(:), gotl(:)
        integer(int64), allocatable :: refn(:), gotn(:)
        integer :: i
        ! **Preconditions, declared rather than assumed.** With the threaded branches preprocessed
        ! out, or on a machine where an explicit threads= clamps back to 1, both arms below are the
        ! same serial code and every assertion passes without testing anything.
#ifndef _OPENMP
        call skip_test(error, "needs OpenMP: extract_f32_par and extract_bool_par " // &
            "(src/parquet_argsort_kernel.f90) sit inside #ifdef _OPENMP, so both arms below " // &
            "would run the same serial code and the equality would hold for the wrong reason")
        return
#else
        if (omp_get_num_procs() < 2) then
            call skip_test(error, "needs at least two processors: resolve_thread_count clamps " // &
                "an explicit threads= to omp_get_num_procs(), so threads=4 resolves to 1 here " // &
                "and no tail pass is threaded")
            return
        end if
#endif
        !
        ! Neither sorted nor reverse-sorted, and carrying ties, so an extraction that dropped or
        ! misplaced a chunk cannot coincide with the right answer.
        do i = 1, n
            v32(i) = real(mod(i * 7919, 1021), real32) * 0.5_real32
            lv(i) = (mod(i, 3) == 0)
            mask(i) = (mod(i, 17) /= 0)
        end do
        !
        ! Restored BEFORE the first assertion: every `check` can return early, and a leaked floor
        ! would silently rethread every later test in this suite.
        call parquet_debug_set_sort_tail_min_rows(1_int64)
        call pf_argsort(v32, got32, threads=4)
        call pf_argsort(lv, gotl, threads=4)
        call pf_argsort(lv, gotn, is_valid=mask, threads=4)
        call parquet_debug_set_sort_tail_min_rows(-1_int64)
        !
        call pf_argsort(v32, ref32, threads=1)
        call pf_argsort(lv, refl, threads=1)
        call pf_argsort(lv, refn, is_valid=mask, threads=1)
        !
        call check(error, size(got32) == n, "the forced-floor run must still return n indices")
        if (allocated(error)) return
        call check(error, all(got32 == ref32), &
            "a threaded real32 extraction must give the serial permutation exactly")
        if (allocated(error)) return
        call check(error, all(gotl == refl), &
            "a threaded logical extraction must give the serial permutation exactly")
        if (allocated(error)) return
        call check(error, all(gotn == refn), &
            "a threaded logical extraction must place the nulls exactly as the serial one does")
    end subroutine test_tail_extraction_real32_logical


    !
    !> The same tail check for the two element types whose extraction lives one tier UP.
    !!
    !! **Why this is a separate test from the one above rather than two more arms of it.** The six
    !! intrinsic types extract in `src/parquet_argsort_kernel.f90`, the Arrow-free argsort tier;
    !! `parquet_date` and `parquet_time` extract in `src/parquet_sorting_keys.f90`, which is what
    !! `parquet_sorting` adds on top. `extract_date_par`/`extract_time_par` are therefore a second,
    !! independent copy of the pre-fill-then-extract pattern -- with their own `tail_team` call, own
    !! static schedules and own blanket fill -- and nothing in the tier below can exercise them.
    !!
    !! **`parquet_timestamp` is deliberately absent**: it binds as TWO integer keys and its
    !! extraction has no threaded arm at all, so there is nothing here for it to test.
    !!
    !! The fixture carries nulls because a date's null state lives inside the element rather than in
    !! a caller's mask, so the serial and threaded arms must agree about where the nulls land as well
    !! as about the values -- and the validity pass is the one part of `extract_date` the threaded
    !! arm does NOT take over, which is exactly the seam a wrong split would show up at.
    subroutine test_tail_extraction_date_time(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first disagreement.
        integer, parameter :: n = 4096
        type(parquet_date) :: dv(n)
        type(parquet_time) :: tv(n)
        integer(int64), allocatable :: refd(:), gotd(:), reft(:), gott(:)
        integer :: i, day, sec
        ! **Preconditions, declared rather than assumed.** With the threaded branches preprocessed
        ! out, or on a machine where an explicit threads= clamps back to 1, both arms below are the
        ! same serial code and every assertion passes without testing anything.
#ifndef _OPENMP
        call skip_test(error, "needs OpenMP: extract_date_par and extract_time_par " // &
            "(src/parquet_sorting_keys.f90) sit inside #ifdef _OPENMP, so both arms below " // &
            "would run the same serial code and the equality would hold for the wrong reason")
        return
#else
        if (omp_get_num_procs() < 2) then
            call skip_test(error, "needs at least two processors: resolve_thread_count clamps " // &
                "an explicit threads= to omp_get_num_procs(), so threads=4 resolves to 1 here " // &
                "and no tail pass is threaded")
            return
        end if
#endif
        !
        ! Neither ordered nor reverse-ordered, with ties every few rows and every eleventh element
        ! left null, so an extraction that dropped, duplicated or misplaced a chunk cannot coincide
        ! with the right answer.
        do i = 1, n
            if (mod(i, 11) == 0) cycle              ! left null: a default-initialized element IS null
            day = 1 + mod(i * 7919, 28)
            call dv(i)%set(2000 + mod(i * 31, 40), 1 + mod(i * 7, 12), day)
            sec = mod(i * 7919, 86400)
            call tv(i)%set(sec / 3600, mod(sec / 60, 60), mod(sec, 60))
        end do
        !
        ! Restored BEFORE the first assertion: every `check` can return early, and a leaked floor
        ! would silently rethread every later test in this suite.
        call parquet_debug_set_sort_tail_min_rows(1_int64)
        call pf_argsort(dv, gotd, threads=4)
        call pf_argsort(tv, gott, threads=4)
        call parquet_debug_set_sort_tail_min_rows(-1_int64)
        !
        call pf_argsort(dv, refd, threads=1)
        call pf_argsort(tv, reft, threads=1)
        !
        call check(error, size(gotd) == n, "the forced-floor run must still return n indices")
        if (allocated(error)) return
        call check(error, all(gotd == refd), &
            "a threaded parquet_date extraction must give the serial permutation exactly")
        if (allocated(error)) return
        call check(error, all(gott == reft), &
            "a threaded parquet_time extraction must give the serial permutation exactly")
        if (allocated(error)) return
        ! Absolute, not just an A/B: both arms agreeing on a permutation that put the nulls in the
        ! wrong place would still agree. Every eleventh row is null and nulls sort last by default,
        ! so the final `n/11` entries of the permutation must be exactly the null rows.
        call check(error, all([(mod(int(gotd(i), int32), 11) == 0, i = n - n / 11 + 1, n)]), &
            "the threaded date sort must still place every null row in the trailing null tier")
    end subroutine test_tail_extraction_date_time


    !
    !> Makes the FORTRAN engine's threaded paths engage at `rows`, or restores their built-in
    !> floors when `rows <= 0`.
    !!
    !! **Fortran-only, deliberately.** The C++ engine has a floor of its own
    !! (`parquet_debug_set_sort_parallel_min_rows`, a `bind(C)` override on `kSortParallelMinRows`)
    !! and `test_sorting_cpp`'s own helper drives that one too, because its tests run both engines.
    !! Nothing in THIS file switches engines, so every sort here runs on the shipped Fortran engine
    !! and the C++ floor would have no effect on it -- driving it anyway is what put 23 of these
    !! tests in the C++ half in the first place. See feature_tests.md §8.2.
    !
    subroutine force_fortran_parallel_threshold(rows)
        integer(int64), intent(in) :: rows !! new threshold; <= 0 restores the built-in floors.

        if (rows <= 0_int64) then
            call parquet_debug_set_sort_engine_min_rows(-1_int64)
            call parquet_debug_set_sort_tail_min_rows(-1_int64)
        else
            call parquet_debug_set_sort_engine_min_rows(rows)
            call parquet_debug_set_sort_tail_min_rows(rows)
        end if
    end subroutine force_fortran_parallel_threshold

    !
    !> Opens a thread team AND makes the threaded radix accept the array, so a test-sized input
    !> really is decomposed across threads; `<= 0` restores all three built-in floors.
    !!
    !! **THREE floors, and the third is the one that was missing.** `force_fortran_parallel_threshold`
    !! above lowers the two that decide whether a *team* opens. It does not lower
    !! `SORT_RADIX_MIN_ROWS = 128`, which decides whether the threaded radix *runs at all* -- and
    !! the radix designs are the only threaded decomposition this engine has. An array under 128
    !! rows therefore opened a team and then sorted on a serial path inside it, so every
    !! "threaded equals serial" assertion below 128 rows was comparing a serial answer with
    !! itself. Measured before the fix, over the dense sweeps' own fixture: `n = 2..127` reported
    !! `parquet_debug_sort_design() == 0` at every thread count -- 126 of the 399 sizes, 882 of
    !! the 2793 sweep points. With this helper all 2793 reach Design B.
    !!
    !! **This is the Fortran engine's analogue of `force_merge_segments`, which it replaces.**
    !! The two engines parallelise differently and the distinction is why the old helper could not
    !! be carried across: the C++ engine sorts chunks and co-ranks a parallel MERGE, so its floor
    !! is a minimum merge segment; the Fortran engine count-prefix-scatters into disjoint BUCKET
    !! ranges and never merges at all, so its floor is a minimum row count for the radix.
    !! `parquet_debug_set_sort_merge_min_segment` is therefore not merely a C++ symbol these tests
    !! cannot reach -- it names a phase this engine does not have. (Nor is
    !! `parquet_debug_set_sort_task_floor` the analogue, which was the standing guess: that one
    !! sizes Design B's *refinement* tasks once the decomposition is already running.)
    !!
    !! Restoring the radix floor needs its own `<= 0` call, so a test that lowers it here and
    !! leaves it lowered would silently change the path every later test in this suite takes.
    !! `feature_risks.md` Risk-49.
    subroutine force_fortran_bucket_split(rows)
        integer(int64), intent(in) :: rows !! new floor for all three; <= 0 restores the built-in ones.

        call force_fortran_parallel_threshold(rows)
        if (rows <= 0_int64) then
            call parquet_debug_set_sort_radix_min_rows(-1_int64)
        else
            call parquet_debug_set_sort_radix_min_rows(rows)
        end if
    end subroutine force_fortran_bucket_split

    !
    !> `.true.` when the last sort really was decomposed across threads by the radix.
    !!
    !! **The positive control every sweep below needs, and the only one that separates the two
    !! failures.** A threaded sort that quietly falls back to a serial path returns the identical
    !! permutation -- that identity is precisely what makes threading safe -- so the oracle passes
    !! just as happily against a sort that never threaded. `parquet_debug_sort_design()` is 0
    !! unless one of the two radix designs ran, and the designs run only with a team: measured at
    !! `n = 4000`, a `real64` key reports design 2 at `threads = 2..8` and design **0** at
    !! `threads = 1`. So this answers the question the oracle cannot.
    !!
    !! Design **1** (the LSD chain) and design **2** (the bucket split) are both real
    !! decompositions and both count -- which design a fixture reaches is a property of its key
    !! family, not of the threading: measured at `n = 4000`, `real64` reaches 2 while a
    !! `character` key and a two-key set reach 1.
    function threaded_split_ran() result(ok)
        logical :: ok !! .true. when a threaded radix design ran for the last sort.

        ok = parquet_debug_sort_design() /= 0_int64
    end function threaded_split_ran

    !
    !> `.true.` when the last sort was decomposed by the design named, specifically.
    !!
    !! **`threaded_split_ran()` is deliberately family-agnostic and that leaves one hole this
    !! closes.** The sweeps are all `real64` and reach design 2; design **1** (the LSD chain) is
    !! threaded in `test_split_key_families` and NOWHERE ELSE, so if a future key-family change
    !! routed a `character` or multi-key sort to design 2 instead, `/= 0` would keep passing and
    !! design 1 would silently lose its only threaded coverage. Asserting the design by number is
    !! what makes that a failure rather than a quiet gap.
    function threaded_design_was(expected) result(ok)
        integer(int64), intent(in) :: expected !! 1 = the LSD chain, 2 = the bucket split.
        logical :: ok                          !! .true. when that design ran for the last sort.

        ok = parquet_debug_sort_design() == expected
    end function threaded_design_was

    !
    !> Forces the introsort by declining BOTH single-key fast paths, or restores the shipped
    !> defaults. Both knobs are Fortran settings, so this reaches no C++.
    !!
    !! Every test that observes the introsort -- its heapsort arm, its depth limit, its presort
    !! invariant -- needs the range actually to reach it. Both fast paths answer identically, so a
    !! test intercepted by one of them does not fail; it goes quiet, which is worse. That is
    !! `feature_risks.md` Risk-49.
    !!
    !! Calling this in pairs (`.true.` ... `.false.`) rather than reading and restoring the previous
    !! values is safe because this suite is excluded from test-drive's per-test parallelism -- both
    !! knobs are process-global, which is why that exclusion exists.
    subroutine engine_only_introsort(on)
        logical, intent(in) :: on !! .true. forces the introsort; .false. restores the shipped floors.

        if (on) then
            call parquet_set_sort_counting_path(.false.)
            call parquet_debug_set_sort_radix_min_rows(huge(0_int64))
        else
            call parquet_set_sort_counting_path(.true.)
            call parquet_debug_set_sort_radix_min_rows(-1_int64)
        end if
    end subroutine engine_only_introsort

    !
    !> `pf_match`, `pf_match_all` and `pf_in` against a BRUTE-FORCE oracle over one fixture.
    !>
    !> The oracle is the point of this test. It is O(nl*nr), shares nothing with the engine, and
    !> so cannot hide a defect in the run walk the way comparing two engine paths against each
    !> other would. It also pins the two presentation choices the walk makes: `pf_match` reports
    !> the SMALLEST matching index, and `pf_match_all` reports every index ascending.
    subroutine test_match_basic(error)
        type(error_type), allocatable, intent(out) :: error
        integer(int32), parameter :: L(8) = [30, 10, 99, 20, 10, 40, 20, 77]
        integer(int32), parameter :: R(7) = [20, 10, 40, 20, 10, 10, 55]
        integer(int64), allocatable :: m(:), off(:), mt(:)
        logical, allocatable :: inmask(:)
        integer(int64) :: nm, want, cnt, pos
        integer :: i, j
        logical :: ok

        call pf_match(L, R, m, n_matched=nm)
        call check(error, size(m) == 8, "pf_match must answer once per element of `left`")
        if (allocated(error)) return
        ok = .true.
        do i = 1, 8
            want = 0_int64
            do j = 1, 7
                if (R(j) == L(i)) then
                    want = int(j, int64)
                    exit
                end if
            end do
            if (m(i) /= want) ok = .false.
        end do
        call check(error, ok, "pf_match must name the FIRST equal element of `right`, or 0")
        if (allocated(error)) return
        call check(error, nm == count(m /= 0_int64, kind=int64), &
            "n_matched must count the left elements that found something")
        if (allocated(error)) return
        call check(error, nm == 5_int64, "five of these eight left elements have a counterpart")
        if (allocated(error)) return

        call pf_match_all(L, R, off, mt)
        call check(error, size(off) == 9, "pf_match_all's offsets must be size(left)+1 long")
        if (allocated(error)) return
        call check(error, off(1) == 1_int64, "pf_match_all's offsets must start at 1")
        if (allocated(error)) return
        call check(error, size(mt) == 11, "these two arrays hold eleven matching pairs")
        if (allocated(error)) return
        ok = .true.
        do i = 1, 8
            cnt = 0_int64
            do j = 1, 7
                if (R(j) == L(i)) cnt = cnt + 1_int64
            end do
            if (off(i + 1) - off(i) /= cnt) ok = .false.
        end do
        call check(error, ok, "each left element's CSR range must be as long as its match count")
        if (allocated(error)) return
        ok = .true.
        do i = 1, 8
            do pos = off(i), off(i + 1) - 1_int64
                if (R(mt(pos)) /= L(i)) ok = .false.
                if (pos > off(i)) then
                    if (mt(pos) <= mt(pos - 1_int64)) ok = .false.
                end if
            end do
        end do
        call check(error, ok, "every CSR entry must be equal to its left element, and ascending")
        if (allocated(error)) return
        ! The two forms have to agree, and this is also the canary for the engine's stability:
        ! pf_match takes a minimum while pf_match_all takes permutation order, so they coincide
        ! only while equal keys really do emerge in increasing row index.
        ok = .true.
        do i = 1, 8
            if (off(i + 1) > off(i)) then
                if (m(i) /= mt(off(i))) ok = .false.
            else
                if (m(i) /= 0_int64) ok = .false.
            end if
        end do
        call check(error, ok, "pf_match must equal the first entry of pf_match_all's range")
        if (allocated(error)) return

        call pf_in(L, R, inmask)
        call check(error, size(inmask) == 8, "pf_in must answer once per element of `values`")
        if (allocated(error)) return
        call check(error, all(inmask .eqv. (m /= 0_int64)), &
            "pf_in must be pf_match reduced to whether there was one")
        if (allocated(error)) return
        ! The worked example on doc/pages/utilities/sorting.md, asserted rather than arithmetic
        ! done by hand -- a guide example nothing runs is a guide example that drifts.
        block
            integer(int64) :: gid(5), gref(4), gn
            integer(int64), allocatable :: gm(:)
            logical, allocatable :: gkeep(:)
            gid = [30_int64, 10_int64, 99_int64, 20_int64, 10_int64]
            gref = [20_int64, 10_int64, 40_int64, 10_int64]
            call pf_match(gid, gref, gm, n_matched=gn)
            call check(error, all(gm == [0_int64, 2_int64, 0_int64, 1_int64, 2_int64]), &
                "the sorting guide's pf_match example must give the answer it prints")
            if (allocated(error)) return
            call check(error, gn == 3_int64, "the guide's example must report n_matched = 3")
            if (allocated(error)) return
            call pf_in(gid, gref, gkeep)
            call check(error, all(gkeep .eqv. [.false., .true., .false., .true., .true.]), &
                "the sorting guide's pf_in example must give the answer it prints")
        end block
    end subroutine test_match_basic
    !
    !> A null matches NOTHING, on either side -- including another null of the same value.
    !>
    !> The five positions cover the whole truth table (valid/valid, null/valid, valid/null,
    !> null/null, valid/valid), and the last assertion is the negative control: with the masks
    !> withheld the identical arrays match everywhere, so the four refusals above are about the
    !> masks rather than about the values.
    subroutine test_match_nulls(error)
        type(error_type), allocatable, intent(out) :: error
        integer(int32), parameter :: L(5) = [10, 20, 30, 40, 50]
        integer(int32), parameter :: R(5) = [10, 20, 30, 40, 50]
        logical, parameter :: LV(5) = [.true., .false., .true., .false., .true.]
        logical, parameter :: RV(5) = [.true., .true., .false., .false., .true.]
        integer(int64), allocatable :: m(:), off(:), mt(:)
        logical, allocatable :: inmask(:)

        call pf_match(L, R, m, is_valid_left=LV, is_valid_right=RV)
        call check(error, m(1) == 1_int64, "a valid element must still find its counterpart")
        if (allocated(error)) return
        call check(error, m(2) == 0_int64, "a NULL left element must find nothing")
        if (allocated(error)) return
        call check(error, m(3) == 0_int64, "a NULL right element must never be found")
        if (allocated(error)) return
        call check(error, m(4) == 0_int64, "a null must not match another null -- unknown /= unknown")
        if (allocated(error)) return
        call check(error, m(5) == 5_int64, "the fifth pair is valid on both sides and must match")
        if (allocated(error)) return
        call pf_match_all(L, R, off, mt, is_valid_left=LV, is_valid_right=RV)
        call check(error, size(mt) == 2, "only the two all-valid pairs may contribute a pair")
        if (allocated(error)) return
        call pf_in(L, R, inmask, is_valid=LV, is_valid_set=RV)
        call check(error, all(inmask .eqv. [.true., .false., .false., .false., .true.]), &
            "pf_in must report a null as a member of no set at all")
        if (allocated(error)) return
        ! Negative control -- without it every assertion above would also pass against a match
        ! that simply never matched anything.
        call pf_match(L, R, m)
        call check(error, all(m == [1_int64, 2_int64, 3_int64, 4_int64, 5_int64]), &
            "with the masks withheld the same arrays must match everywhere")
    end subroutine test_match_nulls
    !
    !> A NaN is a VALUE and matches; `-0.0` and `+0.0` are one value.
    !>
    !> Both are documented contracts rather than accidents of the comparator, and both are the
    !> kind of property a future engine change could flip with nothing else noticing. The signed
    !> zero is built at runtime and its own precondition asserted first, because a fixture whose
    !> whole discriminating power is one bit is worthless if a compiler folded that bit away.
    subroutine test_match_nan_and_zero(error)
        type(error_type), allocatable, intent(out) :: error
        real(real64) :: lv(4), rv(4), nan, zero, negz
        integer(int64), allocatable :: m(:), off(:), mt(:)

        nan = ieee_value(1.0_real64, ieee_quiet_nan)
        zero = 0.0_real64
        negz = sign(zero, -1.0_real64)
        call check(error, negz == 0.0_real64 .and. transfer(negz, 0_int64) < 0_int64, &
            "this fixture needs a real negative zero; the compiler did not produce one")
        if (allocated(error)) return
        lv = [1.0_real64, nan, zero, 2.0_real64]
        rv = [nan, negz, nan, 1.0_real64]
        call pf_match(lv, rv, m)
        call check(error, m(1) == 4_int64, "an ordinary value must match its ordinary counterpart")
        if (allocated(error)) return
        call check(error, m(2) == 1_int64, &
            "a NaN must match a NaN -- every NaN is one value to the comparator -- at the lowest index")
        if (allocated(error)) return
        call check(error, m(3) == 2_int64, "+0.0 and -0.0 must be one value")
        if (allocated(error)) return
        call check(error, m(4) == 0_int64, "2.0 appears on neither side of the right array")
        if (allocated(error)) return
        ! The cartesian hazard the doc-comments warn about, in miniature: one NaN on the left
        ! meets BOTH NaNs on the right, so a pair count is a product rather than a sum.
        call pf_match_all(lv, rv, off, mt)
        call check(error, off(3) - off(2) == 2_int64, &
            "one NaN on the left must match every NaN on the right, not just the first")
    end subroutine test_match_nan_and_zero
    !
    !> Every element type reaches the engine differently, so a match passing for `int32` says
    !> nothing about the other ten. One shape throughout: `left` is [a, b, c] and `right` is
    !> [c, a], so the answer is [2, 0, 1] whatever the type.
    subroutine test_match_all_types(error)
        type(error_type), allocatable, intent(out) :: error
        integer(int64), allocatable :: m(:)
        integer(int64), parameter :: WANT(3) = [2_int64, 0_int64, 1_int64]
        integer :: k

        block
            integer(int32) :: l(3), r(2)
            l = [11_int32, 22_int32, 33_int32]
            r = [33_int32, 11_int32]
            call pf_match(l, r, m)
            call check(error, all(m == WANT), "int32 pf_match")
        end block
        if (allocated(error)) return
        block
            integer(int64) :: l(3), r(2)
            l = [11_int64, 22_int64, 33_int64]
            r = [33_int64, 11_int64]
            call pf_match(l, r, m)
            call check(error, all(m == WANT), "int64 pf_match")
        end block
        if (allocated(error)) return
        block
            real(real32) :: l(3), r(2)
            l = [1.5_real32, 2.5_real32, 3.5_real32]
            r = [3.5_real32, 1.5_real32]
            call pf_match(l, r, m)
            call check(error, all(m == WANT), "real32 pf_match")
        end block
        if (allocated(error)) return
        block
            real(real64) :: l(3), r(2)
            l = [1.5_real64, 2.5_real64, 3.5_real64]
            r = [3.5_real64, 1.5_real64]
            call pf_match(l, r, m)
            call check(error, all(m == WANT), "real64 pf_match")
        end block
        if (allocated(error)) return
        block
            ! `logical` has only two values, so the [a, b, c] shape does not fit: this is the
            ! same question asked with the fixture the type can express.
            logical :: l(3), r(2)
            l = [.true., .false., .true.]
            r = [.false., .true.]
            call pf_match(l, r, m)
            call check(error, all(m == [2_int64, 1_int64, 2_int64]), "logical pf_match")
        end block
        if (allocated(error)) return
        block
            character(len=4) :: l(3), r(2)
            l = ["aaa ", "bbb ", "ccc "]
            r = ["ccc ", "aaa "]
            call pf_match(l, r, m)
            call check(error, all(m == WANT), "character pf_match")
        end block
        if (allocated(error)) return
        block
            type(parquet_date) :: l(3), r(2)
            call l(1)%set(2020, 1, 1)
            call l(2)%set(2021, 6, 15)
            call l(3)%set(2022, 12, 31)
            call r(1)%set(2022, 12, 31)
            call r(2)%set(2020, 1, 1)
            call pf_match(l, r, m)
            call check(error, all(m == WANT), "parquet_date pf_match")
        end block
        if (allocated(error)) return
        block
            type(parquet_time) :: l(3), r(2)
            call l(1)%set(1, 0, 0)
            call l(2)%set(2, 0, 0)
            call l(3)%set(3, 0, 0)
            call r(1)%set(3, 0, 0)
            call r(2)%set(1, 0, 0)
            call pf_match(l, r, m)
            call check(error, all(m == WANT), "parquet_time pf_match")
        end block
        if (allocated(error)) return
        block
            ! A timestamp becomes TWO engine keys, so it is the one type whose concatenation
            ! appends a pair of buffers rather than one.
            type(parquet_timestamp) :: l(3), r(2)
            call l(1)%set(2020, 1, 1, 0, 0, 1)
            call l(2)%set(2020, 1, 1, 0, 0, 2)
            call l(3)%set(2020, 1, 1, 0, 0, 3)
            call r(1)%set(2020, 1, 1, 0, 0, 3)
            call r(2)%set(2020, 1, 1, 0, 0, 1)
            call pf_match(l, r, m)
            call check(error, all(m == WANT), "parquet_timestamp pf_match")
        end block
        if (allocated(error)) return
        block
            type(parquet_string_column) :: l, r
            call l%append_string("aaa")
            call l%append_string("bbb")
            call l%append_string("ccc")
            call r%append_string("ccc")
            call r%append_string("aaa")
            call pf_match(l, r, m)
            call check(error, all(m == WANT), "parquet_string_column pf_match")
        end block
        if (allocated(error)) return
        block
            type(parquet_column) :: l, r
            call l%init(PK_INT32, 3_int64)
            call l%set_all([11_int32, 22_int32, 33_int32])
            call r%init(PK_INT32, 2_int64)
            call r%set_all([33_int32, 11_int32])
            call pf_match(l, r, m)
            call check(error, all(m == WANT), "parquet_column pf_match")
        end block
        if (allocated(error)) return
        ! And the int32 output kind must agree with the int64 one everywhere, since each is a
        ! separate specific with its own narrowing step.
        block
            integer(int32) :: l(3), r(2)
            integer(int32), allocatable :: m32(:)
            l = [11_int32, 22_int32, 33_int32]
            r = [33_int32, 11_int32]
            call pf_match(l, r, m32)
            call pf_match(l, r, m)
            call check(error, all(int(m32, int64) == m), &
                "the int32 and int64 pf_match specifics must agree")
            if (allocated(error)) return
            call check(error, all([(m32(k) == int(WANT(k), int32), k = 1, 3)]), &
                "the int32 pf_match must give the same answer as the oracle")
        end block
    end subroutine test_match_all_types
    !
    !> The degenerate sizes, which are where an off-by-one in the CSR shows up first.
    subroutine test_match_empty(error)
        type(error_type), allocatable, intent(out) :: error
        integer(int32) :: full(3), none(0)
        integer(int64), allocatable :: m(:), off(:), mt(:)
        logical, allocatable :: inmask(:)

        full = [7_int32, 8_int32, 9_int32]
        call pf_match(none, full, m)
        call check(error, size(m) == 0, "an empty left array must give an empty answer")
        if (allocated(error)) return
        call pf_match_all(none, full, off, mt)
        call check(error, size(off) == 1 .and. off(1) == 1_int64, &
            "an empty left array's CSR must still carry its one sentinel offset")
        if (allocated(error)) return
        call check(error, size(mt) == 0, "an empty left array can produce no pairs")
        if (allocated(error)) return
        call pf_match(full, none, m)
        call check(error, all(m == 0_int64), "nothing can match against an empty right array")
        if (allocated(error)) return
        call pf_match_all(full, none, off, mt)
        call check(error, all(off == 1_int64) .and. size(off) == 4, &
            "every CSR range must be empty when the right array is")
        if (allocated(error)) return
        call pf_in(full, none, inmask)
        call check(error, .not. any(inmask), "no element is a member of the empty set")
        if (allocated(error)) return
        ! Both empty: the one path that never enters the engine at all.
        call pf_match(none, none, m)
        call check(error, size(m) == 0, "two empty arrays must answer without entering the engine")
        if (allocated(error)) return
        call pf_match_all(none, none, off, mt)
        call check(error, size(off) == 1 .and. size(mt) == 0, "two empty arrays produce an empty CSR")
    end subroutine test_match_empty
    !
    !> Two `character` arrays of different declared lengths compare at the wider of the two.
    !>
    !> `extract_chr` keys on the array's own `len`, so without the widening the two halves of the
    !> concatenation would be packed at two different widths and nothing would ever match. The
    !> second half of the test is what makes that a real assertion rather than a coincidence: a
    !> value present only as a padded short element must still be found.
    subroutine test_match_string_widths(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=3) :: l(3)
        character(len=7) :: r(3)
        integer(int64), allocatable :: m(:)

        l = ["ab ", "cd ", "zz "]
        r = ["cd     ", "ab     ", "qqqqqqq"]
        call pf_match(l, r, m)
        call check(error, all(m == [2_int64, 1_int64, 0_int64]), &
            "a short array and a wide one must match on their trimmed content")
        if (allocated(error)) return
        ! The reverse direction too: the wide array as `left`.
        call pf_match(r, l, m)
        call check(error, all(m == [2_int64, 1_int64, 0_int64]), &
            "the same must hold with the wide array on the left")
    end subroutine test_match_string_widths
    !
    !> A `pf_sort_keys` built from an EMPTY array must report zero rows, not one.
    !>
    !> The extractors allocate their value arrays with a `max(n, 1)` floor -- a one-element
    !> buffer for a zero-row key -- so measuring the row count back from the buffer could not
    !> tell an empty key from a one-row one, and reported ONE. `pf_argsort` then returned a
    !> one-element permutation naming a row that does not exist and `group_offsets` claiming one
    !> group, which is a wrong answer handed to a caller rather than an abort.
    !>
    !> **The array forms were never affected**, because they pass `size(values)` straight down --
    !> which is what makes them the oracle here: the two forms must agree, and before the fix
    !> they did not. `character` was likewise unaffected (its offsets array is genuinely `n+1`
    !> long), so a single-type test would have missed the defect entirely.
    subroutine test_keys_empty_row_count(error)
        type(error_type), allocatable, intent(out) :: error
        integer(int64), allocatable :: none64(:), perm(:), go(:), aperm(:), ago(:)
        real(real64), allocatable :: noneR(:)
        character(len=4), allocatable :: noneC(:)
        type(pf_sort_keys) :: keys

        allocate(none64(0), noneR(0), noneC(0))
        call pf_argsort(none64, aperm, group_offsets=ago)
        call check(error, size(aperm) == 0 .and. size(ago) == 1, &
            "the array form must give a zero-length permutation and the lone sentinel offset")
        if (allocated(error)) return
        call keys%add(none64)
        call pf_argsort(keys, perm, group_offsets=go)
        call check(error, size(perm) == 0, &
            "an int64 pf_sort_keys over an empty array must give a zero-length permutation")
        if (allocated(error)) return
        call check(error, size(go) == 1 .and. go(1) == 1_int64, &
            "an empty key has no groups, so group_offsets is the sentinel alone")
        if (allocated(error)) return
        call check(error, size(perm) == size(aperm) .and. size(go) == size(ago), &
            "the keys form and the array form must agree about an empty array")
        if (allocated(error)) return
        ! The real family allocates the same way and was wrong the same way.
        call keys%clear()
        call keys%add(noneR)
        call pf_argsort(keys, perm, group_offsets=go)
        call check(error, size(perm) == 0 .and. size(go) == 1, &
            "a real64 pf_sort_keys over an empty array must report zero rows too")
        if (allocated(error)) return
        ! And `character`, which was already right -- kept so a future change that unifies the
        ! three families cannot regress the one that worked.
        call keys%clear()
        call keys%add(noneC)
        call pf_argsort(keys, perm, group_offsets=go)
        call check(error, size(perm) == 0 .and. size(go) == 1, &
            "a character pf_sort_keys over an empty array must report zero rows")
        if (allocated(error)) return
        ! Two empty keys still agree with each other, so the row-count check does not start
        ! refusing a legitimate multi-key sort over an empty table.
        call keys%clear()
        call keys%add(none64)
        call keys%add(noneR)
        call pf_argsort(keys, perm, group_offsets=go)
        call check(error, size(perm) == 0 .and. size(go) == 1, &
            "two empty keys must agree about their row count rather than aborting")
    end subroutine test_keys_empty_row_count
end module test_sorting
