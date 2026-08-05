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
    !
    implicit none
    private
    public :: collect_tests_parquet_sorting
    !
contains
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
            new_unittest("pf_is_sorted agrees with pf_sort", test_is_sorted_agrees), &
            new_unittest("is_sorted is direction- and null-aware", test_is_sorted_options), &
            new_unittest("an int32 permutation equals the int64 one", test_perm_kinds_agree), &
            new_unittest("every scalar type sorts", test_all_scalar_types), &
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
            new_unittest("searches agree with a counting oracle", test_search_linear_oracle), &
            new_unittest("equal_range brackets duplicates", test_equal_range_duplicates), &
            new_unittest("searches follow a descending order", test_search_descending), &
            new_unittest("searches respect the null tier", test_search_nulls), &
            new_unittest("assume_sorted changes no answer", test_search_assume_sorted), &
            new_unittest("int32 and int64 searches agree", test_search_index_kinds), &
            new_unittest("strings and dates are searchable", test_search_other_types), &
            new_unittest("an empty array still gives an insertion point", test_search_degenerate), &
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
            new_unittest("minmax agrees with minval/maxval", test_minmax_basic), &
            new_unittest("minmax skips nulls and NaNs", test_minmax_skips), &
            new_unittest("argminmax reports the first of a tie", test_argminmax_ties), &
            new_unittest("argminmax accepts a parquet_column", test_argminmax_column), &
            new_unittest("merge equals a sort of the concatenation", test_merge_matches_sort), &
            new_unittest("merge tracks validity", test_merge_validity), &
            new_unittest("merge follows a descending order", test_merge_descending), &
            new_unittest("merge handles an empty input", test_merge_empty), &
            new_unittest("merge widens two string lengths", test_merge_string_widths), &
            new_unittest("a threaded sort equals the serial one", test_threads_identical), &
            new_unittest("threads are really created", test_threads_really_used), &
            new_unittest("auto is serial inside a parallel region", test_threads_auto_in_parallel), &
            new_unittest("threads=1 forces serial", test_threads_one_is_serial), &
            new_unittest("unique and rank take threads too", test_threads_on_derived) &
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
        integer :: k

        do k = 1, 40
            v(k) = int(mod(k * 7, 5), int32)   ! only five distinct values: the counting path's case
        end do
        call pf_argsort(v, fast)
        call parquet_set_sort_counting_path(.false.)
        call pf_argsort(v, slow)
        call parquet_set_sort_counting_path(.true.)
        call check(error, all(fast == slow), &
            "the counting fast path must produce exactly the comparator path's permutation")
        if (allocated(error)) return
        call check(error, all([(v(fast(k)), k = 1, 40)] == [(v(slow(k)), k = 1, 40)]), &
            "both paths must gather the same values in the same order")
    end subroutine test_counting_path_agrees
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
    end subroutine test_quantile_rounding_case
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
        integer :: k, c_fast, c_slow

        do k = 1, 60
            v(k) = int(mod(k * 7, 6), int32)   ! six distinct values: the counting path's case
        end do
        call pf_unique_count(v, c_fast)
        call pf_unique(v, fast)
        call parquet_set_sort_counting_path(.false.)
        call pf_unique_count(v, c_slow)
        call pf_unique(v, slow)
        call parquet_set_sort_counting_path(.true.)
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
        integer :: k

        do k = 1, 50
            v(k) = int(mod(k * 3, 4), int32)   ! four distinct values, heavily tied
        end do
        call pf_rank(v, fast)
        call parquet_set_sort_counting_path(.false.)
        call pf_rank(v, slow)
        call parquet_set_sort_counting_path(.true.)
        call check(error, all(fast == slow), &
            "both sort paths must produce the same competition ranks")
        if (allocated(error)) return
        call check(error, maxval(fast) == 50 - count(v == maxval(v)) + 1, &
            "the largest competition rank must be one past the count below the top run")
    end subroutine test_rank_both_paths
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
    ! ==================================================================================
    ! M4: parallel sorting
    ! ==================================================================================
    !
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
end module test_sorting
