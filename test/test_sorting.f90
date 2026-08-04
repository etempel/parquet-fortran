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
            new_unittest("rounding tokens are case-insensitive", test_quantile_rounding_case) &
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
        interface
            subroutine disable_counting(enable) bind(C, name="parquet_debug_set_disable_sort_counting_path")
                use iso_c_binding, only : c_int
                integer(c_int), value :: enable !! nonzero forces the comparator path.
            end subroutine disable_counting
        end interface

        do k = 1, 40
            v(k) = int(mod(k * 7, 5), int32)   ! only five distinct values: the counting path's case
        end do
        call pf_argsort(v, fast)
        call disable_counting(1)
        call pf_argsort(v, slow)
        call disable_counting(0)
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
        interface
            subroutine disable_counting(enable) bind(C, name="parquet_debug_set_disable_sort_counting_path")
                use iso_c_binding, only : c_int
                integer(c_int), value :: enable !! nonzero forces the comparator path.
            end subroutine disable_counting
        end interface

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
            call disable_counting(pass - 1)
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
        call disable_counting(0)
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
end module test_sorting
