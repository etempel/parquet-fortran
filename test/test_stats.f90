!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> Unit tests for `parquet_stats`.
!>
!> **What this suite is really asserting is a set of CONVENTIONS, not a computation.**
!> `pf_count_valid` counts; the interesting content is the exclusion policy every later reduction
!> in this module inherits from it, and each of those rules is a silent wrong answer if it breaks:
!>
!> * **the three exclusion classes are independent** -- a null, a NaN and a zero weight each remove
!>   an element on their own, and each is tested on its own so that a rule which happens to be
!>   implemented by another cannot pass for it;
!> * **the ORDER of exclusion is nullness, then NaN, then weight**, which is only observable in one
!>   place: an element that is null must never have its weight examined, so a null element carrying
!>   a NaN weight must count cleanly rather than abort. That is `test_null_element_ignores_its_weight`,
!>   and without it the ordering could be reversed with every other test still passing;
!> * **`skipnan` exists only where a NaN can exist.** The integer and logical specifics do not take
!>   it, so a test passing it to them would not compile -- which is the assertion;
!> * **an empty or fully excluded population answers 0 and does not abort**, because a per-group
!>   loop meets that case on real data.
!>
!> **Abort paths live elsewhere.** `error stop` kills the process, so a mismatched `is_valid`, a
!> mismatched `weights` and a negative/NaN/infinite weight are `stats_*` scenarios in
!> test/error_scenarios.f90, driven from test_errors.f90.
!>
!> Every test allocates its own arrays and shares no state, so nothing here needs a per-test
!> fixture filename.
module test_stats
    use parquet
    use test_stats_golden
    use iso_fortran_env, only : int32, int64, real32, real64
    use ieee_arithmetic, only : ieee_value, ieee_quiet_nan, ieee_get_flag, ieee_set_flag, &
        ieee_support_flag, ieee_divide_by_zero
    use testdrive, only : new_unittest, unittest_type, error_type, check, skip_test
    implicit none
    private

    public :: collect_tests_parquet_stats

contains

    !> Registers this module's tests with test-drive.
    subroutine collect_tests_parquet_stats(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:) !! the suite to fill.

        testsuite = [ &
            new_unittest("pf_count_valid counts every element of a clean array", test_plain_count), &
            new_unittest("pf_count_valid spans every numeric kind", test_every_kind), &
            new_unittest("pf_count_valid excludes the nulls is_valid marks", test_nulls_excluded), &
            new_unittest("pf_count_valid excludes NaNs by default and counts them under skipnan=.false.", &
                test_nan_policy), &
            new_unittest("pf_count_valid excludes a zero-weighted element", test_zero_weight_excluded), &
            new_unittest("pf_count_valid examines no weight belonging to a null element", &
                test_null_element_ignores_its_weight), &
            new_unittest("pf_count_valid answers 0 for an empty or fully excluded population", &
                test_empty_population_is_not_fatal), &
            new_unittest("the fixture recipe still matches the generator's", test_fixture_recipe), &
            new_unittest("pf_moments matches the 50-digit oracle, unweighted", test_golden_unweighted), &
            new_unittest("variance is shift-invariant at an offset of 1e9", test_shift_invariance), &
            new_unittest("pf_moments matches the oracle under both weight_type conventions", &
                test_golden_weighted), &
            new_unittest("pf_moments matches the oracle with nulls and NaNs excluded", &
                test_golden_exclusions), &
            new_unittest("pf_moments answers NaN, not an abort, on every degenerate population", &
                test_golden_degenerate), &
            new_unittest("the one-shot reductions equal pf_moments bit for bit", test_one_shot_agrees), &
            new_unittest("equal weights give exactly the unweighted answer", test_equal_weights), &
            new_unittest("the mean of a constant array is that constant, exactly", test_mean_of_constant), &
            new_unittest("ok is .true. exactly when the answer is not NaN", test_ok_tracks_the_answer), &
            new_unittest("an empty population sums to 0 while every other answer is NaN", &
                test_empty_sums_to_zero), &
            new_unittest("pf_stats agrees with the one-shot family bit for bit", &
                test_object_matches_one_shot), &
            new_unittest("%compute costs two traversals and every later query costs none", &
                test_compute_costs_two_scans), &
            new_unittest("a retained %update loop equals %compute over the concatenation", &
                test_retained_update_equals_compute), &
            new_unittest("%merge of two halves equals %compute over the whole", test_merge_equals_compute), &
            new_unittest("the array %merge folds in index order and recomputes once", &
                test_merge_many_recomputes_once), &
            new_unittest("a streaming update costs one traversal and keeps nothing", &
                test_streaming_costs_one_scan), &
            new_unittest("a streaming %merge agrees with %compute over the whole", &
                test_streaming_merge_agrees), &
            new_unittest("pf_stats applies the same null, NaN and weight policy", &
                test_object_carries_the_exclusion_policy), &
            new_unittest("an empty pf_stats is computed, and differs from an uncomputed one", &
                test_object_empty_and_cleared), &
            new_unittest("weights arriving part-way through a retained stream backfill as 1", &
                test_weights_arriving_late_backfill), &
            new_unittest("every numeric kind widens exactly to the real64 answer", &
                test_every_kind_agrees_with_real64), &
            new_unittest("the mean of a logical array is the fraction that are true", &
                test_logical_mean_is_a_fraction), &
            new_unittest("a parquet_column gives exactly the answer its array does", &
                test_column_matches_its_array), &
            new_unittest("all five numeric column kinds reach the generic", test_every_column_kind), &
            new_unittest("a real32 NaN is still a NaN after widening", &
                test_real32_nan_survives_widening), &
            new_unittest("pf_stats accepts every kind the one-shot family does", &
                test_object_accepts_every_kind), &
            new_unittest("a column with spare capacity is read only to its row count", &
                test_column_capacity_is_not_the_population), &
            new_unittest("the block tree is the documented one on both sides of its size threshold", &
                test_block_tree_is_the_documented_one), &
            new_unittest("threading the central-moment pass changes not one bit of any answer", &
                test_threading_changes_no_bit), &
            new_unittest("the work floor declines a team that would not pay, and lets one through", &
                test_the_work_floor_decides), &
            new_unittest("four order statistics off one accumulator cost ONE ordering", &
                test_order_cache_costs_one_sort), &
            new_unittest("%update drops the order cache, and the next median is recomputed", &
                test_update_drops_the_order_cache), &
            new_unittest("%merge drops the order cache too", test_merge_drops_the_order_cache), &
            new_unittest("%release_order frees tier B and the next query rebuilds it", &
                test_release_order_rebuilds), &
            new_unittest("every quantile method matches the 50-digit oracle", &
                test_quantile_methods_match_the_oracle), &
            new_unittest("equal weights reproduce the unweighted quantile exactly", &
                test_weighted_quantiles_reduce_to_unweighted), &
            new_unittest("the one-shot family and the accumulator agree", test_order_forms_agree), &
            new_unittest("the selection and ordering paths give identical quantiles", &
                test_selection_and_sort_paths_agree), &
            new_unittest("pf_trim_mean follows scipy's floor(prop*n) contract", &
                test_trim_mean_contract), &
            new_unittest("pf_percentile_of_score reproduces scipy's four conventions", &
                test_percentile_of_score_kinds), &
            new_unittest("an empty order-statistic population is NaN and ok=.false.", &
                test_order_degenerate_populations), &
            new_unittest("q(0) and q(1) are exactly the extremes, weighted or not", &
                test_extreme_quantiles_are_the_extremes), &
            new_unittest("weight_type=frequency defaults the quantile to inverted_cdf", &
                test_frequency_weights_default_to_inverted_cdf), &
            new_unittest("every pf_mad case matches the 50-digit oracle on both scales", &
                test_mad_matches_the_oracle), &
            new_unittest("pf_mad estimates a Gaussian sigma and survives contamination", &
                test_mad_estimates_the_gaussian_sigma), &
            new_unittest("%mad caches the raw deviation and rescaling it is free", &
                test_mad_cache_costs_one_build), &
            new_unittest("%update and %release_order both drop the deviation cache", &
                test_mutation_drops_the_deviation_cache), &
            new_unittest("the one-shot pf_mad and %mad agree over every kind", &
                test_mad_forms_agree), &
            new_unittest("pf_mode breaks a tie to the smallest value, whatever the input order", &
                test_mode_ties_go_to_the_smallest), &
            new_unittest("pf_mode over a character array and a packed string column", &
                test_mode_over_strings), &
            new_unittest("nulls and weights change pf_mode as they change every reduction", &
                test_mode_nulls_and_weights), &
            new_unittest("pf_describe costs one pair of passes and one ordering", &
                test_describe_costs_one_pass_and_one_sort), &
            new_unittest("%print writes the block, can be silenced, and never aborts", &
                test_print_writes_and_can_be_silenced), &
            new_unittest("skipnan=.false. propagates a NaN through every order statistic", &
                test_propagating_nan_reaches_every_tier), &
            new_unittest("every P8 fixture recipe still produces what the generator emitted", &
                test_p8_fixture_recipes), &
            new_unittest("pf_gmean and pf_hmean match the 50-digit oracle", &
                test_power_means_match_the_oracle), &
            new_unittest("a zero gives exactly zero and a negative gives NaN", &
                test_power_mean_domain), &
            new_unittest("pf_cov and pf_corr match the oracle and reproduce both identities", &
                test_two_sample_matches_the_oracle), &
            new_unittest("Spearman sees a monotone relationship Pearson cannot, and ties", &
                test_spearman_sees_monotone), &
            new_unittest("pf_zscore standardises and reports its exclusions", &
                test_zscore_standardises), &
            new_unittest("pf_sigma_clipped_stats reproduces astropy", &
                test_sigma_clip_matches_astropy), &
            new_unittest("a whole sigma-clipping run costs ONE ordering", &
                test_sigma_clip_costs_one_ordering), &
            new_unittest("the sigma clip's keep mask names exactly the survivors", &
                test_sigma_clip_keep_mask), &
            new_unittest("the sigma clip's empty, constant and two-element cases", &
                test_sigma_clip_degenerate), &
            new_unittest("every P8 procedure agrees across all six input kinds", &
                test_p8_kinds_agree), &
            new_unittest("the four properties P8's first mutation round found unasserted", &
                test_p8_mutation_gaps), &
            new_unittest("the cumulative family matches the 50-digit oracle EXACTLY", &
                test_cumulative_matches_the_oracle), &
            new_unittest("a null leaves the running value alone; a kept NaN poisons it", &
                test_cumulative_null_rule), &
            new_unittest("the last cumsum is not pf_sum, and the gap is the documented one", &
                test_cumsum_is_not_pf_sum), &
            new_unittest("pf_histogram matches np.histogram on the golden fixture", &
                test_histogram_matches_numpy), &
            new_unittest("the two edge conventions differ only ON an edge, and cover it once", &
                test_binning_conventions), &
            new_unittest("pf_histogram IS pf_bucketize tallied, on both conventions", &
                test_histogram_is_bucketize_tallied), &
            new_unittest("binning reports every element it could not place", &
                test_binning_counts_and_weights), &
            new_unittest("every P9 procedure agrees across all six input kinds", &
                test_p9_kinds_agree), &
            new_unittest("density= reproduces np.histogram(density=True) and integrates to 1", &
                test_histogram_density), &
            new_unittest("pf_bin_edges reproduces np.histogram_bin_edges, degenerate cases too", &
                test_bin_edges_matches_numpy), &
            new_unittest("pf_bin_edges always emits edges pf_histogram will accept", &
                test_bin_edges_are_always_usable) &
            ]
    end subroutine collect_tests_parquet_stats

    !> With no optional argument, every element is in the population.
    subroutine test_plain_count(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        real(real64) :: v(7)
        integer(int64) :: n
        integer :: i

        do i = 1, size(v)
            v(i) = real(i, real64)
        end do
        call pf_count_valid(v, n)
        call check(error, n == 7_int64, "pf_count_valid over a clean array must count every element")
    end subroutine test_plain_count

    !> Every numeric kind reaches the generic, and each answers for itself.
    !!
    !! A generic that resolved four of five kinds would still pass any test written for one of
    !! them, so all five are exercised rather than a representative.
    subroutine test_every_kind(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        integer(int64) :: n

        call pf_count_valid([1_int32, 2_int32, 3_int32], n)
        call check(error, n == 3_int64, "pf_count_valid must accept an int32 array")
        if (allocated(error)) return
        call pf_count_valid([1_int64, 2_int64, 3_int64, 4_int64], n)
        call check(error, n == 4_int64, "pf_count_valid must accept an int64 array")
        if (allocated(error)) return
        call pf_count_valid([1.0_real32, 2.0_real32], n)
        call check(error, n == 2_int64, "pf_count_valid must accept a real32 array")
        if (allocated(error)) return
        call pf_count_valid([1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64, 5.0_real64], n)
        call check(error, n == 5_int64, "pf_count_valid must accept a real64 array")
        if (allocated(error)) return
        call pf_count_valid([.true., .false., .true.], n)
        call check(error, n == 3_int64, &
            "pf_count_valid must accept a logical array and count .false. as an ordinary value")
    end subroutine test_every_kind

    !> `is_valid(i) == .false.` removes element `i`, and nothing else does.
    subroutine test_nulls_excluded(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        integer(int32) :: v(6)
        logical :: ok(6)
        integer(int64) :: n

        v = [10_int32, 20_int32, 30_int32, 40_int32, 50_int32, 60_int32]
        ok = [.true., .false., .true., .false., .false., .true.]
        call pf_count_valid(v, n, is_valid=ok)
        call check(error, n == 3_int64, "pf_count_valid must exclude every element is_valid marks null")
        if (allocated(error)) return
        call pf_count_valid(v, n, is_valid=[.false., .false., .false., .false., .false., .false.])
        call check(error, n == 0_int64, "an all-null population must count 0, not abort")
    end subroutine test_nulls_excluded

    !> A NaN leaves the population by default and stays in it under `skipnan=.false.`.
    !!
    !! **Both directions are asserted**, because the default is the interesting half and a
    !! one-sided test passes just as happily against an implementation that ignores the argument.
    subroutine test_nan_policy(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        real(real64) :: v(5)
        real(real32) :: v32(4)
        integer(int64) :: n

        v = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64, 5.0_real64]
        v(2) = ieee_value(1.0_real64, ieee_quiet_nan)
        v(4) = ieee_value(1.0_real64, ieee_quiet_nan)

        call pf_count_valid(v, n)
        call check(error, n == 3_int64, "a NaN must leave the population by default, as pf_minmax skips it")
        if (allocated(error)) return
        call pf_count_valid(v, n, skipnan=.true.)
        call check(error, n == 3_int64, "skipnan=.true. must be the default, not a different answer")
        if (allocated(error)) return
        call pf_count_valid(v, n, skipnan=.false.)
        call check(error, n == 5_int64, "skipnan=.false. must count a NaN as an ordinary value, as numpy does")
        if (allocated(error)) return

        v32 = [1.0_real32, 2.0_real32, 3.0_real32, 4.0_real32]
        v32(3) = ieee_value(1.0_real32, ieee_quiet_nan)
        call pf_count_valid(v32, n)
        call check(error, n == 3_int64, "the real32 specific must apply the same NaN rule as real64")
    end subroutine test_nan_policy

    !> Weight zero is how a caller says "drop this row".
    subroutine test_zero_weight_excluded(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        real(real64) :: v(5), w(5)
        integer(int64) :: n

        v = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64, 5.0_real64]
        w = [2.0_real64, 0.0_real64, 1.0_real64, 0.0_real64, 0.5_real64]
        call pf_count_valid(v, n, weights=w)
        call check(error, n == 3_int64, "a zero weight must remove the element from the population")
        if (allocated(error)) return
        call pf_count_valid(v, n)
        call check(error, n == 5_int64, &
            "the same array without weights must count every element -- the two answers differ legitimately")
        if (allocated(error)) return
        call pf_count_valid(v, n, weights=[0.0_real64, 0.0_real64, 0.0_real64, 0.0_real64, 0.0_real64])
        call check(error, n == 0_int64, "an all-zero weight array is the empty population, not an abort")
    end subroutine test_zero_weight_excluded

    !> The exclusion ORDER, which is observable in exactly one place.
    !!
    !! A null element's weight must never be examined, so a NaN or infinite weight sitting on a null
    !! element must not abort. Reverse the order -- validate weights first -- and every other test in
    !! this suite still passes while an ordinary program dies: a weight column computed as
    !! `1/err**2` is routinely NaN or infinite exactly where the value column is null.
    subroutine test_null_element_ignores_its_weight(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        real(real64) :: v(4), w(4)
        integer(int64) :: n

        v = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64]
        w = [1.0_real64, 1.0_real64, 1.0_real64, 1.0_real64]
        w(2) = ieee_value(1.0_real64, ieee_quiet_nan)          ! NaN weight ...
        w(3) = -5.0_real64                                     ! ... and a negative one ...
        call pf_count_valid(v, n, is_valid=[.true., .false., .false., .true.], weights=w)
        call check(error, n == 2_int64, &                      ! ... both on NULL elements: no abort.
            "a weight belonging to a null element must never be validated, let alone abort")
        if (allocated(error)) return

        ! The same for a NaN VALUE: it leaves the population before its weight is looked at.
        v(3) = ieee_value(1.0_real64, ieee_quiet_nan)
        call pf_count_valid(v, n, is_valid=[.true., .false., .true., .true.], weights=w)
        call check(error, n == 2_int64, &
            "a weight belonging to a NaN value must not be validated either -- NaN is excluded first")
    end subroutine test_null_element_ignores_its_weight

    !> A population with nothing in it counts 0. It must never abort: a group loop meets this.
    subroutine test_empty_population_is_not_fatal(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        real(real64), allocatable :: empty(:)
        real(real64) :: v(3)
        integer(int64) :: n

        allocate(empty(0))
        call pf_count_valid(empty, n)
        call check(error, n == 0_int64, "a zero-length array must count 0")
        if (allocated(error)) return

        v = [ieee_value(1.0_real64, ieee_quiet_nan), ieee_value(1.0_real64, ieee_quiet_nan), &
             ieee_value(1.0_real64, ieee_quiet_nan)]
        call pf_count_valid(v, n)
        call check(error, n == 0_int64, "an all-NaN array must count 0 rather than abort")
    end subroutine test_empty_population_is_not_fatal

    ! ==================================================================================
    ! Phase P2: the real64 moment core, against the 50-digit oracle
    !
    ! The expectations live in test_stats_golden, emitted by tools/generate_stats_vectors.py from
    ! an `mpmath` model that shares no arithmetic with the library and none with numpy either.
    ! `--check` in the lint stage is what stops them drifting into a record of whatever the library
    ! did on the day someone regenerated them.
    ! ==================================================================================

    !> The population every non-degenerate golden case is taken over.
    !!
    !! An integer sequence divided by a power of two, so every value is exactly representable and
    !! the generator's Python reproduces it bit for bit -- which is what lets the fixture travel as
    !! a recipe rather than as a thousand-element literal table. Change it here and
    !! `test_fixture_recipe` reports the mismatch against `G_PROBE`.
    subroutine golden_fixture(n, x)
        integer(int64), intent(in) :: n                          !! how many values to build.
        real(real64), allocatable, intent(out) :: x(:)           !! the population.
        integer(int64) :: i, a

        allocate(x(n))
        do i = 1_int64, n
            a = mod(i * i * 7919_int64 + 12345_int64, 1000003_int64)
            x(i) = real(a - 500001_int64, real64) / 1024.0_real64
        end do
    end subroutine golden_fixture

    !> `w(i) = mod(i, 5)`, so every fifth weight is ZERO and removes its element.
    subroutine golden_weights_mod5(n, w)
        integer(int64), intent(in) :: n                          !! how many weights to build.
        real(real64), allocatable, intent(out) :: w(:)           !! the weights.
        integer(int64) :: i

        allocate(w(n))
        do i = 1_int64, n
            w(i) = real(mod(i, 5_int64), real64)
        end do
    end subroutine golden_weights_mod5

    !> Compares one `pf_moments` result against its golden row, quantity by quantity.
    !!
    !! A quantity the oracle marks undefined is asserted to be a quiet NaN rather than compared
    !! against a number -- which is the half that would otherwise pass against a library returning
    !! zero for an empty group.
    subroutine check_case(error, label, got, gold, golddef, tol)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        character(len=*), intent(in) :: label               !! the case name, for the message.
        real(real64), intent(in) :: got(NQ)                 !! what the library answered.
        real(real64), intent(in) :: gold(NQ)                !! what the oracle says.
        logical, intent(in) :: golddef(NQ)                  !! which quantities are defined.
        real(real64), intent(in) :: tol                     !! relative tolerance.
        integer :: q
        character(len=220) :: msg

        do q = 1, NQ
            if (golddef(q)) then
                write(msg, '(a)') label // ": " // trim(QNAME(q)) // " came back NaN but is defined"
                call check(error, got(q) == got(q), trim(msg))
                if (allocated(error)) return
                write(msg, '(a,es24.17,a,es24.17)') label // ": " // trim(QNAME(q)) // " is ", &
                    got(q), " but the oracle says ", gold(q)
                call check(error, abs(got(q) - gold(q)) <= tol * max(1.0_real64, abs(gold(q))), trim(msg))
            else
                write(msg, '(a,es24.17)') label // ": " // trim(QNAME(q)) // &
                    " is undefined for this population and must be NaN, not ", got(q)
                call check(error, got(q) /= got(q), trim(msg))
            end if
            if (allocated(error)) return
        end do
    end subroutine check_case

    !> Runs one case through `pf_moments` and compares it with its golden row.
    subroutine run_case(error, label, gold, golddef, goldn, values, is_valid, weights, &
            weight_type, ddof, bias, excess, skipnan, tol)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        character(len=*), intent(in) :: label               !! the case name, for the message.
        real(real64), intent(in) :: gold(NQ)                !! the oracle's values.
        logical, intent(in) :: golddef(NQ)                  !! which of them are defined.
        integer(int64), intent(in) :: goldn(3)              !! [n_valid, n_null, n_nan].
        real(real64), intent(in) :: values(:)               !! the population.
        logical, intent(in), optional :: is_valid(:)        !! per element: .false. marks a null.
        real(real64), intent(in), optional :: weights(:)    !! per element weight.
        character(len=*), intent(in), optional :: weight_type !! "reliability" or "frequency".
        integer, intent(in), optional :: ddof               !! delta degrees of freedom.
        logical, intent(in), optional :: bias               !! .true. leaves the moments uncorrected.
        logical, intent(in), optional :: excess             !! .false. adds 3 back to the kurtosis.
        logical, intent(in), optional :: skipnan            !! .false. lets a NaN propagate.
        real(real64), intent(in) :: tol                     !! relative tolerance.
        real(real64) :: got(NQ)
        integer(int64) :: nv, nnull, nnan
        character(len=160) :: msg

        call pf_moments(values, n_valid=nv, mean=got(2), variance=got(3), stddev=got(4), &
            sem=got(5), skewness=got(6), kurtosis=got(7), vsum=got(1), vmin=got(8), vmax=got(9), &
            is_valid=is_valid, weights=weights, weight_type=weight_type, ddof=ddof, bias=bias, &
            excess=excess, skipnan=skipnan, n_null=nnull, n_nan=nnan)

        write(msg, '(a,i0,a,i0)') label // ": n_valid is ", nv, " but should be ", goldn(1)
        call check(error, nv == goldn(1), trim(msg))
        if (allocated(error)) return
        write(msg, '(a,i0,a,i0)') label // ": n_null is ", nnull, " but should be ", goldn(2)
        call check(error, nnull == goldn(2), trim(msg))
        if (allocated(error)) return
        write(msg, '(a,i0,a,i0)') label // ": n_nan is ", nnan, " but should be ", goldn(3)
        call check(error, nnan == goldn(3), trim(msg))
        if (allocated(error)) return

        call check_case(error, label, got, gold, golddef, tol)
    end subroutine run_case

    !> The recipe here and the recipe in the generator must be the same recipe.
    !!
    !! Without this, a drifted fixture reports as a tolerance failure in every case at once, which
    !! looks like an accuracy defect in the library rather than a mismatch in the test data.
    subroutine test_fixture_recipe(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        real(real64), allocatable :: x(:)
        integer :: i
        character(len=160) :: msg

        call golden_fixture(int(size(G_PROBE), int64), x)
        do i = 1, size(G_PROBE)
            write(msg, '(a,i0,a,es24.17,a,es24.17)') "golden_fixture element ", i, " is ", x(i), &
                " but the generator emitted ", G_PROBE(i)
            call check(error, x(i) == G_PROBE(i), trim(msg))
            if (allocated(error)) return
        end do
    end subroutine test_fixture_recipe

    !> The unweighted cases: the default configuration, both `ddof` ends, and both `bias` spellings.
    subroutine test_golden_unweighted(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        real(real64), allocatable :: x(:), big(:)
        real(real64), parameter :: TOL = 1.0e-13_real64

        call golden_fixture(32_int64, x)
        call run_case(error, "U32", G_U32, G_U32_DEF, G_U32_N, x, tol=TOL)
        if (allocated(error)) return
        call run_case(error, "U32_D0", G_U32_D0, G_U32_D0_DEF, G_U32_D0_N, x, ddof=0, tol=TOL)
        if (allocated(error)) return
        ! ddof beyond the population is an ordinary data condition -- NaN, never a division by zero
        ! and never an abort. "this group has one member" reaches it constantly.
        call run_case(error, "U32_D99", G_U32_D99, G_U32_D99_DEF, G_U32_D99_N, x, ddof=99, tol=TOL)
        if (allocated(error)) return
        call run_case(error, "U32_BIAS", G_U32_BIAS, G_U32_BIAS_DEF, G_U32_BIAS_N, x, bias=.true., tol=TOL)
        if (allocated(error)) return
        call run_case(error, "U32_RAWK", G_U32_RAWK, G_U32_RAWK_DEF, G_U32_RAWK_N, x, excess=.false., tol=TOL)
        if (allocated(error)) return

        call golden_fixture(1000_int64, big)
        call run_case(error, "U1000", G_U1000, G_U1000_DEF, G_U1000_N, big, tol=TOL)
    end subroutine test_golden_unweighted

    !> The single best regression test in the module: the same population, offset by 1e9.
    !!
    !! The offset is EXACT -- the values are multiples of 2**-10 and `ulp(1e9)` is 2**-23 -- so the
    !! variance of the shifted population is mathematically identical to the variance of the
    !! original, and the two library answers may be compared directly rather than through the
    !! oracle. The textbook `sum(x**2) - sum(x)**2/n` misses this by orders of magnitude; two-pass
    !! misses it by a few ulp, because the derivative of the central moment with respect to the
    !! mean is zero at the mean.
    subroutine test_shift_invariance(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        real(real64), allocatable :: x(:), shifted(:)
        ! NOT `g_shift`: Fortran is case-insensitive, so that local would shadow the golden
        ! parameter `G_SHIFT` and turn it into a scalar at the `run_case` call above.
        real(real64) :: v_plain, v_shift, sd_plain, sd_shift, sk_plain, sk_shift
        character(len=200) :: msg
        integer(int64) :: i

        call golden_fixture(1000_int64, x)
        allocate(shifted(size(x)))
        do i = 1_int64, size(x, kind=int64)
            shifted(i) = x(i) + 1.0e9_real64
        end do

        call run_case(error, "SHIFT", G_SHIFT, G_SHIFT_DEF, G_SHIFT_N, shifted, tol=1.0e-14_real64)
        if (allocated(error)) return

        call pf_variance(x, v_plain)
        call pf_variance(shifted, v_shift)
        call pf_stddev(x, sd_plain)
        call pf_stddev(shifted, sd_shift)
        call pf_skewness(x, sk_plain)
        call pf_skewness(shifted, sk_shift)
        write(msg, '(a,es24.17,a,es24.17)') "variance is not shift-invariant: plain ", v_plain, &
            " vs shifted ", v_shift
        call check(error, abs(v_shift - v_plain) <= 1.0e-13_real64 * v_plain, trim(msg))
        if (allocated(error)) return
        write(msg, '(a,es24.17,a,es24.17)') "stddev is not shift-invariant: plain ", sd_plain, &
            " vs shifted ", sd_shift
        call check(error, abs(sd_shift - sd_plain) <= 1.0e-13_real64 * sd_plain, trim(msg))
        if (allocated(error)) return

        ! The SKEWNESS is the sharp end of this, and the reason `stats_engine` carries the
        ! `sum(w*d)` correction at all. The second moment is immune to a perturbed mean because its
        ! derivative there vanishes; the third picks the error up linearly through `3*delta*m2`,
        ! and without the correction this comparison was wrong in the eighth significant digit
        ! while the variance above was still right in the fifteenth.
        write(msg, '(a,es24.17,a,es24.17)') "skewness is not shift-invariant: plain ", sk_plain, &
            " vs shifted ", sk_shift
        call check(error, abs(sk_shift - sk_plain) <= 1.0e-12_real64 * abs(sk_plain), trim(msg))
    end subroutine test_shift_invariance

    !> The two `weight_type` conventions, which separate only when the weights are unequal.
    subroutine test_golden_weighted(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        real(real64), allocatable :: x(:), w(:)
        real(real64) :: three(32)
        real(real64), parameter :: TOL = 1.0e-13_real64

        call golden_fixture(32_int64, x)
        three = 3.0_real64
        call run_case(error, "W3", G_W3, G_W3_DEF, G_W3_N, x, weights=three, tol=TOL)
        if (allocated(error)) return
        call run_case(error, "W3F", G_W3F, G_W3F_DEF, G_W3F_N, x, weights=three, &
            weight_type="frequency", tol=TOL)
        if (allocated(error)) return

        ! mod(i,5) puts a ZERO weight on every fifth element, so n_valid is 26 rather than 32 --
        ! a zero weight removes the element exactly as a null does.
        call golden_weights_mod5(32_int64, w)
        call run_case(error, "WVAR", G_WVAR, G_WVAR_DEF, G_WVAR_N, x, weights=w, tol=TOL)
        if (allocated(error)) return
        call run_case(error, "WVARF", G_WVARF, G_WVARF_DEF, G_WVARF_N, x, weights=w, &
            weight_type="frequency", tol=TOL)
    end subroutine test_golden_weighted

    !> Nulls and NaNs, including the case that pins the exclusion ORDER.
    subroutine test_golden_exclusions(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        real(real64), allocatable :: x(:), nanx(:), nanw(:)
        logical :: mask(32)
        real(real64), parameter :: TOL = 1.0e-13_real64
        integer(int64) :: i

        call golden_fixture(32_int64, x)
        do i = 1_int64, 32_int64
            mask(i) = mod(i, 3_int64) /= 0_int64
        end do
        call run_case(error, "NULLS", G_NULLS, G_NULLS_DEF, G_NULLS_N, x, is_valid=mask, tol=TOL)
        if (allocated(error)) return

        ! Every tenth value is NaN AND carries a NaN weight. Under the family's exclusion order the
        ! NaN value is out of the population before its weight is looked at, so this must compute
        ! cleanly; reverse the order and it aborts on the weight instead.
        allocate(nanx(32), nanw(32))
        do i = 1_int64, 32_int64
            if (mod(i, 10_int64) == 0_int64) then
                nanx(i) = ieee_value(1.0_real64, ieee_quiet_nan)
                nanw(i) = ieee_value(1.0_real64, ieee_quiet_nan)
            else
                nanx(i) = x(i)
                nanw(i) = 1.0_real64
            end if
        end do
        call run_case(error, "NANS", G_NANS, G_NANS_DEF, G_NANS_N, nanx, weights=nanw, tol=TOL)
        if (allocated(error)) return

        ! skipnan=.false. is numpy's propagating behaviour: the NaN stays in the population, so
        ! every answer is NaN -- including min and max, which are not IEEE minNum/maxNum here.
        call run_case(error, "NANS_KEEP", G_NANS_KEEP, G_NANS_KEEP_DEF, G_NANS_KEEP_N, nanx, &
            skipnan=.false., tol=TOL)
    end subroutine test_golden_exclusions

    !> Every degenerate population a per-group loop meets on real data. None of them may abort.
    subroutine test_golden_degenerate(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        real(real64) :: constv(17), one(1)
        real(real64), allocatable :: nothing(:), x8(:), zerow(:)
        logical :: allnull(8)
        real(real64), parameter :: TOL = 1.0e-13_real64

        constv = 2.5_real64
        call run_case(error, "CONST", G_CONST, G_CONST_DEF, G_CONST_N, constv, tol=TOL)
        if (allocated(error)) return

        one = [3.75_real64]
        call run_case(error, "SINGLE", G_SINGLE, G_SINGLE_DEF, G_SINGLE_N, one, tol=TOL)
        if (allocated(error)) return
        call run_case(error, "SINGLE_D0", G_SINGLE_D0, G_SINGLE_D0_DEF, G_SINGLE_D0_N, one, &
            ddof=0, tol=TOL)
        if (allocated(error)) return

        allocate(nothing(0))
        call run_case(error, "EMPTY", G_EMPTY, G_EMPTY_DEF, G_EMPTY_N, nothing, tol=TOL)
        if (allocated(error)) return

        call golden_fixture(8_int64, x8)
        allnull = .false.
        call run_case(error, "ALLNULL", G_ALLNULL, G_ALLNULL_DEF, G_ALLNULL_N, x8, &
            is_valid=allnull, tol=TOL)
        if (allocated(error)) return

        ! numpy RAISES on an all-zero weight array; this module treats it as the empty case, which
        ! is the only answer consistent with "a zero weight removes the element".
        allocate(zerow(8))
        zerow = 0.0_real64
        call run_case(error, "ALLZEROW", G_ALLZEROW, G_ALLZEROW_DEF, G_ALLZEROW_N, x8, &
            weights=zerow, tol=TOL)
    end subroutine test_golden_degenerate

    !> Every one-shot reduction must be the corresponding `pf_moments` output, bit for bit.
    !!
    !! Compared with `==`, not a tolerance: they go through the same engine, so anything else means
    !! one of them has grown a second code path -- which is how two reductions in one program come
    !! to disagree about the same array.
    subroutine test_one_shot_agrees(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        real(real64), allocatable :: x(:), w(:)
        real(real64) :: mm(NQ), s, m, v, sd, se, g, k

        call golden_fixture(500_int64, x)
        call golden_weights_mod5(500_int64, w)
        call pf_moments(x, mean=mm(2), variance=mm(3), stddev=mm(4), sem=mm(5), skewness=mm(6), &
            kurtosis=mm(7), vsum=mm(1), weights=w)
        call pf_sum(x, s, weights=w)
        call pf_mean(x, m, weights=w)
        call pf_variance(x, v, weights=w)
        call pf_stddev(x, sd, weights=w)
        call pf_sem(x, se, weights=w)
        call pf_skewness(x, g, weights=w)
        call pf_kurtosis(x, k, weights=w)

        call check(error, s == mm(1), "pf_sum disagrees with pf_moments' vsum")
        if (allocated(error)) return
        call check(error, m == mm(2), "pf_mean disagrees with pf_moments' mean")
        if (allocated(error)) return
        call check(error, v == mm(3), "pf_variance disagrees with pf_moments' variance")
        if (allocated(error)) return
        call check(error, sd == mm(4), "pf_stddev disagrees with pf_moments' stddev")
        if (allocated(error)) return
        call check(error, se == mm(5), "pf_sem disagrees with pf_moments' sem")
        if (allocated(error)) return
        call check(error, g == mm(6), "pf_skewness disagrees with pf_moments' skewness")
        if (allocated(error)) return
        call check(error, k == mm(7), "pf_kurtosis disagrees with pf_moments' kurtosis")
    end subroutine test_one_shot_agrees

    !> Equal weights must give EXACTLY the unweighted answer, under the default convention.
    !!
    !! Exactly, not nearly: with every weight equal the weighted formulas reduce to the unweighted
    !! ones algebraically, and any difference means a weight is being folded in somewhere it should
    !! have cancelled. The frequency convention is asserted to DIFFER, which is the negative
    !! control -- without it this test passes against an implementation ignoring `weight_type`.
    subroutine test_equal_weights(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        real(real64), allocatable :: x(:)
        real(real64) :: w(64), v_plain, v_w, v_freq, g_plain, g_w

        call golden_fixture(64_int64, x)
        w = 3.0_real64
        call pf_variance(x, v_plain)
        call pf_variance(x, v_w, weights=w)
        call pf_variance(x, v_freq, weights=w, weight_type="frequency")
        call pf_skewness(x, g_plain)
        call pf_skewness(x, g_w, weights=w)

        ! Agreement to a few ulp rather than bit-for-bit, and the reason is worth stating: the
        ! weighted path forms `w*x` and `sum(w)` where the unweighted one forms `x` and `n`, so
        ! every product rounds once more even though the algebra cancels the weight out exactly.
        ! A bit-for-bit assertion here would be asserting an accident of the multiplier.
        call check(error, abs(v_w - v_plain) <= 1.0e-14_real64 * abs(v_plain), &
            "reliability weights all equal must give the unweighted variance")
        if (allocated(error)) return
        call check(error, abs(g_w - g_plain) <= 1.0e-14_real64 * abs(g_plain), &
            "reliability weights all equal must give the unweighted skewness")
        if (allocated(error)) return
        call check(error, v_freq /= v_plain, &
            "frequency weights of 3 must NOT give the unweighted variance -- ddof is charged against sum(w)")
    end subroutine test_equal_weights

    !> The mean of a constant array is that constant, and its variance is exactly zero.
    !!
    !! **Exactly, but only for a constant the format can hold.** With `2.5` every partial sum is
    !! exact and the division is exact, so `==` is the right assertion and it is a real one: it
    !! fails the moment the mean acquires a spurious correction term. With `0.1` the summation
    !! itself rounds -- 0.1 is not a binary fraction, and the in-block sum is sequential -- so the
    !! same property holds only to a few ulp, and asserting `==` there would be asserting an
    !! accident of the summation order rather than anything about the mean.
    subroutine test_mean_of_constant(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        real(real64) :: c(300), m, v

        c = 2.5_real64
        call pf_mean(c, m)
        call check(error, m == 2.5_real64, "the mean of a constant array must be that constant exactly")
        if (allocated(error)) return
        call pf_variance(c, v)
        call check(error, v == 0.0_real64, "the variance of a constant array must be exactly 0")
        if (allocated(error)) return

        c = 0.1_real64
        call pf_mean(c, m)
        call check(error, abs(m - 0.1_real64) <= 4.0_real64 * spacing(0.1_real64), &
            "the mean of a non-representable constant must still be that constant to a few ulp")
        if (allocated(error)) return
        call pf_variance(c, v)
        call check(error, v <= 1.0e-30_real64, "the variance of a constant array must be negligible")
    end subroutine test_mean_of_constant

    !> `ok` is set from the answer, so the two can never disagree.
    subroutine test_ok_tracks_the_answer(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        real(real64) :: one(1), v, s
        real(real64), allocatable :: nothing(:)
        logical :: ok

        one = [3.75_real64]
        call pf_variance(one, v, ok=ok)
        call check(error, .not. ok, "a one-element population has no ddof=1 variance, so ok must be .false.")
        if (allocated(error)) return
        call check(error, v /= v, "the value beside ok=.false. must be a quiet NaN")
        if (allocated(error)) return

        call pf_variance(one, v, ddof=0, ok=ok)
        call check(error, ok, "at ddof=0 a one-element population does have a variance")
        if (allocated(error)) return
        call check(error, v == 0.0_real64, "the ddof=0 variance of one element is exactly 0")
        if (allocated(error)) return

        allocate(nothing(0))
        call pf_sum(nothing, s, ok=ok)
        call check(error, ok, "an empty sum is defined, so ok must be .true.")
    end subroutine test_ok_tracks_the_answer

    !> The one quantity an empty population still defines, and the reason it is the only one.
    subroutine test_empty_sums_to_zero(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        real(real64), allocatable :: nothing(:)
        real(real64) :: s, m
        integer(int64) :: nv

        allocate(nothing(0))
        call pf_moments(nothing, n_valid=nv, vsum=s, mean=m)
        call check(error, s == 0.0_real64, "an empty population sums to exactly 0, as numpy and pandas do")
        if (allocated(error)) return
        call check(error, m /= m, "an empty population has no mean, so it must come back NaN")
        if (allocated(error)) return
        call check(error, nv == 0_int64, "an empty population has n_valid 0")
    end subroutine test_empty_sums_to_zero

    ! ==============================================================================================
    ! pf_stats
    !
    ! Two things are being asserted here and they are easy to conflate. One is that the OBJECT
    ! agrees with the one-shot procedures, which the golden vectors already pin -- so those tests
    ! compare with `==` rather than a tolerance, because anything less would pass against an object
    ! that quietly reached a different engine. The other is the WORK the object promises to avoid,
    ! which no comparison of answers can see: `parquet_debug_stats_scans()` is the only observable
    ! for it, and every test that reads it also shows the counter moving, so a hook that had stopped
    ! counting could not pass for a query that had stopped traversing.
    ! ==============================================================================================

    !> Fills `got` with every quantity `check_case` compares, read off a `pf_stats`.
    subroutine object_row(s, got)
        type(pf_stats), intent(inout) :: s   !! the accumulator to read.
        real(real64), intent(out) :: got(NQ) !! [sum, mean, var, stddev, sem, skew, kurt, min, max].
        got(1) = s%sum()
        got(2) = s%mean()
        got(3) = s%variance()
        got(4) = s%stddev()
        got(5) = s%sem()
        got(6) = s%skewness()
        got(7) = s%kurtosis()
        got(8) = s%vmin()
        got(9) = s%vmax()
    end subroutine object_row

    !> `%compute` and the one-shot family are the same engine, so they must agree EXACTLY.
    subroutine test_object_matches_one_shot(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        real(real64), allocatable :: v(:)
        real(real64) :: got(NQ), want(NQ)
        type(pf_stats) :: s
        integer :: q

        call golden_fixture(1000_int64, v)
        call s%compute(v)
        call object_row(s, got)
        call pf_sum(v, want(1))
        call pf_mean(v, want(2))
        call pf_variance(v, want(3))
        call pf_stddev(v, want(4))
        call pf_sem(v, want(5))
        call pf_skewness(v, want(6))
        call pf_kurtosis(v, want(7))
        call pf_moments(v, vmin=want(8), vmax=want(9))
        do q = 1, NQ
            call check(error, got(q) == want(q), &
                "pf_stats%" // trim(QNAME(q)) // " must equal the one-shot form bit for bit")
            if (allocated(error)) return
        end do
        call check(error, s%n() == 1000_int64 .and. s%n_valid() == 1000_int64, &
            "a clean population must report every element as seen and valid")
    end subroutine test_object_matches_one_shot

    !> The whole point of the type: two traversals, however many statistics are asked for.
    subroutine test_compute_costs_two_scans(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        real(real64), allocatable :: v(:)
        real(real64) :: junk
        type(pf_stats) :: s
        integer(int64) :: after_compute

        call golden_fixture(1000_int64, v)
        call parquet_debug_reset_stats_scans()
        call check(error, parquet_debug_stats_scans() == 0_int64, &
            "the traversal counter must read 0 after a reset")
        if (allocated(error)) return

        call s%compute(v)
        after_compute = parquet_debug_stats_scans()
        call check(error, after_compute == 2_int64, "%compute must cost exactly two traversals")
        if (allocated(error)) return

        ! Five different tier-A queries, none of which may traverse anything.
        junk = s%mean()
        junk = s%variance()
        junk = s%skewness()
        junk = s%kurtosis()
        junk = s%vmax()
        call check(error, parquet_debug_stats_scans() == after_compute, &
            "five tier-A queries after %compute must cost no further traversal")
        if (allocated(error)) return

        ! The negative control for the counter itself: it has to be capable of moving, or the
        ! assertion above would hold just as well against a hook that had stopped counting.
        call s%compute(v)
        call check(error, parquet_debug_stats_scans() == 4_int64, &
            "a second %compute must take the traversal count to four")
    end subroutine test_compute_costs_two_scans

    !> A retained `%update` loop is EXACTLY `%compute` over the concatenation, and defers its work.
    subroutine test_retained_update_equals_compute(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        real(real64), allocatable :: v(:)
        real(real64) :: got(NQ), want(NQ)
        type(pf_stats) :: s, whole
        integer :: q
        integer(int64) :: after_updates

        call golden_fixture(1000_int64, v)
        call whole%compute(v)
        call object_row(whole, want)

        call parquet_debug_reset_stats_scans()
        call s%init()
        call s%update(v(1:300))
        call s%update(v(301:700))
        call s%update(v(701:1000))
        after_updates = parquet_debug_stats_scans()
        call check(error, after_updates == 3_int64, &
            "three retained updates must cost one traversal each and defer the recomputation")
        if (allocated(error)) return

        call object_row(s, got)
        call check(error, parquet_debug_stats_scans() == 5_int64, &
            "the first query after a retained update loop must cost ONE recomputation, not three")
        if (allocated(error)) return
        do q = 1, NQ
            call check(error, got(q) == want(q), "a retained %update loop must equal %compute " // &
                "over the concatenation, bit for bit: " // trim(QNAME(q)))
            if (allocated(error)) return
        end do
        call check(error, s%n() == 1000_int64, "the update loop must have seen every element")
    end subroutine test_retained_update_equals_compute

    !> `%merge` of two halves is `%compute` over the whole, exactly, in retained mode.
    subroutine test_merge_equals_compute(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        real(real64), allocatable :: v(:)
        real(real64) :: got(NQ), want(NQ)
        type(pf_stats) :: lo, hi, whole
        integer :: q

        call golden_fixture(1000_int64, v)
        call whole%compute(v)
        call object_row(whole, want)

        call lo%compute(v(1:400))
        call hi%compute(v(401:1000))
        call lo%merge(hi)
        call object_row(lo, got)
        do q = 1, NQ
            call check(error, got(q) == want(q), &
                "a retained %merge must equal %compute over the concatenation: " // trim(QNAME(q)))
            if (allocated(error)) return
        end do
        call check(error, lo%n_valid() == 1000_int64, "the merged population must hold every element")
        if (allocated(error)) return
        call check(error, hi%n_valid() == 600_int64, &
            "a source merged without consume= must be left untouched")
    end subroutine test_merge_equals_compute

    !> The array form folds in INDEX order and recomputes once, which is what threading needs.
    subroutine test_merge_many_recomputes_once(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        real(real64), allocatable :: v(:)
        real(real64) :: got(NQ), want(NQ)
        type(pf_stats) :: part(4), total, whole
        integer :: q, k
        integer(int64) :: lo, hi, after_merge

        call golden_fixture(1000_int64, v)
        call whole%compute(v)
        call object_row(whole, want)

        do k = 1, 4
            lo = (k - 1) * 250_int64 + 1_int64
            hi = k * 250_int64
            call part(k)%compute(v(lo:hi))
        end do
        call total%init()
        call parquet_debug_reset_stats_scans()
        call total%merge(part, consume=.true.)
        after_merge = parquet_debug_stats_scans()
        call check(error, after_merge == 0_int64, "%merge itself must traverse nothing")
        if (allocated(error)) return

        call object_row(total, got)
        call check(error, parquet_debug_stats_scans() == 2_int64, &
            "folding four partials must cost ONE recomputation, not four")
        if (allocated(error)) return
        do q = 1, NQ
            call check(error, got(q) == want(q), &
                "the array %merge must equal %compute over the whole: " // trim(QNAME(q)))
            if (allocated(error)) return
        end do
        call check(error, .not. part(1)%is_computed() .and. .not. part(4)%is_computed(), &
            "consume=.true. must clear every source it folded in")
    end subroutine test_merge_many_recomputes_once

    !> Streaming is one traversal per batch, O(1) memory, and close rather than exact.
    subroutine test_streaming_costs_one_scan(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        real(real64), allocatable :: v(:)
        real(real64) :: got(NQ), want(NQ)
        type(pf_stats) :: s, whole
        integer :: q, k
        integer(int64) :: lo, hi

        call golden_fixture(1000_int64, v)
        call whole%compute(v)
        call object_row(whole, want)

        call parquet_debug_reset_stats_scans()
        call s%init(retain=.false.)
        do k = 1, 5
            lo = (k - 1) * 200_int64 + 1_int64
            hi = k * 200_int64
            call s%update(v(lo:hi))
        end do
        call check(error, parquet_debug_stats_scans() == 5_int64, &
            "five streaming updates must cost exactly one traversal each")
        if (allocated(error)) return

        call object_row(s, got)
        call check(error, parquet_debug_stats_scans() == 5_int64, &
            "a streaming accumulator has no buffer to re-walk, so a query must traverse nothing")
        if (allocated(error)) return
        call check(error, .not. s%retains(), "%init(retain=.false.) must report retains() = .false.")
        if (allocated(error)) return

        ! The combination formulas are accurate, not exact: comparing with `==` here would be
        ! asserting something this lifecycle deliberately does not promise.
        do q = 1, NQ
            call check(error, abs(got(q) - want(q)) <= 1.0e-12_real64 * max(1.0_real64, abs(want(q))), &
                "streaming must agree with %compute to the formulas' own accuracy: " // trim(QNAME(q)))
            if (allocated(error)) return
        end do
        call check(error, s%n_valid() == 1000_int64, "the streamed population must count every element")
    end subroutine test_streaming_costs_one_scan

    !> A streaming `%merge` is the same Chan fold as a streaming `%update`, so it agrees too.
    subroutine test_streaming_merge_agrees(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        real(real64), allocatable :: v(:)
        real(real64) :: got(NQ), want(NQ)
        type(pf_stats) :: lo, hi, whole
        integer :: q

        call golden_fixture(1000_int64, v)
        call whole%compute(v)
        call object_row(whole, want)

        call lo%init(retain=.false.)
        call hi%init(retain=.false.)
        call lo%update(v(1:400))
        call hi%update(v(401:1000))
        call lo%merge(hi)
        call object_row(lo, got)
        do q = 1, NQ
            call check(error, abs(got(q) - want(q)) <= 1.0e-12_real64 * max(1.0_real64, abs(want(q))), &
                "a streaming %merge must agree with %compute over the whole: " // trim(QNAME(q)))
            if (allocated(error)) return
        end do
        call check(error, lo%n_valid() == 1000_int64, "the merged stream must count every element")
    end subroutine test_streaming_merge_agrees

    !> Weights, nulls and NaNs reach the object exactly as they reach the one-shot family.
    subroutine test_object_carries_the_exclusion_policy(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        real(real64), allocatable :: v(:), w(:)
        real(real64) :: got(NQ)
        type(pf_stats) :: s
        logical, allocatable :: mask(:)
        integer(int64) :: i

        ! WVAR is the oracle's unequal-weight case: 32 elements, weights mod 5, so every fifth is
        ! zero and leaves the population. Going through the object must reach the same numbers.
        call golden_fixture(32_int64, v)
        call golden_weights_mod5(32_int64, w)
        call s%compute(v, weights=w)
        call object_row(s, got)
        call check_case(error, "WVAR via pf_stats", got, G_WVAR, G_WVAR_DEF, 1.0e-13_real64)
        if (allocated(error)) return
        call check(error, s%n_valid() == G_WVAR_N(1), "the object must exclude the zero-weighted rows")
        if (allocated(error)) return

        ! Nulls and NaNs, counted rather than merely removed, and carried across an update loop.
        deallocate(v)
        call golden_fixture(1000_int64, v)
        allocate(mask(1000))
        mask = .true.
        do i = 1_int64, 1000_int64, 7_int64
            mask(i) = .false.
        end do
        mask(3) = .false.   ! deliberately null AND NaN; see the n_nan assertion below
        v(3) = ieee_value(1.0_real64, ieee_quiet_nan)
        v(500) = ieee_value(1.0_real64, ieee_quiet_nan)
        call s%init()
        call s%update(v(1:500), is_valid=mask(1:500))
        call s%update(v(501:1000), is_valid=mask(501:1000))
        call check(error, s%n() == 1000_int64, "%n must count every element offered")
        if (allocated(error)) return
        call check(error, s%n_null() == count(.not. mask, kind=int64), &
            "%n_null must carry the nulls across an update loop")
        if (allocated(error)) return
        ! Element 3 is null as well as NaN, and nullness is examined first, so only one of the two
        ! NaNs is ever counted as one. That ordering is the whole content of this assertion.
        call check(error, s%n_nan() == 1_int64, &
            "a null element must be counted as null and never reach the NaN test")
        if (allocated(error)) return
        call check(error, s%n_valid() == 1000_int64 - s%n_null() - 1_int64, &
            "the counts must partition the elements offered")
    end subroutine test_object_carries_the_exclusion_policy

    !> An empty population is a state, not a failure -- and it is not the same as an uncomputed one.
    subroutine test_object_empty_and_cleared(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        type(pf_stats) :: s
        real(real64) :: m, t

        call check(error, .not. s%is_computed(), &
            "a default-initialised pf_stats must report that it holds no population")
        if (allocated(error)) return

        call s%init()
        call check(error, s%is_computed(), "%init must leave the object computed, over an empty population")
        if (allocated(error)) return
        call check(error, s%n() == 0_int64 .and. s%n_valid() == 0_int64, "an empty population counts 0")
        if (allocated(error)) return
        t = s%sum()
        call check(error, t == 0.0_real64, "an empty population must sum to exactly 0")
        if (allocated(error)) return
        m = s%mean()
        call check(error, m /= m, "the mean of an empty population must be a quiet NaN")
        if (allocated(error)) return

        call s%compute([1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64])
        call check(error, s%mean() == 2.5_real64, "%compute must overwrite whatever the object held")
        if (allocated(error)) return
        call s%clear()
        call check(error, .not. s%is_computed(), "%clear must return the object to its initial state")
    end subroutine test_object_empty_and_cleared

    !> An unweighted element and a weight-1 element are the same thing, so mixing them must work.
    !!
    !! Nothing forces a caller streaming a file to have weights for the first row group, and the
    !! retained buffer has to hold a weight per value once any weight exists. Without the backfill
    !! the weights would silently be shorter than the values beside them, which is a wrong answer
    !! rather than a crash: the moments would be computed against whatever the buffer's spare
    !! capacity happened to contain.
    subroutine test_weights_arriving_late_backfill(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        real(real64), allocatable :: v(:), w(:)
        real(real64) :: got(NQ), want(NQ)
        type(pf_stats) :: s, whole
        integer :: q
        integer(int64) :: i

        call golden_fixture(1000_int64, v)
        allocate(w(1000))
        w(1:400) = 1.0_real64
        do i = 401_int64, 1000_int64, 1_int64
            w(i) = 1.0_real64 + real(mod(i, 4_int64), real64)
        end do
        call whole%compute(v, weights=w)
        call object_row(whole, want)

        call s%init()
        call s%update(v(1:400))                          ! no weights at all
        call s%update(v(401:1000), weights=w(401:1000))  ! weighted from here on
        call object_row(s, got)
        do q = 1, NQ
            call check(error, got(q) == want(q), "an unweighted update followed by a weighted one " // &
                "must equal one weighted %compute with the missing weights as 1: " // trim(QNAME(q)))
            if (allocated(error)) return
        end do
        call check(error, s%sum_weights() == whole%sum_weights(), &
            "the backfilled weights must sum to the same total")
    end subroutine test_weights_arriving_late_backfill

    ! ==============================================================================================
    ! The per-kind entry layer
    !
    ! Every kind widens into a real64 buffer and calls the ONE engine, so what has to be asserted is
    ! not that each kind computes the right answer -- the golden vectors already pin the engine --
    ! but that widening is EXACT and that every specific reaches it. Both are `==` comparisons
    ! against the real64 form over the same numbers; a tolerance here would pass against a kind that
    ! had quietly reached a different code path.
    ! ==============================================================================================

    !> An integer fixture, chosen so that every kind can hold it EXACTLY.
    !!
    !! Values stay inside +/-500001, which is well under real32's 2**24 exact-integer ceiling and
    !! trivially inside int32. That is what makes `==` the right comparison below: any difference
    !! between kinds would be the library's, not the format's.
    subroutine kind_fixture(n, iv)
        integer(int64), intent(in) :: n                          !! how many values to build.
        integer(int32), allocatable, intent(out) :: iv(:)        !! the population.
        integer(int64) :: i, a

        allocate(iv(n))
        do i = 1_int64, n
            a = mod(i * i * 7919_int64 + 12345_int64, 1000003_int64)
            iv(i) = int(a - 500001_int64, int32)
        end do
    end subroutine kind_fixture

    !> Every numeric kind widens exactly, so all four agree with `real64` bit for bit.
    subroutine test_every_kind_agrees_with_real64(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        integer(int32), allocatable :: iv(:)
        integer(int64), allocatable :: i8(:)
        real(real32), allocatable :: r4(:)
        real(real64), allocatable :: r8(:)
        real(real64) :: want(NQ), got(NQ)
        integer(int64) :: nv
        integer :: q

        call kind_fixture(1000_int64, iv)
        allocate(i8(1000), r4(1000), r8(1000))
        i8 = int(iv, int64)
        r4 = real(iv, real32)
        r8 = real(iv, real64)
        call one_row(r8, want)

        call one_row_i32(iv, got, nv)
        do q = 1, NQ
            call check(error, got(q) == want(q), &
                "an int32 population must give exactly the real64 answer: " // trim(QNAME(q)))
            if (allocated(error)) return
        end do
        call check(error, nv == 1000_int64, "the int32 form must report every element as valid")
        if (allocated(error)) return

        call one_row_i64(i8, got)
        do q = 1, NQ
            call check(error, got(q) == want(q), &
                "an int64 population must give exactly the real64 answer: " // trim(QNAME(q)))
            if (allocated(error)) return
        end do

        call one_row_f32(r4, got)
        do q = 1, NQ
            call check(error, got(q) == want(q), &
                "a real32 population of exact integers must give exactly the real64 answer: " // &
                trim(QNAME(q)))
            if (allocated(error)) return
        end do
    end subroutine test_every_kind_agrees_with_real64

    !> `pf_moments` over a real64 array, packed into the comparison row.
    subroutine one_row(v, got)
        real(real64), intent(in) :: v(:)     !! the population.
        real(real64), intent(out) :: got(NQ) !! the nine compared quantities.
        call pf_moments(v, mean=got(2), variance=got(3), stddev=got(4), sem=got(5), &
            skewness=got(6), kurtosis=got(7), vsum=got(1), vmin=got(8), vmax=got(9))
    end subroutine one_row

    !> The same over an int32 array, which also reports `n_valid` so the counts are covered.
    subroutine one_row_i32(v, got, nv)
        integer(int32), intent(in) :: v(:)      !! the population.
        real(real64), intent(out) :: got(NQ)    !! the nine compared quantities.
        integer(int64), intent(out) :: nv       !! how many were in the population.
        call pf_moments(v, n_valid=nv, mean=got(2), variance=got(3), stddev=got(4), sem=got(5), &
            skewness=got(6), kurtosis=got(7), vsum=got(1), vmin=got(8), vmax=got(9))
    end subroutine one_row_i32

    !> The same over an int64 array.
    subroutine one_row_i64(v, got)
        integer(int64), intent(in) :: v(:)   !! the population.
        real(real64), intent(out) :: got(NQ) !! the nine compared quantities.
        call pf_moments(v, mean=got(2), variance=got(3), stddev=got(4), sem=got(5), &
            skewness=got(6), kurtosis=got(7), vsum=got(1), vmin=got(8), vmax=got(9))
    end subroutine one_row_i64

    !> The same over a real32 array.
    subroutine one_row_f32(v, got)
        real(real32), intent(in) :: v(:)     !! the population.
        real(real64), intent(out) :: got(NQ) !! the nine compared quantities.
        call pf_moments(v, mean=got(2), variance=got(3), stddev=got(4), sem=got(5), &
            skewness=got(6), kurtosis=got(7), vsum=got(1), vmin=got(8), vmax=got(9))
    end subroutine one_row_f32

    !> The same over a parquet_column.
    subroutine one_row_col(c, got, nnull)
        type(parquet_column), intent(in) :: c   !! the column.
        real(real64), intent(out) :: got(NQ)    !! the nine compared quantities.
        integer(int64), intent(out) :: nnull    !! how many rows were null.
        call pf_moments(c, mean=got(2), variance=got(3), stddev=got(4), sem=got(5), &
            skewness=got(6), kurtosis=got(7), vsum=got(1), vmin=got(8), vmax=got(9), n_null=nnull)
    end subroutine one_row_col

    !> The mean of a logical array is the fraction that are `.true.`.
    subroutine test_logical_mean_is_a_fraction(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        logical :: b(10)
        real(real64) :: m, t
        integer(int64) :: n

        b = .false.
        b(1:3) = .true.
        call pf_sum(b, t)
        call check(error, t == 3.0_real64, "the sum of a logical array must count the .true. entries")
        if (allocated(error)) return
        call pf_count_valid(b, n)
        call check(error, n == 10_int64, "pf_count_valid must count a .false. as an ordinary value")
        if (allocated(error)) return

        ! The claim being tested is that `.true.` widens to 1 and `.false.` to 0, so the comparison
        ! is against the widened array and not against a decimal literal. `3/10` is NOT the right
        ! expectation: the two-pass mean refines `sum/n` by the re-centring correction, which is
        ! more accurate and differs from the naive quotient in the last bit here.
        call pf_mean(b, m)
        call pf_mean(merge(1.0_real64, 0.0_real64, b), t)
        call check(error, m == t, "the mean of a logical array must equal the mean of its 0/1 form")
        if (allocated(error)) return

        ! A fraction that IS a binary one, so the refinement is exactly zero and the answer can be
        ! written down: five of ten.
        b = .false.
        b(1:5) = .true.
        call pf_mean(b, m)
        call check(error, m == 0.5_real64, "the mean of a half-true logical array must be exactly 0.5")
    end subroutine test_logical_mean_is_a_fraction

    !> A `parquet_column` reaches the same engine as the array of the same numbers.
    subroutine test_column_matches_its_array(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        integer(int32), allocatable :: iv(:)
        real(real64), allocatable :: r8(:)
        logical, allocatable :: mask(:)
        real(real64) :: want(NQ), got(NQ)
        type(parquet_column) :: c
        integer(int64) :: nnull, i, n
        integer :: q

        call kind_fixture(500_int64, iv)
        allocate(r8(500))
        r8 = real(iv, real64)

        ! (a) a null-free column: the mask never exists, on either side.
        call c%init(PK_INT32, 500_int64)
        call c%set_all(iv)
        call one_row(r8, want)
        call one_row_col(c, got, nnull)
        do q = 1, NQ
            call check(error, got(q) == want(q), &
                "a null-free column must give exactly the array answer: " // trim(QNAME(q)))
            if (allocated(error)) return
        end do
        call check(error, nnull == 0_int64, "a null-free column must report no nulls")
        if (allocated(error)) return

        ! (b) the same column with nulls: the column's own validity must reach the engine as the
        ! equivalent is_valid= mask does on the array side. This is the assertion that would fail
        ! if row_validity were dropped, which nothing else here can see.
        allocate(mask(500))
        mask = .true.
        do i = 1_int64, 500_int64, 6_int64
            call c%set_null(i)
            mask(i) = .false.
        end do
        call pf_moments(r8, is_valid=mask, mean=want(2), variance=want(3), stddev=want(4), &
            sem=want(5), skewness=want(6), kurtosis=want(7), vsum=want(1), vmin=want(8), &
            vmax=want(9))
        call one_row_col(c, got, nnull)
        do q = 1, NQ
            call check(error, got(q) == want(q), &
                "a column's own nulls must reach the engine as is_valid= does: " // trim(QNAME(q)))
            if (allocated(error)) return
        end do
        call check(error, nnull == count(.not. mask, kind=int64), &
            "a column form must report the column's nulls")
        if (allocated(error)) return
        call pf_count_valid(c, n)
        call check(error, n == 500_int64 - nnull, "pf_count_valid must accept a column too")
    end subroutine test_column_matches_its_array

    !> Every one of the five column kinds is accepted, and each answers for itself.
    !!
    !! A generic that resolved four of five would pass any test written for one of them, so all
    !! five are built and read back rather than a representative.
    subroutine test_every_column_kind(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        type(parquet_column) :: c
        real(real64) :: m
        logical :: b(4) = [.true., .false., .true., .true.]

        call c%init(PK_INT32, 4_int64)
        call c%set_all([1_int32, 2_int32, 3_int32, 4_int32])
        call pf_mean(c, m)
        call check(error, m == 2.5_real64, "a PK_INT32 column must reach pf_mean")
        if (allocated(error)) return

        call c%clear()
        call c%init(PK_INT64, 4_int64)
        call c%set_all([1_int64, 2_int64, 3_int64, 4_int64])
        call pf_mean(c, m)
        call check(error, m == 2.5_real64, "a PK_INT64 column must reach pf_mean")
        if (allocated(error)) return

        call c%clear()
        call c%init(PK_FLOAT32, 4_int64)
        call c%set_all([1.0_real32, 2.0_real32, 3.0_real32, 4.0_real32])
        call pf_mean(c, m)
        call check(error, m == 2.5_real64, "a PK_FLOAT32 column must reach pf_mean")
        if (allocated(error)) return

        call c%clear()
        call c%init(PK_FLOAT64, 4_int64)
        call c%set_all([1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64])
        call pf_mean(c, m)
        call check(error, m == 2.5_real64, "a PK_FLOAT64 column must reach pf_mean")
        if (allocated(error)) return

        call c%clear()
        call c%init(PK_LOGICAL, 4_int64)
        call c%set_all(b)
        call pf_mean(c, m)
        call check(error, m == 0.75_real64, "a PK_LOGICAL column must reach pf_mean as 0/1")
    end subroutine test_every_column_kind

    !> An integer kind takes no `skipnan` and reports no `n_nan`, and that is the assertion.
    !!
    !! There is nothing to run here: a call passing `skipnan=` to the int32 specific would not
    !! compile, so the test is that this file compiles at all with the calls below in it. What CAN
    !! be checked at run time is the other half -- that a real32 NaN really is excluded after
    !! widening, which is the one thing the widening could plausibly break.
    subroutine test_real32_nan_survives_widening(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        real(real32) :: v(5)
        real(real64) :: m
        integer(int64) :: nn, n

        v = [1.0_real32, 2.0_real32, 0.0_real32, 4.0_real32, 5.0_real32]
        v(3) = ieee_value(1.0_real32, ieee_quiet_nan)
        call pf_mean(v, m, n_nan=nn)
        call check(error, m == 3.0_real64, "a real32 NaN must be excluded after widening to real64")
        if (allocated(error)) return
        call check(error, nn == 1_int64, "the real32 form must report the NaN it excluded")
        if (allocated(error)) return
        call pf_mean(v, m, skipnan=.false.)
        call check(error, m /= m, "skipnan=.false. must propagate a widened real32 NaN")
        if (allocated(error)) return
        call pf_count_valid(v, n, skipnan=.false.)
        call check(error, n == 5_int64, "pf_count_valid(skipnan=.false.) must keep the NaN")
    end subroutine test_real32_nan_survives_widening

    !> `pf_stats` must not be the one place in the module that is real64-only.
    !!
    !! Each kind's `%compute` widens and delegates to the real64 form, so the assertion is `==`
    !! against that form over the same numbers -- and `%update` gets the same treatment, because a
    !! type whose `%compute` accepts a column and whose `%update` does not would be a worse wart
    !! than the one this closes.
    subroutine test_object_accepts_every_kind(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        integer(int32), allocatable :: iv(:)
        real(real64), allocatable :: r8(:)
        real(real64) :: want(NQ), got(NQ)
        type(pf_stats) :: ref, s
        type(parquet_column) :: c
        integer :: q

        call kind_fixture(400_int64, iv)
        allocate(r8(400))
        r8 = real(iv, real64)
        call ref%compute(r8)
        call object_row(ref, want)

        ! (a) every widened array kind through %compute.
        call s%compute(iv)
        call object_row(s, got)
        do q = 1, NQ
            call check(error, got(q) == want(q), &
                "%compute over int32 must equal %compute over the same real64 values: " // trim(QNAME(q)))
            if (allocated(error)) return
        end do
        call s%compute(int(iv, int64))
        call check(error, s%mean() == want(2), "%compute must accept an int64 array")
        if (allocated(error)) return
        call s%compute(real(iv, real32))
        call check(error, s%mean() == want(2), "%compute must accept a real32 array")
        if (allocated(error)) return
        call s%compute(iv > 0_int32)
        call check(error, s%n_valid() == 400_int64, "%compute must accept a logical array")
        if (allocated(error)) return

        ! (b) a column through %compute.
        call c%init(PK_INT32, 400_int64)
        call c%set_all(iv)
        call s%compute(c)
        call object_row(s, got)
        do q = 1, NQ
            call check(error, got(q) == want(q), &
                "%compute over a column must equal %compute over its values: " // trim(QNAME(q)))
            if (allocated(error)) return
        end do

        ! (c) %update over a widened kind, and over a column, in one retained population.
        call s%init()
        call s%update(iv(1:200))
        call s%update(int(iv(201:400), int64))
        call object_row(s, got)
        do q = 1, NQ
            call check(error, got(q) == want(q), &
                "an %update loop mixing kinds must equal %compute over the whole: " // trim(QNAME(q)))
            if (allocated(error)) return
        end do
        call s%init()
        call s%update(c)
        call check(error, s%mean() == want(2), "%update must accept a column too")
    end subroutine test_object_accepts_every_kind

    !> A column's storage is its CAPACITY, which is larger than its row count after appending.
    !!
    !! `%append_values` grows the buffer geometrically, so the slack beyond `%length()` holds
    !! whatever the allocator left there, and a statistic taken over the storage rather than the
    !! rows is a silent wrong answer -- too many elements, moments over garbage, nothing to
    !! announce it. That is `feature_risks.md` Risk-67 reaching this module.
    !!
    !! **What actually protects it is one level down**, and this test was written believing
    !! otherwise: `parquet_column_data_ptr` returns `col%i32(1:col%nrows)`, so the pointer never
    !! spans the slack at all. Mutating this module to read the whole pointer changes no answer.
    !! The test is kept because it pins that contract END TO END -- a future `data_ptr` that
    !! stopped slicing would fail here -- but it is a regression test for `parquet_columns`'
    !! promise, not for arithmetic in `parquet_stats`, and the distinction is worth knowing before
    !! anyone reads a green run as evidence about this module.
    !!
    !! The fixture is only meaningful with real slack, so that is asserted first: a future growth
    !! rule that happened to fit exactly would fail here rather than quietly stop testing.
    subroutine test_column_capacity_is_not_the_population(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        type(parquet_column) :: c
        real(real64) :: m, want
        integer(int64) :: n
        integer :: k

        ! One row at a time, because the 1.5x growth rule only leaves slack once the geometric
        ! term overtakes the exact requirement -- appending the five in two batches fits exactly.
        call c%init(PK_INT32, 0_int64)
        do k = 1, 5
            call c%append_values([int(k, int32)])
        end do
        n = c%length()
        call check(error, n == 5_int64, "the fixture must hold five rows")
        if (allocated(error)) return
        call check(error, c%capacity() > n, &
            "this fixture is only meaningful with spare capacity; %append_values must have left some")
        if (allocated(error)) return

        call pf_mean(c, m)
        call pf_mean([1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64, 5.0_real64], want)
        call check(error, m == want, &
            "a column must be read to its row count, never to its capacity")
        if (allocated(error)) return
        call pf_count_valid(c, n)
        call check(error, n == 5_int64, "pf_count_valid over a column must count rows, not capacity")
    end subroutine test_column_capacity_is_not_the_population

    !> The fixed block decomposition is a published contract, so it is asserted rather than implied.
    !!
    !! Every other test in this suite compares against the 50-digit oracle to a tolerance, and a
    !! tolerance cannot see the block tree: a naive left-to-right sum agrees with the pairwise one
    !! to fifteen digits on any ordinary fixture. So this test reconstructs the tree explicitly --
    !! blocks of `STATS_BLOCK` elements, summed serially, then combined by adjacent pairs low to
    !! high with an odd tail carried forward -- and compares with `==`.
    !!
    !! **`BLK` below duplicates `STATS_BLOCK`, which is private to `parquet_stats_core`, and that is
    !! the point rather than a compromise**: if the constant changes, this test fails, which is the
    !! only warning anyone gets that every previously published answer has moved in its last bits.
    !!
    !! It reaches **both sides of the size threshold**, per CLAUDE.md's rule that a constant gating
    !! behaviour on input size needs a test on either side of it. One element of `1e16` beside a
    !! thousand ones is a population on which the two summation orders visibly disagree -- each `1`
    !! is lost individually against `1e16`, while a block of them summed first is not -- so:
    !!
    !!   * below one block the tree degenerates to a single serial sum, and the naive total is the
    !!     right answer;
    !!   * above it the answers differ, and the library must give the pairwise one.
    !!
    !! The second assertion is the negative control. Without it the whole test would pass just as
    !! happily against an engine that had abandoned pairwise summation entirely.
    subroutine test_block_tree_is_the_documented_one(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        integer(int64), parameter :: BLK = 128_int64        !! STATS_BLOCK; see above.
        real(real64), allocatable :: x(:)
        real(real64) :: got, want, naive
        integer(int64) :: n

        ! ---- Below one block: the tree is one serial sum, so pairwise and naive must agree ----
        n = 100_int64
        call spiky(x, n)
        naive = running_total(x)
        call pf_sum(x, got)
        call check(error, got == naive, &
            "a population shorter than one block is one serial sum, so it must equal the running total")
        if (allocated(error)) return

        ! ---- Above one block: they must now differ, and the library must take the pairwise side ----
        n = 1000_int64
        call spiky(x, n)
        naive = running_total(x)
        want = pairwise_by_blocks(x, BLK)
        call check(error, want /= naive, &
            "this fixture is only meaningful if the two summation orders disagree; it must span " // &
            "more than one block and lose the small values under a naive total")
        if (allocated(error)) return
        call pf_sum(x, got)
        call check(error, got == want, &
            "pf_sum must equal the pairwise combination of its own blocks, bit for bit")
        if (allocated(error)) return
        call check(error, got /= naive, &
            "pf_sum must NOT equal a naive left-to-right total -- that is what pairwise summation buys")
    end subroutine test_block_tree_is_the_documented_one

    !> One huge value followed by ones: a population whose sum depends on the order of addition.
    subroutine spiky(x, n)
        real(real64), allocatable, intent(out) :: x(:) !! the population.
        integer(int64), intent(in) :: n                !! how many elements.

        allocate(x(n))
        x = 1.0_real64
        x(1) = 1.0e16_real64
    end subroutine spiky

    !> A left-to-right running total: what the engine must NOT be doing.
    function running_total(x) result(res)
        real(real64), intent(in) :: x(:) !! the population.
        real(real64) :: res              !! the naive total.
        integer(int64) :: i

        res = 0.0_real64
        do i = 1_int64, size(x, kind=int64)
            res = res + x(i)
        end do
    end function running_total

    !> The engine's own decomposition: serial sums per block, combined by a fixed pairwise tree.
    function pairwise_by_blocks(x, blk) result(res)
        real(real64), intent(in) :: x(:) !! the population.
        integer(int64), intent(in) :: blk !! elements per block.
        real(real64) :: res               !! the combined total.
        real(real64), allocatable :: p(:)
        integer(int64) :: n, nb, j, i, lo, hi, m, k
        real(real64) :: s

        n = size(x, kind=int64)
        nb = (n + blk - 1_int64) / blk
        allocate(p(nb))
        do j = 1_int64, nb
            lo = (j - 1_int64) * blk + 1_int64
            hi = min(j * blk, n)
            s = 0.0_real64
            do i = lo, hi
                s = s + x(i)
            end do
            p(j) = s
        end do
        m = nb
        do while (m > 1_int64)
            k = 0_int64
            do j = 1_int64, m - 1_int64, 2_int64
                k = k + 1_int64
                p(k) = p(j) + p(j + 1_int64)
            end do
            if (mod(m, 2_int64) == 1_int64) then
                k = k + 1_int64
                p(k) = p(m)
            end if
            m = k
        end do
        res = p(1)
    end function pairwise_by_blocks

    !> The reproducibility contract: `pf_*` answers the same bits at every thread count.
    !!
    !! This is the assertion the whole block decomposition exists to make possible, and it is the
    !! one a reader should be most suspicious of, because **an equality test cannot see threading**.
    !! Every arm below would agree just as exactly against an engine that opened no team at all --
    !! which is exactly what the engine does at these fixture sizes, since the measured work floor
    !! is tens of thousands of survivors per thread. So the test does three things rather than one:
    !!
    !!   1. drops the floor to `0` so a fixture small enough to reason about actually threads;
    !!   2. asserts `parquet_debug_stats_team()` reports a team **greater than 1** on the threaded
    !!      arm and exactly **1** on the serial one -- the negative control, without which the
    !!      equality proves nothing;
    !!   3. compares every one of the nine moments with `==`, not a tolerance.
    !!
    !! It restores the floor before returning, including on the skip path: the override is
    !! process-global and the `stats` suite is excluded from test-drive's per-suite parallelism for
    !! that reason, but a later test in the same run would still see it.
    subroutine test_threading_changes_no_bit(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        real(real64), allocatable :: x(:), w(:)
        real(real64) :: m1, v1, s1, k1, sd1, se1, lo1, hi1, sum1
        real(real64) :: m8, v8, s8, k8, sd8, se8, lo8, hi8, sum8
        integer(int64) :: n1, n8, team_serial, team_threaded
        integer(int64) :: i, n

#ifndef _OPENMP
        call skip_test(error, "needs OpenMP: without it no team can be opened at any thread count, " // &
            "so both arms below would run the identical serial code and the equality would hold " // &
            "for the wrong reason")
        return
#endif
        n = 5000_int64
        allocate(x(n), w(n))
        do i = 1_int64, n
            x(i) = sin(real(i, real64) * 0.37_real64) * 1000.0_real64 + real(i, real64)
            w(i) = 1.0_real64 + real(modulo(i, 5_int64), real64)
        end do

        ! Team up at any size, so the threaded branch is reachable on a fixture this small.
        call parquet_debug_set_stats_min_per_thread(0_int64)

        call pf_moments(x, weights=w, n_valid=n1, mean=m1, variance=v1, stddev=sd1, sem=se1, &
            skewness=s1, kurtosis=k1, vsum=sum1, vmin=lo1, vmax=hi1, threads=1)
        team_serial = parquet_debug_stats_team()
        call pf_moments(x, weights=w, n_valid=n8, mean=m8, variance=v8, stddev=sd8, sem=se8, &
            skewness=s8, kurtosis=k8, vsum=sum8, vmin=lo8, vmax=hi8, threads=8)
        team_threaded = parquet_debug_stats_team()

        call parquet_debug_set_stats_min_per_thread(-1_int64)

        ! ---- The negative control comes FIRST: without it the equalities below are vacuous ----
        call check(error, team_serial == 1_int64, &
            "threads=1 must run pass two serially; the team observable says it did not")
        if (allocated(error)) return
        call check(error, team_threaded > 1_int64, &
            "threads=8 must actually open a team, or every equality in this test is comparing the " // &
            "serial path against itself and asserts nothing about threading")
        if (allocated(error)) return

        call check(error, n1 == n8, "n_valid must not depend on the thread count")
        if (allocated(error)) return
        call check(error, sum1 == sum8, "the sum must be bit-identical at every thread count")
        if (allocated(error)) return
        call check(error, m1 == m8, "the mean must be bit-identical at every thread count")
        if (allocated(error)) return
        call check(error, v1 == v8, "the variance must be bit-identical at every thread count")
        if (allocated(error)) return
        call check(error, sd1 == sd8, "the standard deviation must be bit-identical at every thread count")
        if (allocated(error)) return
        call check(error, se1 == se8, "the standard error must be bit-identical at every thread count")
        if (allocated(error)) return
        call check(error, s1 == s8, "the skewness must be bit-identical at every thread count")
        if (allocated(error)) return
        call check(error, k1 == k8, "the kurtosis must be bit-identical at every thread count")
        if (allocated(error)) return
        call check(error, lo1 == lo8 .and. hi1 == hi8, &
            "the extremes must be bit-identical at every thread count")
    end subroutine test_threading_changes_no_bit

    !> The work floor is a decision, so it is asserted in BOTH directions.
    !!
    !! A floor that never lets a team through and a floor that always does are both wrong, and each
    !! passes half the obvious tests. This one holds the population fixed and moves only the floor,
    !! so nothing else can explain the difference:
    !!
    !!   * at a floor above the population, `threads=4` must still run serially;
    !!   * at a floor of `0`, the same call on the same array must open a team;
    !!   * and the two must return the same bits, which is the contract the floor is allowed to
    !!     trade against speed and never against accuracy.
    !!
    !! The measured default is exercised too: a 5000-element population is far below
    !! `STATS_MIN_PER_THREAD`, so the shipped rule must decline it with no override in force. That
    !! is the arm which fails if someone lowers the constant without re-measuring.
    subroutine test_the_work_floor_decides(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        real(real64), allocatable :: x(:)
        real(real64) :: v_serial, v_threaded, v_default
        integer(int64) :: team_high, team_zero, team_default
        integer(int64) :: i, n

#ifndef _OPENMP
        call skip_test(error, "needs OpenMP: the floor decides how many threads to open, and " // &
            "without OpenMP the answer is 1 whatever the floor says, so both arms would agree " // &
            "without the floor having decided anything")
        return
#endif
        n = 5000_int64
        allocate(x(n))
        do i = 1_int64, n
            x(i) = cos(real(i, real64) * 0.11_real64) * 250.0_real64
        end do

        ! ---- The shipped rule, with no override: this population is far below the floor ----
        call pf_variance(x, v_default, threads=4)
        team_default = parquet_debug_stats_team()

        ! ---- A floor above the population: the team must be refused ----
        call parquet_debug_set_stats_min_per_thread(1000000_int64)
        call pf_variance(x, v_serial, threads=4)
        team_high = parquet_debug_stats_team()

        ! ---- A floor of zero: the same call on the same array must now thread ----
        call parquet_debug_set_stats_min_per_thread(0_int64)
        call pf_variance(x, v_threaded, threads=4)
        team_zero = parquet_debug_stats_team()

        call parquet_debug_set_stats_min_per_thread(-1_int64)

        call check(error, team_default == 1_int64, &
            "the SHIPPED floor must decline a team for a population of 5000; if this fails, the " // &
            "measured constant has been lowered and bench/benchmark_stats.sh --mode=thread should " // &
            "be re-run before trusting the new value")
        if (allocated(error)) return
        call check(error, team_high == 1_int64, &
            "a floor above the population size must refuse the team")
        if (allocated(error)) return
        call check(error, team_zero > 1_int64, &
            "a floor of zero must let the team through; otherwise this test's threaded arm never " // &
            "runs and the comparison below is serial against serial")
        if (allocated(error)) return
        call check(error, v_serial == v_threaded .and. v_serial == v_default, &
            "the floor may trade speed and never accuracy: all three arms must agree bit for bit")
    end subroutine test_the_work_floor_decides


    !> The caching contract: many order statistics, one ordering.
    !!
    !! This is the promise `pf_stats` exists to make, and `parquet_debug_stats_sorts()` is the only
    !! thing that can see it -- every value assertion below would pass just as happily against an
    !! object that re-sorted on every call.
    subroutine test_order_cache_costs_one_sort(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        type(pf_stats) :: s
        real(real64) :: x(101), med, q, r, pc, probs(3), out(3)
        integer(int64) :: i

        do i = 1_int64, 101_int64
            x(i) = sin(real(i, real64) * 0.7_real64) * 100.0_real64
        end do
        call s%compute(x)

        call parquet_debug_reset_stats_sorts()
        call check(error, parquet_debug_stats_sorts() == 0_int64, &
            "the sort counter must reset to zero")
        if (allocated(error)) return

        med = s%median()
        call check(error, parquet_debug_stats_sorts() == 1_int64, &
            "the first order statistic must build the sorted buffer exactly once")
        if (allocated(error)) return
        call check(error, s%is_ordered(), "the accumulator must report itself ordered afterwards")
        if (allocated(error)) return

        q = s%quantile(0.9_real64)
        r = s%iqr()
        pc = s%percentile_of_score(0.0_real64)
        probs = [0.1_real64, 0.5_real64, 0.99_real64]
        call s%quantiles(probs, out)
        call check(error, parquet_debug_stats_sorts() == 1_int64, &
            "four further order statistics must reuse the ordering, not rebuild it")
        if (allocated(error)) return

        ! The answers must also be right, or the counter is measuring a cache of nothing.
        call check(error, med == out(2), &
            "%median and %quantiles(0.5) must agree exactly -- they share one interpolator")
        if (allocated(error)) return
        call check(error, q > med, "the 0.9 quantile must exceed the median on this fixture")
        if (allocated(error)) return
        call check(error, r > 0.0_real64, "the IQR of a spread population must be positive")
        if (allocated(error)) return
        call check(error, pc >= 0.0_real64 .and. pc <= 1.0_real64, &
            "a percentile of score must lie in [0, 1]")
    end subroutine test_order_cache_costs_one_sort

    !> **The negative control this whole phase turns on**, written before the invalidation it
    !! checks and confirmed to fail without it.
    !!
    !! A cached median that survives an `%update` is not a loud failure -- the answer is merely
    !! stale, and a stale median is a perfectly plausible number. Every other assertion in this
    !! suite passes against that defect. Two things are asserted here and both are needed: that the
    !! second median costs a SECOND ordering (the counter), and that it equals `%compute` over the
    !! concatenation (the value). The counter alone would pass against an object that re-sorted
    !! pointlessly; the value alone would pass against one that never invalidated but happened to
    !! be asked in an order where it did not matter.
    subroutine test_update_drops_the_order_cache(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        type(pf_stats) :: s, whole
        real(real64) :: a(41), b(37), both(78), m1, m2, want
        integer(int64) :: i

        do i = 1_int64, 41_int64
            a(i) = real(i, real64)
        end do
        do i = 1_int64, 37_int64
            b(i) = 1000.0_real64 + real(i, real64)
        end do
        both(1:41) = a
        both(42:78) = b

        call s%compute(a)
        call parquet_debug_reset_stats_sorts()
        m1 = s%median()
        call check(error, parquet_debug_stats_sorts() == 1_int64, &
            "the first median must order the buffer once")
        if (allocated(error)) return

        call s%update(b)
        call check(error, .not. s%is_ordered(), &
            "%update must drop the order cache; a median cached across a mutation is stale")
        if (allocated(error)) return

        m2 = s%median()
        call check(error, parquet_debug_stats_sorts() == 2_int64, &
            "the median after an %update must rebuild the ordering, not reuse the stale one")
        if (allocated(error)) return
        call check(error, m2 /= m1, &
            "this fixture is only meaningful if the two medians differ; the appended batch is " // &
            "far above the first, so a stale answer is visibly wrong")
        if (allocated(error)) return

        call whole%compute(both)
        want = whole%median()
        call check(error, m2 == want, &
            "the median after %update must equal %compute over the concatenation, bit for bit")
    end subroutine test_update_drops_the_order_cache

    !> `%merge` is the other mutation, and it must drop the cache for the same reason.
    subroutine test_merge_drops_the_order_cache(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        type(pf_stats) :: s, part, whole
        real(real64) :: a(25), b(31), both(56), m1, m2, want
        integer(int64) :: i

        do i = 1_int64, 25_int64
            a(i) = real(i, real64) * 2.0_real64
        end do
        do i = 1_int64, 31_int64
            b(i) = 500.0_real64 + real(i, real64)
        end do
        both(1:25) = a
        both(26:56) = b

        call s%compute(a)
        call part%compute(b)
        call parquet_debug_reset_stats_sorts()
        m1 = s%median()
        call check(error, parquet_debug_stats_sorts() == 1_int64, &
            "the first median must order the buffer once")
        if (allocated(error)) return

        call s%merge(part)
        call check(error, .not. s%is_ordered(), "%merge must drop the order cache")
        if (allocated(error)) return

        m2 = s%median()
        call check(error, parquet_debug_stats_sorts() == 2_int64, &
            "the median after a %merge must rebuild the ordering")
        if (allocated(error)) return
        call check(error, m2 /= m1, "this fixture is only meaningful if the two medians differ")
        if (allocated(error)) return

        call whole%compute(both)
        want = whole%median()
        call check(error, m2 == want, &
            "the median after %merge must equal %compute over the concatenation, bit for bit")
    end subroutine test_merge_drops_the_order_cache

    !> `%release_order` gives tier B back without disturbing tier A.
    subroutine test_release_order_rebuilds(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        type(pf_stats) :: s
        real(real64) :: x(60), m1, m2, mean_before, mean_after
        integer(int64) :: i

        do i = 1_int64, 60_int64
            x(i) = cos(real(i, real64)) * 10.0_real64
        end do
        call s%compute(x)
        call parquet_debug_reset_stats_sorts()

        m1 = s%median()
        mean_before = s%mean()
        call check(error, parquet_debug_stats_sorts() == 1_int64, "one ordering so far")
        if (allocated(error)) return

        call s%release_order()
        call check(error, .not. s%is_ordered(), "%release_order must drop the ordering")
        if (allocated(error)) return

        m2 = s%median()
        mean_after = s%mean()
        call check(error, parquet_debug_stats_sorts() == 2_int64, &
            "a query after %release_order must rebuild the ordering")
        if (allocated(error)) return
        call check(error, m2 == m1, "releasing and rebuilding tier B must not change the median")
        if (allocated(error)) return
        call check(error, mean_after == mean_before, &
            "%release_order keeps tier A: the mean must be untouched")
    end subroutine test_release_order_rebuilds


    !> Every quantile method against the 50-digit oracle, which itself agrees with numpy.
    !!
    !! `tools/generate_stats_vectors.py --self-test` checks the oracle's unweighted rows against
    !! `np.quantile` at all seven probabilities, method by method -- not at the median alone, which
    !! is precisely where an earlier draft of the weighted rule hid its error. So a failure here is
    !! this library disagreeing with numpy, not two derivations disagreeing with each other.
    subroutine test_quantile_methods_match_the_oracle(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        real(real64), allocatable :: x(:)
        real(real64) :: got(NQP)
        integer :: k

        call golden_fixture(32_int64, x)
        call pf_quantiles(x, G_QPROBS, got, method="linear")
        do k = 1, NQP
            call check(error, close_to(got(k), Q_U32_LINEAR(k)), &
                "pf_quantiles(method=linear) must match the oracle at every probability")
            if (allocated(error)) return
        end do
        call pf_quantiles(x, G_QPROBS, got, method="lower")
        call check(error, all_close(got, Q_U32_LOWER), "method=lower must match the oracle")
        if (allocated(error)) return
        call pf_quantiles(x, G_QPROBS, got, method="higher")
        call check(error, all_close(got, Q_U32_HIGHER), "method=higher must match the oracle")
        if (allocated(error)) return
        call pf_quantiles(x, G_QPROBS, got, method="nearest")
        call check(error, all_close(got, Q_U32_NEAREST), "method=nearest must match the oracle")
        if (allocated(error)) return
        call pf_quantiles(x, G_QPROBS, got, method="midpoint")
        call check(error, all_close(got, Q_U32_MIDPOINT), "method=midpoint must match the oracle")
        if (allocated(error)) return
        call pf_quantiles(x, G_QPROBS, got, method="inverted_cdf")
        call check(error, all_close(got, Q_U32_ICDF), "method=inverted_cdf must match the oracle")
    end subroutine test_quantile_methods_match_the_oracle

    !> The weighted rule, and the invariant it exists for.
    !!
    !! No reference library interpolates a weighted quantile, so the weighted rows come from this
    !! project's own derivation. What makes them trustworthy is the **equal-weight reduction**:
    !! uniform weights must give exactly the unweighted answer, so a script that adds `weights=`
    !! does not see its numbers move. That is asserted with `==`, not a tolerance.
    subroutine test_weighted_quantiles_reduce_to_unweighted(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        real(real64), allocatable :: x(:), w(:)
        real(real64) :: got(NQP), plain(NQP)
        integer :: k

        call golden_fixture(32_int64, x)
        allocate(w(32))
        w = 3.0_real64
        call pf_quantiles(x, G_QPROBS, plain, method="linear")
        call pf_quantiles(x, G_QPROBS, got, weights=w, method="linear")
        do k = 1, NQP
            ! **To a few ulp, not bit for bit, and the difference matters.** The reduction is an
            ! ALGEBRAIC identity -- with every weight equal, `S_{i-1}/(W - w_i)` is exactly
            ! `(i-1)/(m-1)` -- but the weighted and unweighted paths reach it by different
            ! expressions, and floating point does not promise that equal values arrive by equal
            ! roundings. What a user is promised is that adding uniform weights introduces no
            ! DISCONTINUITY in their numbers, and a few ulp is not one. The tolerance is far
            ! tighter than the golden one for that reason: this compares two of our own paths.
            call check(error, abs(got(k) - plain(k)) <= 1.0e-14_real64 * max(1.0_real64, abs(plain(k))), &
                "equal weights must reproduce the unweighted quantile; a discontinuity at " // &
                "'all weights equal' is a trap a user meets by accident")
            if (allocated(error)) return
        end do

        deallocate(w)
        call golden_weights_mod5(32_int64, w)
        call pf_quantiles(x, G_QPROBS, got, weights=w, method="linear")
        call check(error, all_close(got, Q_WVAR_LINEAR), &
            "the weighted linear rule must match the derivation at 50 digits")
        if (allocated(error)) return
        call pf_quantiles(x, G_QPROBS, got, weights=w, method="inverted_cdf")
        call check(error, all_close(got, Q_WVAR_ICDF), &
            "weighted inverted_cdf must match the oracle, which numpy itself agrees with")
    end subroutine test_weighted_quantiles_reduce_to_unweighted

    !> The one-shot family and the object must agree, and `pf_median` must be `pf_quantile(0.5)`.
    !!
    !! They travel different routes on purpose -- a single one-shot probe SELECTS where the object
    !! orders -- so this is a real cross-check of two implementations rather than a tautology.
    subroutine test_order_forms_agree(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        type(pf_stats) :: s
        real(real64), allocatable :: x(:)
        real(real64) :: med, q, r, med_obj, q_obj, r_obj, pc, pc_obj

        call golden_fixture(33_int64, x)
        call pf_median(x, med)
        call pf_quantile(x, 0.5_real64, q)
        call check(error, med == q, "pf_median must be exactly pf_quantile(0.5)")
        if (allocated(error)) return

        call s%compute(x)
        med_obj = s%median()
        call check(error, med_obj == med, &
            "the object's median must equal the one-shot median, though one selects and the " // &
            "other orders")
        if (allocated(error)) return

        call pf_quantile(x, 0.9_real64, q)
        q_obj = s%quantile(0.9_real64)
        call check(error, q_obj == q, "the object and the one-shot quantile must agree")
        if (allocated(error)) return

        call pf_iqr(x, r)
        r_obj = s%iqr()
        call check(error, r_obj == r, "the object and the one-shot IQR must agree")
        if (allocated(error)) return

        call pf_percentile_of_score(x, med, pc)
        pc_obj = s%percentile_of_score(med)
        call check(error, pc_obj == pc, &
            "the object and the one-shot percentile-of-score must agree")
    end subroutine test_order_forms_agree

    !> Both sides of the select-versus-order threshold must give the same answer.
    !!
    !! The two are different code paths -- `pf_nth_element` twice against one `pf_argsort` -- and
    !! nothing else in the suite would notice if one of them were wrong, because the shipped
    !! threshold puts every small test on the same side of it. CLAUDE.md's size-threshold rule is
    !! what this exists for, and `parquet_debug_set_stats_quantile_sort_min` is what reaches the
    !! other side.
    subroutine test_selection_and_sort_paths_agree(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        real(real64), allocatable :: x(:)
        real(real64) :: sel(NQP), srt(NQP)
        integer :: k

        call golden_fixture(64_int64, x)
        ! A huge floor: every count is below it, so every probe SELECTS.
        call parquet_debug_set_stats_quantile_sort_min(1000000_int64)
        call pf_quantiles(x, G_QPROBS, sel, method="linear")
        ! A floor of zero: nothing is below it, so the population is ORDERED once.
        call parquet_debug_set_stats_quantile_sort_min(0_int64)
        call pf_quantiles(x, G_QPROBS, srt, method="linear")
        call parquet_debug_set_stats_quantile_sort_min(-1_int64)

        do k = 1, NQP
            call check(error, sel(k) == srt(k), &
                "selection and ordering must give bit-identical quantiles; they are two code " // &
                "paths and only this test crosses the threshold between them")
            if (allocated(error)) return
        end do
        call check(error, all_close(srt, Q_U32_LINEAR) .eqv. .false., &
            "this fixture is n=64, so it must NOT coincide with the n=32 oracle row -- a " // &
            "sanity check that the comparison above is not comparing two constants")
    end subroutine test_selection_and_sort_paths_agree

    !> `pf_trim_mean` against scipy's contract, and its degenerate cases.
    subroutine test_trim_mean_contract(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        real(real64) :: x(10), m, plain
        integer :: i

        do i = 1, 10
            x(i) = real(i, real64)
        end do
        ! prop = 0 is the ordinary mean.
        call pf_trim_mean(x, 0.0_real64, m)
        call pf_mean(x, plain)
        call check(error, m == plain, "pf_trim_mean(prop=0) must be exactly the ordinary mean")
        if (allocated(error)) return

        ! floor(0.2*10) = 2 from each end: the middle six are 3..8, mean 5.5.
        call pf_trim_mean(x, 0.2_real64, m)
        call check(error, abs(m - 5.5_real64) < 1.0e-12_real64, &
            "trimming 2 from each end of 1..10 must average 3..8, which is 5.5")
        if (allocated(error)) return

        ! floor(0.25*10) = 2 as well, so the answer must not change until the floor does.
        call pf_trim_mean(x, 0.25_real64, m)
        call check(error, abs(m - 5.5_real64) < 1.0e-12_real64, &
            "the trimmed count is floor(prop*n), so the answer changes in steps, not smoothly")
    end subroutine test_trim_mean_contract

    !> `pf_percentile_of_score` against scipy's four conventions.
    subroutine test_percentile_of_score_kinds(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        real(real64) :: x(5), p
        real(real64), parameter :: TOL = 1.0e-12_real64

        ! scipy.stats.percentileofscore([1,2,3,3,5], 3, kind=...) / 100:
        !   strict 0.4, weak 0.8, mean 0.6, rank 0.7
        x = [1.0_real64, 2.0_real64, 3.0_real64, 3.0_real64, 5.0_real64]
        call pf_percentile_of_score(x, 3.0_real64, p, kind="strict")
        call check(error, abs(p - 0.4_real64) < TOL, "kind=strict counts only values below")
        if (allocated(error)) return
        call pf_percentile_of_score(x, 3.0_real64, p, kind="weak")
        call check(error, abs(p - 0.8_real64) < TOL, "kind=weak counts values at or below")
        if (allocated(error)) return
        call pf_percentile_of_score(x, 3.0_real64, p, kind="mean")
        call check(error, abs(p - 0.6_real64) < TOL, "kind=mean averages weak and strict")
        if (allocated(error)) return
        call pf_percentile_of_score(x, 3.0_real64, p, kind="rank")
        call check(error, abs(p - 0.7_real64) < TOL, &
            "kind=rank is scipy's default and carries its half-place continuity correction")
        if (allocated(error)) return
        ! With no ties, rank collapses to the plain share below.
        call pf_percentile_of_score(x, 4.0_real64, p, kind="rank")
        call check(error, abs(p - 0.8_real64) < TOL, &
            "with no tie at the score, rank must agree with strict and weak")
    end subroutine test_percentile_of_score_kinds

    !> An empty or fully excluded population gives NaN and `ok=.false.`, never an abort.
    subroutine test_order_degenerate_populations(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        real(real64) :: empty(0), x(4), med, r, p
        logical :: mask(4), ok
        real(real64) :: q(2)

        call pf_median(empty, med, ok=ok)
        call check(error, med /= med .and. .not. ok, &
            "an empty population must give a quiet NaN with ok=.false., not an abort")
        if (allocated(error)) return

        x = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64]
        mask = .false.
        call pf_median(x, med, is_valid=mask, ok=ok)
        call check(error, med /= med .and. .not. ok, "an all-null population is the empty case")
        if (allocated(error)) return
        call pf_iqr(x, r, is_valid=mask, ok=ok)
        call check(error, r /= r .and. .not. ok, "so is an all-null IQR")
        if (allocated(error)) return
        call pf_percentile_of_score(x, 2.0_real64, p, is_valid=mask, ok=ok)
        call check(error, p /= p .and. .not. ok, "and an all-null percentile of score")
        if (allocated(error)) return

        ! A single element is every quantile of itself.
        call pf_quantiles([7.5_real64], [0.0_real64, 1.0_real64], q)
        call check(error, q(1) == 7.5_real64 .and. q(2) == 7.5_real64, &
            "a one-element population is every one of its own quantiles")
    end subroutine test_order_degenerate_populations

    !> q(0) and q(1) are the minimum and the maximum, exactly, weighted or not.
    !!
    !! A property of the position rule rather than of any fixture: `p_1 = 0` and `p_m = 1` by
    !! construction, so no interpolation happens at either end.
    subroutine test_extreme_quantiles_are_the_extremes(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        real(real64), allocatable :: x(:), w(:)
        real(real64) :: q(2), lo, hi
        type(pf_stats) :: s

        call golden_fixture(50_int64, x)
        call golden_weights_mod5(50_int64, w)
        call s%compute(x, weights=w)
        lo = s%vmin()
        hi = s%vmax()

        call pf_quantiles(x, [0.0_real64, 1.0_real64], q, weights=w)
        call check(error, q(1) == lo, "q(0) must be exactly the minimum, weighted too")
        if (allocated(error)) return
        call check(error, q(2) == hi, "q(1) must be exactly the maximum, weighted too")
        if (allocated(error)) return

        call pf_quantiles(x, [0.0_real64, 1.0_real64], q)
        call check(error, q(1) == minval(x) .and. q(2) == maxval(x), &
            "and unweighted, where the population is the whole array")
    end subroutine test_extreme_quantiles_are_the_extremes

    !> `weight_type="frequency"` changes the DEFAULT method, and an explicit method still wins.
    subroutine test_frequency_weights_default_to_inverted_cdf(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        real(real64) :: x(3), w(3), q_default, q_icdf, q_linear

        ! x = [1,2,3], w = [1,1,2] expands to [1,2,3,3]; at p=0.5 the step rule gives 2 and the
        ! linear rule 2.25, so the two are visibly different and the default is observable.
        x = [1.0_real64, 2.0_real64, 3.0_real64]
        w = [1.0_real64, 1.0_real64, 2.0_real64]

        call pf_quantile(x, 0.5_real64, q_default, weights=w, weight_type="frequency")
        call pf_quantile(x, 0.5_real64, q_icdf, weights=w, method="inverted_cdf")
        call pf_quantile(x, 0.5_real64, q_linear, weights=w, method="linear")
        call check(error, q_icdf /= q_linear, &
            "this fixture is only meaningful if the two rules disagree at p=0.5")
        if (allocated(error)) return
        call check(error, q_default == q_icdf, &
            "weight_type=frequency must default to inverted_cdf; that token IS frequency " // &
            "expansion, and the reliability-shaped median would be the wrong answer for it")
        if (allocated(error)) return

        call pf_quantile(x, 0.5_real64, q_default, weights=w, weight_type="frequency", &
            method="linear")
        call check(error, q_default == q_linear, "an explicit method= must still win")
    end subroutine test_frequency_weights_default_to_inverted_cdf

    !> Every `pf_mad` case matches the 50-digit oracle, on both scales.
    !!
    !! The two scales are asserted as a PAIR from committed numbers rather than against a literal
    !! factor, so a `scale_factor` that lost its constant fails here rather than silently agreeing
    !! with a test that copied the same constant out of the source.
    subroutine test_mad_matches_the_oracle(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        real(real64), allocatable :: x(:), w(:), big(:)
        logical, allocatable :: mask(:)
        real(real64) :: got(2)
        integer(int64) :: i

        call golden_fixture(32_int64, x)
        call pf_mad(x, got(1))
        call pf_mad(x, got(2), scale="raw")
        call check(error, all_close(got, M_U32), "pf_mad over the n=32 fixture")
        if (allocated(error)) return
        ! The EXPLICIT token must give the default's answer. Without this assertion the two carry
        ! separate paths that nothing compares, and a mutation to the explicit one survives the
        ! whole suite -- which is how this assertion came to be written.
        call pf_mad(x, got(2), scale="normal")
        call check(error, got(2) == got(1), &
            'scale="normal" must be exactly what the default gives')
        if (allocated(error)) return
        call pf_mad(x, got(2), scale="NORMAL")
        call check(error, got(2) == got(1), "the token must be matched case-insensitively")
        if (allocated(error)) return
        call pf_mad(x, got(2), scale="raw")

        call golden_fixture(33_int64, x)
        call pf_mad(x, got(1))
        call pf_mad(x, got(2), scale="raw")
        call check(error, all_close(got, M_U33), "pf_mad over the odd-length fixture")
        if (allocated(error)) return

        call golden_fixture(32_int64, x)
        call pf_mad(x, got(1), center=0.5_real64)
        call pf_mad(x, got(2), scale="raw", center=0.5_real64)
        call check(error, all_close(got, M_CTR), "pf_mad about an explicit centre")
        if (allocated(error)) return

        call golden_weights_mod5(32_int64, w)
        call pf_mad(x, got(1), weights=w)
        call pf_mad(x, got(2), weights=w, scale="raw")
        call check(error, all_close(got, M_WVAR), "pf_mad with unequal weights")
        if (allocated(error)) return

        allocate(mask(32))
        do i = 1_int64, 32_int64
            mask(i) = (mod(i, 3_int64) /= 0_int64)
        end do
        call pf_mad(x, got(1), is_valid=mask)
        call pf_mad(x, got(2), is_valid=mask, scale="raw")
        call check(error, all_close(got, M_NULLS), "pf_mad over a partly null population")
        if (allocated(error)) return

        ! The case the statistic exists for: four points far outside the rest.
        allocate(big(36))
        big(1:32) = x
        big(33:36) = [1.0e6_real64, -1.0e6_real64, 2.0e6_real64, -2.0e6_real64]
        call pf_mad(big, got(1))
        call pf_mad(big, got(2), scale="raw")
        call check(error, all_close(got, M_OUT), "pf_mad is barely moved by four wild points")
    end subroutine test_mad_matches_the_oracle

    !> `pf_mad(scale="normal")` tracks `pf_stddev` on Gaussian data and does not on contaminated data.
    !!
    !! **This is P7's own acceptance gate**, and it is written as a PAIR: agreeing on the clean
    !! sample is what makes "consistent estimator of the standard deviation" a claim rather than a
    !! coincidence, and disagreeing on the contaminated one is why anybody reaches for the MAD at
    !! all. Either half alone passes against a `pf_mad` that simply returned `pf_stddev`.
    !!
    !! The sample is built from `pf_random`'s normal draws at a fixed seed, so the tolerances below
    !! are properties of a specific reproducible sample rather than of a distribution -- there is
    !! nothing statistical about this test's pass condition.
    subroutine test_mad_estimates_the_gaussian_sigma(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        integer(int64), parameter :: N = 200000_int64
        real(real64), allocatable :: g(:)
        real(real64) :: sd, mad, sd2, mad2
        integer(int64) :: i

        allocate(g(N))
        ! Stream 1 of a fixed seed. Counter-based, so this is the same sample on every machine
        ! and every thread count -- there is nothing statistical about this test's pass condition.
        call pf_random_fill_normal(20260829_int64, 1_int64, g)
        call pf_stddev(g, sd)
        call pf_mad(g, mad)
        ! 1% of each other on 200000 draws. The MAD's asymptotic efficiency is 37%, so its standard
        ! error is about 1.6x the standard deviation's -- roughly 0.4% here, and 1% is comfortably
        ! outside the sampling noise while being far tighter than anything a broken scale factor
        ! could sneak through (the unscaled MAD would be 33% low).
        call check(error, abs(mad - sd) <= 0.01_real64 * sd, &
            "pf_mad(scale=normal) must track pf_stddev on a clean Gaussian sample")
        if (allocated(error)) return

        ! Contaminate one draw in a thousand with a value 100 sigma out. The standard deviation
        ! moves by more than a factor of three; the MAD must barely notice.
        do i = 1000_int64, N, 1000_int64
            g(i) = 100.0_real64
        end do
        call pf_stddev(g, sd2)
        call pf_mad(g, mad2)
        call check(error, sd2 > 3.0_real64 * sd, &
            "the control: pf_stddev must be wrecked by 0.1% contamination")
        if (allocated(error)) return
        call check(error, abs(mad2 - mad) <= 0.01_real64 * mad, &
            "pf_mad must be almost unmoved by the same contamination")
    end subroutine test_mad_estimates_the_gaussian_sigma

    !> `%mad` caches the RAW deviation, and rescaling it costs no second traversal.
    !!
    !! The negative control is the second half: a `%mad` whose cache ignored the centre would pass
    !! the first two assertions and fail the third, and one that never cached at all fails the
    !! `has_deviation` assertions while every number stays right.
    subroutine test_mad_cache_costs_one_build(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        type(pf_stats) :: s
        real(real64), allocatable :: x(:)
        real(real64) :: a, b, c, d

        call golden_fixture(64_int64, x)
        call s%compute(x)
        call check(error, .not. s%has_deviation(), "tier C must not exist before the first %mad")
        if (allocated(error)) return
        a = s%mad()
        call check(error, s%has_deviation(), "the first %mad must build tier C")
        if (allocated(error)) return
        b = s%mad(scale="raw")
        call check(error, close_to(a, b / 0.6744897501960817_real64), &
            "the scaled and raw answers must be the same cached deviation")
        if (allocated(error)) return
        ! A DIFFERENT centre must rebuild rather than rescale the cached one.
        c = s%mad(center=0.0_real64, scale="raw")
        call check(error, .not. close_to(c, b), &
            "a different centre must give a different raw deviation, not the cached one")
        if (allocated(error)) return
        ! And going back to the default centre must give the original answer again, which is what
        ! fails if the cache is keyed on anything but the resolved centre.
        d = s%mad(scale="raw")
        call check(error, d == b, "returning to the default centre must reproduce its deviation")
    end subroutine test_mad_cache_costs_one_build

    !> `%update` and `%release_order` both drop tier C, and the next `%mad` rebuilds it.
    !!
    !! Written as the P6 invalidation tests were, and for the same reason: a cached deviation that
    !! outlives the population it describes is a plausible number, not a wrong-looking one, so
    !! nothing else in the suite can tell the difference.
    subroutine test_mutation_drops_the_deviation_cache(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        type(pf_stats) :: s
        real(real64), allocatable :: x(:)
        real(real64) :: before, after, fresh
        type(pf_stats) :: whole

        call golden_fixture(64_int64, x)
        call s%compute(x(1:32))
        before = s%mad()
        call check(error, s%has_deviation(), "tier C must be built after the first %mad")
        if (allocated(error)) return

        call s%update(x(33:64))
        call check(error, .not. s%has_deviation(), "%update must drop tier C")
        if (allocated(error)) return
        after = s%mad()
        call whole%compute(x)
        fresh = whole%mad()
        call check(error, after == fresh, &
            "the %mad after an %update must equal a fresh %compute over the whole population")
        if (allocated(error)) return
        call check(error, after /= before, &
            "the control: the two populations must actually have different deviations")
        if (allocated(error)) return

        call s%release_order()
        call check(error, .not. s%has_deviation(), "%release_order must drop tier C with tier B")
        if (allocated(error)) return
        call check(error, s%mad() == after, "the rebuilt deviation must be the same number")
    end subroutine test_mutation_drops_the_deviation_cache

    !> The one-shot `pf_mad` and `%mad` agree, over every kind and both scales.
    subroutine test_mad_forms_agree(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        type(pf_stats) :: s
        real(real64), allocatable :: x(:)
        real(real64) :: one, obj
        integer(int32), allocatable :: iv(:)
        real(real32), allocatable :: rv(:)
        integer(int64) :: i

        call golden_fixture(48_int64, x)
        call pf_mad(x, one)
        call s%compute(x)
        obj = s%mad()
        call check(error, one == obj, "pf_mad and %mad must agree bit for bit")
        if (allocated(error)) return
        call pf_mad(x, one, scale="raw")
        call check(error, one == s%mad(scale="raw"), "and on the raw scale too")
        if (allocated(error)) return

        ! The widened kinds reach the same core, so their answers must equal the real64 one's over
        ! the same values -- which is what a widening loop that dropped or duplicated an element
        ! would break.
        allocate(iv(48), rv(48))
        do i = 1_int64, 48_int64
            iv(i) = int(mod(i * 37_int64, 101_int64), int32)
            rv(i) = real(iv(i), real32)
        end do
        call pf_mad(iv, one)
        call pf_mad(rv, obj)
        call check(error, one == obj, "the int32 and real32 entry points must agree")
        if (allocated(error)) return
        call pf_mad(real(iv, real64), obj)
        call check(error, one == obj, "and both must equal the real64 core over the same values")
    end subroutine test_mad_forms_agree

    !> `pf_mode` answers scipy's mode, and breaks ties to the SMALLEST value.
    subroutine test_mode_ties_go_to_the_smallest(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        integer(int32) :: pop(9), m32
        integer(int64) :: m64, cnt
        logical :: lv(6), mb, ok

        ! 1 and 2 each appear three times; 3 twice; 7 once. scipy.stats.mode answers 1.
        pop = [3_int32, 1_int32, 3_int32, 2_int32, 2_int32, 7_int32, 2_int32, 1_int32, 1_int32]
        call pf_mode(pop, m32, count=cnt, ok=ok)
        call check(error, m32 == 1_int32, "a tie must go to the smallest value, not the first seen")
        if (allocated(error)) return
        call check(error, cnt == 3_int64, "count must be the modal value's occurrences")
        if (allocated(error)) return
        call check(error, ok, "a non-empty population must report ok")
        if (allocated(error)) return

        ! The same population reversed: a first-occurrence rule would now answer 2, which is
        ! exactly the reproducibility defect the smallest-value rule exists to prevent.
        call pf_mode(pop(9:1:-1), m32, count=cnt)
        call check(error, m32 == 1_int32, "the answer must not depend on the input order")
        if (allocated(error)) return

        call pf_mode(int(pop, int64), m64, count=cnt)
        call check(error, m64 == 1_int64 .and. cnt == 3_int64, "the int64 form must agree")
        if (allocated(error)) return

        ! .false. sorts below .true., so an even split answers .false.
        lv = [.true., .false., .true., .false., .true., .false.]
        call pf_mode(lv, mb, count=cnt)
        call check(error, (.not. mb) .and. cnt == 3_int64, &
            "an evenly split logical population must answer .false.")
    end subroutine test_mode_ties_go_to_the_smallest

    !> `pf_mode` over the two string kinds, including the trimming and empty rules.
    subroutine test_mode_over_strings(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        character(len=6) :: fruit(7)
        character(len=:), allocatable :: m
        type(parquet_string_column) :: col
        integer(int64) :: cnt, nnull
        logical :: ok

        ! "fig" and "pear" tie at three each; "fig" sorts first.
        fruit = ["pear  ", "fig   ", "pear  ", "fig   ", "kiwi  ", "fig   ", "pear  "]
        call pf_mode(fruit, m, count=cnt, ok=ok)
        call check(error, ok, "a non-empty character population must report ok")
        if (allocated(error)) return
        call check(error, m == "fig", "the alphabetically smaller of two tied strings must win")
        if (allocated(error)) return
        ! Trimmed, so the result is as long as the VALUE and not as long as the declared width --
        ! which `m == "fig"` alone cannot see, because comparison blank-pads.
        call check(error, len(m) == 3, "the result must be trimmed to its own length")
        if (allocated(error)) return
        call check(error, cnt == 3_int64, "count must be the modal string's occurrences")
        if (allocated(error)) return

        call col%append_string("delta")
        call col%append_string("alpha")
        call col%append_string("delta")
        call col%append_null()
        call col%append_string("alpha")
        call col%append_string("delta")
        call pf_mode(col, m, count=cnt, n_null=nnull, ok=ok)
        call check(error, ok .and. m == "delta", "the string column form must find its mode")
        if (allocated(error)) return
        call check(error, cnt == 3_int64, "delta occurs three times")
        if (allocated(error)) return
        call check(error, nnull == 1_int64, "the column's own null must be reported through n_null")
    end subroutine test_mode_over_strings

    !> Nulls and weights change `pf_mode`'s answer exactly as they change every other reduction's.
    subroutine test_mode_nulls_and_weights(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        integer(int32) :: pop(6), m
        real(real64) :: w(6)
        logical :: mask(6), ok
        integer(int64) :: cnt, nnull

        pop = [5_int32, 5_int32, 5_int32, 8_int32, 8_int32, 8_int32]
        ! Unweighted this is a tie at three each, so the smaller wins.
        call pf_mode(pop, m, count=cnt)
        call check(error, m == 5_int32 .and. cnt == 3_int64, "the tie must go to 5")
        if (allocated(error)) return

        ! Weight tips it: 8 carries more total weight, though both still occur three times -- and
        ! `count` must still report OCCURRENCES rather than weight.
        w = [1.0_real64, 1.0_real64, 1.0_real64, 2.0_real64, 2.0_real64, 2.0_real64]
        call pf_mode(pop, m, count=cnt, weights=w)
        call check(error, m == 8_int32, "the greatest total weight must decide a weighted mode")
        if (allocated(error)) return
        call check(error, cnt == 3_int64, "count must stay an element count under weights")
        if (allocated(error)) return

        ! A zero weight removes the element, exactly as it does everywhere else in this module.
        w = [1.0_real64, 1.0_real64, 1.0_real64, 0.0_real64, 0.0_real64, 2.0_real64]
        call pf_mode(pop, m, count=cnt, weights=w)
        call check(error, m == 5_int32 .and. cnt == 3_int64, &
            "a zero weight must remove its element from the population")
        if (allocated(error)) return

        mask = [.false., .false., .true., .true., .true., .true.]
        call pf_mode(pop, m, count=cnt, is_valid=mask, n_null=nnull)
        call check(error, m == 8_int32 .and. cnt == 3_int64, "nulls must leave the population")
        if (allocated(error)) return
        call check(error, nnull == 2_int64, "n_null must report how many were excluded")
        if (allocated(error)) return

        ! Every element excluded: ok=.false., count=0, and `m` untouched.
        m = -99_int32
        mask = .false.
        call pf_mode(pop, m, count=cnt, is_valid=mask, ok=ok, n_null=nnull)
        call check(error, (.not. ok) .and. cnt == 0_int64, "an empty population must not be ok")
        if (allocated(error)) return
        call check(error, m == -99_int32, "m must be left untouched when there is no mode")
        if (allocated(error)) return
        call check(error, nnull == 6_int64, "and every element must be counted as null")
    end subroutine test_mode_nulls_and_weights

    !> `pf_describe` fills an object that is computed AND ordered, in one pass and one sort.
    subroutine test_describe_costs_one_pass_and_one_sort(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        type(pf_stats) :: s, t
        real(real64), allocatable :: x(:)

        call golden_fixture(200_int64, x)
        call parquet_debug_reset_stats_scans()
        call parquet_debug_reset_stats_sorts()
        call pf_describe(x, s)
        call check(error, parquet_debug_stats_scans() == 2_int64, &
            "pf_describe must traverse the values exactly twice")
        if (allocated(error)) return
        call check(error, parquet_debug_stats_sorts() == 1_int64, &
            "pf_describe must order the population exactly once")
        if (allocated(error)) return
        call check(error, s%is_ordered(), "and must leave the object ordered")
        if (allocated(error)) return

        ! Reading eight statistics off it costs neither another traversal nor another ordering.
        call check(error, close_to(s%mean(), s%mean()) .and. s%n_valid() == 200_int64, &
            "the filled object must answer its counts")
        if (allocated(error)) return
        block
            real(real64) :: junk
            junk = s%stddev() + s%median() + s%iqr() + s%vmin() + s%vmax() + s%skewness()
            call check(error, junk == junk, "eight queries must all return numbers")
        end block
        if (allocated(error)) return
        call check(error, parquet_debug_stats_scans() == 2_int64, &
            "the queries must cost no further traversal")
        if (allocated(error)) return
        call check(error, parquet_debug_stats_sorts() == 1_int64, &
            "and no further ordering")
        if (allocated(error)) return

        ! It must equal %compute plus %prepare_order, which is what it claims to be.
        call t%compute(x)
        call t%prepare_order()
        call check(error, t%mean() == s%mean() .and. t%median() == s%median(), &
            "pf_describe must equal %compute followed by %prepare_order")
    end subroutine test_describe_costs_one_pass_and_one_sort

    !> `%print` writes the block, honours `verbosity="silent"`, and never aborts.
    subroutine test_print_writes_and_can_be_silenced(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        type(pf_stats) :: s, stream
        real(real64), allocatable :: x(:)
        integer :: u, ios, nlines
        character(len=256) :: line
        character(len=:), allocatable :: saved
        character(len=*), parameter :: PATH = "test_run/stats_print_p7.txt"
        logical :: seen_nan, seen_q3

        call golden_fixture(40_int64, x)
        call pf_describe(x, s)
        open(newunit=u, file=PATH, status="replace", action="write")
        call s%print(unit=u, name="demo")
        close(u)

        nlines = 0
        seen_nan = .false.
        seen_q3 = .false.
        open(newunit=u, file=PATH, status="old", action="read")
        do
            read(u, '(a)', iostat=ios) line
            if (ios /= 0) exit
            nlines = nlines + 1
            if (index(line, "n_nan") > 0) seen_nan = .true.
            if (index(line, "75%") > 0) seen_q3 = .true.
        end do
        close(u)
        ! Eleven rows plus the heading. The COUNT is asserted rather than the exact text, so that
        ! a reworded label is not a test failure while a dropped row is.
        call check(error, nlines == 12, "%print must write a heading and eleven rows")
        if (allocated(error)) return
        call check(error, seen_nan, "the n_nan row must always be shown -- it is what makes " // &
            "skipnan=.true. safe to have as a default")
        if (allocated(error)) return
        call check(error, seen_q3, "the quartile rows must be present on a retained accumulator")
        if (allocated(error)) return

        ! A streaming accumulator has no quartiles. Saying so beats aborting, and beats printing
        ! six rows as though nothing were missing.
        call stream%init(retain=.false.)
        call stream%update(x)
        open(newunit=u, file=PATH, status="replace", action="write")
        call stream%print(unit=u)
        close(u)
        nlines = 0
        open(newunit=u, file=PATH, status="old", action="read")
        do
            read(u, '(a)', iostat=ios) line
            if (ios /= 0) exit
            nlines = nlines + 1
        end do
        close(u)
        call check(error, nlines == 10, &
            "a streaming %print must write the eight tier-A rows and one line saying why the " // &
            "quartiles are missing")
        if (allocated(error)) return

        ! Silenced. The negative control is the run above, which wrote ten lines to the same unit.
        call parquet_get_verbosity(saved)
        call parquet_set_verbosity("silent")
        open(newunit=u, file=PATH, status="replace", action="write")
        call s%print(unit=u)
        close(u)
        call parquet_set_verbosity(saved)
        nlines = 0
        open(newunit=u, file=PATH, status="old", action="read")
        do
            read(u, '(a)', iostat=ios) line
            if (ios /= 0) exit
            nlines = nlines + 1
        end do
        close(u)
        call check(error, nlines == 0, 'verbosity="silent" must silence %print entirely')
        if (allocated(error)) return

        ! An object holding nothing must say so rather than abort: a printer is what someone
        ! reaches for to find out what state an object is in.
        block
            type(pf_stats) :: empty
            open(newunit=u, file=PATH, status="replace", action="write")
            call empty%print(unit=u)
            close(u)
        end block
        open(newunit=u, file=PATH, status="old", action="read")
        read(u, '(a)', iostat=ios) line
        close(u, status="delete")
        call check(error, ios == 0 .and. index(line, "no population") > 0, &
            "an uncomputed accumulator must print a line saying so")
    end subroutine test_print_writes_and_can_be_silenced

    !> Under `skipnan=.false.` an order statistic is NaN, exactly as a moment is.
    !!
    !! **The defect this closes was shipped and was silent.** A NaN sorts to one END of the buffer
    !! rather than poisoning the interpolation, so `pf_median` returned an ordinary number from a
    !! population the caller had asked to have poisoned, while `pf_mean` over the identical
    !! arguments correctly returned NaN. Every assertion in the suite passed. The negative control
    !! is the `skipnan=.true.` half below, which must still answer a number.
    subroutine test_propagating_nan_reaches_every_tier(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        real(real64) :: v(9), r
        type(pf_stats) :: s
        integer :: k
        logical :: ok

        do k = 1, 9
            v(k) = real(k, real64)
        end do
        v(4) = ieee_value(1.0_real64, ieee_quiet_nan)

        ! The moments, which have always been right, as the reference for what the tier must match.
        call pf_mean(v, r, skipnan=.false., ok=ok)
        call check(error, r /= r .and. .not. ok, "the control: pf_mean must propagate the NaN")
        if (allocated(error)) return

        call pf_median(v, r, skipnan=.false., ok=ok)
        call check(error, r /= r .and. .not. ok, "pf_median must propagate it too")
        if (allocated(error)) return
        call pf_quantile(v, 0.25_real64, r, skipnan=.false., ok=ok)
        call check(error, r /= r .and. .not. ok, "and pf_quantile")
        if (allocated(error)) return
        call pf_iqr(v, r, skipnan=.false., ok=ok)
        call check(error, r /= r .and. .not. ok, "and pf_iqr")
        if (allocated(error)) return
        call pf_trim_mean(v, 0.1_real64, r, skipnan=.false., ok=ok)
        call check(error, r /= r .and. .not. ok, &
            "and pf_trim_mean, where the NaN would otherwise be TRIMMED AWAY")
        if (allocated(error)) return
        call pf_percentile_of_score(v, 5.0_real64, r, skipnan=.false., ok=ok)
        call check(error, r /= r .and. .not. ok, "and pf_percentile_of_score")
        if (allocated(error)) return
        call pf_mad(v, r, skipnan=.false., ok=ok)
        call check(error, r /= r .and. .not. ok, "and pf_mad")
        if (allocated(error)) return

        ! The object side takes the same route through its own guard.
        call s%compute(v, skipnan=.false.)
        call check(error, s%median() /= s%median(), "%median must propagate it")
        if (allocated(error)) return
        call check(error, s%mad() /= s%mad(), "%mad must propagate it")
        if (allocated(error)) return

        ! The negative control: at the default `skipnan=.true.` every one of these is a number.
        call pf_median(v, r, ok=ok)
        call check(error, r == r .and. ok, &
            "the control: at the default skipnan the NaN is merely excluded")
        if (allocated(error)) return
        call pf_mad(v, r, ok=ok)
        call check(error, r == r .and. ok, "and pf_mad answers a number too")
    end subroutine test_propagating_nan_reaches_every_tier

    !> The second sample the two-sample tests are taken against.
    !!
    !! A different multiplier through the same recipe as `golden_fixture`, so it is exactly
    !! representable for the same reason and is NOT a shift or a permutation of the first --
    !! either of those would pin the correlation family against a degenerate case.
    subroutine golden_fixture_b(n, y)
        integer(int64), intent(in) :: n                          !! how many values to build.
        real(real64), allocatable, intent(out) :: y(:)           !! the second sample.
        integer(int64) :: i, a

        allocate(y(n))
        do i = 1_int64, n
            a = mod(i * i * 3571_int64 + 12345_int64, 1000003_int64)
            y(i) = real(a - 500001_int64, real64) / 1024.0_real64
        end do
    end subroutine golden_fixture_b

    !> A strictly POSITIVE population, which `pf_gmean` and `pf_hmean` need to be defined at all.
    subroutine golden_fixture_pos(n, x)
        integer(int64), intent(in) :: n                          !! how many values to build.
        real(real64), allocatable, intent(out) :: x(:)           !! the population.
        integer(int64) :: i, a

        allocate(x(n))
        do i = 1_int64, n
            a = mod(i * i * 7919_int64 + 12345_int64, 1000003_int64)
            x(i) = real(a + 1_int64, real64) / 1024.0_real64
        end do
    end subroutine golden_fixture_pos

    !> The sigma-clip fixture: the recipe with four wild points written over it.
    subroutine golden_fixture_clip(n, x)
        integer(int64), intent(in) :: n                          !! how many values to build.
        real(real64), allocatable, intent(out) :: x(:)           !! the population.

        call golden_fixture(n, x)
        if (n >= 7_int64) x(7) = 4000.0_real64
        if (n >= 123_int64) x(123) = -3500.0_real64
        if (n >= 200_int64) x(200) = 5000.0_real64
        if (n >= 365_int64) x(365) = -6000.0_real64
    end subroutine golden_fixture_clip

    !> Every P8 fixture recipe still produces what the generator emitted for it.
    !!
    !! Written for the same reason `test_fixture_recipe` is: a recipe that has drifted is reported
    !! here, once, rather than as an unexplained tolerance failure in every case that uses it.
    subroutine test_p8_fixture_recipes(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        real(real64), allocatable :: x(:)

        call golden_fixture_b(8_int64, x)
        call check(error, all(x == G_PROBE_B), "the second sample's recipe has drifted")
        if (allocated(error)) return
        call golden_fixture_pos(8_int64, x)
        call check(error, all(x == G_PROBE_POS), "the positive population's recipe has drifted")
        if (allocated(error)) return
        call golden_fixture_clip(8_int64, x)
        call check(error, all(x == G_PROBE_CLIP), "the sigma-clip fixture's recipe has drifted")
    end subroutine test_p8_fixture_recipes

    !> `pf_gmean` and `pf_hmean` match the 50-digit oracle, weighted and not.
    subroutine test_power_means_match_the_oracle(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        real(real64), allocatable :: x(:), w(:)
        real(real64) :: got(2)

        call golden_fixture_pos(32_int64, x)
        call pf_gmean(x, got(1))
        call pf_hmean(x, got(2))
        call check(error, all_close(got, P_U32), "the unweighted geometric and harmonic means")
        if (allocated(error)) return

        call golden_weights_mod5(32_int64, w)
        call pf_gmean(x, got(1), weights=w)
        call pf_hmean(x, got(2), weights=w)
        call check(error, all_close(got, P_WVAR), "the weighted pair; a zero weight drops its element")
        if (allocated(error)) return

        ! The ordering that always holds for a positive population, and which no rounding can
        ! reverse: harmonic <= geometric <= arithmetic. It costs nothing and it catches the two
        ! procedures being swapped, which the golden rows alone would not if both were wrong.
        block
            real(real64) :: am
            call pf_mean(x, am)
            call check(error, got(2) < got(1) .and. got(1) < am, &
                "harmonic < geometric < arithmetic must hold for a positive population")
        end block
    end subroutine test_power_means_match_the_oracle

    !> The domain rules: a zero gives exactly 0, a negative gives NaN, and neither aborts.
    subroutine test_power_mean_domain(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        real(real64) :: v(4), g, h
        logical :: ok

        v = [2.0_real64, 4.0_real64, 8.0_real64, 16.0_real64]
        call pf_gmean(v, g, ok=ok)
        ! 2, 4, 8, 16 -> exp(mean(log)) = 2**2.5 = 5.656854...
        call check(error, ok .and. close_to(g, 2.0_real64**2.5_real64), &
            "the control: a clean positive population gives the geometric mean")
        if (allocated(error)) return

        v(3) = 0.0_real64
        call pf_gmean(v, g, ok=ok)
        call pf_hmean(v, h)
        call check(error, g == 0.0_real64 .and. h == 0.0_real64, &
            "a value of exactly zero gives exactly zero, as scipy does")
        if (allocated(error)) return
        call check(error, ok, "and zero is a DEFINED answer, so ok must stay .true.")
        if (allocated(error)) return

        v(3) = -1.0_real64
        call pf_gmean(v, g, ok=ok)
        call check(error, g /= g .and. .not. ok, "a negative value gives NaN with ok=.false.")
        if (allocated(error)) return
        call pf_hmean(v, h, ok=ok)
        call check(error, h /= h .and. .not. ok, "and so does the harmonic mean")
        if (allocated(error)) return

        ! A zero AND a negative: undefined, not zero. The negative must be found first, which is
        ! what the screen's ordering guarantees.
        v = [-1.0_real64, 0.0_real64, 2.0_real64, 4.0_real64]
        call pf_gmean(v, g, ok=ok)
        call check(error, g /= g .and. .not. ok, &
            "a population holding both a zero and a negative is undefined, not zero")
        if (allocated(error)) return

        ! An empty population is the ordinary empty case, not an error.
        block
            real(real64) :: none(0)
            call pf_gmean(none, g, ok=ok)
            call check(error, g /= g .and. .not. ok, "an empty population gives NaN with ok=.false.")
        end block
    end subroutine test_power_mean_domain

    !> `pf_cov` and `pf_corr` match the oracle, and reproduce the two identities exactly.
    subroutine test_two_sample_matches_the_oracle(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        real(real64), allocatable :: x(:), y(:), w(:)
        logical, allocatable :: mask(:)
        real(real64) :: got(3), v
        integer(int64) :: i

        call golden_fixture(32_int64, x)
        call golden_fixture_b(32_int64, y)
        call pf_cov(x, y, got(1))
        call pf_corr(x, y, got(2))
        call pf_corr(x, y, got(3), method="spearman")
        call check(error, all_close(got, R_U32), "the unweighted covariance and both correlations")
        if (allocated(error)) return

        call golden_weights_mod5(32_int64, w)
        call pf_cov(x, y, got(1), weights=w)
        call pf_corr(x, y, got(2), weights=w)
        call pf_corr(x, y, got(3), weights=w, method="pearson")
        got(3) = got(2)   ! Spearman refuses weights; the oracle's third slot is unweighted-ranked
        call check(error, close_to(got(1), R_WVAR(1)) .and. close_to(got(2), R_WVAR(2)), &
            "the weighted covariance and Pearson correlation")
        if (allocated(error)) return

        allocate(mask(32))
        do i = 1_int64, 32_int64
            mask(i) = (mod(i, 3_int64) /= 0_int64)
        end do
        call pf_cov(x, y, got(1), is_valid=mask)
        call pf_corr(x, y, got(2), is_valid=mask)
        call pf_corr(x, y, got(3), is_valid=mask, method="spearman")
        call check(error, all_close(got, R_NULLS), &
            "pairwise-complete: one pair in three excluded from BOTH samples")
        if (allocated(error)) return

        ! **The phase's own identities, and both must be EXACT.** `pf_cov(x, x)` reaching the same
        ! block tree as `pf_variance` is the whole reason `stats_pair_moments` lives in the core;
        ! `pf_corr(x, x)` being exactly 1 is why the three-sums equality is tested before the
        ! quotient is formed.
        call pf_cov(x, x, got(1))
        call pf_variance(x, v)
        call check(error, got(1) == v, "pf_cov(x, x) must be pf_variance(x) BIT FOR BIT")
        if (allocated(error)) return
        call pf_cov(x, x, got(1), ddof=0)
        call pf_variance(x, v, ddof=0)
        call check(error, got(1) == v, "and at ddof=0 as well")
        if (allocated(error)) return
        call pf_corr(x, x, got(2))
        call check(error, got(2) == 1.0_real64, "pf_corr(x, x) must be exactly 1")
        if (allocated(error)) return
        call pf_corr(x, x, got(2), method="spearman")
        call check(error, got(2) == 1.0_real64, "and exactly 1 under Spearman too")
        if (allocated(error)) return
        call pf_corr(x, -x, got(2))
        call check(error, got(2) == -1.0_real64, "a perfectly anti-correlated pair must be exactly -1")
    end subroutine test_two_sample_matches_the_oracle

    !> Spearman measures a monotone relationship Pearson cannot see, and handles ties.
    subroutine test_spearman_sees_monotone(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        real(real64) :: x(7), y(7), r
        integer :: i

        ! A strictly increasing but very non-linear relationship: Spearman is exactly 1, Pearson
        ! is not. This is the negative control for "spearman is a synonym for pearson".
        do i = 1, 7
            x(i) = real(i, real64)
            y(i) = real(i, real64)**5
        end do
        call pf_corr(x, y, r, method="spearman")
        call check(error, r == 1.0_real64, "a monotone relationship must give Spearman exactly 1")
        if (allocated(error)) return
        call pf_corr(x, y, r)
        call check(error, r < 0.95_real64, &
            "the control: Pearson must NOT see the same relationship as perfect")
        if (allocated(error)) return

        ! Ties, which are the only thing midranks exist for. Two ties in each sample; the expected
        ! value is scipy's, cross-checked in the oracle's --self-test.
        x = [1.0_real64, 2.0_real64, 2.0_real64, 3.0_real64, 4.0_real64, 4.0_real64, 5.0_real64]
        y = [9.0_real64, 8.0_real64, 8.0_real64, 5.0_real64, 3.0_real64, 1.0_real64, 1.0_real64]
        call pf_corr(x, y, r, method="spearman")
        ! scipy.stats.spearmanr on this pair; the oracle's --self-test compares its own midrank
        ! model against that same call, so this literal is checked from two sides.
        call check(error, close_to(r, -0.97222222222222232_real64), &
            "tied values must share a midrank, matching scipy.stats.spearmanr")
    end subroutine test_spearman_sees_monotone

    !> `pf_zscore` standardises, reports its exclusions, and has mean 0 and stddev 1.
    subroutine test_zscore_standardises(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        real(real64), allocatable :: x(:), z(:)
        logical, allocatable :: mask(:), ov(:)
        real(real64) :: m, sd, got(NZP)
        integer(int64) :: nnull, i
        integer :: k
        logical :: ok

        call golden_fixture(32_int64, x)
        allocate(z(32))
        call pf_zscore(x, z, ok=ok)
        call check(error, ok, "a clean population must standardise")
        if (allocated(error)) return
        do k = 1, NZP
            got(k) = z(G_ZPROBES(k))
        end do
        call check(error, all_close(got, Z_U32), "the standardised values must match the oracle")
        if (allocated(error)) return

        ! The defining property, asserted rather than assumed: mean 0 and standard deviation 1.
        call pf_mean(z, m)
        call pf_stddev(z, sd)
        call check(error, abs(m) < 1.0e-13_real64 .and. abs(sd - 1.0_real64) < 1.0e-13_real64, &
            "the result must have mean 0 and standard deviation 1")
        if (allocated(error)) return

        ! An excluded element: NaN in the output, marked in out_valid, counted in n_null, and ok
        ! comes back .false. -- one flag with two causes, which n_null separates.
        allocate(mask(32), ov(32))
        mask = .true.
        mask(5) = .false.
        mask(11) = .false.
        call pf_zscore(x, z, is_valid=mask, out_valid=ov, n_null=nnull, ok=ok)
        call check(error, (.not. ok) .and. nnull == 2_int64, &
            "an excluded element must give ok=.false. with n_null naming the cause")
        if (allocated(error)) return
        call check(error, (.not. ov(5)) .and. (.not. ov(11)) .and. count(ov) == 30, &
            "out_valid must mark exactly the excluded elements")
        if (allocated(error)) return
        call check(error, z(5) /= z(5) .and. z(11) /= z(11), &
            "and their outputs must be quiet NaNs, not stale or zero")
        if (allocated(error)) return
        do i = 1_int64, 32_int64
            if (mask(i)) then
                if (z(i) /= z(i)) then
                    call check(error, .false., "a surviving element must have a real z-score")
                    return
                end if
            end if
        end do

        ! A constant population has no spread, so EVERY output is undefined -- and `n_null` is 0,
        ! which is what tells the two causes of ok=.false. apart.
        x = 3.0_real64
        call pf_zscore(x, z, n_null=nnull, ok=ok)
        call check(error, (.not. ok) .and. nnull == 0_int64, &
            "a constant population gives ok=.false. with n_null zero")
        if (allocated(error)) return
        call check(error, all(z /= z), "and every output element is NaN")
    end subroutine test_zscore_standardises

    !> `pf_sigma_clipped_stats` reproduces astropy, on ONE ordering.
    subroutine test_sigma_clip_matches_astropy(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        real(real64), allocatable :: x(:)
        real(real64) :: got(3)
        integer(int64) :: nc
        logical :: conv, ok

        call golden_fixture_clip(400_int64, x)
        call pf_sigma_clipped_stats(x, got(1), got(2), got(3), n_clipped=nc, converged=conv, ok=ok)
        call check(error, all_close(got, C_DEF), "astropy's defaults: sigma=3, maxiters=5")
        if (allocated(error)) return
        call check(error, ok .and. conv, "the default run must converge with a usable answer")
        if (allocated(error)) return
        call check(error, nc >= 4_int64, "and must clip at least the four wild points")
        if (allocated(error)) return

        call pf_sigma_clipped_stats(x, got(1), got(2), got(3), cenfunc="mean")
        call check(error, all_close(got, C_MEANC), 'cenfunc="mean"')
        if (allocated(error)) return
        call pf_sigma_clipped_stats(x, got(1), got(2), got(3), stdfunc="mad_std")
        call check(error, all_close(got, C_MADSTD), 'stdfunc="mad_std"')
        if (allocated(error)) return
        call pf_sigma_clipped_stats(x, got(1), got(2), got(3), maxiters=0)
        call check(error, all_close(got, C_CONV), "maxiters<=0 iterates to convergence")
        if (allocated(error)) return
        call pf_sigma_clipped_stats(x, got(1), got(2), got(3), sigma_lower=1.5_real64, &
            sigma_upper=4.0_real64)
        call check(error, all_close(got, C_ASYM), "an asymmetric clip")
    end subroutine test_sigma_clip_matches_astropy

    !> A whole clipping run costs ONE ordering, whatever the number of rounds.
    !!
    !! This is P8's own acceptance gate. It is asserted with a counter rather than by inspection
    !! because the alternative implementation -- sort inside the loop -- gives the same numbers and
    !! differs only in cost, which no equality assertion can see.
    subroutine test_sigma_clip_costs_one_ordering(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        real(real64), allocatable :: x(:)
        real(real64) :: m, md, sd
        integer(int64) :: before, rounds_sorts
        logical :: conv

        call golden_fixture_clip(400_int64, x)
        call parquet_debug_reset_stats_sorts()
        before = parquet_debug_stats_sorts()
        call pf_sigma_clipped_stats(x, m, md, sd, maxiters=0, converged=conv)
        rounds_sorts = parquet_debug_stats_sorts() - before
        call check(error, rounds_sorts == 1_int64, &
            "a whole sigma-clipping run must order the population exactly once")
        if (allocated(error)) return
        call check(error, conv, "the control: the run must actually have iterated to convergence")
        if (allocated(error)) return

        ! `stdfunc="mad_std"` orders the DEVIATIONS once per round as well, which is honest: the
        ! deviations are a different population and their order is not implied by the values'.
        ! Asserted so that the extra cost is a stated property rather than a surprise.
        call parquet_debug_reset_stats_sorts()
        call pf_sigma_clipped_stats(x, m, md, sd, stdfunc="mad_std", maxiters=3)
        call check(error, parquet_debug_stats_sorts() > 1_int64, &
            'stdfunc="mad_std" orders the deviations too, once per round')
    end subroutine test_sigma_clip_costs_one_ordering

    !> `keep(:)` names exactly the survivors, and is what applies one clip to a second column.
    subroutine test_sigma_clip_keep_mask(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        real(real64), allocatable :: x(:)
        logical, allocatable :: kept(:), mask(:)
        real(real64) :: m, md, sd, m2, md2, sd2
        integer(int64) :: nc, i

        call golden_fixture_clip(400_int64, x)
        allocate(kept(400), mask(400))
        call pf_sigma_clipped_stats(x, m, md, sd, n_clipped=nc, keep=kept)
        call check(error, count(kept) == 400 - int(nc), &
            "keep must mark exactly the survivors n_clipped counts")
        if (allocated(error)) return
        call check(error, .not. (kept(7) .or. kept(123) .or. kept(200) .or. kept(365)), &
            "the four wild points must all be clipped")
        if (allocated(error)) return

        ! The mask must reproduce the same statistics when used as `is_valid` on a re-run with the
        ! clipping effectively disabled -- which is what a caller does with it on a second column.
        call pf_mean(x, m2, is_valid=kept)
        call pf_stddev(x, sd2, is_valid=kept, ddof=0)
        call pf_median(x, md2, is_valid=kept)
        call check(error, close_to(m2, m) .and. close_to(md2, md) .and. close_to(sd2, sd), &
            "reducing over keep= must reproduce the clipped statistics")
        if (allocated(error)) return

        ! A null must never be kept, whatever the clip decides about its value.
        mask = .true.
        do i = 1_int64, 400_int64, 10_int64
            mask(i) = .false.
        end do
        call pf_sigma_clipped_stats(x, m, md, sd, keep=kept, is_valid=mask)
        do i = 1_int64, 400_int64
            if (.not. mask(i) .and. kept(i)) then
                call check(error, .false., "an excluded element must never be marked as kept")
                return
            end if
        end do
        call check(error, count(kept) > 0, "and something must still survive")
    end subroutine test_sigma_clip_keep_mask

    !> The clip's own degenerate cases: empty, constant, and a round that would leave one value.
    subroutine test_sigma_clip_degenerate(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        real(real64) :: none(0), flat(9), two(2), m, md, sd
        integer(int64) :: nc
        logical :: ok, conv

        call pf_sigma_clipped_stats(none, m, md, sd, n_clipped=nc, ok=ok)
        call check(error, m /= m .and. md /= md .and. sd /= sd, &
            "an empty population gives three NaNs")
        if (allocated(error)) return
        call check(error, (.not. ok) .and. nc == 0_int64, "with ok=.false. and nothing clipped")
        if (allocated(error)) return

        ! A constant population has no spread, so no interval could remove anything. It must stop
        ! rather than loop, and must report the population it was given.
        flat = 5.0_real64
        call pf_sigma_clipped_stats(flat, m, md, sd, n_clipped=nc, ok=ok, converged=conv)
        call check(error, m == 5.0_real64 .and. md == 5.0_real64 .and. sd == 0.0_real64, &
            "a constant population is reported unchanged")
        if (allocated(error)) return
        call check(error, ok .and. conv .and. nc == 0_int64, &
            "and that is a converged, usable answer with nothing clipped")
        if (allocated(error)) return

        ! Two elements: a clip that would leave fewer than two stops instead and reports the pair.
        two = [1.0_real64, 100.0_real64]
        call pf_sigma_clipped_stats(two, m, md, sd, sigma=0.5_real64, n_clipped=nc, ok=ok)
        call check(error, nc == 0_int64 .and. ok, &
            "a round that would leave fewer than two values stops and keeps the previous set")
        if (allocated(error)) return
        call check(error, close_to(m, 50.5_real64), "and reports that set's statistics")
    end subroutine test_sigma_clip_degenerate

    !> Every P8 procedure reaches the same answer through all six input kinds.
    subroutine test_p8_kinds_agree(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        integer(int32) :: ix(24), iy(24)
        real(real32) :: rx(24), ry(24)
        real(real64) :: dx(24), dy(24), z32(24), z64(24)
        real(real64) :: a, b, m1, m2, s1, s2, d1, d2
        integer :: i

        do i = 1, 24
            ix(i) = int(mod(i * 37, 101) + 1, int32)
            iy(i) = int(mod(i * 53, 97) + 1, int32)
        end do
        rx = real(ix, real32)
        ry = real(iy, real32)
        dx = real(ix, real64)
        dy = real(iy, real64)

        call pf_cov(ix, iy, a)
        call pf_cov(dx, dy, b)
        call check(error, a == b, "pf_cov must widen int32 to exactly the real64 answer")
        if (allocated(error)) return
        call pf_cov(rx, ry, a)
        call check(error, a == b, "and real32 likewise")
        if (allocated(error)) return
        call pf_corr(ix, iy, a)
        call pf_corr(dx, dy, b)
        call check(error, a == b, "pf_corr must agree across kinds")
        if (allocated(error)) return
        call pf_gmean(ix, a)
        call pf_gmean(dx, b)
        call check(error, a == b, "pf_gmean must agree across kinds")
        if (allocated(error)) return
        call pf_zscore(ix, z32)
        call pf_zscore(dx, z64)
        call check(error, all(z32 == z64), "pf_zscore must agree across kinds")
        if (allocated(error)) return
        call pf_sigma_clipped_stats(ix, m1, d1, s1)
        call pf_sigma_clipped_stats(dx, m2, d2, s2)
        call check(error, m1 == m2 .and. d1 == d2 .and. s1 == s2, &
            "pf_sigma_clipped_stats must agree across kinds")
    end subroutine test_p8_kinds_agree

    !> The four properties P8's first mutation round found nothing asserting.
    !!
    !! Each of these was written because a deliberate defect **survived** the suite: the covariance
    !! losing its re-centring correction, a NaN in the SECOND sample no longer dropping the pair,
    !! the clip's lower bound ceasing to be inclusive, and `pf_gmean` reaching `log(0)`. All four
    !! answered plausibly without them, which is what a mutation round is for.
    subroutine test_p8_mutation_gaps(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        real(real64), allocatable :: x(:), y(:), xs(:), ys(:)
        real(real64) :: c0, c1, r0, r1, m, md, sd, v(4), g
        real(real64), parameter :: OFFSET = 1.0e9_real64
        real(real64) :: edge(9)
        integer(int64) :: nc, nn
        logical :: flag, saved, sup

        ! ---- 1. The covariance is SHIFT-INVARIANT, which is what the re-centring correction buys.
        !
        ! Offsetting both samples by 1e9 cannot change a covariance, and the textbook
        ! `sum(xy) - sum(x)sum(y)/n` loses almost every digit doing it. What this pins is the
        ! two-pass STRUCTURE. It does NOT pin the re-centring correction, and no test can: that
        ! term is a product of two rounding errors and is some twenty orders of magnitude below
        ! one ulp of the sum it corrects -- see `stats_pair_moments` for why it is kept anyway.
        call golden_fixture(64_int64, x)
        call golden_fixture_b(64_int64, y)
        allocate(xs(64), ys(64))
        xs = x + OFFSET
        ys = y + OFFSET
        call pf_cov(x, y, c0)
        call pf_cov(xs, ys, c1)
        ! `==`, not a tolerance: the two-pass form is shift-invariant to the LAST BIT here, and
        ! measuring that before writing the assertion is what turned a vague 1e-9 into a real one.
        call check(error, c1 == c0, &
            "pf_cov must be shift-invariant at an offset of 1e9, bit for bit")
        if (allocated(error)) return
        call pf_corr(x, y, r0)
        call pf_corr(xs, ys, r1)
        call check(error, r1 == r0, "and so must pf_corr")
        if (allocated(error)) return

        ! ---- 2. A NaN in EITHER sample drops the pair, not just one of them.
        y(9) = ieee_value(1.0_real64, ieee_quiet_nan)
        y(41) = ieee_value(1.0_real64, ieee_quiet_nan)
        call pf_cov(x, y, c0, n_nan=nn)
        call check(error, nn == 2_int64, &
            "a NaN in the SECOND sample must drop its pair and be counted")
        if (allocated(error)) return
        call check(error, c0 == c0, "and the covariance of what remains must be a number")
        if (allocated(error)) return
        ! The same two positions moved to the first sample must give the identical answer, since
        ! the rule is symmetric in the two samples.
        block
            real(real64), allocatable :: x2(:), y2(:)
            real(real64) :: c2
            call golden_fixture(64_int64, x2)
            call golden_fixture_b(64_int64, y2)
            x2(9) = ieee_value(1.0_real64, ieee_quiet_nan)
            x2(41) = ieee_value(1.0_real64, ieee_quiet_nan)
            call pf_cov(x2, y2, c2, n_nan=nn)
            call check(error, c2 == c0 .and. nn == 2_int64, &
                "the pairwise rule must be symmetric in the two samples")
        end block
        if (allocated(error)) return

        ! ---- 3. The clip's bounds are INCLUSIVE at both ends, as astropy's are.
        !
        ! Every value here is exactly representable and the mean is exactly 0 with a standard
        ! deviation of exactly 1, so `sigma = 1.5` puts the bounds exactly on the four extreme
        ! values. Inclusive keeps all nine; exclusive would keep the five zeros. A fixture whose
        ! boundary did not land on a value cannot tell the two apart at all.
        edge = [-1.5_real64, -1.5_real64, 0.0_real64, 0.0_real64, 0.0_real64, 0.0_real64, &
            0.0_real64, 1.5_real64, 1.5_real64]
        call pf_sigma_clipped_stats(edge, m, md, sd, sigma=1.5_real64, cenfunc="mean", &
            n_clipped=nc)
        call check(error, sd == 1.0_real64, &
            "the fixture's own precondition: mean 0 and standard deviation exactly 1")
        if (allocated(error)) return
        call check(error, nc == 0_int64, &
            "a value sitting exactly on the bound must be KEPT, as astropy keeps it")
        if (allocated(error)) return

        ! ---- 4. `pf_gmean` must not RAISE on a zero, which is the reason for its zero branch.
        !
        ! The branch changes no answer on a compiler that masks the IEEE traps -- `exp(-inf)` is
        ! already 0 -- so removing it survives every value assertion in this suite. What it changes
        ! is whether `log(0)` is reached at all, and under nagfor's default `-ieee=stop` that is
        ! the difference between an answer and a dead process. The flag is saved and restored, so a
        ! flag raised elsewhere in the run is neither hidden nor blamed on this call.
        sup = ieee_support_flag(ieee_divide_by_zero, 0.0_real64)
        if (sup) then
            call ieee_get_flag(ieee_divide_by_zero, saved)
            call ieee_set_flag(ieee_divide_by_zero, .false.)
        end if
        v = [2.0_real64, 0.0_real64, 8.0_real64, 16.0_real64]
        call pf_gmean(v, g)
        if (sup) then
            call ieee_get_flag(ieee_divide_by_zero, flag)
            call ieee_set_flag(ieee_divide_by_zero, saved .or. flag)
        else
            flag = .false.
        end if
        call check(error, g == 0.0_real64, "the control: a zero still gives exactly zero")
        if (allocated(error)) return
        call check(error, .not. flag, &
            "pf_gmean must not reach log(0); under nagfor's default traps that ends the process")
    end subroutine test_p8_mutation_gaps

    ! ==================================================================================
    ! P9 -- the cumulative family and the binning family
    ! ==================================================================================

    !> The four cumulative procedures against the 50-digit oracle, at five positions each.
    !!
    !! **Three of the four are asserted with `==` rather than a tolerance, and that is a property
    !! of the fixture worth knowing before it is weakened.** Every recipe value is an integer over
    !! 1024 with |x| < 512, so every partial SUM is an integer over 1024 below 2**24 -- exactly
    !! representable, so a correct running sum reproduces the oracle bit for bit, and so does a
    !! running maximum or minimum, which only ever selects. Only `pf_cumprod` accumulates rounding,
    !! and only it gets `close_to`.
    subroutine test_cumulative_matches_the_oracle(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        real(real64), allocatable :: x(:), out(:)
        integer :: k
        logical :: ok, same

        call golden_fixture(32_int64, x)
        allocate(out(32))

        call pf_cumsum(x, out, ok=ok)
        same = .true.
        do k = 1, NKP
            if (out(G_KPROBES(k)) /= K_SUM(k)) same = .false.
        end do
        call check(error, same, "the running sum must reproduce the oracle EXACTLY on this fixture")
        if (allocated(error)) return
        call check(error, ok, "nothing was excluded, so ok must be .true.")
        if (allocated(error)) return

        call pf_cummax(x, out)
        same = .true.
        do k = 1, NKP
            if (out(G_KPROBES(k)) /= K_MAX(k)) same = .false.
        end do
        call check(error, same, "the running maximum only ever selects, so it too is exact")
        if (allocated(error)) return

        call pf_cummin(x, out)
        same = .true.
        do k = 1, NKP
            if (out(G_KPROBES(k)) /= K_MIN(k)) same = .false.
        end do
        call check(error, same, "and so is the running minimum")
        if (allocated(error)) return

        call pf_cumprod(x, out)
        same = .true.
        do k = 1, NKP
            if (.not. close_to(out(G_KPROBES(k)), K_PROD(k))) same = .false.
        end do
        call check(error, same, &
            "the running product accumulates rounding, so it is the one checked to a tolerance")
    end subroutine test_cumulative_matches_the_oracle

    !> The family's null rule, in both directions, and the one case where a NaN DOES poison.
    !!
    !! Two statements that are one careless line apart: "the output element is undefined" and "the
    !! rest of the output is undefined". Only the first is meant, and the oracle's `K_SUM_NULL`
    !! row -- the running sum with every 7th element null -- is what separates them: a running
    !! value that restarted, or that carried the NaN forward, disagrees with it at every probe
    !! after the first null.
    subroutine test_cumulative_null_rule(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        real(real64), allocatable :: x(:), out(:)
        logical, allocatable :: v(:), ov(:)
        real(real64) :: nan, small(5), sout(5)
        integer(int64) :: i, nnull, nnan
        integer :: k
        logical :: ok, same

        nan = ieee_value(0.0_real64, ieee_quiet_nan)
        call golden_fixture(32_int64, x)
        allocate(out(32), v(32), ov(32))
        do i = 1_int64, 32_int64
            v(i) = mod(i, 7_int64) /= 0_int64
        end do

        call pf_cumsum(x, out, is_valid=v, out_valid=ov, n_null=nnull, ok=ok)
        same = .true.
        do k = 1, NKP
            if (out(G_KPROBES(k)) /= K_SUM_NULL(k)) same = .false.
        end do
        call check(error, same, "the running sum must carry PAST a null, unchanged and unrestarted")
        if (allocated(error)) return
        call check(error, nnull == 4_int64 .and. .not. ok, &
            "four nulls at 7, 14, 21 and 28, and ok=.false. because elements were excluded")
        if (allocated(error)) return
        call check(error, .not. ov(7) .and. .not. ov(14) .and. ov(8) .and. ov(1), &
            "out_valid must mark exactly the excluded positions")
        if (allocated(error)) return

        ! With `out_valid` absent the same element is a quiet NaN, which is the only way a
        ! `real(real64)` array can say "no value" -- and it is loud, since it propagates.
        call pf_cumsum(x, out, is_valid=v)
        call check(error, out(7) /= out(7) .and. out(8) == K_SUM_NULL(3) * 0.0_real64 + out(8), &
            "an excluded element is a quiet NaN when no mask was asked for")
        if (allocated(error)) return
        call check(error, out(8) == K_SUM_NULL(3), "and the element after it is unaffected")
        if (allocated(error)) return

        ! `skipnan = .false.` is the ONE case where a NaN poisons what follows: it is then an
        ! ordinary value, and this is `np.cumsum`/`skipna=False`. Checked on all four folds,
        ! because `cummax`/`cummin` reach it by a different branch -- `v > acc` is false for a
        ! NaN `v`, so without an explicit test the NaN is silently discarded. A probe found
        ! exactly that before this test existed.
        small = [1.0_real64, nan, 3.0_real64, 4.0_real64, 5.0_real64]
        call pf_cumsum(small, sout, skipnan=.false., n_nan=nnan, ok=ok)
        call check(error, sout(1) == 1.0_real64 .and. all(sout(2:) /= sout(2:)), &
            "a kept NaN poisons every later running SUM")
        if (allocated(error)) return
        call check(error, nnan == 0_int64 .and. ok, &
            "and nothing was EXCLUDED, so n_nan is 0 and ok stays .true.")
        if (allocated(error)) return
        call pf_cumprod(small, sout, skipnan=.false.)
        call check(error, all(sout(2:) /= sout(2:)), "a kept NaN poisons the running PRODUCT")
        if (allocated(error)) return
        call pf_cummax(small, sout, skipnan=.false.)
        call check(error, all(sout(2:) /= sout(2:)), "a kept NaN poisons the running MAXIMUM")
        if (allocated(error)) return
        call pf_cummin(small, sout, skipnan=.false.)
        call check(error, all(sout(2:) /= sout(2:)), "a kept NaN poisons the running MINIMUM")
        if (allocated(error)) return

        ! Skipped, the same NaN leaves the running value exactly where it was.
        call pf_cumsum(small, sout, n_nan=nnan, ok=ok)
        call check(error, sout(3) == 4.0_real64 .and. sout(5) == 13.0_real64, &
            "a skipped NaN contributes nothing and the sum carries past it")
        if (allocated(error)) return
        call check(error, nnan == 1_int64 .and. .not. ok, &
            "and it IS an exclusion, so n_nan is 1 and ok is .false.")
        if (allocated(error)) return

        ! A leading exclusion leaves nothing to carry: the fold starts at the first element that
        ! survives, rather than at an identity that a caller could hold as data.
        call pf_cummax([nan, 2.0_real64, 1.0_real64], sout(1:3))
        call check(error, sout(1) /= sout(1) .and. sout(2) == 2.0_real64 .and. &
            sout(3) == 2.0_real64, "a leading exclusion does not seed the fold with an identity")
    end subroutine test_cumulative_null_rule

    !> The documented divergence: the last cumulative sum is NOT `pf_sum`.
    !!
    !! A running sum is sequential by definition and `pf_sum` is pairwise, so the two agree only
    !! to within a naive sum's accumulated rounding. The doc-comment says so, and a user who does
    !! not read it will compare them; this test pins the claim in both directions -- they are
    !! close, and on a fixture built to make cancellation bite they need not be equal.
    subroutine test_cumsum_is_not_pf_sum(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        real(real64), allocatable :: x(:), out(:)
        real(real64) :: total
        integer(int64) :: i, n

        n = 4096_int64
        allocate(x(n), out(n))
        ! A large value first, then many small ones: the classic shape where a left-to-right sum
        ! loses the tail and a pairwise one does not.
        x(1) = 1.0e16_real64
        do i = 2_int64, n
            x(i) = 1.0_real64
        end do
        call pf_cumsum(x, out)
        call pf_sum(x, total)
        call check(error, close_to(out(n), total), &
            "the two must still agree to the golden tolerance -- this is rounding, not a defect")
        if (allocated(error)) return
        ! Not asserted as an inequality: whether they differ is a property of the compiler's
        ! rounding, and a test that REQUIRED a difference would be asserting a defect. What is
        ! asserted is that the pairwise total is the better of the two, which is the reason the
        ! doc-comment tells a caller to take the total from `pf_sum`.
        call check(error, abs(total - (1.0e16_real64 + real(n - 1_int64, real64))) <= &
            abs(out(n) - (1.0e16_real64 + real(n - 1_int64, real64))), &
            "pf_sum must be at least as close to the true total as the running sum is")
    end subroutine test_cumsum_is_not_pf_sum

    !> `pf_histogram` against `np.histogram` on the golden fixture -- P9's acceptance gate.
    subroutine test_histogram_matches_numpy(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        real(real64), allocatable :: x(:), w(:)
        real(real64) :: counts(NHB)
        integer(int64) :: nout
        logical :: ok

        call golden_fixture(32_int64, x)
        call pf_histogram(x, G_HEDGES, counts, n_outside=nout, ok=ok)
        call check(error, all(counts == H_LEFT), "np.histogram's counts, exactly")
        if (allocated(error)) return
        call check(error, nout == H_LEFT_OUT, "and the values it silently dropped")
        if (allocated(error)) return
        call check(error, .not. ok, "ok is .false. because some values reached no bin")
        if (allocated(error)) return

        call pf_histogram(x, G_HEDGES, counts, right=.true., n_outside=nout)
        call check(error, all(counts == H_RIGHT) .and. nout == H_RIGHT_OUT, &
            "and pd.cut's convention over the same fixture")
        if (allocated(error)) return

        call golden_weights_mod5(32_int64, w)
        call pf_histogram(x, G_HEDGES, counts, weights=w, n_outside=nout)
        call check(error, all(counts == H_WT), &
            "np.histogram(weights=) SUMS the weights rather than counting elements")
        if (allocated(error)) return
        call check(error, nout == H_WT_OUT, &
            "a zero weight leaves the population, so it is not an element that missed a bin")
    end subroutine test_histogram_matches_numpy

    !> The two conventions are mirror images: they differ only ON an edge, and cover it once.
    !!
    !! The golden fixture cannot show this -- no recipe value lands on a round edge, which is why
    !! `H_LEFT` and `H_RIGHT` are equal -- so the exact-boundary case exists for it. Every
    !! interior edge there IS a value, so a mixed-up convention moves a code and is caught.
    subroutine test_binning_conventions(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        integer(int32) :: codes(NHBX)
        real(real64) :: counts(NHBX - 2)
        integer(int64) :: nout
        integer :: k
        logical :: ok, covered

        call pf_bucketize(G_HBX, G_HBE, codes, n_outside=nout, ok=ok)
        call check(error, all(int(codes, kind=kind(HB_LEFT)) == HB_LEFT), &
            "the lower-closed codes: a value ON an interior edge joins the bin ABOVE it")
        if (allocated(error)) return
        call check(error, nout == HB_LEFT_OUT .and. .not. ok, "with two values outside the range")
        if (allocated(error)) return

        call pf_bucketize(G_HBX, G_HBE, codes, right=.true., n_outside=nout)
        call check(error, all(int(codes, kind=kind(HB_RIGHT)) == HB_RIGHT), &
            "the upper-closed codes: the same value joins the bin BELOW it")
        if (allocated(error)) return
        call check(error, nout == HB_RIGHT_OUT, "and the same two values are outside either way")
        if (allocated(error)) return

        ! The whole-family invariant, and the cheapest one available: under BOTH conventions the
        ! closed range [edges(1), edges(nbins+1)] is covered exactly once, so nothing is lost and
        ! nothing is double-counted. A convention that left a gap, or that closed both ends of an
        ! interior bin, fails here rather than only on a golden row.
        covered = .true.
        do k = 1, 2
            call pf_histogram(G_HBX, G_HBE, counts, right=(k == 2), n_outside=nout)
            if (nint(sum(counts)) + int(nout) /= NHBX) covered = .false.
        end do
        call check(error, covered, &
            "every value is either in exactly one bin or outside, under both conventions")
        if (allocated(error)) return
        call pf_histogram(G_HBX, G_HBE, counts)
        call check(error, all(nint(counts) == HB_LEFT_N), "the lower-closed bin counts")
        if (allocated(error)) return
        call pf_histogram(G_HBX, G_HBE, counts, right=.true.)
        call check(error, all(nint(counts) == HB_RIGHT_N), &
            "the upper-closed counts, which move a value between bins 1 and 2")
    end subroutine test_binning_conventions

    !> `pf_histogram` IS `pf_bucketize` followed by a tally, and the identity is asserted.
    !!
    !! Both reach the same `bin_of`, so this holds by construction -- which is exactly why it is
    !! worth a test: a second, separately written edge search inside the histogram would agree on
    !! every ordinary value and diverge on an edge, which no golden row over a round-edged fixture
    !! could see.
    subroutine test_histogram_is_bucketize_tallied(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        real(real64), allocatable :: x(:)
        real(real64) :: counts(NHB), tally(NHB), btally(NHBX - 2)
        integer(int32), allocatable :: codes(:)
        integer(int32) :: bcodes(NHBX)
        integer(int64) :: nout_h, nout_b
        integer :: k, arm
        logical :: agree

        call golden_fixture(32_int64, x)
        allocate(codes(32))
        agree = .true.
        do arm = 1, 2
            call pf_bucketize(x, G_HEDGES, codes, right=(arm == 2), n_outside=nout_b)
            call pf_histogram(x, G_HEDGES, counts, right=(arm == 2), n_outside=nout_h)
            do k = 1, NHB
                tally(k) = real(count(codes == int(k, int32)), real64)
            end do
            if (.not. all(tally == counts)) agree = .false.
            if (nout_b /= nout_h) agree = .false.
        end do
        call check(error, agree, &
            "the count in bin k must equal the number of codes equal to k, on both conventions")
        if (allocated(error)) return

        ! **The fixture above cannot see the failure this test exists for.** No recipe value lands
        ! on a round edge, and a second, separately written edge search would agree everywhere
        ! except ON one -- so the identity has to be re-asserted over the boundary case, where
        ! every interior edge IS a value.
        agree = .true.
        do arm = 1, 2
            call pf_bucketize(G_HBX, G_HBE, bcodes, right=(arm == 2), n_outside=nout_b)
            call pf_histogram(G_HBX, G_HBE, btally, right=(arm == 2), n_outside=nout_h)
            do k = 1, NHBX - 2
                if (real(count(bcodes == int(k, int32)), real64) /= btally(k)) agree = .false.
            end do
            if (nout_b /= nout_h) agree = .false.
        end do
        call check(error, agree, &
            "and it must still hold where every interior edge lands exactly on a value")
    end subroutine test_histogram_is_bucketize_tallied

    !> Every element the binning family could not place is reported, and the causes are separated.
    subroutine test_binning_counts_and_weights(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        real(real64) :: x(7), edges(3), counts(2), nan
        integer(int32) :: codes(7)
        logical :: v(7), ok
        integer(int64) :: nnull, nnan, nout

        nan = ieee_value(0.0_real64, ieee_quiet_nan)
        edges = [0.0_real64, 10.0_real64, 20.0_real64]
        x = [1.0_real64, nan, 5.0_real64, -3.0_real64, 15.0_real64, 99.0_real64, 12.0_real64]
        v = .true.
        v(3) = .false.

        call pf_bucketize(x, edges, codes, is_valid=v, n_null=nnull, n_nan=nnan, n_outside=nout, &
            ok=ok)
        call check(error, nnull == 1_int64 .and. nnan == 1_int64 .and. nout == 2_int64, &
            "one null, one NaN and two out of range, each counted under its own cause")
        if (allocated(error)) return
        call check(error, codes(2) == 0_int32 .and. codes(3) == 0_int32 .and. &
            codes(4) == 0_int32 .and. codes(6) == 0_int32, &
            "and every one of them answers 0 -- the 1-based spelling of pd.cut's -1")
        if (allocated(error)) return
        call check(error, .not. ok, "ok is .false. when anything failed to reach a bin")
        if (allocated(error)) return
        call check(error, codes(1) == 1_int32 .and. codes(5) == 2_int32 .and. codes(7) == 2_int32, &
            "while the three that did reach one are placed correctly")
        if (allocated(error)) return

        ! `skipnan = .false.` makes the NaN a VALUE, so it is no longer excluded -- but it still
        ! reaches no bin, because every comparison against it is false. Only the ACCOUNTING moves,
        ! which is the honest answer and is what the doc-comment states.
        call pf_bucketize(x, edges, codes, is_valid=v, skipnan=.false., n_nan=nnan, n_outside=nout)
        call check(error, nnan == 0_int64 .and. nout == 3_int64 .and. codes(2) == 0_int32, &
            "a kept NaN moves from n_nan to n_outside and still joins no bin")
        if (allocated(error)) return

        ! An entirely empty population is an ordinary data condition, not an error.
        call pf_histogram(x(1:0), edges, counts, n_outside=nout, ok=ok)
        call check(error, all(counts == 0.0_real64) .and. nout == 0_int64 .and. ok, &
            "an empty population gives zero counts, nothing outside, and ok=.true.")
    end subroutine test_binning_counts_and_weights

    !> Every P9 procedure answers the same over all six input kinds.
    !!
    !! The per-kind layer is generated, so a mistake here would be identical in all six specifics
    !! of one family rather than in one of them -- which is precisely what a cross-kind comparison
    !! cannot see. What it CAN see is a widening that lost a value or an argument wired to the
    !! wrong dummy, and both have happened in this module before.
    subroutine test_p9_kinds_agree(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        real(real64) :: xd(6), od(6), edges(4), cd(3), cx(3)
        real(real32) :: xs(6)
        integer(int32) :: xi(6), codes_d(6), codes_x(6)
        integer(int64) :: xl(6)
        logical :: xb(6)
        type(parquet_column) :: col
        real(real64) :: ox(6)
        integer :: k

        xd = [1.0_real64, 4.0_real64, 2.0_real64, 8.0_real64, 3.0_real64, 5.0_real64]
        xs = real(xd, real32)
        xi = int(xd, int32)
        xl = int(xd, int64)
        xb = xd > 3.0_real64
        edges = [0.0_real64, 3.0_real64, 6.0_real64, 9.0_real64]
        call col%init(PK_FLOAT64, 6_int64)
        do k = 1, 6
            call col%set_at(int(k, int64), xd(k))
        end do

        call pf_cumsum(xd, od)
        call pf_cumsum(xs, ox)
        call check(error, all(ox == od), "pf_cumsum over real32")
        if (allocated(error)) return
        call pf_cumsum(xi, ox)
        call check(error, all(ox == od), "pf_cumsum over int32")
        if (allocated(error)) return
        call pf_cumsum(xl, ox)
        call check(error, all(ox == od), "pf_cumsum over int64")
        if (allocated(error)) return
        call pf_cumsum(col, ox)
        call check(error, all(ox == od), "pf_cumsum over a parquet_column")
        if (allocated(error)) return
        ! The logical kind is 1 for .true. and 0 for .false., so its running sum is a count.
        call pf_cumsum(xb, ox)
        call check(error, ox(6) == real(count(xb), real64), "pf_cumsum over a logical array")
        if (allocated(error)) return

        call pf_cumprod(xi, ox)
        call check(error, ox(6) == 960.0_real64, "pf_cumprod over int32")
        if (allocated(error)) return
        call pf_cummax(xl, ox)
        call check(error, ox(6) == 8.0_real64, "pf_cummax over int64")
        if (allocated(error)) return
        call pf_cummin(xs, ox)
        call check(error, ox(6) == 1.0_real64, "pf_cummin over real32")
        if (allocated(error)) return

        call pf_bucketize(xd, edges, codes_d)
        call pf_bucketize(xi, edges, codes_x)
        call check(error, all(codes_x == codes_d), "pf_bucketize over int32")
        if (allocated(error)) return
        call pf_bucketize(col, edges, codes_x)
        call check(error, all(codes_x == codes_d), "pf_bucketize over a parquet_column")
        if (allocated(error)) return

        call pf_histogram(xd, edges, cd)
        call pf_histogram(xs, edges, cx)
        call check(error, all(cx == cd), "pf_histogram over real32")
        if (allocated(error)) return
        call pf_histogram(xl, edges, cx)
        call check(error, all(cx == cd), "pf_histogram over int64")
        if (allocated(error)) return
        call pf_histogram(col, edges, cx)
        call check(error, all(cx == cd), "pf_histogram over a parquet_column")
    end subroutine test_p9_kinds_agree

    !> `density=` against `np.histogram(density=True)`, and against its defining property.
    !!
    !! The golden rows pin the values; the integral pins the thing they are FOR. A normalisation
    !! taken over the wrong population -- everything passed, rather than everything binned -- makes
    !! every bin individually plausible and only the integral wrong, which is why both halves are
    !! here and why the fixture has values outside the edges for the base to get wrong.
    subroutine test_histogram_density(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        real(real64), allocatable :: x(:), w(:)
        real(real64) :: counts(NHB), uneven(4), ucounts(3), integral
        integer :: k
        logical :: ok

        call golden_fixture(32_int64, x)
        call pf_histogram(x, G_HEDGES, counts, density=.true.)
        call check(error, all_close(counts, H_DENS), "np.histogram(density=True)")
        if (allocated(error)) return

        integral = 0.0_real64
        do k = 1, NHB
            integral = integral + counts(k) * (G_HEDGES(k + 1) - G_HEDGES(k))
        end do
        call check(error, close_to(integral, 1.0_real64), &
            "a density integrates to 1 over the bins, which is what it is for")
        if (allocated(error)) return

        call golden_weights_mod5(32_int64, w)
        call pf_histogram(x, G_HEDGES, counts, density=.true., weights=w)
        call check(error, all_close(counts, H_DENS_WT), &
            "weighted, the base is the sum of the WEIGHTS that were binned")
        if (allocated(error)) return

        ! Uneven widths, where a formula that divided by one common width -- or by none -- still
        ! integrates to something and is wrong bin by bin.
        uneven = [-400.0_real64, -100.0_real64, 0.0_real64, 400.0_real64]
        call pf_histogram(x, uneven, ucounts, density=.true.)
        integral = 0.0_real64
        do k = 1, 3
            integral = integral + ucounts(k) * (uneven(k + 1) - uneven(k))
        end do
        call check(error, close_to(integral, 1.0_real64), &
            "and still integrates to 1 when the bins are of different widths")
        if (allocated(error)) return
        call check(error, ucounts(1) /= ucounts(3), &
            "an uneven spacing must give unequal densities for equal-ish counts")
        if (allocated(error)) return

        ! Nothing binned: numpy divides by zero and answers NaN, which is this module's own rule
        ! for an undefined result. Note this is where a density DIFFERS from a histogram -- the
        ! empty histogram just below is all zeros with ok=.true. and is perfectly well defined.
        call pf_histogram([-5000.0_real64, 5000.0_real64], G_HEDGES, counts, density=.true., ok=ok)
        call check(error, all(counts /= counts) .and. .not. ok, &
            "nothing binned means no density: quiet NaNs with ok=.false.")
        if (allocated(error)) return
        call pf_histogram([-5000.0_real64, 5000.0_real64], G_HEDGES, counts)
        call check(error, all(counts == 0.0_real64), &
            "the control: the same call without density= is zeros, not NaNs")
    end subroutine test_histogram_density

    !> `pf_bin_edges` against `np.histogram_bin_edges`, including both degenerate answers.
    subroutine test_bin_edges_matches_numpy(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        real(real64), allocatable :: x(:)
        real(real64) :: e(NBE + 1)
        logical, allocatable :: v(:)
        logical :: ok

        call golden_fixture(32_int64, x)
        call pf_bin_edges(x, NBE, e, ok=ok)
        call check(error, all_close(e, B_EDGES), "np.histogram_bin_edges(values, bins=5)")
        if (allocated(error)) return
        call check(error, ok, "a population with a real range reports ok=.true.")
        if (allocated(error)) return

        ! The range is the ONLY thing an exclusion can move, so the mask has to remove the
        ! population's own extremes -- at 1 and 11 for this recipe. A mask that misses them
        ! produces a row identical to the one above and asserts nothing, which is how the
        ! generator's first attempt at this fixture went.
        allocate(v(32))
        v = .true.
        v(1) = .false.
        v(11) = .false.
        call pf_bin_edges(x, NBE, e, is_valid=v)
        call check(error, all_close(e, B_EDGES_NULL), &
            "excluding the minimum and maximum moves the range the edges span")
        if (allocated(error)) return
        call check(error, e(1) /= B_EDGES(1) .and. e(NBE + 1) /= B_EDGES(NBE + 1), &
            "the control: the masked edges must actually differ from the unmasked ones")
        if (allocated(error)) return

        ! A zero weight removes an element from the population, so it must not stretch the range
        ! over data that `pf_histogram(weights=)` will not count either.
        call pf_bin_edges(x, NBE, e, weights=merge(0.0_real64, 1.0_real64, .not. v))
        call check(error, all_close(e, B_EDGES_NULL), &
            "and a zero WEIGHT removes it from the range for the same reason")
        if (allocated(error)) return

        call pf_bin_edges([7.0_real64, 7.0_real64, 7.0_real64, 7.0_real64, 7.0_real64], NBE, e, &
            ok=ok)
        call check(error, all_close(e, B_CONST) .and. .not. ok, &
            "a constant population widens by half a unit either way, as numpy does")
        if (allocated(error)) return
        call pf_bin_edges(x(1:0), NBE, e, ok=ok)
        call check(error, all_close(e, B_EMPTY) .and. .not. ok, &
            "and an empty one falls back to [0, 1], also as numpy does")
    end subroutine test_bin_edges_matches_numpy

    !> The contract that makes `pf_bin_edges` safe to chain: its edges are always usable.
    !!
    !! `pf_histogram` **aborts** on a pair that is not strictly increasing, so a degenerate
    !! population producing degenerate edges would turn a data condition into a crash one call
    !! later. Every branch is exercised here by actually feeding the result on, which is the only
    !! assertion that tests what the guarantee is for.
    subroutine test_bin_edges_are_always_usable(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        real(real64) :: e(NBE + 1), counts(NBE), tiny_pop(2)
        real(real64) :: e7(8), counts7(7), edgy(2)
        real(real64), allocatable :: x(:)
        integer(int64) :: nout7
        integer :: k
        logical :: ok, rising

        call golden_fixture(32_int64, x)
        ! Four populations with no range to describe, plus the ordinary one as a control.
        call pf_bin_edges(x, NBE, e)
        call pf_histogram(x, e, counts)
        call check(error, nint(sum(counts)) == 32, &
            "every value lands in a bin when the edges span the population's own range")
        if (allocated(error)) return

        call pf_bin_edges([7.0_real64, 7.0_real64], NBE, e)
        call pf_histogram([7.0_real64, 7.0_real64], e, counts)
        call check(error, nint(sum(counts)) == 2, "a constant population still bins")
        if (allocated(error)) return

        call pf_bin_edges(x(1:0), NBE, e)
        call pf_histogram(x(1:0), e, counts)
        call check(error, nint(sum(counts)) == 0, "and an empty one still produces usable edges")
        if (allocated(error)) return

        ! **The case the repair exists for**, and it is not exotic: a relative span of 1e-16 with
        ! five bins makes the spacing underflow, so the interior edges would all collapse onto the
        ! first. Reachable with ordinary nearly-constant data and a few thousand bins.
        tiny_pop = [1.0e300_real64, 1.0e300_real64 * (1.0_real64 + 1.0e-16_real64)]
        call pf_bin_edges(tiny_pop, NBE, e, ok=ok)
        rising = .true.
        do k = 2, NBE + 1
            if (.not. (e(k) > e(k - 1))) rising = .false.
        end do
        call check(error, rising, &
            "an nbins finer than the range can resolve still gives strictly increasing edges")
        if (allocated(error)) return
        call check(error, .not. ok, "and says so through ok=.false. rather than silently")
        if (allocated(error)) return
        ! The proof that "usable" means what it says: this is the call that would abort.
        call pf_histogram(tiny_pop, e, counts)
        call check(error, nint(sum(counts)) == 2, &
            "and pf_histogram accepts them, which is what the repair is for")
        if (allocated(error)) return

        ! **The top edge is ASSIGNED, not computed, and this fixture is why.** For these two
        ! values at seven bins, `lo + nbins*(hi - lo)/nbins` rounds to 810.10242086441235 --
        ! one ulp BELOW the maximum -- so a computed top edge would leave the population's own
        ! largest value outside every bin, silently, with `n_outside` reporting 1. A mutation
        ! replacing the assignment survives every other fixture here; it does not survive this
        ! one.
        edgy = [-906.83463876448741_real64, 810.10242086441258_real64]
        call pf_bin_edges(edgy, 7, e7)
        call check(error, e7(8) == edgy(2), &
            "the top boundary must be the maximum EXACTLY, not a rounded multiple of the span")
        if (allocated(error)) return
        call pf_histogram(edgy, e7, counts7, n_outside=nout7)
        call check(error, nint(sum(counts7)) == 2 .and. nout7 == 0_int64, &
            "so the largest value lands in the last bin rather than outside every one")
    end subroutine test_bin_edges_are_always_usable

    !> Whether two reals agree to the golden tolerance.
    logical function close_to(got, want)
        real(real64), intent(in) :: got  !! the library's answer.
        real(real64), intent(in) :: want !! the oracle's.
        real(real64), parameter :: TOL = 1.0e-12_real64
        close_to = abs(got - want) <= TOL * max(1.0_real64, abs(want))
    end function close_to

    !> Whether every element of two arrays agrees to the golden tolerance.
    logical function all_close(got, want)
        real(real64), intent(in) :: got(:)  !! the library's answers.
        real(real64), intent(in) :: want(:) !! the oracle's.
        integer :: k
        all_close = .true.
        do k = 1, size(got)
            if (.not. close_to(got(k), want(k))) all_close = .false.
        end do
    end function all_close

end module test_stats
