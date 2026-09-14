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
    ! NARROW import, not `use parquet`. The facade would compile just as well and would let a
    ! future test in this file reach the C++ layer without anything saying so; naming the tier
    ! makes that a build error instead. The facade's own re-export of this tier is pinned by
    ! `test_facade_covers_every_layer` (test/test_examples.f90), which is where that claim lives.
    use parquet_stats
    ! `pf_normal_scores`' expectations are `Phi**(-1)` of a plotting position this file forms
    ! itself. `parquet_stats` imports `pf_probit` with an `only:` list and is `private` by
    ! default, so it does not re-export it and this module names it directly -- which is also the
    ! point: the expectation must not come from the same generic the library reached through.
    use parquet_utils, only : pf_probit, pf_norm_cdf
    use parquet_random
    use parquet_columns
    use parquet_strings
    use test_stats_golden
    ! The 50-digit expectations for the probit family over the recipe fixture, from
    ! `tools/generate_probit_reference.py`. An `only:` list rather than the bare `use` that module
    ! invites, because it is `public` throughout and carries short names (`NP`, `NZ`, `P_GRID`)
    ! that a future test here could shadow without noticing.
    use test_probit_golden, only : NFIX, FIT_LOC, FIT_SIGMA, FIT_CORR, FIT_LOC_FILLIBEN, &
        FIT_SIGMA_FILLIBEN, FIT_CORR_FILLIBEN, SCALE_P25, SCALE_P16, PMEAN, PMEAN_W
    use iso_fortran_env, only : int32, int64, real32, real64
    use ieee_arithmetic, only : ieee_value, ieee_quiet_nan, ieee_positive_inf, ieee_get_flag, &
        ieee_set_flag, ieee_support_flag, ieee_divide_by_zero, ieee_invalid, ieee_is_nan
    use testdrive, only : new_unittest, unittest_type, error_type, check, skip_test
    implicit none
    private

    public :: collect_tests_parquet_stats

    !> How many scalar reductions `test_kind_sweep` covers. Declared once so its six per-kind
    !! arms, and the name table they share, cannot disagree about the count.
    integer, parameter :: NSCALAR = 17

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
            new_unittest("pf_count_valid's int32 form answers what the int64 form does", &
                test_count_valid_int32_form), &
            new_unittest("an empty slot in a %merge array is a no-op, not an abort", &
                test_merge_empty_slot_is_a_no_op), &
            new_unittest('pf_mad scale="normal" is one multiplication, never a division', &
                test_mad_normal_is_one_multiplication), &
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
            new_unittest("pf_normal_scores reads the ORDER alone, so a monotone transform " // &
                "changes nothing", test_normal_scores_is_order_only), &
            new_unittest("pf_normal_scores gives tied values one shared midrank score", &
                test_normal_scores_ties_share_a_score), &
            new_unittest("pf_normal_scores denominates by the survivor count, not the array " // &
                "size", test_normal_scores_uses_the_survivor_count), &
            new_unittest("pf_normal_scores marks its exclusions and writes NaN without the mask", &
                test_normal_scores_marks_exclusions), &
            new_unittest("every plotting-position token gives its own documented constant", &
                test_normal_scores_every_method_token), &
            new_unittest("pf_normal_scores' one-element, empty and all-tied populations", &
                test_normal_scores_degenerate), &
            new_unittest("the probit family matches the 50-digit oracle on the recipe fixture", &
                test_probit_family_matches_the_oracle), &
            new_unittest("pf_probit_fit's loc IS the mean of the survivors, for every token", &
                test_probit_fit_loc_is_the_mean), &
            new_unittest("pf_probit_fit recovers a Gaussian and its corr falls on a skewed " // &
                "sample", test_probit_fit_recovers_a_gaussian), &
            new_unittest("pf_probit_scale, pf_mad and pf_stddev agree on Gaussian data and " // &
                "diverge on a contaminated one", test_probit_scale_estimates_the_sigma), &
            new_unittest("pf_probit_fit's and pf_probit_scale's degenerate populations", &
                test_probit_fit_and_scale_degenerate), &
            new_unittest("the probit bindings answer off ONE ordering, bit for bit", &
                test_probit_bindings_read_tier_b), &
            new_unittest("pf_probit_mean is exactly 0 with a zero, exactly 1 with a one and " // &
                "NaN with both", test_probit_mean_boundary_rows), &
            new_unittest("pf_probit_mean's domain screen reads a NaN as a NaN", &
                test_probit_mean_domain_and_weights), &
            new_unittest("pf_sigma_clipped_stats reproduces astropy", &
                test_sigma_clip_matches_astropy), &
            new_unittest("a whole sigma-clipping run costs ONE ordering", &
                test_sigma_clip_costs_one_ordering), &
            new_unittest("the sigma clip's keep mask names exactly the survivors", &
                test_sigma_clip_keep_mask), &
            new_unittest("the sigma clip's empty, constant and two-element cases", &
                test_sigma_clip_degenerate), &
            new_unittest("every moment level returns bit-for-bit what level four returns", &
                test_moment_levels_are_bit_exact), &
            new_unittest("the P8 procedures agree between int32 and real32", &
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
                test_bin_edges_are_always_usable), &
            new_unittest("a hand-picked sample of the generated per-kind specifics is reached", &
                test_every_kind_specific_is_reached), &
            new_unittest("deferring pass one's compaction changes no answer, wherever the first " // &
                "exclusion falls", test_deferred_compaction_is_invisible), &
            new_unittest("EVERY generated entry point agrees across all six input kinds", &
                test_kind_sweep), &
            new_unittest("a non-finite value never aborts, and answers what numpy answers", &
                test_non_finite_values), &
            new_unittest("pf_bin_edges survives an infinity, and pf_histogram still accepts it", &
                test_bin_edges_survive_non_finite), &
            new_unittest("pf_histogram IS pf_bucketize tallied WITH WEIGHTS too", &
                test_weighted_bucketize_identity), &
            new_unittest("pf_count_valid reports why every element left", test_count_valid_why), &
            new_unittest("pf_moments and pf_describe report ok over the outputs asked for", &
                test_aggregate_ok), &
            new_unittest("%gmean and %hmean answer what the one-shot forms do", &
                test_object_power_means), &
            new_unittest("modes= returns every tied value, as pandas does", test_all_modes), &
            new_unittest("pf_cov(x,x) is pf_variance(x) under BOTH weight conventions", &
                test_cov_weight_type), &
            new_unittest("and still is when the centring does not come out exact", &
                test_cov_identity_survives_inexact_centring), &
            new_unittest("the streaming accuracy loss is bounded, and the two-pass route is exact", &
                test_streaming_accuracy_bound), &
            new_unittest("an infinity in either variable leaves pf_cov and pf_corr NaN, never aborting", &
                test_pair_non_finite), &
            new_unittest("a weighted pf_stats answers what the weighted one-shot forms answer", &
                test_weighted_object_order_statistics), &
            new_unittest("%update and %merge carry the weights, and fill in 1 for a batch " // &
                "without them", test_weighted_update_and_merge_keep_the_weights), &
            new_unittest("an armed but empty accumulator answers NaN rather than reading keep", &
                test_empty_retained_object_reads_are_nan), &
            new_unittest("a -0.0 weight is a zero weight in every weights= loop", &
                test_minus_zero_weight_is_a_zero_weight), &
            new_unittest("pf_mode reads weights over every kind, and answers an all-null " // &
                "population", test_mode_weights_and_all_null_across_kinds), &
            new_unittest("the streaming accumulator excludes and poisons as the two-pass " // &
                "route does", test_streaming_update_excludes_and_poisons), &
            new_unittest("the bias corrections refuse the sample sizes they are not defined for", &
                test_bias_corrections_refuse_small_samples), &
            new_unittest("an explicitly spelled default lands where leaving it out does", &
                test_explicit_defaults_match_the_implicit_ones), &
            new_unittest("a kept NaN poisons the general engine walk as it poisons the fast one", &
                test_kept_nan_poisons_the_general_engine_walk), &
            new_unittest("the binning family excludes what every other reduction excludes", &
                test_binning_excludes_nulls_and_nans), &
            new_unittest("pf_cov and pf_corr over two columns intersect their validity masks", &
                test_pairwise_columns_intersect_their_masks), &
            new_unittest("both signs of infinity, in either variable, leave the pair NaN", &
                test_pairwise_infinities_both_signs), &
            new_unittest("pf_probit_fit answers an infinite population with that infinity", &
                test_probit_fit_infinite_extremes), &
            new_unittest("inverted_cdf, and the exact tie nearest has to break", &
                test_selection_route_and_nearest_tie), &
            new_unittest("an absorbing weight keeps the quantile positions ordered", &
                test_absorbing_weight_keeps_positions_ordered), &
            new_unittest("a weight near huge does not overflow the levels that never square it", &
                test_weight_near_huge_survives_the_lower_levels), &
            new_unittest("no reduction raises an IEEE exception on a non-finite population", &
                test_no_reduction_raises_on_a_non_finite_population) &
            ]
    end subroutine collect_tests_parquet_stats

    !> Deferring pass one's compaction changes no answer, wherever the first exclusion falls.
    !!
    !! Pass one no longer materialises a compacted copy of the population when it has nothing to
    !! exclude: the survivors are then `values(1:m)` element for element, and pass two reads them
    !! there. The saving is real -- a whole population's allocation, write and free per call -- but
    !! it introduces a **transition**: at the first excluded element the survivors stop sitting at
    !! their own indices, so the prefix matched so far has to be copied across and the loop has to
    !! start writing from exactly the right place.
    !!
    !! That transition is the only new way this engine can be wrong, and it is an off-by-one on a
    !! prefix length, so this test drives the first NaN to **every position that can be special**:
    !! the very first element (an empty prefix, which is the `m > 0` guard), the second, the middle,
    !! the last, and none at all. Each is checked against a reference computed from the survivors
    !! directly. Per CLAUDE.md's rule about fixtures whose first element is the extreme case, the
    !! leading-NaN arm is deliberately first in the list rather than an afterthought at the end.
    !!
    !! `pf_stats` with `%compute` is included because it is the caller that asks for the survivors
    !! back: the buffer it receives must be the same whether or not pass one needed one for itself,
    !! and it is the one path that forces the deferred case to materialise after the fact.
    subroutine test_deferred_compaction_is_invisible(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        real(real64), allocatable :: x(:), clean(:), strided(:)
        real(real64) :: nan, v_ref, m_ref, v_got, m_got, med_ref, med_got
        integer(int64) :: n, i, k, pos(5), nvalid
        character(len=32) :: where_
        type(pf_stats) :: st

        nan = ieee_value(0.0_real64, ieee_quiet_nan)
        n = 4000_int64
        ! Positions for the FIRST NaN: leading (empty prefix), second, middle, last, and none.
        ! `n + 1` means "no NaN at all", which is the arm the deferral is actually written for.
        pos = [1_int64, 2_int64, n / 2_int64, n, n + 1_int64]

        do k = 1, 5
            allocate(x(n))
            do i = 1_int64, n
                x(i) = sin(real(i, real64) * 0.037_real64) * 600.0_real64
            end do
            if (pos(k) <= n) x(pos(k)) = nan
            write (where_, "(a,i0)") "first NaN at ", pos(k)

            ! The reference: the survivors, gathered by hand, reduced by the same library on a
            ! population that has nothing to exclude. Both arms therefore walk the same values in
            ! the same order, so the comparison is an exact one and not a tolerance.
            nvalid = 0_int64
            allocate(clean(n))
            do i = 1_int64, n
                if (.not. ieee_is_nan(x(i))) then
                    nvalid = nvalid + 1_int64
                    clean(nvalid) = x(i)
                end if
            end do
            call pf_variance(clean(1:nvalid), v_ref)
            call pf_mean(clean(1:nvalid), m_ref)
            call pf_median(clean(1:nvalid), med_ref)

            call pf_variance(x, v_got)
            call pf_mean(x, m_got)
            call pf_median(x, med_got)

            call check(error, v_got == v_ref, "variance must not depend on whether pass one " // &
                "compacted; " // trim(where_))
            if (allocated(error)) return
            call check(error, m_got == m_ref, "mean must not depend on whether pass one " // &
                "compacted; " // trim(where_))
            if (allocated(error)) return
            call check(error, med_got == med_ref, "median must not depend on whether pass one " // &
                "compacted; " // trim(where_))
            if (allocated(error)) return

            ! The retaining path: `%compute` keeps pass one's survivors, so the deferred case has
            ! to materialise the buffer it skipped. If the prefix copy were wrong, this is where a
            ! wrong survivor would show up rather than in a reduction that never reads one twice.
            call st%compute(x)
            call check(error, st%n_valid() == nvalid, "pf_stats must find the same survivor " // &
                "count as a hand gather; " // trim(where_))
            if (allocated(error)) return
            call check(error, st%variance() == v_ref, "a retaining pf_stats must return the " // &
                "reference variance; " // trim(where_))
            if (allocated(error)) return
            call check(error, st%median() == med_ref, "a retaining pf_stats orders the buffer it " // &
                "kept, so a wrong prefix copy surfaces here; " // trim(where_))
            if (allocated(error)) return

            deallocate(x, clean)
        end do

        ! A DISCONTIGUOUS actual argument takes the compacting path by construction -- the deferral
        ! is guarded by `is_contiguous`, because reading the caller's stride in pass two is not the
        ! same trade as reading a packed buffer. The guard is only correct if the answer survives
        ! it, which is what this arm checks.
        allocate(strided(2_int64 * n))
        do i = 1_int64, 2_int64 * n
            strided(i) = cos(real(i, real64) * 0.019_real64) * 400.0_real64
        end do
        allocate(clean(n))
        do i = 1_int64, n
            clean(i) = strided(2_int64 * i)
        end do
        call pf_variance(clean, v_ref)
        call pf_variance(strided(2::2), v_got)
        call check(error, v_got == v_ref, "a discontiguous population must return exactly what " // &
            "the same values return packed; the is_contiguous guard decides which path it takes")
    end subroutine test_deferred_compaction_is_invisible

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

    !> Every entry point asks the engine for only the central moments it needs. This asserts that
    !> doing so costs nothing in the answer.
    !!
    !! **Why an EXACT comparison and not a tolerance.** `stats_engine`'s `nmom` levels drop whole
    !! accumulators; they never change how a surviving one is summed, so each level's arithmetic is
    !! textually the same expression over the same elements in the same order, and the only way the
    !! result can move is if the compiler contracts (`a*b + c` into an FMA) differently in a loop
    !! carrying two accumulators than in one carrying four. That is exactly the failure this test
    !! exists to catch, and a tolerance would hide it -- see CLAUDE.md's note on restating a
    !! formula inline, which is the same hazard one step further along.
    !!
    !! `pf_moments` asking for all nine outputs is the level-4 reference, because its level is
    !! derived from its argument list. Each other entry point is compared against the output of
    !! that one call, over four populations that reach four different pass-one branches: plain,
    !! masked, weighted, and NaN-propagating.
    subroutine test_moment_levels_are_bit_exact(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        real(real64), allocatable :: v(:), w(:)
        logical, allocatable :: ok_mask(:)
        integer(int64) :: i, n
        integer :: shape_no

        n = 4099_int64
        call golden_fixture(n, v)
        allocate(w(n), ok_mask(n))
        do i = 1_int64, n
            w(i) = 0.25_real64 + real(mod(i, 17_int64), real64)
            ok_mask(i) = (mod(i, 5_int64) /= 0_int64)
        end do

        ! Four shapes, so the comparison covers the deferred fast loop and the general one.
        do shape_no = 1, 4
            select case (shape_no)
            case (1)
                call compare_levels(error, v)
            case (2)
                call compare_levels(error, v, is_valid=ok_mask)
            case (3)
                call compare_levels(error, v, weights=w)
            case (4)
                call compare_levels(error, v, is_valid=ok_mask, weights=w)
            end select
            if (allocated(error)) return
        end do
    end subroutine test_moment_levels_are_bit_exact

    !> One population, every entry point against the all-nine `pf_moments` call over the same data.
    subroutine compare_levels(error, v, is_valid, weights)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        real(real64), intent(in) :: v(:)                    !! the population.
        logical, intent(in), optional :: is_valid(:)        !! its validity mask, if any.
        real(real64), intent(in), optional :: weights(:)    !! its weights, if any.
        real(real64) :: r_mean, r_var, r_sd, r_sem, r_skew, r_kurt, r_sum, r_min, r_max, got
        integer(int64) :: r_nvalid

        ! The reference: all nine outputs asked for, so the engine runs at level 4.
        call pf_moments(v, is_valid=is_valid, weights=weights, n_valid=r_nvalid, mean=r_mean, &
            variance=r_var, stddev=r_sd, sem=r_sem, skewness=r_skew, kurtosis=r_kurt, &
            vsum=r_sum, vmin=r_min, vmax=r_max)

        call pf_sum(v, got, is_valid=is_valid, weights=weights)
        call check(error, got == r_sum, "pf_sum (level 0) must equal the level-4 vsum bit for bit")
        if (allocated(error)) return
        call pf_mean(v, got, is_valid=is_valid, weights=weights)
        call check(error, got == r_mean, "pf_mean (level 1) must equal the level-4 mean bit for bit")
        if (allocated(error)) return
        call pf_variance(v, got, is_valid=is_valid, weights=weights)
        call check(error, got == r_var, &
            "pf_variance (level 2) must equal the level-4 variance bit for bit")
        if (allocated(error)) return
        call pf_stddev(v, got, is_valid=is_valid, weights=weights)
        call check(error, got == r_sd, &
            "pf_stddev (level 2) must equal the level-4 stddev bit for bit")
        if (allocated(error)) return
        call pf_sem(v, got, is_valid=is_valid, weights=weights)
        call check(error, got == r_sem, "pf_sem (level 2) must equal the level-4 sem bit for bit")
        if (allocated(error)) return
        call pf_skewness(v, got, is_valid=is_valid, weights=weights)
        call check(error, got == r_skew, &
            "pf_skewness (level 3) must equal the level-4 skewness bit for bit")
        if (allocated(error)) return
        call pf_kurtosis(v, got, is_valid=is_valid, weights=weights)
        call check(error, got == r_kurt, &
            "pf_kurtosis (level 4) must equal the level-4 kurtosis bit for bit")
        if (allocated(error)) return

        ! The extremes and the count are pass-one products, so a reduced level must not disturb
        ! them either -- level 1 is the floor for pf_moments precisely so this holds.
        block
            real(real64) :: a_min, a_max
            integer(int64) :: a_n
            call pf_moments(v, is_valid=is_valid, weights=weights, n_valid=a_n, vmin=a_min, &
                vmax=a_max)
            call check(error, a_n == r_nvalid, "a vmin/vmax-only pf_moments must agree on n_valid")
            if (allocated(error)) return
            call check(error, a_min == r_min .and. a_max == r_max, &
                "a vmin/vmax-only pf_moments must return the level-4 extremes bit for bit")
        end block
    end subroutine compare_levels

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

    !> **`pf_count_valid`'s int32 and int64 forms answer the same numbers, over every value type.**
    !!
    !! `n` is REQUIRED, which is the only reason the pair can exist: a generic's specifics must be
    !! distinguishable, and two differing only in the kind of an OPTIONAL argument are rejected at
    !! the declaration. `n`'s kind therefore selects the specific and `n_null`/`n_nan` follow it.
    !! Every other generic in this module has nothing but optional integers and so cannot have this
    !! pair -- see feature_stats_int32.md.
    !!
    !! **What this cannot assert, and where that is enforced instead.** A MIXED call -- int32 `n`
    !! beside an int64 `n_null` -- matches neither specific and is a COMPILE error, so no runtime
    !! test can reach it; the language is the enforcement. And the narrowing guard in
    !! `stats_narrow_count` needs a population above `huge(int32)` elements, about 17 GB as real64,
    !! so it is unreachable here and is GCOVR-excluded at the source rather than chased.
    !!
    !! Every value type is exercised because each reaches the counting loop through its own
    !! generated specific, and a copy-paste slip in one would be invisible in the others. The
    !! fixture carries a null, a NaN and a zero weight so all three counters are non-zero -- with
    !! every count 0 the two kinds would agree for the wrong reason.
    subroutine test_count_valid_int32_form(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        integer, parameter :: N = 12
        real(real64) :: xr64(N), w(N)
        real(real32) :: xr32(N)
        integer(int32) :: xi32(N)
        integer(int64) :: xi64(N)
        logical :: xb(N), mask(N)
        type(parquet_column) :: col
        integer(int64) :: n64, null64, nan64
        integer(int32) :: n32, null32, nan32
        integer :: i

        do i = 1, N
            xr64(i) = real(i, real64)
            xr32(i) = real(i, real32)
            xi32(i) = int(i, int32)
            xi64(i) = int(i, int64)
            xb(i) = (mod(i, 2) == 0)
            w(i) = 1.0_real64
            mask(i) = .true.
        end do
        mask(3) = .false.               ! one null
        mask(4) = .false.               ! two, so n_null is not 1 by accident
        xr64(7) = ieee_value(0.0_real64, ieee_quiet_nan)
        xr32(7) = ieee_value(0.0_real32, ieee_quiet_nan)
        w(9) = 0.0_real64               ! one zero weight

        ! real64 -- the only type that has all three counters non-zero at once.
        call pf_count_valid(xr64, n64, is_valid=mask, weights=w, n_null=null64, n_nan=nan64)
        call pf_count_valid(xr64, n32, is_valid=mask, weights=w, n_null=null32, n_nan=nan32)
        call check(error, null64 == 2_int64 .and. nan64 == 1_int64 .and. n64 == 8_int64, &
            "this fixture must exclude two nulls, one NaN and one zero weight, leaving eight")
        if (allocated(error)) return
        call check(error, int(n32, int64) == n64 .and. int(null32, int64) == null64 .and. &
            int(nan32, int64) == nan64, "real64: the int32 form must answer what the int64 form does")
        if (allocated(error)) return

        call pf_count_valid(xr32, n64, is_valid=mask, n_null=null64, n_nan=nan64)
        call pf_count_valid(xr32, n32, is_valid=mask, n_null=null32, n_nan=nan32)
        call check(error, int(n32, int64) == n64 .and. int(null32, int64) == null64 .and. &
            int(nan32, int64) == nan64, "real32: the int32 form must answer what the int64 form does")
        if (allocated(error)) return

        ! The three kinds with no NaN to skip declare no n_nan at all, in either form.
        call pf_count_valid(xi32, n64, is_valid=mask, n_null=null64)
        call pf_count_valid(xi32, n32, is_valid=mask, n_null=null32)
        call check(error, int(n32, int64) == n64 .and. int(null32, int64) == null64, &
            "int32 values: the int32 form must answer what the int64 form does")
        if (allocated(error)) return

        call pf_count_valid(xi64, n64, is_valid=mask, n_null=null64)
        call pf_count_valid(xi64, n32, is_valid=mask, n_null=null32)
        call check(error, int(n32, int64) == n64 .and. int(null32, int64) == null64, &
            "int64 values: the int32 form must answer what the int64 form does")
        if (allocated(error)) return

        call pf_count_valid(xb, n64, is_valid=mask, n_null=null64)
        call pf_count_valid(xb, n32, is_valid=mask, n_null=null32)
        call check(error, int(n32, int64) == n64 .and. int(null32, int64) == null64, &
            "logical values: the int32 form must answer what the int64 form does")
        if (allocated(error)) return

        ! A parquet_column carries its own validity, so no is_valid= here -- passing one aborts.
        call col%init(PK_FLOAT64, int(N, int64))
        call col%set_all(xr64)
        call col%set_null(3_int64)
        call pf_count_valid(col, n64, n_null=null64, n_nan=nan64)
        call pf_count_valid(col, n32, n_null=null32, n_nan=nan32)
        call check(error, n64 > 0_int64, "the column fixture must leave something in the population")
        if (allocated(error)) return
        call check(error, int(n32, int64) == n64 .and. int(null32, int64) == null64 .and. &
            int(nan32, int64) == nan64, &
            "parquet_column: the int32 form must answer what the int64 form does")
        if (allocated(error)) return

        ! The bare form, with no optional counter at all: `n`'s kind alone must still resolve it.
        call pf_count_valid(xr64, n64)
        call pf_count_valid(xr64, n32)
        call check(error, int(n32, int64) == n64, &
            "with no optional counter, n's kind alone must select the specific")
    end subroutine test_count_valid_int32_form

    !> **An EMPTY source is folded as a no-op; only an UNCOMPUTED one aborts.** That distinction is
    !! what lets the documented threaded shape -- an array of accumulators sized to the team, folded
    !! with `%merge(part, consume=.true.)` -- survive a loop that never filled every slot.
    !!
    !! **The refusing half is covered and the permitting half was not, which is how this survived.**
    !! `stats_object_merge_uncomputed_source` asserts that merging an accumulator nothing has been
    !! done to aborts, so the pair *looks* tested; but every `%merge` call site in this file passed
    !! a source holding a real population, and `test_merge_many_recomputes_once` above fills all
    !! four of its slots. Delete `stats_merge_worker`'s `if (other%n_seen /= 0_int64)` guard
    !! (src/parquet_stats_core.f90) and no other named test fails.
    !!
    !! Three things are asserted, and the second and third are the controls. The fold must not
    !! abort -- reaching the first `check` is that proof. `%n()` must be the three FILLED thirds'
    !! total, so an empty slot that quietly contributed something is caught. And the answers must
    !! equal `%compute` over the concatenation **bit for bit**, which the page promises for a
    !! retained merge, so `==` is right here and a tolerance would not be.
    subroutine test_merge_empty_slot_is_a_no_op(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        real(real64), allocatable :: v(:)
        real(real64) :: got(NQ), want(NQ)
        type(pf_stats) :: part(4), total, whole, blank
        integer :: q, k
        integer(int64) :: lo, hi

        call golden_fixture(999_int64, v)
        call whole%compute(v)
        call object_row(whole, want)

        ! Slots 1, 3 and 4 hold thirds of the fixture; slot 2 is %init()ed and left EMPTY --
        ! computed, holding no population, which is exactly what a short loop leaves behind.
        do k = 1, 3
            lo = (k - 1) * 333_int64 + 1_int64
            hi = k * 333_int64
            if (k == 1) call part(1)%compute(v(lo:hi))
            if (k == 2) call part(3)%compute(v(lo:hi))
            if (k == 3) call part(4)%compute(v(lo:hi))
        end do
        call part(2)%init()
        call check(error, part(2)%is_computed() .and. part(2)%n() == 0_int64, &
            "this fixture is only meaningful if slot 2 is COMPUTED and EMPTY, not uncomputed")
        if (allocated(error)) return

        call total%init()
        call total%merge(part, consume=.true.)
        call check(error, total%n() == 999_int64, &
            "an empty slot must contribute nothing: %n() must be the three filled thirds' total")
        if (allocated(error)) return

        call object_row(total, got)
        do q = 1, NQ
            call check(error, got(q) == want(q), &
                "a fold over an empty slot must equal %compute over the concatenation: " // &
                trim(QNAME(q)))
            if (allocated(error)) return
        end do

        ! `stats_invalidate_order` runs BEFORE the n_seen guard, so even a fold whose only source
        ! is empty drops the destination's ordering. Asserted because nothing else states it and a
        ! "no-op" that skipped the invalidation would leave a stale median behind.
        call whole%prepare_order()
        call check(error, whole%is_ordered(), "this control needs the destination ordered first")
        if (allocated(error)) return
        call blank%init()
        call whole%merge(blank)
        call check(error, .not. whole%is_ordered(), &
            "merging an EMPTY source must still drop the destination's order cache")
    end subroutine test_merge_empty_slot_is_a_no_op

    !> **`scale="normal"` is ONE multiplication by `1/Phi^-1(3/4)`, never a division by
    !! `Phi^-1(3/4)`.** The two are not the same operation in binary floating point -- measured, they
    !! disagree by one ulp on about 39% of inputs -- and this file's two scaling sites used to
    !! differ, one multiplying by a reciprocal and the other dividing.
    !!
    !! **What this can and cannot see.** It pins `resolve_mad_scale`, behind `pf_mad`/`%mad`:
    !! restore a division there and this fails, because `raw / 0.6744897501960817` is not
    !! `raw * 1.4826022185056018` for most values. It CANNOT see `slice_mad_std`, the other site --
    !! that value is the clip scale inside `pf_sigma_clipped_stats` and never leaves the procedure
    !! (the `stddev` it reports comes from `slice_mean_sd`), so no public observable exists for it.
    !! Both now multiply by the shared `MAD_NORMAL_SCALE`; that half is held by the constant being
    !! shared, not by this test.
    subroutine test_mad_normal_is_one_multiplication(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        real(real64), parameter :: SCALE = 1.4826022185056018_real64 !! 1/Phi^-1(3/4), correctly rounded.
        real(real64), allocatable :: v(:)
        real(real64) :: raw, normal

        ! 501 values, so the median is an element rather than a mean of two -- the scaling is what
        ! is under test, and an interpolated median would add a rounding of its own.
        call golden_fixture(501_int64, v)
        call pf_mad(v, raw, scale="raw")
        call pf_mad(v, normal, scale="normal")
        call check(error, raw > 0.0_real64, "this fixture needs a non-zero raw deviation")
        if (allocated(error)) return
        ! `==`, not a tolerance: the point is WHICH operation was applied, and a tolerance would
        ! pass against the division this test exists to forbid.
        call check(error, normal == raw * SCALE, &
            'pf_mad(scale="normal") must be the raw deviation times 1.4826022185056018 exactly')
        if (allocated(error)) return
        call check(error, normal /= raw / 0.6744897501960817_real64 .or. &
            raw * SCALE == raw / 0.6744897501960817_real64, &
            "this test is vacuous unless multiply and divide can differ on this fixture")
    end subroutine test_mad_normal_is_one_multiplication

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
    !!
    !! **The accumulator is `volatile`, and without it this helper does not compute what its own
    !! name says.** A bare `res = res + x(i)` reduction is precisely the shape a compiler licensed
    !! to reassociate splits into several partial accumulators and adds together at the end -- ifx
    !! does it at its own `-O2` / `-fp-model=fast` defaults, and gfortran does under `-ffast-math`.
    !! That is itself a pairwise-like order, so the "naive" reference silently stops being naive:
    !! measured under ifx 2026.1.1, this function returned `1.0000000000000096e16` over the
    !! 100-element `spiky` fixture where the true left-to-right total is exactly `1e16`, and the
    !! first assertion of `test_block_tree_is_the_documented_one` failed against a correct engine.
    !! `volatile` obliges a store and a reload per iteration, so the additions happen in the
    !! written order on every conforming compiler -- the same barrier, for the same reason, as
    !! `parquet_expkey`'s `ek_rnd`.
    function running_total(x) result(res)
        real(real64), intent(in) :: x(:) !! the population.
        real(real64) :: res              !! the naive total.
        real(real64), volatile :: acc    !! the barrier: forces the additions to happen in order.
        integer(int64) :: i

        acc = 0.0_real64
        do i = 1_int64, size(x, kind=int64)
            acc = acc + x(i)
        end do
        res = acc
    end function running_total

    !> The engine's own decomposition: serial sums per block, combined by a fixed pairwise tree.
    !!
    !! **`s` is `volatile` for the reason spelled out on `running_total` above**, and it matters
    !! here too: a block's sum is a serial left-to-right one inside the engine, so a reassociated
    !! block sum would compare the engine against a tree it does not walk. Under ifx it made this
    !! function return the same bits as `running_total` over the 1000-element fixture, which trips
    !! the "the two summation orders disagree" vacuity guard rather than the assertion it protects.
    function pairwise_by_blocks(x, blk) result(res)
        real(real64), intent(in) :: x(:) !! the population.
        integer(int64), intent(in) :: blk !! elements per block.
        real(real64) :: res               !! the combined total.
        real(real64), allocatable :: p(:)
        integer(int64) :: n, nb, j, i, lo, hi, m, k
        real(real64), volatile :: s       !! one block's serial sum; `volatile` per the note above.

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
            "measured constant has been lowered and bench/benchmark_stats.sh --mode=teamsweep " // &
            "should be re-run before trusting the new value -- NOT --mode=thread, which measures " // &
            "a replica over a resident buffer and reports a ceiling this constant cannot use")
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
    !! threshold puts every test on the same side of it. CLAUDE.md's size-threshold rule is what
    !! this exists for, and `parquet_debug_set_stats_quantile_sort_min` is what reaches the other
    !! side.
    !!
    !! **`QUANTILE_SORT_MIN` is now 1, so the selection route never runs by default and THIS TEST
    !! is the only thing that exercises it at all.** That is deliberate rather than dead code --
    !! see the constant's own comment -- so do not delete either the route or this test on the
    !! grounds that nothing reaches it: the override is how the question gets re-asked on a machine
    !! with a different core count, and an unexercised route could not be trusted when it is.
    subroutine test_selection_and_sort_paths_agree(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        real(real64), allocatable :: x(:)
        real(real64) :: sel(NQP), srt(NQP), sel_icdf, srt_icdf, sel_one, srt_one
        integer :: k

        call golden_fixture(64_int64, x)
        ! A huge floor: every count is below it, so every probe SELECTS.
        call parquet_debug_set_stats_quantile_sort_min(1000000_int64)
        call pf_quantiles(x, G_QPROBS, sel, method="linear")
        ! `inverted_cdf` and a one-element population are SEPARATE arms inside the selection
        ! route -- a clamped `ceil(p*m)` handed to `pf_nth_element`, and a direct return -- so
        ! neither is reached by the "linear" probes above, and the override is the only way to
        ! reach either at all. They ride this test rather than one of their own because the
        ! override is process-global and this suite runs its tests concurrently: one writer.
        call pf_quantile(x, 0.5_real64, sel_icdf, method="inverted_cdf")
        call pf_quantile(x(1:1), 0.3_real64, sel_one)
        ! A floor of zero: nothing is below it, so the population is ORDERED once.
        call parquet_debug_set_stats_quantile_sort_min(0_int64)
        call pf_quantiles(x, G_QPROBS, srt, method="linear")
        call pf_quantile(x, 0.5_real64, srt_icdf, method="inverted_cdf")
        call pf_quantile(x(1:1), 0.3_real64, srt_one)
        call parquet_debug_set_stats_quantile_sort_min(-1_int64)

        call check(error, sel_icdf == srt_icdf, &
            "the two routes disagree on inverted_cdf, which each implements in its own way")
        if (allocated(error)) return
        call check(error, sel_one == x(1) .and. srt_one == x(1), &
            "a one-element population is that element on both routes")
        if (allocated(error)) return

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

        ! Every element excluded: ok=.false. and count=0.
        !
        ! `m` is deliberately NOT asserted here. It is `intent(out)` and `mode_i32` returns without
        ! assigning it on this path, so F2018 leaves it undefined -- and pf_mode's own doc-comment
        ! says so: "An empty population gives ok = .false. and count = 0, and `m` must not be
        ! read." This test used to assert `m == -99_int32` after seeding it, i.e. it asserted the
        ! opposite of the documented contract. gfortran, ifx and flang all leave the caller's prior
        ! value in place, so it passed everywhere; nagfor's -C=undefined reports it as a reference
        ! to an undefined variable, which is exactly right. `ok` is the flag a caller is supposed
        ! to branch on, and it is what is asserted instead.
        m = -99_int32
        mask = .false.
        call pf_mode(pop, m, count=cnt, is_valid=mask, ok=ok, n_null=nnull)
        call check(error, (.not. ok) .and. cnt == 0_int64, "an empty population must not be ok")
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
        real(real64) :: v(9), r, ns(9), fsig, fcor
        type(pf_stats) :: s
        integer :: k
        logical :: ok, ov(9)

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
        ! **Risk-253, and the vector-valued case is the one that would slip through.** A NaN
        ! sorts to one end of the ordering and poisons NO rank, so the other eight elements would
        ! come back with perfectly ordinary scores unless `saw_nan` is tested outright.
        call pf_normal_scores(v, ns, skipnan=.false., ok=ok)
        call check(error, all(ns /= ns) .and. .not. ok, &
            "and pf_normal_scores, where EVERY score must be NaN and not just the NaN's own")
        if (allocated(error)) return
        call pf_probit_fit(v, r, fsig, fcor, skipnan=.false., ok=ok)
        call check(error, r /= r .and. fsig /= fsig .and. fcor /= fcor .and. .not. ok, &
            "and pf_probit_fit, in all THREE of its outputs")
        if (allocated(error)) return
        call pf_probit_scale(v, r, skipnan=.false., ok=ok)
        call check(error, r /= r .and. .not. ok, "and pf_probit_scale")
        if (allocated(error)) return
        ! `pf_probit_mean` reads its population as probabilities, so the NaN travels in one.
        call pf_probit_mean([0.2_real64, ieee_value(1.0_real64, ieee_quiet_nan), 0.7_real64], &
            r, skipnan=.false., ok=ok)
        call check(error, r /= r .and. .not. ok, &
            "and pf_probit_mean, where the NaN must not be read as an out-of-domain value")
        if (allocated(error)) return

        ! The object side takes the same route through its own guard.
        call s%compute(v, skipnan=.false.)
        call check(error, s%median() /= s%median(), "%median must propagate it")
        if (allocated(error)) return
        call check(error, s%mad() /= s%mad(), "%mad must propagate it")
        if (allocated(error)) return
        call s%probit_fit(r, fsig, fcor)
        call check(error, r /= r .and. fsig /= fsig .and. fcor /= fcor, &
            "%probit_fit must propagate it")
        if (allocated(error)) return
        call check(error, s%probit_scale() /= s%probit_scale(), "%probit_scale must propagate it")
        if (allocated(error)) return

        ! The negative control: at the default `skipnan=.true.` every one of these is a number.
        call pf_median(v, r, ok=ok)
        call check(error, r == r .and. ok, &
            "the control: at the default skipnan the NaN is merely excluded")
        if (allocated(error)) return
        call pf_mad(v, r, ok=ok)
        call check(error, r == r .and. ok, "and pf_mad answers a number too")
        if (allocated(error)) return
        ! The second half, without which a procedure returning NaN unconditionally would pass.
        ! `ok` is .false. here because the NaN was EXCLUDED, which is `pf_zscore`'s rule -- but
        ! the eight survivors must all have real scores.
        call pf_normal_scores(v, ns, out_valid=ov)
        call check(error, count(ov) == 8 .and. .not. ov(4) .and. &
            all(ns == ns .or. .not. ov), &
            "and pf_normal_scores scores the eight survivors, excluding only the NaN")
        if (allocated(error)) return
        call pf_probit_fit(v, r, fsig, fcor, ok=ok)
        call check(error, r == r .and. fsig == fsig .and. fcor == fcor .and. ok, &
            "and pf_probit_fit fits the eight survivors")
        if (allocated(error)) return
        call pf_probit_mean([0.2_real64, ieee_value(1.0_real64, ieee_quiet_nan), 0.7_real64], &
            r, ok=ok)
        call check(error, r == r .and. ok, &
            "and pf_probit_mean averages the two survivors rather than refusing the population")
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

    !> The scores read the ORDER and nothing else, so any strictly monotone transform is invisible.
    !!
    !! **This is the strongest structural assertion this family admits, and it needs no
    !! tolerance.** A score computed from the VALUE where the rank belongs -- the single most
    !! natural way to write this procedure wrongly -- fails it on the first transform, as does
    !! standardising the values first and mapping those through `Phi**(-1)`. Two transforms of
    !! different character are used because an affine one would be passed by a z-score as well,
    !! and would therefore prove nothing.
    subroutine test_normal_scores_is_order_only(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        real(real64), allocatable :: x(:), t(:), s0(:), s1(:), s2(:), s3(:)
        integer(int64) :: i

        call golden_fixture(64_int64, x)
        allocate(t(64), s0(64), s1(64), s2(64), s3(64))

        call pf_normal_scores(x, s0)
        ! `exp` over a hundredth of the fixture: strictly increasing, no overflow at this range,
        ! and it stretches the spacing by five orders of magnitude across the array.
        do i = 1_int64, 64_int64
            t(i) = exp(x(i) / 100.0_real64)
        end do
        call pf_normal_scores(t, s1)
        call check(error, all(s0 == s1), &
            "an exponential of the values must give bit-identical scores")
        if (allocated(error)) return

        ! `atan`: strictly increasing and BOUNDED, so it compresses where `exp` stretched.
        do i = 1_int64, 64_int64
            t(i) = atan(x(i))
        end do
        call pf_normal_scores(t, s2)
        call check(error, all(s0 == s2), "and so must an arctangent of them")
        if (allocated(error)) return

        ! Reversing the order negates every score, because the plotting positions are symmetric
        ! about 1/2 and `pf_probit` is antisymmetric about it. Not bit-for-bit: `1 - p` is an
        ! exact double only for `p >= 1/2`, so half the pairs meet across one rounding.
        do i = 1_int64, 64_int64
            t(i) = -x(i)
        end do
        call pf_normal_scores(t, s3)
        call check(error, maxval(abs(s3 + s0)) < 1.0e-14_real64, &
            "reversing the order must negate every score")
        if (allocated(error)) return

        ! The vacuity guard. Without it the three assertions above would all pass against a
        ! procedure that returned the same constant for every element.
        call check(error, maxval(s0) - minval(s0) > 1.0_real64, &
            "this test is vacuous unless the scores actually vary across the population")
    end subroutine test_normal_scores_is_order_only

    !> Ties share ONE score, and it is the midrank's rather than any ordinal position's.
    subroutine test_normal_scores_ties_share_a_score(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        real(real64) :: x(7), s(7), want, ordinal
        real(real64), parameter :: A = 0.375_real64  !! "blom", the default.

        ! Positions 2, 3 and 4 are one tie, so every one of them takes the midrank 3.
        x = [1.0_real64, 5.0_real64, 5.0_real64, 5.0_real64, 9.0_real64, 11.0_real64, 13.0_real64]
        call pf_normal_scores(x, s)
        call check(error, s(2) == s(3) .and. s(3) == s(4), &
            "the three tied elements must receive one identical score")
        if (allocated(error)) return

        ! The midrank of a run spanning sorted positions 2..4 is 3, and the score is
        ! `Phi**(-1)((3 - 3/8)/(7 + 1 - 3/4))`. Computed here from the definition rather than read
        ! from the library, so a midrank replaced by an ordinal rank fails rather than agreeing
        ! with itself.
        want = pf_probit((3.0_real64 - A) / (7.0_real64 + 1.0_real64 - 2.0_real64 * A))
        call check(error, s(2) == want, "and it must be the MIDRANK's score, to the bit")
        if (allocated(error)) return

        ! The vacuity guard: an ordinal rank would put the first of the run at position 2, and
        ! this test is worth nothing unless that is a different number.
        ordinal = pf_probit((2.0_real64 - A) / (7.0_real64 + 1.0_real64 - 2.0_real64 * A))
        call check(error, ordinal /= want, &
            "this test is vacuous unless an ordinal rank would give a different score")
        if (allocated(error)) return

        ! The untied elements keep the positions they always had: 1 and 5, 6, 7.
        call check(error, s(1) < s(2) .and. s(2) < s(5) .and. s(5) < s(6) .and. s(6) < s(7), &
            "the untied elements must stay strictly ordered around the tie")
    end subroutine test_normal_scores_ties_share_a_score

    !> **`feature_risks.md` Risk-254.** The ranks are the SURVIVORS', not the whole array's.
    !!
    !! Denominating by `size(values)` shrinks every score toward zero by roughly `m/n`: the scores
    !! stay ordered, stay centred on zero and stay monotone in the values, so every distributional
    !! check still passes and only the SCALE is wrong -- and only when something is excluded. The
    !! assertion that sees it is bit-for-bit identity with the sub-population computed alone.
    subroutine test_normal_scores_uses_the_survivor_count(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        real(real64), allocatable :: x(:), sub(:), s(:), s_sub(:)
        logical, allocatable :: mask(:)
        integer(int64) :: i, k
        integer :: nkept

        call golden_fixture(40_int64, x)
        allocate(mask(40), s(40))
        ! Every third element excluded: 27 survive out of 40, so `m/n` is far from 1 and a
        ! denominator taken from the array size would be visibly wrong.
        do i = 1_int64, 40_int64
            mask(i) = (mod(i, 3_int64) /= 0_int64)
        end do
        nkept = count(mask)
        allocate(sub(nkept), s_sub(nkept))
        k = 0_int64
        do i = 1_int64, 40_int64
            if (mask(i)) then
                k = k + 1_int64
                sub(k) = x(i)
            end if
        end do

        call pf_normal_scores(x, s, is_valid=mask)
        call pf_normal_scores(sub, s_sub)
        k = 0_int64
        do i = 1_int64, 40_int64
            if (mask(i)) then
                k = k + 1_int64
                if (s(i) /= s_sub(k)) then
                    call check(error, .false., &
                        "a survivor's score must equal its score in the sub-population alone")
                    return
                end if
            end if
        end do
        call check(error, nkept == 27, "the mask must exclude enough to make the scales differ")
        if (allocated(error)) return

        ! The vacuity guard, and it is the whole point: had the denominator been `n`, the scores
        ! would still have been ordered and centred -- they would simply have been these ones
        ! multiplied down. So assert that the two denominators genuinely disagree here.
        call check(error, abs(maxval(s_sub)) > 1.0e-3_real64 + &
            abs(pf_probit((real(nkept, real64) - 0.375_real64) / &
                          (40.0_real64 + 1.0_real64 - 0.75_real64))), &
            "this test is vacuous unless denominating by the array size would give a " // &
            "different extreme score")
    end subroutine test_normal_scores_uses_the_survivor_count

    !> An excluded element is marked in `out_valid`, and is a quiet NaN when the mask is absent.
    subroutine test_normal_scores_marks_exclusions(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        real(real64), allocatable :: x(:), s(:)
        logical, allocatable :: mask(:), ov(:)
        integer(int64) :: nnull, i
        logical :: ok

        call golden_fixture(32_int64, x)
        allocate(s(32), mask(32), ov(32))
        call pf_normal_scores(x, s, ok=ok)
        call check(error, ok .and. all(s == s), "a clean population must score every element")
        if (allocated(error)) return

        mask = .true.
        mask(5) = .false.
        mask(11) = .false.
        call pf_normal_scores(x, s, is_valid=mask, out_valid=ov, n_null=nnull, ok=ok)
        call check(error, (.not. ok) .and. nnull == 2_int64, &
            "an excluded element must give ok=.false. with n_null naming the cause")
        if (allocated(error)) return
        call check(error, (.not. ov(5)) .and. (.not. ov(11)) .and. count(ov) == 30, &
            "out_valid must mark exactly the excluded elements")
        if (allocated(error)) return
        ! The NaN is written whether or not the mask was asked for -- so an output element is
        ! never left as undefined memory, which nagfor's `-nan` build reports.
        call check(error, s(5) /= s(5) .and. s(11) /= s(11), &
            "and their outputs must be quiet NaNs, not stale or zero")
        if (allocated(error)) return
        do i = 1_int64, 32_int64
            if (mask(i)) then
                if (s(i) /= s(i)) then
                    call check(error, .false., "a surviving element must have a real score")
                    return
                end if
            end if
        end do

        ! Without `out_valid` the NaN is the only signal, and it must still be there.
        call pf_normal_scores(x, s, is_valid=mask, ok=ok)
        call check(error, (.not. ok) .and. s(5) /= s(5) .and. s(11) /= s(11), &
            "the NaN must be written even when out_valid was not asked for")
    end subroutine test_normal_scores_marks_exclusions

    !> Each `method=` token uses its own documented plotting position, and no two agree.
    !!
    !! A tie-free population is used deliberately, so the midranks are exactly `1..m` and the
    !! expectation is the published formula evaluated by hand. **A token falling through to the
    !! default is what this catches** -- the defect a `select case` without a `case default`
    !! produces, and one that changes every score by a few percent while leaving them ordered,
    !! centred and monotone.
    subroutine test_normal_scores_every_method_token(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        integer, parameter :: M = 11                    !! a tie-free population of eleven.
        real(real64) :: x(M), s(M), want(M), blom(M)
        real(real64) :: rm, den
        integer :: i, t
        character(len=8), parameter :: TOKENS(5) = &
            [character(len=8) :: "blom", "weibull", "tukey", "hazen", "cunnane"]
        real(real64), parameter :: AVAL(5) = &
            [0.375_real64, 0.0_real64, 1.0_real64 / 3.0_real64, 0.5_real64, 0.4_real64]
        !> Filliben's median rank: the two ends are closed forms and the interior is its own
        !! formula.
        !!
        !! **These are PARAMETERs, and the five `AVAL` positions below deliberately are not.** The
        !! interior denominator here is a constant the compiler can see, and a fast floating-point
        !! model turns a division by one of those into a multiply by the reciprocal -- 1 ulp out,
        !! which `pf_probit` amplifies into roughly ten, against a bit-equality assertion
        !! (`fortran-gotchas.md`, ifx). Folding it as a `parameter` instead is correctly rounded on
        !! every compiler and agrees with the true division the library performs on its run-time
        !! `m`. `rm + 1 - 2*a` is a different case: it is an FMA-contractable shape, so the library
        !! and a matching run-time expression contract alike and agree, while a folded constant --
        !! being unfused -- would not.
        real(real64), parameter :: FPOS(M) = &
            [1.0_real64 - 0.5_real64 ** (1.0_real64 / real(M, real64)), &
             ((real(i, real64) - 0.3175_real64) / (real(M, real64) + 0.365_real64), i = 2, M - 1), &
             0.5_real64 ** (1.0_real64 / real(M, real64))]

        do i = 1, M
            x(i) = real(i, real64) * 3.0_real64   ! strictly increasing, so rank(i) = i.
        end do
        rm = real(M, real64)

        do t = 1, 5
            den = rm + 1.0_real64 - 2.0_real64 * AVAL(t)
            do i = 1, M
                want(i) = pf_probit((real(i, real64) - AVAL(t)) / den)
            end do
            call pf_normal_scores(x, s, method=trim(TOKENS(t)))
            call check(error, all(s == want), &
                'method="' // trim(TOKENS(t)) // '" must use its own documented constant')
            if (allocated(error)) return
            if (t == 1) blom = want
            ! Every token but the first must DIFFER from the default, or a fall-through would
            ! pass this test unnoticed.
            if (t > 1) then
                call check(error, any(s /= blom), &
                    'method="' // trim(TOKENS(t)) // '" must not agree with "blom" everywhere')
                if (allocated(error)) return
            end if
        end do

        ! The default is "blom", asserted rather than assumed.
        call pf_normal_scores(x, s)
        call check(error, all(s == blom), 'the default method must be "blom"')
        if (allocated(error)) return

        ! "filliben" is the MEDIAN rank and is a different SHAPE, not a different constant: two
        ! closed forms at the ends and an interior formula between.
        do i = 1, M
            want(i) = pf_probit(FPOS(i))
        end do
        call pf_normal_scores(x, s, method="FILLIBEN")
        call check(error, all(s == want), &
            'method="filliben" must use the median rank, ends included, and match case-insensitively')
        if (allocated(error)) return
        call check(error, any(s /= blom), '"filliben" must not agree with "blom" everywhere')
    end subroutine test_normal_scores_every_method_token

    !> The three degenerate populations: one element, none, and every element tied.
    subroutine test_normal_scores_degenerate(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        real(real64) :: one(1), s1(1), empty(0), s0(0), flat(6), s6(6)
        real(real64) :: mid
        logical :: ok
        integer :: t
        character(len=8), parameter :: TOKENS(6) = &
            [character(len=8) :: "blom", "weibull", "tukey", "hazen", "cunnane", "filliben"]

        ! **One survivor scores EXACTLY zero, under every token.** Each rule collapses to the
        ! plotting position 1/2 at m = 1, and `pf_probit(0.5)` is exactly 0 -- so this is `== 0`
        ! rather than a tolerance, and it catches a quotient left to round on its own.
        one(1) = 42.0_real64
        do t = 1, 6
            call pf_normal_scores(one, s1, method=trim(TOKENS(t)), ok=ok)
            call check(error, s1(1) == 0.0_real64 .and. ok, &
                'a single survivor must score exactly 0 under method="' // trim(TOKENS(t)) // '"')
            if (allocated(error)) return
        end do

        ! An empty population is not an error: `ok` is .false. and nothing aborts, because a
        ! per-group loop meets an empty group on real data.
        call pf_normal_scores(empty, s0, ok=ok)
        call check(error, .not. ok, "an empty population must report ok=.false. and not abort")
        if (allocated(error)) return

        ! Every element tied: one midrank, so one score, and it is the centre.
        flat = 7.0_real64
        call pf_normal_scores(flat, s6, ok=ok)
        mid = pf_probit((3.5_real64 - 0.375_real64) / (6.0_real64 + 1.0_real64 - 0.75_real64))
        call check(error, all(s6 == mid) .and. ok, &
            "a constant population must give every element the same central score")
        if (allocated(error)) return
        call check(error, abs(mid) < 1.0e-15_real64, &
            "and that score is the centre, because the midrank of a full tie is (m+1)/2")
    end subroutine test_normal_scores_degenerate

    !> The probability fixture `pf_probit_mean`'s golden cases are taken over.
    !!
    !! `a(i) + 1` runs over `[1, 1000003]` and the divisor is 1000005, so every value is STRICTLY
    !! inside `(0, 1)`: the `0` and `1` rows are the subject of their own test and are asserted
    !! exactly, and a fixture that stumbled onto one would quietly turn this case into that one.
    !! Both integers are exact doubles and the division is one correctly rounded operation, so
    !! `probs()` in `tools/generate_probit_reference.py` produces the same doubles.
    subroutine golden_probs(n, p)
        integer(int64), intent(in) :: n                          !! how many values to build.
        real(real64), allocatable, intent(out) :: p(:)           !! the probabilities.
        integer(int64) :: i, a

        allocate(p(n))
        do i = 1_int64, n
            a = mod(i * i * 7919_int64 + 12345_int64, 1000003_int64)
            p(i) = real(a + 1_int64, real64) / 1000005.0_real64
        end do
    end subroutine golden_probs

    !> `pf_probit_fit`, `pf_probit_scale` and `pf_probit_mean` against the 50-digit oracle.
    !!
    !! **The oracle shares no arithmetic with the library**: every expectation comes from `mpmath`
    !! at 50 digits through `tools/generate_probit_reference.py`, whose `--self-test` also checks
    !! the fit against `scipy.stats.probplot` and the scale against numpy's percentiles. What is
    !! asserted here is that this library's double-precision route lands on the correctly rounded
    !! answer to within the accumulated rounding of a thousand-element reduction.
    !!
    !! **Both `method=` arms are asserted, and they must DIFFER.** `scipy.stats.probplot`'s
    !! plotting positions are Filliben's median ranks rather than this generic's `"blom"` default,
    !! so the two tokens are what the two golden sets are; a token silently falling through to the
    !! default would make one set match the other, which the third assertion is what catches.
    subroutine test_probit_family_matches_the_oracle(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        real(real64), parameter :: TOL = 1.0e-12_real64
        real(real64), allocatable :: x(:), p(:), w(:)
        real(real64) :: loc, sigma, corr, floc, fsigma, fcorr, sc, m
        integer(int64) :: i

        call golden_fixture(int(NFIX, int64), x)
        call pf_probit_fit(x, loc, sigma, corr)
        call check(error, abs(loc - FIT_LOC) <= TOL * abs(FIT_LOC), &
            "pf_probit_fit's intercept must match the oracle over the recipe fixture")
        if (allocated(error)) return
        call check(error, abs(sigma - FIT_SIGMA) <= TOL * abs(FIT_SIGMA), &
            "and its slope, which is the whole point of the procedure")
        if (allocated(error)) return
        call check(error, abs(corr - FIT_CORR) <= TOL * abs(FIT_CORR), &
            "and the probability-plot correlation coefficient")
        if (allocated(error)) return
        ! 0.974 rather than 1: the fixture is a scrambled uniform, not a Gaussian. A `corr`
        ! computed against the FITTED values rather than against the normal scores would be
        ! exactly 1 here, which is why the oracle's value being well below 1 is load-bearing.
        call check(error, FIT_CORR < 0.99_real64, &
            "the control: the fixture is not Gaussian, so a corr of 1 would mean corr is " // &
            "being computed by construction")
        if (allocated(error)) return

        call pf_probit_fit(x, floc, fsigma, fcorr, method="filliben")
        call check(error, abs(fsigma - FIT_SIGMA_FILLIBEN) <= TOL * abs(FIT_SIGMA_FILLIBEN) .and. &
            abs(fcorr - FIT_CORR_FILLIBEN) <= TOL * abs(FIT_CORR_FILLIBEN) .and. &
            abs(floc - FIT_LOC_FILLIBEN) <= TOL * abs(FIT_LOC_FILLIBEN), &
            'method="filliben" must match its own golden set, which is scipy.stats.probplot')
        if (allocated(error)) return
        call check(error, abs(fsigma - sigma) > 1.0e-6_real64 * abs(sigma), &
            "the control: the two tokens must give DIFFERENT slopes, or one of them fell " // &
            "through to the other")
        if (allocated(error)) return

        call pf_probit_scale(x, sc)
        call check(error, abs(sc - SCALE_P25) <= TOL * abs(SCALE_P25), &
            "pf_probit_scale at the default prob=0.25 must match the oracle")
        if (allocated(error)) return
        call pf_probit_scale(x, sc, prob=0.15865525393145705_real64)
        call check(error, abs(sc - SCALE_P16) <= TOL * abs(SCALE_P16), &
            "and at the 16th/84th percentile pair, whose divisor is exactly 2")
        if (allocated(error)) return

        call golden_probs(int(NFIX, int64), p)
        call pf_probit_mean(p, m)
        call check(error, abs(m - PMEAN) <= TOL * abs(PMEAN), &
            "pf_probit_mean must match the oracle over the probability fixture")
        if (allocated(error)) return
        allocate(w(NFIX))
        do i = 1_int64, int(NFIX, int64)
            w(i) = real(mod(i, 5_int64), real64)
        end do
        call pf_probit_mean(p, m, weights=w)
        call check(error, abs(m - PMEAN_W) <= TOL * abs(PMEAN_W), &
            "and weighted, where every fifth weight is ZERO and removes its element")
        if (allocated(error)) return
        call check(error, abs(PMEAN_W - PMEAN) > 1.0e-6_real64, &
            "the control: the weighted and unweighted answers must differ, or the weights are " // &
            "not reaching the accumulation")
    end subroutine test_probit_family_matches_the_oracle

    !> `pf_probit_fit`'s `loc` is the MEAN of the survivors, under every token.
    !!
    !! **This is `feature_risks.md` Risk-255, and equality is the assertion rather than a
    !! tolerance.** Every one of the six plotting-position rules is symmetric about the median, so
    !! the normal scores sum to zero and the least-squares intercept IS the mean -- which is why
    !! `probit_line` evaluates it as the mean. Two changes break that silently: an asymmetric
    !! `method=` token added later, and replacing the ORDINAL ranks with `pf_normal_scores`'
    !! midranks, which a tie makes asymmetric. The TIED population below is what sees the second.
    !!
    !! Exclusions are in the fixture too, because the identity is over the SURVIVORS: a `loc`
    !! taken over the whole array would fail here and pass on an unmasked population.
    subroutine test_probit_fit_loc_is_the_mean(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        character(len=8), parameter :: TOKENS(6) = ["blom    ", "weibull ", "tukey   ", &
            "hazen   ", "cunnane ", "filliben"]
        real(real64) :: tied(9), want, loc, sigma, plain
        real(real64), allocatable :: x(:), kept(:)
        logical, allocatable :: mask(:)
        integer(int64) :: i, k
        integer :: j

        ! A population with ties spanning three different runs, so the ordinal-versus-midrank
        ! choice is observable rather than a distinction without a difference.
        tied = [3.0_real64, 1.0_real64, 3.0_real64, 7.0_real64, 1.0_real64, 9.0_real64, &
            3.0_real64, 12.0_real64, 9.0_real64]
        ! Small integers, so BOTH routes to the mean are exact here and `==` is available: this
        ! population sums to 48 whatever order it is added in, and the single division that
        ! follows rounds the same way for both. At the fixture's scale below that is no longer
        ! true -- `pf_mean` refines its quotient and its own doc-comment says so -- which is why
        ! the second half of this test uses an ulp bound rather than equality.
        call pf_mean(tied, want)
        do j = 1, 6
            call pf_probit_fit(tied, loc, sigma, method=trim(TOKENS(j)))
            call check(error, loc == want, &
                "pf_probit_fit(method=" // trim(TOKENS(j)) // ")'s loc must be the mean of the " // &
                "survivors; a midrank or an asymmetric position rule breaks it")
            if (allocated(error)) return
        end do

        ! The same identity with a third of the population excluded. `kept` is the survivors on
        ! their own, so `want` is a mean of the sub-population and not of the array.
        call golden_fixture(60_int64, x)
        allocate(mask(60), kept(40))
        k = 0_int64
        do i = 1_int64, 60_int64
            mask(i) = mod(i, 3_int64) /= 0_int64
            if (mask(i)) then
                k = k + 1_int64
                kept(k) = x(i)
            end if
        end do
        ! **`pf_sum` rather than `pf_mean`, and equality rather than a tolerance.** Every fixture
        ! value is an integer over 1024 with a magnitude below 512, so forty of them sum exactly
        ! in any order -- a PAIRWISE sum and the plain sorted one inside `probit_line` reach the
        ! same double, and the single division that follows rounds identically. `pf_mean` is a
        ! deliberately different quantity here: it refines its quotient by `sum(x - mu)/n`, which
        ! is the right centre for the moments and, on a population that already summed exactly,
        ! costs about 20 ulp. Comparing against it would need a bound loose enough to hide
        ! nothing this test is looking for, but would say the wrong thing about which value is
        ! the accurate one.
        call pf_sum(kept, want)
        want = want / real(size(kept, kind=int64), real64)
        call pf_probit_fit(x, loc, sigma, is_valid=mask)
        call check(error, loc == want, &
            "and over the SURVIVORS when a third of the population is excluded")
        if (allocated(error)) return
        ! And the sub-population computed on its own must give the same line, which is Risk-254's
        ! shape one tier up: a denominator of `size(values)` leaves `loc` right and `sigma` wrong.
        call pf_probit_fit(kept, plain, want)
        call check(error, sigma == want, &
            "pf_probit_fit's slope must denominate by the SURVIVOR count: the masked fit and " // &
            "the sub-population fit must be bit-for-bit equal")
    end subroutine test_probit_fit_loc_is_the_mean

    !> The fit recovers a Gaussian's mean and sigma, and `corr` falls when the sample is skewed.
    !!
    !! **Both halves are needed and neither alone is worth much.** Without the second, a `corr`
    !! computed as 1 by construction passes; without the first, a fit that returned the slope and
    !! the intercept the other way round would still show a `corr` that falls. The skewed arm is
    !! also what shows `corr` is a diagnostic rather than a formality.
    subroutine test_probit_fit_recovers_a_gaussian(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        integer(int64), parameter :: N = 50000_int64
        real(real64), allocatable :: g(:), sk(:)
        real(real64) :: loc, sigma, corr, loc2, sigma2, corr2, mu, sd
        integer(int64) :: i

        allocate(g(N), sk(N))
        ! Counter-based at a fixed seed, so this is the same sample on every machine and thread
        ! count: nothing about the pass condition below is statistical.
        call pf_random_fill_normal(20260911_int64, 1_int64, g)
        g = 12.5_real64 + 3.25_real64 * g
        call pf_mean(g, mu)
        call pf_stddev(g, sd)
        call pf_probit_fit(g, loc, sigma, corr)
        call check(error, abs(loc - mu) <= 1.0e-12_real64 * abs(mu), &
            "the fitted intercept must be the sample mean")
        if (allocated(error)) return
        ! 0.5% of the sample standard deviation. The two estimate the same quantity and differ
        ! only by their efficiency, so anything looser would also pass for a slope that had been
        ! divided by the wrong denominator.
        call check(error, abs(sigma - sd) <= 0.005_real64 * sd, &
            "and the fitted slope must be the sample standard deviation, to sampling error")
        if (allocated(error)) return
        call check(error, corr > 0.9995_real64, &
            "a Gaussian sample's normal-probability plot must be very nearly straight")
        if (allocated(error)) return
        ! Slope and intercept transposed would put the SCALE where the location is. The two are
        ! 12.5 and 3.25 here, deliberately far apart, so the transposition cannot hide.
        call check(error, abs(loc - sigma) > 1.0_real64, &
            "the control: loc and sigma must be distinguishable, or a transposition passes")
        if (allocated(error)) return

        ! The skewed arm: exponentiating a Gaussian is a lognormal, whose normal-probability plot
        ! is visibly bent. `corr` must fall, and it must fall a long way.
        do i = 1_int64, N
            sk(i) = exp(0.9_real64 * (g(i) - 12.5_real64) / 3.25_real64)
        end do
        call pf_probit_fit(sk, loc2, sigma2, corr2)
        call check(error, corr2 < 0.96_real64, &
            "and a lognormal sample's must not: corr is the normality diagnostic")
        if (allocated(error)) return
        call check(error, corr - corr2 > 0.03_real64, &
            "the two corrs must be far apart, or corr is not discriminating between them")
    end subroutine test_probit_fit_recovers_a_gaussian

    !> `pf_probit_scale` is a robust sigma: it tracks `pf_stddev` on clean data and `pf_mad` on
    !! contaminated data.
    !!
    !! Both halves, exactly as `test_mad_estimates_the_gaussian_sigma` has them: agreeing on the
    !! clean sample is no coincidence, and disagreeing with `pf_stddev` on the contaminated one is
    !! the entire reason to reach for either. The `pf_iqr` identity is what pins the DIVISOR --
    !! a denominator formed as `Phi^-1(1-prob)` without the factor 2 doubles every answer, which
    !! the sampling-error comparisons alone would not distinguish from a bad estimator.
    subroutine test_probit_scale_estimates_the_sigma(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        integer(int64), parameter :: N = 200000_int64
        real(real64), allocatable :: g(:)
        real(real64) :: sd, mad, sc, sd2, mad2, sc2, r
        integer(int64) :: i

        allocate(g(N))
        call pf_random_fill_normal(20260829_int64, 1_int64, g)
        call pf_stddev(g, sd)
        call pf_mad(g, mad)
        call pf_probit_scale(g, sc)
        call check(error, abs(sc - sd) <= 0.01_real64 * sd, &
            "pf_probit_scale must track pf_stddev on a clean Gaussian sample")
        if (allocated(error)) return
        call check(error, abs(sc - mad) <= 0.01_real64 * mad, &
            "and pf_mad, which it generalises from the deviations to the values")
        if (allocated(error)) return

        ! The divisor, asserted as an identity rather than to a tolerance: at prob=0.25 this
        ! procedure IS pf_iqr over 2*Phi^-1(0.75). One ulp of slack only, because both sides take
        ! the same two quantiles off the same ordering.
        call pf_iqr(g, r)
        call check(error, abs(sc - r / 1.3489795003921634_real64) <= &
            4.0_real64 * spacing(sc), &
            "pf_probit_scale(prob=0.25) must be exactly pf_iqr over 1.3489795003921634")
        if (allocated(error)) return
        call check(error, abs(sc - r / 0.6744897501960817_real64) > 0.1_real64 * sc, &
            "the control: dropping the factor 2 from the divisor must be visible")
        if (allocated(error)) return

        do i = 1000_int64, N, 1000_int64
            g(i) = 100.0_real64
        end do
        call pf_stddev(g, sd2)
        call pf_mad(g, mad2)
        call pf_probit_scale(g, sc2)
        call check(error, sd2 > 3.0_real64 * sd, &
            "the control: pf_stddev must be wrecked by 0.1% contamination")
        if (allocated(error)) return
        call check(error, abs(sc2 - sc) <= 0.01_real64 * sc .and. &
            abs(mad2 - mad) <= 0.01_real64 * mad, &
            "pf_probit_scale and pf_mad must both be almost unmoved by it")
    end subroutine test_probit_scale_estimates_the_sigma

    !> The populations a line cannot be fitted to, and the ones where only `corr` is undefined.
    subroutine test_probit_fit_and_scale_degenerate(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        real(real64) :: empty(0), one(1), two(2), flat(5), v(9)
        real(real64) :: loc, sigma, corr, sc
        logical :: ok, ok2
        integer :: k

        call pf_probit_fit(empty, loc, sigma, corr, ok=ok)
        call check(error, loc /= loc .and. sigma /= sigma .and. corr /= corr .and. .not. ok, &
            "an empty population fits no line: three NaNs and ok=.false., never an abort")
        if (allocated(error)) return
        one = [4.0_real64]
        call pf_probit_fit(one, loc, sigma, corr, ok=ok)
        call check(error, loc /= loc .and. sigma /= sigma .and. .not. ok, &
            "and one point fits no line either -- a line needs two")
        if (allocated(error)) return
        two = [1.0_real64, 5.0_real64]
        call pf_probit_fit(two, loc, sigma, corr, ok=ok)
        call check(error, loc == 3.0_real64 .and. sigma > 0.0_real64 .and. ok, &
            "two points DO fit a line, and its intercept is their mean")
        if (allocated(error)) return
        ! Two points lie on their own line exactly, so the correlation is 1 to rounding. This is
        ! the one population where a corr of 1 is the right answer.
        call check(error, abs(corr - 1.0_real64) < 1.0e-12_real64, &
            "and two points are collinear, so corr is 1")
        if (allocated(error)) return

        ! A CONSTANT population: the location and the scale are both perfectly well defined and
        ! only the correlation is not, which is the one shape where `ok` depends on whether the
        ! caller asked for `corr` at all.
        flat = 7.5_real64
        call pf_probit_fit(flat, loc, sigma, ok=ok)
        call check(error, loc == 7.5_real64 .and. sigma == 0.0_real64 .and. ok, &
            "a constant population has loc = that constant and sigma exactly 0, with ok=.true.")
        if (allocated(error)) return
        call pf_probit_fit(flat, loc, sigma, corr, ok=ok2)
        call check(error, corr /= corr .and. .not. ok2, &
            "but its corr is a NaN, and asking for it is what turns ok .false.")
        if (allocated(error)) return
        call pf_probit_scale(flat, sc, ok=ok)
        call check(error, sc == 0.0_real64 .and. ok, &
            "and its implied scale is exactly 0, which is an answer rather than a failure")
        if (allocated(error)) return
        call pf_probit_scale(empty, sc, ok=ok)
        call check(error, sc /= sc .and. .not. ok, &
            "an empty population's implied scale is a quiet NaN with ok=.false.")
        if (allocated(error)) return

        ! Risk-253: under skipnan=.false. the NaN sorts to one end and the line through the rest
        ! is an entirely ordinary one, so `saw_nan` has to be tested rather than inherited.
        do k = 1, 9
            v(k) = real(k, real64)
        end do
        v(4) = ieee_value(1.0_real64, ieee_quiet_nan)
        call pf_probit_fit(v, loc, sigma, corr, skipnan=.false., ok=ok)
        call check(error, loc /= loc .and. sigma /= sigma .and. corr /= corr .and. .not. ok, &
            "a propagating NaN must reach every one of pf_probit_fit's three outputs")
        if (allocated(error)) return
        call pf_probit_scale(v, sc, skipnan=.false., ok=ok)
        call check(error, sc /= sc .and. .not. ok, "and pf_probit_scale's")
        if (allocated(error)) return
        ! The second half, without which a procedure returning NaN unconditionally would pass.
        call pf_probit_fit(v, loc, sigma, corr, ok=ok)
        call check(error, loc == loc .and. sigma == sigma .and. ok, &
            "the control: at the default skipnan the NaN is merely excluded and the line is real")
    end subroutine test_probit_fit_and_scale_degenerate

    !> `%probit_fit`, `%probit_scale` and `%probit_mean` answer off the object, adding no ordering.
    !!
    !! **The bit-for-bit comparison is the assertion.** A binding that recomputed instead of
    !! reading tier B would agree to a tolerance and differ in the last bits, and one that ordered
    !! a second time would agree exactly -- so the sort counter and the equality are each needed
    !! for what the other cannot see. `%probit_mean` orders NOTHING, which is the third assertion:
    !! the counter must be unchanged by it even on an object that has never been ordered.
    subroutine test_probit_bindings_read_tier_b(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        real(real64), allocatable :: x(:), p(:)
        real(real64) :: loc, sigma, corr, oloc, osigma, ocorr, sc, osc, m, om
        type(pf_stats) :: s, ps
        integer(int64) :: before

        call golden_fixture(400_int64, x)
        call pf_probit_fit(x, loc, sigma, corr)
        call pf_probit_scale(x, sc)

        call s%compute(x)
        call parquet_debug_reset_stats_sorts()
        call s%probit_fit(oloc, osigma, ocorr)
        call check(error, oloc == loc .and. osigma == sigma .and. ocorr == corr, &
            "%probit_fit must return bit for bit what pf_probit_fit returns")
        if (allocated(error)) return
        osc = s%probit_scale()
        call check(error, osc == sc, "and %probit_scale what pf_probit_scale returns")
        if (allocated(error)) return
        ! A median as well, so the count is over FOUR order statistics rather than two.
        osc = s%median()
        osc = s%iqr()
        call check(error, parquet_debug_stats_sorts() == 1_int64, &
            "a fit, a scale, a median and an IQR off one object must cost ONE ordering")
        if (allocated(error)) return

        call golden_probs(200_int64, p)
        call pf_probit_mean(p, m)
        call ps%compute(p)
        call parquet_debug_reset_stats_sorts()
        before = parquet_debug_stats_sorts()
        om = ps%probit_mean()
        call check(error, om == m, &
            "%probit_mean must return bit for bit what pf_probit_mean returns")
        if (allocated(error)) return
        call check(error, parquet_debug_stats_sorts() == before .and. .not. ps%is_ordered(), &
            "and it must order NOTHING: a probit mean is not an order statistic")
    end subroutine test_probit_bindings_read_tier_b

    !> The four boundary rows of `pf_probit_mean`, asserted EXACTLY.
    !!
    !! **`feature_risks.md` Risk-256.** `Phi^-1(0)` is `-Infinity` and `Phi^-1(1)` is `+Infinity`,
    !! so a zero in the population drags the probit-space mean to `-Inf` and `Phi(-Inf)` is
    !! exactly `0`; a one does the mirror; a population holding both is `-Inf + Inf` and has no
    !! mean at all. An implementer meeting an infinity in a mean loop reaches for
    !! `max(p, tiny)`/`min(p, 1 - eps)`, which turns all three into finite numbers near the
    !! boundary -- plausible, in range, and wrong. Equality is therefore the assertion; a
    !! tolerance would accept every clamped answer.
    subroutine test_probit_mean_boundary_rows(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        real(real64) :: withzero(4), withone(4), both(5), plain(4), flat(6), empty(0)
        real(real64) :: m
        logical :: ok

        withzero = [0.2_real64, 0.0_real64, 0.5_real64, 0.9_real64]
        call pf_probit_mean(withzero, m, ok=ok)
        call check(error, m == 0.0_real64 .and. ok, &
            "a zero in the population makes the probit mean EXACTLY 0, and that is a defined " // &
            "answer rather than a failure")
        if (allocated(error)) return
        withone = [0.2_real64, 1.0_real64, 0.5_real64, 0.9_real64]
        call pf_probit_mean(withone, m, ok=ok)
        call check(error, m == 1.0_real64 .and. ok, "and a one makes it exactly 1")
        if (allocated(error)) return
        both = [0.2_real64, 1.0_real64, 0.5_real64, 0.0_real64, 0.9_real64]
        call pf_probit_mean(both, m, ok=ok)
        call check(error, m /= m .and. .not. ok, &
            "a population holding BOTH is -Inf + Inf, which is a NaN and not a half")
        if (allocated(error)) return
        ! The control that makes the three above mean something: the same population without the
        ! boundary values is an ordinary number strictly inside (0, 1).
        plain = [0.2_real64, 0.4_real64, 0.5_real64, 0.9_real64]
        call pf_probit_mean(plain, m, ok=ok)
        call check(error, m > 0.0_real64 .and. m < 1.0_real64 .and. ok, &
            "the control: without a boundary value the answer is strictly inside (0, 1)")
        if (allocated(error)) return

        ! A constant probability is its own probit mean, which is the round trip through both
        ! directions of the kernel and would catch either one drifting alone.
        flat = 0.37_real64
        call pf_probit_mean(flat, m, ok=ok)
        call check(error, abs(m - 0.37_real64) <= 8.0_real64 * spacing(0.37_real64) .and. ok, &
            "a constant probability must come back as itself: Phi(Phi^-1(p)) round-trips")
        if (allocated(error)) return
        call pf_probit_mean(empty, m, ok=ok)
        call check(error, m /= m .and. .not. ok, "an empty population has no probit mean")
    end subroutine test_probit_mean_boundary_rows

    !> The domain screen reads a NaN as a NaN, and the weight convention is `pf_gmean`'s.
    !!
    !! **The first assertion is Risk-172's shape.** `p >= 0.0 .and. p <= 1.0` is FALSE for a NaN,
    !! so a domain screen written that way classifies a NaN as an out-of-domain VALUE -- and the
    !! two have different answers under `skipnan`: an out-of-range value is a NaN with
    !! `ok = .false.` whatever `skipnan` says, while a NaN is EXCLUDED at the default and
    !! propagates only when the caller asks it to.
    subroutine test_probit_mean_domain_and_weights(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        real(real64) :: high(4), low(4), withnan(4), p(4), w(4), zw(4)
        real(real64) :: m, mkept, want
        real(real64), allocatable :: kept(:)
        logical :: ok

        high = [0.2_real64, 1.5_real64, 0.5_real64, 0.9_real64]
        call pf_probit_mean(high, m, ok=ok)
        call check(error, m /= m .and. .not. ok, &
            "a value above 1 is not a probability: NaN with ok=.false.")
        if (allocated(error)) return
        low = [0.2_real64, -0.25_real64, 0.5_real64, 0.9_real64]
        call pf_probit_mean(low, m, ok=ok)
        call check(error, m /= m .and. .not. ok, "and a value below 0")
        if (allocated(error)) return
        ! An out-of-range value is refused whatever `skipnan` says -- it is a value, not a NaN.
        call pf_probit_mean(high, m, skipnan=.false., ok=ok)
        call check(error, m /= m .and. .not. ok, &
            "and it stays refused under skipnan=.false., where it is still a value")
        if (allocated(error)) return

        withnan = [0.2_real64, ieee_value(1.0_real64, ieee_quiet_nan), 0.5_real64, 0.9_real64]
        call pf_probit_mean(withnan, m, ok=ok)
        call check(error, m == m .and. ok, &
            "a NaN is EXCLUDED at the default skipnan, so the other three still have a mean -- " // &
            "a screen written `p >= 0 .and. p <= 1` would have refused the whole population")
        if (allocated(error)) return
        call pf_probit_mean([0.2_real64, 0.5_real64, 0.9_real64], want)
        call check(error, m == want, &
            "and that mean is bit for bit the mean of the three survivors")
        if (allocated(error)) return
        call pf_probit_mean(withnan, m, skipnan=.false., ok=ok)
        call check(error, m /= m .and. .not. ok, &
            "and only skipnan=.false. propagates it")
        if (allocated(error)) return

        ! A ZERO weight removes the element, exactly as it does everywhere else in this module --
        ! so the weighted answer must equal the unweighted answer over the survivors alone.
        p = [0.2_real64, 0.44_real64, 0.5_real64, 0.9_real64]
        zw = [1.0_real64, 0.0_real64, 1.0_real64, 1.0_real64]
        allocate(kept(3))
        kept = [0.2_real64, 0.5_real64, 0.9_real64]
        call pf_probit_mean(p, m, weights=zw)
        call pf_probit_mean(kept, mkept)
        call check(error, m == mkept, &
            "a zero weight must REMOVE its element, not merely down-weight it")
        if (allocated(error)) return
        w = [1.0_real64, 3.0_real64, 1.0_real64, 1.0_real64]
        call pf_probit_mean(p, m, weights=w)
        call check(error, m /= mkept .and. m == m, &
            "the control: a non-zero weight on the same element must change the answer")
    end subroutine test_probit_mean_domain_and_weights

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

    !> Reaches the 24 generated per-kind specifics that P10's coverage run found never called.
    !!
    !! **Why this is worth a test of its own.** The per-kind layer is generated from one template,
    !! so a mistake in the template is a mistake in all six specifics of a family and any one of
    !! them catches it. What a template CANNOT make uniform is the part that varies by kind:
    !! `kind_opts` drops `skipnan` and `n_nan` from a kind that cannot hold a NaN, so those
    !! specifics have a different argument list and a different generated call. Those are exactly
    !! the ones a sampled cross-kind test misses, and P10's coverage run found 24 of them shipping
    !! as public API with no test ever having executed them -- a user calling `pf_hmean` on an
    !! `integer(int64)` array would have been the first.
    !!
    !! The assertions are deliberately thin: each specific widens and forwards, so the only thing
    !! that can go wrong is a mis-wired argument or a lost value, and comparing against the same
    !! call over `real(real64)` catches both. Depth belongs in the family's own test.
    subroutine test_every_kind_specific_is_reached(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        real(real64) :: xd(4), got, want, od(4), ox(4), e3(3), ed(3)
        real(real32) :: xs(4)
        integer(int32) :: xi(4)
        integer(int64) :: xl(4)
        logical :: xb(4), allt(4)
        type(pf_stats) :: acc
        type(parquet_column) :: ca, cb
        integer :: k

        xd = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64]
        xs = real(xd, real32)
        xi = int(xd, int32)
        xl = int(xd, int64)
        xb = [.true., .true., .false., .true.]
        ! All-true for the two power means: a logical array maps .false. to 0, and the geometric
        ! mean of a population containing a zero is exactly 0 while the harmonic mean of one is
        ! degenerate. Those are the documented answers and they are asserted in the power-mean
        ! test; here the point is only that the specific is reached and forwards correctly.
        allt = .true.

        call pf_sum(xd, want)
        call pf_sum(xi, got)
        call check(error, got == want, "pf_sum over int32")
        if (allocated(error)) return
        call pf_sum(xl, got)
        call check(error, got == want, "pf_sum over int64")
        if (allocated(error)) return

        call pf_mean(xd, want)
        call pf_mean(xi, got)
        call check(error, got == want, "pf_mean over int32")
        if (allocated(error)) return
        call pf_mean(xl, got)
        call check(error, got == want, "pf_mean over int64")
        if (allocated(error)) return

        call pf_gmean(xd, want)
        call pf_gmean(xl, got)
        call check(error, close_to(got, want), "pf_gmean over int64")
        if (allocated(error)) return
        call pf_gmean(allt, got)
        call check(error, got == 1.0_real64, "pf_gmean over a logical array")
        if (allocated(error)) return

        call pf_hmean(xd, want)
        call pf_hmean(xi, got)
        call check(error, close_to(got, want), "pf_hmean over int32")
        if (allocated(error)) return
        call pf_hmean(xl, got)
        call check(error, close_to(got, want), "pf_hmean over int64")
        if (allocated(error)) return
        call pf_hmean(allt, got)
        call check(error, got == 1.0_real64, "pf_hmean over a logical array")
        if (allocated(error)) return

        call pf_cummax(xd, od)
        call pf_cummax(xi, ox)
        call check(error, all(ox == od), "pf_cummax over int32")
        if (allocated(error)) return
        call pf_cummin(xi, ox)
        call pf_cummin(xd, od)
        call check(error, all(ox == od), "pf_cummin over int32")
        if (allocated(error)) return
        call pf_cummin(xl, ox)
        call check(error, all(ox == od), "pf_cummin over int64")
        if (allocated(error)) return
        call pf_cumprod(xl, ox)
        call pf_cumprod(xd, od)
        call check(error, all(ox == od), "pf_cumprod over int64")
        if (allocated(error)) return

        ! The logical arms of the cumulative family, against the 1/0 mapping written out by hand.
        call pf_cummax(xb, ox)
        call check(error, all(ox == [1.0_real64, 1.0_real64, 1.0_real64, 1.0_real64]), &
            "pf_cummax over a logical array")
        if (allocated(error)) return
        call pf_cummin(xb, ox)
        call check(error, all(ox == [1.0_real64, 1.0_real64, 0.0_real64, 0.0_real64]), &
            "pf_cummin over a logical array")
        if (allocated(error)) return
        call pf_cumprod(xb, ox)
        call check(error, all(ox == [1.0_real64, 1.0_real64, 0.0_real64, 0.0_real64]), &
            "pf_cumprod over a logical array")
        if (allocated(error)) return

        call pf_zscore(xd, od)
        call pf_zscore(xl, ox)
        call check(error, all(ox == od), "pf_zscore over int64")
        if (allocated(error)) return
        call pf_zscore(xb, ox)
        call check(error, ox(1) == ox(2) .and. ox(3) < ox(1), &
            "pf_zscore over a logical array standardises the 1/0 mapping")
        if (allocated(error)) return

        call pf_bin_edges(xd, 2, ed)
        call pf_bin_edges(xi, 2, e3)
        call check(error, all(e3 == ed), "pf_bin_edges over int32")
        if (allocated(error)) return
        call pf_bin_edges(xl, 2, e3)
        call check(error, all(e3 == ed), "pf_bin_edges over int64")
        if (allocated(error)) return
        call pf_bin_edges(xb, 2, e3)
        call check(error, e3(1) == 0.0_real64 .and. e3(3) == 1.0_real64, &
            "pf_bin_edges over a logical array spans 0 to 1")
        if (allocated(error)) return

        ! `pf_cov` over two columns: the one two-sample specific no test had reached, and the one
        ! whose entry point has to combine BOTH columns' own validity masks rather than one.
        call ca%init(PK_FLOAT64, 4_int64)
        call cb%init(PK_FLOAT64, 4_int64)
        do k = 1, 4
            call ca%set_at(int(k, int64), xd(k))
            call cb%set_at(int(k, int64), xd(5 - k))
        end do
        call pf_cov(xd, xd(4:1:-1), want)
        call pf_cov(ca, cb, got)
        call check(error, close_to(got, want), "pf_cov over two parquet_columns")
        if (allocated(error)) return

        ! `%update`'s logical and real32 arms.
        call acc%init()
        call acc%update(xs)
        call check(error, acc%n_valid() == 4_int64 .and. close_to(acc%mean(), 2.5_real64), &
            "%update over a real32 array")
        if (allocated(error)) return
        call acc%clear()
        call acc%init()
        call acc%update(xb)
        call check(error, acc%n_valid() == 4_int64 .and. close_to(acc%mean(), 0.75_real64), &
            "%update over a logical array")
    end subroutine test_every_kind_specific_is_reached

    ! ==================================================================================
    ! The pre-release round: the exhaustive kind sweep, the non-finite contract, and the
    ! arguments added while the API was still free to change.
    ! ==================================================================================

    !> EVERY generated entry point agrees across all six input kinds.
    !!
    !! **`test_every_kind_specific_is_reached` only SAMPLES, which is why this exists.** That test
    !! names a couple of dozen call sites by hand while the generated forwarding layer has roughly
    !! a hundred and eighty, and the gap is not hypothetical: a coverage run over this suite put
    !! `src/parquet_stats_kernel.f90` at 44%, with 94 procedures never executed. Every one of them
    !! was correct when checked by hand, and that is precisely the state worth protecting -- the
    !! layer is GENERATED, so its risk is a template change, and a template change breaks every
    !! kind at once rather than one.
    !!
    !! **The assertion is `==`, never a tolerance.** Each kind widens to `real64` and reaches the
    !! same engine, so the answers are not merely close, they are the same bits; a tolerance here
    !! would accept a forwarding bug that dropped an argument.
    !!
    !! The population is small integers, exactly representable in `real32`, `int32`, `int64` and
    !! `real64` alike, in a scrambled order -- so a bug that summarised an index range instead of
    !! the population could not agree by accident. `logical` cannot hold them and gets its own arm
    !! against the 1/0 mapping written out by hand.
    subroutine test_kind_sweep(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        integer, parameter :: N = 8
        real(real64) :: xd(N), yd(N), wd(N), want, got
        real(real32) :: xs(N), ys(N)
        integer(int32) :: xi(N), yi(N)
        integer(int64) :: xl(N), yl(N)
        type(parquet_column) :: cx, cy
        integer(int64) :: nw, ng
        integer :: f, k

        xd = [3.0_real64, 1.0_real64, 4.0_real64, 1.0_real64, 5.0_real64, 9.0_real64, &
              2.0_real64, 6.0_real64]
        yd = [2.0_real64, 7.0_real64, 1.0_real64, 8.0_real64, 2.0_real64, 8.0_real64, &
              1.0_real64, 4.0_real64]
        wd = [1.0_real64, 2.0_real64, 1.0_real64, 3.0_real64, 1.0_real64, 1.0_real64, &
              2.0_real64, 1.0_real64]
        xs = real(xd, real32);  ys = real(yd, real32)
        xi = int(xd, int32);    yi = int(yd, int32)
        xl = int(xd, int64);    yl = int(yd, int64)
        call cx%init(PK_FLOAT64, int(N, int64))
        call cy%init(PK_FLOAT64, int(N, int64))
        do k = 1, N
            call cx%set_at(int(k, int64), xd(k))
            call cy%set_at(int(k, int64), yd(k))
        end do

        do f = 1, NSCALAR
            call scalar_f64(f, xd, yd, wd, want)
            call scalar_r32(f, xs, ys, wd, got)
            call check(error, got == want, "real32 disagrees on " // trim(fam_name(f)))
            if (allocated(error)) return
            call scalar_i32(f, xi, yi, wd, got)
            call check(error, got == want, "int32 disagrees on " // trim(fam_name(f)))
            if (allocated(error)) return
            call scalar_i64(f, xl, yl, wd, got)
            call check(error, got == want, "int64 disagrees on " // trim(fam_name(f)))
            if (allocated(error)) return
            call scalar_col(f, cx, cy, wd, got)
            call check(error, got == want, "parquet_column disagrees on " // trim(fam_name(f)))
            if (allocated(error)) return
        end do

        ! `pf_count_valid` answers an integer, so it cannot ride the loop above.
        call pf_count_valid(xd, nw)
        call pf_count_valid(xs, ng); call check(error, ng == nw, "pf_count_valid over real32")
        if (allocated(error)) return
        call pf_count_valid(xi, ng); call check(error, ng == nw, "pf_count_valid over int32")
        if (allocated(error)) return
        call pf_count_valid(xl, ng); call check(error, ng == nw, "pf_count_valid over int64")
        if (allocated(error)) return
        call pf_count_valid(cx, ng); call check(error, ng == nw, "pf_count_valid over a column")
        if (allocated(error)) return

        call sweep_vectors(error, xd, xs, xi, xl, cx, yd, cy)
        if (allocated(error)) return
        call sweep_logical(error, wd)
        if (allocated(error)) return
        call sweep_probit_mean(error)
    end subroutine test_kind_sweep

    !> The scalar reductions over the `real64` reference population.
    subroutine scalar_f64(f, x, y, w, res)
        integer, intent(in) :: f              !! which family, 1..NSCALAR.
        real(real64), intent(in) :: x(:)      !! the population.
        real(real64), intent(in) :: y(:)      !! the second sample, for the pairwise families.
        real(real64), intent(in) :: w(:)      !! the weights, for the weighted arms.
        real(real64), intent(out) :: res      !! the answer.
        select case (f)
        case (1);  call pf_sum(x, res)
        case (2);  call pf_mean(x, res)
        case (3);  call pf_gmean(x, res)
        case (4);  call pf_hmean(x, res)
        case (5);  call pf_variance(x, res)
        case (6);  call pf_stddev(x, res)
        case (7);  call pf_sem(x, res)
        case (8);  call pf_skewness(x, res)
        case (9);  call pf_kurtosis(x, res)
        case (10); call pf_median(x, res)
        case (11); call pf_quantile(x, 0.3_real64, res)
        case (12); call pf_iqr(x, res)
        case (13); call pf_trim_mean(x, 0.25_real64, res)
        case (14); call pf_percentile_of_score(x, 4.0_real64, res)
        case (15); call pf_mad(x, res)
        case (16); call pf_mean(x, res, weights=w)
        case (17); call pf_probit_scale(x, res)
        end select
    end subroutine scalar_f64

    !> The same reductions through the `real(real32)` generic.
    subroutine scalar_r32(f, x, y, w, res)
        integer, intent(in) :: f              !! which family, 1..NSCALAR.
        real(real32), intent(in) :: x(:)      !! the population.
        real(real32), intent(in) :: y(:)      !! the second sample.
        real(real64), intent(in) :: w(:)      !! the weights.
        real(real64), intent(out) :: res      !! the answer.
        select case (f)
        case (1);  call pf_sum(x, res)
        case (2);  call pf_mean(x, res)
        case (3);  call pf_gmean(x, res)
        case (4);  call pf_hmean(x, res)
        case (5);  call pf_variance(x, res)
        case (6);  call pf_stddev(x, res)
        case (7);  call pf_sem(x, res)
        case (8);  call pf_skewness(x, res)
        case (9);  call pf_kurtosis(x, res)
        case (10); call pf_median(x, res)
        case (11); call pf_quantile(x, 0.3_real64, res)
        case (12); call pf_iqr(x, res)
        case (13); call pf_trim_mean(x, 0.25_real64, res)
        case (14); call pf_percentile_of_score(x, 4.0_real64, res)
        case (15); call pf_mad(x, res)
        case (16); call pf_mean(x, res, weights=w)
        case (17); call pf_probit_scale(x, res)
        end select
    end subroutine scalar_r32

    !> The same reductions through the `integer(int32)` generic.
    subroutine scalar_i32(f, x, y, w, res)
        integer, intent(in) :: f              !! which family, 1..NSCALAR.
        integer(int32), intent(in) :: x(:)    !! the population.
        integer(int32), intent(in) :: y(:)    !! the second sample.
        real(real64), intent(in) :: w(:)      !! the weights.
        real(real64), intent(out) :: res      !! the answer.
        select case (f)
        case (1);  call pf_sum(x, res)
        case (2);  call pf_mean(x, res)
        case (3);  call pf_gmean(x, res)
        case (4);  call pf_hmean(x, res)
        case (5);  call pf_variance(x, res)
        case (6);  call pf_stddev(x, res)
        case (7);  call pf_sem(x, res)
        case (8);  call pf_skewness(x, res)
        case (9);  call pf_kurtosis(x, res)
        case (10); call pf_median(x, res)
        case (11); call pf_quantile(x, 0.3_real64, res)
        case (12); call pf_iqr(x, res)
        case (13); call pf_trim_mean(x, 0.25_real64, res)
        case (14); call pf_percentile_of_score(x, 4.0_real64, res)
        case (15); call pf_mad(x, res)
        case (16); call pf_mean(x, res, weights=w)
        case (17); call pf_probit_scale(x, res)
        end select
    end subroutine scalar_i32

    !> The same reductions through the `integer(int64)` generic.
    subroutine scalar_i64(f, x, y, w, res)
        integer, intent(in) :: f              !! which family, 1..NSCALAR.
        integer(int64), intent(in) :: x(:)    !! the population.
        integer(int64), intent(in) :: y(:)    !! the second sample.
        real(real64), intent(in) :: w(:)      !! the weights.
        real(real64), intent(out) :: res      !! the answer.
        select case (f)
        case (1);  call pf_sum(x, res)
        case (2);  call pf_mean(x, res)
        case (3);  call pf_gmean(x, res)
        case (4);  call pf_hmean(x, res)
        case (5);  call pf_variance(x, res)
        case (6);  call pf_stddev(x, res)
        case (7);  call pf_sem(x, res)
        case (8);  call pf_skewness(x, res)
        case (9);  call pf_kurtosis(x, res)
        case (10); call pf_median(x, res)
        case (11); call pf_quantile(x, 0.3_real64, res)
        case (12); call pf_iqr(x, res)
        case (13); call pf_trim_mean(x, 0.25_real64, res)
        case (14); call pf_percentile_of_score(x, 4.0_real64, res)
        case (15); call pf_mad(x, res)
        case (16); call pf_mean(x, res, weights=w)
        case (17); call pf_probit_scale(x, res)
        end select
    end subroutine scalar_i64

    !> The same reductions through the scalar-numeric `parquet_column` generic.
    subroutine scalar_col(f, x, y, w, res)
        integer, intent(in) :: f                    !! which family, 1..NSCALAR.
        type(parquet_column), intent(in) :: x       !! the population.
        type(parquet_column), intent(in) :: y       !! the second sample.
        real(real64), intent(in) :: w(:)            !! the weights.
        real(real64), intent(out) :: res            !! the answer.
        select case (f)
        case (1);  call pf_sum(x, res)
        case (2);  call pf_mean(x, res)
        case (3);  call pf_gmean(x, res)
        case (4);  call pf_hmean(x, res)
        case (5);  call pf_variance(x, res)
        case (6);  call pf_stddev(x, res)
        case (7);  call pf_sem(x, res)
        case (8);  call pf_skewness(x, res)
        case (9);  call pf_kurtosis(x, res)
        case (10); call pf_median(x, res)
        case (11); call pf_quantile(x, 0.3_real64, res)
        case (12); call pf_iqr(x, res)
        case (13); call pf_trim_mean(x, 0.25_real64, res)
        case (14); call pf_percentile_of_score(x, 4.0_real64, res)
        case (15); call pf_mad(x, res)
        case (16); call pf_mean(x, res, weights=w)
        case (17); call pf_probit_scale(x, res)
        end select
    end subroutine scalar_col

    !> The family names, so a failure message says which reduction disagreed.
    pure function fam_name(f) result(res)
        integer, intent(in) :: f       !! which family, 1..NSCALAR.
        character(len=24) :: res       !! its public name, blank-padded.
        character(len=24), parameter :: NAMES(NSCALAR) = [ &
            "pf_sum                  ", "pf_mean                 ", "pf_gmean                ", &
            "pf_hmean                ", "pf_variance             ", "pf_stddev               ", &
            "pf_sem                  ", "pf_skewness             ", "pf_kurtosis             ", &
            "pf_median               ", "pf_quantile             ", "pf_iqr                  ", &
            "pf_trim_mean            ", "pf_percentile_of_score  ", "pf_mad                  ", &
            "pf_mean(weights=)       ", "pf_probit_scale         "]
        res = NAMES(f)
    end function fam_name

    !> The vector-valued, three-output, object-filling and two-sample families, across five kinds.
    !!
    !! Separate from the scalar loop because each returns something a `real(real64)` scalar cannot
    !! hold, so none of them can share the `select case` dispatch above.
    subroutine sweep_vectors(error, xd, xs, xi, xl, cx, yd, cy)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        real(real64), intent(in) :: xd(:)                   !! the real64 reference population.
        real(real32), intent(in) :: xs(:)                   !! the same values as real32.
        integer(int32), intent(in) :: xi(:)                 !! ... as int32.
        integer(int64), intent(in) :: xl(:)                 !! ... as int64.
        type(parquet_column), intent(in) :: cx              !! ... as a scalar float64 column.
        real(real64), intent(in) :: yd(:)                   !! the second sample, as real64.
        type(parquet_column), intent(in) :: cy              !! the second sample, as a column.
        real(real64) :: wv(size(xd)), gv(size(xd)), edges(4), wc(3), gc(3)
        real(real64) :: wm, wmd, wsd, gm, gmd, gsd
        integer(int32) :: wk(size(xd)), gk(size(xd))
        type(pf_stats) :: sw, sg
        integer :: c

        edges = [0.0_real64, 3.0_real64, 6.0_real64, 10.0_real64]

        call pf_zscore(xd, wv)
        call pf_zscore(xs, gv); call check(error, all(gv == wv), "pf_zscore over real32")
        if (allocated(error)) return
        call pf_zscore(xi, gv); call check(error, all(gv == wv), "pf_zscore over int32")
        if (allocated(error)) return
        call pf_zscore(xl, gv); call check(error, all(gv == wv), "pf_zscore over int64")
        if (allocated(error)) return
        call pf_zscore(cx, gv); call check(error, all(gv == wv), "pf_zscore over a column")
        if (allocated(error)) return

        ! The probit family's other three entry points. `pf_probit_scale` answers one `real64`
        ! and rides the scalar loop above; these do not -- `pf_normal_scores` fills a vector and
        ! `pf_probit_fit` fills three scalars at once. `pf_probit_mean` needs a population inside
        ! `[0, 1]`, which this one is not, and so gets its own arm (`sweep_probit_mean`).
        call pf_normal_scores(xd, wv)
        call pf_normal_scores(xs, gv)
        call check(error, all(gv == wv), "pf_normal_scores over real32")
        if (allocated(error)) return
        call pf_normal_scores(xi, gv)
        call check(error, all(gv == wv), "pf_normal_scores over int32")
        if (allocated(error)) return
        call pf_normal_scores(xl, gv)
        call check(error, all(gv == wv), "pf_normal_scores over int64")
        if (allocated(error)) return
        call pf_normal_scores(cx, gv)
        call check(error, all(gv == wv), "pf_normal_scores over a column")
        if (allocated(error)) return

        call pf_probit_fit(xd, wm, wmd, corr=wsd)
        call pf_probit_fit(xs, gm, gmd, corr=gsd)
        call check(error, gm == wm .and. gmd == wmd .and. gsd == wsd, "pf_probit_fit over real32")
        if (allocated(error)) return
        call pf_probit_fit(xi, gm, gmd, corr=gsd)
        call check(error, gm == wm .and. gmd == wmd .and. gsd == wsd, "pf_probit_fit over int32")
        if (allocated(error)) return
        call pf_probit_fit(xl, gm, gmd, corr=gsd)
        call check(error, gm == wm .and. gmd == wmd .and. gsd == wsd, "pf_probit_fit over int64")
        if (allocated(error)) return
        call pf_probit_fit(cx, gm, gmd, corr=gsd)
        call check(error, gm == wm .and. gmd == wmd .and. gsd == wsd, "pf_probit_fit over a column")
        if (allocated(error)) return

        do c = 1, 4
            call cum_f64(c, xd, wv)
            call cum_r32(c, xs, gv)
            call check(error, all(gv == wv), "real32 disagrees on " // trim(cum_name(c)))
            if (allocated(error)) return
            call cum_i32(c, xi, gv)
            call check(error, all(gv == wv), "int32 disagrees on " // trim(cum_name(c)))
            if (allocated(error)) return
            call cum_i64(c, xl, gv)
            call check(error, all(gv == wv), "int64 disagrees on " // trim(cum_name(c)))
            if (allocated(error)) return
            call cum_col(c, cx, gv)
            call check(error, all(gv == wv), "parquet_column disagrees on " // trim(cum_name(c)))
            if (allocated(error)) return
        end do

        call pf_histogram(xd, edges, wc)
        call pf_histogram(xs, edges, gc); call check(error, all(gc == wc), "pf_histogram real32")
        if (allocated(error)) return
        call pf_histogram(xi, edges, gc); call check(error, all(gc == wc), "pf_histogram int32")
        if (allocated(error)) return
        call pf_histogram(xl, edges, gc); call check(error, all(gc == wc), "pf_histogram int64")
        if (allocated(error)) return
        call pf_histogram(cx, edges, gc); call check(error, all(gc == wc), "pf_histogram column")
        if (allocated(error)) return
        call pf_bucketize(xd, edges, wk)
        call pf_bucketize(xs, edges, gk); call check(error, all(gk == wk), "pf_bucketize real32")
        if (allocated(error)) return
        call pf_bucketize(xi, edges, gk); call check(error, all(gk == wk), "pf_bucketize int32")
        if (allocated(error)) return
        call pf_bucketize(xl, edges, gk); call check(error, all(gk == wk), "pf_bucketize int64")
        if (allocated(error)) return
        call pf_bucketize(cx, edges, gk); call check(error, all(gk == wk), "pf_bucketize column")
        if (allocated(error)) return
        call pf_bin_edges(xd, 2, wc)
        call pf_bin_edges(xs, 2, gc); call check(error, all(gc == wc), "pf_bin_edges real32")
        if (allocated(error)) return
        call pf_bin_edges(xi, 2, gc); call check(error, all(gc == wc), "pf_bin_edges int32")
        if (allocated(error)) return
        call pf_bin_edges(xl, 2, gc); call check(error, all(gc == wc), "pf_bin_edges int64")
        if (allocated(error)) return
        call pf_bin_edges(cx, 2, gc); call check(error, all(gc == wc), "pf_bin_edges column")
        if (allocated(error)) return

        call pf_sigma_clipped_stats(xd, wm, wmd, wsd)
        call pf_sigma_clipped_stats(xs, gm, gmd, gsd)
        call check(error, gm == wm .and. gmd == wmd .and. gsd == wsd, "sigma clip over real32")
        if (allocated(error)) return
        call pf_sigma_clipped_stats(xi, gm, gmd, gsd)
        call check(error, gm == wm .and. gmd == wmd .and. gsd == wsd, "sigma clip over int32")
        if (allocated(error)) return
        call pf_sigma_clipped_stats(xl, gm, gmd, gsd)
        call check(error, gm == wm .and. gmd == wmd .and. gsd == wsd, "sigma clip over int64")
        if (allocated(error)) return
        call pf_sigma_clipped_stats(cx, gm, gmd, gsd)
        call check(error, gm == wm .and. gmd == wmd .and. gsd == wsd, "sigma clip over a column")
        if (allocated(error)) return

        call pf_describe(xd, sw)
        call pf_describe(xs, sg)
        call check(error, sg%mean() == sw%mean() .and. sg%median() == sw%median(), &
            "pf_describe over real32")
        if (allocated(error)) return
        call pf_describe(xi, sg)
        call check(error, sg%mean() == sw%mean() .and. sg%median() == sw%median(), &
            "pf_describe over int32")
        if (allocated(error)) return
        call pf_describe(xl, sg)
        call check(error, sg%mean() == sw%mean() .and. sg%median() == sw%median(), &
            "pf_describe over int64")
        if (allocated(error)) return
        call pf_describe(cx, sg)
        call check(error, sg%mean() == sw%mean() .and. sg%median() == sw%median(), &
            "pf_describe over a column")
        if (allocated(error)) return

        ! `pf_moments` and `pf_quantiles`, which return several values at once and so cannot ride
        ! the scalar loop either, and the two-sample pair over both arguments' kinds at once.
        call sweep_multis(error, xd, xs, xi, xl, cx, yd, cy)
    end subroutine sweep_vectors

    !> The multi-output and two-sample families, across five kinds.
    subroutine sweep_multis(error, xd, xs, xi, xl, cx, yd, cy)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        real(real64), intent(in) :: xd(:)                   !! the real64 reference population.
        real(real32), intent(in) :: xs(:)                   !! the same values as real32.
        integer(int32), intent(in) :: xi(:)                 !! ... as int32.
        integer(int64), intent(in) :: xl(:)                 !! ... as int64.
        type(parquet_column), intent(in) :: cx              !! ... as a scalar float64 column.
        real(real64), intent(in) :: yd(:)                   !! the second sample, as real64.
        type(parquet_column), intent(in) :: cy              !! the same second sample, as a column.
        real(real64) :: probs(3), wq(3), gq(3), wm(4), gm(4), want, got

        probs = [0.1_real64, 0.5_real64, 0.9_real64]

        call pf_quantiles(xd, probs, wq)
        call pf_quantiles(xs, probs, gq); call check(error, all(gq == wq), "pf_quantiles real32")
        if (allocated(error)) return
        call pf_quantiles(xi, probs, gq); call check(error, all(gq == wq), "pf_quantiles int32")
        if (allocated(error)) return
        call pf_quantiles(xl, probs, gq); call check(error, all(gq == wq), "pf_quantiles int64")
        if (allocated(error)) return
        call pf_quantiles(cx, probs, gq); call check(error, all(gq == wq), "pf_quantiles column")
        if (allocated(error)) return

        call pf_moments(xd, mean=wm(1), variance=wm(2), skewness=wm(3), vmax=wm(4))
        call pf_moments(xs, mean=gm(1), variance=gm(2), skewness=gm(3), vmax=gm(4))
        call check(error, all(gm == wm), "pf_moments over real32")
        if (allocated(error)) return
        call pf_moments(xi, mean=gm(1), variance=gm(2), skewness=gm(3), vmax=gm(4))
        call check(error, all(gm == wm), "pf_moments over int32")
        if (allocated(error)) return
        call pf_moments(xl, mean=gm(1), variance=gm(2), skewness=gm(3), vmax=gm(4))
        call check(error, all(gm == wm), "pf_moments over int64")
        if (allocated(error)) return
        call pf_moments(cx, mean=gm(1), variance=gm(2), skewness=gm(3), vmax=gm(4))
        call check(error, all(gm == wm), "pf_moments over a column")
        if (allocated(error)) return

        ! The second sample is a DIFFERENT population, not a copy of the first: a covariance of a
        ! sample with itself is the variance and a correlation is exactly 1, either of which would
        ! agree across kinds for a reason that has nothing to do with the forwarding under test.
        call pf_cov(xd, yd, want)
        call pf_cov(xs, real(yd, real32), got); call check(error, got == want, "pf_cov real32")
        if (allocated(error)) return
        call pf_cov(xi, int(yd, int32), got); call check(error, got == want, "pf_cov int32")
        if (allocated(error)) return
        call pf_cov(xl, int(yd, int64), got); call check(error, got == want, "pf_cov int64")
        if (allocated(error)) return
        call pf_cov(cx, cy, got); call check(error, got == want, "pf_cov over two columns")
        if (allocated(error)) return
        call pf_corr(xd, yd, want)
        call pf_corr(xs, real(yd, real32), got); call check(error, got == want, "pf_corr real32")
        if (allocated(error)) return
        call pf_corr(xi, int(yd, int32), got); call check(error, got == want, "pf_corr int32")
        if (allocated(error)) return
        call pf_corr(xl, int(yd, int64), got); call check(error, got == want, "pf_corr int64")
        if (allocated(error)) return
        call pf_corr(cx, cy, got); call check(error, got == want, "pf_corr over two columns")
    end subroutine sweep_multis

    !> The four running folds over the `real64` reference population.
    subroutine cum_f64(c, x, out)
        integer, intent(in) :: c                !! which fold, 1..4.
        real(real64), intent(in) :: x(:)        !! the population.
        real(real64), intent(out) :: out(:)     !! the running value.
        select case (c)
        case (1); call pf_cumsum(x, out)
        case (2); call pf_cumprod(x, out)
        case (3); call pf_cummax(x, out)
        case (4); call pf_cummin(x, out)
        end select
    end subroutine cum_f64

    !> The four running folds through the `real(real32)` generic.
    subroutine cum_r32(c, x, out)
        integer, intent(in) :: c                !! which fold, 1..4.
        real(real32), intent(in) :: x(:)        !! the population.
        real(real64), intent(out) :: out(:)     !! the running value.
        select case (c)
        case (1); call pf_cumsum(x, out)
        case (2); call pf_cumprod(x, out)
        case (3); call pf_cummax(x, out)
        case (4); call pf_cummin(x, out)
        end select
    end subroutine cum_r32

    !> The four running folds through the `integer(int32)` generic.
    subroutine cum_i32(c, x, out)
        integer, intent(in) :: c                !! which fold, 1..4.
        integer(int32), intent(in) :: x(:)      !! the population.
        real(real64), intent(out) :: out(:)     !! the running value.
        select case (c)
        case (1); call pf_cumsum(x, out)
        case (2); call pf_cumprod(x, out)
        case (3); call pf_cummax(x, out)
        case (4); call pf_cummin(x, out)
        end select
    end subroutine cum_i32

    !> The four running folds through the `integer(int64)` generic.
    subroutine cum_i64(c, x, out)
        integer, intent(in) :: c                !! which fold, 1..4.
        integer(int64), intent(in) :: x(:)      !! the population.
        real(real64), intent(out) :: out(:)     !! the running value.
        select case (c)
        case (1); call pf_cumsum(x, out)
        case (2); call pf_cumprod(x, out)
        case (3); call pf_cummax(x, out)
        case (4); call pf_cummin(x, out)
        end select
    end subroutine cum_i64

    !> The four running folds through the scalar-numeric `parquet_column` generic.
    subroutine cum_col(c, x, out)
        integer, intent(in) :: c                    !! which fold, 1..4.
        type(parquet_column), intent(in) :: x       !! the population.
        real(real64), intent(out) :: out(:)         !! the running value.
        select case (c)
        case (1); call pf_cumsum(x, out)
        case (2); call pf_cumprod(x, out)
        case (3); call pf_cummax(x, out)
        case (4); call pf_cummin(x, out)
        end select
    end subroutine cum_col

    !> The running folds' names, for a failure message that says which one disagreed.
    pure function cum_name(c) result(res)
        integer, intent(in) :: c       !! which fold, 1..4.
        character(len=12) :: res       !! its public name, blank-padded.
        character(len=12), parameter :: NAMES(4) = [ &
            "pf_cumsum   ", "pf_cumprod  ", "pf_cummax   ", "pf_cummin   "]
        res = NAMES(c)
    end function cum_name

    !> The logical arm, against the 1/0 mapping written out as `real64`.
    !!
    !! A logical population cannot hold the sweep's own values, so its reference is built here
    !! rather than shared -- which is the whole reason this kind needs a separate arm at all.
    subroutine sweep_logical(error, w)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        real(real64), intent(in) :: w(:)                    !! the weights, for the weighted arm.
        integer, parameter :: N = 8
        logical :: xb(N)
        real(real64) :: ref(N), want, got, wv(N), gv(N), edges(4), wc(3), gc(3)
        integer(int32) :: wk(N), gk(N)
        real(real64) :: wm, wmd, wsd, gm, gmd, gsd
        type(pf_stats) :: sw, sg
        integer(int64) :: nw, ng
        integer :: f, c, k

        xb = [.true., .true., .false., .true., .false., .true., .true., .false.]
        do k = 1, N
            ref(k) = merge(1.0_real64, 0.0_real64, xb(k))
        end do
        edges = [-0.5_real64, 0.5_real64, 1.5_real64, 2.5_real64]

        ! `pf_gmean` and `pf_hmean` are omitted: a population containing a zero makes both exactly
        ! 0 by their documented domain rule, so they would agree for a reason that has nothing to
        ! do with the forwarding under test. `test_every_kind_specific_is_reached` covers the
        ! all-true case, where they are 1.
        do f = 1, NSCALAR
            if (f == 3 .or. f == 4) cycle
            call scalar_f64(f, ref, ref, w, want)
            call scalar_bool(f, xb, xb, w, got)
            call check(error, got == want, "logical disagrees on " // trim(fam_name(f)))
            if (allocated(error)) return
        end do
        call pf_count_valid(ref, nw)
        call pf_count_valid(xb, ng); call check(error, ng == nw, "pf_count_valid over logical")
        if (allocated(error)) return

        call pf_zscore(ref, wv)
        call pf_zscore(xb, gv); call check(error, all(gv == wv), "pf_zscore over logical")
        if (allocated(error)) return
        call pf_normal_scores(ref, wv)
        call pf_normal_scores(xb, gv)
        call check(error, all(gv == wv), "pf_normal_scores over logical")
        if (allocated(error)) return
        call pf_probit_fit(ref, wm, wmd, corr=wsd)
        call pf_probit_fit(xb, gm, gmd, corr=gsd)
        call check(error, gm == wm .and. gmd == wmd .and. gsd == wsd, "pf_probit_fit over logical")
        if (allocated(error)) return
        do c = 1, 4
            call cum_f64(c, ref, wv)
            call cum_bool(c, xb, gv)
            call check(error, all(gv == wv), "logical disagrees on " // trim(cum_name(c)))
            if (allocated(error)) return
        end do
        call pf_histogram(ref, edges, wc)
        call pf_histogram(xb, edges, gc); call check(error, all(gc == wc), "pf_histogram logical")
        if (allocated(error)) return
        call pf_bucketize(ref, edges, wk)
        call pf_bucketize(xb, edges, gk); call check(error, all(gk == wk), "pf_bucketize logical")
        if (allocated(error)) return
        call pf_bin_edges(ref, 2, wc)
        call pf_bin_edges(xb, 2, gc); call check(error, all(gc == wc), "pf_bin_edges logical")
        if (allocated(error)) return
        call pf_sigma_clipped_stats(ref, wm, wmd, wsd)
        call pf_sigma_clipped_stats(xb, gm, gmd, gsd)
        call check(error, gm == wm .and. gmd == wmd .and. gsd == wsd, "sigma clip over logical")
        if (allocated(error)) return
        call pf_describe(ref, sw)
        call pf_describe(xb, sg)
        call check(error, sg%mean() == sw%mean() .and. sg%median() == sw%median(), &
            "pf_describe over logical")
        if (allocated(error)) return
        call pf_quantiles(ref, [0.1_real64, 0.5_real64, 0.9_real64], wc)
        call pf_quantiles(xb, [0.1_real64, 0.5_real64, 0.9_real64], gc)
        call check(error, all(gc == wc), "pf_quantiles over logical")
        if (allocated(error)) return
        call pf_moments(ref, mean=wc(1), variance=wc(2), skewness=wc(3))
        call pf_moments(xb, mean=gc(1), variance=gc(2), skewness=gc(3))
        call check(error, all(gc == wc), "pf_moments over logical")
        if (allocated(error)) return
        call pf_cov(ref, ref, want)
        call pf_cov(xb, xb, got); call check(error, got == want, "pf_cov over logical")
        if (allocated(error)) return
        call pf_corr(ref, ref, want)
        call pf_corr(xb, xb, got); call check(error, got == want, "pf_corr over logical")
    end subroutine sweep_logical

    !> `pf_probit_mean` across all six kinds, on populations its DOMAIN admits.
    !!
    !! It cannot ride `test_kind_sweep`'s scalar loop, and that is a property of the statistic
    !! rather than an inconvenience: the loop's population is `[3, 1, 4, ...]`, every value
    !! outside `[0, 1]` gives a quiet NaN, and `NaN == NaN` is false -- so the sweep's own
    !! assertion would fail on an answer that is entirely correct.
    !!
    !! The real kinds therefore take dyadic fractions, which are exact in `real32` as in
    !! `real64`; the widening is lossless and the two must agree BIT FOR BIT rather than nearly,
    !! which is what makes `==` the right assertion here as it is there.
    !!
    !! `integer` and `logical` populations can hold only 0 and 1, so they take the two LIMITS
    !! the documentation promises exactly -- an all-zero population is exactly 0 and an all-one
    !! population exactly 1 -- and a mixed one is NaN because that limit is `-Inf + Inf`. Those
    !! are the only in-domain populations those three kinds have, and asserting them is what
    !! reaches their forwarding specifics at all.
    subroutine sweep_probit_mean(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        integer, parameter :: N = 6
        real(real64) :: pd(N), want, got
        real(real32) :: ps(N)
        integer(int32) :: zi(N), oi(N)
        integer(int64) :: zl(N), ol(N)
        logical :: zb(N), ob(N)
        type(parquet_column) :: cp
        integer :: k

        pd = [0.25_real64, 0.5_real64, 0.75_real64, 0.125_real64, 0.375_real64, 0.625_real64]
        ps = real(pd, real32)
        call check(error, all(real(ps, real64) == pd), &
            "the fixture is not exact in real32 -- a bit-for-bit comparison below would be unfair")
        if (allocated(error)) return
        call cp%init(PK_FLOAT64, int(N, int64))
        do k = 1, N
            call cp%set_at(int(k, int64), pd(k))
        end do

        call pf_probit_mean(pd, want)
        call check(error, want > 0.0_real64 .and. want < 1.0_real64, &
            "the fixture's probit mean is outside (0, 1) -- the comparisons below prove nothing")
        if (allocated(error)) return
        call pf_probit_mean(ps, got)
        call check(error, got == want, "pf_probit_mean over real32")
        if (allocated(error)) return
        call pf_probit_mean(cp, got)
        call check(error, got == want, "pf_probit_mean over a column")
        if (allocated(error)) return

        zi = 0_int32;   oi = 1_int32
        zl = 0_int64;   ol = 1_int64
        zb = .false.;   ob = .true.
        call pf_probit_mean(zi, got)
        call check(error, got == 0.0_real64, "an all-zero int32 population is exactly 0")
        if (allocated(error)) return
        call pf_probit_mean(oi, got)
        call check(error, got == 1.0_real64, "an all-one int32 population is exactly 1")
        if (allocated(error)) return
        call pf_probit_mean(zl, got)
        call check(error, got == 0.0_real64, "an all-zero int64 population is exactly 0")
        if (allocated(error)) return
        call pf_probit_mean(ol, got)
        call check(error, got == 1.0_real64, "an all-one int64 population is exactly 1")
        if (allocated(error)) return
        call pf_probit_mean(zb, got)
        call check(error, got == 0.0_real64, "an all-false logical population is exactly 0")
        if (allocated(error)) return
        call pf_probit_mean(ob, got)
        call check(error, got == 1.0_real64, "an all-true logical population is exactly 1")
        if (allocated(error)) return

        ! The mixed case, which is the reason the two above are separate populations rather than
        ! one: both limits at once is `-Inf + Inf`, and every kind must answer the same NaN.
        zi(1) = 1_int32
        call pf_probit_mean(zi, got)
        call check(error, ieee_is_nan(got), "0 and 1 together give a NaN, whatever the kind")
    end subroutine sweep_probit_mean

    !> The scalar reductions through the `logical` generic.
    subroutine scalar_bool(f, x, y, w, res)
        integer, intent(in) :: f              !! which family, 1..NSCALAR.
        logical, intent(in) :: x(:)           !! the population.
        logical, intent(in) :: y(:)           !! the second sample.
        real(real64), intent(in) :: w(:)      !! the weights.
        real(real64), intent(out) :: res      !! the answer.
        select case (f)
        case (1);  call pf_sum(x, res)
        case (2);  call pf_mean(x, res)
        case (3);  call pf_gmean(x, res)
        case (4);  call pf_hmean(x, res)
        case (5);  call pf_variance(x, res)
        case (6);  call pf_stddev(x, res)
        case (7);  call pf_sem(x, res)
        case (8);  call pf_skewness(x, res)
        case (9);  call pf_kurtosis(x, res)
        case (10); call pf_median(x, res)
        case (11); call pf_quantile(x, 0.3_real64, res)
        case (12); call pf_iqr(x, res)
        case (13); call pf_trim_mean(x, 0.25_real64, res)
        case (14); call pf_percentile_of_score(x, 4.0_real64, res)
        case (15); call pf_mad(x, res)
        case (16); call pf_mean(x, res, weights=w)
        case (17); call pf_probit_scale(x, res)
        end select
    end subroutine scalar_bool

    !> The four running folds through the `logical` generic.
    subroutine cum_bool(c, x, out)
        integer, intent(in) :: c                !! which fold, 1..4.
        logical, intent(in) :: x(:)             !! the population.
        real(real64), intent(out) :: out(:)     !! the running value.
        select case (c)
        case (1); call pf_cumsum(x, out)
        case (2); call pf_cumprod(x, out)
        case (3); call pf_cummax(x, out)
        case (4); call pf_cummin(x, out)
        end select
    end subroutine cum_bool

    !> A non-finite value never aborts, and answers what numpy answers.
    !!
    !! **This is the gap the whole family had.** Weights are validated as finite and that abort is
    !! tested; `pf_mad`'s `center=` and `pf_percentile_of_score`'s `score=` are validated and both
    !! aborts are tested. The VALUES array was not, and no fixture in
    !! `tools/generate_stats_vectors.py` contained an infinity -- which is why four separate
    !! defects lived in this corner at once, all reachable from an ordinary data condition (an
    !! upstream division by zero, a `log(0)`, an overflowed unit conversion).
    !!
    !! The contract asserted here, against numpy 2.5 and pandas 3.0 on the same inputs:
    !!
    !! * `+Inf` in the population makes the SUM and the MEAN `+Inf`, with `ok = .true.` -- an
    !!   infinity is a value, not a failure, and `pf_sum` and `pf_mean` must not disagree about
    !!   one population. This is what the two-pass mean refinement used to break: `Inf - Inf` in
    !!   the correction term turned a correct `+Inf` into a NaN;
    !! * every CENTRAL MOMENT is then NaN, which is numpy's answer too -- every deviation from an
    !!   infinite mean is infinite, and the infinite element's own is `Inf - Inf`;
    !! * both signs of infinity present makes even the mean NaN, again as numpy;
    !! * `pf_sigma_clipped_stats` reports `ok = .false.` when ANY of its three outputs is a NaN.
    !!   It is the family's only three-output procedure and they do not fail together: the clip
    !!   keeps a finite median beside an infinite mean and a NaN standard deviation.
    !!
    !! Nothing here may abort: an infinity is a data condition, and this module aborts only on
    !! misuse.
    subroutine test_non_finite_values(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        real(real64) :: x(7), s, m, v, sd, med, nan, inf, ninf
        logical :: ok

        nan = ieee_value(1.0_real64, ieee_quiet_nan)
        inf = ieee_value(1.0_real64, ieee_positive_inf)
        ninf = -inf
        ! Built at run time rather than as a constant expression: a constant `Inf` is not
        ! portable, and a compiler that folds it may fold the arithmetic under test with it.
        call check(error, .not. ieee_is_nan(inf) .and. inf > huge(1.0_real64), &
            "the fixture's +Inf is not an infinity -- the assertions below prove nothing")
        if (allocated(error)) return

        x = [1.0_real64, 2.0_real64, inf, 4.0_real64, 5.0_real64, 6.0_real64, 7.0_real64]
        call pf_sum(x, s, ok=ok)
        call check(error, s > huge(1.0_real64) .and. ok, "pf_sum over +Inf must be +Inf, ok=.true.")
        if (allocated(error)) return
        call pf_mean(x, m, ok=ok)
        call check(error, m > huge(1.0_real64) .and. ok, &
            "pf_mean over +Inf must be +Inf with ok=.true., as numpy and pandas answer")
        if (allocated(error)) return
        call pf_variance(x, v, ok=ok)
        call check(error, ieee_is_nan(v) .and. .not. ok, &
            "pf_variance over +Inf must be NaN with ok=.false., as numpy answers")
        if (allocated(error)) return
        call pf_median(x, med, ok=ok)
        call check(error, med == 5.0_real64 .and. ok, &
            "pf_median is an order statistic and is unmoved by an infinity")
        if (allocated(error)) return

        ! Only -Inf: the mirror, so the sign is not being lost somewhere.
        x(3) = ninf
        call pf_mean(x, m, ok=ok)
        call check(error, m < -huge(1.0_real64) .and. ok, "pf_mean over -Inf must be -Inf")
        if (allocated(error)) return

        ! Both signs: Inf - Inf, so even the sum is undefined. numpy answers nan here too.
        x(3) = inf
        x(4) = ninf
        call pf_sum(x, s, ok=ok)
        call check(error, ieee_is_nan(s) .and. .not. ok, "both infinities make the sum NaN")
        if (allocated(error)) return
        call pf_mean(x, m, ok=ok)
        call check(error, ieee_is_nan(m) .and. .not. ok, "both infinities make the mean NaN")
        if (allocated(error)) return

        ! The sigma clip's three outputs, which do NOT fail together.
        x = [1.0_real64, 2.0_real64, inf, 4.0_real64, 5.0_real64, 6.0_real64, 7.0_real64]
        call pf_sigma_clipped_stats(x, m, med, sd, ok=ok)
        call check(error, .not. ok, &
            "pf_sigma_clipped_stats must report ok=.false. when any of its three outputs is NaN")
        if (allocated(error)) return
        call check(error, ieee_is_nan(sd) .and. .not. ieee_is_nan(med), &
            "the clip's stddev is the NaN and its median is not -- the case ok= has to catch")
        if (allocated(error)) return

        ! The negative control for that flag: an ordinary population must still report .true.
        call pf_sigma_clipped_stats([1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64, 5.0_real64], &
            m, med, sd, ok=ok)
        call check(error, ok, "a clean population must still leave the sigma clip's ok .true.")
        if (allocated(error)) return

        ! A NaN kept under skipnan=.false. is the family's own poisoning rule and is unchanged.
        x = [1.0_real64, 2.0_real64, nan, 4.0_real64, 5.0_real64, 6.0_real64, 7.0_real64]
        call pf_mean(x, m, skipnan=.false., ok=ok)
        call check(error, ieee_is_nan(m) .and. .not. ok, "a kept NaN still poisons the mean")
    end subroutine test_non_finite_values

    !> `pf_bin_edges` survives an infinity, and `pf_histogram` still accepts what it emits.
    !!
    !! **`test_bin_edges_are_always_usable` asserts exactly this property and did not cover this
    !! input**, which is how the contract came to be violated by a single `+Inf`: the range scan
    !! excluded nulls, NaNs and zero-weighted elements but not infinities, so `span` was infinite,
    !! every interior edge came out `Infinity`, and the repair -- a one-ulp nudge -- cannot
    !! separate two infinities. The result was edges `pf_histogram` ABORTED on, one call after the
    !! call documented to produce them.
    !!
    !! Five fixtures, each of which used to break it or is the control for one that did. The
    !! assertion is the same every time and is the contract itself: strictly increasing, and
    !! accepted by `pf_histogram` without aborting.
    subroutine test_bin_edges_survive_non_finite(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        real(real64) :: x(6), be(5), counts(4), inf, nan
        integer(int64) :: nout
        logical :: ok
        integer :: k, f

        inf = ieee_value(1.0_real64, ieee_positive_inf)
        nan = ieee_value(1.0_real64, ieee_quiet_nan)

        do f = 1, 5
            x = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64, 5.0_real64, 6.0_real64]
            select case (f)
            case (1); x(3) = inf                       ! one +Inf
            case (2); x(3) = -inf                      ! one -Inf
            case (3); x(3) = inf;  x(4) = -inf         ! both signs: span would be NaN
            case (4); x = inf                          ! nothing finite at all
            case (5); x(3) = nan                       ! a NaN kept by skipnan=.false. below
            end select
            if (f == 5) then
                call pf_bin_edges(x, 4, be, skipnan=.false., ok=ok)
            else
                call pf_bin_edges(x, 4, be, ok=ok)
            end if
            do k = 2, 5
                call check(error, be(k) > be(k - 1), &
                    "pf_bin_edges emitted a non-increasing pair on a non-finite fixture")
                if (allocated(error)) return
            end do
            call check(error, .not. ok, &
                "a non-finite population must report ok=.false. -- the edges do not span it")
            if (allocated(error)) return
            ! The whole point: the documented next call must not abort. It would, on edges that
            ! are not strictly increasing, and that abort is what this test exists to prevent.
            if (f == 5) then
                call pf_histogram(x, be, counts, skipnan=.false., n_outside=nout)
            else
                call pf_histogram(x, be, counts, n_outside=nout)
            end if
            call check(error, sum(counts) >= 0.0_real64, &
                "pf_histogram did not accept the edges pf_bin_edges produced")
            if (allocated(error)) return
        end do

        ! The infinite element itself reaches no bin, which is the honest answer -- it is outside
        ! every finite one -- and is reported rather than silently dropped.
        x = [1.0_real64, 2.0_real64, inf, 4.0_real64, 5.0_real64, 6.0_real64]
        call pf_bin_edges(x, 4, be)
        call pf_histogram(x, be, counts, n_outside=nout)
        call check(error, nout == 1_int64 .and. sum(counts) == 5.0_real64, &
            "the infinity must be counted in n_outside, not into a bin")
        if (allocated(error)) return

        ! The negative control: an ordinary population still reports ok=.true., so the flag above
        ! is not simply always .false.
        call pf_bin_edges([1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64], 4, be, ok=ok)
        call check(error, ok, "a finite population must still leave pf_bin_edges' ok .true.")
    end subroutine test_bin_edges_survive_non_finite

    !> `pf_histogram` IS `pf_bucketize` tallied, WITH weights too.
    !!
    !! `test_histogram_is_bucketize` asserts the identity on both edge conventions and exercises
    !! the unweighted case only -- and the identity was FALSE with weights, because `pf_bucketize`
    !! had no `weights` argument at all. It could therefore not apply the module's own "a zero
    !! weight removes the element" rule, so it handed a zero-weighted element an ordinary bin code
    !! while `pf_histogram` over the same arguments left it out.
    !!
    !! The tally is over WEIGHTS rather than occurrences, which is the identity's weighted form: a
    !! weight cannot change a bin number, only membership.
    subroutine test_weighted_bucketize_identity(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        integer, parameter :: N = 8
        real(real64) :: x(N), w(N), edges(4), counts(3), tally(3), nan
        integer(int32) :: codes(N)
        integer(int64) :: i, nz
        logical :: right

        nan = ieee_value(1.0_real64, ieee_quiet_nan)
        x = [0.5_real64, 1.5_real64, 2.5_real64, 3.5_real64, 4.5_real64, 5.5_real64, nan, 6.5_real64]
        ! A zero weight in the middle, so the element it removes is one an unweighted tally would
        ! have placed -- the case that used to disagree.
        w = [1.0_real64, 0.0_real64, 1.0_real64, 1.0_real64, 3.0_real64, 1.0_real64, 1.0_real64, &
             1.0_real64]
        edges = [0.0_real64, 2.0_real64, 4.0_real64, 6.0_real64]

        do i = 0_int64, 1_int64
            right = (i == 1_int64)
            call pf_histogram(x, edges, counts, right=right, weights=w)
            call pf_bucketize(x, edges, codes, right=right, weights=w)
            tally = 0.0_real64
            do nz = 1_int64, int(N, int64)
                if (codes(nz) > 0_int32) tally(codes(nz)) = tally(codes(nz)) + w(nz)
            end do
            call check(error, all(tally == counts), &
                "the weighted histogram is not the weighted bucketize tally")
            if (allocated(error)) return
        end do

        ! The zero-weighted element must carry code 0, exactly as a null or an out-of-range value
        ! does -- it left the population, so it reached no bin.
        call pf_bucketize(x, edges, codes, weights=w)
        call check(error, codes(2) == 0_int32, &
            "a zero-weighted element must bucketize to 0, as pf_histogram excludes it")
        if (allocated(error)) return

        ! The negative control: without weights that same element takes an ordinary code, so the
        ! assertion above is about the weight and not about the value.
        call pf_bucketize(x, edges, codes)
        call check(error, codes(2) == 1_int32, &
            "unweighted, that element must take its ordinary bin -- the control for the above")
    end subroutine test_weighted_bucketize_identity

    !> `pf_count_valid` reports why every element left.
    !!
    !! The three counts plus the survivors account for every element except the zero-weighted
    !! ones, and that difference IS what a zero weight is -- which is the only way a caller can
    !! recover it, since nothing reports it directly.
    subroutine test_count_valid_why(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        real(real64) :: x(8), w(8), nan
        logical :: mask(8)
        integer(int64) :: n, nn, nx

        nan = ieee_value(1.0_real64, ieee_quiet_nan)
        x = [1.0_real64, 2.0_real64, nan, 4.0_real64, nan, 6.0_real64, 7.0_real64, 8.0_real64]
        mask = [.true., .false., .true., .true., .false., .true., .true., .true.]
        w = [1.0_real64, 1.0_real64, 1.0_real64, 0.0_real64, 1.0_real64, 1.0_real64, 1.0_real64, &
             0.0_real64]

        ! Element 5 is BOTH null and NaN: the exclusion order is nullness first, so it counts as a
        ! null and never as a NaN. Without that rule the two counts would sum to more than the
        ! elements removed, and the accounting identity below would not hold.
        call pf_count_valid(x, n, is_valid=mask, weights=w, n_null=nn, n_nan=nx)
        call check(error, nn == 2_int64, "n_null must count both is_valid=.false. elements")
        if (allocated(error)) return
        call check(error, nx == 1_int64, &
            "n_nan must count only the NaN that was not ALREADY null -- the exclusion order")
        if (allocated(error)) return
        call check(error, n == 3_int64, "three elements survive: 1, 6 and 7")
        if (allocated(error)) return
        call check(error, size(x, kind=int64) - nn - nx - n == 2_int64, &
            "the accounting identity: what is left over is exactly the zero-weighted elements")
        if (allocated(error)) return

        ! Both outputs are optional and independent, so each is asked for on its own too.
        call pf_count_valid(x, n, is_valid=mask, n_null=nn)
        call check(error, nn == 2_int64, "n_null alone must still answer")
        if (allocated(error)) return
        call pf_count_valid(x, n, n_nan=nx)
        call check(error, nx == 2_int64, "n_nan alone, with no mask, sees both NaNs")
        if (allocated(error)) return

        ! The integer kinds carry no `n_nan`, by construction -- there is no NaN to count -- so
        ! this is the arm that proves `n_null` reaches them.
        call pf_count_valid([1_int32, 2_int32, 3_int32], n, &
            is_valid=[.true., .false., .true.], n_null=nn)
        call check(error, n == 2_int64 .and. nn == 1_int64, "n_null over an int32 population")
    end subroutine test_count_valid_why

    !> `pf_moments` and `pf_describe` report `ok` over the outputs actually asked for.
    !!
    !! `pf_moments` has nine optional outputs, so "is the answer usable" has only one workable
    !! reading: test the PRESENT ones. Testing all nine would report a failure to a caller who
    !! asked for the sum of an empty population and got the correct `0`; testing none of them
    !! would make the argument useless. `pf_describe` fills an object instead, so its `ok` is
    !! about the population rather than about a selection.
    subroutine test_aggregate_ok(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        real(real64) :: x(5), m, v, s, nan
        type(pf_stats) :: acc
        logical :: ok

        nan = ieee_value(1.0_real64, ieee_quiet_nan)
        x = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64, 5.0_real64]

        call pf_moments(x, mean=m, variance=v, ok=ok)
        call check(error, ok, "a clean population must leave pf_moments' ok .true.")
        if (allocated(error)) return

        ! One element at the default ddof=1: the variance is NaN and the mean is not, so `ok`
        ! must follow the variance -- and must NOT, when the variance was not asked for.
        call pf_moments(x(1:1), mean=m, variance=v, ok=ok)
        call check(error, .not. ok, "an undefined variance must make pf_moments' ok .false.")
        if (allocated(error)) return
        call pf_moments(x(1:1), mean=m, ok=ok)
        call check(error, ok, &
            "asking only for a defined mean must leave ok .true. -- only PRESENT outputs count")
        if (allocated(error)) return

        ! The empty sum is exactly 0 and is defined, so a caller asking only for it is not failing.
        call pf_moments(x(1:0), vsum=s, ok=ok)
        call check(error, ok .and. s == 0.0_real64, &
            "an empty population's sum is a correct 0, so ok stays .true. for it alone")
        if (allocated(error)) return
        call pf_moments(x(1:0), vsum=s, mean=m, ok=ok)
        call check(error, .not. ok, "... but its mean is NaN, so asking for both makes ok .false.")
        if (allocated(error)) return

        ! `%range` is the object's one extremes binding nothing else in this suite reached.
        call acc%compute(x)
        call check(error, acc%range() == acc%vmax() - acc%vmin(), "%range is vmax - vmin")
        if (allocated(error)) return

        call pf_describe(x, acc, ok=ok)
        call check(error, ok, "a clean population must leave pf_describe's ok .true.")
        if (allocated(error)) return
        call pf_describe(x(1:0), acc, ok=ok)
        call check(error, .not. ok, "an empty population must make pf_describe's ok .false.")
        if (allocated(error)) return
        call pf_describe([1.0_real64, nan, 3.0_real64], acc, skipnan=.false., ok=ok)
        call check(error, .not. ok, "a kept NaN must make pf_describe's ok .false. too")
    end subroutine test_aggregate_ok

    !> `%gmean` and `%hmean` answer exactly what the one-shot forms do.
    !!
    !! They are the two queries that need the RETAINED values rather than the accumulator: a
    !! log-sum is a fifth quantity the four central moments do not contain. That is why they abort
    !! on a streaming accumulator (`stats_object_gmean_without_retain` in test/error_scenarios.f90)
    !! and why they order nothing, unlike every other query that needs the buffer.
    subroutine test_object_power_means(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        real(real64) :: x(6), w(6), g, h
        type(pf_stats) :: acc

        x = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64, 5.0_real64, 6.0_real64]
        w = [1.0_real64, 2.0_real64, 1.0_real64, 3.0_real64, 1.0_real64, 1.0_real64]

        call acc%compute(x)
        call pf_gmean(x, g)
        call check(error, acc%gmean() == g, "%gmean must equal pf_gmean bit for bit")
        if (allocated(error)) return
        call pf_hmean(x, h)
        call check(error, acc%hmean() == h, "%hmean must equal pf_hmean bit for bit")
        if (allocated(error)) return

        call acc%compute(x, weights=w)
        call pf_gmean(x, g, weights=w)
        call check(error, acc%gmean() == g, "%gmean must equal pf_gmean under weights")
        if (allocated(error)) return
        call pf_hmean(x, h, weights=w)
        call check(error, acc%hmean() == h, "%hmean must equal pf_hmean under weights")
        if (allocated(error)) return

        ! **They must not order anything.** That is the property separating them from every other
        ! query needing the retained buffer, and the only way to see it is the sort counter.
        call acc%compute(x)
        call check(error, .not. acc%is_ordered(), "a freshly computed object is not ordered")
        if (allocated(error)) return
        g = acc%gmean()
        h = acc%hmean()
        call check(error, .not. acc%is_ordered(), &
            "%gmean/%hmean must not build tier B -- they need the values, not an order")
        if (allocated(error)) return

        ! The domain rules are the one-shot forms', reached through the shared worker.
        call acc%compute([1.0_real64, 0.0_real64, 3.0_real64])
        call check(error, acc%gmean() == 0.0_real64 .and. acc%hmean() == 0.0_real64, &
            "a zero makes both power means exactly 0, as scipy returns")
        if (allocated(error)) return
        call acc%compute([1.0_real64, -2.0_real64, 3.0_real64])
        call check(error, ieee_is_nan(acc%gmean()) .and. ieee_is_nan(acc%hmean()), &
            "a negative value makes both power means NaN")
        if (allocated(error)) return
        call acc%init()
        call check(error, ieee_is_nan(acc%gmean()) .and. ieee_is_nan(acc%hmean()), &
            "an empty population's power means are NaN")
        if (allocated(error)) return

        ! `%trim_mean` is the object's one order binding nothing else in this suite reached, and
        ! it belongs here rather than in a test of its own: it is the third way to take a mean off
        ! a `pf_stats`, beside the two above.
        call acc%compute(x)
        call pf_trim_mean(x, 0.25_real64, g)
        call check(error, acc%trim_mean(0.25_real64) == g, &
            "%trim_mean must equal pf_trim_mean bit for bit")
    end subroutine test_object_power_means

    !> `modes=` returns every tied value, as pandas' `Series.mode()` does.
    !!
    !! `m` alone is `scipy.stats.mode` -- the smallest of a tie -- and the two must agree on their
    !! first element by construction rather than by coincidence, which is why `best_run` derives
    !! both from the same maximum.
    !!
    !! The character arm deliberately puts the SHORTEST tied value first. A deferred-length
    !! allocatable character array has one length for all its elements, so sizing it from the
    !! first -- which the ascending order makes systematically likely to be the short one -- would
    !! truncate every longer mode beside it. That is this repository's standing
    !! sized-from-the-first-element bug class, and this fixture is built to trigger it.
    subroutine test_all_modes(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        integer(int32) :: vi(9), mi
        character(len=6) :: vc(7)
        character(len=:), allocatable :: mc, mcs(:)
        integer(int32), allocatable :: mods(:)
        real(real64) :: w(9)
        integer(int64) :: n
        logical :: ok

        ! Four values tie at two occurrences each; 4 occurs once. pandas answers [1, 2, 3, 5].
        vi = [1_int32, 1_int32, 2_int32, 2_int32, 3_int32, 3_int32, 4_int32, 5_int32, 5_int32]
        call pf_mode(vi, mi, count=n, modes=mods, ok=ok)
        call check(error, ok .and. n == 2_int64, "the modal count is 2")
        if (allocated(error)) return
        call check(error, size(mods) == 4, "four values tie for the mode")
        if (allocated(error)) return
        call check(error, all(mods == [1_int32, 2_int32, 3_int32, 5_int32]), &
            "modes must be every tied value, ascending -- pandas' Series.mode()")
        if (allocated(error)) return
        call check(error, mods(1) == mi, "modes(1) is always m, since both take the smallest tie")
        if (allocated(error)) return

        ! One clear winner: a one-element array, not a scalar special case.
        vi = [7_int32, 1_int32, 1_int32, 2_int32, 9_int32, 9_int32, 9_int32, 5_int32, 5_int32]
        call pf_mode(vi, mi, count=n, modes=mods)
        call check(error, size(mods) == 1 .and. mods(1) == 9_int32 .and. n == 3_int64, &
            "one clear winner gives a one-element modes array")
        if (allocated(error)) return

        ! Empty: allocated to zero length, never left unallocated, so size() is always defined.
        call pf_mode(vi(1:0), mi, count=n, modes=mods, ok=ok)
        call check(error, .not. ok .and. n == 0_int64, "an empty population has no mode")
        if (allocated(error)) return
        call check(error, allocated(mods) .and. size(mods) == 0, &
            "an empty population's modes must be a zero-length array, not unallocated")
        if (allocated(error)) return

        ! Weighted: 1 and 5 both carry 18, while 9 occurs more often and carries only 4.
        vi = [1_int32, 1_int32, 2_int32, 9_int32, 9_int32, 9_int32, 5_int32, 5_int32, 5_int32]
        w = [9.0_real64, 9.0_real64, 1.0_real64, 1.0_real64, 1.0_real64, 1.0_real64, &
             6.0_real64, 6.0_real64, 6.0_real64]
        call pf_mode(vi, mi, count=n, modes=mods, weights=w)
        call check(error, size(mods) == 2, "two values tie on total weight")
        if (allocated(error)) return
        call check(error, all(mods == [1_int32, 5_int32]), &
            "the weighted modes are the values carrying the greatest total weight")
        if (allocated(error)) return

        ! The character arm, with the SHORTEST tied value first on purpose.
        vc = ["aa    ", "aa    ", "zzzzzz", "zzzzzz", "b     ", "b     ", "q     "]
        call pf_mode(vc, mc, count=n, modes=mcs)
        call check(error, size(mcs) == 3, "three character values tie for the mode")
        if (allocated(error)) return
        call check(error, len(mcs) == 6, &
            "the shared length must come from the LONGEST tied value, not the first")
        if (allocated(error)) return
        call check(error, trim(mcs(1)) == "aa" .and. trim(mcs(2)) == "b" .and. &
            trim(mcs(3)) == "zzzzzz", "the character modes, ascending and untruncated")
        if (allocated(error)) return
        call check(error, trim(mcs(1)) == mc, "modes(1) is m for the character forms too")
        if (allocated(error)) return
        call pf_mode(vc(1:0), mc, modes=mcs, ok=ok)
        call check(error, .not. ok .and. allocated(mcs) .and. size(mcs) == 0, &
            "an empty character population's modes is a zero-length array, not unallocated")
        if (allocated(error)) return

        call modes_other_kinds(error)
    end subroutine test_all_modes

    !> `modes=` over the three kinds `test_all_modes`' own fixtures do not reach.
    !!
    !! Each of the five `pf_mode` specifics gathers `modes` itself -- the element type differs, so
    !! the gather cannot be shared -- which is exactly the shape where four arms are right and one
    !! is not. Split out of `test_all_modes` only to keep either subroutine readable.
    subroutine modes_other_kinds(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        integer(int64) :: vl(6), ml
        logical :: vb(4), mb
        type(parquet_string_column) :: col
        integer(int64), allocatable :: modl(:)
        logical, allocatable :: modb(:)
        character(len=:), allocatable :: ms, mss(:)
        integer(int64) :: n
        logical :: ok

        ! int64: 10 and 30 tie at two each.
        vl = [10_int64, 30_int64, 10_int64, 20_int64, 30_int64, 40_int64]
        call pf_mode(vl, ml, count=n, modes=modl, ok=ok)
        call check(error, ok .and. size(modl) == 2 .and. all(modl == [10_int64, 30_int64]), &
            "the int64 modes are every tied value, ascending")
        if (allocated(error)) return
        call pf_mode(vl(1:0), ml, modes=modl, ok=ok)
        call check(error, .not. ok .and. allocated(modl) .and. size(modl) == 0, &
            "an empty int64 population's modes is a zero-length array")
        if (allocated(error)) return

        ! logical: an even split, where BOTH values are modal. `.false.` sorts first.
        vb = [.true., .false., .true., .false.]
        call pf_mode(vb, mb, count=n, modes=modb, ok=ok)
        call check(error, ok .and. size(modb) == 2, "an even split makes both logicals modal")
        if (allocated(error)) return
        call check(error, (.not. modb(1)) .and. modb(2) .and. (mb .eqv. modb(1)), &
            "the logical modes are .false. then .true., and m is the first of them")
        if (allocated(error)) return

        ! parquet_string_column: the packed store, whose values are fetched one at a time, with
        ! the SHORTEST tied value first again so the shared length cannot come from it.
        call col%append_string("ab")
        call col%append_string("ab")
        call col%append_string("zzzz")
        call col%append_null()
        call col%append_string("zzzz")
        call col%append_string("q")
        call pf_mode(col, ms, count=n, modes=mss, ok=ok)
        call check(error, ok .and. size(mss) == 2, "two string-column values tie for the mode")
        if (allocated(error)) return
        call check(error, len(mss) == 4, &
            "the string column's shared length must come from the LONGEST tied value")
        if (allocated(error)) return
        call check(error, trim(mss(1)) == "ab" .and. trim(mss(2)) == "zzzz" .and. &
            trim(mss(1)) == ms, "the string-column modes, ascending and untruncated")
    end subroutine modes_other_kinds

    !> An infinity in EITHER variable of a pair leaves every paired statistic undefined -- and,
    !! like everything else in this module, reports that rather than aborting.
    !!
    !! **The single-variable family had this covered and the paired one did not**, which is why
    !! the paired route reached `Inf - Inf` on `nagfor`, whose default `-ieee=stop` unmasks the
    !! IEEE traps and turns that into a dead process. Every other compiler in the fleet answers
    !! quietly, so nothing but a NAG build can fail this test -- which is exactly why it has to
    !! exist rather than being left to the equivalent single-variable one.
    !!
    !! All three of `pf_cov`, `pf_corr` and the `+Inf`/`-Inf`-together case are exercised, because
    !! they reach the two screens separately: one sign poisons the centred sums through an
    !! infinite mean, both signs poison the running total itself through `Inf + (-Inf)`.
    !!
    !! The negative control matters as much as the assertions: the same call on a finite pair must
    !! still come back with `ok = .true.` and the covariance it always had, or a screen that fired
    !! unconditionally would pass every line above it.
    subroutine test_pair_non_finite(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        real(real64) :: x(5), y(5), c, inf
        logical :: ok

        inf = ieee_value(1.0_real64, ieee_positive_inf)
        call check(error, .not. ieee_is_nan(inf) .and. inf > huge(1.0_real64), &
            "the fixture's +Inf is not an infinity -- the assertions below prove nothing")
        if (allocated(error)) return
        y = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64, 5.0_real64]

        ! The negative control first, so a screen that fired for every population would be caught
        ! here rather than passing as a NaN three lines further down.
        x = [2.0_real64, 4.0_real64, 6.0_real64, 8.0_real64, 10.0_real64]
        call pf_cov(x, y, c, ok=ok)
        call check(error, ok .and. c == 5.0_real64, &
            "a finite pair must still give pf_cov its ordinary answer with ok=.true.")
        if (allocated(error)) return

        ! One infinity in x: the mean of x is infinite, so every centred sum is undefined.
        x = [1.0_real64, 2.0_real64, inf, 4.0_real64, 5.0_real64]
        call pf_cov(x, y, c, ok=ok)
        call check(error, ieee_is_nan(c) .and. .not. ok, &
            "an infinity in x must leave pf_cov NaN with ok=.false.")
        if (allocated(error)) return
        call pf_corr(x, y, c, ok=ok)
        call check(error, ieee_is_nan(c) .and. .not. ok, &
            "an infinity in x must leave pf_corr NaN with ok=.false.")
        if (allocated(error)) return

        ! The mirror, so the screen is not keyed on one argument position.
        call pf_cov(y, x, c, ok=ok)
        call check(error, ieee_is_nan(c) .and. .not. ok, &
            "an infinity in y must leave pf_cov NaN with ok=.false. too")
        if (allocated(error)) return

        ! Both signs: the running total itself is `Inf + (-Inf)`, a different screen from the one
        ! above, and the only one a single-sign fixture cannot reach.
        x = [1.0_real64, -inf, 3.0_real64, inf, 5.0_real64]
        call pf_cov(x, y, c, ok=ok)
        call check(error, ieee_is_nan(c) .and. .not. ok, &
            "both signs of infinity must leave pf_cov NaN with ok=.false.")
        if (allocated(error)) return

        ! And the control again, after the infinite calls, so that nothing above has left the
        ! accumulator or a screen flag in a state the next ordinary call inherits.
        x = [2.0_real64, 4.0_real64, 6.0_real64, 8.0_real64, 10.0_real64]
        call pf_cov(x, y, c, ok=ok)
        call check(error, ok .and. c == 5.0_real64, &
            "a finite pair must still answer correctly after an infinite one")
    end subroutine test_pair_non_finite

    !> `pf_cov(x, x)` is `pf_variance(x)` under BOTH weight conventions.
    !!
    !! The identity is the reason `pf_cov` accumulates through the same block tree the variance
    !! does. It held only for the reliability convention until `pf_cov` gained `weight_type=`:
    !! a caller asking `pf_variance` for frequency weights and `pf_cov` for the same population
    !! got two different denominators from the same `ddof`, silently.
    !!
    !! `==` rather than a tolerance, deliberately -- the two reach the same kernel, so anything
    !! short of equality is a defect rather than rounding. This fixture's centring is EXACT, which
    !! is what keeps it readable and also what keeps it blind to the rounding half of the same
    !! identity; `test_cov_identity_survives_inexact_centring` is the fixture for that.
    subroutine test_cov_weight_type(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        real(real64) :: x(3), w(3), c, v
        integer :: d

        x = [10.0_real64, 12.0_real64, 14.0_real64]
        w = [1.0_real64, 2.0_real64, 1.0_real64]

        do d = 0, 1
            call pf_cov(x, x, c, weights=w, ddof=d)
            call pf_variance(x, v, weights=w, ddof=d)
            call check(error, c == v, "pf_cov(x,x) must equal pf_variance(x), reliability")
            if (allocated(error)) return
            call pf_cov(x, x, c, weights=w, weight_type="frequency", ddof=d)
            call pf_variance(x, v, weights=w, weight_type="frequency", ddof=d)
            call check(error, c == v, "pf_cov(x,x) must equal pf_variance(x), frequency")
            if (allocated(error)) return
        end do

        ! The two conventions must actually DIFFER on this fixture, or the four assertions above
        ! would hold just as well against a pf_cov that ignored the token entirely.
        call pf_cov(x, x, c, weights=w)
        call pf_cov(x, x, v, weights=w, weight_type="frequency")
        call check(error, c /= v, &
            "the two weight conventions must differ here -- otherwise the identity proves nothing")
        if (allocated(error)) return

        ! A frequency weight of 2 must agree with writing the value out twice: that IS the token.
        call pf_cov(x, x, c, weights=w, weight_type="frequency")
        call pf_variance([10.0_real64, 12.0_real64, 12.0_real64, 14.0_real64], v)
        call check(error, close_to(c, v), &
            "a frequency-weighted covariance must equal the expanded population's variance")
        if (allocated(error)) return

        ! Unweighted, the token cannot be observed at all.
        call pf_cov(x, x, c)
        call pf_cov(x, x, v, weight_type="frequency")
        call check(error, c == v, "without weights the two conventions are the same number")
    end subroutine test_cov_weight_type

    !> The same identity over populations whose centring does NOT come out exact.
    !!
    !! **Every other fixture for `pf_cov(x, x) == pf_variance(x)` in this suite has an exactly
    !! representable mean**, and such a population cannot see the defect this test exists for: two
    !! loops written to mirror each other statement for statement still round differently, and the
    !! identity is a documented EXACT one. Both mechanisms were measured against gfortran 15.2 on
    !! arm64 before `stats_pair_moments` grew its diagonal fork -- FMA contraction that reaches the
    !! pair kernel's single-use product and not the single-sample kernel's multi-use one (24% of
    !! random unweighted populations, at `--profile release` only), and a different operand
    !! grouping in the two weighted arms (20%, at every optimisation level). The fork's own comment
    !! carries the detail; this is the fixture that would have refused to pass either of them.
    !!
    !! So the population is built to round rather than to read well: values with no exact binary
    !! form, weights that are not powers of two, and every size from 3 to 200, which crosses
    !! `STATS_BLOCK` and leaves a partial last block at every offset. `==` throughout -- the
    !! identity is exact or it is broken -- and the two weight conventions are asserted to DIFFER
    !! here, or the weighted half would hold just as well against a `pf_cov` ignoring the token.
    subroutine test_cov_identity_survives_inexact_centring(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        real(real64), allocatable :: x(:), w(:)
        real(real64) :: c, v
        character(len=160) :: msg
        integer :: n, i, d

        do n = 3, 200
            allocate(x(n), w(n))
            do i = 1, n
                x(i) = sqrt(real(i, real64)) * 1.7_real64 - 0.3_real64
                w(i) = 0.25_real64 + real(mod(i * 29, 13), real64) / 3.0_real64
            end do

            do d = 0, 1
                call pf_cov(x, x, c, ddof=d)
                call pf_variance(x, v, ddof=d)
                write(msg, '(a,i0,a,i0)') "pf_cov(x,x) /= pf_variance(x) unweighted at n = ", n, &
                    ", ddof = ", d
                call check(error, c == v, trim(msg))
                if (allocated(error)) return

                call pf_cov(x, x, c, weights=w, ddof=d)
                call pf_variance(x, v, weights=w, ddof=d)
                write(msg, '(a,i0,a,i0)') "pf_cov(x,x) /= pf_variance(x) reliability-weighted " // &
                    "at n = ", n, ", ddof = ", d
                call check(error, c == v, trim(msg))
                if (allocated(error)) return

                call pf_cov(x, x, c, weights=w, weight_type="frequency", ddof=d)
                call pf_variance(x, v, weights=w, weight_type="frequency", ddof=d)
                write(msg, '(a,i0,a,i0)') "pf_cov(x,x) /= pf_variance(x) frequency-weighted " // &
                    "at n = ", n, ", ddof = ", d
                call check(error, c == v, trim(msg))
                if (allocated(error)) return
            end do

            ! The fixture has to be one the weighted assertions can actually fail on.
            call pf_cov(x, x, c, weights=w)
            call pf_cov(x, x, v, weights=w, weight_type="frequency")
            write(msg, '(a,i0)') "the two weight conventions must differ at n = ", n
            call check(error, c /= v, trim(msg))
            if (allocated(error)) return

            deallocate(x, w)
        end do
    end subroutine test_cov_identity_survives_inexact_centring

    !> The streaming accuracy loss is bounded, and the two-pass route is exact.
    !!
    !! `pf_stats`' doc-comment says "in streaming mode there is no buffer to re-walk and the
    !! formulas' own accuracy is what you get", which is true and unmeasured. This puts a number
    !! on it, on the population shape that provokes it -- a large offset with a small scatter,
    !! which is what an MJD or a magnitude looks like.
    !!
    !! Three routes over the same values, and the distinction between the last two is the one
    !! worth pinning:
    !!
    !! * one-shot `pf_variance` -- the reference;
    !! * `%compute(retain=.false.)` -- **exact**, because it still makes two passes over the
    !!   resident array. Streaming is about MEMORY, not about a different algorithm, and a test
    !!   that lumped these two together would let a regression turn `%compute` approximate
    !!   without failing;
    !! * a loop of `%update` on a streaming accumulator -- the Chan/Pebay path, which is where the
    !!   loss actually is.
    !!
    !! The bound asserted is 1e-6 relative, against a measured ~1e-8 on this fixture. Loose on
    !! purpose: the point is to catch a route that becomes ORDERS worse, not to pin a digit that
    !! moves with the block decomposition. The exact arm is asserted with `==`, which is what
    !! makes the pair informative -- a bound alone would pass with both routes approximate.
    subroutine test_streaming_accuracy_bound(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        integer(int64), parameter :: N = 80000_int64, CHUNK = 10000_int64
        real(real64), allocatable :: x(:)
        real(real64) :: ref, two_pass, streamed, merged, rel
        type(pf_stats) :: acc, part
        integer(int64) :: i, lo

        allocate(x(N))
        do i = 1_int64, N
            ! 1e9 with a millisecond-scale scatter: the shape on which a naive
            ! sum(x**2) - sum(x)**2/n cancels away every significant digit.
            x(i) = 1.0e9_real64 + real(mod(i * 7919_int64, 1000_int64), real64) * 1.0e-3_real64
        end do

        call pf_variance(x, ref)
        call check(error, ref > 0.0_real64 .and. .not. ieee_is_nan(ref), &
            "the reference variance is not a usable number -- nothing below means anything")
        if (allocated(error)) return

        ! Route two: streaming MODE over a resident array. Two passes, so exact.
        call acc%compute(x, retain=.false.)
        two_pass = acc%variance()
        call check(error, two_pass == ref, &
            "%compute(retain=.false.) must be EXACT -- it still makes two passes")
        if (allocated(error)) return

        ! Route three: a genuine %update loop, which has no buffer to re-walk.
        call acc%clear()
        call acc%init(retain=.false.)
        lo = 1_int64
        do while (lo <= N)
            call acc%update(x(lo:min(lo + CHUNK - 1_int64, N)))
            lo = lo + CHUNK
        end do
        streamed = acc%variance()
        rel = abs(streamed - ref) / ref
        call check(error, rel < 1.0e-6_real64, &
            "the streamed variance is more than 1e-6 relative from the two-pass answer")
        if (allocated(error)) return
        ! The other half of the claim: it is NOT exact, so the bound above is measuring something.
        call check(error, streamed /= ref, &
            "the streamed variance is exact -- either the route changed or this test is vacuous")
        if (allocated(error)) return

        ! %merge inherits the same formulas, so it must land in the same place rather than worse.
        call acc%clear()
        call acc%init(retain=.false.)
        lo = 1_int64
        do while (lo <= N)
            call part%clear()
            call part%compute(x(lo:min(lo + CHUNK - 1_int64, N)), retain=.false.)
            call acc%merge(part)
            lo = lo + CHUNK
        end do
        merged = acc%variance()
        call check(error, abs(merged - ref) / ref < 1.0e-6_real64, &
            "the merged variance is more than 1e-6 relative from the two-pass answer")
    end subroutine test_streaming_accuracy_bound

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

    !> A WEIGHTED, retaining `pf_stats` answers what the weighted one-shot forms answer.
    !!
    !! Every order-statistic binding on this object has two arms -- one reading `keep` alone and
    !! one reading `keep` beside `keep_w` -- and only the unweighted arm was under test. The
    !! second is not a variation on the first: a weighted quantile is taken on the CUMULATIVE
    !! WEIGHT scale rather than the rank scale, so an arm that quietly ignored `keep_w` would
    !! return a perfectly plausible number computed for a population nobody asked about.
    !!
    !! Each assertion is `==` against the one-shot array form given the same `weights=`, because
    !! the two reach the same worker over the same survivors in the same order: not close, the
    !! same bits. A tolerance here would accept exactly the defect the test exists to catch.
    !!
    !! The weights are deliberately unequal and not a permutation of each other, so a binding
    !! that used them in the wrong order would disagree.
    subroutine test_weighted_object_order_statistics(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        integer, parameter :: N = 11
        real(real64) :: x(N), w(N), want, got, wq(3), gq(3)
        type(pf_stats) :: s

        x = [5.0_real64, 1.0_real64, 9.0_real64, 3.0_real64, 7.0_real64, 2.0_real64, &
             8.0_real64, 4.0_real64, 6.0_real64, 0.5_real64, 9.5_real64]
        w = [1.0_real64, 4.0_real64, 2.0_real64, 1.0_real64, 3.0_real64, 5.0_real64, &
             1.0_real64, 2.0_real64, 1.0_real64, 6.0_real64, 1.0_real64]

        call s%compute(x, weights=w)
        call check(error, s%retains(), "the fixture must retain, or none of the tier-B arms run")
        if (allocated(error)) return

        call pf_quantile(x, 0.3_real64, want, weights=w)
        got = s%quantile(0.3_real64)
        call check(error, got == want, "%quantile disagrees with the weighted pf_quantile")
        if (allocated(error)) return

        call pf_median(x, want, weights=w)
        got = s%median()
        call check(error, got == want, "%median disagrees with the weighted pf_median")
        if (allocated(error)) return

        call pf_iqr(x, want, weights=w)
        got = s%iqr()
        call check(error, got == want, "%iqr disagrees with the weighted pf_iqr")
        if (allocated(error)) return

        call pf_trim_mean(x, 0.2_real64, want, weights=w)
        got = s%trim_mean(0.2_real64)
        call check(error, got == want, "%trim_mean disagrees with the weighted pf_trim_mean")
        if (allocated(error)) return

        call pf_percentile_of_score(x, 6.0_real64, want, weights=w)
        got = s%percentile_of_score(6.0_real64)
        call check(error, got == want, &
            "%percentile_of_score disagrees with the weighted pf_percentile_of_score")
        if (allocated(error)) return

        call pf_mad(x, want, weights=w)
        got = s%mad()
        call check(error, got == want, "%mad disagrees with the weighted pf_mad")
        if (allocated(error)) return

        call pf_probit_scale(x, want, weights=w)
        got = s%probit_scale()
        call check(error, got == want, "%probit_scale disagrees with the weighted pf_probit_scale")
        if (allocated(error)) return

        call pf_quantiles(x, [0.1_real64, 0.5_real64, 0.9_real64], wq, weights=w)
        call s%quantiles([0.1_real64, 0.5_real64, 0.9_real64], gq)
        call check(error, all(gq == wq), "%quantiles disagrees with the weighted pf_quantiles")
        if (allocated(error)) return

        ! The negative control the rest of this test needs: drop the weights and the answers must
        ! MOVE. Without it every assertion above would still pass had `keep_w` been ignored on
        ! both sides at once.
        call pf_quantile(x, 0.3_real64, want)
        call check(error, s%quantile(0.3_real64) /= want, &
            "the weighted and unweighted quantiles agree -- this fixture cannot see the weights")
        if (allocated(error)) return

        ! `%probit_mean` takes its own population: the values above are outside `[0, 1]`, which
        ! that statistic answers with a NaN by its documented domain rule.
        call s%compute(x / 10.0_real64, weights=w)
        call pf_probit_mean(x / 10.0_real64, want, weights=w)
        got = s%probit_mean()
        call check(error, got == want, "%probit_mean disagrees with the weighted pf_probit_mean")
    end subroutine test_weighted_object_order_statistics

    !> `%update` and `%merge` carry the weights, and fill in 1 for a batch that has none.
    !!
    !! A weighted accumulator's `keep_w` has to stay in step with `keep` through every route that
    !! grows either, and there are four: an update that carries weights, an update that does not
    !! (every element of that batch weighs 1), a merge from another weighted accumulator, and a
    !! merge from an unweighted one (same rule). Each writes `keep_w` in its own loop, so none is
    !! covered by testing another, and a slip in any leaves the two arrays describing different
    !! populations -- with no symptom at all until an order statistic is read.
    !!
    !! The reference is the one-shot form over the concatenated population, which is what
    !! `%update` and `%merge` promise to equal EXACTLY while `retain` is on.
    subroutine test_weighted_update_and_merge_keep_the_weights(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        real(real64) :: a(4), b(5), c(3), wa(4), wb(5), wc(3)
        real(real64) :: all_x(12), all_w(12), all_w6(12), want, want6
        type(pf_stats) :: s, t

        a = [3.0_real64, 8.0_real64, 1.0_real64, 6.0_real64]
        b = [5.0_real64, 2.0_real64, 9.0_real64, 4.0_real64, 7.0_real64]
        c = [0.5_real64, 9.5_real64, 2.5_real64]
        wa = [2.0_real64, 1.0_real64, 4.0_real64, 3.0_real64]
        wb = [1.0_real64, 5.0_real64, 2.0_real64, 1.0_real64, 3.0_real64]
        wc = [6.0_real64, 6.0_real64, 6.0_real64]
        all_x(1:4) = a;   all_x(5:9) = b;   all_x(10:12) = c
        all_w(1:4) = wa;  all_w(5:9) = wb;  all_w(10:12) = 1.0_real64
        all_w6 = all_w;   all_w6(10:12) = wc

        ! The fixture's own precondition: the last batch's weight has to be legible in the answer,
        ! or "the unweighted arm filled in 1" and "the unweighted arm dropped the batch's weights
        ! entirely" would be the same assertion.
        call pf_median(all_x, want, weights=all_w)
        call pf_median(all_x, want6, weights=all_w6)
        call check(error, want /= want6, &
            "weighting the last batch at 6 rather than 1 moves nothing -- this fixture is blind")
        if (allocated(error)) return

        ! `%compute`, then a weighted `%update` -- the growth that finds `keep_w` already
        ! allocated and non-empty -- then an UNWEIGHTED one, whose elements each weigh 1.
        call s%compute(a, weights=wa)
        call s%update(b, weights=wb)
        call s%update(c)
        call check(error, s%median() == want, "%update lost the weights in the concatenation")
        if (allocated(error)) return
        call pf_mean(all_x, want6, weights=all_w)
        call check(error, s%mean() == want6, "%update's weighted mean is not the one-shot mean")
        if (allocated(error)) return

        ! The same population assembled by MERGE instead: weighted into weighted, then
        ! UNWEIGHTED into weighted, which is the arm that fills 1 for every element it takes.
        call s%compute(a, weights=wa)
        call t%compute(b, weights=wb)
        call s%merge(t)
        call t%compute(c)
        call s%merge(t)
        call check(error, s%median() == want, "%merge lost the weights in the fold")
        if (allocated(error)) return
        call pf_quantile(all_x, 0.8_real64, want6, weights=all_w)
        call check(error, s%quantile(0.8_real64) == want6, &
            "%merge's weighted quantile is not the one-shot weighted quantile")
        if (allocated(error)) return

        ! The negative control for that fill: the same merge with the source actually weighted
        ! must land on the OTHER reference. A merge that ignored the source's weights would
        ! answer `want` both times.
        call pf_median(all_x, want6, weights=all_w6)
        call s%compute(a, weights=wa)
        call t%compute(b, weights=wb)
        call s%merge(t)
        call t%compute(c, weights=wc)
        call s%merge(t)
        call check(error, s%median() == want6, "%merge ignored the weighted source's own weights")
    end subroutine test_weighted_update_and_merge_keep_the_weights

    !> An armed but EMPTY retaining accumulator answers NaN rather than reading `keep`.
    !!
    !! `%init` leaves `keep` unallocated, so every tier-B binding has to answer the empty
    !! population before it associates that array with a non-optional dummy -- which is not a
    !! detail of taste: passing an unallocated allocatable there is not conforming, and nagfor's
    !! `-C=array` is the only build that says so.
    !!
    !! The answer is the same NaN the one-shot forms give an empty array, so this also pins that
    !! `%init` followed by a read is not a special case a caller has to know about.
    subroutine test_empty_retained_object_reads_are_nan(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        real(real64) :: empty(0), want
        type(pf_stats) :: s

        call s%init()
        call check(error, s%is_computed(), "%init must leave the accumulator live")
        if (allocated(error)) return
        call check(error, ieee_is_nan(s%probit_mean()), "%probit_mean of an empty population")
        if (allocated(error)) return
        call check(error, ieee_is_nan(s%percentile_of_score(1.0_real64)), &
            "%percentile_of_score of an empty population")
        if (allocated(error)) return
        call check(error, ieee_is_nan(s%quantile(0.5_real64)), "%quantile of an empty population")
        if (allocated(error)) return
        call check(error, ieee_is_nan(s%trim_mean(0.25_real64)), "%trim_mean of an empty population")
        if (allocated(error)) return

        ! The one-shot forms answer the same NaN for the same population, which is the point.
        call pf_probit_mean(empty, want)
        call check(error, ieee_is_nan(want), "pf_probit_mean of an empty array must be NaN too")
    end subroutine test_empty_retained_object_reads_are_nan

    !> A `-0.0` weight is a ZERO weight, and every `weights=` loop in the family says so.
    !!
    !! **This is the one input that takes the fast weight screen's slow path and survives it.**
    !! Each loop screens a weight by reading its bits as an `int64` and branching when they fall
    !! outside `[0, STATS_W_LIM)`; a negative zero has the sign bit set, so it reads negative and
    !! goes to `stats_check_weight` -- which accepts it, because `-0.0 < 0.0` is false -- and the
    !! `<= 0` test after the call is what finally excludes it. Every other value on that branch
    !! aborts, so without a negative zero the line after the validator call is never executed in
    !! any of the seven loops that have one.
    !!
    !! The contract asserted is the one `STATS_W_LIM`'s own documentation states: the answer is
    !! the same as for `+0.0` in that position, which is the same as for the element removed.
    !! Getting it wrong is silent -- the element would be counted at weight zero instead of
    !! dropped, which changes `pf_count_valid` and every effective-size denominator.
    subroutine test_minus_zero_weight_is_a_zero_weight(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        integer, parameter :: N = 6
        real(real64) :: x(N), y(N), wz(N), wp(N), edges(4)
        real(real64) :: mz, mp, cz, cp, hz(3), hp(3), ez(3), ep(3)
        real(real32) :: xs(N)
        integer(int32) :: xi(N), kz(N), kp(N), mi1, mi2
        integer(int64) :: xl(N), nz, np, negbits
        logical :: xb(N)

        x = [4.0_real64, 1.0_real64, 7.0_real64, 2.0_real64, 9.0_real64, 5.0_real64]
        y = [1.0_real64, 8.0_real64, 3.0_real64, 6.0_real64, 2.0_real64, 4.0_real64]
        xs = real(x, real32)
        xi = int(x, int32)
        xl = int(x, int64)
        xb = [.true., .false., .true., .true., .false., .true.]
        edges = [0.0_real64, 3.0_real64, 6.0_real64, 10.0_real64]

        ! Built by setting the sign bit of a `+0.0` at run time, and checked before it is used:
        ! a literal `-0.0` is one the compiler may fold, and `sign(0.0, -1.0)` is not a reliable
        ! spelling under every `-fp-model`. `==` cannot tell the two zeros apart, so the
        ! precondition has to be read from the bits.
        wp = 1.0_real64
        wz = 1.0_real64
        negbits = transfer(0.0_real64, 0_int64)
        negbits = ibset(negbits, 63)
        wz(3) = transfer(negbits, 0.0_real64)
        wp(3) = 0.0_real64
        ! A `+0.0` beside it, so both halves of the screen are exercised by one population: the
        ! positive zero takes the FAST arm (`wbits == 0`) and the negative zero the slow one.
        wz(5) = 0.0_real64
        wp(5) = 0.0_real64
        call check(error, wz(3) == 0.0_real64 .and. transfer(wz(3), 0_int64) < 0_int64, &
            "the fixture's -0.0 is not a negative zero -- every assertion below proves nothing")
        if (allocated(error)) return
        call check(error, transfer(wz(5), 0_int64) == 0_int64, &
            "the fixture's +0.0 is not a positive zero, so the fast arm is not being reached")
        if (allocated(error)) return

        ! The counting entry point, over every kind that has a `weights=` loop of its own. The
        ! answer is 4 rather than 6 in each: both zero-weighted elements are out of the
        ! population, however their sign bit is set.
        call pf_count_valid(x, nz, weights=wz)
        call check(error, nz == 4_int64, "both zero weights must remove their elements")
        if (allocated(error)) return
        call pf_count_valid(xs, np, weights=wz)
        call check(error, np == nz, "pf_count_valid over real32 disagrees on a -0.0 weight")
        if (allocated(error)) return
        call pf_count_valid(xi, np, weights=wz)
        call check(error, np == nz, "pf_count_valid over int32 disagrees on a -0.0 weight")
        if (allocated(error)) return
        call pf_count_valid(xl, np, weights=wz)
        call check(error, np == nz, "pf_count_valid over int64 disagrees on a -0.0 weight")
        if (allocated(error)) return
        call pf_count_valid(xb, np, weights=wz)
        call check(error, np == nz, "pf_count_valid over logical disagrees on a -0.0 weight")
        if (allocated(error)) return

        ! The two-pass engine's own loop, and the negative control that makes the rest legible:
        ! the mean must MOVE when the third element leaves.
        call pf_mean(x, mz, weights=wz)
        call pf_mean(x, mp, weights=wp)
        call check(error, mz == mp, "a -0.0 weight and a +0.0 weight must give the same mean")
        if (allocated(error)) return
        call pf_mean([x(1:2), x(4), x(6)], mp)
        call check(error, mz == mp, "a zero-weighted element must be gone, not merely weightless")
        if (allocated(error)) return
        call pf_mean(x, mp)
        call check(error, mz /= mp, &
            "dropping the third element changed nothing -- this fixture cannot see a zero weight")
        if (allocated(error)) return

        ! The binning family, whose three loops each carry their own copy of the screen.
        call pf_histogram(x, edges, hz, weights=wz)
        call pf_histogram(x, edges, hp, weights=wp)
        call check(error, all(hz == hp), "pf_histogram disagrees on a -0.0 weight")
        if (allocated(error)) return
        call pf_bucketize(x, edges, kz, weights=wz)
        call pf_bucketize(x, edges, kp, weights=wp)
        call check(error, all(kz == kp), "pf_bucketize disagrees on a -0.0 weight")
        if (allocated(error)) return
        call pf_bin_edges(x, 2, ez, weights=wz)
        call pf_bin_edges(x, 2, ep, weights=wp)
        call check(error, all(ez == ep), "pf_bin_edges disagrees on a -0.0 weight")
        if (allocated(error)) return

        ! The PAIRWISE compaction, which screens the weight once for the pair rather than twice.
        call pf_cov(x, y, cz, weights=wz)
        call pf_cov(x, y, cp, weights=wp)
        call check(error, cz == cp, "pf_cov disagrees on a -0.0 weight")
        if (allocated(error)) return
        call pf_corr(x, y, cz, weights=wz)
        call pf_corr(x, y, cp, weights=wp)
        call check(error, cz == cp, "pf_corr disagrees on a -0.0 weight")
        if (allocated(error)) return

        ! The modal survivor scan, whose screen sits in a different submodule again.
        call pf_mode(xi, mi1, weights=wz)
        call pf_mode(xi, mi2, weights=wp)
        call check(error, mi1 == mi2, "pf_mode disagrees on a -0.0 weight")
        if (allocated(error)) return

        ! The STRING-COLUMN modal scan, which screens its weights through the survivor
        ! permutation rather than through the values, and so carries a third copy of the screen.
        block
            type(parquet_string_column) :: col
            character(len=:), allocatable :: m1, m2
            integer :: j
            do j = 1, N
                call col%append_string(repeat(char(iachar("a") + int(x(j))), 2))
            end do
            call pf_mode(col, m1, weights=wz)
            call pf_mode(col, m2, weights=wp)
            call check(error, m1 == m2, "the string-column pf_mode disagrees on a -0.0 weight")
        end block
    end subroutine test_minus_zero_weight_is_a_zero_weight

    !> `pf_mode` reads `weights=` over EVERY kind, and answers an all-null population.
    !!
    !! The modal scan is written out once per element type, and each copy gathers the survivors'
    !! weights into its own permuted array before walking the runs. Only the `int32` and `real64`
    !! copies were ever called with weights, so the gather in the other four was untested -- and
    !! a gather that lost the permutation would not fail loudly: it would report a real value
    !! from the population as the mode, just not the modal one.
    !!
    !! The empty-survivor arm is per-kind for the same reason, and has its own shape in each: the
    !! `logical` copy allocates a zero-length `modes`, the string-column copy a zero-length
    !! DEFERRED-LENGTH one, which is a different statement that a shared test cannot reach.
    subroutine test_mode_weights_and_all_null_across_kinds(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        integer, parameter :: N = 7
        integer(int64) :: vl(N), ml, cnt, nnull
        logical :: vb(N), mb, mask(N), ok
        character(len=5) :: vc(N)
        character(len=:), allocatable :: mc
        type(parquet_string_column) :: col
        real(real64) :: w(N)

        ! "b" occurs three times and "a" twice, so "b" is the unweighted mode; the weights below
        ! put 10 on each "a" and 1 on everything else, which must move it to "a".
        vl = [2_int64, 1_int64, 2_int64, 1_int64, 2_int64, 3_int64, 3_int64]
        vb = [.true., .false., .true., .false., .true., .true., .true.]
        vc = ["b    ", "a    ", "b    ", "a    ", "b    ", "c    ", "c    "]
        w = [1.0_real64, 10.0_real64, 1.0_real64, 10.0_real64, 1.0_real64, 1.0_real64, 1.0_real64]

        call pf_mode(vl, ml, count=cnt)
        call check(error, ml == 2_int64, "the unweighted int64 mode must be the commonest value")
        if (allocated(error)) return
        call pf_mode(vl, ml, count=cnt, weights=w)
        call check(error, ml == 1_int64 .and. cnt == 2_int64, &
            "weights must move the int64 mode; count stays an ELEMENT count")
        if (allocated(error)) return

        call pf_mode(vb, mb, count=cnt)
        call check(error, mb, "the unweighted logical mode must be .true., the commoner value")
        if (allocated(error)) return
        call pf_mode(vb, mb, count=cnt, weights=w)
        call check(error, (.not. mb) .and. cnt == 2_int64, &
            "weights must move the logical mode to .false.")
        if (allocated(error)) return

        call pf_mode(vc, mc, count=cnt)
        call check(error, mc == "b", "the unweighted character mode must be b")
        if (allocated(error)) return
        call pf_mode(vc, mc, count=cnt, weights=w)
        call check(error, mc == "a" .and. cnt == 2_int64, &
            "weights must move the character-array mode to a")
        if (allocated(error)) return

        call col%append_string("b")
        call col%append_string("a")
        call col%append_string("b")
        call col%append_string("a")
        call col%append_string("b")
        call col%append_string("c")
        call col%append_string("c")
        call pf_mode(col, mc, count=cnt)
        call check(error, mc == "b", "the unweighted string-column mode must be b")
        if (allocated(error)) return
        call pf_mode(col, mc, count=cnt, weights=w)
        call check(error, mc == "a" .and. cnt == 2_int64, &
            "weights must move the string-column mode to a")
        if (allocated(error)) return

        ! Every element excluded: each kind reports no mode rather than inventing one, and says
        ! through `n_null` how many elements it lost.
        mask = .false.
        call pf_mode(vb, mb, count=cnt, is_valid=mask, n_null=nnull, ok=ok)
        call check(error, .not. ok .and. cnt == 0_int64 .and. nnull == int(N, int64), &
            "an all-null logical population must report ok=.false. and count its nulls")
        if (allocated(error)) return
        call col%clear()
        call col%append_null()
        call col%append_null()
        call pf_mode(col, mc, count=cnt, n_null=nnull, ok=ok)
        call check(error, .not. ok .and. cnt == 0_int64 .and. nnull == 2_int64, &
            "an all-null string column must report ok=.false. and count its nulls")
    end subroutine test_mode_weights_and_all_null_across_kinds

    !> The STREAMING accumulator excludes and poisons exactly as the two-pass route does.
    !!
    !! `%compute(retain=.false.)` is still two passes over a resident array; the streaming walk is
    !! reached only by `%init` followed by `%update`, which is what a loop over a file larger than
    !! RAM does. That walk carries its own copy of the family's exclusion order -- nullness, then
    !! NaN, then weight -- and its own poisoning rule, and none of it was under test: every
    !! `%update` the suite performed handed over a clean, unweighted batch.
    !!
    !! The counts are asserted EXACTLY, because exclusion is counting rather than arithmetic and
    !! a streamed count that drifts is a defect rather than a rounding loss. The mean is asserted
    !! against a bound instead, which is the trade `test_streaming_accuracy_bound` documents: the
    !! combination formulas are not the two-pass formulas.
    !!
    !! Three arms no other test reaches:
    !!
    !! * a batch every element of which is excluded, which contributes no block at all and must
    !!   leave the running answer alone rather than clearing it;
    !! * `skipnan = .false.` with a NaN, which poisons this accumulator for good;
    !! * merging a poisoned accumulator into a clean one, which must poison the destination --
    !!   the combination step's own arm, and the one that decides whether a partial that saw a
    !!   NaN can be quietly folded away.
    subroutine test_streaming_update_excludes_and_poisons(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        real(real64) :: a(5), b(5), c(4), wa(5), wb(5), keep(11), nan, ref, rel
        logical :: ma(5), allbad(4), had_inv, got_inv
        type(pf_stats) :: s, t

        nan = ieee_value(1.0_real64, ieee_quiet_nan)
        a = [3.0_real64, 8.0_real64, 1.0_real64, 6.0_real64, 4.0_real64]
        b = [5.0_real64, nan, 9.0_real64, 2.0_real64, 7.0_real64]
        c = [11.0_real64, 12.0_real64, 13.0_real64, 14.0_real64]
        ma = [.true., .true., .false., .true., .true.]
        wa = [2.0_real64, 1.0_real64, 9.0_real64, 3.0_real64, 1.0_real64]
        wb = [1.0_real64, 1.0_real64, 0.0_real64, 2.0_real64, 1.0_real64]
        allbad = .false.

        ! Batch one is masked, batch two carries a NaN and a zero weight, batch three is entirely
        ! excluded. The survivors are a(1), a(2), a(4), a(5), b(1), b(4), b(5) -- seven of
        ! fourteen -- and the third element of `a` and the third of `b` leave by different routes
        ! that must be reported through different counters.
        call s%init(retain=.false.)
        call s%update(a, is_valid=ma, weights=wa)
        call s%update(b, weights=wb)
        call s%update(c, is_valid=allbad)
        call check(error, s%n() == 14_int64, "%n counts every element offered, excluded or not")
        if (allocated(error)) return
        call check(error, s%n_valid() == 7_int64, "seven elements survive all three exclusions")
        if (allocated(error)) return
        call check(error, s%n_null() == 5_int64, "one masked element plus the whole third batch")
        if (allocated(error)) return
        call check(error, s%n_nan() == 1_int64, "the NaN must be counted as a NaN, not as a null")
        if (allocated(error)) return

        keep(1:4) = [a(1), a(2), a(4), a(5)]
        keep(5:7) = [b(1), b(4), b(5)]
        call pf_mean(keep(1:7), ref, weights=[wa(1), wa(2), wa(4), wa(5), wb(1), wb(4), wb(5)])
        rel = abs(s%mean() - ref) / abs(ref)
        call check(error, rel < 1.0e-12_real64, &
            "the streamed weighted mean is not the mean of the survivors it reported")
        if (allocated(error)) return

        ! An entirely excluded batch must not disturb what came before it: the same two batches
        ! without the third have to give the same answer bit for bit.
        call t%init(retain=.false.)
        call t%update(a, is_valid=ma, weights=wa)
        call t%update(b, weights=wb)
        call check(error, t%mean() == s%mean(), &
            "a batch with no survivors changed the running mean")
        if (allocated(error)) return

        ! `skipnan = .false.`: the NaN stays in the population and every answer is NaN from then
        ! on, including after a later clean batch.
        !
        ! **Guarded by IEEE_INVALID**, because a kept NaN is the one input on this path that can
        ! reach an ordered comparison against one. `<` and `>` are signalling predicates, and
        ! nagfor unmasks the traps by default, so a raised flag here is a dead process there and
        ! a silent line at exit everywhere else -- which is why it is asserted rather than left
        ! to a build nobody runs daily. The two-pass engine keeps a kept NaN out of its own
        ! extremes for exactly this reason (`stats_engine`'s `saw_nan_l` guard).
        call ieee_get_flag(ieee_invalid, had_inv)
        call ieee_set_flag(ieee_invalid, .false.)
        call s%clear()
        call s%init(retain=.false., skipnan=.false.)
        call s%update(b)
        call ieee_get_flag(ieee_invalid, got_inv)
        call ieee_set_flag(ieee_invalid, had_inv .or. got_inv)
        call check(error, .not. got_inv, &
            "streaming a kept NaN raised IEEE_INVALID; nagfor's -ieee=stop would abort here")
        if (allocated(error)) return
        call check(error, ieee_is_nan(s%mean()), "a kept NaN must poison the streamed mean")
        if (allocated(error)) return
        call s%update(a)
        call check(error, ieee_is_nan(s%mean()), "a later clean batch must not un-poison it")
        if (allocated(error)) return
        call check(error, ieee_is_nan(s%variance()), "and the variance with it")
        if (allocated(error)) return
        call check(error, s%n_valid() == 10_int64, &
            "a kept NaN is IN the population, so it is counted as a survivor")
        if (allocated(error)) return

        ! Merging that poisoned accumulator into a clean one must carry the poison across.
        call t%clear()
        call t%init(retain=.false., skipnan=.false.)
        call t%update(a)
        call check(error, .not. ieee_is_nan(t%mean()), "the clean destination starts clean")
        if (allocated(error)) return
        call t%merge(s)
        call check(error, ieee_is_nan(t%mean()), &
            "merging an accumulator that kept a NaN must poison the destination")
    end subroutine test_streaming_update_excludes_and_poisons

    !> The bias corrections refuse the sample sizes they are not defined for.
    !!
    !! `bias = .false.` multiplies the raw moment by a factor with `n - 2` (skewness) or
    !! `(n - 2)(n - 3)` (kurtosis) underneath. At or below those sizes the correction is a
    !! division by zero or by a negative, and the honest answer is that the corrected statistic
    !! does not exist for this population -- which is what scipy answers too.
    !!
    !! The negative control is the same call one element larger, which must produce a number: a
    !! guard that fired for every population would pass the NaN half on its own.
    subroutine test_bias_corrections_refuse_small_samples(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        real(real64) :: g, k

        ! Skewness needs n > 2 once corrected.
        call pf_skewness([1.0_real64, 5.0_real64], g, bias=.false.)
        call check(error, ieee_is_nan(g), "the corrected skewness of two elements must be NaN")
        if (allocated(error)) return
        call pf_skewness([1.0_real64, 5.0_real64, 2.0_real64], g, bias=.false.)
        call check(error, .not. ieee_is_nan(g), "three elements must give a corrected skewness")
        if (allocated(error)) return

        ! Kurtosis needs n > 3.
        call pf_kurtosis([1.0_real64, 5.0_real64, 2.0_real64], k, bias=.false.)
        call check(error, ieee_is_nan(k), "the corrected kurtosis of three elements must be NaN")
        if (allocated(error)) return
        call pf_kurtosis([1.0_real64, 5.0_real64, 2.0_real64, 9.0_real64], k, bias=.false.)
        call check(error, .not. ieee_is_nan(k), "four elements must give a corrected kurtosis")
        if (allocated(error)) return

        ! And the uncorrected forms, which are defined wherever the moment is: the guards above
        ! belong to the correction rather than to the statistic.
        call pf_skewness([1.0_real64, 5.0_real64], g, bias=.true.)
        call check(error, .not. ieee_is_nan(g), "the uncorrected skewness of two elements exists")
        if (allocated(error)) return
        call pf_kurtosis([1.0_real64, 5.0_real64, 2.0_real64], k, bias=.true.)
        call check(error, .not. ieee_is_nan(k), "the uncorrected kurtosis of three elements exists")
    end subroutine test_bias_corrections_refuse_small_samples

    !> `weight_type="reliability"` is spelled out, and `pf_zscore` forwards `ddof=`.
    !!
    !! Both are DEFAULTS that a caller may also state, and a default reached by two routes is a
    !! place where the two can drift: `stats_weight_kind` has an arm for the explicit token and a
    !! fall-through for the absent argument, and `pf_zscore` passes `ddof` on only when it was
    !! given. Stating each explicitly must land exactly where leaving it out does.
    !!
    !! The negative control is the other token in each pair, which must move the answer -- without
    !! it "the explicit token is honoured" and "the argument is ignored" look the same.
    subroutine test_explicit_defaults_match_the_implicit_ones(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        real(real64) :: x(6), w(6), a, b, za(6), zb(6)

        x = [2.0_real64, 4.0_real64, 4.0_real64, 4.0_real64, 5.0_real64, 9.0_real64]
        w = [1.0_real64, 3.0_real64, 1.0_real64, 2.0_real64, 1.0_real64, 4.0_real64]

        call pf_variance(x, a, weights=w)
        call pf_variance(x, b, weights=w, weight_type="reliability")
        call check(error, a == b, '"reliability" spelled out must be the default it already is')
        if (allocated(error)) return
        call pf_variance(x, b, weights=w, weight_type="frequency")
        call check(error, a /= b, &
            '"frequency" gave the same answer as "reliability" -- this fixture cannot tell them apart')
        if (allocated(error)) return

        call pf_zscore(x, za)
        call pf_zscore(x, zb, ddof=1)
        call check(error, all(za == zb), "ddof=1 spelled out must be the default it already is")
        if (allocated(error)) return
        call pf_zscore(x, zb, ddof=0)
        call check(error, .not. all(za == zb), &
            "ddof=0 gave the same scores as ddof=1 -- pf_zscore is not forwarding the argument")
    end subroutine test_explicit_defaults_match_the_implicit_ones

    !> A kept NaN poisons the general engine loop as it poisons the fast one.
    !!
    !! `stats_engine` has TWO pass-one walks: a fast one for an unweighted, unmasked call whose
    !! survivors need not be kept, and a general one for everything else. Each carries its own
    !! copy of the NaN rule, and only the fast one was ever reached with `skipnan = .false.` --
    !! so the general walk's poisoning, which is the one a weighted or masked call depends on,
    !! was untested.
    !!
    !! Each arm is entered by a different argument, so all three are asserted rather than one
    !! taken as representative.
    subroutine test_kept_nan_poisons_the_general_engine_walk(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        real(real64) :: x(6), w(6), nan, m
        logical :: mask(6), ok
        type(pf_stats) :: s

        nan = ieee_value(1.0_real64, ieee_quiet_nan)
        x = [2.0_real64, 4.0_real64, nan, 4.0_real64, 5.0_real64, 9.0_real64]
        w = [1.0_real64, 3.0_real64, 1.0_real64, 2.0_real64, 1.0_real64, 4.0_real64]
        mask = .true.

        call pf_mean(x, m, weights=w, skipnan=.false., ok=ok)
        call check(error, ieee_is_nan(m) .and. .not. ok, "a weighted call must keep the NaN too")
        if (allocated(error)) return
        call pf_mean(x, m, is_valid=mask, skipnan=.false., ok=ok)
        call check(error, ieee_is_nan(m) .and. .not. ok, "a masked call must keep the NaN too")
        if (allocated(error)) return
        call s%compute(x, retain=.true., skipnan=.false.)
        call check(error, ieee_is_nan(s%mean()), "a RETAINING call must keep the NaN too")
        if (allocated(error)) return

        ! The negative control: the same three calls with the default `skipnan` must each answer
        ! the mean of the five real elements.
        call pf_mean([x(1:2), x(4:6)], m)
        call check(error, .not. ieee_is_nan(m), "the reference mean is itself a NaN")
        if (allocated(error)) return
        call pf_mean(x, m, weights=w, ok=ok)
        call check(error, .not. ieee_is_nan(m) .and. ok, "skipnan=.true. must drop it again")
    end subroutine test_kept_nan_poisons_the_general_engine_walk

    !> `pf_histogram` and `pf_bin_edges` exclude what every other reduction excludes.
    !!
    !! The binning family walks its own loops rather than going through `stats_compact`, so each
    !! of the three has its own copy of the exclusion order -- and the `is_valid` arm of
    !! `pf_histogram` and the NaN arm of `pf_bin_edges` had no caller. A binning routine that
    !! counted a null would put it in whichever bin its undefined payload happened to fall in,
    !! which is a wrong tally rather than a crash.
    subroutine test_binning_excludes_nulls_and_nans(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        real(real64) :: x(7), edges(4), h(3), href(3), e(3), eref(3), nan
        logical :: mask(7)
        integer(int64) :: nnull, nnan

        nan = ieee_value(1.0_real64, ieee_quiet_nan)
        edges = [0.0_real64, 3.0_real64, 6.0_real64, 10.0_real64]
        x = [1.0_real64, 4.0_real64, 7.0_real64, 2.0_real64, 5.0_real64, 8.0_real64, 9.0_real64]
        mask = [.true., .true., .false., .true., .true., .true., .false.]

        call pf_histogram(x, edges, h, is_valid=mask, n_null=nnull)
        call pf_histogram([x(1:2), x(4:6)], edges, href)
        call check(error, all(h == href), "pf_histogram must tally only the unmasked elements")
        if (allocated(error)) return
        call check(error, nnull == 2_int64, "and report how many it excluded")
        if (allocated(error)) return
        ! The negative control: counted rather than excluded, the two extra elements would land in
        ! the last bin, so the tallies must differ.
        call pf_histogram(x, edges, href)
        call check(error, .not. all(h == href), &
            "masking changed no bin -- this fixture cannot see an excluded element")
        if (allocated(error)) return

        ! `pf_bin_edges` sizes its range from the population, so a NaN it failed to skip would
        ! propagate into both edges rather than into one bin.
        x(3) = nan
        call pf_bin_edges(x, 2, e, n_nan=nnan)
        call pf_bin_edges([x(1:2), x(4:7)], 2, eref)
        call check(error, all(e == eref), "pf_bin_edges must ignore a NaN as every reduction does")
        if (allocated(error)) return
        call check(error, nnan == 1_int64, "and report it through n_nan")
    end subroutine test_binning_excludes_nulls_and_nans

    !> `pf_cov` and `pf_corr` over two COLUMNS combine the two validity masks.
    !!
    !! A pairwise statistic needs an element to be present in BOTH samples, so the column form has
    !! to intersect the two columns' own null masks before it computes anything. Either column may
    !! carry no mask at all -- that is how "this column has no nulls" is spelled -- so the
    !! intersection has four shapes, and the answer for each must be the one the array form gives
    !! for the surviving pairs.
    !!
    !! Getting it wrong is silent and plausible: a covariance computed over a row whose `y` is
    !! null reads whatever payload sat in the gap, which is a number.
    subroutine test_pairwise_columns_intersect_their_masks(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        integer, parameter :: N = 6
        real(real64) :: xd(N), yd(N), want, got
        type(parquet_column) :: cx, cy, cxn, cyn
        integer :: k

        xd = [2.0_real64, 4.0_real64, 6.0_real64, 8.0_real64, 10.0_real64, 12.0_real64]
        yd = [1.0_real64, 5.0_real64, 2.0_real64, 9.0_real64, 3.0_real64, 7.0_real64]
        call cx%init(PK_FLOAT64, int(N, int64))
        call cy%init(PK_FLOAT64, int(N, int64))
        call cxn%init(PK_FLOAT64, int(N, int64))
        call cyn%init(PK_FLOAT64, int(N, int64))
        do k = 1, N
            call cx%set_at(int(k, int64), xd(k))
            call cy%set_at(int(k, int64), yd(k))
            call cxn%set_at(int(k, int64), xd(k))
            call cyn%set_at(int(k, int64), yd(k))
        end do
        ! One null in each column, at DIFFERENT rows, so the intersection is smaller than either
        ! mask and a form that took only one of them would answer something else.
        call cxn%set_null(2_int64)
        call cyn%set_null(5_int64)

        ! Neither column has a mask: the pair takes the no-mask fast path.
        call pf_cov(xd, yd, want)
        call pf_cov(cx, cy, got)
        call check(error, got == want, "two null-free columns must give the array answer")
        if (allocated(error)) return

        ! One mask, on the left and then on the right.
        call pf_cov([xd(1), xd(3:N)], [yd(1), yd(3:N)], want)
        call pf_cov(cxn, cy, got)
        call check(error, got == want, "a null on the left must drop that PAIR")
        if (allocated(error)) return
        call pf_cov([xd(1:4), xd(6)], [yd(1:4), yd(6)], want)
        call pf_cov(cx, cyn, got)
        call check(error, got == want, "a null on the right must drop that pair")
        if (allocated(error)) return

        ! Both masks: the survivors are the rows present in each.
        call pf_cov([xd(1), xd(3:4), xd(6)], [yd(1), yd(3:4), yd(6)], want)
        call pf_cov(cxn, cyn, got)
        call check(error, got == want, "two masks must INTERSECT, not merely one of them apply")
        if (allocated(error)) return
        call pf_corr([xd(1), xd(3:4), xd(6)], [yd(1), yd(3:4), yd(6)], want)
        call pf_corr(cxn, cyn, got)
        call check(error, got == want, "pf_corr must intersect them the same way")
        if (allocated(error)) return

        ! The negative control: taking only the left mask would answer this instead.
        call pf_cov([xd(1), xd(3:N)], [yd(1), yd(3:N)], want)
        call pf_cov(cxn, cyn, got)
        call check(error, got /= want, "the right column's null was not applied")
    end subroutine test_pairwise_columns_intersect_their_masks

    !> Both SIGNS of infinity, in either variable, leave `pf_cov` and `pf_corr` NaN.
    !!
    !! The pairwise engine tracks the two variables' infinities in four flags of its own rather
    !! than reading extremes it does not keep, and each flag has its own fold-back arm. The
    !! existing non-finite test drives `x` positive and `x` both-signed; the mirrors -- `x`
    !! negative only, `y` negative only, `y` both-signed -- were untested, and they are separate
    !! statements rather than a symmetry the compiler provides.
    !!
    !! Nothing here may abort: an infinity is a data condition, and pass two is skipped entirely
    !! so that `Inf - Inf` never happens. That is asserted through IEEE_INVALID rather than
    !! assumed, because a raised flag is a dead process under nagfor and silent everywhere else.
    !!
    !! The block boundary is crossed in the same test: the pair walk sums into fixed-size blocks
    !! and its roll-over had no fixture, every pairwise test until now having been smaller than
    !! one block.
    subroutine test_pairwise_infinities_both_signs(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        integer, parameter :: N = 6, BIG = 300
        real(real64) :: x(N), y(N), c, r, inf, ninf, bx(BIG), by(BIG), want
        logical :: ok, had_inv, got_inv
        integer :: k

        inf = ieee_value(1.0_real64, ieee_positive_inf)
        ninf = -inf
        x = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64, 5.0_real64, 6.0_real64]
        y = [2.0_real64, 1.0_real64, 4.0_real64, 3.0_real64, 6.0_real64, 5.0_real64]

        call ieee_get_flag(ieee_invalid, had_inv)

        ! x negative only.
        x(3) = ninf
        call ieee_set_flag(ieee_invalid, .false.)
        call pf_cov(x, y, c, ok=ok)
        call ieee_get_flag(ieee_invalid, got_inv)
        call check(error, ieee_is_nan(c) .and. .not. ok, "a -Inf in x must leave pf_cov NaN")
        if (allocated(error)) return
        call check(error, .not. got_inv, "a -Inf in x raised IEEE_INVALID; nagfor's -ieee=stop would abort here")
        if (allocated(error)) return

        ! y negative only.
        x(3) = 3.0_real64
        y(4) = ninf
        call ieee_set_flag(ieee_invalid, .false.)
        call pf_cov(x, y, c, ok=ok)
        call ieee_get_flag(ieee_invalid, got_inv)
        call check(error, ieee_is_nan(c) .and. .not. ok, "a -Inf in y must leave pf_cov NaN")
        if (allocated(error)) return
        call check(error, .not. got_inv, "a -Inf in y raised IEEE_INVALID; nagfor's -ieee=stop would abort here")
        if (allocated(error)) return
        call ieee_set_flag(ieee_invalid, .false.)
        call pf_corr(x, y, r, ok=ok)
        call ieee_get_flag(ieee_invalid, got_inv)
        call check(error, ieee_is_nan(r) .and. .not. ok, "and pf_corr with it")
        if (allocated(error)) return
        call check(error, .not. got_inv, "pf_corr compared a NaN centred sum with <=, which raises IEEE_INVALID")
        if (allocated(error)) return

        ! y both signs, which cancel to a NaN mean rather than to an infinite one.
        y(2) = inf
        call ieee_set_flag(ieee_invalid, .false.)
        call pf_cov(x, y, c, ok=ok)
        call ieee_get_flag(ieee_invalid, got_inv)
        call check(error, ieee_is_nan(c) .and. .not. ok, "both signs in y must leave pf_cov NaN")
        if (allocated(error)) return
        call check(error, .not. got_inv, "both signs in y raised IEEE_INVALID; nagfor's -ieee=stop would abort here")
        if (allocated(error)) return
        call ieee_set_flag(ieee_invalid, had_inv)

        ! A population several blocks long, so the pair walk's roll-over runs. The answer is
        ! still the identity `pf_cov(x, x) == pf_variance(x)`, which is what makes a block that
        ! lost its partial sums visible rather than merely different.
        do k = 1, BIG
            bx(k) = real(mod(k * 37, 101), real64)
            by(k) = real(mod(k * 53, 97), real64)
        end do
        call pf_variance(bx, want)
        call pf_cov(bx, bx, c)
        call check(error, c == want, &
            "pf_cov(x, x) must still be pf_variance(x) once the pair walk spans several blocks")
        if (allocated(error)) return
        call pf_corr(bx, by, r, ok=ok)
        call check(error, ok .and. abs(r) <= 1.0_real64, &
            "a multi-block correlation must be a correlation")
    end subroutine test_pairwise_infinities_both_signs

    !> `pf_probit_fit` answers an infinite population with that infinity, not with arithmetic.
    !!
    !! The fit regresses the sorted values on their normal plotting positions, and an infinite
    !! extreme makes the whole line degenerate: the mean IS that infinity, every deviation from
    !! it is infinite or `Inf - Inf`, and the slope and correlation are NaN whichever way it is
    !! computed. The routine therefore reads the two extremes of its own sorted buffer and
    !! returns before the regression -- both because the answer is already known and because
    !! running it would evaluate `Inf - Inf` and raise IEEE_INVALID.
    !!
    !! Both signs at once leaves even `loc` undefined, since that limit is `-Inf + Inf`.
    subroutine test_probit_fit_infinite_extremes(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        real(real64) :: x(6), loc, sigma, corr, inf, ninf
        logical :: had_inv, got_inv

        inf = ieee_value(1.0_real64, ieee_positive_inf)
        ninf = -inf
        x = [3.0_real64, 1.0_real64, 4.0_real64, 1.0_real64, 5.0_real64, 9.0_real64]

        call ieee_get_flag(ieee_invalid, had_inv)
        call ieee_set_flag(ieee_invalid, .false.)

        x(6) = inf
        call pf_probit_fit(x, loc, sigma, corr=corr)
        call check(error, loc > huge(1.0_real64), "a +Inf population's loc IS that infinity")
        if (allocated(error)) return
        call check(error, ieee_is_nan(sigma) .and. ieee_is_nan(corr), &
            "its scale and correlation are undefined")
        if (allocated(error)) return

        x(6) = 9.0_real64
        x(2) = ninf
        call pf_probit_fit(x, loc, sigma, corr=corr)
        call check(error, loc < -huge(1.0_real64), "a -Inf population's loc is the mirror")
        if (allocated(error)) return

        x(6) = inf
        call pf_probit_fit(x, loc, sigma, corr=corr)
        call check(error, ieee_is_nan(loc), "both signs leave even loc undefined")
        if (allocated(error)) return

        call ieee_get_flag(ieee_invalid, got_inv)
        call ieee_set_flag(ieee_invalid, had_inv .or. got_inv)
        call check(error, .not. got_inv, &
            "pf_probit_fit ran its regression on an infinity and raised IEEE_INVALID")
    end subroutine test_probit_fit_infinite_extremes

    !> `inverted_cdf`, and the exact tie `nearest` has to break.
    !!
    !! A one-quantile unweighted call does not sort: it selects, which is a different
    !! implementation of every interpolation rule and has to answer what the ordering route
    !! answers. `inverted_cdf` had no unweighted caller at all, and `nearest` was only ever asked
    !! at positions that were not exact ties -- the one case where the rule has to say something
    !! beyond "the closer one".
    !!
    !! numpy breaks that tie to the EVEN zero-based index, so `p = 0.125` over five sorted values
    !! sits exactly between ranks 0 and 1 and takes rank 0. The two routes are compared against
    !! each other as well, since disagreeing there is a defect neither answer alone reveals.
    subroutine test_selection_route_and_nearest_tie(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        real(real64) :: x(5), q, qs(1), probs(1)

        x = [10.0_real64, 20.0_real64, 30.0_real64, 40.0_real64, 50.0_real64]

        ! The exact tie: p*(m-1) = 0.5, between ranks 0 and 1, and rank 0 is the even one.
        call pf_quantile(x, 0.125_real64, q, method="nearest")
        call check(error, q == 10.0_real64, "nearest must break an exact tie to the even rank")
        if (allocated(error)) return
        ! Its odd-index mirror, which must go the OTHER way: p*(m-1) = 1.5 takes rank 2.
        call pf_quantile(x, 0.375_real64, q, method="nearest")
        call check(error, q == 30.0_real64, "at an odd lower rank the tie goes upward")
        if (allocated(error)) return

        ! `inverted_cdf` unweighted, which reaches the selection route rather than the ordering
        ! one: the smallest value whose cumulative share reaches p.
        call pf_quantile(x, 0.5_real64, q, method="inverted_cdf")
        call check(error, q == 30.0_real64, "inverted_cdf at p=0.5 over five values is the third")
        if (allocated(error)) return
        call pf_quantile(x, 0.01_real64, q, method="inverted_cdf")
        call check(error, q == 10.0_real64, "and just above zero it is the smallest")
        if (allocated(error)) return
        call pf_quantile(x, 1.0_real64, q, method="inverted_cdf")
        call check(error, q == 50.0_real64, "and at one it is the largest")
        if (allocated(error)) return

        ! The vector form must answer the same, over the ordering route both take as shipped.
        ! The SELECTION route's own `inverted_cdf` arm is reached only through the debug
        ! threshold, and rides `test_selection_and_sort_paths_agree`, which owns that override.
        probs = [0.5_real64]
        call pf_quantiles(x, probs, qs, method="inverted_cdf")
        call check(error, qs(1) == 30.0_real64, "pf_quantiles disagrees with pf_quantile")
    end subroutine test_selection_route_and_nearest_tie

    !> A weight large enough to ABSORB the rest of the population puts every position at 0.
    !!
    !! The weighted quantile scale is `run / (total - w(i))`, and that denominator vanishes when
    !! one weight is so much larger than the others that the total rounds to it. The element is
    !! then simultaneously the minimum and the maximum of the weighted population, and position 0
    !! is what keeps the position sequence non-decreasing -- without which the walk that finds
    !! the bracketing pair would step past the end.
    !!
    !! This is an ordinary consequence of finite precision rather than a contrived input: a
    !! weight of `1/err**2` over a measurement with a tiny error produces it.
    subroutine test_absorbing_weight_keeps_positions_ordered(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        real(real64) :: x(3), w(3), q, prev
        !> **`volatile`, and load-bearing rather than decoration.** The precondition below asks
        !! whether the STORED sum still equals `w(1)`, and written plainly it is the algebraic
        !! identity `(a + b + c) - a == 0`, which a compiler free to reassociate folds to
        !! `b + c` -- 2e-300, not 0 -- so the fixture reports itself unusable when it is exactly
        !! what the test needs. ifx 2026.1 does that at `-O0`, `-fp-model fast` being its default;
        !! it is also a Heisenbug, since printing `total` first is enough to hide it. `volatile`
        !! forces the value to be read back instead of the expression that produced it, which is
        !! the question being asked.
        real(real64), volatile :: total
        integer :: k

        x = [1.0_real64, 2.0_real64, 3.0_real64]
        w = [1.0e308_real64, 1.0e-300_real64, 1.0e-300_real64]
        ! The precondition, read rather than assumed: the two small weights must vanish into the
        ! sum, or the denominator this test is about never reaches zero.
        total = w(1) + w(2) + w(3)
        call check(error, total - w(1) == 0.0_real64, &
            "the small weights did not absorb -- this fixture cannot reach the guard")
        if (allocated(error)) return

        ! The two ends are exact by rule, and everything between them is bracketed and
        ! non-decreasing -- which is what a position sequence that stayed ordered buys, and what
        ! a `0/0` position would have destroyed.
        call pf_quantile(x, 0.0_real64, q, weights=w)
        call check(error, q == 1.0_real64, "q(0) must be the smallest value")
        if (allocated(error)) return
        call pf_quantile(x, 1.0_real64, q, weights=w)
        call check(error, q == 3.0_real64, "q(1) must be the largest value")
        if (allocated(error)) return
        prev = 1.0_real64
        do k = 0, 10
            call pf_quantile(x, real(k, real64) / 10.0_real64, q, weights=w)
            call check(error, .not. ieee_is_nan(q) .and. q >= 1.0_real64 .and. q <= 3.0_real64, &
                "an absorbed denominator left a quantile outside the population")
            if (allocated(error)) return
            call check(error, q >= prev, "the weighted quantiles are not non-decreasing in p")
            if (allocated(error)) return
            prev = q
        end do
    end subroutine test_absorbing_weight_keeps_positions_ordered

    !> A weight near `huge` does not overflow the levels that never read `sum(w**2)`.
    !!
    !! **A contract about the process rather than the answer**, and the twin of
    !! `test_no_reduction_raises_on_a_non_finite_population` one weight-shaped step along. Squaring
    !! a weight above about 1.3e154 overflows, and `stats_engine` formed `sum(w**2)` for every
    !! caller regardless of the moment level it was asked for -- so `pf_sum`, `pf_mean` and every
    !! ORDER statistic (which reach pass one at level 0, through `stats_compact`) executed an
    !! overflow on a population they answer about perfectly well. Under gfortran and ifx that is a
    !! line at program exit attached to nothing; under nagfor, whose default `-ieee=stop` unmasks
    !! the traps, the process DIES and takes every later suite in the runner with it.
    !!
    !! The fix is that the total is formed at level 2 and above, where the variance family reads
    !! it, and nowhere below -- so this test calls exactly the entry points BELOW that line and
    !! asserts each answers. `1/err**2` over a precise measurement reaches this range, so the
    !! fixture is an ordinary input. The variance family itself is out of scope here: it needs the
    !! quantity, and what it should answer for a weight this large is its own question.
    subroutine test_weight_near_huge_survives_the_lower_levels(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        real(real64) :: x(3), w(3), s, mn, q, med

        x = [1.0_real64, 2.0_real64, 3.0_real64]
        ! The precondition, read rather than assumed: this weight must be past the square root of
        ! `huge`, or squaring it does not overflow and the test asserts nothing.
        w = [1.0e308_real64, 1.0e-300_real64, 1.0e-300_real64]
        call check(error, w(1) > sqrt(huge(0.0_real64)), &
            "the weight is not large enough to overflow when squared -- this fixture proves nothing")
        if (allocated(error)) return

        ! Level 0: the plain sum, and the order tier that shares its compaction.
        call pf_sum(x, s, weights=w)
        call check(error, s == w(1) + w(2) + w(3), "pf_sum(weights=) did not answer the weighted total")
        if (allocated(error)) return
        call pf_median(x, med, weights=w)
        call check(error, med >= 1.0_real64 .and. med <= 3.0_real64, "pf_median(weights=) left the population")
        if (allocated(error)) return
        call pf_quantile(x, 0.25_real64, q, weights=w)
        call check(error, q >= 1.0_real64 .and. q <= 3.0_real64, "pf_quantile(weights=) left the population")
        if (allocated(error)) return

        ! Level 1: the refined mean. The absorbing weight puts it on that element's value.
        call pf_mean(x, mn, weights=w)
        call check(error, mn == 1.0_real64, "pf_mean(weights=) did not collapse onto the absorbing weight's value")
    end subroutine test_weight_near_huge_survives_the_lower_levels

    !> No reduction RAISES an IEEE exception on a non-finite population.
    !!
    !! **This is a contract about the process, not about the answer**, and it needs its own test
    !! because every symptom of breaking it is somewhere else. A NaN or an infinity reaching an
    !! ordered comparison -- `<`, `<=`, `>`, `>=` -- raises IEEE_INVALID, and:
    !!
    !! * under nagfor, whose default `-ieee=stop` unmasks the traps, the process DIES, taking
    !!   every later suite in that runner with it and reporting the abort beside whichever test
    !!   happened to be printing;
    !! * under gfortran and ifx it is one line at program exit, attached to nothing, which is
    !!   indistinguishable from noise.
    !!
    !! So the ordinary suite cannot see it, and four sites were raising: `pf_zscore`'s
    !! `sd > 0` test, `stats_skew`'s and `stats_kurt`'s `p2 <= 0` test, and `pf_corr`'s
    !! `sxx <= 0` test -- each against a value that is a NaN for exactly the populations this
    !! module documents itself as ANSWERING rather than refusing. `/=` is the one quiet
    !! predicate, so each now screens the NaN with it first, as its own statement.
    !!
    !! The flag is saved and restored around the whole test, and each call gets a cleared flag of
    !! its own so that a failure names the procedure rather than the batch. Guarded on
    !! `ieee_support_flag`, since a processor need not support reading it at all.
    subroutine test_no_reduction_raises_on_a_non_finite_population(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        real(real64) :: x(6), y(6), z(6), nan, inf, a, b, c, med, sd
        logical :: raised, had, ok
        character(len=200) :: culprits
        type(pf_stats) :: s

        if (.not. ieee_support_flag(ieee_invalid, 1.0_real64)) then
            call skip_test(error, "this build cannot read IEEE_INVALID")
            return
        end if

        culprits = ""
        nan = ieee_value(1.0_real64, ieee_quiet_nan)
        inf = ieee_value(1.0_real64, ieee_positive_inf)
        x = [1.0_real64, 2.0_real64, nan, 4.0_real64, 5.0_real64, 6.0_real64]
        y = [2.0_real64, 1.0_real64, 4.0_real64, 3.0_real64, 6.0_real64, 5.0_real64]
        call ieee_get_flag(ieee_invalid, had)

        ! A NaN KEPT in the population: every one of these answers NaN, and none may compare it.
        call ieee_set_flag(ieee_invalid, .false.)
        call pf_zscore(x, z, skipnan=.false.)
        call ieee_get_flag(ieee_invalid, raised)
        if (raised) culprits = trim(culprits) // " pf_zscore(kept NaN)"
        call ieee_set_flag(ieee_invalid, .false.)
        call pf_normal_scores(x, z, skipnan=.false.)
        call ieee_get_flag(ieee_invalid, raised)
        if (raised) culprits = trim(culprits) // " pf_normal_scores(kept NaN)"
        call ieee_set_flag(ieee_invalid, .false.)
        call pf_median(x, a, skipnan=.false.)
        call ieee_get_flag(ieee_invalid, raised)
        if (raised) culprits = trim(culprits) // " pf_median(kept NaN)"
        call ieee_set_flag(ieee_invalid, .false.)
        call pf_mad(x, a, skipnan=.false.)
        call ieee_get_flag(ieee_invalid, raised)
        if (raised) culprits = trim(culprits) // " pf_mad(kept NaN)"
        call ieee_set_flag(ieee_invalid, .false.)
        call pf_sigma_clipped_stats(x, a, med, sd, skipnan=.false.)
        call ieee_get_flag(ieee_invalid, raised)
        if (raised) culprits = trim(culprits) // " pf_sigma_clipped_stats(kept NaN)"
        call ieee_set_flag(ieee_invalid, .false.)
        call pf_probit_fit(x, a, b, corr=c, skipnan=.false.)
        call ieee_get_flag(ieee_invalid, raised)
        if (raised) culprits = trim(culprits) // " pf_probit_fit(kept NaN)"
        call ieee_set_flag(ieee_invalid, .false.)
        call pf_probit_scale(x, a, skipnan=.false.)
        call ieee_get_flag(ieee_invalid, raised)
        if (raised) culprits = trim(culprits) // " pf_probit_scale(kept NaN)"
        call ieee_set_flag(ieee_invalid, .false.)
        call s%compute(x, skipnan=.false.)
        a = s%mad()
        b = s%quantile(0.5_real64)
        c = s%probit_scale()
        call ieee_get_flag(ieee_invalid, raised)
        if (raised) culprits = trim(culprits) // " pf_stats reads(kept NaN)"

        ! An INFINITY in the population, which poisons the central moments without a NaN ever
        ! being in the data the caller supplied.
        x(3) = inf
        call ieee_set_flag(ieee_invalid, .false.)
        call pf_zscore(x, z)
        call ieee_get_flag(ieee_invalid, raised)
        if (raised) culprits = trim(culprits) // " pf_zscore(infinity)"
        call ieee_set_flag(ieee_invalid, .false.)
        call pf_skewness(x, a)
        call ieee_get_flag(ieee_invalid, raised)
        if (raised) culprits = trim(culprits) // " pf_skewness(infinity)"
        call ieee_set_flag(ieee_invalid, .false.)
        call pf_kurtosis(x, a)
        call ieee_get_flag(ieee_invalid, raised)
        if (raised) culprits = trim(culprits) // " pf_kurtosis(infinity)"
        call ieee_set_flag(ieee_invalid, .false.)
        call pf_sigma_clipped_stats(x, a, med, sd, ok=ok)
        call ieee_get_flag(ieee_invalid, raised)
        if (raised) culprits = trim(culprits) // " pf_sigma_clipped_stats(infinity)"
        call ieee_set_flag(ieee_invalid, .false.)
        call pf_corr(x, y, a)
        call ieee_get_flag(ieee_invalid, raised)
        if (raised) culprits = trim(culprits) // " pf_corr(infinity)"
        call ieee_set_flag(ieee_invalid, .false.)
        call pf_corr(x, y, a, method="spearman")
        call ieee_get_flag(ieee_invalid, raised)
        if (raised) culprits = trim(culprits) // " pf_corr spearman(infinity)"

        call ieee_set_flag(ieee_invalid, had)
        call check(error, len_trim(culprits) == 0, &
            "these raised IEEE_INVALID on a population they answer rather than refuse:" // &
            trim(culprits))
    end subroutine test_no_reduction_raises_on_a_non_finite_population

end module test_stats
