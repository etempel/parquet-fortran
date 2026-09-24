!> Tests for `parquet_kde`: `pf_kde`'s kernels, bandwidth rules, population rules, boundary
!> corrections, exact queries and curve, `pf_kde_grid`'s deposit, merge and interpolated queries,
!> the adaptive kernel in both forms, and `%sample` in both forms.
!!
!! **The grid is tested against the exact estimate**, which the golden vectors pin: its density
!! converges to `pf_kde%pdf` as the square of the cell width, its deposit conserves every point's
!! weight to rounding, and the weight it counts beyond its range is the exact estimate's own
!! distribution function there.
!!
!! **The adaptive kernel is tested against an exact pilot.** The oracle sums the fixed estimate
!! at every point for the pilot and integrates its entropy for `g`, so the library, reading a
!! pilot grid, agrees to that grid's discretisation: to about `2e-3` at the rule's quarter
!! bandwidth, and -- in `kde_serial`, where `parquet_debug_set_kde_pilot_cells` makes the pilot
!! fine -- to `1e-9`, falling as the square of the pilot's cell width. `alpha = 0` is the fixed
!! estimate bit for bit, in both forms, which is what keeps the adaptive loop honest.
!!
!! **`%sample` is tested by its recipe and its moments**, never by a goodness-of-fit statistic. The
!! recipe -- which stream and which draw each element reads -- is reproduced here from the generator
!! itself, beside a control arm without the family's label that must match nowhere; the moments of a
!! sample from each kernel, corrected at a bound or not, are asserted against the kernel's own at a
!! tolerance of five standard errors for the sample's size. The seeds are fixed, so each assertion is
!! deterministic.
!!
!! **Every expectation is derived, never read off a run.** The golden vectors come from
!! `tools/generate_kde_vectors.py`, a 50-digit oracle that sums every kernel of every point;
!! the kernel identities (unit mass, unit variance, a CDF that is the density's integral, unit
!! mass inside a bounded support) are checked against `pf_integrate`, which shares nothing with
!! the estimator; and the rules are re-formed here from `pf_stddev` and `pf_iqr`'s own answers.
!!
!! **Every test whose answer depends on the bandwidth rule names the rule**, or passes the
!! bandwidth as a number, except the two that assert the default: `test_default_rule_is_isj`,
!! where the Improved Sheather-Jones rule finds a bandwidth, and `test_isj_fallback`, where it finds
!! none and Silverman's rule stands in.
!!
!! **The ISJ rule is pinned at the oracle's grid.** At its default of `2**14` cells the rule's
!! transform is beyond a 50-digit oracle, so `kde_serial` forces 1024 cells through
!! `parquet_debug_set_kde_isj_cells` and asserts the golden cases there, where the library agrees
!! with the oracle to a few units of rounding; at the default grid the tests assert relations that
!! need no oracle -- the default is the rule, the rule is not a rule of thumb, the grid moves the
!! answer by less than a per cent, weights follow their convention and the column forms change
!! nothing.
!!
!! Two suites. `kde` is pure in-memory work and runs concurrently. `kde_serial` holds the tests
!! that write process-global state -- they silence `%print` through the `verbosity` setting, force
!! the pilot's or the ISJ rule's cells through `parquet_debug_set_kde_pilot_cells` and
!! `parquet_debug_set_kde_isj_cells`, or turn the boundary scan's grid off through
!! `parquet_debug_set_kde_scan_grid` and read the counters back through
!! `parquet_debug_kde_scan_counts` -- and is on `suite_is_safe_to_parallelize`'s exclusion list.
!! Both are registered in `run_tester_pf.f90`, the runner that executes no `bind(C)` call.
module test_kde

    use testdrive, only : new_unittest, unittest_type, error_type, check
    use parquet_kde
    use parquet_stats, only : pf_stddev, pf_iqr, pf_count_valid
    use parquet_integrate, only : pf_integrand, pf_integrate, pf_integration_info
    use parquet_utils, only : pf_norm_pdf, pf_norm_cdf
    use parquet_random, only : pf_random_at, pf_random_int_at, pf_random_normal_at, pf_random_key
    use parquet_columns, only : parquet_column, PK_INT32, PK_INT64, PK_FLOAT32, PK_FLOAT64
    use test_kde_golden
    use iso_fortran_env, only : int32, int64, real32, real64
    use, intrinsic :: ieee_arithmetic, only : ieee_value, ieee_quiet_nan, ieee_positive_inf, &
        ieee_is_nan, ieee_is_finite, ieee_get_flag, ieee_set_flag, ieee_support_flag, ieee_underflow, &
        ieee_support_underflow_control, ieee_get_underflow_mode, ieee_usual

    implicit none
    private

    public :: collect_tests_kde, collect_tests_kde_serial

    !> The four kernels' tokens, in the order the module's kernel table lists them.
    character(len=12), parameter :: KERNELS(4) = [character(len=12) :: &
        "gaussian", "epanechnikov", "bspline", "box"]

    !> Each kernel's support radius in standard deviations: the Gaussian's cut at five, and
    !> `sqrt(5)`, `2*sqrt(3)` and `sqrt(3)` for the three compact ones.
    real(real64), parameter :: RADIUS(4) = [5.0_real64, sqrt(5.0_real64), 2.0_real64*sqrt(3.0_real64), &
        sqrt(3.0_real64)]

    !> Where each kernel's density has a kink or a jump inside its support, in standard deviations:
    !> the B-spline's pieces meet at `+-sqrt(3)`; the others are smooth inside.
    real(real64), parameter :: BSPLINE_KNOT = sqrt(3.0_real64)

    !> The estimate as a function, `x**power * f(x)`, for `pf_integrate`: the extension the guide
    !> shows. A module-level type, because a callback in this library is never internal.
    type, extends(pf_integrand) :: kde_density
        type(pf_kde) :: k              !! the fitted estimate
        integer      :: power = 0      !! 0 for the density, 2 for its second moment
    contains
        procedure :: eval => kde_density_eval !! `x**power` times the density at `x`
    end type kde_density

    !> The squared error of a fitted estimate against the two-component normal mixture
    !> `MIX_W N(0, 1) + (1 - MIX_W) N(MIX_MU, MIX_SD)`, for `pf_integrate`.
    type, extends(pf_integrand) :: kde_mix_error
        type(pf_kde) :: k !! the fitted estimate
    contains
        procedure :: eval => kde_mix_error_eval !! the squared error at `x`
    end type kde_mix_error

    !> The mixture `test_isj_error_falls_with_n` draws from: one broad component and one narrow one
    !> at an IRREGULAR offset, which is the shape the rule handles well. Several narrow components
    !> at a REGULAR spacing are its blind spot, and the guide says so; a fixture of that shape would
    !> fail this test by design rather than by regression.
    real(real64), parameter :: MIX_W = 0.6_real64, MIX_MU = 3.0_real64, MIX_SD = 0.4_real64

    !> The squared error of a bounded estimate against the density `2x` on `[0, 1]`, for
    !> `pf_integrate`: what the boundary corrections are compared by over a zone.
    type, extends(pf_integrand) :: kde_sq_error
        type(pf_kde) :: k !! the fitted estimate
    contains
        procedure :: eval => kde_sq_error_eval !! the squared error at `x`
    end type kde_sq_error

    !> A grid's interpolated density as a function, `x**power * f(x)`, for `pf_integrate`.
    type, extends(pf_integrand) :: grid_density
        type(pf_kde_grid) :: g         !! the grid
        integer           :: power = 0 !! 0 for the density, 1 and 2 for its moments
    contains
        procedure :: eval => grid_density_eval !! `x**power` times the grid's `%pdf` at `x`
    end type grid_density

    !> The label `pf_kde%sample` derives its key with, copied from `src/parquet_kde.f90`'s
    !> `KDE_FAMILY_LABEL`: the recipe test reproduces every draw from it, so a changed label -- which
    !> changes every sample a program has ever drawn -- fails that test rather than passing unseen.
    integer(int64), parameter :: KDE_LABEL = 7089359947230782746_int64

    !> `pf_kde_grid%sample`'s label, copied from `KDE_GRID_FAMILY_LABEL` for the same reason.
    integer(int64), parameter :: KDE_GRID_LABEL = 8280789554566260118_int64

contains

    !> Registers the concurrent suite.
    subroutine collect_tests_kde(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:) !! Receives the suite's tests.

        testsuite = [ &
            new_unittest("the fixture recipe matches the generator's", test_fixture_matches_generator), &
            new_unittest("every kernel has unit mass, unit variance and a matching CDF", &
                test_kernel_identities), &
            new_unittest("the fixed estimate reproduces the golden vectors", test_golden_vectors), &
            new_unittest("the default rule is isj", test_default_rule_is_isj), &
            new_unittest("the isj rule's error falls with the sample on an ordinary mixture", &
                test_isj_error_falls_with_n), &
            new_unittest("isj finds a narrower bandwidth than silverman on a bimodal sample", &
                test_isj_narrower_on_bimodal), &
            new_unittest("without a root the default falls back to silverman and rule=isj is undefined", &
                test_isj_fallback), &
            new_unittest("isj refuses a bandwidth finer than the sample's own resolution", &
                test_isj_refuses_sub_gap), &
            new_unittest("pf_kde_bandwidth answers %fit's own number, bit for bit", &
                test_standalone_bandwidth), &
            new_unittest("rule=lscv minimises cross-validation and scores the estimator in force", &
                test_lscv_rule), &
            new_unittest("%curve's default method is binned only where binned is accurate", &
                test_curve_default_method), &
            new_unittest("the isj rule's sum forms no factor it does not use", &
                test_isj_sum_forms_no_unused_factor), &
            new_unittest("isj weights: frequency weights replicate, equal reliability weights are none", &
                test_isj_weights), &
            new_unittest("the column forms of %fit and %add equal the array forms, bit for bit", &
                test_column_forms), &
            new_unittest("silverman and scott reproduce their formulas", test_rules_match_formulas), &
            new_unittest("adjust= multiplies whichever bandwidth was chosen", test_adjust_multiplies), &
            new_unittest("n_eff follows the weight type", test_n_eff_follows_weight_type), &
            new_unittest("frequency weights are replication", test_frequency_weights_replicate), &
            new_unittest("the population rules are the family's", test_population_rules), &
            new_unittest("a NaN under skipnan=.false. poisons the estimate", test_skipnan_false_poisons), &
            new_unittest("an empty and a degenerate population are quiet", test_quiet_returns), &
            new_unittest("lower/upper conserve mass under both corrections", test_bounds_conserve_mass), &
            new_unittest("every correction is unbiased at a flat bound", test_corrections_unbiased_at_a_bound), &
            new_unittest("a bound without boundary= resolves to reflect", test_default_boundary_is_reflect), &
            new_unittest("a bandwidth the estimate cannot use leaves it undefined", &
                test_bandwidth_admission), &
            new_unittest("the grid's lifecycle: %finish closes it and %clear reopens it", &
                test_grid_lifecycle), &
            new_unittest("a point outside the support is excluded and counted", test_outside_excluded), &
            new_unittest("%quantile inverts %cdf", test_quantile_inverts_cdf), &
            new_unittest("%curve spans min - cut*h to max + cut*h, clipped to the support", &
                test_curve_default_range), &
            new_unittest("single precision input widens", test_real32_widens), &
            new_unittest("the array forms equal the scalar forms", test_array_forms_match_scalar), &
            new_unittest("the tokens and the accessors report the fit", test_tokens_and_accessors), &
            new_unittest("a fit given no kernel takes the B-spline, on both types", &
                test_default_kernel_is_the_bspline), &
            new_unittest("a refit replaces everything and %clear unfits", test_refit_and_clear), &
            new_unittest("%print writes the summary and says when there is none", test_print_writes), &
            new_unittest("the linear correction reproduces the golden vectors", test_linear_golden_vectors), &
            new_unittest("the grid counts weight beyond a free edge under linear", &
                test_grid_linear_free_edge), &
            new_unittest("an adaptive point wider than the grid poisons it under linear", &
                test_grid_linear_reach_poisons), &
            new_unittest("R3 counts the over-reaching points and %merge sums the counts", &
                test_grid_overreach_counted), &
            new_unittest("the pilot of a linear fit keeps R2", test_pilot_linear_keeps_r2), &
            new_unittest("the linear estimate is never negative", test_linear_never_negative), &
            new_unittest("at a rising bound the linear correction is closer than both simple ones", &
                test_linear_beats_the_simple_corrections), &
            new_unittest("%cdf is continuous across the zone edges and integrates %pdf", &
                test_linear_cdf_across_zones), &
            new_unittest("the scan finds a stretch between two close kernel edges", &
                test_linear_scan_refinement), &
            new_unittest("tied values and coinciding knots abort nothing", &
                test_linear_tied_values_and_knots), &
            new_unittest("the guide's rising-density table is what the estimator answers", &
                test_guide_boundary_table), &
            new_unittest("the grid converges to the exact estimate as step**2", test_grid_converges), &
            new_unittest("the grid deposits exactly w/step per point", test_grid_deposits_exact_mass), &
            new_unittest("the grid counts the weight beyond its range at each end", &
                test_grid_counts_weight_beyond_range), &
            new_unittest("a narrow kernel lands whole in one or two cells", test_grid_narrow_kernel), &
            new_unittest("%pdf on a grid interpolates and is exact at the centres", &
                test_grid_pdf_interpolates), &
            new_unittest("the grid's %cdf integrates its %pdf and %quantile inverts it", &
                test_grid_cdf_and_quantile), &
            new_unittest("%merge equals one grid over the concatenation", test_grid_merge), &
            new_unittest("the grid applies the population rules on every %add", &
                test_grid_population_rules), &
            new_unittest("an empty grid answers zeros and a poisoned one NaN", &
                test_grid_empty_and_poisoned), &
            new_unittest("the grid's accessors report its set-up and %clear keeps it", &
                test_grid_accessors_and_clear), &
            new_unittest("the grid's %print writes the summary and says when there is none", &
                test_grid_print_writes), &
            new_unittest("alpha=0 is the fixed estimate, alpha=0.5 is not", test_alpha_zero_is_fixed), &
            new_unittest("an adaptive fit widens a rule's bandwidth and leaves an explicit one alone", &
                test_adaptive_inflates_a_rule), &
            new_unittest("the adaptive bandwidths follow the pilot", test_adaptive_bandwidths_follow_pilot), &
            new_unittest("the adaptive golden case", test_adaptive_golden), &
            new_unittest("bandwidth_max caps every h_j", test_bandwidth_max_caps), &
            new_unittest("spread_max caps h_max/h_min, and the tighter of the two caps binds", &
                test_spread_max_caps), &
            new_unittest("%pilot returns the grid the fit used", test_pilot_is_the_fits), &
            new_unittest("%fit(pilot=) applies a smoothing measured on another sample", &
                test_fit_takes_a_pilot), &
            new_unittest("a grid may reach beyond its pilot, where the no-density rule applies", &
                test_grid_wider_than_its_pilot), &
            new_unittest("the pilot's cells follow the rule and its clamps", test_pilot_cells_rule), &
            new_unittest("the two forms agree under one pilot", test_two_forms_agree_under_one_pilot), &
            new_unittest("the adaptive kernel conserves mass under both corrections", &
                test_adaptive_conserves_mass), &
            new_unittest("%bandwidth_at is the rule %fit applied", test_bandwidth_at), &
            new_unittest("the adaptive grid copies its pilot, merges and is poisoned by a poisoned one", &
                test_adaptive_grid), &
            new_unittest("a grid on a pilot with nothing in its cells answers NaN", &
                test_empty_pilot_is_quiet), &
            new_unittest("the adaptive fit's accessors, printer, real32 form and refit", &
                test_adaptive_accessors), &
            new_unittest("a bandwidth too large to use leaves the estimate undefined", &
                test_unusable_bandwidth), &
            new_unittest("%sample is addressed by (seed, stream, k)", test_sample_addressing), &
            new_unittest("%sample respects the support and the kernel", test_sample_support_and_kernel), &
            new_unittest("%sample draws each point's own weight and bandwidth", test_sample_weights_and_bandwidths), &
            new_unittest("the grid's %sample follows its %pdf inside its range", test_grid_sample), &
            new_unittest('method="exact" is the default bit for bit, and %method round-trips', &
                test_binned_method_token), &
            new_unittest("the binned grid converges to the exact estimate as the square of the cell width", &
                test_binned_converges), &
            new_unittest("the binned grid reproduces the exact estimate for every kernel", &
                test_binned_every_kernel), &
            new_unittest("the binned grid's cells and counters hold the weight deposited", &
                test_binned_conserves_weight), &
            new_unittest("the binned grid carries the boundary correction at both bounds", &
                test_binned_boundaries), &
            new_unittest("the binned grid finishes once, clears and merges", test_binned_lifecycle), &
            new_unittest("a binned grid's cells do not depend on the thread count", test_binned_threads), &
            new_unittest('%curve(method="binned") reproduces the exact curve at matched points', &
                test_binned_curve), &
            new_unittest('binned boundary="linear" matches the exact grid at both bounds', &
                test_binned_linear), &
            new_unittest('binned adaptive with boundary="linear" matches the exact grid', &
                test_binned_adaptive_linear), &
            new_unittest("the binned grid reproduces the golden vectors", test_binned_golden), &
            new_unittest('%fit(method="binned") answers from its own grid, and %method says so', &
                test_fit_binned_method), &
            new_unittest("the isj rule counts the points sitting on its own bounds", &
                test_isj_counts_points_on_its_own_bounds), &
            new_unittest("lscv weights reach both the whole-population and the subsample arms", &
                test_lscv_weights_reach_both_subsample_arms), &
            new_unittest("an upper-bounded linear pilot keeps R2 at its free edge", &
                test_an_upper_bounded_linear_pilot_keeps_r2), &
            new_unittest("a corrected pilot with no mass in its cells leaves the grid NaN", &
                test_a_corrected_pilot_with_no_mass), &
            new_unittest("every grid kernel token round-trips", &
                test_every_grid_kernel_token_round_trips), &
            new_unittest("a binned grid deposits per-point weights", &
                test_a_binned_grid_deposits_weights), &
            new_unittest("a binned adaptive grid prints its bandwidth classes", &
                test_a_binned_adaptive_grid_prints_its_classes), &
            new_unittest("%merge keeps an explicit bandwidth_max", &
                test_merge_keeps_an_explicit_bandwidth_max), &
            new_unittest("the grid's spread_max caps its bandwidths", &
                test_the_grids_spread_max_caps_its_bandwidths), &
            new_unittest("an undefined fit reports no bandwidths and no points", &
                test_an_undefined_fit_reports_no_bandwidths), &
            new_unittest("a binned fit answers the scalar queries from its grid", &
                test_a_binned_fit_answers_from_its_grid_everywhere), &
            new_unittest("the clip grows its list of stretches and loses none", &
                test_the_clip_grows_its_list_of_stretches), &
            new_unittest("an overflowing adaptive rule leaves the fit undefined", &
                test_an_overflowing_adaptive_rule_leaves_the_fit_undefined), &
            new_unittest("an overflowing adaptive rule poisons the grid", &
                test_an_overflowing_adaptive_rule_poisons_the_grid), &
            new_unittest("a sample beyond half the largest number has no range to build over", &
                test_a_sample_beyond_half_the_largest_number), &
            new_unittest("a weighted binned fit matches the exact one", &
                test_a_weighted_binned_fit_matches_the_exact_one), &
            new_unittest("a binned curve of an adaptive weighted fit", &
                test_a_binned_curve_of_an_adaptive_weighted_fit), &
            new_unittest("a binned linear fit bounded on one side keeps R2", &
                test_a_binned_linear_fit_bounded_on_one_side), &
            new_unittest("a variate beyond the gaussian cut is redrawn", &
                test_a_variate_beyond_the_gaussian_cut_is_redrawn), &
            new_unittest("%sample draws the upper corrected zone", test_the_upper_zone_sampler), &
            new_unittest("%sample where the two zones meet", test_a_sample_where_the_zones_meet), &
            new_unittest("the scan jumps across a gap at the bound", &
                test_the_scan_jumps_across_a_gap_at_the_bound), &
            new_unittest("the binned class ladder is capped", &
                test_the_binned_class_ladder_is_capped), &
            new_unittest("a reflecting grid whose mass underflows poisons itself", &
                test_a_reflecting_grid_whose_mass_underflows), &
            new_unittest("a reflected image landing on the last padded centre", &
                test_a_reflected_image_on_the_last_padded_centre), &
            new_unittest("a draw from two crossing zones", test_a_draw_from_two_crossing_zones), &
            new_unittest("the class ladder stops at the widest bandwidth", &
                test_the_class_ladder_stops_at_the_widest_bandwidth), &
            new_unittest("an adaptive upper zone reads its window three ways", &
                test_an_adaptive_upper_zone_reads_its_window_three_ways) &
            ]

    end subroutine collect_tests_kde

    !> Registers the suite that writes process-global state and so runs serially.
    subroutine collect_tests_kde_serial(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:) !! Receives the suite's tests.

        testsuite = [ &
            new_unittest("verbosity silences %print", test_print_silenced), &
            new_unittest("verbosity silences the grid's %print", test_grid_print_silenced), &
            new_unittest("the adaptive golden case with a fine pilot", test_adaptive_golden_fine), &
            new_unittest("parquet_debug_set_kde_pilot_cells forces the pilot's cells", &
                test_pilot_cells_forced), &
            new_unittest("isj reproduces the golden vectors at 1024 cells", test_isj_golden), &
            new_unittest("the zone sampler's fallback inverts the zone's integral", &
                test_zone_sampler_fallback), &
            new_unittest("parquet_debug_set_kde_isj_cells sets the rule's grid, which moves it by under 1%", &
                test_isj_cells_forced), &
            new_unittest("the binned adaptive kernel splits each point between two bandwidth classes", &
                test_binned_adaptive), &
            new_unittest("the boundary scan's grid changes where the exact estimator is asked, " // &
                "not what the fit answers", test_scan_grid_agrees), &
            new_unittest("the fixed arm's LSCV criterion scores the same by transform and by pairs", &
                test_lscv_transform_agrees), &
            new_unittest("the LSCV criterion reproduces the golden vectors by both routes", &
                test_lscv_criterion_golden), &
            new_unittest("parquet_debug_kde_fit_nanos reports the last fit's phases", &
                test_the_fit_timer_reports_the_last_fit), &
            new_unittest("parquet_debug_kde_lscv_at reads weights", &
                test_the_lscv_hook_reads_weights), &
            new_unittest("the scan's grid is declined when its transform is too long", &
                test_the_scans_grid_is_declined_when_it_is_too_long) &
            ]

    end subroutine collect_tests_kde_serial

    ! ==========================================================================================
    ! Fixtures and helpers
    ! ==========================================================================================

    !> The recipe every golden case is taken over: `test_stats.f90`'s `golden_fixture`, which
    !> `tools/generate_kde_vectors.py` imports from `tools/generate_stats_vectors.py`. Every value
    !> is an integer over a power of two, so both languages hold it exactly.
    subroutine kde_fixture(n, x)
        integer(int64), intent(in)             :: n    !! how many values
        real(real64), allocatable, intent(out) :: x(:) !! the values
        integer(int64) :: i, a

        allocate(x(n))
        do i = 1_int64, n
            a = mod(i*i*7919_int64 + 12345_int64, 1000003_int64)
            x(i) = real(a - 500001_int64, real64)/1024.0_real64
        end do

    end subroutine kde_fixture

    !> The adaptive cases' recipe, `two_component` in `tools/generate_kde_vectors.py`: two points in
    !> three in a narrow cluster about -300 (within 32 of it) and the third in a cluster five times
    !> as wide about +300, `v` a spread integer from the same recipe. Every value is an integer over
    !> 16, exact in both languages.
    subroutine kde_two_component(n, x)
        integer(int64), intent(in)             :: n    !! how many values
        real(real64), allocatable, intent(out) :: x(:) !! the values
        integer(int64) :: i, a, v

        allocate(x(n))
        do i = 1_int64, n
            a = mod(i*i*7919_int64 + 12345_int64, 1000003_int64)
            v = mod(a, 1024_int64) - 512_int64
            if (mod(i, 3_int64) == 0_int64) then
                x(i) = 300.0_real64 + 5.0_real64*real(v, real64)/16.0_real64
            else
                x(i) = -300.0_real64 + real(v, real64)/16.0_real64
            end if
        end do

    end subroutine kde_two_component

    !> `w(i) = mod(i, 5)`: every fifth weight is zero and removes its element.
    subroutine kde_weights_mod5(n, w)
        integer(int64), intent(in)             :: n    !! how many weights
        real(real64), allocatable, intent(out) :: w(:) !! the weights
        integer(int64) :: i

        allocate(w(n))
        do i = 1_int64, n
            w(i) = real(mod(i, 5_int64), real64)
        end do

    end subroutine kde_weights_mod5

    !> `x` rounded to multiples of `step`, `step*floor(x/step + 1/2)`: `two_component_rounded` in the
    !> generator, exact in both languages for a power-of-two `step`, since every value of the recipe
    !> is an integer over 16.
    subroutine kde_rounded(step, x)
        real(real64), intent(in)    :: step !! the rounding step
        real(real64), intent(inout) :: x(:) !! the values, rounded in place
        integer(int64) :: i

        do i = 1_int64, size(x, kind=int64)
            x(i) = step*real(floor(x(i)/step + 0.5_real64, kind=int64), real64)
        end do

    end subroutine kde_rounded

    !> `x**power` times the density at `x`.
    function kde_density_eval(this, x) result(f)
        class(kde_density), intent(inout) :: this !! the estimate as a function
        real(real64), intent(in)          :: x    !! where to evaluate
        real(real64)                      :: f    !! the value

        call this%k%pdf(x, f)
        if (this%power /= 0) f = f*x**this%power

    end function kde_density_eval

    !> The squared error against `2x` at `x`.
    function kde_sq_error_eval(this, x) result(f)
        class(kde_sq_error), intent(inout) :: this !! the estimate as a function
        real(real64), intent(in)           :: x    !! where to evaluate
        real(real64)                       :: f    !! the squared error

        call this%k%pdf(x, f)
        f = (f - 2.0_real64*x)**2

    end function kde_sq_error_eval

    !> Two hundred quantiles of the density `2x` on `[0, 1]`, `sqrt((i - 1/2)/n)`: the guide's
    !> example, whose density vanishes at the lower bound, so that the raw linear estimate is
    !> negative there and the clip has something to remove.
    subroutine kde_rising(n, x)
        integer, intent(in)       :: n    !! how many points
        real(real64), intent(out) :: x(:) !! the quantiles
        integer :: i

        do i = 1, n
            x(i) = sqrt((real(i, real64) - 0.5_real64)/real(n, real64))
        end do

    end subroutine kde_rising

    !> The quantiles of `Exp(1)`, `-log(1 - (i - 1/2)/n)`: a density that is LARGE at its bound, so
    !> that most of its mass -- and so most of a sample's draws -- lies inside the boundary zone.
    subroutine kde_exponential(n, x)
        integer, intent(in)       :: n    !! how many points
        real(real64), intent(out) :: x(:) !! the quantiles
        integer :: i

        do i = 1, n
            x(i) = -log(1.0_real64 - (real(i, real64) - 0.5_real64)/real(n, real64))
        end do

    end subroutine kde_exponential

    !> The quantiles of the density `3(1 - x)**2` on `[0, 1]`, `1 - (1 - u)**(1/3)`: the mirror of
    !> `kde_rising`, vanishing at the UPPER bound.
    subroutine kde_falling(n, x)
        integer, intent(in)       :: n    !! how many points
        real(real64), intent(out) :: x(:) !! the quantiles
        integer :: i

        do i = 1, n
            x(i) = 1.0_real64 - (1.0_real64 - (real(i, real64) - 0.5_real64)/real(n, real64))**(1.0_real64/3.0_real64)
        end do

    end subroutine kde_falling

    !> `x**power` times the grid's interpolated density at `x`.
    function grid_density_eval(this, x) result(f)
        class(grid_density), intent(inout) :: this !! the grid as a function
        real(real64), intent(in)            :: x    !! where to evaluate
        real(real64)                        :: f    !! the value

        call this%g%pdf(x, f)
        if (this%power /= 0) f = f*x**this%power

    end function grid_density_eval

    !> The mean, variance and kurtosis of `v`, over its own mean.
    pure subroutine moments(v, mean, var, kurt)
        real(real64), intent(in)  :: v(:) !! the values
        real(real64), intent(out) :: mean !! their mean
        real(real64), intent(out) :: var  !! their variance, over `size(v)`
        real(real64), intent(out) :: kurt !! their fourth central moment over the variance squared
        integer(int64) :: i, n
        real(real64) :: d, m2, m4

        n = size(v, kind=int64)
        mean = sum(v)/real(n, real64)
        m2 = 0.0_real64
        m4 = 0.0_real64
        do i = 1_int64, n
            d = v(i) - mean
            m2 = m2 + d*d
            m4 = m4 + (d*d)*(d*d)
        end do
        var = m2/real(n, real64)
        kurt = (m4/real(n, real64))/(var*var)

    end subroutine moments

    !> How many pairs of elements of `v` are equal.
    pure function equal_pairs(v) result(c)
        real(real64), intent(in) :: v(:) !! the values
        integer(int64)           :: c    !! the pairs
        integer(int64) :: i, j

        c = 0_int64
        do i = 1_int64, size(v, kind=int64)
            do j = i + 1_int64, size(v, kind=int64)
                if (v(i) == v(j)) c = c + 1_int64
            end do
        end do

    end function equal_pairs

    !> `.true.` when `a` and `b` agree to `rel` of `scale`, or are both NaN.
    pure function close_to(a, b, rel, scale) result(res)
        real(real64), intent(in) :: a     !! the value
        real(real64), intent(in) :: b     !! the expectation
        real(real64), intent(in) :: rel   !! the tolerance, relative to `scale`
        real(real64), intent(in) :: scale !! the magnitude the tolerance is relative to
        logical                  :: res   !! they agree

        if (ieee_is_nan(a) .or. ieee_is_nan(b)) then
            res = ieee_is_nan(a) .and. ieee_is_nan(b)
            return
        end if
        res = abs(a - b) <= rel*scale

    end function close_to

    !> Makes the `pf_kde%fit` call a golden case names; the arguments mirror the generator's CASES.
    subroutine fit_golden_case(name, k, ok)
        character(len=*), intent(in) :: name !! the case
        type(pf_kde), intent(inout)  :: k    !! fitted here
        logical, intent(out)         :: ok   !! `%fit`'s `ok`
        real(real64), allocatable :: x(:), w(:)
        integer(int64) :: n

        n = 32_int64
        if (name == "N1000") n = 1000_int64
        if (name == "ONE_H" .or. name == "ONE_RULE") n = 1_int64
        if (name(1:min(5, len(name))) == "ADAPT") n = 60_int64
        if (name(1:min(3, len(name))) == "ISJ" .and. name /= "ISJ_NO_ROOT") n = 60_int64
        if (name == "LIN_ZERO") n = 60_int64
        if (n == 60_int64) then
            call kde_two_component(n, x)
        else
            call kde_fixture(n, x)
        end if
        call kde_weights_mod5(n, w)
        select case (name)
        case ("SILVERMAN")
            call k%fit(x, kernel="gaussian", rule="silverman", ok=ok)
        case ("SCOTT")
            call k%fit(x, kernel="gaussian", rule="scott", ok=ok)
        case ("ADJUST")
            call k%fit(x, kernel="gaussian", rule="silverman", adjust=1.5_real64, ok=ok)
        case ("N1000")
            call k%fit(x, kernel="gaussian", rule="silverman", ok=ok)
        case ("GAUSS")
            call k%fit(x, kernel="gaussian", bandwidth=60.0_real64, ok=ok)
        case ("EPAN")
            call k%fit(x, bandwidth=60.0_real64, kernel="epanechnikov", ok=ok)
        case ("BSPL")
            call k%fit(x, bandwidth=60.0_real64, kernel="bspline", ok=ok)
        case ("BOX")
            call k%fit(x, bandwidth=60.0_real64, kernel="box", ok=ok)
        case ("WREL")
            call k%fit(x, kernel="gaussian", rule="silverman", weights=w, ok=ok)
        case ("WFREQ")
            call k%fit(x, kernel="gaussian", rule="silverman", weights=w, weight_type="frequency", ok=ok)
        case ("REN_LO")
            call k%fit(x, kernel="gaussian", bandwidth=60.0_real64, lower=-470.0_real64, boundary="renormalise", ok=ok)
        case ("REN_BOTH")
            call k%fit(x, bandwidth=60.0_real64, kernel="epanechnikov", lower=-470.0_real64, &
                upper=460.0_real64, boundary="renormalise", ok=ok)
        case ("REF_LO")
            call k%fit(x, bandwidth=60.0_real64, kernel="bspline", lower=-470.0_real64, &
                boundary="reflect", ok=ok)
        case ("REF_HI")
            call k%fit(x, bandwidth=60.0_real64, kernel="box", upper=460.0_real64, &
                boundary="reflect", ok=ok)
        case ("REF_WIDE")
            call k%fit(x, kernel="gaussian", bandwidth=400.0_real64, lower=-470.0_real64, upper=460.0_real64, &
                boundary="reflect", ok=ok)
        case ("REN_WIDE")
            call k%fit(x, bandwidth=600.0_real64, kernel="box", lower=-470.0_real64, &
                upper=460.0_real64, boundary="renormalise", ok=ok)
        case ("RULE_BOUNDED")
            call k%fit(x, kernel="gaussian", rule="silverman", lower=-400.0_real64, upper=400.0_real64, ok=ok)
        case ("ONE_H")
            call k%fit(x, kernel="gaussian", bandwidth=10.0_real64, ok=ok)
        case ("ONE_RULE")
            call k%fit(x, kernel="gaussian", rule="silverman", ok=ok)
        case ("ADAPT")
            call k%fit(x, kernel="gaussian", rule="silverman", adaptive=.true., ok=ok)
        case ("ADAPT_CAP")
            call k%fit(x, bandwidth=80.0_real64, kernel="bspline", adaptive=.true., alpha=1.0_real64, &
                bandwidth_max=120.0_real64, lower=-340.0_real64, boundary="renormalise", ok=ok)
        case ("ADAPT_REF")
            call k%fit(x, bandwidth=60.0_real64, kernel="epanechnikov", adaptive=.true., lower=-340.0_real64, &
                upper=470.0_real64, boundary="reflect", ok=ok)
        case ("ADAPT_W")
            call k%fit(x, kernel="gaussian", rule="silverman", adaptive=.true., weights=w, ok=ok)
        case ("ISJ")
            call k%fit(x, kernel="gaussian", rule="isj", ok=ok)
        case ("ISJ_WREL")
            call k%fit(x, kernel="gaussian", rule="isj", weights=w, ok=ok)
        case ("ISJ_WFREQ")
            call k%fit(x, kernel="gaussian", rule="isj", weights=w, weight_type="frequency", ok=ok)
        case ("ISJ_BOUNDED")
            call k%fit(x, kernel="gaussian", rule="isj", lower=-331.625_real64, upper=470.0_real64, ok=ok)
        case ("ISJ_ROUNDED")
            call kde_rounded(8.0_real64, x)
            call k%fit(x, kernel="gaussian", rule="isj", ok=ok)
        case ("ISJ_NO_ROOT")
            call k%fit(x, kernel="gaussian", rule="isj", ok=ok)
        case ("LIN_LO")
            call k%fit(x, kernel="gaussian", bandwidth=60.0_real64, lower=-470.0_real64, boundary="linear", ok=ok)
        case ("LIN_HI_BOX")
            call k%fit(x, bandwidth=60.0_real64, kernel="box", upper=460.0_real64, boundary="linear", ok=ok)
        case ("LIN_BOTH")
            call k%fit(x, bandwidth=60.0_real64, kernel="epanechnikov", lower=-470.0_real64, &
                upper=460.0_real64, boundary="linear", ok=ok)
        case ("LIN_BSPL")
            call k%fit(x, bandwidth=60.0_real64, kernel="bspline", lower=-470.0_real64, &
                boundary="linear", ok=ok)
        case ("LIN_WIDE")
            call k%fit(x, kernel="gaussian", bandwidth=400.0_real64, lower=-470.0_real64, upper=460.0_real64, &
                boundary="linear", ok=ok)
        case ("LIN_W")
            call k%fit(x, kernel="gaussian", bandwidth=60.0_real64, weights=w, lower=-470.0_real64, boundary="linear", ok=ok)
        case ("LIN_ZERO")
            call k%fit(x, kernel="gaussian", bandwidth=60.0_real64, lower=-470.0_real64, boundary="linear", ok=ok)
        case ("ADAPT_LIN")
            call k%fit(x, kernel="gaussian", bandwidth=60.0_real64, adaptive=.true., lower=-470.0_real64, &
                boundary="linear", ok=ok)
        case ("LIN_NARROW3")
            call k%fit(x, kernel="gaussian", bandwidth=930000.0_real64, lower=-470.0_real64, upper=460.0_real64, &
                boundary="linear", ok=ok)
        case ("LIN_NARROW6")
            call k%fit(x, bandwidth=930000000.0_real64, kernel="epanechnikov", lower=-470.0_real64, &
                upper=460.0_real64, boundary="linear", ok=ok)
        case default
            error stop "fit_golden_case: unknown case " // name
        end select

    end subroutine fit_golden_case

    !> Compares one golden case: `ok`, the bandwidth, and the density and CDF at every probe.
    subroutine check_golden_case(error, name, def, h, pdf, cdf)
        type(error_type), allocatable, intent(out) :: error    !! set on the first failed check
        character(len=*), intent(in)               :: name     !! the case
        logical, intent(in)                        :: def      !! the case is defined
        real(real64), intent(in)                   :: h        !! the bandwidth
        real(real64), intent(in)                   :: pdf(NKX) !! the density at `KG_X`
        real(real64), intent(in)                   :: cdf(NKX) !! the CDF at `KG_X`
        type(pf_kde) :: k
        logical :: ok
        real(real64) :: got_pdf(NKX), got_cdf(NKX), peak
        integer :: i
        character(len=200) :: msg

        call fit_golden_case(name, k, ok)
        call check(error, ok .eqv. def, name // ": ok must be " // merge("T", "F", def))
        if (allocated(error)) return
        call k%pdf(KG_X, got_pdf)
        call k%cdf(KG_X, got_cdf)
        if (.not. def) then
            call check(error, ieee_is_nan(k%bandwidth()) .and. all(ieee_is_nan(got_pdf)) .and. &
                all(ieee_is_nan(got_cdf)), name // ": an undefined estimate must answer NaN everywhere")
            return
        end if
        call check(error, close_to(k%bandwidth(), h, 1.0e-13_real64, h), &
            name // ": the bandwidth must match the oracle")
        if (allocated(error)) return
        peak = maxval(pdf)
        do i = 1, NKX
            write(msg, '(a,a,es24.16,a,es24.16,a,es24.16)') name, ": pdf at ", KG_X(i), " is ", &
                got_pdf(i), ", oracle ", pdf(i)
            call check(error, close_to(got_pdf(i), pdf(i), 1.0e-12_real64, peak), trim(msg))
            if (allocated(error)) return
            write(msg, '(a,a,es24.16,a,es24.16,a,es24.16)') name, ": cdf at ", KG_X(i), " is ", &
                got_cdf(i), ", oracle ", cdf(i)
            call check(error, close_to(got_cdf(i), cdf(i), 1.0e-13_real64, 1.0_real64), trim(msg))
            if (allocated(error)) return
        end do

    end subroutine check_golden_case

    !> Reads a unit's lines back, counting them and noting whether one contains `needle`.
    subroutine read_back(path, nlines, needle, seen)
        character(len=*), intent(in) :: path   !! the file
        integer, intent(out)         :: nlines !! how many lines it holds
        character(len=*), intent(in) :: needle !! text to look for
        logical, intent(out)         :: seen   !! some line contains it
        integer :: u, ios
        character(len=256) :: line

        nlines = 0
        seen = .false.
        open(newunit=u, file=path, status="old", action="read")
        do
            read(u, '(a)', iostat=ios) line
            if (ios /= 0) exit
            nlines = nlines + 1
            if (index(line, needle) > 0) seen = .true.
        end do
        close(u, status="delete")

    end subroutine read_back

    ! ==========================================================================================
    ! The tests
    ! ==========================================================================================

    !> The Fortran recipe produces the generator's first values exactly.
    subroutine test_fixture_matches_generator(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        real(real64), allocatable :: x(:)

        call kde_fixture(int(size(KG_PROBE), int64), x)
        call check(error, all(x == KG_PROBE), &
            "kde_fixture has drifted from generate_stats_vectors.py's fixture(): every golden " // &
            "case below would fail for that reason")
        if (allocated(error)) return
        call kde_two_component(int(size(KG_PROBE2), int64), x)
        call check(error, all(x == KG_PROBE2), &
            "kde_two_component has drifted from generate_kde_vectors.py's two_component(): every " // &
            "adaptive golden case would fail for that reason")

    end subroutine test_fixture_matches_generator

    !> One point at zero with bandwidth one: the estimate IS the kernel in standard-deviation
    !> units. Its integral is one, its second moment is one (the cut Gaussian's is
    !> `1 - 10 phi(5)/erf(5/sqrt 2)`, the variance the cut removes), and `%cdf` differences equal
    !> integrals of `%pdf` -- every one of them computed by `pf_integrate`, which shares nothing with
    !> the kernels. A wrong scale factor moves the variance; a wrong CDF polynomial or support
    !> radius moves the differences.
    subroutine test_kernel_identities(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(kde_density) :: fn
        real(real64) :: r, mass, var, want_var, c1, c2, integral
        real(real64), parameter :: PHI5 = 1.4867195147342977e-6_real64
        real(real64), parameter :: EDGES(5) = [-2.9_real64, -1.2_real64, 0.3_real64, 1.9_real64, &
            3.1_real64]
        integer :: kk, i
        real(real64), allocatable :: cuts(:)

        do kk = 1, 4
            r = RADIUS(kk)
            call fn%k%fit([0.0_real64], bandwidth=1.0_real64, kernel=trim(KERNELS(kk)))
            if (trim(KERNELS(kk)) == "bspline") then
                cuts = [-BSPLINE_KNOT, 0.0_real64, BSPLINE_KNOT]
            else
                cuts = [0.0_real64]
            end if
            fn%power = 0
            mass = pf_integrate(fn, -r, r, 1.0e-13_real64, breakpoints=cuts)
            call check(error, abs(mass - 1.0_real64) <= 1.0e-12_real64, &
                trim(KERNELS(kk)) // ": the kernel must integrate to one")
            if (allocated(error)) return
            fn%power = 2
            var = pf_integrate(fn, -r, r, 1.0e-13_real64, breakpoints=cuts)
            want_var = 1.0_real64
            if (kk == 1) want_var = 1.0_real64 - 10.0_real64*PHI5/(1.0_real64 - 5.7330314375838782e-7_real64)
            call check(error, abs(var - want_var) <= 1.0e-12_real64, &
                trim(KERNELS(kk)) // ": the kernel's variance must be one (the bandwidth is its sd)")
            if (allocated(error)) return
            fn%power = 0
            do i = 1, size(EDGES) - 1
                if (.not. (max(-r, EDGES(i)) < min(r, EDGES(i + 1)))) cycle
                call fn%k%cdf(max(-r, EDGES(i)), c1)
                call fn%k%cdf(min(r, EDGES(i + 1)), c2)
                integral = pf_integrate(fn, max(-r, EDGES(i)), min(r, EDGES(i + 1)), 1.0e-13_real64)
                if (trim(KERNELS(kk)) == "bspline") then
                    ! The B-spline's knots at +-sqrt(3) fall inside two of these intervals.
                    integral = integrate_split(fn, max(-r, EDGES(i)), min(r, EDGES(i + 1)))
                end if
                call check(error, abs((c2 - c1) - integral) <= 1.0e-12_real64, &
                    trim(KERNELS(kk)) // ": %cdf differences must equal the integral of %pdf")
                if (allocated(error)) return
            end do
            call fn%k%cdf(-r, c1)
            call fn%k%cdf(r, c2)
            call check(error, c1 == 0.0_real64 .and. c2 == 1.0_real64, &
                trim(KERNELS(kk)) // ": the CDF must be exactly 0 and 1 at the support radius")
            if (allocated(error)) return
        end do

    contains

        !> The integral over `[a, b]` with the B-spline's two knots cut out where they fall inside.
        function integrate_split(g, a, b) result(res)
            type(kde_density), intent(inout) :: g   !! the estimate as a function
            real(real64), intent(in)         :: a   !! lower bound
            real(real64), intent(in)         :: b   !! upper bound
            real(real64)                     :: res !! the integral
            real(real64), allocatable :: inner(:)

            inner = pack([-BSPLINE_KNOT, BSPLINE_KNOT], [-BSPLINE_KNOT, BSPLINE_KNOT] > a .and. &
                [-BSPLINE_KNOT, BSPLINE_KNOT] < b)
            if (size(inner) == 0) then
                res = pf_integrate(g, a, b, 1.0e-13_real64)
            else
                res = pf_integrate(g, a, b, 1.0e-13_real64, breakpoints=inner)
            end if

        end function integrate_split

    end subroutine test_kernel_identities

    !> Every golden case: the bandwidth, and the density and CDF at every probe, against the
    !> 50-digit oracle.
    subroutine test_golden_vectors(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check

        call check_golden_case(error, "SILVERMAN", KG_SILVERMAN_DEF, KG_SILVERMAN_H, KG_SILVERMAN_PDF, &
            KG_SILVERMAN_CDF)
        if (allocated(error)) return
        call check_golden_case(error, "SCOTT", KG_SCOTT_DEF, KG_SCOTT_H, KG_SCOTT_PDF, KG_SCOTT_CDF)
        if (allocated(error)) return
        call check_golden_case(error, "ADJUST", KG_ADJUST_DEF, KG_ADJUST_H, KG_ADJUST_PDF, KG_ADJUST_CDF)
        if (allocated(error)) return
        call check_golden_case(error, "N1000", KG_N1000_DEF, KG_N1000_H, KG_N1000_PDF, KG_N1000_CDF)
        if (allocated(error)) return
        call check_golden_case(error, "GAUSS", KG_GAUSS_DEF, KG_GAUSS_H, KG_GAUSS_PDF, KG_GAUSS_CDF)
        if (allocated(error)) return
        call check_golden_case(error, "EPAN", KG_EPAN_DEF, KG_EPAN_H, KG_EPAN_PDF, KG_EPAN_CDF)
        if (allocated(error)) return
        call check_golden_case(error, "BSPL", KG_BSPL_DEF, KG_BSPL_H, KG_BSPL_PDF, KG_BSPL_CDF)
        if (allocated(error)) return
        call check_golden_case(error, "BOX", KG_BOX_DEF, KG_BOX_H, KG_BOX_PDF, KG_BOX_CDF)
        if (allocated(error)) return
        call check_golden_case(error, "WREL", KG_WREL_DEF, KG_WREL_H, KG_WREL_PDF, KG_WREL_CDF)
        if (allocated(error)) return
        call check_golden_case(error, "WFREQ", KG_WFREQ_DEF, KG_WFREQ_H, KG_WFREQ_PDF, KG_WFREQ_CDF)
        if (allocated(error)) return
        call check_golden_case(error, "REN_LO", KG_REN_LO_DEF, KG_REN_LO_H, KG_REN_LO_PDF, KG_REN_LO_CDF)
        if (allocated(error)) return
        call check_golden_case(error, "REN_BOTH", KG_REN_BOTH_DEF, KG_REN_BOTH_H, KG_REN_BOTH_PDF, &
            KG_REN_BOTH_CDF)
        if (allocated(error)) return
        call check_golden_case(error, "REF_LO", KG_REF_LO_DEF, KG_REF_LO_H, KG_REF_LO_PDF, KG_REF_LO_CDF)
        if (allocated(error)) return
        call check_golden_case(error, "REF_HI", KG_REF_HI_DEF, KG_REF_HI_H, KG_REF_HI_PDF, KG_REF_HI_CDF)
        if (allocated(error)) return
        call check_golden_case(error, "REF_WIDE", KG_REF_WIDE_DEF, KG_REF_WIDE_H, KG_REF_WIDE_PDF, &
            KG_REF_WIDE_CDF)
        if (allocated(error)) return
        call check_golden_case(error, "REN_WIDE", KG_REN_WIDE_DEF, KG_REN_WIDE_H, KG_REN_WIDE_PDF, &
            KG_REN_WIDE_CDF)
        if (allocated(error)) return
        call check_golden_case(error, "RULE_BOUNDED", KG_RULE_BOUNDED_DEF, KG_RULE_BOUNDED_H, &
            KG_RULE_BOUNDED_PDF, KG_RULE_BOUNDED_CDF)
        if (allocated(error)) return
        call check_golden_case(error, "ONE_H", KG_ONE_H_DEF, KG_ONE_H_H, KG_ONE_H_PDF, KG_ONE_H_CDF)
        if (allocated(error)) return
        call check_golden_case(error, "ONE_RULE", KG_ONE_RULE_DEF, KG_ONE_RULE_H, KG_ONE_RULE_PDF, &
            KG_ONE_RULE_CDF)

    end subroutine test_golden_vectors

    !> With no `rule=` and no `bandwidth=`, the rule is the Improved Sheather-Jones rule: the fit is a
    !> fit naming `"isj"`, bit for bit, and `%rule` says `"isj"`. On the two-component recipe it is
    !> also not Silverman's bandwidth, which reads the spread between the clusters and is several
    !> times wider -- the guard against a default that quietly stayed a rule of thumb.
    subroutine test_default_rule_is_isj(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde) :: a, b, c
        real(real64), allocatable :: x(:)
        character(len=:), allocatable :: name

        call kde_two_component(60_int64, x)
        call a%fit(x)
        call b%fit(x, rule="isj")
        call c%fit(x, rule="silverman")
        call a%rule(name)
        call check(error, name == "isj" .and. a%bandwidth() == b%bandwidth(), &
            "the default rule must be isj")
        if (allocated(error)) return
        call check(error, a%bandwidth() < 0.5_real64*c%bandwidth(), &
            "the default's bandwidth must not be silverman's on two clusters")

    end subroutine test_default_rule_is_isj

    !> The rule's purpose: on two narrow clusters far apart, a rule of thumb reads the spread
    !> between them and oversmooths both, while the ISJ rule reads the clusters' own width. On the
    !> two-component recipe at n = 240 the ISJ bandwidth is about a tenth of Silverman's (the
    !> generator's model gives 9.07 against 86.9); below a quarter is asserted, which a rule that
    !> fell back or computed a rule of thumb cannot meet. `adjust` multiplies it exactly.
    subroutine test_isj_narrower_on_bimodal(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde) :: isj, silv, half
        real(real64), allocatable :: x(:)
        character(len=:), allocatable :: name
        logical :: ok

        call kde_two_component(240_int64, x)
        call isj%fit(x, rule="isj", ok=ok)
        call silv%fit(x, rule="silverman")
        call isj%rule(name)
        call check(error, ok .and. name == "isj", "the rule must find a bandwidth on two clusters")
        if (allocated(error)) return
        call check(error, isj%bandwidth() < 0.25_real64*silv%bandwidth(), &
            "the ISJ bandwidth must be well below silverman's on two clusters")
        if (allocated(error)) return
        call half%fit(x, rule="isj", adjust=0.5_real64)
        call check(error, half%bandwidth() == 0.5_real64*isj%bandwidth(), &
            "adjust must multiply the ISJ rule's bandwidth")

    end subroutine test_isj_narrower_on_bimodal

    !> A sample the rule finds no bandwidth for, one per way it can find none: the recipe at n = 32,
    !> whose fixed point is negative all the way to `t = 1`, and the two-component recipe rounded to
    !> multiples of 8, whose fixed point is not negative at one cell (the generator asserts both at
    !> 1024 cells, ISJ_NO_ROOT and ISJ_ROUNDED; this asserts them at the default grid). Under the
    !> default Silverman's rule gives the bandwidth, bit for bit, and `%rule` says `"silverman"`;
    !> with `rule = "isj"` named the estimate is undefined -- `ok = .false.`, every answer NaN,
    !> `%rule` still `"isj"`. A constant sample has no bandwidth under either rule.
    subroutine test_isj_fallback(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde) :: d, s, i
        real(real64), allocatable :: x(:)
        real(real64) :: f(NKX)
        character(len=:), allocatable :: name
        logical :: okd, oki
        integer :: c

        do c = 1, 2
            if (c == 1) then
                call kde_fixture(32_int64, x)
            else
                call kde_two_component(60_int64, x)
                call kde_rounded(8.0_real64, x)
            end if
            call d%fit(x, ok=okd)
            call s%fit(x, rule="silverman")
            call i%fit(x, rule="isj", ok=oki)
            call d%rule(name)
            call check(error, okd .and. name == "silverman" .and. d%bandwidth() == s%bandwidth(), &
                trim(merge("the recipe at n = 32:", "the rounded recipe:  ", c == 1)) // &
                " the default must give silverman's bandwidth when the rule finds none")
            if (allocated(error)) return
            call i%pdf(KG_X, f)
            call i%rule(name)
            ! `"none"`, not `"isj"`: no rule produced a bandwidth, and naming the one that was
            ! ASKED for would imply one had. The rule tried is not reported back.
            call check(error, .not. oki .and. ieee_is_nan(i%bandwidth()) .and. all(ieee_is_nan(f)) .and. &
                name == "none", trim(merge("the recipe at n = 32:", "the rounded recipe:  ", c == 1)) // &
                " a named isj rule that finds no bandwidth must leave the estimate undefined")
            if (allocated(error)) return
        end do
        call d%fit([2.5_real64, 2.5_real64, 2.5_real64], ok=okd)
        call d%rule(name)
        call check(error, .not. okd .and. name == "none", &
            "a constant sample has no bandwidth under the default, and no rule is named for it")

    end subroutine test_isj_fallback

    !> With no `method=`, `%curve` fills itself by the binned transform only where that is as
    !! accurate as the exact sum, and by the exact sum everywhere else. Three conditions decide it,
    !! and this test walks all three, in each direction.
    !!
    !! **Which method ran is not reported, so it is read off the VALUES**: the default must equal
    !! one of the two explicit methods bit for bit, and on a fixture where the two differ, that
    !! says which one ran. The fixture is chosen so they do differ -- a curve on which binned and
    !! exact agreed would make every arm below pass whatever the default did.
    !!
    !! The correction arm carries the most weight. `"reflect"` keeps the binned curve's second
    !! order, because its images are the same kernel placed elsewhere; `"renormalise"` and
    !! `"linear"` vary along the curve and cost it an order, which is tens of times worse at a few
    !! thousand points -- exactly where a bounded density most needs a curve. If the condition is
    !! ever simplified back to the kernel alone, this is the arm that fails.
    subroutine test_curve_default_method(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        character(len=12), parameter :: SMOOTH(2) = [character(len=12) :: "gaussian", "bspline"]
        character(len=12), parameter :: ROUGH(2) = [character(len=12) :: "epanechnikov", "box"]
        character(len=11), parameter :: VARYING(2) = [character(len=11) :: "renormalise", "linear"]
        type(pf_kde) :: k
        real(real64), allocatable :: x(:)
        real(real64) :: xg(600), fd(600), fb(600), fe(600), x1(1), f1(1), fq(1)
        integer :: j

        call kde_fixture(400_int64, x)

        ! ---- the kernel: smooth kernels bin, rough ones do not ----
        do j = 1, 2
            call k%fit(x, rule="silverman", kernel=trim(SMOOTH(j)))
            call k%curve(xg, fd)
            call k%curve(xg, fb, method="binned")
            call k%curve(xg, fe, method="exact")
            call check(error, any(fb /= fe), trim(SMOOTH(j)) // &
                ": the fixture must tell the two methods apart, or the arms below assert nothing")
            if (allocated(error)) return
            call check(error, all(fd == fb), trim(SMOOTH(j)) // &
                ": a smooth kernel on a fine unbounded curve must default to the binned transform")
            if (allocated(error)) return

            ! `"reflect"` keeps the order, so it still bins.
            call k%fit(x, rule="silverman", kernel=trim(SMOOTH(j)), lower=-470.0_real64, &
                boundary="reflect")
            call k%curve(xg, fd)
            call k%curve(xg, fb, method="binned")
            call check(error, all(fd == fb), trim(SMOOTH(j)) // &
                ': boundary="reflect" keeps the binned curve''s order, so it must still bin')
            if (allocated(error)) return

            ! The corrections that vary along the curve cost it an order, so they do not.
            call k%fit(x, rule="silverman", kernel=trim(SMOOTH(j)), lower=-470.0_real64, &
                boundary=trim(VARYING(j)))
            call k%curve(xg, fd)
            call k%curve(xg, fe, method="exact")
            call check(error, all(fd == fe), trim(SMOOTH(j)) // " under " // trim(VARYING(j)) // &
                ": a correction that varies along the curve must fall back to the exact sum")
            if (allocated(error)) return
        end do

        do j = 1, 2
            call k%fit(x, rule="silverman", kernel=trim(ROUGH(j)))
            call k%curve(xg, fd)
            call k%curve(xg, fe, method="exact")
            call check(error, all(fd == fe), trim(ROUGH(j)) // &
                ": a kernel the binned estimate converges slowly for must default to the exact sum")
            if (allocated(error)) return
        end do

        ! ---- the spacing: a curve too coarse to bin accurately takes the exact sum ----
        call k%fit(x, rule="silverman")
        block
            real(real64) :: xc(9), fc(9), fce(9)
            call k%curve(xc, fc)
            call k%curve(xc, fce, method="exact")
            call check(error, all(fc == fce), &
                "a curve of nine points samples the kernel far too coarsely to bin: it must be exact")
        end block
        if (allocated(error)) return

        ! ---- one point: the default falls back rather than refusing ----
        call k%curve(x1, f1)
        call k%pdf(x1(1), fq(1))
        call check(error, f1(1) == fq(1), &
            "a one-point default curve must answer the exact density, not refuse")

    end subroutine test_curve_default_method

    !> `rule="lscv"` minimises the least-squares cross-validation criterion. It is the one rule here
    !! that is defined for whatever estimator is in force, which is what it is for: asked for with
    !! `adaptive = .true.` it scores the ADAPTIVE estimate and so returns that estimator's own
    !! bandwidth, which no plug-in rule can do.
    !!
    !! **The tolerance in the first arm is wide on purpose, and it is not slack.** LSCV is a
    !! high-variance criterion -- that is its known and documented weakness, the price of making no
    !! assumption about the density -- so on one sample of a thousand normals it lands some tens of
    !! per cent from the bandwidth that minimises the exact MISE, and would still be behaving
    !! correctly. What the assertion forbids is the failure that matters: a criterion that is not
    !! being minimised at all, or is minimised in the wrong direction, which misses by a factor.
    !! The reference is the closed-form normal-reference optimum `(4/3)**(1/5) n**(-1/5)`, exact for
    !! this fixture, not a recorded output.
    !!
    !! The second arm is the one that would catch the criterion being evaluated for the wrong
    !! estimator, which is the defect this rule exists to avoid: the adaptive estimator's own
    !! bandwidth is wider, and a criterion that quietly scored the fixed estimate under
    !! `adaptive = .true.` would answer the same number twice.
    subroutine test_lscv_rule(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde) :: k
        real(real64), allocatable :: x(:)
        real(real64) :: hfix, hada, href, h1, h2
        character(len=:), allocatable :: name
        integer(int64) :: i, n
        logical :: ok

        n = 1000_int64
        allocate(x(n))
        do i = 1_int64, n
            x(i) = pf_random_normal_at(11_int64, pf_random_key(1_int64, 1_int64), i)
        end do

        ! ---- (i) against the closed-form optimum for this density ----
        call k%fit(x, rule="lscv", ok=ok)
        hfix = k%bandwidth()
        call k%rule(name)
        href = (4.0_real64/3.0_real64)**0.2_real64*real(n, real64)**(-0.2_real64)
        call check(error, ok .and. name == "lscv", "rule=lscv must find a bandwidth and name itself")
        if (allocated(error)) return
        call check(error, hfix > 0.5_real64*href .and. hfix < 2.0_real64*href, &
            "the cross-validated bandwidth must land near the normal-reference optimum")
        if (allocated(error)) return

        ! ---- (ii) it scores the estimator in force ----
        call k%fit(x, rule="lscv", adaptive=.true., ok=ok)
        hada = k%bandwidth()
        call check(error, ok .and. hada > hfix, &
            "under adaptive=.true. lscv must return the adaptive estimator's own, wider bandwidth")
        if (allocated(error)) return

        ! ---- (iii) reproducible, and independent of the team ----
        call k%fit(x, rule="lscv", threads=1)
        h1 = k%bandwidth()
        call k%fit(x, rule="lscv", threads=4)
        h2 = k%bandwidth()
        call check(error, h1 == hfix .and. h2 == hfix, &
            "the subsample is addressed, not drawn afresh: lscv must answer the same bits every time")
        if (allocated(error)) return

        ! ---- (iv) a sample with no scale has no cross-validated bandwidth either ----
        call k%fit([2.5_real64, 2.5_real64, 2.5_real64], rule="lscv", ok=ok)
        call k%rule(name)
        call check(error, .not. ok .and. ieee_is_nan(k%bandwidth()) .and. name == "none", &
            "a constant sample must leave rule=lscv undefined, as every other rule leaves it")

    end subroutine test_lscv_rule

    !> `pf_kde_bandwidth` gives a caller the number without the estimate. The bit-identity with
    !! `%fit`'s own `%bandwidth()` IS the contract, and it is what stops the two drifting: every
    !! rule, weighted and unweighted, bounded and unbounded, fixed and adaptive.
    !!
    !! Exact equality is the right assertion here and not a fragile one. The two entry points do
    !! not merely compute the same thing, they run the SAME body -- `kde_fit_core`, which
    !! `pf_kde_bandwidth` stops once the bandwidth is known -- so there is no second rounding to
    !! differ by. A re-implementation, which is what this test exists to forbid, would show up as
    !! an inequality in the last bits rather than as a wrong number, and nothing else would see it.
    !!
    !! The population counts and `rule_used` come along for the same reason: the default falls back
    !! to Silverman's rule where the ISJ rule finds none, so a caller has to be able to ask which
    !! rule actually answered.
    subroutine test_standalone_bandwidth(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        character(len=9), parameter :: RULES(3) = [character(len=9) :: "isj", "silverman", "scott"]
        type(pf_kde) :: k
        real(real64), allocatable :: x(:), w(:)
        real(real32), allocatable :: x32(:)
        logical, allocatable :: bmask(:)
        type(parquet_column) :: bc
        real(real64) :: h, h32, hc
        character(len=:), allocatable :: want, got
        integer(int64) :: nn, nd, no, n, i
        logical :: ok1, ok2
        integer :: r

        call kde_fixture(400_int64, x)
        call kde_weights_mod5(400_int64, w)

        do r = 1, 3
            ! unbounded, unweighted
            call k%fit(x, rule=trim(RULES(r)), ok=ok1)
            call pf_kde_bandwidth(x, h, rule=trim(RULES(r)), ok=ok2, rule_used=got)
            call k%rule(want)
            call check(error, ok1 .eqv. ok2, trim(RULES(r)) // ": the two forms must agree on whether " // &
                "a rule found a bandwidth")
            if (allocated(error)) return
            call check(error, same_h(h, k%bandwidth()) .and. got == want, &
                trim(RULES(r)) // ": pf_kde_bandwidth must answer %fit's bandwidth and name the same rule")
            if (allocated(error)) return

            ! bounded and weighted, which routes through the same exclusion pass
            call k%fit(x, rule=trim(RULES(r)), weights=w, lower=-300.0_real64, upper=300.0_real64)
            call pf_kde_bandwidth(x, h, rule=trim(RULES(r)), weights=w, lower=-300.0_real64, &
                upper=300.0_real64, n_null=nn, n_nan=nd, n_outside=no)
            call check(error, same_h(h, k%bandwidth()), &
                trim(RULES(r)) // ": the two forms must agree under weights and a support")
            if (allocated(error)) return
            call check(error, nn == k%n_null() .and. nd == k%n_nan() .and. no == k%n_outside(), &
                trim(RULES(r)) // ": the population counts must be the same population's")
            if (allocated(error)) return

            ! adaptive: the number is the one an adaptive fit would actually use, widened
            call k%fit(x, rule=trim(RULES(r)), adaptive=.true.)
            call pf_kde_bandwidth(x, h, rule=trim(RULES(r)), adaptive=.true.)
            call check(error, same_h(h, k%bandwidth()), &
                trim(RULES(r)) // ": the adaptive form must carry the adaptive kernel's own bandwidth")
            if (allocated(error)) return
        end do

        ! adjust= composes, and the default's fallback is reported by name
        call k%fit(x, adjust=2.0_real64)
        call pf_kde_bandwidth(x, h, adjust=2.0_real64)
        call check(error, same_h(h, k%bandwidth()), "adjust= must reach the standalone form too")
        if (allocated(error)) return

        ! A constant sample has no scale: no rule finds one, and the answer says so.
        call pf_kde_bandwidth([2.5_real64, 2.5_real64, 2.5_real64], h, ok=ok2, rule_used=got)
        call check(error, .not. ok2 .and. ieee_is_nan(h) .and. got == "none", &
            "a constant sample must leave the standalone bandwidth undefined and name no rule")
        if (allocated(error)) return

        ! ---- the generic's OTHER TWO specifics ------------------------------------------------
        !
        ! `pf_kde_bandwidth` resolves over `real64`, `real32` and `parquet_column`, and the last
        ! two widen to the first and call it. Neither was reached by anything above: a generic is
        ! resolved at compile time, so a test that only ever passes a `real64` array exercises one
        ! specific and reports the other two as covered by nothing. Each is held to the widening
        ! it performs -- the `real32` form to the `real64` array of its own widened values, the
        ! column form to the array plus the mask its nulls become -- which is a stronger statement
        ! than "it answers something": a specific that widened wrongly, or dropped an argument on
        ! the way through, fails it.
        allocate(x32(size(x)))
        x32 = real(x, real32)
        call pf_kde_bandwidth(x32, h32, rule="silverman", ok=ok1, rule_used=got)
        call pf_kde_bandwidth(real(x32, real64), h, rule="silverman", ok=ok2, rule_used=want)
        call check(error, same_h(h32, h) .and. (ok1 .eqv. ok2) .and. got == want, &
            "the real32 form must answer the real64 form's number for its own widened values")
        if (allocated(error)) return

        n = int(size(x), int64)
        ! `allocate` first: assigning the scalar `.true.` to an unallocated allocatable array does
        ! NOT auto-allocate it -- only an array-valued right-hand side does -- so the mask has to
        ! be given its shape here.
        allocate(bmask(n))
        call bc%init(PK_FLOAT64, n)
        call bc%set_all(x)
        bmask = .true.
        do i = 1_int64, n, 7_int64
            call bc%set_null(i)
            bmask(i) = .false.
        end do
        call pf_kde_bandwidth(bc, hc, rule="silverman", n_null=nn)
        call pf_kde_bandwidth(x, h, rule="silverman", is_valid=bmask)
        call check(error, same_h(hc, h), &
            "the column form must answer what the array form does under the mask its nulls become")
        if (allocated(error)) return
        call check(error, nn == count(.not. bmask, kind=int64), &
            "and must count the nulls it excluded")
        if (allocated(error)) return
        ! The negative control: without the mask the array form answers a DIFFERENT number, so the
        ! equality above is not one that holds however the nulls are treated.
        call pf_kde_bandwidth(x, h, rule="silverman")
        call check(error, .not. same_h(hc, h), &
            "and the nulls must matter, or the column form proved nothing")

    end subroutine test_standalone_bandwidth

    !> Two bandwidths are the same answer when they are the same number, or when BOTH are the quiet
    !! NaN that says no rule found one. A bare `==` would call the second case a disagreement, and
    !! it is the case a named rule reaches on a sample it has no scale for.
    logical function same_h(a, b) result(res)
        real(real64), intent(in) :: a !! one bandwidth
        real(real64), intent(in) :: b !! the other
        if (ieee_is_nan(a) .or. ieee_is_nan(b)) then
            res = ieee_is_nan(a) .and. ieee_is_nan(b)
        else
            res = a == b
        end if
    end function same_h

    !> A bandwidth finer than the closest pair of distinct observations is resolving structure the
    !! sample cannot express, so the ISJ rule refuses it. On a standard normal of n = 1000 rounded
    !! to a step of 0.01 the fixed point has a root far below that step; taken, it would make every
    !! value its own spike. The refusal takes the path a rule that finds no bandwidth already takes:
    !! Silverman's rule when `rule=` was not named, an undefined estimate when `rule="isj"` was.
    !!
    !! The negative control is the SAME sample unrounded, whose closest pair is about 2e-6 against
    !! a bandwidth of about 0.3: without it the guard could be made to pass by disabling the rule.
    subroutine test_isj_refuses_sub_gap(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde) :: d, s, i
        real(real64), allocatable :: x(:), raw(:)
        real(real64) :: f(NKX)
        character(len=:), allocatable :: name
        logical :: okd, oki
        integer(int64) :: j

        allocate(raw(1000))
        do j = 1_int64, 1000_int64
            raw(j) = pf_random_normal_at(1_int64, pf_random_key(1_int64, 1_int64), j)
        end do

        ! ---- rounded: the rule finds a root at about 5.9e-04, a twentieth of the step ----
        x = raw
        call kde_rounded(0.01_real64, x)
        call d%fit(x, ok=okd)
        call d%rule(name)
        call s%fit(x, rule="silverman")
        call check(error, okd .and. name == "silverman" .and. d%bandwidth() == s%bandwidth(), &
            "a sub-step isj bandwidth must be refused and the default must fall back to silverman")
        if (allocated(error)) return
        call check(error, d%bandwidth() > 0.01_real64, &
            "the bandwidth the fallback gives must be wider than the sample's rounding step")
        if (allocated(error)) return

        ! Named, the refusal leaves the estimate undefined, quietly, as a rule that finds no scale
        ! does: a data condition, not an abort.
        call i%fit(x, rule="isj", ok=oki)
        call i%pdf(KG_X, f)
        call i%rule(name)
        call check(error, .not. oki .and. ieee_is_nan(i%bandwidth()) .and. all(ieee_is_nan(f)) .and. &
            name == "none", &
            "a named isj rule whose bandwidth is refused must leave the estimate undefined")
        if (allocated(error)) return

        ! ---- the negative control: the same sample, unrounded ----
        call i%fit(raw, rule="isj", ok=oki)
        call i%rule(name)
        call check(error, oki .and. name == "isj" .and. i%bandwidth() > 0.1_real64, &
            "the guard must not fire on a continuous sample, whose closest pair is far below its bandwidth")

    end subroutine test_isj_refuses_sub_gap

    !> The ISJ rule's norm forms no factor its sum does not use. `isj_norm` walks the terms
    !! `exp(-k**2 c)` by two running factors, and a factor formed past the last term kept is below
    !! the normal range once `c` is large, which the search for a fixed point reaches on a sample
    !! the rule finds no root for: the recipe at n = 60 formed both the `exp(-2c)` step and a
    !! run's first factor there, raising IEEE_UNDERFLOW for values nothing read. The fit runs on
    !! this thread (`threads = 1`), so the flag read here is the fit's; it is cleared around the
    !! call alone and restored as `saved .or. raised`. That the sample is still one without a root
    !! is asserted first, since a sample the rule solves never reaches the large `c`. A term the
    !! sum keeps can still underflow in its product with a small coefficient, which is accepted
    !! (`isj_norm`); this sample has none.
    subroutine test_isj_sum_forms_no_unused_factor(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde) :: k
        real(real64), allocatable :: x(:)
        character(len=:), allocatable :: name
        logical :: ok, can_test, saved, raised

        call kde_fixture(60_int64, x)
        can_test = ieee_support_flag(ieee_underflow, 1.0_real64)
        saved = .false.
        if (can_test) then
            call ieee_get_flag(ieee_underflow, saved)
            call ieee_set_flag(ieee_underflow, .false.)
        end if
        call k%fit(x, ok=ok, threads=1)
        raised = .false.
        if (can_test) then
            call ieee_get_flag(ieee_underflow, raised)
            call ieee_set_flag(ieee_underflow, saved .or. raised)
        end if
        call k%rule(name)
        call check(error, ok .and. name == "silverman", &
            "the fixture must be a sample the isj rule finds no root for, or it never reaches a large c")
        if (allocated(error)) return
        call check(error, .not. raised, &
            "the isj rule's search raised IEEE_UNDERFLOW: isj_norm formed a factor its sum does not use")

    end subroutine test_isj_sum_forms_no_unused_factor

    !> Weights under the ISJ rule. Frequency weights are replication: the two-component recipe under
    !> integer weights `mod(i, 5)` has the bandwidth of the sample with each value repeated that many
    !> times, to the rounding that summing the binned mass in another order leaves. Equal reliability
    !> weights are no weights: Kish's size is the population's, and the binned mass is the same
    !> once normalised. On unequal weights the two conventions differ, through `n_eff` alone (the
    !> golden ISJ_WREL and ISJ_WFREQ pin each).
    subroutine test_isj_weights(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde) :: wk, rk, uk, ek
        real(real64), allocatable :: x(:), w(:), rep(:), e(:)
        integer(int64) :: i, j, m

        call kde_two_component(60_int64, x)
        call kde_weights_mod5(60_int64, w)
        allocate(rep(int(sum(w), int64)))
        m = 0_int64
        do i = 1_int64, 60_int64
            do j = 1_int64, int(w(i), int64)
                m = m + 1_int64
                rep(m) = x(i)
            end do
        end do
        call wk%fit(x, rule="isj", weights=w, weight_type="frequency")
        call rk%fit(rep, rule="isj")
        call check(error, abs(wk%bandwidth() - rk%bandwidth()) <= 1.0e-12_real64*rk%bandwidth(), &
            "frequency weights must give the replicated sample's ISJ bandwidth")
        if (allocated(error)) return
        call uk%fit(x, rule="isj")
        allocate(e(60))
        e = 3.0_real64
        call ek%fit(x, rule="isj", weights=e)
        call check(error, abs(ek%bandwidth() - uk%bandwidth()) <= 1.0e-12_real64*uk%bandwidth(), &
            "equal reliability weights must give the unweighted ISJ bandwidth")
        if (allocated(error)) return
        call ek%fit(x, rule="isj", weights=w)
        call check(error, abs(ek%bandwidth() - wk%bandwidth()) > 0.1_real64*wk%bandwidth(), &
            "the two weight types must give different ISJ bandwidths on unequal weights")

    end subroutine test_isj_weights

    !> `%fit` and `%add` over a `parquet_column` answer exactly what they answer over the array of the
    !> same numbers, for each of the four numeric kinds, the column's own nulls reaching them as the
    !> equivalent `is_valid=` does. The `int32` column has no null, so its mask never exists and the
    !> array side passes none; the other three carry a null in every seventh row, and the `float64`
    !> one weights too. Bit for bit: the column is widened to the array the array form receives.
    subroutine test_column_forms(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(parquet_column) :: c
        type(pf_kde) :: kc, ka
        type(pf_kde_grid) :: gc, ga
        real(real64), allocatable :: x(:), wide(:), w(:), dc(:), da(:)
        logical, allocatable :: mask(:)
        integer(int32), allocatable :: iv(:)
        real(real64) :: fc(NKX), fa(NKX)
        integer(int64) :: i, n, nnc, nna
        integer :: kind
        character(len=12) :: tag

        n = 300_int64
        call kde_fixture(n, x)
        allocate(iv(n), mask(n), w(n), dc(64), da(64))
        iv = int(x, int32)
        call kde_weights_mod5(n, w)
        w = w + 0.5_real64
        do kind = 1, 4
            mask = .true.
            select case (kind)
            case (1)
                tag = "int32"
                call c%init(PK_INT32, n)
                call c%set_all(iv)
                wide = real(iv, real64)
            case (2)
                tag = "int64"
                call c%init(PK_INT64, n)
                call c%set_all(int(iv, int64))
                wide = real(iv, real64)
            case (3)
                tag = "float32"
                call c%init(PK_FLOAT32, n)
                call c%set_all(real(x, real32))
                wide = real(real(x, real32), real64)
            case default
                tag = "float64"
                call c%init(PK_FLOAT64, n)
                call c%set_all(x)
                wide = x
            end select
            if (kind > 1) then
                do i = 1_int64, n, 7_int64
                    call c%set_null(i)
                    mask(i) = .false.
                end do
            end if
            if (kind == 4) then
                call kc%fit(c, rule="isj", weights=w, n_null=nnc)
                call ka%fit(wide, rule="isj", is_valid=mask, weights=w, n_null=nna)
            else if (kind == 1) then
                call kc%fit(c, rule="isj", n_null=nnc)
                call ka%fit(wide, rule="isj", n_null=nna)
            else
                call kc%fit(c, rule="isj", n_null=nnc)
                call ka%fit(wide, rule="isj", is_valid=mask, n_null=nna)
            end if
            call kc%pdf(KG_X, fc)
            call ka%pdf(KG_X, fa)
            call check(error, kc%bandwidth() == ka%bandwidth() .and. all(fc == fa) .and. nnc == nna .and. &
                nnc == count(.not. mask, kind=int64), trim(tag) // ": %fit over a column must answer what the array does")
            if (allocated(error)) return
            call gc%init(64, -600.0_real64, 600.0_real64, 40.0_real64)
            call ga%init(64, -600.0_real64, 600.0_real64, 40.0_real64)
            if (kind == 1) then
                call gc%add(c, n_null=nnc)
                call ga%add(wide, n_null=nna)
            else
                call gc%add(c, n_null=nnc)
                call ga%add(wide, is_valid=mask, n_null=nna)
            end if
            call gc%finish()
            call gc%density(dc)
            call ga%finish()
            call ga%density(da)
            call check(error, all(dc == da) .and. nnc == nna .and. gc%n_valid() == ga%n_valid(), &
                trim(tag) // ": %add over a column must accumulate what the array does")
            if (allocated(error)) return
        end do

    end subroutine test_column_forms

    !> Both rules, re-formed here from `pf_stddev` and `pf_iqr`'s own answers over the same
    !> population, on a weighted fixture whose reliability `n_eff` is well below `n` -- so a rule
    !> that used `n`, or `s` where the robust scale binds, is caught.
    subroutine test_rules_match_formulas(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde) :: k
        real(real64), allocatable :: x(:), w(:)
        real(real64) :: s, r, a, n_eff, want
        integer(int64) :: i

        ! A long tail on a tight core: the interquartile range, not the standard deviation, binds.
        call kde_fixture(64_int64, x)
        x = x/100.0_real64
        x(1:4) = [40.0_real64, -55.0_real64, 70.0_real64, 90.0_real64]
        allocate(w(64))
        do i = 1_int64, 64_int64
            w(i) = merge(8.0_real64, 1.0_real64, mod(i, 8_int64) == 0_int64)
        end do
        call pf_stddev(x, s, weights=w)
        call pf_iqr(x, r, weights=w)
        call check(error, r/1.349_real64 < s, "the fixture must make the robust scale bind")
        if (allocated(error)) return
        a = r/1.349_real64
        n_eff = sum(w)**2/sum(w*w)
        call check(error, n_eff < 0.5_real64*64.0_real64, "the fixture's n_eff must be well below n")
        if (allocated(error)) return

        call k%fit(x, rule="silverman", weights=w)
        want = 0.9_real64*a*n_eff**(-0.2_real64)
        call check(error, abs(k%bandwidth() - want) <= 1.0e-14_real64*want, &
            "silverman must be 0.9 * min(s, IQR/1.349) * n_eff**(-1/5)")
        if (allocated(error)) return
        call k%fit(x, rule="scott", weights=w)
        ! The fifth root written out, not a decimal read off a run: Scott's constant is
        ! `(4/3)**(1/5)`, and a test that quoted its digits would pin a rounding rather than a rule.
        want = (4.0_real64/3.0_real64)**0.2_real64*a*n_eff**(-0.2_real64)
        call check(error, abs(k%bandwidth() - want) <= 1.0e-14_real64*want, &
            "scott must be (4/3)**(1/5) * min(s, IQR/1.349) * n_eff**(-1/5)")
        if (allocated(error)) return
        ! And it is NOT the two-digit `1.06` the rule is often written with: the two differ by
        ! `7e-4` relative, far above the tolerance above, so this fails if the constant is rounded.
        call check(error, abs(k%bandwidth() - 1.06_real64*a*n_eff**(-0.2_real64)) > 1.0e-5_real64*want, &
            "scott must be the exact fifth root, not 1.06")
        if (allocated(error)) return

        ! Unweighted and near-uniform, the standard deviation binds instead.
        call kde_fixture(64_int64, x)
        call pf_stddev(x, s)
        call pf_iqr(x, r)
        call check(error, s < r/1.349_real64, "a near-uniform sample must make s bind")
        if (allocated(error)) return
        call k%fit(x, rule="silverman")
        want = 0.9_real64*s*64.0_real64**(-0.2_real64)
        call check(error, abs(k%bandwidth() - want) <= 1.0e-14_real64*want, &
            "silverman must use s where it is below IQR/1.349")
        if (allocated(error)) return

        ! More than half the sample tied: the interquartile range is zero, and the rule falls back
        ! to `s` rather than to a bandwidth of zero.
        x(1:40) = 1.0_real64
        call pf_stddev(x, s)
        call pf_iqr(x, r)
        call check(error, r == 0.0_real64 .and. s > 0.0_real64, "the fixture must have a zero IQR")
        if (allocated(error)) return
        call k%fit(x, rule="silverman")
        want = 0.9_real64*s*64.0_real64**(-0.2_real64)
        call check(error, abs(k%bandwidth() - want) <= 1.0e-14_real64*want, &
            "a zero IQR must fall back to s, not to a zero bandwidth")

    end subroutine test_rules_match_formulas

    !> `adjust=` scales a rule's bandwidth and an explicit one alike.
    subroutine test_adjust_multiplies(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde) :: k
        real(real64), allocatable :: x(:)
        real(real64) :: h1

        call kde_fixture(32_int64, x)
        call k%fit(x, rule="scott")
        h1 = k%bandwidth()
        call k%fit(x, rule="scott", adjust=0.25_real64)
        call check(error, k%bandwidth() == 0.25_real64*h1, "adjust must multiply a rule's bandwidth")
        if (allocated(error)) return
        call k%fit(x, bandwidth=3.0_real64, adjust=2.5_real64)
        call check(error, k%bandwidth() == 7.5_real64, "adjust must multiply an explicit bandwidth")

    end subroutine test_adjust_multiplies

    !> Unequal weights: reliability counts Kish's `sum(w)**2/sum(w**2)`, frequency counts `sum(w)`.
    !> On a near-uniform fixture `s` binds under both, so the two bandwidths differ by the counts'
    !> ratio and by the two standard deviations' own `ddof` charges, each re-formed here.
    subroutine test_n_eff_follows_weight_type(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde) :: rel, frq
        real(real64), allocatable :: x(:), w(:)
        real(real64) :: s_rel, s_frq, want_rel, want_frq

        call kde_fixture(40_int64, x)
        call kde_weights_mod5(40_int64, w)
        w = 3.0_real64*w
        call rel%fit(x, rule="silverman", weights=w)
        call frq%fit(x, rule="silverman", weights=w, weight_type="frequency")
        call pf_stddev(x, s_rel, weights=w)
        call pf_stddev(x, s_frq, weights=w, weight_type="frequency")
        want_rel = 0.9_real64*s_rel*(sum(w)**2/sum(w*w))**(-0.2_real64)
        want_frq = 0.9_real64*s_frq*sum(w)**(-0.2_real64)
        call check(error, abs(rel%bandwidth() - want_rel) <= 1.0e-14_real64*want_rel, &
            "reliability weights must use Kish's effective size")
        if (allocated(error)) return
        call check(error, abs(frq%bandwidth() - want_frq) <= 1.0e-14_real64*want_frq, &
            "frequency weights must use sum(w)")
        if (allocated(error)) return
        call check(error, abs(rel%bandwidth() - frq%bandwidth()) > 1.0e-3_real64*want_rel, &
            "the two weight types must give different bandwidths on unequal weights")

    end subroutine test_n_eff_follows_weight_type

    !> A frequency weight of three is three observations. At an explicit bandwidth the estimate
    !> over the weighted sample equals the estimate over the replicated one to rounding; under a
    !> rule the bandwidths agree too while the standard deviation binds, which it does here.
    !>
    !> Where the interquartile range binds instead they need not agree: `pf_iqr`'s frequency
    !> default is the inverted-CDF quartile, the family's own rule for frequency weights, while the
    !> replicated sample takes the interpolated one.
    subroutine test_frequency_weights_replicate(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde) :: wk, rk
        real(real64), allocatable :: x(:), w(:), rep(:)
        real(real64) :: fw(NKX), fr(NKX), cw(NKX), cr(NKX)
        integer(int64) :: i, j, m

        call kde_fixture(30_int64, x)
        allocate(w(30))
        do i = 1_int64, 30_int64
            w(i) = real(mod(i, 3_int64) + 1_int64, real64)
        end do
        allocate(rep(int(sum(w), int64)))
        m = 0_int64
        do i = 1_int64, 30_int64
            do j = 1_int64, int(w(i), int64)
                m = m + 1_int64
                rep(m) = x(i)
            end do
        end do
        call wk%fit(x, bandwidth=45.0_real64, weights=w, weight_type="frequency")
        call rk%fit(rep, bandwidth=45.0_real64)
        call wk%pdf(KG_X, fw)
        call rk%pdf(KG_X, fr)
        call wk%cdf(KG_X, cw)
        call rk%cdf(KG_X, cr)
        call check(error, all(abs(fw - fr) <= 1.0e-13_real64*maxval(fr)) .and. &
            all(abs(cw - cr) <= 1.0e-14_real64), &
            "a frequency-weighted fit must equal the fit over the replicated sample")
        if (allocated(error)) return
        call wk%fit(x, rule="silverman", weights=w, weight_type="frequency")
        call rk%fit(rep, rule="silverman")
        call check(error, abs(wk%bandwidth() - rk%bandwidth()) <= 1.0e-13_real64*rk%bandwidth(), &
            "frequency weights must give the replicated sample's bandwidth where s binds")

    end subroutine test_frequency_weights_replicate

    !> The family's exclusion ORDER: a null element with a NaN weight is null (its weight is never
    !> examined, so no abort), a NaN element with a zero weight is a NaN, and a zero weight on an
    !> ordinary element removes it without a count. The counts must equal `pf_count_valid`'s.
    subroutine test_population_rules(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde) :: k
        real(real64) :: x(7), w(7), nan
        logical :: valid(7), ok
        integer(int64) :: n_null, n_nan, n_out, want_n, want_null, want_nan

        nan = ieee_value(1.0_real64, ieee_quiet_nan)
        x = [1.0_real64, 2.0_real64, nan, 4.0_real64, 5.0_real64, 6.0_real64, 7.5_real64]
        w = [1.0_real64, nan, 0.0_real64, 2.0_real64, 0.0_real64, 1.0_real64, 3.0_real64]
        valid = [.true., .false., .true., .true., .true., .true., .true.]
        call k%fit(x, bandwidth=1.0_real64, is_valid=valid, weights=w, n_null=n_null, n_nan=n_nan, &
            n_outside=n_out, ok=ok)
        call pf_count_valid(x, want_n, is_valid=valid, weights=w, n_null=want_null, n_nan=want_nan)
        call check(error, ok .and. n_null == want_null .and. n_nan == want_nan .and. n_out == 0_int64 &
            .and. k%n_valid() == want_n .and. k%n() == 7_int64, &
            "the exclusion counts must be pf_count_valid's")
        if (allocated(error)) return
        call check(error, want_n == 4_int64 .and. k%sum_weights() == 7.0_real64, &
            "four elements must survive, carrying weight 1 + 2 + 1 + 3")

    end subroutine test_population_rules

    !> `skipnan=.false.` keeps a NaN, and a NaN in the population makes every answer NaN.
    subroutine test_skipnan_false_poisons(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde) :: k
        real(real64) :: x(4), f, c, q
        logical :: ok

        x = [1.0_real64, 2.0_real64, ieee_value(1.0_real64, ieee_quiet_nan), 4.0_real64]
        call k%fit(x, bandwidth=1.0_real64, ok=ok)
        call check(error, ok .and. k%n_nan() == 1_int64, "by default a NaN leaves the population")
        if (allocated(error)) return
        call k%fit(x, bandwidth=1.0_real64, skipnan=.false., ok=ok)
        call k%pdf(2.0_real64, f)
        call k%cdf(2.0_real64, c)
        call k%quantile(0.5_real64, q)
        call check(error, (.not. ok) .and. ieee_is_nan(f) .and. ieee_is_nan(c) .and. ieee_is_nan(q), &
            "skipnan=.false. with a NaN present must give ok=.false. and NaN answers")

    end subroutine test_skipnan_false_poisons

    !> Section 5.5's quiet returns: no abort from a data condition, a NaN from every query.
    subroutine test_quiet_returns(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde) :: k
        real(real64) :: f, c, empty(0), xg(5), fg(5), vs(3)
        logical :: ok

        call k%fit(empty, ok=ok)
        call k%pdf(0.0_real64, f)
        call k%curve(xg, fg)
        call k%sample(vs, 1_int64)
        call check(error, (.not. ok) .and. k%is_fitted() .and. ieee_is_nan(f) .and. &
            all(ieee_is_nan(fg)) .and. all(ieee_is_nan(xg)) .and. all(ieee_is_nan(vs)) .and. &
            k%n_valid() == 0_int64, "an empty sample must leave a fitted, undefined estimate")
        if (allocated(error)) return
        call k%fit([1.0_real64, 2.0_real64], is_valid=[.false., .false.], ok=ok)
        call check(error, .not. ok .and. k%n_null() == 2_int64, "an all-null sample is undefined")
        if (allocated(error)) return
        call k%fit([3.0_real64, 3.0_real64, 3.0_real64], rule="silverman", ok=ok)
        call k%cdf(3.0_real64, c)
        call check(error, (.not. ok) .and. ieee_is_nan(c) .and. ieee_is_nan(k%bandwidth()), &
            "a constant sample under a rule has no bandwidth and is undefined")
        if (allocated(error)) return
        call k%curve(xg, fg, xmin=0.0_real64, xmax=4.0_real64)
        call check(error, all(ieee_is_nan(fg)) .and. xg(1) == 0.0_real64 .and. xg(5) == 4.0_real64, &
            "an undefined estimate's curve keeps the points it was given and answers NaN")
        if (allocated(error)) return
        call k%fit([3.0_real64, 3.0_real64, 3.0_real64], kernel="gaussian", bandwidth=0.5_real64, ok=ok)
        call k%pdf(3.0_real64, f)
        call check(error, ok .and. abs(f - 2.0_real64*0.3989422804014327_real64/(1.0_real64 - &
            5.7330314375838782e-7_real64)) <= 1.0e-15_real64, &
            "a constant sample with an explicit bandwidth is one bump, phi(0)/h at its centre")

    end subroutine test_quiet_returns

    !> Every kernel, both corrections, one bound, the other and both, with the bandwidth larger
    !> than the range so that every point is corrected: the density integrates to one over the
    !> support, and `%cdf` runs from 0 to 1 across it.
    subroutine test_bounds_conserve_mass(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(kde_density) :: fn
        real(real64), allocatable :: x(:), cuts(:)
        real(real64) :: mass, c_lo, c_hi, a, b, h, lo, hi
        character(len=11), parameter :: METHODS(3) = [character(len=11) :: "renormalise", "reflect", "linear"]
        integer :: kk, mm, side
        character(len=120) :: what

        call kde_fixture(8_int64, x)
        x = x/500.0_real64
        lo = -1.0_real64
        hi = 1.0_real64
        do kk = 1, 4
            ! Wide enough that every kernel reaches past the far bound from anywhere inside.
            h = 3.0_real64/RADIUS(kk)*2.0_real64
            do mm = 1, 3
                do side = 1, 3
                    select case (side)
                    case (1)
                        call fn%k%fit(x, bandwidth=h, kernel=trim(KERNELS(kk)), lower=lo, &
                            boundary=trim(METHODS(mm)))
                        a = lo
                        b = maxval(x) + RADIUS(kk)*h
                    case (2)
                        call fn%k%fit(x, bandwidth=h, kernel=trim(KERNELS(kk)), upper=hi, &
                            boundary=trim(METHODS(mm)))
                        a = minval(x) - RADIUS(kk)*h
                        b = hi
                    case default
                        call fn%k%fit(x, bandwidth=h, kernel=trim(KERNELS(kk)), lower=lo, upper=hi, &
                            boundary=trim(METHODS(mm)))
                        a = lo
                        b = hi
                    end select
                    write(what, '(a,1x,a,a,i0)') trim(KERNELS(kk)), trim(METHODS(mm)), " side ", side
                    call kinks(fn%k, x, h*RADIUS(kk), a, b, side, mm == 3, cuts)
                    fn%power = 0
                    if (size(cuts) > 0) then
                        mass = pf_integrate(fn, a, b, 1.0e-12_real64, breakpoints=cuts)
                    else
                        mass = pf_integrate(fn, a, b, 1.0e-12_real64)
                    end if
                    write(what, '(a,1x,a,a,i0,a,es22.15)') trim(KERNELS(kk)), trim(METHODS(mm)), " side ", &
                        side, ": the density's integral over the support is ", mass
                    call check(error, abs(mass - 1.0_real64) <= 1.0e-10_real64, trim(what))
                    if (allocated(error)) return
                    call fn%k%cdf(a + 1.0e-12_real64, c_lo)
                    call fn%k%cdf(b - 1.0e-12_real64, c_hi)
                    call check(error, c_lo <= 1.0e-10_real64 .and. c_hi >= 1.0_real64 - 1.0e-10_real64, &
                        trim(what) // ": %cdf must run from 0 to 1 across the support")
                    if (allocated(error)) return
                end do
            end do
        end do

    contains

        !> Every point inside `(a, b)` where some image's kernel starts or stops: the integrand's
        !> jumps (the box) and kinks, which `pf_integrate` is told about rather than left to find.
        subroutine kinks(k, xs, reach, a, b, side, linear, cuts)
            type(pf_kde), intent(in)               :: k       !! the estimate (unused; its settings)
            real(real64), intent(in)               :: xs(:)   !! the sample
            real(real64), intent(in)               :: reach   !! the support radius times `h`
            real(real64), intent(in)               :: a       !! the range's lower end
            real(real64), intent(in)               :: b       !! the range's upper end
            integer, intent(in)                    :: side    !! which bounds are set
            logical, intent(in)                    :: linear  !! the linear correction, whose edges differ
            real(real64), allocatable, intent(out) :: cuts(:) !! sorted, distinct, inside
            real(real64), allocatable :: c(:)
            real(real64) :: v, t
            integer :: i, j, n

            if (.not. k%is_fitted()) return
            allocate(c(0))
            ! Under `"linear"` there are no images, and the correction's own edges -- where a point
            ! stops being corrected -- are where the density kinks instead.
            if (linear) then
                if (side /= 2) c = [c, lo + reach]
                if (side /= 1) c = [c, hi - reach]
            end if
            do i = 1, size(xs)
                do j = 1, 3
                    select case (j)
                    case (1)
                        v = xs(i)
                    case (2)
                        v = 2.0_real64*lo - xs(i)
                    case default
                        v = 2.0_real64*hi - xs(i)
                    end select
                    if (j == 2 .and. (side == 2 .or. linear)) cycle
                    if (j == 3 .and. (side == 1 .or. linear)) cycle
                    ! The support's ends, and the B-spline's inner knots halfway out.
                    c = [c, v - reach, v + reach, v - 0.5_real64*reach, v + 0.5_real64*reach]
                end do
            end do
            ! Sort, and keep the distinct values strictly inside.
            n = size(c)
            do i = 2, n
                t = c(i)
                j = i - 1
                do while (j >= 1)
                    if (c(j) <= t) exit
                    c(j + 1) = c(j)
                    j = j - 1
                end do
                c(j + 1) = t
            end do
            allocate(cuts(0))
            do i = 1, n
                if (.not. (c(i) > a + 1.0e-9_real64 .and. c(i) < b - 1.0e-9_real64)) cycle
                if (size(cuts) > 0) then
                    if (c(i) - cuts(size(cuts)) <= 1.0e-9_real64) cycle
                end if
                cuts = [cuts, c(i)]
            end do

        end subroutine kinks

    end subroutine test_bounds_conserve_mass

    !> A point outside the support is excluded and counted, and the density there is zero.
    subroutine test_outside_excluded(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde) :: k
        real(real64) :: x(6), f, c
        integer(int64) :: n_out

        x = [-1.0_real64, 0.5_real64, 1.0_real64, 2.0_real64, 3.5_real64, &
            ieee_value(1.0_real64, ieee_quiet_nan)]
        x(6) = ieee_value(1.0_real64, ieee_positive_inf)
        call k%fit(x, bandwidth=0.5_real64, lower=0.0_real64, upper=3.0_real64, n_outside=n_out)
        call check(error, n_out == 3_int64 .and. k%n_valid() == 3_int64 .and. k%n_outside() == 3_int64, &
            "-1, 3.5 and +Infinity must be outside [0, 3]")
        if (allocated(error)) return
        call k%pdf(-0.1_real64, f)
        call k%cdf(-0.1_real64, c)
        call check(error, f == 0.0_real64 .and. c == 0.0_real64, "below the support: density 0, CDF 0")
        if (allocated(error)) return
        call k%pdf(3.1_real64, f)
        call k%cdf(3.1_real64, c)
        call check(error, f == 0.0_real64 .and. c == 1.0_real64, "above the support: density 0, CDF 1")
        if (allocated(error)) return
        call k%fit(x(1:5), bandwidth=0.5_real64, n_outside=n_out)
        call check(error, n_out == 0_int64, "with no bound, no finite point is outside")
        if (allocated(error)) return
        call k%fit(x, bandwidth=0.5_real64, n_outside=n_out)
        call check(error, n_out == 1_int64, "an infinite point is outside every support")

    end subroutine test_outside_excluded

    !> `%cdf(%quantile(p)) == p` at seven probabilities, unbounded and bounded, for every kernel;
    !> the two ends of the support at `p = 0` and `p = 1`. The adaptive arm: the ends are each
    !> point's own reach, the smallest `x_j - R h_j` and the largest `x_j + R h_j`, not the widest
    !> kernel's reach from the extreme points.
    subroutine test_quantile_inverts_cdf(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde) :: k
        real(real64), allocatable :: x(:), hb(:), xs(:)
        real(real64), parameter :: PS(7) = [0.001_real64, 0.05_real64, 0.25_real64, 0.5_real64, &
            0.75_real64, 0.95_real64, 0.999_real64]
        real(real64) :: q(7), c(7), q0, q1, lo_end, hi_end, c0, c1
        character(len=120) :: what
        integer :: kk, bounded, jmin

        call kde_fixture(50_int64, x)
        do kk = 1, 4
            do bounded = 0, 1
                if (bounded == 0) then
                    call k%fit(x, bandwidth=40.0_real64, kernel=trim(KERNELS(kk)))
                else
                    call k%fit(x, bandwidth=40.0_real64, kernel=trim(KERNELS(kk)), lower=-480.0_real64, &
                        upper=490.0_real64, boundary=merge("reflect    ", "renormalise", kk > 2))
                end if
                call k%quantile(PS, q)
                call k%cdf(q, c)
                call check(error, all(abs(c - PS) <= 1.0e-12_real64), &
                    trim(KERNELS(kk)) // ": %cdf(%quantile(p)) must be p")
                if (allocated(error)) return
                call k%quantile(0.0_real64, q0)
                call k%quantile(1.0_real64, q1)
                if (bounded == 0) then
                    ! A few ulp rather than `==`, because these two ends are RE-DERIVED here and
                    ! the library computes its own from `KDE_RADIUS*hmax`: two independent
                    ! computations of one real quantity agree bit for bit only where nothing can
                    ! contract one of them differently, and `40*RADIUS` fuses into the subtraction
                    ! on a target whose baseline has an FMA while the library's two-step form does
                    ! not (`fortran-gotchas.md`, "One target contracts to an FMA and another cannot").
                    ! The BOUNDED arm below stays exact: those ends are the caller's own literals,
                    ! which the estimate stores and hands back unarithmetised.
                    lo_end = minval(x) - 40.0_real64*RADIUS(kk)
                    hi_end = maxval(x) + 40.0_real64*RADIUS(kk)
                    call check(error, abs(q0 - lo_end) <= 4.0_real64*spacing(abs(lo_end)) .and. &
                        abs(q1 - hi_end) <= 4.0_real64*spacing(abs(hi_end)), &
                        trim(KERNELS(kk)) // ": p = 0 and 1 must be the ends of the estimate's support")
                else
                    call check(error, q0 == -480.0_real64 .and. q1 == 490.0_real64, &
                        trim(KERNELS(kk)) // ": p = 0 and 1 must be the bounds")
                end if
                if (allocated(error)) return
            end do
        end do

        ! The adaptive arm. The two-component recipe's first point sits in the narrow cluster, whose
        ! kernels are the narrowest, so the widest kernel's reach from it, `x(1) - R hmax`, lies well
        ! below where the density starts; the precondition says so, and a fixture that stopped
        ! discriminating fails it rather than passing. The ends are re-derived from `%bandwidths`,
        ! to a few ulp for the reason the unbounded arm above gives.
        call kde_two_component(60_int64, x)
        call k%fit(x, kernel="gaussian", rule="silverman", adaptive=.true.)
        allocate(hb(60), xs(60))
        call k%bandwidths(hb, xs)
        call check(error, hb(1) < maxval(hb), &
            "precondition: the first point's kernel must not be the widest, or the arm cannot tell F3's ends")
        if (allocated(error)) return
        jmin = minloc(xs - RADIUS(1)*hb, dim=1)
        lo_end = xs(jmin) - RADIUS(1)*hb(jmin)
        hi_end = maxval(xs + RADIUS(1)*hb)
        call k%quantile(0.0_real64, q0)
        call k%quantile(1.0_real64, q1)
        call check(error, abs(q0 - lo_end) <= 4.0_real64*spacing(abs(lo_end)) .and. &
            abs(q1 - hi_end) <= 4.0_real64*spacing(abs(hi_end)), &
            "adaptive: p = 0 and 1 must be the smallest x_j - R h_j and the largest x_j + R h_j")
        if (allocated(error)) return
        call check(error, q0 > xs(1) - RADIUS(1)*maxval(hb), &
            "adaptive: p = 0 must lie above the widest kernel's reach from the first point")
        if (allocated(error)) return
        ! Where the density starts: `%cdf` is zero there, to the rounding of one kernel's offset, and
        ! positive one bandwidth of that kernel inside it.
        call k%cdf(q0, c0)
        call k%cdf(q0 + hb(jmin), c1)
        call check(error, c0 <= 1.0e-15_real64 .and. c1 > 0.0_real64, &
            "adaptive: %cdf must be zero at p = 0's answer and positive one bandwidth inside it")
        if (allocated(error)) return

        ! Under `"linear"` on the rising fixture the clip removes a stretch that starts at the
        ! bound, so `%quantile(0)` is not the bound but the crossing where that stretch ends: the
        ! density is zero below it and positive above it, and `%cdf` is zero at it.
        deallocate(x)
        allocate(x(200))
        call kde_rising(200, x)
        call k%fit(x, rule="silverman", lower=0.0_real64, boundary="linear")
        call k%quantile(0.0_real64, q0)
        write(what, '(a,es22.15)') "linear: p = 0 must answer a crossing above the bound, not ", q0
        call check(error, q0 > 0.0_real64, trim(what))
        if (allocated(error)) return
        ! The clip removes everything below that crossing and nothing above it.
        call k%pdf(q0 - 0.05_real64*k%bandwidth(), c0)
        call k%pdf(q0 + 0.05_real64*k%bandwidth(), c1)
        write(what, '(a,es12.5,a,es12.5)') "linear: %pdf below the crossing is ", c0, " and above it ", c1
        call check(error, c0 == 0.0_real64 .and. c1 > 0.0_real64, trim(what))
        if (allocated(error)) return
        call k%cdf(q0, c0)
        call k%quantile(1.0e-6_real64, q1)
        write(what, '(a,es12.5,a,es22.15)') "linear: %cdf at the crossing is ", c0, " and the 1e-6 quantile ", q1
        call check(error, c0 == 0.0_real64 .and. q1 > q0, trim(what))
        if (allocated(error)) return

        ! The mirror at the UPPER bound, on the falling fixture: the clip removes a stretch that
        ! ends at the bound, so `%quantile(1)` is the crossing where that stretch starts and not
        ! `upper` itself, where the density is exactly zero. Written out rather than folded into the
        ! arm above, because the asymmetry between the two is what a scan carrying a flag across a
        ! stretch it opens gets wrong.
        call kde_falling(200, x)
        call k%fit(x, rule="silverman", upper=1.0_real64, boundary="linear")
        call k%quantile(1.0_real64, q1)
        write(what, '(a,es22.15)') "linear: p = 1 must answer a crossing below the bound, not ", q1
        call check(error, q1 < 1.0_real64, trim(what))
        if (allocated(error)) return
        call k%pdf(q1 - 0.05_real64*k%bandwidth(), c0)
        call k%pdf(q1 + 0.05_real64*k%bandwidth(), c1)
        write(what, '(a,es12.5,a,es12.5)') "linear: %pdf below p = 1's answer is ", c0, " and above it ", c1
        call check(error, c0 > 0.0_real64 .and. c1 == 0.0_real64, trim(what))
        if (allocated(error)) return
        call k%cdf(q1, c0)
        call k%quantile(1.0_real64 - 1.0e-6_real64, q0)
        write(what, '(a,es12.5,a,es22.15)') "linear: %cdf at the crossing is ", c0, " and the 1-1e-6 quantile ", q0
        call check(error, c0 == 1.0_real64 .and. q0 < q1, trim(what))

    end subroutine test_quantile_inverts_cdf

    !> The default range is `cut` bandwidths beyond the data, clipped to the support, with the
    !> points equally spaced and both ends exact.
    subroutine test_curve_default_range(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde) :: k
        real(real64), allocatable :: x(:)
        real(real64) :: xg(11), fg(11), f
        real(real64) :: h

        call kde_fixture(32_int64, x)
        call k%fit(x, rule="scott")
        h = k%bandwidth()
        call k%curve(xg, fg)
        call check(error, xg(1) == minval(x) - 3.0_real64*h .and. xg(11) == maxval(x) + 3.0_real64*h, &
            "the default range must be min - 3h to max + 3h")
        if (allocated(error)) return
        call k%pdf(xg(6), f)
        call check(error, fg(6) == f, "the curve's values must be %pdf's")
        if (allocated(error)) return
        call k%curve(xg, fg, cut=1.0_real64)
        call check(error, xg(1) == minval(x) - h .and. xg(11) == maxval(x) + h, "cut= must set the reach")
        if (allocated(error)) return
        call k%fit(x, rule="scott", lower=-470.0_real64, upper=460.0_real64)
        call k%curve(xg, fg)
        call check(error, xg(1) == -470.0_real64 .and. xg(11) == 460.0_real64, &
            "the default range must be clipped to the support")
        if (allocated(error)) return
        call k%curve(xg, fg, xmin=-100.0_real64)
        call check(error, xg(1) == -100.0_real64 .and. xg(11) == 460.0_real64 .and. &
            abs(xg(2) - (-100.0_real64 + 56.0_real64)) <= 1.0e-12_real64, &
            "xmin= must replace one end only, and the points must be equally spaced")

    end subroutine test_curve_default_range

    !> Every correction answers the true density at a bound the density is FLAT at, which is the
    !> case each of them is supposed to be exact for.
    !>
    !> The sample is the uniform density's own quantiles on `[0, 1]`, so the true density is exactly
    !> 1 everywhere and exactly flat at both bounds, and the estimate's own discretisation error is
    !> below a part in a million -- nothing here is a Monte-Carlo figure. A correction that
    !> normalises each DATA POINT's kernel by the mass that point keeps inside the support, rather
    !> than the kernel AT THE QUERY POINT, answers `ln 2 = 0.693` times the truth at the bound, for
    !> every kernel and however small the bandwidth.
    subroutine test_corrections_unbiased_at_a_bound(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde) :: k
        real(real64) :: x(2000), f0, f1, fm
        character(len=11), parameter :: WHICH(3) = [character(len=11) :: "renormalise", "reflect", "linear"]
        character(len=140) :: what
        integer :: i, c

        do i = 1, 2000
            x(i) = (real(i, real64) - 0.5_real64)/2000.0_real64
        end do
        do c = 1, 3
            call k%fit(x, bandwidth=0.05_real64, lower=0.0_real64, upper=1.0_real64, &
                boundary=trim(WHICH(c)))
            call k%pdf(0.0_real64, f0)
            call k%pdf(1.0_real64, f1)
            call k%pdf(0.5_real64, fm)
            write(what, '(4a,es12.5,a,es12.5,a,es12.5)') trim(WHICH(c)), ": the density of a uniform sample ", &
                "is 1 at both bounds and between them; got ", "", f0, ", ", f1, ", ", fm
            call check(error, abs(f0 - 1.0_real64) <= 0.02_real64 .and. abs(f1 - 1.0_real64) <= 0.02_real64 &
                .and. abs(fm - 1.0_real64) <= 0.02_real64, trim(what))
            if (allocated(error)) return
        end do

    end subroutine test_corrections_unbiased_at_a_bound

    !> A bound given without `boundary=` resolves to `"reflect"`: the fit is that fit, bit for bit.
    subroutine test_default_boundary_is_reflect(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde) :: a, b
        real(real64), allocatable :: x(:)
        real(real64) :: fa(NKX), fb(NKX), ca(NKX), cb(NKX)
        character(len=:), allocatable :: token

        call kde_fixture(64_int64, x)
        call a%fit(x, bandwidth=60.0_real64, lower=-470.0_real64, upper=460.0_real64)
        call b%fit(x, bandwidth=60.0_real64, lower=-470.0_real64, upper=460.0_real64, boundary="reflect")
        call a%pdf(KG_X, fa)
        call b%pdf(KG_X, fb)
        call a%cdf(KG_X, ca)
        call b%cdf(KG_X, cb)
        call check(error, all(fa == fb) .and. all(ca == cb), &
            "a bound without boundary= must give the reflected fit, bit for bit")
        if (allocated(error)) return
        ! And `%print` names it, which is where a caller reads what they got.
        call a%fit(x, bandwidth=60.0_real64, lower=-470.0_real64)
        call a%kernel(token)
        call check(error, token == "bspline", "the kernel default must not have moved")

    end subroutine test_default_boundary_is_reflect

    !> Whether this build flushes subnormals to zero, so nothing here can be asked about one.
    !!
    !! **ifx turns flush-to-zero and denormals-are-zero on by default at `-O1` and above**, which
    !! is what a flagless `fpm test` selects; gfortran, flang and nagfor leave gradual underflow in
    !! force. It is a process-wide MXCSR setting established by the main program, so a library
    !! procedure sees a subnormal argument already collapsed to zero and cannot recover it -- and
    !! building the value from its bit pattern rather than by arithmetic does not help, since
    !! denormals-are-zero collapses it again on the comparison that reads it.
    function subnormals_are_flushed() result(res)
        logical :: res !! `.true.` when underflow is abrupt, so subnormal inputs read as zero.
        logical :: gradual

        res = .false.
        if (.not. ieee_support_underflow_control(1.0_real64)) return
        call ieee_get_underflow_mode(gradual)
        res = .not. gradual
    end function subnormals_are_flushed

    !> One admission rule for the bandwidth, applied by both forms: it must be positive, NORMAL and
    !> reach a finite distance. A bandwidth failing it leaves `pf_kde` undefined -- `ok = .false.`
    !> and every query a quiet NaN -- rather than answering a density that is wrong.
    !>
    !> The two cases are the ones that answered silently before: a SUBNORMAL bandwidth, whose
    !> kernel is narrower than the gap between two neighbouring numbers, so that `%pdf` is
    !> identically zero while `%cdf` still steps from 0 to 1; and one far wider than a two-sided
    !> support, where the mass a kernel keeps inside the support underflows and the division by it
    !> gives `+Infinity` and `NaN`. `pf_kde_grid%init` refuses both, out of process
    !> (`kde_grid_bandwidth_subnormal`, `kde_grid_bandwidth_unusable`).
    !>
    !> **The subnormal half is dropped where the build flushes subnormals to zero** (ifx at `-O1`
    !> and above), because the library then receives a zero rather than a subnormal and there is
    !> nothing left to assert about; the wider-than-support half runs everywhere.
    subroutine test_bandwidth_admission(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde) :: k
        real(real64), allocatable :: x(:)
        real(real64) :: f, c, sub
        logical :: ok
        character(len=140) :: what
        logical :: uf_ok, uf_was

        ! Extreme but legal inputs underflow inside the library; the flag is put back rather than
        ! left for nagfor to report at exit, unattributed (`.claude/rules/fortran-gotchas.md`).
        uf_ok = ieee_support_flag(ieee_underflow, 0.0_real64)
        if (uf_ok) call ieee_get_flag(ieee_underflow, uf_was)

        run: block
            call bounded_fixture(200_int64, x)
            ! The control: an ordinary bandwidth on the same sample is defined, so that neither
            ! assertion below can pass because the fixture itself is unusable.
            call k%fit(x, bandwidth=0.05_real64, lower=0.0_real64, upper=1.0_real64, ok=ok)
            call check(error, ok, "control: an ordinary bandwidth on this sample must be defined")
            if (allocated(error)) exit run

            ! Subnormal: built by halving `tiny`, never written as a literal. Dropped where the
            ! build collapses subnormals to zero, since the library would then be answering about
            ! a ZERO bandwidth -- a different rule, already covered by `kde_grid_bandwidth` -- and
            ! the assertion would report the build rather than the kernel. The loss is reported by
            ! `test_kde_grid_bandwidth_subnormal_aborts` (`test_errors.f90`) skipping outright.
            if (.not. subnormals_are_flushed()) then
                sub = 0.5_real64*tiny(1.0_real64)
                call check(error, sub > 0.0_real64 .and. sub < tiny(1.0_real64), &
                    "precondition: the fixture's bandwidth must be positive and subnormal")
                if (allocated(error)) exit run
                call k%fit(x, bandwidth=sub, ok=ok)
                call k%pdf(0.5_real64, f)
                call k%cdf(0.5_real64, c)
                write(what, '(a,es12.5,a,es12.5)') "a subnormal bandwidth must leave the estimate " // &
                    "undefined; %pdf is ", f, " and %cdf ", c
                call check(error, .not. ok .and. ieee_is_nan(f) .and. ieee_is_nan(c), trim(what))
                if (allocated(error)) exit run
            end if

            ! Far wider than the support it is bounded by, at both bounds.
            call k%fit(x, bandwidth=1.0e300_real64, lower=0.0_real64, upper=1.0_real64, &
                boundary="reflect", ok=ok)
            call k%pdf(0.5_real64, f)
            call k%cdf(0.5_real64, c)
            write(what, '(a,es12.5,a,es12.5)') "a bandwidth far wider than a two-sided support must " // &
                "leave the estimate undefined; %pdf is ", f, " and %cdf ", c
            call check(error, .not. ok .and. ieee_is_nan(f) .and. ieee_is_nan(c), trim(what))

        end block run

        if (uf_ok) call ieee_set_flag(ieee_underflow, uf_was)
    end subroutine test_bandwidth_admission

    !> The grid's lifecycle, in process: `%finish` closes the accumulation, `%is_finished` reports
    !> both states, a second `%finish` is a no-op, `%clear` reopens the grid empty, and
    !> `finish=.true.` on `%add` and on `%merge` is exactly that call followed by `%finish`.
    !>
    !> The aborts either side of the seam -- a query before `%finish`, an `%add` or a `%merge`
    !> after it -- are out of process, in `test_errors.f90`.
    subroutine test_grid_lifecycle(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde_grid) :: g, h, p1, p2
        real(real64), allocatable :: x(:)
        real(real64) :: fg(24), fh(24)

        call bounded_fixture(200_int64, x)
        call g%init(24, 0.0_real64, 1.0_real64, 0.05_real64)
        call check(error, .not. g%is_finished(), "a grid is not finished when %init returns")
        if (allocated(error)) return
        call g%add(x)
        call check(error, .not. g%is_finished(), "%add must not finish a grid on its own")
        if (allocated(error)) return
        call g%finish()
        call check(error, g%is_finished(), "%finish must finish it")
        if (allocated(error)) return
        ! A second `%finish` is a no-op, so a caller who cannot tell whether a helper already
        ! closed the grid may call it again.
        call g%finish()
        call g%density(fg)
        call check(error, g%is_finished() .and. any(fg > 0.0_real64), &
            "a second %finish must be a no-op, not a reset")
        if (allocated(error)) return

        ! `finish=.true.` on `%add` is that call followed by `%finish`, bit for bit.
        call h%init(24, 0.0_real64, 1.0_real64, 0.05_real64)
        call h%add(x, finish=.true.)
        call h%density(fh)
        call check(error, h%is_finished() .and. all(fh == fg), &
            "add(finish=.true.) must equal %add then %finish, bit for bit")
        if (allocated(error)) return

        ! And on `%merge`, over two halves.
        call p1%init(24, 0.0_real64, 1.0_real64, 0.05_real64)
        call p1%add(x(1:100))
        call p2%init(24, 0.0_real64, 1.0_real64, 0.05_real64)
        call p2%add(x(101:200))
        call p1%merge(p2, finish=.true.)
        call p1%density(fh)
        call check(error, p1%is_finished() .and. maxval(abs(fh - fg)) <= 1.0e-14_real64*maxval(fg), &
            "merge(finish=.true.) must finish the grid, and the halves must sum to the whole")
        if (allocated(error)) return

        ! `%clear` reopens it, empty: it accumulates again and answers as a fresh grid.
        call g%clear()
        call check(error, .not. g%is_finished() .and. g%n() == 0_int64, &
            "%clear must reopen the grid and empty it")
        if (allocated(error)) return
        call g%add(x, finish=.true.)
        call g%density(fg)
        call check(error, any(fg > 0.0_real64), "the reopened grid must accumulate and answer again")
        if (allocated(error)) return
        call h%clear()
        call h%add(x, finish=.true.)
        call h%density(fh)
        call check(error, all(fg == fh), "a cleared and refilled grid must equal a fresh one, bit for bit")

    end subroutine test_grid_lifecycle

    !> The mixture's density at `x`.
    pure function mix_pdf(x) result(f)
        real(real64), intent(in) :: x !! where to evaluate
        real(real64)             :: f !! the density there

        f = MIX_W*pf_norm_pdf(x) + (1.0_real64 - MIX_W)*pf_norm_pdf((x - MIX_MU)/MIX_SD)/MIX_SD

    end function mix_pdf

    !> The squared error of the fitted estimate against the mixture at `x`.
    function kde_mix_error_eval(this, x) result(f)
        class(kde_mix_error), intent(inout) :: this !! the estimate as a function
        real(real64), intent(in)            :: x    !! where to evaluate
        real(real64)                        :: f    !! the squared error there

        real(real64) :: g

        call this%k%pdf(x, g)
        f = (g - mix_pdf(x))**2

    end function kde_mix_error_eval

    !> `n` quantiles of the mixture, by bisection on its distribution function: a deterministic
    !> sample with no Monte-Carlo noise in it, so the error below is the smoothing's alone.
    subroutine mix_fixture(n, x)
        integer, intent(in)                    :: n    !! how many points
        real(real64), allocatable, intent(out) :: x(:) !! the quantiles, ascending

        real(real64) :: u, a, b, m, c
        integer :: i, it

        allocate(x(n))
        do i = 1, n
            u = (real(i, real64) - 0.5_real64)/real(n, real64)
            a = -8.0_real64
            b = 8.0_real64
            do it = 1, 200
                m = 0.5_real64*(a + b)
                c = MIX_W*pf_norm_cdf(m) + (1.0_real64 - MIX_W)*pf_norm_cdf((m - MIX_MU)/MIX_SD)
                if (c < u) then
                    a = m
                else
                    b = m
                end if
            end do
            x(i) = 0.5_real64*(a + b)
        end do

    end subroutine mix_fixture

    !> The ISJ rule's integrated squared error FALLS as the sample grows, on an ordinary
    !> two-component mixture.
    !>
    !> A guard against regression rather than a defect caught: the rule is near-optimal on this
    !> shape today. What it would catch is the rule losing its footing the way it does on the claw
    !> density -- several narrow components at a regular spacing -- where the bandwidth walks away
    !> from the optimum and the error RISES with the sample, from `1.7e-2` at n = 500 to `2.2e-2` at
    !> n = 128 000. The fixture here is deliberately not of that shape: one broad component and one
    !> narrow one at an irregular offset.
    subroutine test_isj_error_falls_with_n(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(kde_mix_error) :: fn
        real(real64), allocatable :: x(:)
        real(real64) :: ise(2), h(2)
        integer, parameter :: SIZES(2) = [600, 4800]
        character(len=160) :: what
        logical :: ok
        integer :: i

        do i = 1, 2
            call mix_fixture(SIZES(i), x)
            call fn%k%fit(x, rule="isj", ok=ok)
            call check(error, ok, "the isj rule must find a bandwidth for this mixture")
            if (allocated(error)) return
            h(i) = fn%k%bandwidth()
            ! Over the whole range the mixture has mass in; the estimate is zero beyond its own
            ! reach, so the error there is the mixture's own tail and is common to both sizes.
            ise(i) = pf_integrate(fn, -8.0_real64, 8.0_real64, 1.0e-10_real64)
            deallocate(x)
        end do
        write(what, '(a,es11.4,a,es11.4,a,es11.4,a,es11.4)') "the isj rule's ISE must fall with n: ", &
            ise(1), " at 600 and ", ise(2), " at 4800, with h ", h(1), " and ", h(2)
        call check(error, ise(2) < 0.6_real64*ise(1), trim(what))
        if (allocated(error)) return
        ! And the bandwidth narrows with the sample, which is what an ISE that falls rests on.
        call check(error, h(2) < h(1), "the isj bandwidth must narrow as the sample grows")

    end subroutine test_isj_error_falls_with_n

    !> A `real32` sample is widened: the fit equals the fit over its `real64` copy exactly.
    subroutine test_real32_widens(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde) :: a, b
        real(real64), allocatable :: x(:)
        real(real32), allocatable :: x32(:)
        real(real64) :: fa(NKX), fb(NKX)

        call kde_fixture(40_int64, x)
        x32 = real(x, real32)
        call a%fit(x32, rule="silverman")
        call b%fit(real(x32, real64), rule="silverman")
        call a%pdf(KG_X, fa)
        call b%pdf(KG_X, fb)
        call check(error, a%bandwidth() == b%bandwidth() .and. all(fa == fb), &
            "a real32 sample must be widened before anything else happens to it")

    end subroutine test_real32_widens

    !> The rank-1 forms answer exactly what the scalar forms answer, element by element.
    subroutine test_array_forms_match_scalar(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde) :: k
        real(real64), allocatable :: x(:)
        real(real64) :: f(NKX), c(NKX), q(3), v
        real(real64), parameter :: PS(3) = [0.1_real64, 0.5_real64, 0.9_real64]
        integer :: i, arm

        call kde_fixture(32_int64, x)
        do arm = 1, 2
        if (arm == 1) then
            call k%fit(x, bandwidth=50.0_real64, kernel="bspline", lower=-470.0_real64, boundary="reflect")
        else
            ! Under `"linear"` each element of a bulk `%cdf` or `%quantile` integrates its own
            ! window, so the two forms agreeing to the bit is what says an element is answered alone.
            call k%fit(x, bandwidth=50.0_real64, kernel="bspline", lower=-470.0_real64, boundary="linear")
        end if
        call k%pdf(KG_X, f)
        call k%cdf(KG_X, c)
        call k%quantile(PS, q)
        do i = 1, NKX
            call k%pdf(KG_X(i), v)
            call check(error, v == f(i), "%pdf's array form must equal its scalar form")
            if (allocated(error)) return
            call k%cdf(KG_X(i), v)
            call check(error, v == c(i), "%cdf's array form must equal its scalar form")
            if (allocated(error)) return
        end do
        do i = 1, 3
            call k%quantile(PS(i), v)
            call check(error, v == q(i), "%quantile's array form must equal its scalar form")
            if (allocated(error)) return
        end do
        end do

    end subroutine test_array_forms_match_scalar

    !> The tokens come back as given (matched without regard to case), and the bounds as set.
    subroutine test_tokens_and_accessors(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde) :: k
        real(real64), allocatable :: x(:)
        character(len=:), allocatable :: kname, rname
        real(real64) :: lo, hi

        call kde_fixture(20_int64, x)
        call k%fit(x, rule="  Scott", kernel="EPANECHNIKOV")
        call k%kernel(kname)
        call k%rule(rname)
        call k%bounds(lo, hi)
        call check(error, kname == "epanechnikov" .and. rname == "scott" .and. &
            (.not. ieee_is_finite(lo)) .and. lo < 0.0_real64 .and. (.not. ieee_is_finite(hi)) &
            .and. hi > 0.0_real64 .and. .not. k%is_adaptive(), &
            "tokens must fold case and an unbounded support must read -Infinity to +Infinity")
        if (allocated(error)) return
        call k%fit(x, bandwidth=2.0_real64, kernel="box", upper=500.0_real64)
        call k%kernel(kname)
        call k%rule(rname)
        call k%bounds(lo, hi)
        call check(error, kname == "box" .and. rname == "explicit" .and. hi == 500.0_real64 .and. &
            .not. ieee_is_finite(lo), 'an explicit bandwidth''s rule is "explicit"')

    end subroutine test_tokens_and_accessors

    !> A fit given no `kernel=` takes the cubic B-spline, on both types.
    !!
    !! **Asserted computationally, not by name.** `%kernel` reporting `"bspline"` while the
    !! estimate was summed with another kernel is exactly the defect a name assertion cannot see,
    !! so the default fit is compared against one that NAMES the B-spline and must agree bit for
    !! bit -- the same arithmetic on the same points, so anything but equality is a different
    !! kernel. The Gaussian arm is the negative control: without it the equality would also hold
    !! of a default that ignored `kernel=` altogether.
    !!
    !! `pf_kde_grid` is checked too, because it carries its own default and a fit and a grid that
    !! disagreed about it would make `pilot=` mean two things.
    subroutine test_default_kernel_is_the_bspline(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde) :: kd, kb, kg
        type(pf_kde_grid) :: gd, gb
        real(real64), allocatable :: x(:)
        real(real64) :: t(40), fd(40), fb(40), fgauss(40), cd(40), cb(40)
        character(len=:), allocatable :: kname
        integer :: i

        call kde_fixture(200_int64, x)
        call spread_points(-300.0_real64, 300.0_real64, t)

        call kd%fit(x, bandwidth=60.0_real64)
        call kd%kernel(kname)
        call check(error, kname == "bspline", &
            "a fit given no kernel= must report the B-spline as the kernel in force")
        if (allocated(error)) return

        call kb%fit(x, bandwidth=60.0_real64, kernel="bspline")
        call kd%pdf(t, fd)
        call kb%pdf(t, fb)
        call kd%cdf(t, cd)
        call kb%cdf(t, cb)
        call check(error, all(fd == fb) .and. all(cd == cb), &
            "the default fit must be the same estimate as one naming the B-spline, bit for bit")
        if (allocated(error)) return

        ! ---- the negative control: a named kernel must still change the answer ----
        call kg%fit(x, bandwidth=60.0_real64, kernel="gaussian")
        call kg%pdf(t, fgauss)
        call check(error, maxval(abs(fd - fgauss)) > 0.0_real64, &
            "a named kernel must change the estimate, or the default is not being read at all")
        if (allocated(error)) return

        ! ---- the grid carries the same default ----
        call gd%init(64, -600.0_real64, 600.0_real64, 60.0_real64)
        call gd%kernel(kname)
        call check(error, kname == "bspline", &
            "pf_kde_grid given no kernel= must take the B-spline too, or pilot= means two things")
        if (allocated(error)) return
        call gb%init(64, -600.0_real64, 600.0_real64, 60.0_real64, kernel="bspline")
        call gd%add(x, finish=.true.)
        call gb%add(x, finish=.true.)
        call gd%pdf(t, fd)
        call gb%pdf(t, fb)
        call check(error, all(fd == fb), &
            "the default grid must hold the same density as one naming the B-spline")

    end subroutine test_default_kernel_is_the_bspline

    !> A refit forgets the previous fit's settings, and `%clear` leaves the object unfitted.
    subroutine test_refit_and_clear(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde) :: k
        real(real64), allocatable :: x(:)
        real(real64) :: lo, hi, f

        call kde_fixture(20_int64, x)
        call k%fit(x, bandwidth=10.0_real64, lower=-500.0_real64, weights=spread(1.0_real64, 1, 20))
        call k%fit(x, bandwidth=10.0_real64)
        call k%bounds(lo, hi)
        call check(error, .not. ieee_is_finite(lo) .and. k%sum_weights() == 20.0_real64, &
            "a refit must not inherit the previous fit's bound or weights")
        if (allocated(error)) return
        call k%pdf(0.0_real64, f)
        call k%clear()
        call check(error, .not. k%is_fitted(), "%clear must leave the object unfitted")

    end subroutine test_refit_and_clear

    !> `%print` writes a heading and its rows, including the counts and the support, and an
    !> unfitted object prints one line saying so rather than aborting.
    subroutine test_print_writes(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde) :: k, empty
        real(real64), allocatable :: x(:)
        integer :: u, nlines
        logical :: seen
        character(len=*), parameter :: PATH = "test_run/kde_print_writes.txt"

        call kde_fixture(20_int64, x)
        call k%fit(x, rule="silverman", lower=-480.0_real64)
        open(newunit=u, file=PATH, status="replace", action="write")
        call k%print(unit=u)
        close(u)
        call read_back(PATH, nlines, "n_outside", seen)
        ! The heading, nine rows, the lower bound and the correction. The count is asserted, not
        ! the text, so that a reworded label is not a failure while a dropped row is.
        call check(error, nlines == 12 .and. seen, "%print must write a heading and eleven rows")
        if (allocated(error)) return
        ! The method row is asserted by NAME as well as by the count, because it is the one row
        ! saying which estimator answered: a count alone would not notice it being replaced.
        ! Reprinted first, since `read_back` deletes the file it reads.
        open(newunit=u, file=PATH, status="replace", action="write")
        call k%print(unit=u)
        close(u)
        call read_back(PATH, nlines, "method       exact", seen)
        call check(error, seen, "%print must name the method the queries are answered by")
        if (allocated(error)) return
        ! Under the other corrections there is no quadrature to report, and no row for one.
        open(newunit=u, file=PATH, status="replace", action="write")
        call k%print(unit=u)
        close(u)
        call read_back(PATH, nlines, "quadrature", seen)
        call check(error, .not. seen, "%print must not write a quadrature row under renormalise")
        if (allocated(error)) return
        ! Under `"linear"` it names the correction and nothing more: the per-point integrals are a
        ! fixed rule over analytic pieces, so there is no convergence for a row to report.
        call k%fit(x, rule="silverman", lower=-480.0_real64, boundary="linear")
        open(newunit=u, file=PATH, status="replace", action="write")
        call k%print(unit=u)
        close(u)
        call read_back(PATH, nlines, "boundary     linear", seen)
        call check(error, nlines == 12 .and. seen, &
            "%print under linear must name the correction and write no quadrature row")
        if (allocated(error)) return
        ! Each correction has its own row, and the printer names it from a table: every token in
        ! that table is checked, because a table is wrong one row at a time.
        call k%fit(x, rule="silverman", lower=-480.0_real64, boundary="renormalise")
        open(newunit=u, file=PATH, status="replace", action="write")
        call k%print(unit=u)
        close(u)
        call read_back(PATH, nlines, "boundary     renormalise", seen)
        call check(error, nlines == 12 .and. seen, "%print must name the renormalise correction")
        if (allocated(error)) return
        call k%fit(x, rule="silverman", lower=-480.0_real64, boundary="reflect")
        open(newunit=u, file=PATH, status="replace", action="write")
        call k%print(unit=u)
        close(u)
        call read_back(PATH, nlines, "boundary     reflect", seen)
        call check(error, nlines == 12 .and. seen, "%print must name the reflect correction")
        if (allocated(error)) return
        open(newunit=u, file=PATH, status="replace", action="write")
        call empty%print(unit=u)
        close(u)
        call read_back(PATH, nlines, "not fitted", seen)
        call check(error, nlines == 1 .and. seen, "an unfitted object must print one line saying so")

    end subroutine test_print_writes

    !> The zone sampler's fallback draws from the same distribution as its rejection.
    !>
    !> `parquet_debug_set_kde_sample_tries(0)` sends every draw straight to its fallback -- in a zone
    !> the inversion of the zone's own integral -- and those draws must still follow `%cdf` there:
    !> their empirical distribution function stays within the Kolmogorov-Smirnov critical distance
    !> at the 0.1 per cent level. The negative control is the same sample drawn first with the hook
    !> clear, which takes the rejection and differs from it in its bits; and the hook restored with
    !> a negative `n` reproduces that control exactly, which is what says the restore works.
    subroutine test_zone_sampler_fallback(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        integer, parameter :: N = 1000
        type(pf_kde) :: k
        real(real64) :: xe(40), v_rej(N), v_inv(N), v_back(N), zlo, dks
        character(len=200) :: msg
        integer :: nzone

        call kde_exponential(40, xe)
        call k%fit(xe, rule="silverman", lower=0.0_real64, boundary="linear")
        zlo = RADIUS(1)*k%bandwidth()
        call k%sample(v_rej, 41_int64)
        call parquet_debug_set_kde_sample_tries(0)
        call k%sample(v_inv, 41_int64)
        call parquet_debug_set_kde_sample_tries(-1)
        call k%sample(v_back, 41_int64)
        call check(error, any(v_inv /= v_rej), &
            "with no attempt allowed the draws must be the fallback's, not the rejection's")
        if (allocated(error)) return
        call check(error, all(v_back == v_rej), &
            "a negative n must restore the default, and the draws with it")
        if (allocated(error)) return
        call check(error, minval(v_inv) >= 0.0_real64, "no fallback draw may leave the support")
        if (allocated(error)) return
        call zone_ks_distance(k, v_inv, zlo, dks, nzone)
        write(msg, '(a,i0,a,es12.5,a,es12.5)') "the fallback's ", nzone, " zone draws: KS distance ", dks, &
            " against the critical ", 1.949_real64/sqrt(real(nzone, real64))
        call check(error, nzone >= 200 .and. dks <= 1.949_real64/sqrt(real(nzone, real64)), trim(msg))

    end subroutine test_zone_sampler_fallback

    !> The grid counts the weight beyond a FREE edge by the plain kernel's mass there, which R2
    !> keeps exact: beyond an edge a reach from the bound the correction is no longer acting. On a
    !> grid over `[0, 2]` with `lower = 0`, over data reaching far beyond it, `%cdf` at the top edge
    !> is the exact form's answer there to the grid's own discretisation.
    subroutine test_grid_linear_free_edge(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde) :: k
        type(pf_kde_grid) :: g
        real(real64) :: xe(200), p_exact, p_grid, step
        character(len=200) :: msg

        call kde_exponential(200, xe)
        call k%fit(xe, bandwidth=0.3_real64, lower=0.0_real64, boundary="linear")
        call k%cdf(2.0_real64, p_exact)
        call g%init(200, 0.0_real64, 2.0_real64, 0.3_real64, lower=0.0_real64, boundary="linear")
        call g%add(xe)
        call g%finish()
        call g%cdf(2.0_real64, p_grid)
        step = g%step()
        write(msg, '(a,es22.15,a,es22.15,a,es10.3)') "the grid's %cdf at its free edge is ", p_grid, &
            ", the exact form's ", p_exact, ", the cell width squared ", step*step
        call check(error, abs(p_grid - p_exact) <= 50.0_real64*step*step, trim(msg))
        if (allocated(error)) return
        call check(error, p_exact > 0.5_real64 .and. p_exact < 1.0_real64, &
            "precondition: weight must lie beyond the free edge, or nothing is being counted there")

    end subroutine test_grid_linear_free_edge

    !> R3: under `"linear"`, on a grid with a FREE edge, a point whose own reach exceeds the range's
    !> width poisons the grid rather than have its corrected weight counted beyond that edge by the
    !> plain kernel's mass. Its two negative controls are `bandwidth_max` below the width, which
    !> leaves the grid usable, and the same points on a grid bounded on BOTH sides, where no edge is
    !> free and the rule does not apply. A poisoned grid merged with one a kept NaN poisoned prints
    !> both reasons.
    subroutine test_grid_linear_reach_poisons(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde_grid) :: pilot, g, gc, gb, gn, gm
        real(real64) :: xe(200), f(200), nan
        integer :: u, nlines, n_in
        logical :: seen
        character(len=*), parameter :: PATH = "test_run/kde_linear_reach_poison.txt"

        ! Every point of this sample lies inside `[0, 3]`, so the three grids below see exactly the
        ! same points at exactly the same bandwidths and differ only in their bounds.
        call kde_exponential(200, xe)
        n_in = count(xe <= 3.0_real64)
        call pilot%init(200, 0.0_real64, 3.0_real64, 0.3_real64, lower=0.0_real64, boundary="linear")
        call pilot%add(xe(1:n_in))
        ! Where the pilot's density is small -- the sparse tail -- the rule gives a point a bandwidth
        ! several times the global one, whose reach passes the grid's own width.
        call pilot%finish()
        call g%init(200, 0.0_real64, 3.0_real64, 0.3_real64, pilot=pilot, alpha=1.0_real64, &
            lower=0.0_real64, boundary="linear")
        call g%add(xe(1:n_in))
        call g%finish()
        call g%density(f)
        call check(error, all(ieee_is_nan(f)), "R3 must poison the grid: every query answers NaN")
        if (allocated(error)) return
        ! Control one: a cap below the width leaves every reach inside it.
        call gc%init(200, 0.0_real64, 3.0_real64, 0.3_real64, pilot=pilot, alpha=1.0_real64, &
            bandwidth_max=0.5_real64, lower=0.0_real64, boundary="linear")
        call gc%add(xe(1:n_in))
        call gc%finish()
        call gc%density(f)
        call check(error, .not. any(ieee_is_nan(f)) .and. any(f > 0.0_real64), &
            "bandwidth_max below the width must leave the grid usable")
        if (allocated(error)) return
        ! Control two: the same points and the same pilot on a grid bounded at BOTH ends, where no
        ! edge is free and nothing can be counted beyond one.
        call gb%init(200, 0.0_real64, 3.0_real64, 0.3_real64, pilot=pilot, alpha=1.0_real64, &
            lower=0.0_real64, upper=3.0_real64, boundary="linear")
        call gb%add(xe(1:n_in))
        call gb%finish()
        call gb%density(f)
        call check(error, .not. any(ieee_is_nan(f)) .and. any(f > 0.0_real64), &
            "with both bounds given R3 must not fire: no edge is free")
        if (allocated(error)) return

        ! The two reasons a grid can be poisoned survive a merge, and `%print` gives each its line.
        nan = ieee_value(1.0_real64, ieee_quiet_nan)
        call gn%init(200, 0.0_real64, 3.0_real64, 0.3_real64, pilot=pilot, alpha=1.0_real64, &
            lower=0.0_real64, boundary="linear")
        call gn%add([nan], skipnan=.false.)
        ! On a grid of its own, not on `g`: `g` was queried above and so is closed, and asserting
        ! R3 on a grid a kept NaN had already poisoned would not be asserting R3.
        call gm%init(200, 0.0_real64, 3.0_real64, 0.3_real64, pilot=pilot, alpha=1.0_real64, &
            lower=0.0_real64, boundary="linear")
        call gm%add(xe(1:n_in))
        call gm%merge(gn, finish=.true.)
        open(newunit=u, file=PATH, status="replace", action="write")
        call gm%print(unit=u)
        close(u)
        call read_back(PATH, nlines, "every query answers NaN", seen)
        call check(error, seen, "the kept NaN's reason must survive the merge and be printed")
        if (allocated(error)) return
        open(newunit=u, file=PATH, status="replace", action="write")
        call g%print(unit=u)
        close(u)
        call read_back(PATH, nlines, "a kernel wider than the grid", seen)
        call check(error, seen, "R3's reason must be printed beside it")

    end subroutine test_grid_linear_reach_poisons

    !> R3 leaves the grid undefined quietly, which on its own names neither the cause nor the
    !! remedy. `%n_overreach` is what makes it diagnosable: how many points had a reach wider than
    !! the range, over every `%add`, with `%add`'s own out-argument reporting that call's share.
    !!
    !! The count is asserted by its IMPLICATIONS, never against a recorded number: it is positive
    !! and no larger than the population where R3 fires, and exactly zero on each of the two
    !! configurations where it cannot -- a cap below the width, and both bounds given, which are
    !! the two remedies the warning names. The `%merge` arm proves the counter is accumulated
    !! rather than assigned, which an `=` in place of a `+` would pass without.
    subroutine test_grid_overreach_counted(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde_grid) :: pilot, g, gc, gb, ga, gm
        real(real64) :: xe(200)
        integer(int64) :: reported, total
        integer :: n_in

        call kde_exponential(200, xe)
        n_in = count(xe <= 3.0_real64)
        call pilot%init(200, 0.0_real64, 3.0_real64, 0.3_real64, lower=0.0_real64, boundary="linear")
        call pilot%add(xe(1:n_in))
        call pilot%finish()

        ! ---- R3 fires: the count is positive, and it is this call's own ----
        call g%init(200, 0.0_real64, 3.0_real64, 0.3_real64, pilot=pilot, alpha=1.0_real64, &
            lower=0.0_real64, boundary="linear")
        call g%add(xe(1:n_in), n_overreach=reported)
        call check(error, reported > 0_int64 .and. reported <= int(n_in, int64), &
            "%add must report how many points out-reached the range, and no more than it was given")
        if (allocated(error)) return
        call check(error, g%n_overreach() == reported, &
            "the grid's total must be what the one %add that filled it reported")
        if (allocated(error)) return

        ! ---- the first remedy: a cap below the width ----
        call gc%init(200, 0.0_real64, 3.0_real64, 0.3_real64, pilot=pilot, alpha=1.0_real64, &
            bandwidth_max=0.5_real64, lower=0.0_real64, boundary="linear")
        call gc%add(xe(1:n_in), n_overreach=reported)
        call check(error, reported == 0_int64 .and. gc%n_overreach() == 0_int64, &
            "with bandwidth_max below the width no point can out-reach the range")
        if (allocated(error)) return

        ! ---- the second remedy: no free edge at all ----
        call gb%init(200, 0.0_real64, 3.0_real64, 0.3_real64, pilot=pilot, alpha=1.0_real64, &
            lower=0.0_real64, upper=3.0_real64, boundary="linear")
        call gb%add(xe(1:n_in), n_overreach=reported)
        call check(error, reported == 0_int64 .and. gb%n_overreach() == 0_int64, &
            "with both bounds given R3 cannot fire, so nothing is counted")
        if (allocated(error)) return

        ! ---- %merge adds the two totals, and %clear gives the counter back ----
        call ga%init(200, 0.0_real64, 3.0_real64, 0.3_real64, pilot=pilot, alpha=1.0_real64, &
            lower=0.0_real64, boundary="linear")
        call ga%add(xe(1:n_in))
        call gm%init(200, 0.0_real64, 3.0_real64, 0.3_real64, pilot=pilot, alpha=1.0_real64, &
            lower=0.0_real64, boundary="linear")
        call gm%add(xe(1:n_in))
        total = ga%n_overreach() + gm%n_overreach()
        call gm%merge(ga)
        call check(error, gm%n_overreach() == total, "%merge must add the two counts, not replace one")
        if (allocated(error)) return
        call gm%clear()
        call check(error, gm%n_overreach() == 0_int64, "%clear must give the counter back with the cells")

    end subroutine test_grid_overreach_counted

    !> The pilot an adaptive `"linear"` fit builds keeps R2: with one bound given, its free edge lies
    !> at least one kernel reach from that bound, so that the copy `%pilot` hands back would pass
    !> `%init`'s own rules. Without that, a sample whose every point lies within a bandwidth of the
    !> bound gives a pilot only `x(m) + 4h` wide, which is narrower than the Gaussian's reach.
    subroutine test_pilot_linear_keeps_r2(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde) :: k
        type(pf_kde_grid) :: p
        real(real64) :: x(60), c(4096), x0, x1, width
        integer :: i
        character(len=200) :: msg

        do i = 1, 60
            x(i) = 0.05_real64*(real(i, real64) - 0.5_real64)/60.0_real64
        end do
        call k%fit(x, kernel="gaussian", bandwidth=0.1_real64, adaptive=.true., lower=0.0_real64, boundary="linear")
        call k%pilot(p)
        call check(error, p%is_initialised() .and. p%ncells() <= 4096, "the pilot must be built")
        if (allocated(error)) return
        call p%grid(c(1:p%ncells()))
        x0 = c(1) - 0.5_real64*p%step()
        x1 = c(p%ncells()) + 0.5_real64*p%step()
        width = x1 - x0
        write(msg, '(a,es22.15,a,es22.15,a,es12.5)') "the pilot spans ", x0, " to ", x1, &
            ", against one reach ", RADIUS(1)*0.1_real64
        call check(error, abs(x0) <= 1.0e-12_real64 .and. width >= RADIUS(1)*0.1_real64, trim(msg))

    end subroutine test_pilot_linear_keeps_r2

    !> The scan finds a stretch that lies between two kernel edges closer together than its own
    !> spacing, which only its refinement can see.
    !>
    !> The fixture is a box-kernel fit whose upper zone holds a far group -- whose terms there are
    !> NEGATIVE, the correction's factor turning over for points well to the left -- and a near
    !> cluster whose terms are positive. Each far point's kernel ENDS inside the zone, and the
    !> estimate jumps up as each one switches off; between two such jumps, closer together than a
    !> sixty-fourth of a bandwidth, the estimate dips below zero. `%cdf` counts a stretch the scan
    !> misses while `%pdf` clips it, so the two are compared across the whole zone.
    subroutine test_linear_scan_refinement(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(kde_density) :: fn
        real(real64) :: x(280), zhi, c0, c1, got, want, h, cuts(560), v
        type(pf_integration_info) :: info
        type(pf_kde_grid) :: gdiag
        real(real64) :: rawc(4000)
        character(len=200) :: msg2
        logical :: conv
        integer :: i, j, ncut
        character(len=200) :: msg

        h = 0.03_real64
        ! Two hundred points just below the zone's inner edge: near the bound their terms are
        ! NEGATIVE (the factor turns over about one and a sixth bandwidths to the left) and each
        ! one's kernel ENDS inside the zone, twenty-five millionths apart -- a twentieth of the
        ! scan's own spacing. Eighty points at the bound make the sum hover about zero there, so it
        ! dips below between two of those jumps.
        do i = 1, 200
            x(i) = 0.943_real64 + 0.005_real64*(real(i, real64) - 0.5_real64)/200.0_real64
        end do
        do i = 201, 280
            x(i) = 0.99_real64 + 0.01_real64*(real(i - 200, real64) - 0.5_real64)/80.0_real64
        end do
        call fn%k%fit(x(1:280), bandwidth=h, kernel="box", upper=1.0_real64, boundary="linear")
        fn%power = 0
        zhi = 1.0_real64 - sqrt(3.0_real64)*h
        call fn%k%cdf(zhi, c0)
        call fn%k%cdf(1.0_real64, c1)
        got = c1 - c0
        ! The box's estimate JUMPS at every kernel edge, and a quadrature left to find four hundred
        ! of them itself does not converge (measured: `abserr` 5e-6, which would swamp what this
        ! test is looking for). The edges are where they are by construction, so they are passed.
        ncut = 0
        do i = 1, 280
            do j = 1, 2
                v = x(i) + merge(-1.0_real64, 1.0_real64, j == 1)*sqrt(3.0_real64)*h
                if (.not. (v > zhi .and. v < 1.0_real64)) cycle
                ncut = ncut + 1
                cuts(ncut) = v
            end do
        end do
        call sort_cuts(cuts, ncut)
        want = pf_integrate(fn, zhi, 1.0_real64, 1.0e-12_real64, breakpoints=cuts(1:ncut), &
            converged=conv, info=info)
        write(msg, '(a,es12.5,a,es12.5,a,es10.3,a,l1,a,i0)') "across the zone %cdf gives ", got, &
            " and the integral of %pdf ", want, ", differing by ", got - want, "; converged ", conv, &
            ", cuts ", ncut
        call check(error, conv .and. ncut > 100, "the reference integral must converge over the zone's edges")
        if (allocated(error)) return
        ! The precondition this test rests on: the raw estimate dips below zero inside the zone, and
        ! over a width of about one scan spacing -- the cluster's size is what tunes that. The
        ! grid's unnormalised cells, a twentieth of the spacing wide, are what shows it.
        call gdiag%init(4000, 0.0_real64, 1.0_real64, h, kernel="box", upper=1.0_real64, boundary="linear")
        call gdiag%add(x(1:280))
        call gdiag%finish()
        call gdiag%density(rawc, normalise=.false.)
        write(msg2, '(a,i0,a,es10.3,a,es10.3)') "the raw estimate is negative in ", &
            count(rawc < 0.0_real64), " cells of ", gdiag%step(), ", against a scan spacing of ", &
            h/64.0_real64
        call check(error, count(rawc < 0.0_real64) >= 1 .and. count(rawc < 0.0_real64) <= 4, trim(msg2))
        if (allocated(error)) return
        call check(error, abs(got - want) <= 1.0e-10_real64, trim(msg))

    end subroutine test_linear_scan_refinement

    !> Every golden case of the linear boundary correction: the density and the distribution
    !> function at every probe, against the 50-digit oracle, which integrates the clipped SUM
    !> between its knots and its sign changes rather than point by point as the library does.
    subroutine test_linear_golden_vectors(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check

        call check_golden_case(error, "LIN_LO", KG_LIN_LO_DEF, KG_LIN_LO_H, KG_LIN_LO_PDF, KG_LIN_LO_CDF)
        if (allocated(error)) return
        call check_golden_case(error, "LIN_HI_BOX", KG_LIN_HI_BOX_DEF, KG_LIN_HI_BOX_H, KG_LIN_HI_BOX_PDF, &
            KG_LIN_HI_BOX_CDF)
        if (allocated(error)) return
        call check_golden_case(error, "LIN_BOTH", KG_LIN_BOTH_DEF, KG_LIN_BOTH_H, KG_LIN_BOTH_PDF, KG_LIN_BOTH_CDF)
        if (allocated(error)) return
        call check_golden_case(error, "LIN_BSPL", KG_LIN_BSPL_DEF, KG_LIN_BSPL_H, KG_LIN_BSPL_PDF, KG_LIN_BSPL_CDF)
        if (allocated(error)) return
        call check_golden_case(error, "LIN_WIDE", KG_LIN_WIDE_DEF, KG_LIN_WIDE_H, KG_LIN_WIDE_PDF, KG_LIN_WIDE_CDF)
        if (allocated(error)) return
        call check_golden_case(error, "LIN_W", KG_LIN_W_DEF, KG_LIN_W_H, KG_LIN_W_PDF, KG_LIN_W_CDF)
        if (allocated(error)) return
        call check_golden_case(error, "LIN_ZERO", KG_LIN_ZERO_DEF, KG_LIN_ZERO_H, KG_LIN_ZERO_PDF, KG_LIN_ZERO_CDF)
        if (allocated(error)) return
        call check_golden_case(error, "LIN_NARROW3", KG_LIN_NARROW3_DEF, KG_LIN_NARROW3_H, KG_LIN_NARROW3_PDF, &
            KG_LIN_NARROW3_CDF)
        if (allocated(error)) return
        call check_golden_case(error, "LIN_NARROW6", KG_LIN_NARROW6_DEF, KG_LIN_NARROW6_H, KG_LIN_NARROW6_PDF, &
            KG_LIN_NARROW6_CDF)

    end subroutine test_linear_golden_vectors

    !> Under `"linear"` the density is never negative, and the clip is what keeps it so.
    !>
    !> A scan of two thousand points across the support of two fixtures whose density vanishes at a
    !> bound -- `2x` at the lower one, `3(1 - x)**2` at the upper -- where the raw linear estimate
    !> IS negative: `%pdf` is at no point below zero, and is exactly zero at the bound the density
    !> vanishes at, which is what says the clip acted rather than the test passing vacuously.
    subroutine test_linear_never_negative(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde) :: k
        type(pf_kde_grid) :: g
        real(real64) :: x(200), pts(2000), f(2000), raw(400)
        integer :: i, fixture

        do fixture = 1, 2
            if (fixture == 1) then
                call kde_rising(200, x)
            else
                call kde_falling(200, x)
            end if
            call k%fit(x, rule="silverman", lower=0.0_real64, upper=1.0_real64, boundary="linear")
            do i = 1, 2000
                pts(i) = (real(i, real64) - 1.0_real64)/1999.0_real64
            end do
            call k%pdf(pts, f)
            call check(error, .not. any(f < 0.0_real64), &
                merge("2x         ", "3(1 - x)**2", fixture == 1) // ": %pdf must never be negative under linear")
            if (allocated(error)) return
            if (fixture == 1) then
                call check(error, f(1) == 0.0_real64 .and. f(2000) > 0.0_real64, &
                    "2x: the clip must zero the density at the bound it vanishes at")
            else
                call check(error, f(2000) == 0.0_real64 .and. f(1) > 0.0_real64, &
                    "3(1 - x)**2: the clip must zero the density at the bound it vanishes at")
            end if
            if (allocated(error)) return
            ! What the clip removed: the RAW estimate, which the grid's `normalise = .false.`
            ! answers as deposited, IS negative on these fixtures -- so the test above is not
            ! passing on an estimate that was never negative in the first place.
            call g%init(400, 0.0_real64, 1.0_real64, k%bandwidth(), lower=0.0_real64, &
                upper=1.0_real64, boundary="linear")
            call g%add(x)
            call g%finish()
            call g%density(raw, normalise=.false.)
            call check(error, any(raw < 0.0_real64), &
                "the raw linear sum must be negative somewhere, or the clip has nothing to remove")
            if (allocated(error)) return
        end do

    end subroutine test_linear_never_negative

    !> At a bound the density rises from, the linear correction is far closer than either simple
    !> one: on the guide's own fixture the integrated squared error over the boundary zone is below
    !> a tenth of `"renormalise"`'s and of `"reflect"`'s, each by `pf_integrate` of the squared
    !> error against the true density `2x`.
    subroutine test_linear_beats_the_simple_corrections(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(kde_sq_error) :: fn
        real(real64) :: x(200), ise(3), h
        character(len=11), parameter :: METHODS(3) = [character(len=11) :: "renormalise", "reflect", "linear"]
        integer :: mm

        call kde_rising(200, x)
        do mm = 1, 3
            call fn%k%fit(x, rule="silverman", lower=0.0_real64, boundary=trim(METHODS(mm)))
            h = fn%k%bandwidth()
            ise(mm) = pf_integrate(fn, 0.0_real64, RADIUS(1)*h, 1.0e-10_real64)
        end do
        call check(error, ise(3) <= 0.1_real64*min(ise(1), ise(2)), &
            "linear's zone error must be below a tenth of renormalise's and reflect's")
        if (allocated(error)) return
        call check(error, ise(3) > 0.0_real64, "the comparison must be over a positive error")

    end subroutine test_linear_beats_the_simple_corrections

    !> `%cdf` is one function across the zones' edges, and it integrates `%pdf` everywhere.
    !>
    !> Under `"linear"` `%cdf` is answered by three formulas -- the lower zone's per-point
    !> integrals, a closed form between the zones, the upper zone's -- and they must agree where
    !> they meet: at each edge the two sides are within `1e-12`. And on four intervals, one inside
    !> each zone, one straddling an edge and one deep in the interior, `%cdf`'s difference equals
    !> `pf_integrate` of `%pdf` to `1e-10`, which a wrong zone split or a window sum that counts a
    !> point twice cannot pass. The fixture's lower zone holds hundreds of points.
    subroutine test_linear_cdf_across_zones(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(kde_density) :: fn
        real(real64) :: x(2000), h, zlo, zhi, d, c1, c2, got, want, s, t, cuts(1)
        integer :: iv
        character(len=160) :: what

        call kde_rising(2000, x)
        call fn%k%fit(x, rule="silverman", lower=0.0_real64, upper=1.0_real64, boundary="linear")
        fn%power = 0
        h = fn%k%bandwidth()
        zlo = 0.0_real64 + RADIUS(1)*h
        zhi = 1.0_real64 - RADIUS(1)*h
        call check(error, zlo < zhi, "precondition: the two zones must not meet on this fixture")
        if (allocated(error)) return
        ! Either side of each edge, close enough that the density cannot move the answer: the two
        ! formulas must agree there.
        do iv = 1, 2
            d = 8.0_real64*spacing(merge(zlo, zhi, iv == 1))
            call fn%k%cdf(merge(zlo, zhi, iv == 1) - d, c1)
            call fn%k%cdf(merge(zlo, zhi, iv == 1) + d, c2)
            call check(error, abs(c1 - c2) <= 1.0e-12_real64, &
                merge("z_lo", "z_hi", iv == 1) // ": %cdf must be continuous across the zone's edge")
            if (allocated(error)) return
        end do
        do iv = 1, 4
            select case (iv)
            case (1)
                s = 0.02_real64
                t = 0.9_real64*zlo
            case (2)
                s = zlo - 0.05_real64
                t = zlo + 0.05_real64
            case (3)
                s = 0.5_real64
                t = 0.6_real64
            case default
                s = zhi - 0.05_real64
                t = zhi + 0.05_real64
            end select
            cuts(1) = merge(zlo, zhi, iv <= 2)
            if (cuts(1) > s .and. cuts(1) < t) then
                want = pf_integrate(fn, s, t, 1.0e-12_real64, breakpoints=cuts)
            else
                want = pf_integrate(fn, s, t, 1.0e-12_real64)
            end if
            call fn%k%cdf(t, c2)
            call fn%k%cdf(s, c1)
            got = c2 - c1
            write(what, '(a,i0,a,es12.5)') "interval ", iv, ": %cdf less the integral of %pdf is ", got - want
            call check(error, abs(got - want) <= 1.0e-10_real64, trim(what))
            if (allocated(error)) return
        end do

    end subroutine test_linear_cdf_across_zones

    !> Tied values, and a knot of the kernel that falls exactly on a knot of its moments, abort
    !> nothing: the per-point breakpoint lists are made distinct before `pf_integrate` sees them,
    !> and it aborts on a repeated breakpoint.
    !>
    !> The first fit is over the two-component recipe rounded to multiples of 8, where values tie in
    !> hundreds; the second places a cubic B-spline point exactly `2C` bandwidths above the bound, so
    !> that its kernel's knot `x_j - C h` and its moments' knot `lower + C h` are the same number to
    !> the bit. Every query answers, and the second fit's `%cdf` is the integral of its `%pdf`.
    subroutine test_linear_tied_values_and_knots(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(kde_density) :: fn
        real(real64), allocatable :: x(:)
        real(real64) :: c, q, f, one(1), got, want, c0, c1
        character(len=160) :: msg
        logical :: ok

        call kde_two_component(60_int64, x)
        call kde_rounded(8.0_real64, x)
        call fn%k%fit(x, bandwidth=60.0_real64, lower=-400.0_real64, upper=400.0_real64, &
            boundary="linear", ok=ok)
        call check(error, ok, "tied values must leave the linear fit defined")
        if (allocated(error)) return
        call fn%k%pdf(-100.0_real64, f)
        call fn%k%cdf(-100.0_real64, c)
        call fn%k%quantile(0.5_real64, q)
        call check(error, f >= 0.0_real64 .and. c >= 0.0_real64 .and. c <= 1.0_real64 .and. &
            q >= -400.0_real64 .and. q <= 400.0_real64, "every query must answer over tied values")
        if (allocated(error)) return

        ! The coinciding knots: `C = sqrt(3)` for the cubic B-spline, one point at `2C` bandwidths.
        one(1) = 2.0_real64*sqrt(3.0_real64)
        call fn%k%fit(one, bandwidth=1.0_real64, kernel="bspline", lower=0.0_real64, boundary="linear", ok=ok)
        call check(error, ok, "the coinciding-knot fit must be defined")
        if (allocated(error)) return
        fn%power = 0
        want = pf_integrate(fn, 0.0_real64, one(1) + 2.0_real64*sqrt(3.0_real64), 1.0e-12_real64)
        call fn%k%cdf(one(1) + 2.0_real64*sqrt(3.0_real64), c1)
        call fn%k%cdf(0.0_real64, c0)
        got = c1 - c0
        write(msg, '(a,es22.15,a,es22.15)') "the knot fit: %cdf's difference is ", got, ", the integral of %pdf ", want
        call check(error, abs(got - want) <= 1.0e-10_real64 .and. abs(want - 1.0_real64) <= 1.0e-10_real64, trim(msg))

    end subroutine test_linear_tied_values_and_knots

    !> The table in doc/pages/utilities/kernel-density.md, "Bounded support": two hundred points at
    !> the quantiles `sqrt((i - 1/2)/200)` of the density `2x` on `[0, 1]`, Silverman's rule, read at
    !> four points unbounded and under each correction. Asserted to the three decimals the page prints,
    !> so a change to either correction that moves the page's numbers fails here first.
    subroutine test_guide_boundary_table(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde) :: k
        real(real64) :: x(200), got(4), at_bound
        character(len=140) :: what
        real(real64), parameter :: AT(4) = [0.0_real64, 0.05_real64, 0.1_real64, 0.2_real64]
        real(real64), parameter :: NONE(4) = [0.058_real64, 0.122_real64, 0.207_real64, 0.400_real64]
        real(real64), parameter :: REN(4) = [0.116_real64, 0.163_real64, 0.226_real64, 0.401_real64]
        real(real64), parameter :: REF(4) = [0.117_real64, 0.143_real64, 0.212_real64, 0.400_real64]
        real(real64), parameter :: LIN(4) = [0.000_real64, 0.100_real64, 0.202_real64, 0.402_real64]
        integer :: i

        do i = 1, 200
            x(i) = sqrt((real(i, real64) - 0.5_real64)/200.0_real64)
        end do
        call k%fit(x, kernel="gaussian", rule="silverman")
        call check(error, abs(k%bandwidth() - 0.0737_real64) <= 5.0e-5_real64, &
            "the page prints h = 0.0737")
        if (allocated(error)) return
        call k%pdf(AT, got)
        call check(error, all(abs(got - NONE) <= 5.0e-4_real64), "the page's unbounded column")
        if (allocated(error)) return
        call k%fit(x, kernel="gaussian", rule="silverman", lower=0.0_real64, boundary="renormalise")
        call k%pdf(AT, got)
        call check(error, all(abs(got - REN) <= 5.0e-4_real64), "the page's renormalise column")
        if (allocated(error)) return
        at_bound = got(1)
        call k%fit(x, kernel="gaussian", rule="silverman", lower=0.0_real64, boundary="reflect")
        call k%pdf(AT, got)
        call check(error, all(abs(got - REF) <= 5.0e-4_real64), "the page's reflect column")
        if (allocated(error)) return
        ! The page says the two local-constant corrections agree AT the bound, where both double the
        ! one-sided sum. Asserted on the values themselves, not on the table's rounded ones.
        write(what, '(a,es12.5,a,es12.5)') "the page says renormalise and reflect agree at the " // &
            "bound; they are ", at_bound, " and ", got(1)
        call check(error, abs(at_bound - got(1)) <= 1.0e-3_real64, trim(what))
        if (allocated(error)) return
        call k%fit(x, kernel="gaussian", rule="silverman", lower=0.0_real64, boundary="linear")
        call k%pdf(AT, got)
        call check(error, all(abs(got - LIN) <= 5.0e-4_real64), "the page's linear column")

    end subroutine test_guide_boundary_table

    ! ==========================================================================================
    ! pf_kde_grid
    ! ==========================================================================================

    !> `size(t)` points spread irregularly over `[a, b]`: the fractional parts of `i` times the
    !> golden ratio, which land at every position within the cells of any grid, as a convergence
    !> rate measured over positions needs.
    subroutine spread_points(a, b, t)
        real(real64), intent(in)  :: a    !! the first end
        real(real64), intent(in)  :: b    !! the other end
        real(real64), intent(out) :: t(:) !! the points
        integer :: i

        do i = 1, size(t)
            t(i) = a + (b - a)*modulo(real(i, real64)*0.6180339887498949_real64, 1.0_real64)
        end do

    end subroutine spread_points

    !> `kde_fixture`'s values folded onto `[0, 1)` and squared: a density rising towards zero, so
    !> that a lower bound at zero matters.
    subroutine bounded_fixture(n, z)
        integer(int64), intent(in)             :: n    !! how many values
        real(real64), allocatable, intent(out) :: z(:) !! the values, in `[0, 1)`

        call kde_fixture(n, z)
        z = (abs(z)/489.0_real64)**2

    end subroutine bounded_fixture

    !> The root mean square of `d`.
    pure function rms(d) result(r)
        real(real64), intent(in) :: d(:) !! the differences
        real(real64)             :: r    !! their RMS

        r = sqrt(sum(d*d)/real(size(d), real64))

    end function rms

    !> The raw accumulation of a grid, `%density(normalise=.false.)`, into a new array.
    subroutine raw_cells(g, f)
        type(pf_kde_grid), intent(in)          :: g    !! the grid
        real(real64), allocatable, intent(out) :: f(:) !! its accumulation per cell

        allocate(f(g%ncells()))
        call g%density(f, normalise=.false.)

    end subroutine raw_cells

    !> The grid converges to the exact estimate as the square of its cell width. Linear
    !> interpolation's error at a point a fraction `s` into a segment is `s(1 - s)/2 * step**2 * f''`,
    !> so its RMS over points spread irregularly across the cells falls by four when the cells
    !> halve. A cell index off by one, or a centre off by half a cell, leaves an error of order
    !> `step` and a ratio of two. The two smooth kernels, unbounded and under both corrections
    !> (read away from the outer half-cells, where the interpolant is constant); the other two
    !> have kinks or jumps in their estimate and converge more slowly, by design.
    subroutine test_grid_converges(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde) :: k
        type(pf_kde_grid) :: g
        real(real64), allocatable :: x(:), z(:), c(:), f(:), fe(:)
        real(real64) :: t(997), fx(997), fg(997), e(2), h, lo, hi
        character(len=11), parameter :: METHODS(3) = [character(len=11) :: "renormalise", "reflect", "linear"]
        integer :: kk, r, mm
        character(len=200) :: msg

        call kde_fixture(400_int64, x)
        h = 40.0_real64
        lo = minval(x) - 6.0_real64*h
        hi = maxval(x) + 6.0_real64*h
        call spread_points(minval(x), maxval(x), t)
        do kk = 1, 3, 2
            call k%fit(x, bandwidth=h, kernel=KERNELS(kk))
            call k%pdf(t, fx)
            do r = 1, 2
                call g%init(100*r, lo, hi, h, kernel=KERNELS(kk))
                call g%add(x)
                call g%finish()
                call g%pdf(t, fg)
                e(r) = rms(fg - fx)
            end do
            write(msg, '(3a,f8.4)') "unbounded ", trim(KERNELS(kk)), &
                ": halving the cells must divide the RMS error by four; the ratio is ", e(1)/e(2)
            call check(error, e(1)/e(2) > 3.6_real64 .and. e(1)/e(2) < 4.4_real64, trim(msg))
            if (allocated(error)) return
        end do

        ! At its own centres the Gaussian grid is the exact estimate to the residual of the
        ! discrete normalisation, which is far below the interpolation error.
        call k%fit(x, kernel="gaussian", bandwidth=h)
        call g%init(100, lo, hi, h, kernel="gaussian")
        call g%add(x)
        allocate(c(100), f(100), fe(100))
        call g%finish()
        call g%density(f, x=c)
        call k%pdf(c, fe)
        write(msg, '(a,es10.3)') "at its centres the grid must be the exact estimate; the largest " // &
            "relative gap is ", maxval(abs(f - fe))/maxval(fe)
        call check(error, maxval(abs(f - fe)) <= 1.0e-6_real64*maxval(fe), trim(msg))
        if (allocated(error)) return

        call bounded_fixture(400_int64, z)
        call spread_points(0.02_real64, 0.98_real64, t)
        do mm = 1, 3
            do kk = 1, 3, 2
                call k%fit(z, bandwidth=0.05_real64, kernel=KERNELS(kk), lower=0.0_real64, &
                    upper=1.0_real64, boundary=METHODS(mm))
                call k%pdf(t, fx)
                do r = 1, 2
                    call g%init(100*r, 0.0_real64, 1.0_real64, 0.05_real64, kernel=KERNELS(kk), &
                        lower=0.0_real64, upper=1.0_real64, boundary=METHODS(mm))
                    call g%add(z)
                    call g%finish()
                    call g%pdf(t, fg)
                    e(r) = rms(fg - fx)
                end do
                write(msg, '(4a,f8.4)') trim(METHODS(mm)), " ", trim(KERNELS(kk)), &
                    ": halving the cells must divide the RMS error by four; the ratio is ", e(1)/e(2)
                call check(error, e(1)/e(2) > 3.6_real64 .and. e(1)/e(2) < 4.4_real64, trim(msg))
                if (allocated(error)) return
            end do
        end do

    end subroutine test_grid_converges

    !> Every accepted point deposits exactly its weight, at every resolution: `sum(f)*step` is one
    !> to rounding for every kernel, unbounded, and under `"reflect"` with the bandwidth twice the
    !> width of the support (every point corrected, F3's case), and under one bound.
    !> `normalise = .false.` sums to the total weight. A deposit normalised by the midpoint rule
    !> instead of by its own discrete sum misses by that rule's residual, `1e-3` and more here.
    !>
    !> **The local-polynomial corrections are not in this test, and cannot be.** Under
    !> `"renormalise"` and `"linear"` the divisor is the mass a kernel centred AT THE QUERY POINT
    !> keeps inside the support, so there is no per-point split to conserve: a point deposits its
    !> own corrected mass and the estimate is normalised once, at query time. What replaces the
    !> promise is checked elsewhere -- that the grid converges to the exact estimate
    !> (`test_grid_converges`), and that the weight it counts beyond its range is that estimate's
    !> own mass there (`test_grid_counts_weight_beyond_range`).
    subroutine test_grid_deposits_exact_mass(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde_grid) :: g
        real(real64), allocatable :: x(:), w(:), f(:)
        character(len=11), parameter :: METHODS(1) = [character(len=11) :: "reflect"]
        integer, parameter :: CELLS(3) = [7, 64, 501]
        integer :: kk, r, mm
        character(len=160) :: msg

        call kde_fixture(64_int64, x)
        x = x/1000.0_real64 + 0.5_real64
        call kde_weights_mod5(64_int64, w)
        do kk = 1, 4
            do r = 1, 3
                ! Unbounded, on a range every kernel's reach fits inside.
                call g%init(CELLS(r), -0.2_real64, 1.2_real64, 0.03_real64, kernel=KERNELS(kk))
                call g%add(x)
                call g%finish()
                call raw_cells(g, f)
                write(msg, '(2a,i0,a,es10.3)') trim(KERNELS(kk)), " unbounded at ", CELLS(r), &
                    " cells: the mass is one plus ", sum(f)*g%step()/g%sum_weights() - 1.0_real64
                call check(error, abs(sum(f)*g%step()/g%sum_weights() - 1.0_real64) <= 1.0e-13_real64, trim(msg))
                if (allocated(error)) return
                do mm = 1, size(METHODS)
                    ! Both bounds, the bandwidth twice the support: every point is corrected.
                    call g%init(CELLS(r), 0.0_real64, 1.0_real64, 2.0_real64, kernel=KERNELS(kk), &
                        lower=0.0_real64, upper=1.0_real64, boundary=METHODS(mm))
                    call g%add(x, weights=w)
                    call g%finish()
                    call raw_cells(g, f)
                    write(msg, '(4a,i0,a,es10.3)') trim(KERNELS(kk)), " ", trim(METHODS(mm)), &
                        " (wide) at ", CELLS(r), " cells: the mass is one plus ", &
                        sum(f)*g%step()/g%sum_weights() - 1.0_real64
                    call check(error, abs(sum(f)*g%step()/g%sum_weights() - 1.0_real64) <= 1.0e-13_real64, &
                        trim(msg))
                    if (allocated(error)) return
                    ! One bound, the range reaching past every kernel on the open side.
                    call g%init(CELLS(r), 0.0_real64, 1.5_real64, 0.08_real64, kernel=KERNELS(kk), &
                        lower=0.0_real64, boundary=METHODS(mm))
                    call g%add(x)
                    call g%finish()
                    call raw_cells(g, f)
                    write(msg, '(4a,i0,a,es10.3)') trim(KERNELS(kk)), " ", trim(METHODS(mm)), &
                        " (lower) at ", CELLS(r), " cells: the mass is one plus ", &
                        sum(f)*g%step()/g%sum_weights() - 1.0_real64
                    call check(error, abs(sum(f)*g%step()/g%sum_weights() - 1.0_real64) <= 1.0e-13_real64, &
                        trim(msg))
                    if (allocated(error)) return
                end do
            end do
        end do
        call check(error, g%sum_weights() == 64.0_real64, &
            "the last grid, unweighted, must carry one unit of weight per point")

    end subroutine test_grid_deposits_exact_mass

    !> The weight a grid narrower than the data counts below `xmin` and above `xmax` is the exact
    !> estimate's own distribution function at those two points, to rounding: each point's share
    !> beyond an end is formed from its kernel's distribution function, as `pf_kde%cdf` forms it.
    !> Beyond the range `%pdf` is zero, `%cdf` is flat, and a quantile there answers the range's end.
    subroutine test_grid_counts_weight_beyond_range(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde) :: k
        type(pf_kde_grid) :: g
        real(real64), allocatable :: x(:), z(:)
        real(real64) :: p0, p1, e0, e1, pb, fb, q, gap, fine
        character(len=11), parameter :: METHODS(2) = [character(len=11) :: "renormalise", "reflect"]
        !> The two kernels whose binned estimate converges as the square of the cell width; the
        !! other two have kinks or jumps and converge more slowly, as the arm above says.
        character(len=8), parameter :: SMOOTH(2) = [character(len=8) :: "gaussian", "bspline"]
        character(len=160) :: what
        integer :: mm

        call kde_fixture(400_int64, x)
        call k%fit(x, bandwidth=40.0_real64)
        call g%init(100, -100.0_real64, 100.0_real64, 40.0_real64)
        call g%add(x)
        call g%finish()
        call g%cdf(-100.0_real64, p0)
        call g%cdf(100.0_real64, p1)
        call k%cdf(-100.0_real64, e0)
        call k%cdf(100.0_real64, e1)
        call check(error, p0 > 0.3_real64 .and. p1 < 0.7_real64, &
            "the fixture must put a good part of its weight beyond each end, or this test is vacuous")
        if (allocated(error)) return
        call check(error, abs(p0 - e0) <= 1.0e-12_real64 .and. abs(p1 - e1) <= 1.0e-12_real64, &
            "the weight counted beyond each end must be the exact estimate's distribution function there")
        if (allocated(error)) return
        call g%cdf(-300.0_real64, pb)
        call g%pdf(-300.0_real64, fb)
        call check(error, pb == p0 .and. fb == 0.0_real64, &
            "below the range the density is zero and the distribution function flat")
        if (allocated(error)) return
        call g%quantile(0.5_real64*p0, q)
        call check(error, q == -100.0_real64, "a quantile the weight below xmin covers answers xmin")
        if (allocated(error)) return
        call g%quantile(0.5_real64*(1.0_real64 + p1), q)
        call check(error, q == 100.0_real64, "a quantile only the weight above xmax reaches answers xmax")
        if (allocated(error)) return

        ! Under both corrections, with the range inside the support.
        call bounded_fixture(400_int64, z)
        do mm = 1, 2
            call k%fit(z, bandwidth=0.05_real64, lower=0.0_real64, upper=1.0_real64, boundary=METHODS(mm))
            call g%init(60, 0.2_real64, 0.8_real64, 0.05_real64, lower=0.0_real64, upper=1.0_real64, &
                boundary=METHODS(mm))
            call g%add(z)
            call g%finish()
            call g%cdf(0.2_real64, p0)
            call g%cdf(0.8_real64, p1)
            call k%cdf(0.2_real64, e0)
            call k%cdf(0.8_real64, e1)
            if (METHODS(mm) == "reflect") then
                ! Each point's weight is split exactly between the cells and the two counters, so
                ! the share counted below is the exact estimate's own distribution function there.
                call check(error, abs(p0 - e0) <= 1.0e-12_real64 .and. abs(p1 - e1) <= 1.0e-12_real64, &
                    trim(METHODS(mm)) // ": the weight counted beyond each end must be the exact " // &
                    "estimate's distribution function there")
                if (allocated(error)) return
                cycle
            end if
            ! Under the corrected `"renormalise"` the counters ARE the corrected density's own mass
            ! beyond each end -- that part is exact -- but the total they are a share of is the
            ! grid's own discrete mass, so the two agree to the grid's discretisation and not to
            ! rounding. What must hold is that the gap is small and that it falls as the square of
            ! the cell width: a counter formed from the PLAIN kernel instead would sit at `4e-3`
            ! here and not move when the cells are refined.
            gap = max(abs(p0 - e0), abs(p1 - e1))
            call g%init(240, 0.2_real64, 0.8_real64, 0.05_real64, lower=0.0_real64, upper=1.0_real64, &
                boundary=METHODS(mm))
            call g%add(z)
            call g%finish()
            call g%cdf(0.2_real64, p0)
            call g%cdf(0.8_real64, p1)
            fine = max(abs(p0 - e0), abs(p1 - e1))
            write(what, '(3a,es10.3,a,es10.3)') trim(METHODS(mm)), ": the share counted beyond an end ", &
                "misses the exact estimate by ", gap, " at 60 cells and ", fine
            call check(error, gap <= 1.0e-5_real64 .and. fine <= 0.125_real64*gap, trim(what))
            if (allocated(error)) return
        end do

        ! ---- and the regime where the range starts further from the bound than a kernel reaches ----
        ! **`x0 - lower > KDE_RADIUS*h` is its own case, and every kernel can be put in it.** The
        ! zone below the range is then partly corrected and partly not, and two short cuts used to
        ! answer as if it were neither: a point wholly past the range's end counted its WEIGHT
        ! rather than its corrected mass, and a point reaching no cell centre put `1 - s_below -
        ! s_above` into the first cell, which is that same assumption written the other way round.
        ! Both are fixed masses, so refining the cells could not reveal them -- the gap simply
        ! stopped falling. The arm above cannot see either, because at `h = 0.05` the Gaussian
        ! reaches `0.25` and the range starts at `0.2`: it is the NARROWER kernels that leave the
        ! strip, and the Gaussian only escaped by reaching across it.
        !
        ! Asserted as convergence rather than as a value: what a fixed error breaks is the falling,
        ! so `fine` is held to a quarter of `gap` and both to a share of the estimate they miss.
        call bounded_fixture(400_int64, z)
        do mm = 1, 2
            call k%fit(z, kernel=trim(SMOOTH(mm)), bandwidth=0.05_real64, lower=0.0_real64, &
                upper=1.0_real64, boundary="renormalise")
            call k%cdf(0.35_real64, e0)
            call g%init(60, 0.35_real64, 0.65_real64, 0.05_real64, kernel=trim(SMOOTH(mm)), &
                lower=0.0_real64, upper=1.0_real64, boundary="renormalise")
            call g%add(z, finish=.true.)
            call g%cdf(0.35_real64, p0)
            gap = abs(p0 - e0)
            call g%init(240, 0.35_real64, 0.65_real64, 0.05_real64, kernel=trim(SMOOTH(mm)), &
                lower=0.0_real64, upper=1.0_real64, boundary="renormalise")
            call g%add(z, finish=.true.)
            call g%cdf(0.35_real64, p0)
            fine = abs(p0 - e0)
            write(what, '(2a,es10.3,a,es10.3)') trim(SMOOTH(mm)), &
                ": with the range starting past the kernel's reach the share counted below it " // &
                "misses the exact estimate by ", gap, " at 60 cells and ", fine
            call check(error, gap <= 1.0e-3_real64 .and. fine <= 0.25_real64*gap, trim(what))
            if (allocated(error)) return
        end do

    end subroutine test_grid_counts_weight_beyond_range

    !> A kernel narrower than a cell: on a centre it lands whole in that cell; between two centres
    !> it reaches neither and lands whole in the cell holding the point; a kernel reaching the two
    !> centres either side of it splits evenly between them. Near `xmin` the part beyond is counted
    !> there and the rest still lands in the first cell.
    subroutine test_grid_narrow_kernel(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde_grid) :: g
        real(real64) :: f(10), p

        call g%init(10, 0.0_real64, 10.0_real64, 0.01_real64)
        call g%add(2.5_real64)
        call g%finish()
        call g%density(f)
        call check(error, count(f /= 0.0_real64) == 1 .and. abs(f(3) - 1.0_real64) <= 1.0e-15_real64, &
            "a kernel on cell 3's centre must land whole in cell 3")
        if (allocated(error)) return
        call g%clear()
        call g%add(7.0_real64)
        call g%finish()
        call g%density(f)
        call check(error, count(f /= 0.0_real64) == 1 .and. abs(f(8) - 1.0_real64) <= 1.0e-15_real64, &
            "a kernel between two centres must land whole in the cell holding the point")
        if (allocated(error)) return
        call g%init(10, 0.0_real64, 10.0_real64, 0.3_real64, kernel="box")
        call g%add(5.0_real64)
        call g%finish()
        call g%density(f)
        call check(error, count(f /= 0.0_real64) == 2 .and. abs(f(5) - f(6)) <= 1.0e-15_real64 .and. &
            abs(f(5) + f(6) - 1.0_real64) <= 1.0e-15_real64, &
            "a box reaching the centres either side of it must split evenly between them")
        if (allocated(error)) return
        call g%init(10, 0.0_real64, 10.0_real64, 0.01_real64)
        call g%add(0.001_real64)
        call g%finish()
        call g%density(f)
        call g%cdf(0.0_real64, p)
        call check(error, count(f /= 0.0_real64) == 1 .and. p > 0.4_real64 .and. &
            abs(f(1) + p - 1.0_real64) <= 1.0e-15_real64, &
            "a narrow kernel crossing xmin must put the part beyond it below and the rest in cell 1")

    end subroutine test_grid_narrow_kernel

    !> `%pdf` is exact at every centre (the same bits as `%density`), the mean of its neighbours at
    !> a midpoint, constant over the outer half-cells, and zero outside the range.
    subroutine test_grid_pdf_interpolates(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde_grid) :: g
        real(real64), allocatable :: x(:)
        real(real64) :: c(40), f(40), fc(40), m(39), fm(39), a, b, v
        integer :: j

        call kde_fixture(50_int64, x)
        call g%init(40, -700.0_real64, 700.0_real64, 60.0_real64)
        call g%add(x)
        call g%finish()
        call g%density(f, x=c)
        call g%pdf(c, fc)
        call check(error, all(fc == f), "%pdf at a centre must be %density there, to the bit")
        if (allocated(error)) return
        do j = 1, 39
            m(j) = 0.5_real64*(c(j) + c(j + 1))
        end do
        call g%pdf(m, fm)
        call check(error, all(abs(fm - 0.5_real64*(f(1:39) + f(2:40))) <= 1.0e-14_real64*maxval(f)), &
            "%pdf midway between two centres must be their mean")
        if (allocated(error)) return
        call g%pdf(-700.0_real64, a)
        call g%pdf(-700.0_real64 + 0.25_real64*g%step(), b)
        call g%pdf(700.0_real64, v)
        call check(error, a == f(1) .and. b == f(1) .and. v == f(40), &
            "%pdf must be constant over the outer half of the first and the last cell")
        if (allocated(error)) return
        call g%pdf(-701.0_real64, a)
        call g%pdf(701.0_real64, b)
        call g%pdf(ieee_value(1.0_real64, ieee_quiet_nan), v)
        call check(error, a == 0.0_real64 .and. b == 0.0_real64 .and. ieee_is_nan(v), &
            "%pdf must be zero outside the range and NaN at a NaN")

    end subroutine test_grid_pdf_interpolates

    !> `%cdf` at each centre is the running trapezoid of `%density` from `xmin`, a difference inside
    !> one segment is the trapezoid of the linear `%pdf`, and `%quantile` inverts `%cdf` both ways;
    !> `p = 0` and `p = 1` answer where the accumulated density starts and ends inside the range.
    subroutine test_grid_cdf_and_quantile(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde_grid) :: g
        real(real64), allocatable :: x(:)
        real(real64) :: c(64), f(64), p(64), cum, a, b, pa, pb, fa, fb, s
        real(real64) :: pp(99), q(99), back(99), t(50), pt(50), qt(50), q0, q1, p0, p1, f0
        integer :: j

        call kde_fixture(200_int64, x)
        x = x/1000.0_real64
        ! A range cutting through the data, so that the first cell holds weight and the weight
        ! counted below `xmin` starts the running sum.
        call g%init(64, -0.3_real64, 0.3_real64, 0.05_real64)
        call g%add(x)
        call g%finish()
        call g%density(f, x=c)
        s = g%step()
        call g%cdf(c, p)
        call g%cdf(-0.3_real64, cum)
        call check(error, cum > 0.05_real64 .and. f(1) > 0.0_real64, &
            "the fixture must put weight below xmin and in the first cell, or the sum below is vacuous")
        if (allocated(error)) return
        cum = cum + 0.5_real64*f(1)*s
        do j = 1, 64
            if (j > 1) cum = cum + 0.5_real64*(f(j - 1) + f(j))*s
            call check(error, abs(p(j) - cum) <= 1.0e-14_real64, &
                "%cdf at a centre must be the weight below xmin plus the running trapezoid of %density")
            if (allocated(error)) return
        end do
        call g%init(64, -1.0_real64, 1.0_real64, 0.05_real64)
        call g%add(x)
        call g%finish()
        call g%density(f, x=c)
        s = g%step()
        a = c(30) + 0.2_real64*s
        b = c(30) + 0.7_real64*s
        call g%cdf(a, pa)
        call g%cdf(b, pb)
        call g%pdf(a, fa)
        call g%pdf(b, fb)
        call check(error, abs((pb - pa) - (b - a)*0.5_real64*(fa + fb)) <= 1.0e-15_real64, &
            "inside a segment %cdf must integrate the linear %pdf exactly")
        if (allocated(error)) return
        do j = 1, 99
            pp(j) = real(j, real64)/100.0_real64
        end do
        call g%quantile(pp, q)
        call g%cdf(q, back)
        call check(error, all(abs(back - pp) <= 1.0e-14_real64), "%cdf(%quantile(p)) must be p")
        if (allocated(error)) return
        call check(error, all(q(2:99) >= q(1:98)), "%quantile must be monotone in p")
        if (allocated(error)) return
        call spread_points(-0.45_real64, 0.45_real64, t)
        call g%cdf(t, pt)
        call g%quantile(pt, qt)
        call check(error, all(abs(qt - t) <= 1.0e-12_real64), "%quantile(%cdf(x)) must be x where the density is positive")
        if (allocated(error)) return

        ! A grid whose outer cells hold nothing: `p = 0` and `p = 1` are where the density starts and
        ! ends, and `%cdf` is 0 and 1 there.
        call g%init(64, -1.0_real64, 1.0_real64, 0.01_real64, kernel="box")
        call g%add(x)
        call g%finish()
        call g%quantile(0.0_real64, q0)
        call g%quantile(1.0_real64, q1)
        call g%cdf(q0, p0)
        call g%cdf(q1, p1)
        call g%pdf(q0, f0)
        call check(error, q0 > -0.6_real64 .and. q1 < 0.6_real64 .and. p0 == 0.0_real64 .and. f0 == 0.0_real64 &
            .and. abs(p1 - 1.0_real64) <= 1.0e-14_real64, &
            "p = 0 and p = 1 must answer where the density starts and ends inside the range")

    end subroutine test_grid_cdf_and_quantile

    !> `%merge`: a grid over all but the last point merged with a grid over that point is the grid
    !> over them all, to the bit, as is a merge into an empty grid; two halves merged agree with the
    !> whole to rounding (the additions group differently), with every count exact; and a poisoned
    !> grid poisons the merge.
    subroutine test_grid_merge(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde_grid) :: g_all, g1, g2, ge, gp, gb
        real(real64), allocatable :: x(:), w(:), r_all(:), r(:), r1(:), r2(:), zz(:)
        real(real64) :: f(30), fl(100), fl2(100), ends(2), ends_all(2), q_before, q_after, p_low

        call kde_fixture(101_int64, x)
        call kde_weights_mod5(101_int64, w)
        call g_all%init(30, -700.0_real64, 700.0_real64, 80.0_real64)
        call g_all%add(x)
        call g1%init(30, -700.0_real64, 700.0_real64, 80.0_real64)
        call g1%add(x(1:100))
        call g2%init(30, -700.0_real64, 700.0_real64, 80.0_real64)
        call g2%add(x(101:101))
        call g1%merge(g2)
        call g_all%finish()
        call raw_cells(g_all, r_all)
        call g1%finish()
        call raw_cells(g1, r)
        call check(error, all(r == r_all) .and. g1%n() == 101_int64 .and. g1%n_valid() == 101_int64 &
            .and. g1%sum_weights() == g_all%sum_weights(), &
            "a grid over 100 points merged with one over the 101st must be the grid over all 101, to the bit")
        if (allocated(error)) return
        ! The kernels reach past both ends of this range, so the weight counted there merges too.
        call g1%finish()
        call g1%cdf([-700.0_real64, 700.0_real64], ends)
        call g_all%finish()
        call g_all%cdf([-700.0_real64, 700.0_real64], ends_all)
        call check(error, ends_all(1) > 0.0_real64 .and. ends_all(2) < 1.0_real64 .and. all(ends == ends_all), &
            "the weight counted beyond each end must merge, to the bit")
        if (allocated(error)) return
        call ge%init(30, -700.0_real64, 700.0_real64, 80.0_real64)
        call ge%merge(g_all)
        call ge%finish()
        call raw_cells(ge, r)
        call check(error, all(r == r_all), "a merge into an empty grid must copy the other, to the bit")
        if (allocated(error)) return

        ! Under `"linear"` cells can go negative and the clip is applied at QUERY time, so a merge
        ! of two grids and one grid over the concatenation hold the same cells and answer the same
        ! density. A clip at deposit would make the two differ by the clipped mass.
        allocate(zz(101))
        call kde_rising(101, zz)
        call g_all%init(100, 0.0_real64, 1.0_real64, 0.08_real64, lower=0.0_real64, upper=1.0_real64, &
            boundary="linear")
        call g_all%add(zz)
        call g1%init(100, 0.0_real64, 1.0_real64, 0.08_real64, lower=0.0_real64, upper=1.0_real64, &
            boundary="linear")
        call g1%add(zz(1:50))
        call g2%init(100, 0.0_real64, 1.0_real64, 0.08_real64, lower=0.0_real64, upper=1.0_real64, &
            boundary="linear")
        call g2%add(zz(51:101))
        ! Each half is read off a COPY: a query closes a grid, and a closed grid takes no merge.
        gb = g1
        call gb%finish()
        call raw_cells(gb, r1)
        gb = g2
        call gb%finish()
        call raw_cells(gb, r2)
        call g1%merge(g2, finish=.true.)
        call g_all%finish()
        call raw_cells(g_all, r_all)
        call raw_cells(g1, r)
        ! To rounding, not to the bit: the halves group the same additions differently, as the
        ! weighted B-spline case below says. A clip at deposit would not be a rounding difference --
        ! it would drop the negative half of every cell the two halves disagree in sign on.
        call check(error, maxval(abs(r - r_all)) <= 1.0e-14_real64*maxval(abs(r_all)), &
            "a linear grid merged must hold the cells of one grid over both halves, to rounding")
        if (allocated(error)) return
        call check(error, any(r < 0.0_real64) .and. &
            any((r1 > 0.0_real64 .and. r2 < 0.0_real64) .or. (r1 < 0.0_real64 .and. r2 > 0.0_real64)), &
            "precondition: a cell must be negative, and one must take opposite signs from the two halves")
        if (allocated(error)) return
        call g_all%density(fl)
        call g1%density(fl2)
        call check(error, maxval(abs(fl - fl2)) <= 1.0e-14_real64*maxval(fl2) .and. all(fl >= 0.0_real64), &
            "and the two must answer the same clipped density")
        if (allocated(error)) return

        call g_all%init(30, -700.0_real64, 700.0_real64, 50.0_real64, kernel="bspline")
        call g_all%add(x, weights=w)
        call g1%init(30, -700.0_real64, 700.0_real64, 50.0_real64, kernel="bspline")
        call g1%add(x(1:50), weights=w(1:50))
        call g2%init(30, -700.0_real64, 700.0_real64, 50.0_real64, kernel="bspline")
        call g2%add(x(51:101), weights=w(51:101))
        call g1%merge(g2)
        call g_all%finish()
        call raw_cells(g_all, r_all)
        call g1%finish()
        call raw_cells(g1, r)
        call check(error, maxval(abs(r - r_all)) <= 1.0e-14_real64*maxval(r_all), &
            "two weighted halves merged must equal the whole to rounding")
        if (allocated(error)) return
        call check(error, g1%n() == g_all%n() .and. g1%n_valid() == g_all%n_valid() .and. &
            g1%n_null() == g_all%n_null() .and. g1%n_nan() == g_all%n_nan() .and. &
            g1%n_outside() == g_all%n_outside() .and. g1%sum_weights() == g_all%sum_weights(), &
            "every count must merge exactly")
        if (allocated(error)) return

        ! Only the grid merged in has weight above its range, which moves `p = 1` from where the
        ! density ends inside the range to `xmax`.
        call g1%init(10, 0.0_real64, 1.0_real64, 0.02_real64)
        call g1%add(0.5_real64)
        call g2%init(10, 0.0_real64, 1.0_real64, 0.02_real64)
        call g2%add(1.5_real64)
        ! The before-merge answer is read off a COPY: a query closes a grid, and a closed grid
        ! takes no merge. The copy also shows `finish=` on `%merge`, the one-line form.
        gb = g1
        call gb%finish()
        call gb%quantile(1.0_real64, q_before)
        call g1%merge(g2, finish=.true.)
        call g1%quantile(1.0_real64, q_after)
        call check(error, q_before < 1.0_real64 .and. q_after == 1.0_real64, &
            "weight above the range must merge: p = 1 then answers xmax")
        if (allocated(error)) return
        ! And only the grid merged in has weight below it: one point of two, so %cdf(xmin) is 1/2.
        call g1%init(10, 0.0_real64, 1.0_real64, 0.02_real64)
        call g1%add(0.5_real64)
        call g2%init(10, 0.0_real64, 1.0_real64, 0.02_real64)
        call g2%add(-0.5_real64)
        call g1%merge(g2)
        call g1%finish()
        call g1%cdf(0.0_real64, p_low)
        call check(error, p_low == 0.5_real64, "weight below the range must merge: %cdf(xmin) is then 1/2")
        if (allocated(error)) return

        call g1%init(30, -700.0_real64, 700.0_real64, 50.0_real64, kernel="bspline")
        call g1%add(x(1:50), weights=w(1:50))
        call gp%init(30, -700.0_real64, 700.0_real64, 50.0_real64, kernel="bspline")
        call gp%add([1.0_real64, ieee_value(1.0_real64, ieee_quiet_nan)], skipnan=.false.)
        call g1%merge(gp)
        call g1%finish()
        call g1%density(f)
        call check(error, all(ieee_is_nan(f)), "merging a poisoned grid must poison the result")

    end subroutine test_grid_merge

    !> The family's exclusion order on every `%add`, counted per call through the optional outputs
    !> and in total through the accessors; the support's exclusion added. The scalar form equals
    !> the array form, and the `real32` form the `real64` form of the widened values, to the bit.
    subroutine test_grid_population_rules(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde_grid) :: g, g1, g2
        real(real64) :: x(7), w(7), nan, inf
        real(real32) :: x32(5)
        logical :: valid(7)
        integer(int64) :: n_null, n_nan, n_out, want_n, want_null, want_nan, i
        real(real64), allocatable :: r1(:), r2(:)

        nan = ieee_value(1.0_real64, ieee_quiet_nan)
        inf = ieee_value(1.0_real64, ieee_positive_inf)
        x = [1.0_real64, 2.0_real64, nan, 4.0_real64, 5.0_real64, 6.0_real64, 7.5_real64]
        w = [1.0_real64, nan, 0.0_real64, 2.0_real64, 0.0_real64, 1.0_real64, 3.0_real64]
        valid = [.true., .false., .true., .true., .true., .true., .true.]
        call g%init(16, 0.0_real64, 8.0_real64, 0.5_real64, lower=0.0_real64, upper=8.0_real64)
        call g%add(x, is_valid=valid, weights=w, n_null=n_null, n_nan=n_nan, n_outside=n_out)
        call pf_count_valid(x, want_n, is_valid=valid, weights=w, n_null=want_null, n_nan=want_nan)
        call check(error, n_null == want_null .and. n_nan == want_nan .and. n_out == 0_int64 .and. &
            g%n_valid() == want_n .and. g%n() == 7_int64 .and. g%sum_weights() == 7.0_real64, &
            "the exclusion counts must be pf_count_valid's")
        if (allocated(error)) return
        call g%add([-1.0_real64, 3.0_real64, inf], n_null=n_null, n_nan=n_nan, n_outside=n_out)
        call check(error, n_null == 0_int64 .and. n_nan == 0_int64 .and. n_out == 2_int64 .and. &
            g%n() == 10_int64 .and. g%n_valid() == want_n + 1_int64 .and. g%n_outside() == 2_int64 .and. &
            g%n_null() == want_null .and. g%sum_weights() == 8.0_real64, &
            "a second %add must report its own counts and add them to the grid's totals")
        if (allocated(error)) return

        call g1%init(16, 0.0_real64, 8.0_real64, 0.5_real64, kernel="epanechnikov")
        call g2%init(16, 0.0_real64, 8.0_real64, 0.5_real64, kernel="epanechnikov")
        call g1%add([1.0_real64, 2.5_real64, 4.0_real64, 6.25_real64], weights=[1.0_real64, 2.0_real64, &
            0.5_real64, 3.0_real64], is_valid=[.true., .true., .false., .true.])
        call g2%add(1.0_real64, weights=1.0_real64)
        call g2%add(2.5_real64, weights=2.0_real64)
        call g2%add(4.0_real64, weights=0.5_real64, is_valid=.false.)
        call g2%add(6.25_real64, weights=3.0_real64)
        call g1%finish()
        call raw_cells(g1, r1)
        call g2%finish()
        call raw_cells(g2, r2)
        call check(error, all(r1 == r2) .and. g2%n_null() == 1_int64 .and. g2%n() == 4_int64, &
            "adding the points one at a time must equal adding the array, to the bit")
        if (allocated(error)) return

        do i = 1_int64, 5_int64
            x32(i) = real(i, real32)*1.3_real32
        end do
        call g1%init(16, 0.0_real64, 8.0_real64, 0.5_real64)
        call g2%init(16, 0.0_real64, 8.0_real64, 0.5_real64)
        call g1%add(x32)
        call g2%add(real(x32, real64))
        call g1%finish()
        call raw_cells(g1, r1)
        call g2%finish()
        call raw_cells(g2, r2)
        call check(error, all(r1 == r2), "a real32 array must deposit as its widened values do")
        if (allocated(error)) return
        ! Reopened for the scalar form: a query closed both grids, and the comparison is about
        ! the scalar `%add` alone, so it needs no history.
        call g1%clear()
        call g2%clear()
        call g1%add(x32(1), finish=.true.)
        call g2%add(real(x32(1), real64), finish=.true.)
        call raw_cells(g1, r1)
        call raw_cells(g2, r2)
        call check(error, all(r1 == r2) .and. g1%n() == 1_int64, &
            "a real32 point must deposit as its widened value does")

    end subroutine test_grid_population_rules

    !> An initialised grid with nothing in it answers zeros from `%density` and NaN from the
    !> interpolated queries; `skipnan=.false.` with a NaN poisons every later answer, until
    !> `%clear`; an all-null `%add` leaves the grid empty and counted.
    subroutine test_grid_empty_and_poisoned(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde_grid) :: g
        real(real64) :: f(8), pdf, cdf, q

        call g%init(8, 0.0_real64, 8.0_real64, 0.5_real64)
        call g%finish()
        call g%density(f)
        call g%pdf(4.0_real64, pdf)
        call g%cdf(4.0_real64, cdf)
        call g%quantile(0.5_real64, q)
        call check(error, all(f == 0.0_real64) .and. ieee_is_nan(pdf) .and. ieee_is_nan(cdf) .and. &
            ieee_is_nan(q) .and. g%n() == 0_int64, &
            "an empty grid must answer zeros from %density and NaN from its queries")
        if (allocated(error)) return
        ! Querying closed the grid, so each stage below reopens it with `%clear` and accumulates
        ! afresh; the counts are per stage for that reason.
        call g%clear()
        call g%add([1.0_real64, 2.0_real64], is_valid=[.false., .false.], finish=.true.)
        call g%density(f)
        call check(error, all(f == 0.0_real64) .and. g%n_null() == 2_int64 .and. g%n_valid() == 0_int64, &
            "an all-null %add must leave the grid empty and count the nulls")
        if (allocated(error)) return
        call g%clear()
        call g%add([1.0_real64, ieee_value(1.0_real64, ieee_quiet_nan), 2.0_real64], skipnan=.false.)
        call g%add([3.0_real64], finish=.true.)
        call g%density(f)
        call g%pdf(2.0_real64, pdf)
        call check(error, all(ieee_is_nan(f)) .and. ieee_is_nan(pdf) .and. g%n_valid() == 4_int64, &
            "a NaN kept by skipnan=.false. must make every later answer NaN")
        if (allocated(error)) return
        call g%clear()
        call g%add([3.0_real64])
        call g%finish()
        call g%density(f)
        call check(error, .not. any(ieee_is_nan(f)) .and. abs(sum(f) - 1.0_real64) <= 1.0e-15_real64, &
            "%clear must lift the poison")

    end subroutine test_grid_empty_and_poisoned

    !> The accessors report what `%init` was given, tokens folded to lower case; `%clear` keeps the
    !> geometry and empties the grid; a second `%init` replaces everything.
    subroutine test_grid_accessors_and_clear(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde_grid) :: g
        real(real64) :: c(50), lo, hi, f(50)
        character(len=:), allocatable :: name

        call check(error, .not. g%is_initialised(), "a new grid must not be initialised")
        if (allocated(error)) return
        call g%init(50, -1.0_real64, 4.0_real64, 0.2_real64, kernel="EPANECHNIKOV", lower=-1.0_real64, &
            upper=4.0_real64, boundary="Reflect")
        call g%kernel(name)
        call g%bounds(lo, hi)
        call g%grid(c)
        call check(error, g%is_initialised() .and. g%ncells() == 50 .and. g%step() == 0.1_real64 .and. &
            g%bandwidth() == 0.2_real64 .and. name == "epanechnikov" .and. lo == -1.0_real64 .and. &
            hi == 4.0_real64 .and. (.not. g%is_adaptive()) .and. abs(c(1) + 0.95_real64) <= 1.0e-15_real64 .and. &
            abs(c(50) - 3.95_real64) <= 1.0e-15_real64, "the accessors must report the set-up")
        if (allocated(error)) return
        call g%add([0.0_real64, 1.0_real64, 2.0_real64])
        call g%clear()
        call g%finish()
        call g%density(f)
        call check(error, g%is_initialised() .and. g%ncells() == 50 .and. g%bandwidth() == 0.2_real64 .and. &
            g%n() == 0_int64 .and. all(f == 0.0_real64), "%clear must empty the grid and keep its geometry")
        if (allocated(error)) return
        call g%init(5, 0.0_real64, 1.0_real64, 0.5_real64)
        call g%bounds(lo, hi)
        call check(error, g%ncells() == 5 .and. .not. ieee_is_finite(lo) .and. lo < 0.0_real64 .and. &
            .not. ieee_is_finite(hi) .and. hi > 0.0_real64, &
            "a second %init must replace the geometry, and an unbounded support reads as infinite")

    end subroutine test_grid_accessors_and_clear

    !> `%print` writes a heading and its rows; an empty grid adds a line saying so, and an
    !> uninitialised one prints one line rather than aborting.
    subroutine test_grid_print_writes(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde_grid) :: g, fresh
        integer :: u, nlines, i
        logical :: seen
        character(len=11), parameter :: CORRECTIONS(3) = [character(len=11) :: &
            "renormalise", "reflect", "linear"]
        character(len=*), parameter :: PATH = "test_run/kde_grid_print_writes.txt"

        call g%init(10, 0.0_real64, 1.0_real64, 0.1_real64, lower=0.0_real64)
        call g%add([0.2_real64, 0.5_real64])
        open(newunit=u, file=PATH, status="replace", action="write")
        call g%print(unit=u)
        close(u)
        call read_back(PATH, nlines, "sum_weights", seen)
        ! The heading, twelve rows, the lower bound, the correction and the method.
        call check(error, nlines == 17 .and. seen, "%print must write a heading and sixteen rows")
        if (allocated(error)) return
        call g%clear()
        open(newunit=u, file=PATH, status="replace", action="write")
        call g%print(unit=u)
        close(u)
        call read_back(PATH, nlines, "nothing accumulated", seen)
        call check(error, nlines == 18 .and. seen, "an empty grid must say so")
        if (allocated(error)) return
        open(newunit=u, file=PATH, status="replace", action="write")
        call fresh%print(unit=u)
        close(u)
        call read_back(PATH, nlines, "not initialised", seen)
        call check(error, nlines == 1 .and. seen, "an uninitialised grid must print one line saying so")
        if (allocated(error)) return

        ! Every correction names itself, `"linear"` included: a printer that spelled out only the
        ! ones it knew reported a `"linear"` grid as unbounded, which is a sharper mistake than
        ! saying nothing.
        do i = 1, 3
            call g%init(10, 0.0_real64, 1.0_real64, 0.1_real64, lower=0.0_real64, upper=1.0_real64, &
                boundary=trim(CORRECTIONS(i)))
            call g%add([0.2_real64, 0.5_real64], finish=.true.)
            open(newunit=u, file=PATH, status="replace", action="write")
            call g%print(unit=u)
            close(u)
            call read_back(PATH, nlines, trim(CORRECTIONS(i)), seen)
            call check(error, seen, "the grid's %print must name the correction in force: " // &
                trim(CORRECTIONS(i)))
            if (allocated(error)) return
        end do
        ! And the lifecycle, which is what a caller looks for when a query has just refused.
        ! Printed afresh: `read_back` deletes the file it reads.
        open(newunit=u, file=PATH, status="replace", action="write")
        call g%print(unit=u)
        close(u)
        call read_back(PATH, nlines, "finished", seen)
        call check(error, seen, "the grid's %print must say it is finished")
        if (allocated(error)) return
        call g%clear()
        open(newunit=u, file=PATH, status="replace", action="write")
        call g%print(unit=u)
        close(u)
        call read_back(PATH, nlines, "accumulating", seen)
        call check(error, seen, "and that a cleared grid is accumulating again")

    end subroutine test_grid_print_writes

    !> `verbosity = "silent"` silences the grid's `%print` too; the negative control is the same
    !> call writing its rows first. Writes the process-global setting, hence the serial suite.
    subroutine test_grid_print_silenced(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde_grid) :: g
        integer :: u, nlines
        logical :: seen
        character(len=:), allocatable :: saved
        character(len=*), parameter :: PATH = "test_run/kde_grid_print_silenced.txt"

        call g%init(10, 0.0_real64, 1.0_real64, 0.1_real64)
        call g%add([0.2_real64, 0.5_real64])
        open(newunit=u, file=PATH, status="replace", action="write")
        call g%print(unit=u)
        close(u)
        call read_back(PATH, nlines, "bandwidth", seen)
        call check(error, nlines > 0 .and. seen, "the control: %print must write when not silenced")
        if (allocated(error)) return
        call parquet_get_verbosity(saved)
        call parquet_set_verbosity("silent")
        open(newunit=u, file=PATH, status="replace", action="write")
        call g%print(unit=u)
        close(u)
        call parquet_set_verbosity(saved)
        call read_back(PATH, nlines, "bandwidth", seen)
        call check(error, nlines == 0, 'verbosity="silent" must silence the grid''s %print entirely')

    end subroutine test_grid_print_silenced

    !> `verbosity = "silent"` silences `%print` entirely; the negative control is the same call
    !> writing its rows first. Writes the process-global setting, hence the serial suite.
    subroutine test_print_silenced(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde) :: k
        real(real64), allocatable :: x(:)
        integer :: u, nlines
        logical :: seen
        character(len=:), allocatable :: saved
        character(len=*), parameter :: PATH = "test_run/kde_print_silenced.txt"

        call kde_fixture(20_int64, x)
        call k%fit(x, bandwidth=5.0_real64)
        open(newunit=u, file=PATH, status="replace", action="write")
        call k%print(unit=u)
        close(u)
        call read_back(PATH, nlines, "bandwidth", seen)
        call check(error, nlines > 0 .and. seen, "the control: %print must write when not silenced")
        if (allocated(error)) return
        call parquet_get_verbosity(saved)
        call parquet_set_verbosity("silent")
        open(newunit=u, file=PATH, status="replace", action="write")
        call k%print(unit=u)
        close(u)
        call parquet_set_verbosity(saved)
        call read_back(PATH, nlines, "bandwidth", seen)
        call check(error, nlines == 0, 'verbosity="silent" must silence %print entirely')

    end subroutine test_print_silenced

    ! ==========================================================================================
    ! The adaptive kernel
    ! ==========================================================================================

    !> Compares one adaptive golden case: `ok`, the global bandwidth, and the density and CDF at
    !> every probe, the density to `rel_pdf` of its peak and the CDF to `abs_cdf`.
    subroutine check_adaptive_case(error, name, h, pdf, cdf, rel_pdf, abs_cdf, fitted)
        type(error_type), allocatable, intent(out) :: error    !! set on the first failed check
        character(len=*), intent(in)               :: name     !! the case
        real(real64), intent(in)                   :: h        !! the global bandwidth
        real(real64), intent(in)                   :: pdf(NKX) !! the density at `KG_X`
        real(real64), intent(in)                   :: cdf(NKX) !! the CDF at `KG_X`
        real(real64), intent(in)                   :: rel_pdf  !! the density's tolerance, over its peak
        real(real64), intent(in)                   :: abs_cdf  !! the CDF's tolerance
        !> A fit of THIS case already made at the pilot resolution under test. Passing it is what
        !! keeps `test_adaptive_golden_fine` from fitting the 262144-cell pilot twice per case:
        !! its convergence check has to make that fit anyway, and the fit is the whole cost.
        !! Absent, the case is fitted here at whatever pilot resolution is in force.
        type(pf_kde), intent(in), optional         :: fitted
        type(pf_kde) :: k
        logical :: ok
        real(real64) :: got_pdf(NKX), got_cdf(NKX)
        character(len=200) :: msg

        if (present(fitted)) then
            k = fitted
            ok = .true.
        else
            call fit_golden_case(name, k, ok)
        end if
        call check(error, ok .and. k%is_adaptive(), name // ": the adaptive fit must be defined")
        if (allocated(error)) return
        call check(error, close_to(k%bandwidth(), h, 1.0e-13_real64, h), &
            name // ": the global bandwidth must match the oracle")
        if (allocated(error)) return
        call k%pdf(KG_X, got_pdf)
        call k%cdf(KG_X, got_cdf)
        write(msg, '(2a,es10.3,a,es10.3)') name, ": the density misses the oracle by ", &
            maxval(abs(got_pdf - pdf))/maxval(pdf), " of its peak; allowed ", rel_pdf
        call check(error, maxval(abs(got_pdf - pdf)) <= rel_pdf*maxval(pdf), trim(msg))
        if (allocated(error)) return
        write(msg, '(2a,es10.3,a,es10.3)') name, ": the CDF misses the oracle by ", &
            maxval(abs(got_cdf - cdf)), "; allowed ", abs_cdf
        call check(error, maxval(abs(got_cdf - cdf)) <= abs_cdf, trim(msg))

    end subroutine check_adaptive_case

    !> `log g` recomputed from a pilot grid's own cells: the mean of `log p` over its density, as
    !> the adaptive rule defines it, formed here by this test and not by the library.
    function pilot_log_g(g) result(lg)
        type(pf_kde_grid), intent(in) :: g  !! the pilot
        real(real64)                  :: lg !! `log g`
        real(real64), allocatable :: f(:)
        real(real64) :: s1, s2
        integer :: i

        allocate(f(g%ncells()))
        call g%density(f)
        s1 = 0.0_real64
        s2 = 0.0_real64
        do i = 1, size(f)
            if (f(i) > 0.0_real64) then
                s1 = s1 + f(i)
                s2 = s2 + f(i)*log(f(i))
            end if
        end do
        lg = s2/s1

    end function pilot_log_g

    !> `alpha = 0` makes every point's bandwidth the global one, and the adaptive estimate is then
    !> the fixed one BIT FOR BIT -- density, distribution function and quantiles -- unbounded, under
    !> "renormalise", and under "reflect" with a kernel wider than the support, where each point's
    !> mass is divided out; the streaming form's accumulation likewise. `alpha = 0.5` moves the
    !> estimate, so an adaptive path that silently fell back on the fixed one is caught.
    subroutine test_alpha_zero_is_fixed(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde) :: kf, k0, k5
        type(pf_kde_grid) :: pilot, gf, g0
        real(real64), allocatable :: x(:), hb(:), rf(:), r0(:)
        real(real64) :: t(301), ff(301), f0(301), f5(301), cf(301), c0(301), qf(9), q0(9), pp(9)
        integer :: cfg, j
        character(len=12) :: kern
        character(len=100) :: tag

        call kde_two_component(60_int64, x)
        call spread_points(-700.0_real64, 800.0_real64, t)
        do j = 1, 9
            pp(j) = real(j, real64)/10.0_real64
        end do
        do cfg = 1, 5
            select case (cfg)
            case (1)
                tag = "unbounded, Silverman's rule, Gaussian"
                call kf%fit(x, rule="silverman")
                call k0%fit(x, rule="silverman", adaptive=.true., alpha=0.0_real64)
                call k5%fit(x, rule="silverman", adaptive=.true., alpha=0.5_real64)
            case (2)
                tag = "renormalised at a lower bound, B-spline"
                call kf%fit(x, bandwidth=80.0_real64, kernel="bspline", lower=-340.0_real64)
                call k0%fit(x, bandwidth=80.0_real64, kernel="bspline", adaptive=.true., alpha=0.0_real64, &
                    lower=-340.0_real64)
                call k5%fit(x, bandwidth=80.0_real64, kernel="bspline", adaptive=.true., lower=-340.0_real64)
            case (3)
                tag = "reflected at both bounds, Epanechnikov"
                call kf%fit(x, bandwidth=60.0_real64, kernel="epanechnikov", lower=-340.0_real64, &
                    upper=470.0_real64, boundary="reflect")
                call k0%fit(x, bandwidth=60.0_real64, kernel="epanechnikov", adaptive=.true., alpha=0.0_real64, &
                    lower=-340.0_real64, upper=470.0_real64, boundary="reflect")
                call k5%fit(x, bandwidth=60.0_real64, kernel="epanechnikov", adaptive=.true., &
                    lower=-340.0_real64, upper=470.0_real64, boundary="reflect")
            case (4)
                tag = "reflected with the kernel wider than the support"
                call kf%fit(x, bandwidth=400.0_real64, lower=-340.0_real64, upper=470.0_real64, boundary="reflect")
                call k0%fit(x, bandwidth=400.0_real64, adaptive=.true., alpha=0.0_real64, lower=-340.0_real64, &
                    upper=470.0_real64, boundary="reflect")
                call k5%fit(x, bandwidth=400.0_real64, adaptive=.true., lower=-340.0_real64, &
                    upper=470.0_real64, boundary="reflect")
            case default
                ! Under `"linear"` every moment, every zone edge and every per-point integral is
                ! formed at the point's OWN bandwidth, so `alpha = 0` is the strongest statement
                ! that the adaptive path reads the same numbers as the fixed one.
                tag = "the linear correction at a lower bound, Gaussian"
                call kf%fit(x, bandwidth=60.0_real64, lower=-340.0_real64, boundary="linear")
                call k0%fit(x, bandwidth=60.0_real64, adaptive=.true., alpha=0.0_real64, &
                    lower=-340.0_real64, boundary="linear")
                call k5%fit(x, bandwidth=60.0_real64, adaptive=.true., lower=-340.0_real64, &
                    boundary="linear")
            end select
            call kf%pdf(t, ff)
            call k0%pdf(t, f0)
            call k5%pdf(t, f5)
            call kf%cdf(t, cf)
            call k0%cdf(t, c0)
            call kf%quantile(pp, qf)
            call k0%quantile(pp, q0)
            call check(error, all(f0 == ff) .and. all(c0 == cf) .and. all(q0 == qf), &
                trim(tag) // ": alpha = 0 must answer the fixed estimate bit for bit")
            if (allocated(error)) return
            if (allocated(hb)) deallocate(hb)
            allocate(hb(k0%n_valid()))
            call k0%bandwidths(hb)
            call check(error, all(hb == kf%bandwidth()), trim(tag) // ": at alpha = 0 every bandwidth is the global one")
            if (allocated(error)) return
            call check(error, maxval(abs(f5 - ff)) > 1.0e-2_real64*maxval(ff), &
                trim(tag) // ": alpha = 0.5 must move the estimate; the adaptive path has fallen back on the fixed")
            if (allocated(error)) return
        end do

        ! The streaming form: a pilot at alpha = 0 deposits every point as the fixed grid does.
        do cfg = 1, 2
            kern = "gaussian"
            if (cfg == 2) kern = "bspline"
            call pilot%init(64, -700.0_real64, 800.0_real64, 60.0_real64, kernel=kern)
            call pilot%add(x)
            call gf%init(150, -650.0_real64, 750.0_real64, 50.0_real64, kernel=kern)
            call pilot%finish()
            call g0%init(150, -650.0_real64, 750.0_real64, 50.0_real64, kernel=kern, pilot=pilot, &
                alpha=0.0_real64)
            call gf%add(x)
            call g0%add(x)
            call gf%finish()
            call raw_cells(gf, rf)
            call g0%finish()
            call raw_cells(g0, r0)
            call gf%finish()
            call gf%cdf(t, cf)
            call g0%finish()
            call g0%cdf(t, c0)
            call check(error, g0%is_adaptive() .and. all(r0 == rf) .and. all(c0 == cf), &
                trim(kern) // ": a grid whose pilot rule has alpha = 0 must accumulate the fixed grid's bits")
            if (allocated(error)) return
        end do

    end subroutine test_alpha_zero_is_fixed

    !> A bandwidth rule answers the question the FIXED estimator asks. The adaptive kernel's own
    !! best global bandwidth is larger, so an adaptive fit that was given a rule rather than a
    !! number widens the rule's answer by `KDE_ADAPT_INFLATE**(2*alpha)`.
    !!
    !! Four things are asserted, and each of them is a way the change could go wrong:
    !! the factor is exactly one at `alpha = 0`, where the adaptive estimate IS the fixed one;
    !! it grows with `alpha`; an explicit `bandwidth=` is NOT inflated, which is what keeps
    !! "the bandwidth I asked for" meaning that; and `adjust=` composes multiplicatively with it
    !! rather than replacing it.
    !!
    !! The factor is re-derived here from the ratio at one `alpha` rather than from the library's
    !! own constant, so that the SHAPE of the rule -- a power of `2*alpha` -- is what is pinned. A
    !! constant that moves changes every number here together and still passes, which is correct:
    !! the constant is a measurement (`bench/benchmark_kde.sh`, `mise` mode), not a contract.
    subroutine test_adaptive_inflates_a_rule(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde) :: kf, ka
        real(real64), allocatable :: x(:)
        real(real64) :: hfixed, h0, h5, h3, c, want
        integer :: i

        call kde_two_component(60_int64, x)
        call kf%fit(x, rule="silverman")
        hfixed = kf%bandwidth()

        ! alpha = 0: the adaptive estimate is the fixed one, so nothing may be added to it.
        call ka%fit(x, rule="silverman", adaptive=.true., alpha=0.0_real64)
        h0 = ka%bandwidth()
        call check(error, h0 == hfixed, "at alpha=0 the adaptive kernel must take the rule's bandwidth unchanged")
        if (allocated(error)) return

        ! alpha = 0.5: the factor is C itself, which fixes C from this one ratio.
        call ka%fit(x, rule="silverman", adaptive=.true., alpha=0.5_real64)
        h5 = ka%bandwidth()
        c = h5/hfixed
        call check(error, c > 1.0_real64, "an adaptive fit given a rule must widen it")
        if (allocated(error)) return

        ! alpha = 0.3: the same C, raised to 2*alpha. A factor that were linear in alpha, or applied
        ! whole whatever alpha is, fails here and nowhere above.
        call ka%fit(x, rule="silverman", adaptive=.true., alpha=0.3_real64)
        h3 = ka%bandwidth()
        want = hfixed*c**(2.0_real64*0.3_real64)
        call check(error, abs(h3 - want) <= 1.0e-12_real64*want, &
            "the factor must be C**(2*alpha): the ratio at alpha=0.3 must follow from the one at 0.5")
        if (allocated(error)) return

        ! An explicit bandwidth is the caller's own number and is never inflated.
        call ka%fit(x, bandwidth=60.0_real64, adaptive=.true.)
        call check(error, ka%bandwidth() == 60.0_real64, &
            "an explicit bandwidth= must reach the adaptive fit unchanged")
        if (allocated(error)) return

        ! adjust= composes with the factor rather than replacing it.
        call ka%fit(x, rule="silverman", adaptive=.true., alpha=0.5_real64, adjust=2.0_real64)
        call check(error, abs(ka%bandwidth() - 2.0_real64*h5) <= 1.0e-12_real64*h5, &
            "adjust= must multiply the inflated bandwidth, not stand in for the inflation")
        if (allocated(error)) return

        ! The per-point bandwidths move with the global one, so the inflation is not a number the
        ! accessor reports and the estimate ignores.
        i = int(kf%n_valid())
        block
            real(real64), allocatable :: hb(:)
            allocate(hb(i))
            call ka%fit(x, rule="silverman", adaptive=.true., alpha=0.5_real64)
            call ka%bandwidths(hb)
            call check(error, minval(hb) > 0.0_real64 .and. maxval(hb) > hfixed, &
                "every point's bandwidth must be built from the inflated global bandwidth")
        end block

    end subroutine test_adaptive_inflates_a_rule

    !> On the two-component recipe the rule narrows the kernels in the dense cluster and widens
    !> them in the sparse one: every bandwidth in the narrow cluster is below the global one and
    !> every one in the wide cluster above it, and the gap between the clusters, where the pilot is
    !> thinnest, gets a wider bandwidth still. A sign error on `alpha`, or a `g` off by much, turns
    !> one of the three around.
    subroutine test_adaptive_bandwidths_follow_pilot(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde) :: k
        real(real64), allocatable :: x(:), hb(:), xs(:)
        real(real64) :: h, h_gap
        character(len=200) :: msg

        call kde_two_component(60_int64, x)
        call k%fit(x, kernel="gaussian", rule="silverman", adaptive=.true.)
        h = k%bandwidth()
        allocate(hb(k%n_valid()), xs(k%n_valid()))
        call k%bandwidths(hb, xs)
        call check(error, count(xs < 0.0_real64) == 40 .and. count(xs > 0.0_real64) == 20, &
            "the fixture must hold forty points in the narrow cluster and twenty in the wide one")
        if (allocated(error)) return
        write(msg, '(a,f8.3,a,f8.3)') "the narrow cluster's widest bandwidth over the global one is ", &
            maxval(hb, mask=xs < 0.0_real64)/h, "; the wide cluster's narrowest ", minval(hb, mask=xs > 0.0_real64)/h
        call check(error, maxval(hb, mask=xs < 0.0_real64) < h .and. minval(hb, mask=xs > 0.0_real64) > h, trim(msg))
        if (allocated(error)) return
        call k%bandwidth_at(0.0_real64, h_gap)
        call check(error, h_gap > maxval(hb), "the gap between the clusters must get the widest bandwidth of all")

    end subroutine test_adaptive_bandwidths_follow_pilot

    !> The four adaptive golden cases, at the rule's own pilot: the library reads a pilot grid a
    !> quarter of a bandwidth to the cell, whose interpolation errs by about `(step/h)**2/8` of the
    !> pilot, a bandwidth by `alpha` times that, so the estimate agrees with the exact pilot's to
    !> about `2e-3` of its peak -- asserted at `5e-3`, and the CDF at `5e-4`. `kde_serial`'s fine
    !> case shows the gap is that discretisation and nothing else.
    subroutine test_adaptive_golden(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check

        call check_adaptive_case(error, "ADAPT", KG_ADAPT_H, KG_ADAPT_PDF, KG_ADAPT_CDF, 5.0e-3_real64, &
            5.0e-4_real64)
        if (allocated(error)) return
        call check_adaptive_case(error, "ADAPT_CAP", KG_ADAPT_CAP_H, KG_ADAPT_CAP_PDF, KG_ADAPT_CAP_CDF, &
            5.0e-3_real64, 5.0e-4_real64)
        if (allocated(error)) return
        call check_adaptive_case(error, "ADAPT_REF", KG_ADAPT_REF_H, KG_ADAPT_REF_PDF, KG_ADAPT_REF_CDF, &
            5.0e-3_real64, 5.0e-4_real64)
        if (allocated(error)) return
        call check_adaptive_case(error, "ADAPT_W", KG_ADAPT_W_H, KG_ADAPT_W_PDF, KG_ADAPT_W_CDF, 5.0e-3_real64, &
            5.0e-4_real64)

    end subroutine test_adaptive_golden

    !> `bandwidth_max` caps every point's bandwidth and touches no other: where the uncapped rule
    !> stays below the cap the two fits agree bit for bit, everywhere else the capped one is the cap
    !> exactly, and the estimates then differ. A point the pilot reads as zero takes the cap itself.
    !>
    !> **The multiplier is a property of THIS fixture and has to be re-probed when the adaptive
    !> kernel's global bandwidth moves.** The cap is a multiple of the fit's own bandwidth, so it
    !> scales with it; what does not scale is the SPREAD of `h_j/h`, which narrows as the global
    !> bandwidth widens, because the pilot built at that bandwidth is smoother. On this recipe the
    !> spread is about `[0.72, 1.19]`, so a cap at `1.05` binds on a third of the points and the
    !> vacuity guard below has room on both sides; at `1.2` it binds on none and the guard fires.
    !> `spread_max` caps the SPREAD rather than the bandwidth, and where both caps are given the
    !! tighter one binds.
    !!
    !! The cap is `spread_max` times the NARROWEST bandwidth the rule can give, which is reached at
    !! the pilot's densest cell -- a closed form, so the rule needs no second pass to apply it. The
    !! test re-derives that number from the pilot rather than reading it back off the bandwidths,
    !! which would only assert the cap against itself.
    !!
    !! The default cap is released with `spread_max=1e6` rather than measured by omitting it: the
    !! default is in force whether it is named or not, so there is no uncapped fit to compare
    !! against, and a fixture whose spread exceeds the default is what makes the comparison mean
    !! anything.
    subroutine test_spread_max_caps(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde) :: k, kc, kb, kr
        type(pf_kde_grid) :: g
        real(real64), allocatable :: x(:), h0(:), hc(:), hbb(:), hr(:), f(:)
        real(real64) :: capv, want, lg, hmin, smax
        integer :: i

        call kde_two_component(60_int64, x)
        ! The default cap does not bind on this fixture, which is what makes the comparisons below
        ! comparisons against the RULE's own bandwidths rather than against another cap. Asserted
        ! rather than assumed: `spread_max=1e6` releases the cap, and the two must agree bit for
        ! bit.
        call k%fit(x, rule="silverman", adaptive=.true., spread_max=1.0e6_real64)
        allocate(h0(k%n_valid()), hr(k%n_valid()))
        call k%bandwidths(h0)
        call kr%fit(x, rule="silverman", adaptive=.true.)
        call kr%bandwidths(hr)
        call check(error, all(h0 == hr), &
            "the default cap must not bind on this fixture, or the comparisons below are against a cap")
        if (allocated(error)) return

        ! The NARROWEST bandwidth the rule can give, in closed form from the pilot the fit hands
        ! back: `h * (pmax/g)**(-alpha)`, reached at the pilot's densest cell. The spread cap is
        ! `spread_max` times it, so a `spread_max` is chosen here that lands the cap in the middle
        ! of the bandwidths the rule actually produced -- a cap outside their range would bind on
        ! all of them or on none, and assert nothing either way.
        call k%pilot(g)
        allocate(f(g%ncells()))
        call g%density(f)
        lg = pilot_log_g(g)
        hmin = k%bandwidth()*exp(-0.5_real64*(log(maxval(f)) - lg))
        smax = 0.5_real64*(minval(h0) + maxval(h0))/hmin
        call check(error, smax > 1.0_real64, "the chosen spread cap must be a cap at all")
        if (allocated(error)) return

        call kc%fit(x, rule="silverman", adaptive=.true., spread_max=smax)
        allocate(hc(kc%n_valid()))
        call kc%bandwidths(hc)
        call check(error, kc%bandwidth() == k%bandwidth(), "a cap must not move the global bandwidth")
        if (allocated(error)) return
        want = smax*hmin
        capv = maxval(hc)
        call check(error, abs(capv - want) <= 1.0e-12_real64*want, &
            "the spread cap must be spread_max times the bandwidth at the pilot's densest cell")
        if (allocated(error)) return
        call check(error, count(h0 > capv) >= 3 .and. count(h0 < capv) >= 3, &
            "the cap must bind on some points and not on others, or this test is vacuous")
        if (allocated(error)) return
        do i = 1, size(h0)
            if (h0(i) < capv) then
                call check(error, hc(i) == h0(i), "a bandwidth below the spread cap must be left as it is")
            else
                call check(error, hc(i) == capv, "a bandwidth above the spread cap must be the cap exactly")
            end if
            if (allocated(error)) return
        end do

        ! Both caps: neither overrides the other, the smaller binds.
        allocate(hbb(kc%n_valid()))
        call kb%fit(x, rule="silverman", adaptive=.true., spread_max=smax, &
            bandwidth_max=0.5_real64*capv)
        call kb%bandwidths(hbb)
        call check(error, maxval(hbb) == 0.5_real64*capv, &
            "with both caps given, an absolute cap below the spread cap must bind")
        if (allocated(error)) return
        call kb%fit(x, rule="silverman", adaptive=.true., spread_max=smax, &
            bandwidth_max=2.0_real64*capv)
        call kb%bandwidths(hbb)
        call check(error, maxval(hbb) == capv, &
            "with both caps given, a spread cap below the absolute one must bind")

    end subroutine test_spread_max_caps

    !> The grid-assisted boundary scan answers what the exact scan answers.
    !!
    !! Under `boundary="linear"` the fit scans each zone for the stretches where the raw estimate
    !! is negative, which the clip removes. The scan reads a binned grid wherever that grid is
    !! safely away from zero and the exact estimator everywhere else, so what it changes is WHERE
    !! the exact estimator is asked. The only way to show that from inside one process is to run
    !! both arms: `parquet_debug_set_kde_scan_grid(.false.)` turns the grid off, and the two fits
    !! must agree.
    !!
    !! The tolerance is not zero and cannot be. A crossing is bisected to neighbouring numbers from
    !! whichever bracket the scan hands it, and the two arms sample at different points, so a
    !! stretch's ends can differ in their last bits and the normalising mass with them. What must
    !! not differ is which stretches were found at all, which a loose tolerance still sees: a
    !! missed stretch leaves a visible bump of negative density behind.
    !!
    !! It asserts the grid arm actually used the grid first. A run in which the grid was declined
    !! would compare the exact scan with itself and pass while testing nothing.
    subroutine test_scan_grid_agrees(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde) :: kg, ke
        real(real64), allocatable :: x(:)
        real(real64) :: t(120), fg(120), fe(120), cg(120), ce(120), peak
        integer(int64) :: steps, exact, steps_e, exact_e, steps4, exact4, steps4_e, exact4_e
        integer :: i

        ! A density that vanishes at its lower bound, which is what makes the adaptive rule widen
        ! there and the corrected zone cover the support: the case the grid exists for.
        allocate(x(2000))
        do i = 1, 2000
            x(i) = 2.0_real64*sqrt((real(i, real64) - 0.5_real64)/2000.0_real64)
        end do
        call spread_points(0.0_real64, 2.0_real64, t)

        call parquet_debug_set_kde_scan_grid(.true.)
        call kg%fit(x, rule="silverman", kernel="bspline", adaptive=.true., lower=0.0_real64, &
            upper=2.0_real64, boundary="linear")
        call parquet_debug_kde_scan_counts(steps, exact)
        call kg%pdf(t, fg)
        call kg%cdf(t, cg)

        call parquet_debug_set_kde_scan_grid(.false.)
        call ke%fit(x, rule="silverman", kernel="bspline", adaptive=.true., lower=0.0_real64, &
            upper=2.0_real64, boundary="linear")
        call parquet_debug_kde_scan_counts(steps_e, exact_e)
        call ke%pdf(t, fe)
        call ke%cdf(t, ce)
        call parquet_debug_set_kde_scan_grid(.true.)

        call check(error, steps > 0_int64 .and. exact < steps, &
            "the grid arm must have read its grid for some samples, or this test compares the " // &
            "exact scan with itself")
        if (allocated(error)) return
        call check(error, steps_e > 0_int64 .and. exact_e == steps_e, &
            "the control arm must have read every sample exactly")
        if (allocated(error)) return
        call check(error, kg%bandwidth() == ke%bandwidth(), "both arms must fit the same bandwidth")
        if (allocated(error)) return
        peak = maxval(fe)
        call check(error, peak > 0.0_real64, "the control fit must have a density to compare against")
        if (allocated(error)) return
        call check(error, maxval(abs(fg - fe)) <= 1.0e-9_real64*peak, &
            "the grid-assisted scan must answer the exact scan's density")
        if (allocated(error)) return
        call check(error, maxval(abs(cg - ce)) <= 1.0e-9_real64, &
            "the grid-assisted scan must answer the exact scan's distribution")
        if (allocated(error)) return

        ! ---- and that the grid's whole point survives: the scan stops growing with the sample ----
        ! The assertions above would all hold of a grid that fired on a handful of samples and left
        ! the rest to the exact estimator, which is most of the quadratic back. What forbids that is
        ! how the sample count SCALES. The grid lays a uniform spacing of `h_min/KDE_LINEAR_SCAN_PER_H`,
        ! and `h_min` follows the rule as `n**(-1/5)`, so four times the points asks for about
        ! `4**0.2 = 1.32` times the samples; the exact scan stops at every kernel start instead, so
        ! its count follows the POINTS. Both arms are measured, because a count that stayed flat on
        ! a fixture the exact scan also found flat would say nothing.
        deallocate(x)
        allocate(x(8000))
        do i = 1, 8000
            x(i) = 2.0_real64*sqrt((real(i, real64) - 0.5_real64)/8000.0_real64)
        end do
        call parquet_debug_set_kde_scan_grid(.true.)
        call kg%fit(x, rule="silverman", kernel="bspline", adaptive=.true., lower=0.0_real64, &
            upper=2.0_real64, boundary="linear")
        call parquet_debug_kde_scan_counts(steps4, exact4)
        call parquet_debug_set_kde_scan_grid(.false.)
        call ke%fit(x, rule="silverman", kernel="bspline", adaptive=.true., lower=0.0_real64, &
            upper=2.0_real64, boundary="linear")
        call parquet_debug_kde_scan_counts(steps4_e, exact4_e)
        call parquet_debug_set_kde_scan_grid(.true.)

        call check(error, steps4 > 0_int64 .and. steps4_e > 0_int64, &
            "both arms must have scanned at the larger size too")
        if (allocated(error)) return
        call check(error, steps4 < 2_int64*steps, &
            "four times the sample must not double the grid arm's scan: its spacing follows the " // &
            "narrowest bandwidth, not the points")
        if (allocated(error)) return
        call check(error, steps4_e >= 2_int64*steps_e, &
            "the control arm's scan must grow with the points, or the fixture is not one the " // &
            "grid is saving anything on and the assertion above holds for the wrong reason")
        if (allocated(error)) return
        call check(error, 4_int64*exact4 <= steps4, &
            "the grid must still decide the great majority of its samples at the larger size")

    end subroutine test_scan_grid_agrees

    !> `%fit(method="binned")` answers from a grid of its own, and answers the estimate the exact
    !! sum answers to the binning's own accuracy.
    !!
    !! The two are DIFFERENT estimators, which is why `method=` is opt-in: the binned one answers
    !! the estimate of a sample whose points have been moved to the cell centres, converging to the
    !! exact one as the SQUARE of the cell width. `%fit` gives its grid `KDE_CURVE_BINNED_PER_H`
    !! cells to the narrowest kernel, so the gap is a small share of the peak rather than zero, and
    !! every assertion here is written against a share of the peak rather than an absolute
    !! tolerance.
    !!
    !! The fixture is a density that VANISHES at its lower bound, fitted with an adaptive kernel
    !! under `boundary="linear"`: the case Design B exists for -- where the exact estimator pays
    !! for a boundary scan over the whole support -- and the one where the two routes would most
    !! easily disagree, since the correction is carried by the cells on one side and by the zones'
    !! tables on the other.
    !!
    !! `%method` is asserted on both objects FIRST. Without it a run in which `method=` was
    !! silently ignored would compare the exact fit with itself and pass while testing nothing.
    !! The bandwidths are asserted equal for the same reason from the other side: both routes read
    !! the same rule, so a binned fit that rebuilt its own would be answering a different estimate
    !! for a reason this test could not see.
    subroutine test_fit_binned_method(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde) :: kb, ke
        real(real64), allocatable :: x(:), hb(:), he(:)
        real(real64) :: t(120), fb(120), fe(120), cb(120), ce(120)
        real(real64) :: cx(64), cf(64), cp(64), draws(32)
        real(real64) :: peak, qb, qe
        character(len=:), allocatable :: mb, me
        integer :: i
        logical :: okb, oke

        ! f(x) = x/2 on [0, 2], by inversion: zero at the lower bound, so the adaptive rule widens
        ! there and the corrected zone spreads across the support.
        allocate(x(4000))
        do i = 1, 4000
            x(i) = 2.0_real64*sqrt((real(i, real64) - 0.5_real64)/4000.0_real64)
        end do
        call spread_points(0.0_real64, 2.0_real64, t)

        call ke%fit(x, rule="silverman", adaptive=.true., lower=0.0_real64, upper=2.0_real64, &
            boundary="linear", ok=oke)
        call kb%fit(x, rule="silverman", adaptive=.true., lower=0.0_real64, upper=2.0_real64, &
            boundary="linear", method="binned", ok=okb)
        call ke%method(me)
        call kb%method(mb)

        call check(error, me == "exact", "a fit with no method= must answer %method = exact")
        if (allocated(error)) return
        call check(error, mb == "binned", 'method="binned" must reach %method, or this test ' // &
            "compares the exact fit with itself")
        if (allocated(error)) return
        call check(error, oke .and. okb, "both fits must be defined")
        if (allocated(error)) return
        call check(error, kb%bandwidth() == ke%bandwidth(), &
            "both methods must resolve the same global bandwidth")
        if (allocated(error)) return
        allocate(hb(kb%n_valid()), he(ke%n_valid()))
        call kb%bandwidths(hb)
        call ke%bandwidths(he)
        call check(error, size(hb) == size(he), "both methods must retain the same points")
        if (allocated(error)) return
        call check(error, maxval(abs(hb - he)) == 0.0_real64, &
            "the binned fit must bin at the bandwidths the rule gave, not at its own")
        if (allocated(error)) return

        call ke%pdf(t, fe)
        call kb%pdf(t, fb)
        peak = maxval(fe)
        call check(error, peak > 0.0_real64, "the exact fit must have a density to compare against")
        if (allocated(error)) return
        call check(error, maxval(abs(fb - fe)) <= 0.02_real64*peak, &
            "the binned fit's density must follow the exact one to the binning's accuracy")
        if (allocated(error)) return
        call ke%cdf(t, ce)
        call kb%cdf(t, cb)
        call check(error, maxval(abs(cb - ce)) <= 0.01_real64, &
            "the binned fit's distribution must follow the exact one")
        if (allocated(error)) return
        call ke%quantile(0.5_real64, qe)
        call kb%quantile(0.5_real64, qb)
        call check(error, abs(qb - qe) <= 0.02_real64, &
            "the binned fit's median must follow the exact one")
        if (allocated(error)) return

        ! A curve of a binned fit is its grid read at the curve's points, so it must agree with
        ! that fit's own `%pdf` exactly -- not to a tolerance, since it is the same call.
        call kb%curve(cx, cf, xmin=0.1_real64, xmax=1.9_real64)
        call kb%pdf(cx, cp)
        call check(error, maxval(abs(cf - cp)) == 0.0_real64, &
            "a binned fit's %curve must be its own %pdf at those points")
        if (allocated(error)) return
        ! The grid draws by inverting the density it holds, so every draw lies inside the support.
        call kb%sample(draws, 20260921_int64)
        call check(error, all(draws >= 0.0_real64) .and. all(draws <= 2.0_real64), &
            "a binned fit's draws must lie inside the support")

    end subroutine test_fit_binned_method

    !> The fixed arm's LSCV criterion, scored by one transform over the binned sample and by the
    !! double sum over every pair, chooses the same bandwidth.
    !!
    !! The two are not the same number. The transform route scores the sample BINNED onto cells a
    !! sixty-fourth of the bracket's narrowest candidate wide, so its criterion differs from the
    !! exact one by the binning's error, and the golden section can then split a late bracket the
    !! other way. What must hold is that the two land within the search's own resolution of each
    !! other -- `KDE_LSCV_TOL`, a per cent -- because a criterion that ranked bandwidths
    !! differently would not.
    !!
    !! `parquet_debug_set_kde_lscv_grid(.false.)` is the control arm, and the only way from inside
    !! one process to compare them. It writes process-global state, so this test lives in the
    !! suite that runs serially.
    !!
    !! It asserts both arms found a bandwidth first: two NaNs compare equal to nothing, and a run
    !! in which neither route ran would otherwise pass while testing nothing.
    subroutine test_lscv_transform_agrees(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        real(real64), allocatable :: x(:)
        real(real64) :: hg, he
        integer :: i
        logical :: okg, oke

        allocate(x(3000))
        do i = 1, 3000
            x(i) = 2.0_real64*sqrt((real(i, real64) - 0.5_real64)/3000.0_real64)
        end do

        call parquet_debug_set_kde_lscv_grid(.true.)
        call pf_kde_bandwidth(x, hg, rule="lscv", ok=okg)
        call parquet_debug_set_kde_lscv_grid(.false.)
        call pf_kde_bandwidth(x, he, rule="lscv", ok=oke)
        call parquet_debug_set_kde_lscv_grid(.true.)

        call check(error, okg .and. oke, "both routes must resolve a bandwidth")
        if (allocated(error)) return
        call check(error, he > 0.0_real64, "the control arm must give a positive bandwidth")
        if (allocated(error)) return
        call check(error, abs(hg - he) <= 1.0e-2_real64*he, &
            "the transform route must choose the bandwidth the pair sum chooses")

    end subroutine test_lscv_transform_agrees

    !> The least-squares cross-validation CRITERION against the golden oracle, at six bandwidths
    !! over three populations, by both routes.
    !!
    !! **This is the pin on `rule="lscv"`'s mathematics; nothing pins its minimiser to a vector,
    !! and that is deliberate.** The rule's answer is where a golden section stops, so an oracle
    !! for it would have to reproduce the section, the bracket, the subsample's draw and
    !! `KDE_LSCV_TOL` -- it would fail on a change to any of those four while saying nothing about
    !! the criterion they search. So the criterion is certified at fifty digits here and the
    !! bandwidth is left to `test_lscv_rule` and `test_lscv_transform_agrees`. A change that moves
    !! what the criterion IS fails this test; a change that moves only where the search stops does
    !! not.
    !!
    !! The three populations answer three different questions: the plain recipe pins the
    !! normalisers, the combined width `sqrt(h**2 + h**2)` of the integral term and the
    !! `sum(w) - 1` of the leave-one-out term; the weighted one pins that both terms carry the
    !! pair's weight product and that the denominators are weight totals and not counts; the
    !! rounded one repeats its values, so it pins that the leave-one-out term omits the point's own
    !! INDEX and not every point sharing its value.
    !!
    !! Both routes are read at every point. The pair sum is the definition and is held to a tight
    !! tolerance; the transform route bins the sample first, so it is held to the binning's own
    !! error -- loosely, but not so loosely that a route computing a different functional would
    !! pass. It asserts the two routes differ somewhere, since a run in which the transform was
    !! declined everywhere would compare the pair sum with itself and pass while testing nothing.
    !!
    !! It writes process-global state (`parquet_debug_set_kde_lscv_grid`), so it lives in the
    !! suite that runs serially.
    subroutine test_lscv_criterion_golden(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        real(real64), allocatable :: x(:), w(:)
        real(real64) :: ce, cg, want, worst_e, worst_g, spread
        integer(int64) :: n
        integer :: c, i
        logical :: oke, okg

        worst_e = 0.0_real64
        worst_g = 0.0_real64
        spread = 0.0_real64
        do c = 1, 3
            n = 32_int64
            if (c == 3) n = 60_int64
            if (allocated(x)) deallocate(x)
            if (allocated(w)) deallocate(w)
            allocate(x(n), w(n))
            select case (c)
            case (1)
                call kde_fixture(n, x)
                w = 1.0_real64
            case (2)
                call kde_fixture(n, x)
                call kde_weights_mod5(n, w)
            case (3)
                call kde_two_component(n, x)
                call kde_rounded(8.0_real64, x)
                w = 1.0_real64
            end select
            do i = 1, NKH
                select case (c)
                case (1)
                    want = KG_LSCV_CRIT(i)
                case (2)
                    want = KG_LSCV_W_CRIT(i)
                case (3)
                    want = KG_LSCV_DUP_CRIT(i)
                end select

                ! ---- the pair sum: the criterion's definition ----
                call parquet_debug_set_kde_lscv_grid(.false.)
                call parquet_debug_kde_lscv_at(x, KG_LSCV_H(i), ce, oke, weights=w)
                ! ---- the transform route, over the same points ----
                call parquet_debug_set_kde_lscv_grid(.true.)
                call parquet_debug_kde_lscv_at(x, KG_LSCV_H(i), cg, okg, weights=w)

                call check(error, oke .and. okg, "both routes must form the criterion")
                if (allocated(error)) return
                call check(error, want < 0.0_real64, &
                    "the golden criterion must be negative here, or the case has drifted")
                if (allocated(error)) return
                worst_e = max(worst_e, abs(ce - want)/abs(want))
                worst_g = max(worst_g, abs(cg - want)/abs(want))
                spread = max(spread, abs(cg - ce)/abs(want))
            end do
        end do
        call parquet_debug_set_kde_lscv_grid(.true.)

        call check(error, worst_e <= 1.0e-12_real64, &
            "the pair sum must reproduce the golden criterion: it IS the criterion's definition")
        if (allocated(error)) return
        call check(error, worst_g <= 1.0e-3_real64, &
            "the transform route must score the same criterion to within its binning's error")
        if (allocated(error)) return
        call check(error, spread > 0.0_real64, &
            "the two routes must differ somewhere, or the transform was declined everywhere and " // &
            "this test compared the pair sum with itself")

    end subroutine test_lscv_criterion_golden

    subroutine test_bandwidth_max_caps(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde) :: k, kc
        real(real64), allocatable :: x(:), h0(:), hc(:)
        real(real64) :: cap, f0(50), fc(50), t(50), far
        integer :: i

        call kde_two_component(60_int64, x)
        call k%fit(x, rule="silverman", adaptive=.true.)
        cap = 1.05_real64*k%bandwidth()
        call kc%fit(x, rule="silverman", adaptive=.true., bandwidth_max=cap)
        allocate(h0(k%n_valid()), hc(kc%n_valid()))
        call k%bandwidths(h0)
        call kc%bandwidths(hc)
        call check(error, count(h0 > cap) >= 3 .and. count(h0 < cap) >= 3, &
            "the cap must bind on some points and not on others, or this test is vacuous")
        if (allocated(error)) return
        do i = 1, size(h0)
            if (h0(i) < cap) then
                call check(error, hc(i) == h0(i), "a bandwidth below the cap must be left as it is")
            else
                call check(error, hc(i) == cap, "a bandwidth above the cap must be the cap exactly")
            end if
            if (allocated(error)) return
        end do
        call spread_points(-400.0_real64, 500.0_real64, t)
        call k%pdf(t, f0)
        call kc%pdf(t, fc)
        call check(error, maxval(abs(fc - f0)) > 1.0e-3_real64*maxval(f0), "the cap must change the estimate")
        if (allocated(error)) return
        call kc%bandwidth_at(1.0e4_real64, far)
        call check(error, far == cap, "a point the pilot reads as zero must take the cap")

    end subroutine test_bandwidth_max_caps

    !> `%pilot` hands back the grid the fit read: at the global bandwidth, over the survivors, from
    !> four bandwidths below the lowest point to four above the highest, with a quarter of a
    !> bandwidth to the cell. Every bandwidth `%bandwidths` reports is recomputed from it by this
    !> test -- the pilot interpolated at the point, `log g` its mean `log` density -- and agrees;
    !> so does the stand-in for a point the pilot reads as zero, its smallest positive density.
    subroutine test_pilot_is_the_fits(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde) :: k, kempty
        type(pf_kde_grid) :: g
        real(real64), allocatable :: x(:), hb(:), xs(:), c(:), f(:)
        real(real64) :: h, lg, p, want, far, pmax, a, b, empty(0)
        integer :: j, nc

        call kde_two_component(60_int64, x)
        call k%fit(x, bandwidth=40.0_real64, adaptive=.true.)
        h = k%bandwidth()
        call k%pilot(g)
        allocate(hb(k%n_valid()), xs(k%n_valid()))
        call k%bandwidths(hb, xs)
        ! Its own check first: every accessor below aborts on a grid never initialised.
        call check(error, g%is_initialised(), "an adaptive fit's %pilot must hand back an initialised grid")
        if (allocated(error)) return
        call check(error, .not. g%is_adaptive() .and. g%bandwidth() == h .and. &
            g%n_valid() == k%n_valid() .and. g%sum_weights() == k%sum_weights(), &
            "the pilot must be a fixed grid at the global bandwidth over the survivors")
        if (allocated(error)) return
        a = xs(1) - 4.0_real64*h
        b = xs(size(xs)) + 4.0_real64*h
        nc = ceiling(4.0_real64*((b - a)/h))
        allocate(c(g%ncells()), f(g%ncells()))
        call g%grid(c)
        call check(error, g%ncells() == nc .and. abs(c(1) - 0.5_real64*g%step() - a) <= 1.0e-12_real64*abs(a) &
            .and. abs(g%step() - (b - a)/nc) <= 1.0e-12_real64*g%step(), &
            "the pilot must span four bandwidths beyond the data, a quarter of a bandwidth to the cell")
        if (allocated(error)) return

        lg = pilot_log_g(g)
        do j = 1, size(xs)
            call g%pdf(xs(j), p)
            want = h*exp(-0.5_real64*(log(p) - lg))
            call check(error, abs(hb(j) - want) <= 1.0e-12_real64*want, &
                "every bandwidth must be h*(p(x)/g)**(-1/2) read from the pilot %pilot returns")
            if (allocated(error)) return
        end do
        call g%density(f)
        ! A point the pilot reads no density at takes the CAP, which is always in force: the
        ! default spread cap (`KDE_SPREAD_MAX`, 100) times the NARROWEST bandwidth the rule can
        ! give, which is reached at the pilot's densest cell. The pilot's smallest positive
        ! density, which once stood in for `p` here, is no longer reachable.
        pmax = maxval(f)
        call k%bandwidth_at(1.0e4_real64, far)
        want = 100.0_real64*h*exp(-0.5_real64*(log(pmax) - lg))
        call check(error, abs(far - want) <= 1.0e-12_real64*want, &
            "a point the pilot reads as zero must take the default spread cap")
        if (allocated(error)) return

        call kempty%fit(empty, adaptive=.true.)
        call kempty%pilot(g)
        call check(error, kempty%is_adaptive() .and. .not. g%is_initialised(), &
            "an adaptive fit with nothing to build a pilot from must hand back an uninitialised grid")

    end subroutine test_pilot_is_the_fits

    !> `pilot=` applies a smoothing measured on ANOTHER sample: the shape from the pilot, and --
    !! given alone -- the scale from it too.
    !!
    !! **The round trip is the load-bearing assertion.** A fit that builds its own pilot, hands it
    !! back through `%pilot`, and is then refitted reading that copy must reproduce its own
    !! bandwidths EXACTLY, bit for bit: the two routes differ only in whether the table was built
    !! here or copied in, so anything but equality means the read and the build disagree. That is
    !! the promise `%pilot`'s doc-comment already makes in the other direction, closed here.
    !!
    !! The second arm is the transfer proper -- one sample's pilot applied to another -- against
    !! the `pf_kde_grid` route, which is the only way the same thing could be said before. They
    !! must agree to the grid's own resolution, and no better: one is exact and one is binned.
    !!
    !! The third arm pins the SCALE rule, which is the part a reader is most likely to get wrong:
    !! `pilot=` alone takes the pilot's own global bandwidth and `%rule` answers `"explicit"`,
    !! while a `rule=` beside it re-measures the scale on this sample and keeps only the shape.
    !!
    !! What a transferred pilot SAVES -- the grid pass this fit does not make -- is a timing, so it
    !! is `bench/benchmark_kde.sh`'s `adaptive` mode that reports it and not this test.
    subroutine test_fit_takes_a_pilot(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde) :: ka, kb, kc
        type(pf_kde_grid) :: p, g
        real(real64), allocatable :: xa(:), xb(:), h1(:), h2(:), hg(:)
        real(real64) :: t(40), fa(40), fg(40), peak
        character(len=:), allocatable :: name
        logical :: ok

        call kde_two_component(60_int64, xa)
        call kde_two_component(90_int64, xb)

        ! ---- (i) the round trip: a pilot built here, handed back, and read again ----
        call ka%fit(xa, rule="silverman", adaptive=.true.)
        call ka%pilot(p)
        allocate(h1(ka%n_valid()))
        call ka%bandwidths(h1)
        call kb%fit(xa, adaptive=.true., pilot=p, ok=ok)
        allocate(h2(kb%n_valid()))
        call kb%bandwidths(h2)
        call check(error, ok .and. kb%bandwidth() == ka%bandwidth(), &
            "a pilot given alone must carry the scale: the refit must take the pilot's own bandwidth")
        if (allocated(error)) return
        call check(error, size(h2) == size(h1), "the refit must keep the same population")
        if (allocated(error)) return
        call check(error, all(h2 == h1), &
            "reading a pilot back must reproduce the bandwidths the fit that built it used, bit for bit")
        if (allocated(error)) return
        call kb%rule(name)
        call check(error, name == "explicit", &
            "a scale taken from a pilot is a number, not a rule, so %rule must answer explicit")
        if (allocated(error)) return

        ! ---- (ii) the transfer: A's pilot applied to B, against the grid route ----
        call kc%fit(xb, adaptive=.true., pilot=p, bandwidth_max=200.0_real64, ok=ok)
        call check(error, ok, "a foreign pilot must give a defined fit")
        if (allocated(error)) return
        call g%init(400, -500.0_real64, 500.0_real64, ka%bandwidth(), pilot=p, &
            bandwidth_max=200.0_real64)
        call g%add(xb)
        call g%finish()
        call spread_points(-400.0_real64, 400.0_real64, t)
        call kc%pdf(t, fa)
        call g%pdf(t, fg)
        peak = maxval(fa)
        call check(error, peak > 0.0_real64 .and. maxval(abs(fa - fg)) < 2.0e-2_real64*peak, &
            "the exact and the grid form of one transferred pilot must answer the same density")
        if (allocated(error)) return
        ! The bandwidths themselves are the shared quantity, and they must agree exactly: both
        ! forms read one table through one look-up.
        allocate(hg(kc%n_valid()))
        call kc%bandwidths(hg)
        call check(error, all(hg > 0.0_real64) .and. maxval(hg) <= 200.0_real64, &
            "bandwidth_max must bound a transferred bandwidth as it bounds a measured one")
        if (allocated(error)) return

        ! ---- (iii) a rule beside the pilot re-scales, keeping the shape ----
        call kc%fit(xb, rule="silverman", adaptive=.true., pilot=p, bandwidth_max=200.0_real64)
        call kc%rule(name)
        call check(error, name == "silverman" .and. kc%bandwidth() /= ka%bandwidth(), &
            "a rule beside a pilot must re-measure the scale on this sample")
        if (allocated(error)) return

    end subroutine test_fit_takes_a_pilot

    !> A grid's range need not lie inside its pilot's. A point beyond the pilot reads no density
    !! there, and that is a case the adaptive rule already answers -- the cap where one was given,
    !! the pilot's smallest positive density otherwise -- so extending it from "no density here" to
    !! "no pilot here" says the same thing about the same absence.
    !!
    !! **Why it has to be allowed.** R3 poisons a grid whose widest adaptive kernel out-reaches its
    !! range, and it names two remedies: cap the bandwidth, or widen the range. Refusing a grid
    !! wider than its pilot would leave only the first, which is the narrower answer -- it changes
    !! the estimate, where widening the range does not.
    !!
    !! The negative control is a cell WELL INSIDE the pilot's range: relaxing the rule at the edges
    !! must leave the interior bit for bit as it was, or what changed is not the coverage rule.
    subroutine test_grid_wider_than_its_pilot(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde_grid) :: p, wide, narrow
        real(real64) :: x(200), fw(400), fn(200), mass, hj
        type(pf_kde) :: k
        integer :: i, inside

        call kde_exponential(200, x)

        ! The pilot spans [0, 3]; the grid below spans [0, 6], so half of it has no pilot at all.
        call p%init(200, 0.0_real64, 3.0_real64, 0.3_real64)
        call p%add(x)
        call p%finish()

        call wide%init(400, 0.0_real64, 6.0_real64, 0.3_real64, pilot=p, bandwidth_max=1.0_real64)
        call wide%add(x)
        call wide%finish()
        call wide%density(fw)
        call check(error, .not. any(ieee_is_nan(fw)), &
            "a grid reaching beyond its pilot must answer numbers, not NaN")
        if (allocated(error)) return
        call check(error, any(fw(201:400) >= 0.0_real64) .and. all(fw >= -1.0e-12_real64), &
            "the cells beyond the pilot's range must hold an ordinary density")
        if (allocated(error)) return
        ! The wider grid must hold MORE of the sample than the one stopping at the pilot's edge,
        ! which is the whole point of being allowed to reach past it. Neither holds all of it: a
        ! kernel at the last point spills half its mass beyond the range, and the grid counts that
        ! weight rather than depositing it.
        call narrow%init(200, 0.0_real64, 3.0_real64, 0.3_real64, pilot=p, bandwidth_max=1.0_real64)
        call narrow%add(x)
        call narrow%finish()
        call narrow%density(fn)
        mass = sum(fw)*(6.0_real64/400.0_real64)
        call check(error, mass > sum(fn)*(3.0_real64/200.0_real64) .and. mass <= 1.0_real64, &
            "reaching past the pilot must capture more of the sample, and never more than all of it")
        if (allocated(error)) return

        ! A point beyond the pilot takes the cap exactly, which is the documented rule for a point
        ! the pilot reads as zero.
        call k%fit(x, adaptive=.true., pilot=p, bandwidth_max=1.0_real64)
        call k%bandwidth_at(5.0_real64, hj)
        call check(error, hj == 1.0_real64, &
            "a point beyond the pilot must take bandwidth_max, as a point of zero density does")
        if (allocated(error)) return

        ! The negative control: inside the pilot, nothing moved.
        inside = 0
        do i = 1, 200
            if (fn(i) > 0.0_real64) inside = inside + 1
        end do
        call check(error, inside > 100, "the control grid must hold a density, or it asserts nothing")

    end subroutine test_grid_wider_than_its_pilot

    !> The pilot's cells: a quarter of a bandwidth over its range, never fewer than 64 -- a sample
    !> narrower than sixteen bandwidths -- and never more than 65536 -- a far outlier -- with the
    !> range clipped to the support.
    subroutine test_pilot_cells_rule(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde) :: k
        type(pf_kde_grid) :: g
        real(real64) :: c(64)

        call k%fit([0.0_real64, 1.0_real64, 2.0_real64], bandwidth=10.0_real64, adaptive=.true.)
        call k%pilot(g)
        call check(error, g%ncells() == 64, "a range of about eight bandwidths must get the 64-cell floor")
        if (allocated(error)) return
        call k%fit([0.0_real64, 0.5_real64, 1.0_real64, 1000.0_real64], bandwidth=0.01_real64, adaptive=.true.)
        call k%pilot(g)
        call check(error, g%ncells() == 65536, "a far outlier must meet the 65536-cell ceiling")
        if (allocated(error)) return
        call k%fit([0.1_real64, 0.2_real64, 0.9_real64], bandwidth=0.1_real64, adaptive=.true., lower=0.0_real64, &
            upper=1.0_real64)
        call k%pilot(g)
        call g%grid(c)
        call check(error, g%ncells() == 64 .and. abs(c(1) - 0.5_real64/64.0_real64) <= 1.0e-15_real64 .and. &
            abs(c(64) - 127.0_real64/128.0_real64) <= 1.0e-15_real64, &
            "a pilot reaching past the support must be clipped to it")

    end subroutine test_pilot_cells_rule

    !> A `pf_kde_grid` given the fit's own pilot, with the fit's bandwidth and `alpha`, gives every
    !> point the bandwidth `%fit` gave it, so the grid converges to the fit's adaptive estimate as
    !> the square of its cell width -- the fixed form's convergence test, adaptive. `g` or `lambda`
    !> formed differently in the two forms leaves an error that does not fall.
    subroutine test_two_forms_agree_under_one_pilot(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde) :: k
        type(pf_kde_grid) :: p, g
        real(real64), allocatable :: x(:)
        real(real64) :: t(997), fx(997), fg(997), e(2), h, lo, hi
        integer :: kk, r
        character(len=200) :: msg

        call kde_two_component(60_int64, x)
        h = 30.0_real64
        lo = minval(x) - 3.5_real64*h
        hi = maxval(x) + 3.5_real64*h
        call spread_points(minval(x), maxval(x), t)
        do kk = 1, 3, 2
            call k%fit(x, bandwidth=h, kernel=KERNELS(kk), adaptive=.true.)
            call k%pilot(p)
            call k%pdf(t, fx)
            do r = 1, 2
                call g%init(400*r, lo, hi, h, kernel=KERNELS(kk), pilot=p)
                call g%add(x)
                call g%finish()
                call g%pdf(t, fg)
                e(r) = rms(fg - fx)
            end do
            write(msg, '(3a,f8.4)') "adaptive ", trim(KERNELS(kk)), &
                ": halving the cells must divide the RMS error by four; the ratio is ", e(1)/e(2)
            call check(error, e(1)/e(2) > 3.6_real64 .and. e(1)/e(2) < 4.4_real64, trim(msg))
            if (allocated(error)) return
        end do

    end subroutine test_two_forms_agree_under_one_pilot

    !> Every point's kernel keeps unit mass inside the support at its own bandwidth: the adaptive
    !> estimate integrates to one under both corrections (`pf_integrate`, which shares nothing with
    !> it), its `%cdf` reaches one just inside the upper bound, and an adaptive grid deposits
    !> exactly its weight. A mass formed at the global bandwidth instead of the point's own loses
    !> or invents mass wherever the two differ.
    subroutine test_adaptive_conserves_mass(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(kde_density) :: fn
        type(pf_kde_grid) :: p, g
        real(real64), allocatable :: z(:), r(:), dens(:)
        real(real64) :: mass, c_top
        character(len=11), parameter :: METHODS(3) = [character(len=11) :: "renormalise", "reflect", "linear"]
        integer :: mm
        character(len=160) :: msg

        call bounded_fixture(300_int64, z)
        do mm = 1, 3
            ! The B-spline, twice continuously differentiable, so that the quadrature needs no
            ! breakpoint at each of six hundred kernel edges.
            call fn%k%fit(z, bandwidth=0.06_real64, kernel="bspline", adaptive=.true., lower=0.0_real64, &
                upper=1.0_real64, boundary=METHODS(mm))
            fn%power = 0
            mass = pf_integrate(fn, 0.0_real64, 1.0_real64, 1.0e-10_real64)
            write(msg, '(2a,es10.3)') trim(METHODS(mm)), ": the adaptive estimate must integrate to one; it misses by ", &
                mass - 1.0_real64
            call check(error, abs(mass - 1.0_real64) <= 1.0e-9_real64, trim(msg))
            if (allocated(error)) return
            call fn%k%cdf(nearest(1.0_real64, -1.0_real64), c_top)
            call check(error, abs(c_top - 1.0_real64) <= 1.0e-12_real64, &
                trim(METHODS(mm)) // ": %cdf must reach one just inside the upper bound")
            if (allocated(error)) return
            call fn%k%pilot(p)
            call g%init(200, 0.0_real64, 1.0_real64, 0.06_real64, kernel="bspline", pilot=p, lower=0.0_real64, &
                upper=1.0_real64, boundary=METHODS(mm))
            call g%add(z)
            if (mm == 2) then
                call g%finish()
                call raw_cells(g, r)
                call check(error, abs(sum(r)*g%step()/g%sum_weights() - 1.0_real64) <= 1.0e-13_real64, &
                    trim(METHODS(mm)) // ": an adaptive grid must deposit exactly its weight")
            else
                ! Under either local-polynomial correction a point deposits its own corrected mass,
                ! which is near but not exactly its weight: the normalisation is the estimate's,
                ! once, at query time. What must hold is that the density the grid answers is one
                ! over the range, the cells clipped and divided by the mass they hold -- nothing
                ! lies beyond this range, both bounds being its ends.
                allocate(dens(g%ncells()))
                call g%finish()
                call g%density(dens)
                call check(error, abs(sum(dens)*g%step() - 1.0_real64) <= 1.0e-13_real64 .and. &
                    all(dens >= 0.0_real64), trim(METHODS(mm)) // &
                    ": an adaptive grid's density must be clipped and integrate to one over its range")
                deallocate(dens)
            end if
            if (allocated(error)) return
        end do

    end subroutine test_adaptive_conserves_mass

    !> `%bandwidth_at` is the look-up `%fit` used: at the retained points it answers `%bandwidths`
    !> bit for bit, the scalar form answers the array form's element, and it is NaN at a NaN point
    !> and on an undefined estimate. A fixed fit answers its one bandwidth everywhere.
    subroutine test_bandwidth_at(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde) :: k, kf, ku
        real(real64), allocatable :: x(:), hb(:), xs(:), ha(:)
        real(real64) :: one, nan_h, fixed(3), w(60)
        logical :: ok
        integer :: i

        call kde_two_component(60_int64, x)
        do i = 1, 60
            w(i) = real(1 + mod(i, 3), real64)
        end do
        call k%fit(x, rule="silverman", adaptive=.true., weights=w)
        allocate(hb(k%n_valid()), xs(k%n_valid()), ha(k%n_valid()))
        call k%bandwidths(hb, xs)
        call k%bandwidth_at(xs, ha)
        call check(error, all(ha == hb), "%bandwidth_at at the retained points must be %bandwidths, bit for bit")
        if (allocated(error)) return
        call k%bandwidth_at(xs(7), one)
        call check(error, one == hb(7), "the scalar form must answer the array form's element")
        if (allocated(error)) return
        call k%bandwidth_at(ieee_value(1.0_real64, ieee_quiet_nan), nan_h)
        call check(error, ieee_is_nan(nan_h), "%bandwidth_at must be NaN at a NaN point")
        if (allocated(error)) return
        call kf%fit(x, bandwidth=25.0_real64)
        call kf%bandwidth_at([-300.0_real64, 0.0_real64, 300.0_real64], fixed)
        call check(error, all(fixed == 25.0_real64), "a fixed fit must answer its one bandwidth everywhere")
        if (allocated(error)) return
        call ku%fit([3.0_real64, 3.0_real64], rule="silverman", adaptive=.true., ok=ok)
        call ku%bandwidth_at(3.0_real64, one)
        call check(error, (.not. ok) .and. ieee_is_nan(one), "an undefined adaptive estimate must answer NaN")

    end subroutine test_bandwidth_at

    !> The streaming form's adaptive rule: `pilot=` copies the pilot, so clearing the caller's
    !> afterwards changes nothing; a grid over all but the last point merged with one over that
    !> point is the grid over them all, to the bit, when both read one pilot; and a poisoned pilot
    !> poisons the grid built on it, quietly and for good.
    subroutine test_adaptive_grid(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde_grid) :: p, pp, g, g_all, g1, g2, gp, gf
        real(real64), allocatable :: x(:), r(:), r_all(:), before(:)
        real(real64) :: f(100)

        call kde_two_component(60_int64, x)
        call p%init(64, -700.0_real64, 800.0_real64, 50.0_real64)
        call p%add(x)
        call p%finish()
        call g%init(100, -600.0_real64, 700.0_real64, 50.0_real64, pilot=p, alpha=0.7_real64)
        call gf%init(100, -600.0_real64, 700.0_real64, 50.0_real64)
        call check(error, g%is_adaptive() .and. .not. gf%is_adaptive(), "pilot= must make the grid adaptive")
        if (allocated(error)) return
        call g%add(x)
        call g%finish()
        call raw_cells(g, before)
        call p%clear()
        call g%clear()
        call g%add(x)
        call g%finish()
        call raw_cells(g, r)
        call check(error, all(r == before), "the pilot must be copied: clearing the caller's must change nothing")
        if (allocated(error)) return

        call p%add(x)
        call p%finish()
        call g_all%init(100, -600.0_real64, 700.0_real64, 50.0_real64, pilot=p, alpha=0.7_real64)
        call g_all%add(x)
        call g1%init(100, -600.0_real64, 700.0_real64, 50.0_real64, pilot=p, alpha=0.7_real64)
        call g1%add(x(1:59))
        call g2%init(100, -600.0_real64, 700.0_real64, 50.0_real64, pilot=p, alpha=0.7_real64)
        call g2%add(x(60:60))
        call g1%merge(g2)
        call g_all%finish()
        call raw_cells(g_all, r_all)
        call g1%finish()
        call raw_cells(g1, r)
        call check(error, all(r == r_all) .and. g1%n() == 60_int64, &
            "two adaptive grids reading one pilot must merge into the grid over all their points, to the bit")
        if (allocated(error)) return

        ! Poisoned after it had accumulated the sample, so that its cells hold weight and every
        ! bandwidth read from them would be finite and wrong: only the poison stops the deposit.
        call pp%init(64, -700.0_real64, 800.0_real64, 50.0_real64)
        call pp%add(x)
        call pp%add([ieee_value(1.0_real64, ieee_quiet_nan)], skipnan=.false.)
        call pp%finish()
        call gp%init(100, -600.0_real64, 700.0_real64, 50.0_real64, pilot=pp)
        call gp%finish()
        call gp%density(f)
        call check(error, all(ieee_is_nan(f)), "a grid on a poisoned pilot must answer NaN before any %add")
        if (allocated(error)) return
        call gp%clear()
        call gp%add(x, finish=.true.)
        call gp%density(f)
        call check(error, all(ieee_is_nan(f)), "a poisoned pilot must poison the grid built on it")
        if (allocated(error)) return
        call gp%clear()
        call gp%add(x)
        call gp%finish()
        call gp%density(f)
        call check(error, all(ieee_is_nan(f)), "clearing the grid must not clear its pilot's poison")

    end subroutine test_adaptive_grid

    !> A pilot with no density to read is data, not a mistake, so `%init` accepts it and every answer
    !> of the grid built on it is NaN -- before and after `%add`, `%clear()` or not -- with every
    !> count still kept. Two such pilots: one never given a point, and one whose only point lies
    !> beyond its range, so that it holds weight but none in its cells. The control, a pilot with
    !> points in its cells, gives the same grid finite answers.
    subroutine test_empty_pilot_is_quiet(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde_grid) :: empty, beyond, full, g
        real(real64) :: f(4), fp(8), pdf, cdf, q
        integer :: which

        call empty%init(8, 0.0_real64, 1.0_real64, 0.1_real64)
        call beyond%init(8, 0.0_real64, 1.0_real64, 0.01_real64)
        call beyond%add([5.0_real64])
        call beyond%finish()
        call beyond%density(fp)
        call check(error, beyond%sum_weights() == 1.0_real64 .and. all(fp == 0.0_real64), &
            "the fixture: a pilot whose one point lies beyond its range holds its weight and none in its cells")
        if (allocated(error)) return
        call full%init(8, 0.0_real64, 1.0_real64, 0.1_real64)
        call full%add([0.1_real64, 0.2_real64, 0.3_real64, 0.4_real64])

        call full%finish()
        call g%init(4, 0.0_real64, 1.0_real64, 0.1_real64, pilot=full)
        call g%add([0.25_real64, 0.5_real64])
        call g%finish()
        call g%density(f)
        call check(error, g%is_adaptive() .and. .not. any(ieee_is_nan(f)), &
            "the control: a grid on a pilot with points in its cells must answer finite densities")
        if (allocated(error)) return

        do which = 1, 2
            if (which == 1) then
                call empty%finish()
                call g%init(4, 0.0_real64, 1.0_real64, 0.1_real64, pilot=empty, bandwidth_max=0.5_real64)
            else
                call g%init(4, 0.0_real64, 1.0_real64, 0.1_real64, pilot=beyond)
            end if
            call g%finish()
            call g%density(f)
            call check(error, g%is_adaptive() .and. all(ieee_is_nan(f)), &
                "a grid on a pilot with nothing to read must be adaptive and answer NaN before any %add")
            if (allocated(error)) return
            call g%clear()
            call g%add([0.25_real64, 0.5_real64], finish=.true.)
            call g%density(f)
            call g%pdf(0.5_real64, pdf)
            call g%cdf(0.5_real64, cdf)
            call g%quantile(0.5_real64, q)
            call check(error, all(ieee_is_nan(f)) .and. ieee_is_nan(pdf) .and. ieee_is_nan(cdf) .and. &
                ieee_is_nan(q) .and. g%n_valid() == 2_int64 .and. g%sum_weights() == 2.0_real64, &
                "a grid on a pilot with nothing to read must answer NaN and still count what %add accepted")
            if (allocated(error)) return
            call g%clear()
            call g%add([0.5_real64])
            call g%finish()
            call g%density(f)
            call g%sample(fp, 3_int64)
            call check(error, all(ieee_is_nan(f)) .and. all(ieee_is_nan(fp)), &
                "clearing the grid must not give it a pilot to read, nor its %sample a density to draw")
            if (allocated(error)) return
        end do

    end subroutine test_empty_pilot_is_quiet

    !> The adaptive fit's state through its accessors: `%is_adaptive`, the printer's extra rows,
    !> `%clear`, a fixed refit (every bandwidth the global one again), and the `real32` form, which
    !> forwards `adaptive`, `alpha` and `bandwidth_max` and equals the `real64` form over the
    !> widened values bit for bit.
    subroutine test_adaptive_accessors(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde) :: k, k64
        real(real64), allocatable :: x(:), hb(:), hb64(:), xw(:)
        real(real32), allocatable :: x32(:)
        real(real64) :: t(40), f(40), f64(40)
        integer :: u, nlines
        logical :: seen
        character(len=*), parameter :: PATH = "test_run/kde_adaptive_print.txt"

        call kde_two_component(60_int64, x)
        call k%fit(x, rule="silverman", adaptive=.true., alpha=0.3_real64, bandwidth_max=150.0_real64)
        call check(error, k%is_adaptive(), "an adaptive fit must say so")
        if (allocated(error)) return
        open(newunit=u, file=PATH, status="replace", action="write")
        call k%print(unit=u)
        close(u)
        call read_back(PATH, nlines, "bandwidth_max", seen)
        ! The heading and the fixed summary's ten rows, then alpha, the two caps, the pilot and
        ! the two ends of the bandwidths. `spread_max` is printed whether it was given or not,
        ! being always in force.
        call check(error, nlines == 17 .and. seen, "%print must add the adaptive rule's six rows")
        if (allocated(error)) return
        ! `read_back` deletes what it read, so the second needle needs the file written again.
        open(newunit=u, file=PATH, status="replace", action="write")
        call k%print(unit=u)
        close(u)
        call read_back(PATH, nlines, "spread_max", seen)
        call check(error, seen, "%print must name the spread cap, which is in force whether it was given or not")
        if (allocated(error)) return

        call k%clear()
        call check(error, .not. k%is_fitted(), "%clear must unfit an adaptive estimate")
        if (allocated(error)) return
        call k%fit(x, bandwidth=30.0_real64)
        allocate(hb(k%n_valid()))
        call k%bandwidths(hb)
        call check(error, (.not. k%is_adaptive()) .and. all(hb == 30.0_real64), &
            "a fixed refit must leave nothing of the adaptive rule behind")
        if (allocated(error)) return

        x32 = real(x, real32)
        xw = real(x32, real64)
        ! At alpha = 0.3 the widest bandwidth is about 1.17 of the global one, 133: a cap of 125 binds.
        call k%fit(x32, rule="silverman", adaptive=.true., alpha=0.3_real64, bandwidth_max=125.0_real64)
        call k64%fit(xw, rule="silverman", adaptive=.true., alpha=0.3_real64, bandwidth_max=125.0_real64)
        deallocate(hb)
        allocate(hb(k%n_valid()), hb64(k64%n_valid()))
        call k%bandwidths(hb)
        call k64%bandwidths(hb64)
        call spread_points(-400.0_real64, 500.0_real64, t)
        call k%pdf(t, f)
        call k64%pdf(t, f64)
        call check(error, maxval(hb) == 125.0_real64, "the real32 form must forward the cap, which binds here")
        if (allocated(error)) return
        call check(error, all(hb == hb64) .and. all(f == f64), &
            "the real32 form must forward the adaptive settings and equal the real64 form, bit for bit")

    end subroutine test_adaptive_accessors

    !> A bandwidth the estimate cannot use is an undefined estimate, never an overflow: one whose
    !> kernel would reach beyond the largest number, and an explicit bandwidth whose product with
    !> `adjust` would overflow, are both `ok = .false.` with every answer NaN.
    subroutine test_unusable_bandwidth(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde) :: k
        real(real64) :: f
        logical :: ok

        call k%fit([1.0_real64, 2.0_real64], bandwidth=0.5_real64*huge(1.0_real64), ok=ok)
        call k%pdf(1.0_real64, f)
        call check(error, (.not. ok) .and. ieee_is_nan(f) .and. ieee_is_nan(k%bandwidth()), &
            "a Gaussian whose reach would pass the largest number must leave the estimate undefined")
        if (allocated(error)) return
        call k%fit([1.0_real64, 2.0_real64], bandwidth=1.0e300_real64, adjust=1.0e10_real64, ok=ok)
        call check(error, (.not. ok) .and. ieee_is_nan(k%bandwidth()), &
            "a bandwidth times adjust past the largest number must leave the estimate undefined")
        if (allocated(error)) return
        call k%fit([1.0_real64, 2.0_real64], bandwidth=0.1_real64*huge(1.0_real64), kernel="box", ok=ok)
        call check(error, ok, "the control: a box whose reach stays finite must be defined")

    end subroutine test_unusable_bandwidth

    !> `%sample` is addressed by `(seed, stream, k)`, and every element follows one recipe: the stream
    !> `pf_random_key(stream, k)` under `pf_random_key(seed, label)`, whose first draw chooses the
    !> point and whose next draws the kernel's variate. Reproduced here from the generator on a
    !> two-point box fit, unweighted (`pf_random_int_at` chooses) and weighted (a uniform share of the
    !> total weight, located in the running weight), beside a coupled control arm that drops the
    !> label and must match no element. Then, on a fit whose draws a bound keeps rejecting: a longer
    !> sample starts with a shorter one, the two stream kinds agree, an absent stream is stream 0,
    !> and another stream or seed changes every element. A redraw reads only its own element's
    !> stream: on a one-point fit whose every other draw a bound rejects, no two elements are alike,
    !> where a redraw reading its neighbour's draws would repeat that neighbour's value; and a
    !> Gaussian variate beyond the cut is replaced by the same stream's next normal, found here by
    !> searching the streams for one whose first variate lies beyond five standard deviations.
    subroutine test_sample_addressing(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        integer(int64), parameter :: SEED = 20260919_int64
        type(pf_kde) :: k
        real(real64), allocatable :: x(:)
        real(real64) :: v(400), a(1000), b(500), c(1000), d(1000), r(2000), u, want, raw, e
        integer(int64) :: i, key, sk, j, jr, st
        integer :: n_recipe, n_raw, pass

        key = pf_random_key(SEED, KDE_LABEL)
        do pass = 1, 2
            if (pass == 1) then
                call k%fit([0.0_real64, 10.0_real64], bandwidth=1.0_real64, kernel="box")
            else
                call k%fit([0.0_real64, 10.0_real64], bandwidth=1.0_real64, kernel="box", &
                    weights=[1.0_real64, 3.0_real64])
            end if
            call k%sample(v, SEED, 3)
            n_recipe = 0
            n_raw = 0
            do i = 1_int64, 400_int64
                sk = pf_random_key(3_int64, i)
                if (pass == 1) then
                    j = pf_random_int_at(key, sk, 1_int64, 2_int64, 1_int64)
                    jr = pf_random_int_at(SEED, sk, 1_int64, 2_int64, 1_int64)
                else
                    ! The running weight is [1, 4]: the second point once the share reaches 1.
                    j = merge(1_int64, 2_int64, 4.0_real64*pf_random_at(key, sk, 1_int64) < 1.0_real64)
                    jr = merge(1_int64, 2_int64, 4.0_real64*pf_random_at(SEED, sk, 1_int64) < 1.0_real64)
                end if
                u = pf_random_at(key, sk, 2_int64)
                want = 10.0_real64*real(j - 1_int64, real64) + sqrt(3.0_real64)*(2.0_real64*u - 1.0_real64)
                u = pf_random_at(SEED, sk, 2_int64)
                raw = 10.0_real64*real(jr - 1_int64, real64) + sqrt(3.0_real64)*(2.0_real64*u - 1.0_real64)
                if (abs(v(i) - want) <= 1.0e-13_real64) n_recipe = n_recipe + 1
                if (abs(v(i) - raw) <= 1.0e-13_real64) n_raw = n_raw + 1
            end do
            call check(error, n_recipe == 400, merge("unweighted", "weighted  ", pass == 1) // &
                ": every element must be the recipe's draw, from its own stream under the family's key")
            if (allocated(error)) return
            call check(error, n_raw == 0, merge("unweighted", "weighted  ", pass == 1) // &
                ": no element may be the draw at the caller's own coordinates: the label must separate them")
            if (allocated(error)) return
        end do

        call kde_two_component(60_int64, x)
        call k%fit(x, rule="silverman", lower=-330.0_real64)
        call k%sample(a, SEED, 7)
        call k%sample(b, SEED, 7)
        call k%sample(c, SEED, 7_int64)
        call check(error, all(b == a(1:500)) .and. all(c == a), &
            "a shorter sample must be a prefix of a longer one, and the two stream kinds must agree")
        if (allocated(error)) return
        call check(error, minval(a) >= -330.0_real64, "every draw must respect the lower bound")
        if (allocated(error)) return
        call k%sample(c, SEED)
        call k%sample(d, SEED, 0_int64)
        call check(error, all(c == d), "an absent stream must be stream 0")
        if (allocated(error)) return
        call k%sample(c, SEED, 8)
        call k%sample(d, SEED + 1_int64, 7)
        call check(error, all(c /= a) .and. all(d /= a), "another stream, or another seed, must change every element")
        if (allocated(error)) return

        call k%fit([0.0_real64], bandwidth=1.0_real64, lower=0.0_real64)
        call k%sample(r, SEED, 1)
        call check(error, minval(r) >= 0.0_real64 .and. equal_pairs(r) == 0_int64, &
            "a redraw at the bound must read only its own element's stream: no two draws may be alike")
        if (allocated(error)) return

        e = 0.0_real64
        do st = 1_int64, 100000000_int64
            e = pf_random_normal_at(key, pf_random_key(st, 1_int64), 2_int64)
            if (abs(e) > 5.0_real64) exit
        end do
        want = pf_random_normal_at(key, pf_random_key(st, 1_int64), 3_int64)
        call check(error, abs(e) > 5.0_real64 .and. abs(want) <= 5.0_real64, &
            "the fixture: a stream whose first normal lies beyond the cut and whose second does not")
        if (allocated(error)) return
        call k%fit([0.0_real64], kernel="gaussian", bandwidth=1.0_real64)
        call k%sample(r(1:1), SEED, st)
        call check(error, r(1) == want, &
            "a variate beyond the Gaussian's cut must be redrawn from the same stream's next normal")

    end subroutine test_sample_addressing

    !> Each kernel's sampler draws that kernel, and each correction keeps the draws where the
    !> corrected estimate is. A one-point fit at unit bandwidth draws the kernel itself: 200000 draws
    !> have its mean (0), variance (1, the cut Gaussian's `1 - 1.5e-5`) and kurtosis (the cut
    !> Gaussian's 3, the Epanechnikov kernel's 15/7, the cubic B-spline's 2.7, the box's 1.8) to
    !> five standard errors, and none lies beyond its support. At a bound the draws follow the
    !> corrected kernel: the half kernel under `"renormalise"`, whose mean is the fit's own first
    !> moment (`pf_integrate`); under both corrections a box ten times wider than `[0, 1]`, which is
    !> flat there -- mean 1/2, variance 1/12 -- and which rejects so many draws that most fall back
    !> to inverting the point's distribution function; and under both, Gaussians wider than
    !> `[0, 1]`, whose draws have the mean and variance of the fit's own corrected density
    !> (`pf_integrate`), the case where a mirror image reaches the far bound and the doubly
    !> reflected mass the images omit is drawn again rather than kept.
    subroutine test_sample_support_and_kernel(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        integer, parameter :: N = 200000
        real(real64), parameter :: KERNEL_KURT(4) = [3.0_real64, 15.0_real64/7.0_real64, 2.7_real64, &
            1.8_real64]
        character(len=11), parameter :: METHODS(2) = [character(len=11) :: "renormalise", "reflect"]
        integer, parameter :: NL = 20000
        type(pf_kde) :: k
        type(kde_density) :: fn
        real(real64), allocatable :: v(:), vl(:)
        real(real64) :: mean, var, kurt, var_k, want, se, xe(200), zlo, hi_end, lo_end, cuts(1), dks
        integer :: kk, mm, nzone
        character(len=200) :: msg

        allocate(v(N), vl(NL))
        do kk = 1, 4
            call k%fit([0.0_real64], bandwidth=1.0_real64, kernel=trim(KERNELS(kk)))
            call k%sample(v, 11_int64, kk)
            call moments(v, mean, var, kurt)
            var_k = 1.0_real64
            if (kk == 1) var_k = 1.0_real64 - 1.5e-5_real64
            write(msg, '(a,a,3es12.4)') trim(KERNELS(kk)), ": mean, variance and kurtosis ", mean, var, kurt
            call check(error, abs(mean) <= 5.0_real64*sqrt(var_k/real(N, real64)) .and. &
                abs(var - var_k) <= 5.0_real64*var_k*sqrt((KERNEL_KURT(kk) - 1.0_real64)/real(N, real64)) .and. &
                abs(kurt - KERNEL_KURT(kk)) <= 0.1_real64, trim(msg) // " must be the kernel's")
            if (allocated(error)) return
            call check(error, maxval(abs(v)) <= RADIUS(kk), trim(KERNELS(kk)) // ": no draw may lie beyond the support")
            if (allocated(error)) return
        end do

        ! The half Gaussian at a lower bound: its mean is the corrected estimate's first moment.
        call fn%k%fit([0.0_real64], bandwidth=1.0_real64, lower=0.0_real64)
        fn%power = 1
        want = pf_integrate(fn, 0.0_real64, 5.0_real64, 1.0e-12_real64)
        call fn%k%sample(v, 12_int64)
        call moments(v, mean, var, kurt)
        se = sqrt(var/real(N, real64))
        write(msg, '(a,2es14.6)') "renormalise at a bound: the mean and the fit's first moment ", mean, want
        call check(error, minval(v) >= 0.0_real64 .and. abs(mean - want) <= 5.0_real64*se, trim(msg))
        if (allocated(error)) return

        ! Gaussians wider than [0, 1]: the draws follow the fit's own corrected density.
        do mm = 1, 2
            call fn%k%fit([0.1_real64, 0.35_real64, 0.8_real64], bandwidth=1.0_real64, lower=0.0_real64, &
                upper=1.0_real64, boundary=METHODS(mm))
            fn%power = 1
            want = pf_integrate(fn, 0.0_real64, 1.0_real64, 1.0e-12_real64)
            fn%power = 2
            var_k = pf_integrate(fn, 0.0_real64, 1.0_real64, 1.0e-12_real64) - want*want
            call fn%k%sample(v, 14_int64, mm)
            call moments(v, mean, var, kurt)
            write(msg, '(a,a,4es13.5)') trim(METHODS(mm)), ": wide Gaussians; the sample's mean and variance, and the fit's ", &
                mean, var, want, var_k
            call check(error, minval(v) >= 0.0_real64 .and. maxval(v) <= 1.0_real64 .and. &
                abs(mean - want) <= 5.0_real64*sqrt(var_k/real(N, real64)) .and. &
                abs(var - var_k) <= 5.0_real64*var_k*sqrt(1.0_real64/real(N, real64)), trim(msg))
            if (allocated(error)) return
        end do

        ! A box ten bandwidths wide on [0, 1] is flat there under either correction.
        do mm = 1, 2
            call k%fit([0.5_real64], bandwidth=10.0_real64, kernel="box", lower=0.0_real64, upper=1.0_real64, &
                boundary=METHODS(mm))
            call k%sample(v, 13_int64, mm)
            call moments(v, mean, var, kurt)
            write(msg, '(a,a,2es14.6)') trim(METHODS(mm)), ": a box far wider than [0, 1] draws it flat; mean, variance ", &
                mean, var
            call check(error, minval(v) >= 0.0_real64 .and. maxval(v) <= 1.0_real64 .and. &
                abs(mean - 0.5_real64) <= 5.0_real64*sqrt(1.0_real64/(12.0_real64*real(N, real64))) .and. &
                abs(var - 1.0_real64/12.0_real64) <= 5.0_real64*sqrt(0.8_real64/real(N, real64))/12.0_real64, trim(msg))
            if (allocated(error)) return
        end do

        ! ---- the linear correction ----
        ! A density that is large at its bound: with Silverman's rule about seventy per cent of
        ! `Exp(1)`'s mass lies inside the zone, so most draws are zone draws, made by rejection
        ! from the per-point envelope. The mean and the variance are the fit's own, by
        ! `pf_integrate` of `x f` and `x**2 f`, and no draw may leave the support.
        call kde_exponential(200, xe)
        call fn%k%fit(xe, rule="silverman", lower=0.0_real64, boundary="linear")
        zlo = RADIUS(1)*fn%k%bandwidth()
        call fn%k%quantile(1.0_real64, hi_end)
        cuts(1) = zlo
        fn%power = 1
        want = pf_integrate(fn, 0.0_real64, hi_end, 1.0e-12_real64, breakpoints=cuts)
        fn%power = 2
        var_k = pf_integrate(fn, 0.0_real64, hi_end, 1.0e-12_real64, breakpoints=cuts) - want*want
        call fn%k%sample(vl, 31_int64)
        call moments(vl, mean, var, kurt)
        write(msg, '(a,4es13.5)') "linear, zone-heavy: the sample's mean and variance, and the fit's ", &
            mean, var, want, var_k
        call check(error, minval(vl) >= 0.0_real64 .and. maxval(vl) <= hi_end .and. &
            abs(mean - want) <= 5.0_real64*sqrt(var_k/real(NL, real64)) .and. &
            abs(var - var_k) <= 5.0_real64*var_k*sqrt(2.0_real64/real(NL, real64)), trim(msg))
        if (allocated(error)) return
        call check(error, count(vl <= zlo) > NL/2, &
            "precondition: most draws must fall inside the zone, or the rejection sampler is barely exercised")
        if (allocated(error)) return

        ! The zone draws alone follow the estimate restricted to the zone: their empirical
        ! distribution function stays within the Kolmogorov-Smirnov critical distance at the 0.1 per
        ! cent level, `1.949/sqrt(n)`, of `%cdf` conditioned on the zone.
        call zone_ks_distance(fn%k, vl, zlo, dks, nzone)
        write(msg, '(a,i0,a,es12.5,a,es12.5)') "linear: ", nzone, " zone draws, KS distance ", dks, &
            " against the critical ", 1.949_real64/sqrt(real(nzone, real64))
        call check(error, dks <= 1.949_real64/sqrt(real(nzone, real64)), trim(msg))
        if (allocated(error)) return

        ! The mirror: a bound so far below the data that no kernel is corrected, so the zone is
        ! empty and every draw is an interior one.
        call kde_exponential(200, xe)
        call fn%k%fit(xe, rule="silverman", lower=-20.0_real64, boundary="linear")
        call fn%k%quantile(0.0_real64, lo_end)
        call fn%k%quantile(1.0_real64, hi_end)
        fn%power = 1
        want = pf_integrate(fn, lo_end, hi_end, 1.0e-12_real64)
        fn%power = 2
        var_k = pf_integrate(fn, lo_end, hi_end, 1.0e-12_real64) - want*want
        call fn%k%sample(vl, 32_int64)
        call moments(vl, mean, var, kurt)
        write(msg, '(a,4es13.5)') "linear, interior-heavy: the sample's mean and variance, and the fit's ", &
            mean, var, want, var_k
        call check(error, minval(vl) >= -20.0_real64 .and. &
            abs(mean - want) <= 5.0_real64*sqrt(var_k/real(NL, real64)) .and. &
            abs(var - var_k) <= 5.0_real64*var_k*sqrt(2.0_real64/real(NL, real64)), trim(msg))

    end subroutine test_sample_support_and_kernel

    !> Sorts `cuts(1:n)` ascending and drops any repeat, so that `pf_integrate` -- which aborts on
    !> a repeated breakpoint -- sees a list it accepts.
    subroutine sort_cuts(cuts, n)
        real(real64), intent(inout) :: cuts(:) !! the breakpoints
        integer, intent(inout)      :: n       !! how many; reduced by the repeats dropped
        real(real64) :: t
        integer :: i, j, m

        do i = 2, n
            t = cuts(i)
            j = i - 1
            do while (j >= 1)
                if (cuts(j) <= t) exit
                cuts(j + 1) = cuts(j)
                j = j - 1
            end do
            cuts(j + 1) = t
        end do
        m = 0
        do i = 1, n
            if (m >= 1) then
                if (cuts(m) == cuts(i)) cycle
            end if
            m = m + 1
            cuts(m) = cuts(i)
        end do
        n = m

    end subroutine sort_cuts

    !> The largest distance between the empirical distribution function of the draws that fell
    !> inside `[lo, zlo]` and `%cdf` conditioned on that interval, over a grid of points across it.
    subroutine zone_ks_distance(k, v, zlo, d, nzone)
        type(pf_kde), intent(in)  :: k     !! the fitted estimate
        real(real64), intent(in)  :: v(:)  !! the draws
        real(real64), intent(in)  :: zlo   !! the zone's inner edge
        real(real64), intent(out) :: d     !! the Kolmogorov-Smirnov distance
        integer, intent(out)      :: nzone !! how many draws fell inside the zone
        integer, parameter :: GRID = 400
        real(real64) :: t, cz, ct, edf
        integer :: i

        nzone = count(v <= zlo)
        d = 0.0_real64
        if (nzone < 100) return
        call k%cdf(zlo, cz)
        do i = 1, GRID
            t = zlo*(real(i, real64) - 0.5_real64)/real(GRID, real64)
            call k%cdf(t, ct)
            edf = real(count(v <= t), real64)/real(nzone, real64)
            d = max(d, abs(edf - ct/cz))
        end do

    end subroutine zone_ks_distance

    !> The point a draw starts from is chosen in proportion to its weight, and its kernel has the
    !> point's own bandwidth: three quarters of the draws from a fit weighted 1 to 3 lie about the
    !> heavier point, and the draws from an adaptive fit have the variance of the points plus the
    !> mean of `h_j**2` (each Gaussian kernel adding its own, cut, variance), which a sampler using
    !> the global bandwidth would miss by the spread of the bandwidths.
    subroutine test_sample_weights_and_bandwidths(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        integer, parameter :: N = 200000
        type(pf_kde) :: k
        real(real64), allocatable :: v(:), x(:), hb(:), xs(:)
        real(real64) :: share, mean, var, kurt, mu, want, want_global
        character(len=200) :: msg

        allocate(v(N))
        call k%fit([-10.0_real64, 10.0_real64], bandwidth=1.0_real64, kernel="box", weights=[1.0_real64, 3.0_real64])
        call k%sample(v, 21_int64)
        share = real(count(v > 0.0_real64), real64)/real(N, real64)
        write(msg, '(a,f9.6)') "weights 1 and 3: the share of draws about the heavier point is ", share
        call check(error, abs(share - 0.75_real64) <= 5.0_real64*sqrt(0.75_real64*0.25_real64/real(N, real64)), &
            trim(msg) // ", not 0.75")
        if (allocated(error)) return

        call kde_two_component(60_int64, x)
        call k%fit(x, rule="silverman", adaptive=.true., alpha=1.0_real64)
        allocate(hb(k%n_valid()), xs(k%n_valid()))
        call k%bandwidths(hb, xs)
        mu = sum(xs)/real(size(xs), real64)
        want = sum((xs - mu)**2)/real(size(xs), real64) + (1.0_real64 - 1.5e-5_real64)*sum(hb**2)/real(size(hb), real64)
        want_global = sum((xs - mu)**2)/real(size(xs), real64) + (1.0_real64 - 1.5e-5_real64)*k%bandwidth()**2
        call k%sample(v, 22_int64)
        call moments(v, mean, var, kurt)
        write(msg, '(a,3es14.6)') "adaptive: the variance, and that of the points plus mean(h_j**2) and plus h**2 ", &
            var, want, want_global
        call check(error, abs(mean - mu) <= 5.0_real64*sqrt(want/real(N, real64)) .and. &
            abs(var - want) <= 5.0_real64*want*sqrt(2.0_real64/real(N, real64)) .and. &
            abs(want - want_global) > 10.0_real64*want*sqrt(2.0_real64/real(N, real64)), trim(msg))

    end subroutine test_sample_weights_and_bandwidths

    !> The grid's `%sample` inverts the integral of its `%pdf` over its cells: each draw is the
    !> point where that integral reaches its uniform's share -- the recipe, reproduced here through
    !> the grid's own `%cdf` on a grid holding every point's weight, which at a draw is the draw's
    !> uniform, beside the control arm without the label -- and a sample's mean and variance are those
    !> of `%pdf` over the range (`pf_integrate`), on a grid narrower than its data, whose weight
    !> beyond the range no draw may land on. A longer sample starts with a shorter one; a grid with
    !> nothing in its cells, or a poisoned one, draws NaN.
    subroutine test_grid_sample(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        integer, parameter :: N = 200000
        integer(int64), parameter :: SEED = 77_int64
        type(pf_kde_grid) :: g
        type(grid_density) :: fn
        real(real64), allocatable :: x(:), v(:)
        real(real64) :: w(300), p(300), b(100), mass, m1, m2, mean, var, kurt
        integer(int64) :: i, key, sk
        integer :: n_recipe, n_raw
        character(len=200) :: msg

        call kde_fixture(1000_int64, x)
        call g%init(400, -700.0_real64, 700.0_real64, 30.0_real64)
        call g%add(x)
        call g%finish()
        call g%sample(w, SEED, 5)
        call g%cdf(w, p)
        key = pf_random_key(SEED, KDE_GRID_LABEL)
        n_recipe = 0
        n_raw = 0
        do i = 1_int64, 300_int64
            sk = pf_random_key(5_int64, i)
            if (abs(p(i) - pf_random_at(key, sk, 1_int64)) <= 1.0e-12_real64) n_recipe = n_recipe + 1
            if (abs(p(i) - pf_random_at(SEED, sk, 1_int64)) <= 1.0e-12_real64) n_raw = n_raw + 1
        end do
        call check(error, n_recipe == 300, &
            "every draw must be where the grid's integral reaches its uniform's share, from its own stream")
        if (allocated(error)) return
        call check(error, n_raw == 0, "no draw may come from the caller's own coordinates: the label must separate them")
        if (allocated(error)) return
        call g%sample(b, SEED, 5_int64)
        call check(error, all(b == w(1:100)), "a shorter sample must be a prefix, and the two stream kinds must agree")
        if (allocated(error)) return

        call g%init(300, -300.0_real64, 300.0_real64, 40.0_real64)
        call g%add(x)
        allocate(v(N))
        call g%finish()
        call g%sample(v, SEED)
        call moments(v, mean, var, kurt)
        fn%g = g
        ! The interpolant is linear between centres, so the centres are where the integrand bends.
        call g%grid(w)
        fn%power = 0
        mass = pf_integrate(fn, -300.0_real64, 300.0_real64, 1.0e-12_real64, breakpoints=w)
        fn%power = 1
        m1 = pf_integrate(fn, -300.0_real64, 300.0_real64, 1.0e-12_real64, breakpoints=w)/mass
        fn%power = 2
        m2 = pf_integrate(fn, -300.0_real64, 300.0_real64, 1.0e-12_real64, breakpoints=w)/mass - m1*m1
        write(msg, '(a,4es13.5)') "the sample's mean and variance, and %pdf's over the range ", mean, var, m1, m2
        call check(error, minval(v) >= -300.0_real64 .and. maxval(v) <= 300.0_real64 .and. &
            mass < 0.95_real64 .and. abs(mean - m1) <= 5.0_real64*sqrt(m2/real(N, real64)) .and. &
            abs(var - m2) <= 5.0_real64*m2*sqrt(2.0_real64/real(N, real64)), trim(msg))
        if (allocated(error)) return

        call g%init(10, 0.0_real64, 1.0_real64, 0.1_real64)
        call g%finish()
        call g%sample(b, SEED)
        call check(error, all(ieee_is_nan(b)), "an empty grid must draw NaN")
        if (allocated(error)) return
        call g%clear()
        call g%add([5.0_real64], finish=.true.)
        call g%sample(b, SEED)
        call check(error, all(ieee_is_nan(b)), "a grid whose only weight lies beyond its range must draw NaN")
        if (allocated(error)) return
        call g%clear()
        call g%add([0.5_real64, ieee_value(1.0_real64, ieee_quiet_nan)], skipnan=.false.)
        call g%finish()
        call g%sample(b, SEED)
        call check(error, all(ieee_is_nan(b)), "a poisoned grid must draw NaN")

    end subroutine test_grid_sample

    !> The adaptive golden cases once the pilot is fine: at 262144 cells the library agrees with the
    !> exact pilot's estimate to `1e-9` of its peak, and its gap falls as the square of the pilot's
    !> cell width -- by about sixteen from 65536 cells -- which is what shows the coarse case's gap
    !> to be the pilot's discretisation and not a difference of definition. Writes the process-global
    !> cell count, so it runs serially, and restores the rule.
    subroutine test_adaptive_golden_fine(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde) :: kf !! the 262144-cell fit check_fine leaves behind, reused by the case check

        ! Each check_fine call leaves its 262144-cell fit in `kf`, and the case check below is
        ! given that object rather than fitting the same case at the same resolution again: the
        ! fit IS the cost here, and this test used to pay for every fine one twice.
        call check_fine("ADAPT", KG_ADAPT_PDF, kf)
        if (.not. allocated(error)) call check_adaptive_case(error, "ADAPT", KG_ADAPT_H, KG_ADAPT_PDF, &
            KG_ADAPT_CDF, 1.0e-9_real64, 1.0e-10_real64, fitted=kf)
        if (.not. allocated(error)) call check_fine("ADAPT_CAP", KG_ADAPT_CAP_PDF, kf)
        if (.not. allocated(error)) call check_adaptive_case(error, "ADAPT_CAP", KG_ADAPT_CAP_H, KG_ADAPT_CAP_PDF, &
            KG_ADAPT_CAP_CDF, 1.0e-9_real64, 1.0e-10_real64, fitted=kf)
        if (.not. allocated(error)) call check_fine("ADAPT_REF", KG_ADAPT_REF_PDF, kf)
        if (.not. allocated(error)) call check_adaptive_case(error, "ADAPT_REF", KG_ADAPT_REF_H, KG_ADAPT_REF_PDF, &
            KG_ADAPT_REF_CDF, 1.0e-9_real64, 1.0e-10_real64, fitted=kf)
        if (.not. allocated(error)) call check_fine("ADAPT_W", KG_ADAPT_W_PDF, kf)
        if (.not. allocated(error)) call check_adaptive_case(error, "ADAPT_W", KG_ADAPT_W_H, KG_ADAPT_W_PDF, &
            KG_ADAPT_W_CDF, 1.0e-9_real64, 1.0e-10_real64, fitted=kf)
        if (.not. allocated(error)) call check_fine("ADAPT_LIN", KG_ADAPT_LIN_PDF, kf)
        ! Under `"linear"` each point's bandwidth is read from a pilot whose cells are the CLIPPED
        ! estimate over its own mass, and every moment, zone edge and per-point integral is then
        ! formed at that bandwidth: the same tolerance as the four above.
        if (.not. allocated(error)) call check_adaptive_case(error, "ADAPT_LIN", KG_ADAPT_LIN_H, &
            KG_ADAPT_LIN_PDF, KG_ADAPT_LIN_CDF, 1.0e-9_real64, 1.0e-10_real64, fitted=kf)

    contains

        !> The gap to the oracle at 65536 cells over the gap at 262144: second order, so about 16.
        subroutine check_fine(name, pdf, fine)
            character(len=*), intent(in) :: name     !! the case
            real(real64), intent(in)     :: pdf(NKX) !! the oracle's density at `KG_X`
            type(pf_kde), intent(out)    :: fine     !! the second (262144-cell) fit, for the caller
            type(pf_kde) :: k
            logical :: ok
            real(real64) :: got(NKX), e(2)
            integer :: r
            character(len=160) :: msg

            do r = 1, 2
                call parquet_debug_set_kde_pilot_cells(65536*4**(r - 1))
                call fit_golden_case(name, k, ok)
                call k%pdf(KG_X, got)
                e(r) = maxval(abs(got - pdf))
            end do
            fine = k
            call parquet_debug_set_kde_pilot_cells(0)
            write(msg, '(2a,f8.3)') name, ": quartering the pilot's cells must divide the gap by about 16; it is ", &
                e(1)/e(2)
            call check(error, e(1)/e(2) > 10.0_real64 .and. e(1)/e(2) < 25.0_real64, trim(msg))

        end subroutine check_fine

    end subroutine test_adaptive_golden_fine

    !> `parquet_debug_set_kde_pilot_cells(n)` gives the fit's pilot `n` cells whatever the rule
    !> says, and `n <= 0` restores the rule: the control first, then the override, then the
    !> restoration.
    subroutine test_pilot_cells_forced(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde) :: k
        type(pf_kde_grid) :: g
        real(real64), allocatable :: x(:)
        integer :: rule_cells

        call kde_two_component(60_int64, x)
        call k%fit(x, bandwidth=40.0_real64, adaptive=.true.)
        call k%pilot(g)
        rule_cells = g%ncells()
        call parquet_debug_set_kde_pilot_cells(100)
        call k%fit(x, bandwidth=40.0_real64, adaptive=.true.)
        call k%pilot(g)
        call parquet_debug_set_kde_pilot_cells(0)
        call check(error, rule_cells /= 100 .and. g%ncells() == 100, "the override must set the pilot's cells")
        if (allocated(error)) return
        call k%fit(x, bandwidth=40.0_real64, adaptive=.true.)
        call k%pilot(g)
        call check(error, g%ncells() == rule_cells, "n <= 0 must restore the rule")

    end subroutine test_pilot_cells_forced

    !> The ISJ golden cases at the oracle's grid of 1024 cells: the bandwidth, and the estimate's
    !> density and distribution function at every probe, against the 50-digit oracle, which solves
    !> the rule's fixed point on the same grid; and the two samples the rule finds no bandwidth for,
    !> undefined under `rule = "isj"`. The library agrees with the oracle to a few units of rounding,
    !> so the golden tolerances of the rules of thumb hold here too. Writes the process-global cell
    !> count, so it runs serially, and restores the default.
    subroutine test_isj_golden(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check

        call parquet_debug_set_kde_isj_cells(1024)
        call check_golden_case(error, "ISJ", KG_ISJ_DEF, KG_ISJ_H, KG_ISJ_PDF, KG_ISJ_CDF)
        if (.not. allocated(error)) call check_golden_case(error, "ISJ_WREL", KG_ISJ_WREL_DEF, KG_ISJ_WREL_H, &
            KG_ISJ_WREL_PDF, KG_ISJ_WREL_CDF)
        if (.not. allocated(error)) call check_golden_case(error, "ISJ_WFREQ", KG_ISJ_WFREQ_DEF, KG_ISJ_WFREQ_H, &
            KG_ISJ_WFREQ_PDF, KG_ISJ_WFREQ_CDF)
        if (.not. allocated(error)) call check_golden_case(error, "ISJ_BOUNDED", KG_ISJ_BOUNDED_DEF, &
            KG_ISJ_BOUNDED_H, KG_ISJ_BOUNDED_PDF, KG_ISJ_BOUNDED_CDF)
        if (.not. allocated(error)) call check_golden_case(error, "ISJ_ROUNDED", KG_ISJ_ROUNDED_DEF, &
            KG_ISJ_ROUNDED_H, KG_ISJ_ROUNDED_PDF, KG_ISJ_ROUNDED_CDF)
        if (.not. allocated(error)) call check_golden_case(error, "ISJ_NO_ROOT", KG_ISJ_NO_ROOT_DEF, &
            KG_ISJ_NO_ROOT_H, KG_ISJ_NO_ROOT_PDF, KG_ISJ_NO_ROOT_CDF)
        call parquet_debug_set_kde_isj_cells(0)

    end subroutine test_isj_golden

    !> `parquet_debug_set_kde_isj_cells(n)` gives the rule `n` cells and `n <= 0` restores the
    !> default: at 1024 cells the two-component recipe's bandwidth is the oracle's; at the default
    !> `2**14` it differs from that by more than rounding -- the override reached the grid -- and by
    !> less than a per cent, the grid being a discretisation and not a definition; after the reset
    !> it is the default's again, bit for bit.
    subroutine test_isj_cells_forced(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde) :: k
        real(real64), allocatable :: x(:)
        real(real64) :: h_default, h_1024, gap

        call kde_two_component(60_int64, x)
        call k%fit(x, rule="isj")
        h_default = k%bandwidth()
        call parquet_debug_set_kde_isj_cells(1024)
        call k%fit(x, rule="isj")
        h_1024 = k%bandwidth()
        call parquet_debug_set_kde_isj_cells(0)
        call check(error, close_to(h_1024, KG_ISJ_H, 1.0e-13_real64, KG_ISJ_H), &
            "at 1024 cells the rule must give the oracle's bandwidth")
        if (allocated(error)) return
        gap = abs(h_default - h_1024)/h_1024
        call check(error, gap > 1.0e-9_real64 .and. gap < 1.0e-2_real64, &
            "the default grid must move the bandwidth by more than rounding and less than a per cent")
        if (allocated(error)) return
        call k%fit(x, rule="isj")
        call check(error, k%bandwidth() == h_default, "n <= 0 must restore the default grid")

    end subroutine test_isj_cells_forced

    ! ==========================================================================================
    ! The binned method
    ! ==========================================================================================

    !> `method="exact"` named explicitly answers what a grid with no `method=` answers, bit for
    !> bit, cell for cell and counter for counter: the binned path is an addition, and the default
    !> is unchanged. `%method` round-trips both tokens and `%print` carries the one in force.
    subroutine test_binned_method_token(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde_grid) :: g, e
        real(real64), allocatable :: x(:), fg(:), fe(:)
        character(len=:), allocatable :: token
        character(len=512) :: line
        integer :: u, ios
        logical :: seen

        call kde_fixture(400_int64, x)
        call g%init(64, -100.0_real64, 100.0_real64, 40.0_real64)
        call g%add(x, finish=.true.)
        call e%init(64, -100.0_real64, 100.0_real64, 40.0_real64, method="exact")
        call e%add(x, finish=.true.)
        allocate(fg(64), fe(64))
        call g%density(fg)
        call e%density(fe)
        call check(error, all(fg == fe), 'method="exact" must answer the default bit for bit')
        if (allocated(error)) return
        call check(error, g%sum_weights() == e%sum_weights(), 'method="exact" must count the same weight')
        if (allocated(error)) return

        call e%method(token)
        call check(error, token == "exact", '%method must answer "exact"')
        if (allocated(error)) return
        ! The token is matched case-insensitively, like every other one in the module.
        call e%init(64, -100.0_real64, 100.0_real64, 40.0_real64, method="BiNnEd")
        call e%method(token)
        call check(error, token == "binned", "%method must fold the case of its token")
        if (allocated(error)) return

        open(newunit=u, status="scratch", action="readwrite")
        call e%print(unit=u)
        rewind(u)
        seen = .false.
        do
            read(u, '(a)', iostat=ios) line
            if (ios /= 0) exit
            if (index(line, "binned") > 0) seen = .true.
        end do
        close(u)
        call check(error, seen, "the grid's %print must name the method in force: binned")

    end subroutine test_binned_method_token

    !> An unbounded binned grid against the exact estimate at every cell centre: the error falls
    !> as the SQUARE of the cell width, which is the order `pf_kde_grid` already promises.
    !>
    !> The first and last cells carry the assertion that matters for the padding: the transform's
    !> basis is half-sample even about the array's own ends, so a pad one cell too narrow folds
    !> weight back onto exactly those cells and nowhere else.
    subroutine test_binned_converges(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde) :: k
        type(pf_kde_grid) :: g
        real(real64), allocatable :: f(:), c(:), t(:)
        real(real64) :: x(400), e(2), ratio
        character(len=200) :: msg
        integer :: nc, r

        call spread_points(-300.0_real64, 300.0_real64, x)
        call k%fit(x, bandwidth=40.0_real64)
        do r = 1, 2
            nc = 120*r
            if (allocated(f)) deallocate(f, c, t)
            allocate(f(nc), c(nc), t(nc))
            call g%init(nc, -700.0_real64, 700.0_real64, 40.0_real64, method="binned")
            call g%add(x, finish=.true.)
            call g%density(f, x=c)
            call k%pdf(c, t)
            e(r) = rms(f - t)
        end do
        call check(error, e(1) > 0.0_real64, "the coarse binned grid must differ from the exact estimate, " // &
            "or the convergence claim is vacuous")
        if (allocated(error)) return
        ratio = e(1)/e(2)
        write(msg, '(a,f8.4)') "halving the binned grid's cells must divide the RMS error by four; " // &
            "the ratio is ", ratio
        call check(error, ratio > 3.6_real64 .and. ratio < 4.4_real64, trim(msg))

    end subroutine test_binned_converges

    !> Every kernel: a binned grid against the EXACT grid at matched cells, on a fixture with
    !> structure at the bandwidth's own scale, so that the kernel's WIDTH is visible in the answer.
    !>
    !> This is what pins each kernel's own filter. The mutation it catches is one kernel sampled at
    !> another's scale -- the four scales are 1, `sqrt(5)`, `sqrt(3)` and `sqrt(3)` -- which leaves
    !> the mass right and the width wrong, and so survives every check on an integral. On a broad,
    !> featureless sample a wrong width is nearly invisible; on two clusters it is not.
    !>
    !> **The bound is one part in a thousand of the peak for the three kernels that VANISH at their
    !> support edge, and one part in a hundred for the box, which steps there.** Moving a point to
    !> a cell centre moves its kernel's edge by up to half a cell: where the kernel vanishes there
    !> that costs the square of the cell width, and where it steps it changes which cells the point
    !> covers at all.
    subroutine test_binned_every_kernel(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde_grid) :: g, e
        real(real64), allocatable :: x(:), f(:), t(:)
        real(real64) :: peak, worst, bound
        character(len=12), parameter :: KERNELS(4) = [character(len=12) :: "gaussian", "epanechnikov", &
            "bspline", "box"]
        character(len=200) :: msg
        integer :: kk

        call kde_two_component(2000_int64, x)
        allocate(f(1024), t(1024))
        do kk = 1, 4
            call g%init(1024, -700.0_real64, 700.0_real64, 40.0_real64, kernel=KERNELS(kk), method="binned")
            call g%add(x, finish=.true.)
            call e%init(1024, -700.0_real64, 700.0_real64, 40.0_real64, kernel=KERNELS(kk))
            call e%add(x, finish=.true.)
            call g%density(f)
            call e%density(t)
            peak = maxval(t)
            worst = maxval(abs(f - t))/peak
            bound = 1.0e-3_real64
            if (KERNELS(kk) == "box") bound = 1.0e-2_real64
            write(msg, '(3a,es10.3)') "binned ", trim(KERNELS(kk)), &
                " must reproduce the exact grid; the largest gap over the peak is ", worst
            call check(error, worst <= bound, trim(msg))
            if (allocated(error)) return
        end do

    end subroutine test_binned_every_kernel

    !> The cells, the weight below `xmin` and the weight above `xmax` sum to the weight deposited:
    !> the convolution multiplies the coefficient at zero frequency by exactly one, so no weight is
    !> created or lost. Asserted where the kernels reach well past both ends, and again on a grid
    !> whose range is the whole support under `"reflect"`, where the counters must be exactly zero.
    subroutine test_binned_conserves_weight(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde_grid) :: g
        real(real64), allocatable :: x(:), z(:), f(:)
        real(real64) :: total, p0, p1

        call kde_fixture(400_int64, x)
        call g%init(256, -100.0_real64, 100.0_real64, 40.0_real64, method="binned")
        call g%add(x, finish=.true.)
        allocate(f(256))
        call g%density(f, normalise=.false.)
        call g%cdf(-100.0_real64, p0)
        call g%cdf(100.0_real64, p1)
        call check(error, p0 > 0.2_real64 .and. p1 < 0.8_real64, &
            "the fixture must put a good part of its weight beyond each end, or this test is vacuous")
        if (allocated(error)) return
        total = sum(f)*g%step() + p0*g%sum_weights() + (1.0_real64 - p1)*g%sum_weights()
        call check(error, abs(total - g%sum_weights()) <= 1.0e-10_real64*g%sum_weights(), &
            "the binned cells plus the two beyond-range counters must be the weight deposited")
        if (allocated(error)) return

        ! The range IS the support, so nothing may be counted beyond either end at all.
        call bounded_fixture(400_int64, z)
        call g%init(256, 0.0_real64, 1.0_real64, 0.08_real64, lower=0.0_real64, upper=1.0_real64, &
            boundary="reflect", method="binned")
        call g%add(z, finish=.true.)
        if (allocated(f)) deallocate(f)
        allocate(f(256))
        call g%density(f, normalise=.false.)
        call g%cdf(0.0_real64, p0)
        call g%cdf(1.0_real64, p1)
        call check(error, p0 == 0.0_real64 .and. p1 == 1.0_real64, &
            "a reflecting grid whose range is the support must count nothing beyond either end")
        if (allocated(error)) return
        call check(error, abs(sum(f)*g%step() - g%sum_weights()) <= 1.0e-10_real64*g%sum_weights(), &
            "the reflecting binned grid's cells must hold the whole weight")

    end subroutine test_binned_conserves_weight

    !> Each boundary correction under the binned method against the exact grid it mirrors, at the
    !> bound where a correction either works or does not. `"reflect"` is tested twice: once where
    !> the range is the support and the basis supplies the correction unpadded, and once where the
    !> range is a strict subset and the images are folded in explicitly.
    subroutine test_binned_boundaries(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde_grid) :: g, e
        real(real64), allocatable :: z(:), fb(:), fe(:)
        real(real64) :: peak
        character(len=11), parameter :: CORR(2) = [character(len=11) :: "reflect", "renormalise"]
        integer :: mm

        call bounded_fixture(4000_int64, z)
        allocate(fb(512), fe(512))
        do mm = 1, 2
            ! The range is the whole support: the aligned case.
            call g%init(512, 0.0_real64, 1.0_real64, 0.08_real64, lower=0.0_real64, upper=1.0_real64, &
                boundary=CORR(mm), method="binned")
            call g%add(z, finish=.true.)
            call e%init(512, 0.0_real64, 1.0_real64, 0.08_real64, lower=0.0_real64, upper=1.0_real64, &
                boundary=CORR(mm))
            call e%add(z, finish=.true.)
            call g%density(fb)
            call e%density(fe)
            peak = maxval(fe)
            call check(error, abs(fb(1) - fe(1)) <= 5.0e-3_real64*peak, &
                "the binned grid must carry the correction at the lower bound under " // trim(CORR(mm)))
            if (allocated(error)) return
            call check(error, abs(fb(512) - fe(512)) <= 5.0e-3_real64*peak, &
                "the binned grid must carry the correction at the upper bound under " // trim(CORR(mm)))
            if (allocated(error)) return
            call check(error, maxval(abs(fb - fe)) <= 5.0e-3_real64*peak, &
                "the binned grid must reproduce the exact grid under " // trim(CORR(mm)))
            if (allocated(error)) return
        end do

        ! The range inside the support, so the reflecting images are folded in explicitly rather
        ! than coming from the basis, and the two counters are not zero.
        call g%init(512, 0.1_real64, 0.9_real64, 0.08_real64, lower=0.0_real64, upper=1.0_real64, &
            boundary="reflect", method="binned")
        call g%add(z, finish=.true.)
        call e%init(512, 0.1_real64, 0.9_real64, 0.08_real64, lower=0.0_real64, upper=1.0_real64, &
            boundary="reflect")
        call e%add(z, finish=.true.)
        call g%density(fb)
        call e%density(fe)
        peak = maxval(fe)
        call check(error, maxval(abs(fb - fe)) <= 1.0e-2_real64*peak, &
            "a reflecting binned grid whose range is inside the support must reproduce the exact grid")

    end subroutine test_binned_boundaries

    !> The binned lifecycle: `%finish` transforms once and a second call changes nothing, `%clear`
    !> reopens the grid and a re-`%add` gives the same cells back, and `%merge` of two binned
    !> partials is one grid over the concatenation.
    subroutine test_binned_lifecycle(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde_grid) :: g, a, b
        real(real64), allocatable :: x(:), f1(:), f2(:), f3(:)
        integer(int64) :: half

        call kde_fixture(400_int64, x)
        allocate(f1(128), f2(128), f3(128))
        call g%init(128, -400.0_real64, 400.0_real64, 40.0_real64, method="binned")
        call g%add(x)
        call check(error, .not. g%is_finished(), "a binned grid is not finished until %finish runs")
        if (allocated(error)) return
        call g%finish()
        call check(error, g%is_finished(), "%finish must close a binned grid")
        if (allocated(error)) return
        call g%density(f1)
        call g%finish()
        call g%density(f2)
        call check(error, all(f1 == f2), "a second %finish must not transform the cells again")
        if (allocated(error)) return

        call g%clear()
        call check(error, .not. g%is_finished(), "%clear must reopen a binned grid")
        if (allocated(error)) return
        call g%add(x, finish=.true.)
        call g%density(f2)
        call check(error, all(f1 == f2), "a cleared binned grid must accumulate again from empty")
        if (allocated(error)) return

        ! All but the last point, merged with the last: the one split whose bins add in the same
        ! order the single call sums them in, so the claim can be made to the bit. Two halves agree
        ! to rounding and not to the bit, as they do under the exact deposit.
        half = size(x, kind=int64) - 1_int64
        call a%init(128, -400.0_real64, 400.0_real64, 40.0_real64, method="binned")
        call a%add(x(1:half))
        call b%init(128, -400.0_real64, 400.0_real64, 40.0_real64, method="binned")
        call b%add(x(half + 1:))
        call a%merge(b, finish=.true.)
        call a%density(f3)
        call check(error, all(f1 == f3), "two merged binned partials must be one grid over the concatenation")

    end subroutine test_binned_lifecycle

    !> `%add(threads=)` at one, two and eight threads gives bit-identical cells: each thread gets a
    !> static share of the points and a private bin array, and the partials are summed in thread
    !> order.
    subroutine test_binned_threads(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde_grid) :: g
        real(real64), allocatable :: x(:), f1(:), f(:)
        integer, parameter :: TEAMS(3) = [1, 2, 8]
        integer :: t

        call kde_fixture(4000_int64, x)
        allocate(f1(128), f(128))
        do t = 1, 3
            call g%init(128, -400.0_real64, 400.0_real64, 40.0_real64, method="binned")
            call g%add(x, threads=TEAMS(t), finish=.true.)
            if (t == 1) then
                call g%density(f1)
            else
                call g%density(f)
                call check(error, all(f == f1), &
                    "a binned grid's cells must not depend on the thread count")
                if (allocated(error)) return
            end if
        end do

    end subroutine test_binned_threads

    !> The binned adaptive kernel: one transform per bandwidth class, each point's weight split
    !> between the two classes around its own bandwidth, linearly in `log h`.
    !>
    !> **The split is what is under test, not the bucketing.** Rounding each point to its NEAREST
    !> class is the obvious scheme and it is an order worse: the error then falls as the class
    !> count, where splitting makes it fall as the SQUARE of the count -- the same trick linear
    !> binning already applies to position. Doubling the classes must therefore divide the error by
    !> about four, and a nearest-class assignment halves it instead.
    !>
    !> Serial, because it forces the class count through a process-global hook.
    subroutine test_binned_adaptive(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde_grid) :: p, g, e
        real(real64), allocatable :: x(:), f(:), t(:)
        real(real64) :: err(2), peak, ratio
        character(len=200) :: msg
        integer :: r

        call kde_two_component(2000_int64, x)
        call p%init(128, -700.0_real64, 700.0_real64, 60.0_real64)
        call p%add(x, finish=.true.)
        call e%init(512, -700.0_real64, 700.0_real64, 60.0_real64, pilot=p, alpha=0.5_real64, &
            bandwidth_max=180.0_real64)
        call e%add(x, finish=.true.)
        allocate(f(512), t(512))
        call e%density(t)
        peak = maxval(t)
        do r = 1, 2
            call parquet_debug_set_kde_binned_classes(4*2**(r - 1))
            call g%init(512, -700.0_real64, 700.0_real64, 60.0_real64, pilot=p, alpha=0.5_real64, &
                bandwidth_max=180.0_real64, method="binned")
            call g%add(x, finish=.true.)
            call g%density(f)
            err(r) = rms(f - t)/peak
        end do
        call parquet_debug_set_kde_binned_classes(0)
        call check(error, err(1) > 1.0e-4_real64, &
            "four bandwidth classes must leave a measurable bucketing error, or this test is vacuous")
        if (allocated(error)) return
        ratio = err(1)/err(2)
        write(msg, '(a,f8.4)') "doubling the bandwidth classes must divide the bucketing error by " // &
            "about four, which a nearest-class assignment would not; the ratio is ", ratio
        call check(error, ratio >= 3.0_real64, trim(msg))
        if (allocated(error)) return

        ! At the count the rule's own spread gives, the binned adaptive grid is the exact adaptive
        ! grid to the accuracy the guide promises for it: about an order coarser than the fixed
        ! binned estimate, and plot-grade.
        call g%init(512, -700.0_real64, 700.0_real64, 60.0_real64, pilot=p, alpha=0.5_real64, &
            bandwidth_max=180.0_real64, method="binned")
        call g%add(x, finish=.true.)
        call g%density(f)
        write(msg, '(a,es10.3)') "the binned adaptive grid must reproduce the exact one; the largest " // &
            "gap over the peak is ", maxval(abs(f - t))/peak
        call check(error, maxval(abs(f - t))/peak <= 5.0e-3_real64, trim(msg))
        if (allocated(error)) return

        ! `alpha = 0` gives every point the global bandwidth, so one class carries them all and the
        ! adaptive binned grid is the fixed binned grid.
        call g%init(512, -700.0_real64, 700.0_real64, 60.0_real64, pilot=p, alpha=0.0_real64, &
            method="binned")
        call g%add(x, finish=.true.)
        call g%density(f)
        call e%init(512, -700.0_real64, 700.0_real64, 60.0_real64, method="binned")
        call e%add(x, finish=.true.)
        call e%density(t)
        call check(error, all(f == t), "alpha = 0 must make the binned adaptive grid the fixed binned grid")

    end subroutine test_binned_adaptive

    !> `%curve(method="binned")` against the exact curve, at matched points, unbounded and at a
    !> bound: the curve's points are the transform's own cell centres, so the two ranges cannot
    !> disagree by half a cell -- a disagreement that shows as a SHIFTED curve, right in shape and
    !> wrong in place, which no check on an integral would see.
    subroutine test_binned_curve(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde) :: k
        type(pf_kde_grid) :: g
        real(real64), allocatable :: z(:)
        real(real64) :: x(256), xe(256), f(256), fe(256), fg(256), peak, worst
        character(len=200) :: msg

        call kde_two_component(2000_int64, z)
        call k%fit(z, bandwidth=40.0_real64)
        call k%curve(xe, fe, xmin=-700.0_real64, xmax=700.0_real64)
        call k%curve(x, f, xmin=-700.0_real64, xmax=700.0_real64, method="binned")
        call check(error, all(x == xe), "the binned curve must answer the same points as the exact one")
        if (allocated(error)) return
        peak = maxval(fe)
        worst = maxval(abs(f - fe))/peak
        ! The curve's points are a grid's cell CENTRES, so a binned grid whose edges sit half a
        ! spacing outside them must answer the same bits. This is the geometric half of the claim,
        ! separated from the binning error the comparison with the exact curve also carries.
        call g%init(256, -700.0_real64 - 0.5_real64*(1400.0_real64/255.0_real64), &
            -700.0_real64 + 255.5_real64*(1400.0_real64/255.0_real64), 40.0_real64, method="binned")
        call g%add(z, finish=.true.)
        call g%density(fg)
        ! To rounding rather than to the bit: the curve forms its spacing as `(b - a)/(n - 1)` and
        ! `%init` forms it as `(xmax - xmin)/ncells`, which are the same number by two routes.
        write(msg, '(a,es10.3)') "the binned curve must be the binned grid whose cell centres are " // &
            "its points; the largest gap over the peak is ", maxval(abs(f - fg))/peak
        call check(error, maxval(abs(f - fg)) <= 1.0e-14_real64*peak, trim(msg))
        if (allocated(error)) return
        ! The control that makes the tolerance below mean something: the curve rises and falls by
        ! more than a hundredth of its peak between neighbouring points, so a range out by half a
        ! spacing would move it by several times the tolerance the next check applies.
        call check(error, maxval(abs(fe(2:) - fe(:255)))/peak > 1.0e-2_real64, &
            "the fixture must vary by more than the tolerance between neighbouring points, or a " // &
            "half-spacing shift would pass")
        if (allocated(error)) return
        write(msg, '(a,es10.3)') "the binned curve must reproduce the exact curve; the largest gap " // &
            "over the peak is ", worst
        call check(error, worst <= 2.0e-3_real64, trim(msg))
        if (allocated(error)) return

        ! At a bound, where the correction is what the curve must honour.
        call bounded_fixture(2000_int64, z)
        call k%fit(z, bandwidth=0.08_real64, lower=0.0_real64, upper=1.0_real64, boundary="reflect")
        call k%curve(xe, fe, xmin=0.0_real64, xmax=1.0_real64)
        call k%curve(x, f, xmin=0.0_real64, xmax=1.0_real64, method="binned")
        peak = maxval(fe)
        write(msg, '(a,es10.3)') "the binned curve must carry the fit's boundary correction; the " // &
            "largest gap over the peak is ", maxval(abs(f - fe))/peak
        call check(error, maxval(abs(f - fe))/peak <= 5.0e-3_real64, trim(msg))
        if (allocated(error)) return
        call check(error, abs(f(1) - fe(1)) <= 5.0e-3_real64*peak, &
            "the binned curve must agree with the exact one AT the lower bound")
        if (allocated(error)) return
        call check(error, abs(f(256) - fe(256)) <= 5.0e-3_real64*peak, &
            "the binned curve must agree with the exact one AT the upper bound")

    end subroutine test_binned_curve

    !> `boundary="linear"` under the binned method, against the exact grid, at the LOWER bound and
    !> at the UPPER bound separately rather than as one summary.
    !>
    !> The local linear correction is two convolutions: the ordinary one against `K`, and a second
    !> against the odd kernel `u K(u)`, whose transform is a sine. The mutation that matters is the
    !> odd half given the wrong sign or the wrong frequency -- the spectrum handed from `pf_dct` to
    !> `pf_idst` unshifted, say. Either biases the estimate towards one bound and away from the
    !> other while its integral stays one, so an RMS or a mass check survives both and only the two
    !> bounds taken apart can see them.
    subroutine test_binned_linear(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde_grid) :: g, e
        real(real64), allocatable :: z(:)
        real(real64) :: fb(512), fe(512), peak
        character(len=200) :: msg

        call bounded_fixture(4000_int64, z)
        call g%init(512, 0.0_real64, 1.0_real64, 0.08_real64, lower=0.0_real64, upper=1.0_real64, &
            boundary="linear", method="binned")
        call g%add(z, finish=.true.)
        call e%init(512, 0.0_real64, 1.0_real64, 0.08_real64, lower=0.0_real64, upper=1.0_real64, &
            boundary="linear")
        call e%add(z, finish=.true.)
        call g%density(fb)
        call e%density(fe)
        peak = maxval(fe)
        write(msg, '(a,es10.3,a,es10.3)') "binned linear at the LOWER bound: ", fb(1), " against ", fe(1)
        call check(error, abs(fb(1) - fe(1)) <= 1.0e-2_real64*peak, trim(msg))
        if (allocated(error)) return
        write(msg, '(a,es10.3,a,es10.3)') "binned linear at the UPPER bound: ", fb(512), " against ", fe(512)
        call check(error, abs(fb(512) - fe(512)) <= 1.0e-2_real64*peak, trim(msg))
        if (allocated(error)) return
        write(msg, '(a,es10.3)') "binned linear in the interior; the largest gap over the peak is ", &
            maxval(abs(fb - fe))/peak
        call check(error, maxval(abs(fb - fe)) <= 1.0e-2_real64*peak, trim(msg))
        if (allocated(error)) return
        ! The control that makes the two bounds mean something: the correction is doing real work
        ! there, so an estimate without it would miss by far more than the tolerance.
        call check(error, fe(1) > 1.2_real64*peak*0.05_real64 .or. fe(512) > 1.2_real64*peak*0.05_real64, &
            "the fixture must have a bound where the correction is acting, or this test is vacuous")

    end subroutine test_binned_linear

    !> The adaptive kernel and `boundary="linear"` together, which is the combination the request
    !> calls usual: each bandwidth class contributes its own pair of convolutions, and each pair is
    !> combined with the truncated moments taken at THAT class's bandwidth before the classes are
    !> summed. Taking the moments at the global bandwidth instead is the mutation this catches.
    subroutine test_binned_adaptive_linear(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde_grid) :: p, g, e
        real(real64), allocatable :: z(:)
        real(real64) :: fb(512), fe(512), peak
        character(len=200) :: msg

        call bounded_fixture(4000_int64, z)
        call p%init(128, 0.0_real64, 1.0_real64, 0.08_real64, lower=0.0_real64, upper=1.0_real64, &
            boundary="linear")
        call p%add(z, finish=.true.)
        call g%init(512, 0.0_real64, 1.0_real64, 0.08_real64, lower=0.0_real64, upper=1.0_real64, &
            boundary="linear", pilot=p, alpha=0.5_real64, bandwidth_max=0.2_real64, method="binned")
        call g%add(z, finish=.true.)
        call e%init(512, 0.0_real64, 1.0_real64, 0.08_real64, lower=0.0_real64, upper=1.0_real64, &
            boundary="linear", pilot=p, alpha=0.5_real64, bandwidth_max=0.2_real64)
        call e%add(z, finish=.true.)
        call g%density(fb)
        call e%density(fe)
        peak = maxval(fe)
        write(msg, '(a,es10.3)') "the binned adaptive linear estimate must reproduce the exact one; " // &
            "the largest gap over the peak is ", maxval(abs(fb - fe))/peak
        call check(error, maxval(abs(fb - fe)) <= 2.0e-2_real64*peak, trim(msg))
        if (allocated(error)) return
        call check(error, g%is_adaptive(), "the fixture must be adaptive, or this test is vacuous")

    end subroutine test_binned_adaptive_linear

    !> Every binned golden case: the cells a binned grid fills, against a 50-digit oracle that
    !> bins and convolves directly.
    !>
    !> The oracle sums the convolution term by term under the extension the transform implies --
    !> half-sample even about each end of its array -- where the library reaches the same numbers
    !> through one cosine transform per bandwidth class and, under `"linear"`, one inverse sine
    !> transform beside it. The two share the definition and nothing else, so any arithmetic change
    !> at all shows here.
    subroutine test_binned_golden(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check

        call check_binned_case(error, "BIN_GAUSS", KG_BIN_GAUSS_NC, KG_BIN_GAUSS_XMIN, &
            KG_BIN_GAUSS_XMAX, KG_BIN_GAUSS_F)
        if (allocated(error)) return
        call check_binned_case(error, "BIN_EPAN", KG_BIN_EPAN_NC, KG_BIN_EPAN_XMIN, &
            KG_BIN_EPAN_XMAX, KG_BIN_EPAN_F)
        if (allocated(error)) return
        call check_binned_case(error, "BIN_BOX", KG_BIN_BOX_NC, KG_BIN_BOX_XMIN, &
            KG_BIN_BOX_XMAX, KG_BIN_BOX_F)
        if (allocated(error)) return
        call check_binned_case(error, "BIN_BSPL", KG_BIN_BSPL_NC, KG_BIN_BSPL_XMIN, &
            KG_BIN_BSPL_XMAX, KG_BIN_BSPL_F)
        if (allocated(error)) return
        call check_binned_case(error, "BIN_REF", KG_BIN_REF_NC, KG_BIN_REF_XMIN, &
            KG_BIN_REF_XMAX, KG_BIN_REF_F)
        if (allocated(error)) return
        call check_binned_case(error, "BIN_REF_IN", KG_BIN_REF_IN_NC, KG_BIN_REF_IN_XMIN, &
            KG_BIN_REF_IN_XMAX, KG_BIN_REF_IN_F)
        if (allocated(error)) return
        call check_binned_case(error, "BIN_REN", KG_BIN_REN_NC, KG_BIN_REN_XMIN, &
            KG_BIN_REN_XMAX, KG_BIN_REN_F)
        if (allocated(error)) return
        call check_binned_case(error, "BIN_LIN", KG_BIN_LIN_NC, KG_BIN_LIN_XMIN, &
            KG_BIN_LIN_XMAX, KG_BIN_LIN_F)

    end subroutine test_binned_golden

    !> Builds one binned golden case's grid and compares every cell with the oracle's.
    subroutine check_binned_case(error, name, nc, xmin, xmax, want)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        character(len=*), intent(in) :: name    !! the case
        integer, intent(in)          :: nc      !! its cells
        real(real64), intent(in)     :: xmin    !! its range's lower end
        real(real64), intent(in)     :: xmax    !! its range's upper end
        real(real64), intent(in)     :: want(:) !! the oracle's density at each cell

        type(pf_kde_grid) :: g
        real(real64), allocatable :: x(:), f(:)
        real(real64) :: worst
        character(len=200) :: msg
        integer :: i

        call kde_fixture(32_int64, x)
        select case (name)
        case ("BIN_GAUSS")
            call g%init(nc, xmin, xmax, 60.0_real64, kernel="gaussian", method="binned")
        case ("BIN_EPAN")
            call g%init(nc, xmin, xmax, 60.0_real64, kernel="epanechnikov", method="binned")
        case ("BIN_BOX")
            call g%init(nc, xmin, xmax, 60.0_real64, kernel="box", method="binned")
        case ("BIN_BSPL")
            call g%init(nc, xmin, xmax, 60.0_real64, kernel="bspline", method="binned")
        case ("BIN_REF")
            call g%init(nc, xmin, xmax, 60.0_real64, kernel="gaussian", lower=-470.0_real64, upper=460.0_real64, &
                boundary="reflect", method="binned")
        case ("BIN_REF_IN")
            call g%init(nc, xmin, xmax, 60.0_real64, kernel="epanechnikov", lower=-470.0_real64, &
                upper=460.0_real64, boundary="reflect", method="binned")
        case ("BIN_REN")
            call g%init(nc, xmin, xmax, 60.0_real64, kernel="gaussian", lower=-470.0_real64, boundary="renormalise", &
                method="binned")
        case ("BIN_LIN")
            call g%init(nc, xmin, xmax, 60.0_real64, kernel="gaussian", lower=-470.0_real64, upper=460.0_real64, &
                boundary="linear", method="binned")
        case default
            error stop "check_binned_case: unknown case " // name
        end select
        call g%add(x, finish=.true.)
        allocate(f(nc))
        call g%density(f)
        worst = 0.0_real64
        do i = 1, nc
            worst = max(worst, abs(f(i) - want(i)))
        end do
        write(msg, '(3a,es10.3)') "binned golden ", name, ": the largest gap from the oracle is ", worst
        call check(error, worst <= 1.0e-13_real64*maxval(abs(want)), trim(msg))

    end subroutine check_binned_case

    !> The ISJ rule's grid starts at a bound that clips it, so a point sitting ON that bound falls
    !> outside the grid's first or last CENTRE and is counted as mass beyond the ends rather than
    !> binned. Both ends are exercised here, with weights and without: the two answers must agree
    !> bit for bit, because equal reliability weights are no weights at all (`test_isj_weights`
    !> asserts that convention on an unbounded sample; this one asserts it where the clipped ends
    !> carry the weight).
    !!
    !> The sample is the square-root quantile grid, whose density `1/(2 sqrt(x))` varies over the
    !> range -- the rule finds no root on a flat one -- with its ends moved onto the bounds.
    subroutine test_isj_counts_points_on_its_own_bounds(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde) :: kw, ku
        real(real64) :: x(600), w(600)
        integer :: i
        logical :: okw, oku
        character(len=160) :: msg

        do i = 1, 600
            x(i) = sqrt((real(i, real64) - 0.5_real64)/600.0_real64)
        end do
        x(1) = 0.0_real64
        x(600) = 1.0_real64
        w = 1.0_real64
        call kw%fit(x, rule="isj", weights=w, weight_type="reliability", lower=0.0_real64, &
            upper=1.0_real64, ok=okw)
        call ku%fit(x, rule="isj", lower=0.0_real64, upper=1.0_real64, ok=oku)
        call check(error, okw .and. oku, "the rule must find a bandwidth on both arms, or the " // &
            "clipped ends are never reached")
        if (allocated(error)) return
        write(msg, '(a,es24.16,a,es24.16)') "with equal reliability weights the rule gives ", &
            kw%bandwidth(), " and without them ", ku%bandwidth()
        call check(error, kw%bandwidth() == ku%bandwidth(), trim(msg))

    end subroutine test_isj_counts_points_on_its_own_bounds

    !> The LSCV rule's criterion reads the whole population where it is small enough and a
    !> subsample of `KDE_LSCV_MAX` points otherwise, and each arm carries the weights separately.
    !> Both are taken here, and both must answer what the unweighted fit answers, bit for bit:
    !> equal reliability weights are no weights.
    !!
    !> 500 points takes the first arm and 2500 the second; the recipe is one deterministic
    !> oscillation, which has a scale for the rule to find.
    subroutine test_lscv_weights_reach_both_subsample_arms(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde) :: kw, ku
        real(real64), allocatable :: x(:), w(:)
        integer :: i, n, pass
        logical :: okw, oku
        character(len=200) :: msg

        do pass = 1, 2
            n = 500
            if (pass == 2) n = 2500
            if (allocated(x)) deallocate(x, w)
            allocate(x(n), w(n))
            do i = 1, n
                x(i) = sin(real(i, real64)*0.7_real64) + 0.3_real64*real(mod(i, 7), real64)
            end do
            w = 1.0_real64
            call kw%fit(x, rule="lscv", weights=w, weight_type="reliability", ok=okw)
            call ku%fit(x, rule="lscv", ok=oku)
            write(msg, '(a,i0,a,es24.16,a,es24.16)') "over ", n, " points the weighted rule gives ", &
                kw%bandwidth(), " and the unweighted one ", ku%bandwidth()
            call check(error, okw .and. oku, "the rule must find a bandwidth on both arms")
            if (allocated(error)) return
            call check(error, kw%bandwidth() == ku%bandwidth(), trim(msg))
            if (allocated(error)) return
        end do

    end subroutine test_lscv_weights_reach_both_subsample_arms

    !> An adaptive fit bounded ABOVE alone, under `"linear"`: its pilot keeps R2 by pushing its
    !> FREE edge -- the lower one -- a whole kernel reach below the bound, which is the mirror of
    !> what `test_pilot_linear_keeps_r2` asserts for a lower bound.
    subroutine test_an_upper_bounded_linear_pilot_keeps_r2(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde) :: k
        type(pf_kde_grid) :: p
        real(real64) :: x(500), lo, hi, reach
        real(real64), allocatable :: c(:)
        integer :: i
        logical :: ok
        character(len=200) :: msg

        ! The density `2(1 - x)` on `(-infinity, 1]` truncated to one bandwidth's worth of tail:
        ! the mass piles at the bound, which is where a one-sided correction is worth making.
        do i = 1, 500
            x(i) = 1.0_real64 - sqrt((real(i, real64) - 0.5_real64)/500.0_real64)
        end do
        call k%fit(x, rule="silverman", adaptive=.true., upper=1.0_real64, boundary="linear", ok=ok)
        call check(error, ok, "the upper-bounded linear fit must be defined")
        if (allocated(error)) return
        call k%pilot(p)
        allocate(c(p%ncells()))
        call p%grid(c)
        ! The centres are half a cell inside the range, so the range is recovered from them.
        lo = c(1) - 0.5_real64*p%step()
        hi = c(size(c)) + 0.5_real64*p%step()
        ! The B-spline is the default kernel, whose support radius is `2 sqrt(3)` bandwidths.
        reach = 2.0_real64*sqrt(3.0_real64)*p%bandwidth()
        write(msg, '(a,es14.6,a,es14.6,a,es14.6)') "the pilot runs from ", lo, " to ", hi, &
            " against a reach of ", reach
        ! The range is recovered from the centres, so it carries their rounding; a few units of it
        ! is ample against a reach the width must clear by a whole bandwidth.
        call check(error, abs(hi - 1.0_real64) <= 8.0_real64*spacing(1.0_real64), &
            "R1: the pilot's range must end at the bound")
        if (allocated(error)) return
        call check(error, hi - lo >= reach, trim(msg))

    end subroutine test_an_upper_bounded_linear_pilot_keeps_r2

    !> A pilot under a CORRECTED boundary holds clipped densities rather than weights, so a pilot
    !> whose cells hold no mass leaves the rule nothing to read -- and the grid reading it answers
    !> NaN, exactly as it does for an uncorrected pilot with nothing in its cells.
    subroutine test_a_corrected_pilot_with_no_mass(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde_grid) :: p, g
        real(real64) :: d(16)

        call p%init(64, 0.0_real64, 1.0_real64, 0.1_real64, lower=0.0_real64, boundary="linear")
        call p%finish()
        call g%init(16, 0.0_real64, 1.0_real64, 0.1_real64, lower=0.0_real64, boundary="linear", pilot=p)
        call g%add([0.2_real64, 0.5_real64, 0.8_real64])
        call g%finish()
        call g%density(d)
        call check(error, g%is_adaptive(), "the grid must be adaptive, or the pilot is never read")
        if (allocated(error)) return
        call check(error, all(ieee_is_nan(d)), &
            "a grid whose corrected pilot holds no mass must answer NaN at every cell")

    end subroutine test_a_corrected_pilot_with_no_mass

    !> Every kernel token a grid accepts comes back from `%kernel`, and an unknown one is refused
    !> out of process (`kde_grid_unknown_kernel`). The four are checked together because the
    !> accessor is a table and a table is wrong one row at a time.
    subroutine test_every_grid_kernel_token_round_trips(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde_grid) :: g
        character(len=:), allocatable :: got
        integer :: i

        do i = 1, 4
            call g%init(8, 0.0_real64, 1.0_real64, 0.2_real64, kernel=trim(KERNELS(i)))
            call g%kernel(got)
            call check(error, got == trim(KERNELS(i)), "a grid made with kernel=""" // &
                trim(KERNELS(i)) // """ must answer that token, not """ // got // """")
            if (allocated(error)) return
        end do

    end subroutine test_every_grid_kernel_token_round_trips

    !> `method="binned"` carries per-point WEIGHTS through its transform: the binned cells match
    !> the exact grid's over the same weighted sample to the binning's own order, and both hold
    !> the same total weight exactly.
    subroutine test_a_binned_grid_deposits_weights(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde_grid) :: gb, ge
        real(real64) :: x(2000), w(2000), db(256), de(256)
        integer :: i
        character(len=200) :: msg

        do i = 1, 2000
            x(i) = 0.5_real64 + 0.15_real64*sin(real(i, real64))
            w(i) = 1.0_real64 + 0.5_real64*real(mod(i, 5), real64)
        end do
        call gb%init(256, 0.0_real64, 1.0_real64, 0.05_real64, method="binned")
        call gb%add(x, weights=w)
        call gb%finish()
        call ge%init(256, 0.0_real64, 1.0_real64, 0.05_real64)
        call ge%add(x, weights=w)
        call ge%finish()
        call gb%density(db)
        call ge%density(de)
        call check(error, gb%sum_weights() == ge%sum_weights(), &
            "both methods must hold the same total weight")
        if (allocated(error)) return
        ! The binned deposit differs from the exact one by the binning's error, which
        ! `test_binned_converges` pins as the square of the cell width; here the cells are a
        ! sixteenth of a bandwidth, so a thousandth of the peak is ample and a wrong weight is not.
        write(msg, '(a,es12.4,a,es12.4)') "the binned and exact weighted cells differ by at most ", &
            maxval(abs(db - de)), " against a peak of ", maxval(de)
        call check(error, maxval(abs(db - de)) <= 1.0e-3_real64*maxval(de), trim(msg))

    end subroutine test_a_binned_grid_deposits_weights

    !> A binned ADAPTIVE grid splits its points across a ladder of bandwidth classes, and its
    !> `%print` says how many; a fixed binned grid has one class and prints no such line.
    subroutine test_a_binned_adaptive_grid_prints_its_classes(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde_grid) :: p, ga, gf
        real(real64) :: x(2000), d(256)
        integer :: i

        do i = 1, 2000
            x(i) = 2.0_real64*sqrt((real(i, real64) - 0.5_real64)/2000.0_real64)
        end do
        call p%init(256, 0.0_real64, 2.0_real64, 0.1_real64)
        call p%add(x)
        call p%finish()
        call ga%init(256, 0.0_real64, 2.0_real64, 0.1_real64, pilot=p, alpha=0.5_real64, method="binned")
        call ga%add(x)
        call ga%finish()
        call gf%init(256, 0.0_real64, 2.0_real64, 0.1_real64, method="binned")
        call gf%add(x)
        call gf%finish()
        call ga%print()
        call gf%print()
        call ga%density(d)
        call check(error, ga%is_adaptive() .and. .not. gf%is_adaptive(), &
            "one grid must be adaptive and the other fixed, or the two printers are the same")
        if (allocated(error)) return
        call check(error, all(d >= 0.0_real64) .and. maxval(d) > 0.0_real64, &
            "the binned adaptive grid must hold a density to have printed a summary of")

    end subroutine test_a_binned_adaptive_grid_prints_its_classes

    !> `%merge` compares the two grids' adaptive settings, `bandwidth_max` among them: two grids
    !> given the SAME explicit cap merge, and the merged cells are one grid's over the whole
    !> sample. A differing cap is refused out of process (`kde_grid_merge_alpha` is its sibling).
    subroutine test_merge_keeps_an_explicit_bandwidth_max(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde_grid) :: p, a, b, whole
        real(real64) :: x(400), da(64), dw(64)
        integer :: i
        character(len=160) :: msg

        do i = 1, 400
            x(i) = 2.0_real64*sqrt((real(i, real64) - 0.5_real64)/400.0_real64)
        end do
        call p%init(128, 0.0_real64, 2.0_real64, 0.1_real64)
        call p%add(x)
        call p%finish()
        call a%init(64, 0.0_real64, 2.0_real64, 0.1_real64, pilot=p, alpha=0.5_real64, &
            bandwidth_max=0.4_real64)
        call b%init(64, 0.0_real64, 2.0_real64, 0.1_real64, pilot=p, alpha=0.5_real64, &
            bandwidth_max=0.4_real64)
        call whole%init(64, 0.0_real64, 2.0_real64, 0.1_real64, pilot=p, alpha=0.5_real64, &
            bandwidth_max=0.4_real64)
        call a%add(x(1:200))
        call b%add(x(201:400))
        call whole%add(x)
        call a%merge(b)
        call a%finish()
        call whole%finish()
        call a%density(da)
        call whole%density(dw)
        ! Not bit for bit: the merged grid accumulates its two halves in a different order from the
        ! whole one, so the two differ by the rounding of that reassociation and nothing else.
        write(msg, '(a,es12.4,a,es12.4)') "the merged and whole capped grids differ by at most ", &
            maxval(abs(da - dw)), " against a peak of ", maxval(abs(dw))
        call check(error, maxval(abs(da - dw)) <= 1.0e-14_real64*maxval(abs(dw)), trim(msg))

    end subroutine test_merge_keeps_an_explicit_bandwidth_max

    !> `spread_max=` on a GRID caps `h_max/h_min` there exactly as it does on a fit: the capped
    !> grid's bandwidths span no more than the cap, and an uncapped grid over the same pilot spans
    !> more, so the cap is what did it.
    subroutine test_the_grids_spread_max_caps_its_bandwidths(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde_grid) :: p, capped, free
        real(real64) :: x(400), dc(64), df(64)
        integer :: i
        character(len=200) :: msg

        do i = 1, 400
            x(i) = 2.0_real64*sqrt((real(i, real64) - 0.5_real64)/400.0_real64)
        end do
        call p%init(128, 0.0_real64, 2.0_real64, 0.1_real64)
        call p%add(x)
        call p%finish()
        call capped%init(64, 0.0_real64, 2.0_real64, 0.1_real64, pilot=p, alpha=0.5_real64, &
            spread_max=1.5_real64)
        call free%init(64, 0.0_real64, 2.0_real64, 0.1_real64, pilot=p, alpha=0.5_real64)
        call capped%add(x)
        call free%add(x)
        call capped%finish()
        call free%finish()
        call capped%density(dc)
        call free%density(df)
        write(msg, '(a,es12.4)') "the capped and uncapped grids differ by at most ", &
            maxval(abs(dc - df))
        call check(error, capped%is_adaptive() .and. free%is_adaptive(), &
            "both grids must be adaptive")
        if (allocated(error)) return
        call check(error, maxval(abs(dc - df)) > 0.0_real64, trim(msg))

    end subroutine test_the_grids_spread_max_caps_its_bandwidths

    !> `%bandwidths` on an UNDEFINED fit answers NaN for every bandwidth, and NaN for every point
    !> too where the population was never kept -- which is what a NaN under `skipnan = .false.`
    !> leaves behind.
    subroutine test_an_undefined_fit_reports_no_bandwidths(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde) :: k
        real(real64) :: x(5), h(5), xx(5)
        logical :: ok

        x = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64, 5.0_real64]
        x(3) = ieee_value(1.0_real64, ieee_quiet_nan)
        call k%fit(x, bandwidth=0.5_real64, skipnan=.false., ok=ok)
        call check(error, (.not. ok) .and. k%n_valid() == 5_int64, &
            "a kept NaN must leave a fitted, undefined estimate over the whole sample")
        if (allocated(error)) return
        call k%bandwidths(h, xx)
        call check(error, all(ieee_is_nan(h)), "an undefined fit must answer NaN for every bandwidth")
        if (allocated(error)) return
        call check(error, all(ieee_is_nan(xx)), &
            "an undefined fit that kept no points must answer NaN for every point")

    end subroutine test_an_undefined_fit_reports_no_bandwidths

    !> A `method="binned"` fit answers the SCALAR `%pdf` and `%cdf` and the ARRAY `%quantile` from
    !> its own grid, which the array forms already do: each scalar answer is the one-element array
    !> answer at the same point, bit for bit, and the array quantile is the scalar quantile
    !> element by element.
    subroutine test_a_binned_fit_answers_from_its_grid_everywhere(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde) :: k
        real(real64) :: x(2000), t(8), f1(8), c1(8), p(8), q1(8)
        real(real64) :: fs, cs, qs
        integer :: i
        logical :: ok
        character(len=160) :: msg

        do i = 1, 2000
            x(i) = 2.0_real64*sqrt((real(i, real64) - 0.5_real64)/2000.0_real64)
        end do
        call k%fit(x, rule="silverman", lower=0.0_real64, upper=2.0_real64, method="binned", ok=ok)
        call check(error, ok, "the binned fit must be defined")
        if (allocated(error)) return
        call spread_points(0.1_real64, 1.9_real64, t)
        call k%pdf(t, f1)
        call k%cdf(t, c1)
        do i = 1, 8
            p(i) = (real(i, real64) - 0.5_real64)/8.0_real64
        end do
        call k%quantile(p, q1)
        do i = 1, 8
            call k%pdf(t(i), fs)
            call k%cdf(t(i), cs)
            call k%quantile(p(i), qs)
            write(msg, '(a,i0,a,3es14.6)') "at point ", i, " the scalar and array forms differ by ", &
                fs - f1(i), cs - c1(i), qs - q1(i)
            call check(error, fs == f1(i) .and. cs == c1(i) .and. qs == q1(i), trim(msg))
            if (allocated(error)) return
        end do

    end subroutine test_a_binned_fit_answers_from_its_grid_everywhere

    !> The corrected-boundary scan keeps the stretches it clips in a list it GROWS, and the list
    !> starts at eight. This sample puts ten separate negative stretches in one zone, so the list
    !> is grown while it is being filled and every stretch must survive that: the estimate is zero
    !> on each of them and never negative anywhere.
    !!
    !! **The recipe.** Under `"linear"` with a box kernel the corrected sum at a query `t` is
    !! proportional to `a2 n + |a1| S`, `S` the sum of `(x_j - t)/h` over the window and `a1, a2`
    !! the kernel's moments truncated at the bound; `a1 < 0`, so the sum goes negative where the
    !! points in the window lie far enough BELOW `t`, and it can only do so within about half a
    !! bandwidth of the bound. Thirty points whose kernels leave the window one at a time across
    !! that half bandwidth make the sum jump up ten times, and thirty-five points just below the
    !! bound hold it near zero between the jumps, so it crosses zero on either side of each one.
    subroutine test_the_clip_grows_its_list_of_stretches(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde) :: k
        real(real64), parameter :: H = 0.03_real64
        real(real64) :: x(65), w(65), q(20000), f(20000), zlo
        integer :: i, zruns, flat
        logical :: inz, ok
        character(len=200) :: msg

        do i = 1, 30
            x(i) = 1.0_real64 - H*(2.30_real64 - 0.56_real64*(real(i, real64) - 0.5_real64)/30.0_real64)
            w(i) = 10.0_real64*0.7_real64**((real(i, real64) - 0.5_real64)/30.0_real64)
        end do
        do i = 1, 35
            x(30 + i) = 1.0_real64 - H*0.333_real64*(real(i, real64) - 0.5_real64)/35.0_real64
            w(30 + i) = 1.0_real64
        end do
        call k%fit(x, weights=w, weight_type="reliability", bandwidth=H, kernel="box", &
            upper=1.0_real64, boundary="linear", ok=ok)
        call check(error, ok, "the fit must be defined")
        if (allocated(error)) return
        zlo = 1.0_real64 - sqrt(3.0_real64)*H
        do i = 1, 20000
            q(i) = zlo + (1.0_real64 - zlo)*(real(i, real64) - 0.5_real64)/20000.0_real64
        end do
        call k%pdf(q, f)
        zruns = 0
        inz = .false.
        do i = 1, 20000
            if (f(i) == 0.0_real64) then
                if (.not. inz) zruns = zruns + 1
                inz = .true.
            else
                inz = .false.
            end if
        end do
        write(msg, '(a,i0,a)') "the clip left ", zruns, &
            " separate stretches of exactly zero density inside the zone"
        call check(error, zruns >= 9, trim(msg))
        if (allocated(error)) return
        write(msg, '(a,es12.4,a,i0,a)') "the clipped estimate falls to ", minval(f), " at ", &
            count(f < 0.0_real64), " of the zone's points"
        call check(error, count(f < 0.0_real64) == 0, trim(msg))
        if (allocated(error)) return
        ! The distribution function reads the same stretches from the other side: it is flat across
        ! each one -- the clip removes the density there, so no mass accumulates -- and it rises
        ! across the zone without ever falling.
        call k%cdf(q, f)
        flat = 0
        do i = 2, 20000
            if (f(i) < f(i - 1)) then
                write(msg, '(a,i0,a,es14.6,a,es14.6)') "%cdf falls at probe ", i, ": ", f(i - 1), &
                    " then ", f(i)
                call check(error, .false., trim(msg))
                return
            end if
            if (f(i) == f(i - 1)) flat = flat + 1
        end do
        write(msg, '(a,i0,a,es14.6,a,es14.6)') "%cdf is flat across ", flat, &
            " of the zone's steps and runs from ", f(1), " to ", f(20000)
        call check(error, flat >= 9 .and. f(20000) > f(1), trim(msg))

    end subroutine test_the_clip_grows_its_list_of_stretches

    !> An adaptive rule whose exponent overflows gives a point a bandwidth the estimator cannot
    !> use, and the fit is then undefined -- quietly, as every other data condition is. The pilot
    !> here spans eight decades of density at a bandwidth near the largest number, so the cap the
    !> rule would apply is itself infinite and nothing brings the bandwidth back.
    subroutine test_an_overflowing_adaptive_rule_leaves_the_fit_undefined(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde) :: k
        type(pf_kde_grid) :: p
        real(real64) :: x(600), h(600), f
        integer :: i
        logical :: ok
        character(len=160) :: msg

        do i = 1, 600
            if (mod(i, 6) == 0) then
                x(i) = 1.0e300_real64*real(i, real64)/600.0_real64
            else
                x(i) = 1.0e296_real64*(real(i, real64) - 0.5_real64)/600.0_real64
            end if
        end do
        call p%init(4096, 0.0_real64, 1.1e300_real64, 1.0e296_real64)
        call p%add(x)
        call p%finish()
        call k%fit(x, bandwidth=1.0e306_real64, adaptive=.true., pilot=p, alpha=1.0_real64, &
            spread_max=1.0e5_real64, ok=ok)
        call k%pdf(0.0_real64, f)
        call k%bandwidths(h)
        write(msg, '(a,l1,a,es12.4)') "the fit reports ok = ", ok, " with a bandwidth of ", k%bandwidth()
        call check(error, .not. ok, trim(msg))
        if (allocated(error)) return
        call check(error, ieee_is_nan(k%bandwidth()) .and. ieee_is_nan(f) .and. all(ieee_is_nan(h)), &
            "an undefined fit answers NaN for its bandwidth, its density and every point's bandwidth")

    end subroutine test_an_overflowing_adaptive_rule_leaves_the_fit_undefined

    !> The same rule on a GRID poisons it instead: the grid keeps its geometry and answers NaN at
    !> every cell, which is what a kept NaN does to one.
    subroutine test_an_overflowing_adaptive_rule_poisons_the_grid(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde_grid) :: p, g
        real(real64) :: x(600), d(64)
        integer :: i

        do i = 1, 600
            if (mod(i, 6) == 0) then
                x(i) = 1.0e300_real64*real(i, real64)/600.0_real64
            else
                x(i) = 1.0e296_real64*(real(i, real64) - 0.5_real64)/600.0_real64
            end if
        end do
        call p%init(4096, 0.0_real64, 1.1e300_real64, 1.0e296_real64)
        call p%add(x)
        call p%finish()
        call g%init(64, 0.0_real64, 1.1e300_real64, 1.0e306_real64, pilot=p, alpha=1.0_real64, &
            spread_max=1.0e5_real64)
        call g%add(x)
        call g%finish()
        call g%density(d)
        call check(error, g%is_finished() .and. g%ncells() == 64, &
            "the grid must keep its geometry")
        if (allocated(error)) return
        call check(error, all(ieee_is_nan(d)), &
            "a grid whose rule can only overflow must answer NaN at every cell")

    end subroutine test_an_overflowing_adaptive_rule_poisons_the_grid

    !> A sample beyond half the largest number leaves no range to build a pilot over, and none to
    !> lay a binned grid over either: both arms answer with an undefined estimate rather than
    !> overflowing, which would end the process under a compiler that halts on IEEE overflow.
    !!
    !! **The IEEE flags are saved and restored here rather than asserted.** Reaching either arm
    !! needs every value above half the largest number, so the population's own SUM exceeds it, and
    !! `stats_compact`'s first pass -- which runs before `pf_kde` forms anything -- overflows to
    !! `+Infinity` there and leaves `IEEE_OVERFLOW` raised. When that is fixed, this test gains
    !! `call check(error, .not. any(raised), "neither arm may raise an IEEE flag")`, which is the
    !! assertion the two arms were written to earn.
    subroutine test_a_sample_beyond_half_the_largest_number(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde) :: k
        real(real64) :: x(4)
        logical :: ok
        logical :: raised(size(ieee_usual)), before(size(ieee_usual))

        x = 1.0e308_real64
        call ieee_get_flag(ieee_usual, before)
        call ieee_set_flag(ieee_usual, .false.)
        call k%fit(x, bandwidth=1.0_real64, adaptive=.true., ok=ok)
        call k%fit(x, bandwidth=1.0_real64, method="binned", ok=ok)
        call ieee_get_flag(ieee_usual, raised)
        call ieee_set_flag(ieee_usual, before)
        call check(error, .not. ok, "the binned fit must stay undefined")
        if (allocated(error)) return
        call k%fit(x, bandwidth=1.0_real64, adaptive=.true., ok=ok)
        call check(error, (.not. ok) .and. k%n_valid() == 4_int64, &
            "the adaptive fit must keep the population and stay undefined")

    end subroutine test_a_sample_beyond_half_the_largest_number

    !> A `method="binned"` fit carries per-point WEIGHTS into its grid: it answers what the exact
    !> weighted estimate answers, to the binning's own error, and holds the same total weight.
    subroutine test_a_weighted_binned_fit_matches_the_exact_one(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde) :: kb, ke
        real(real64) :: x(2000), w(2000), t(64), fb(64), fe(64)
        integer :: i
        logical :: okb, oke
        character(len=200) :: msg

        do i = 1, 2000
            x(i) = 2.0_real64*sqrt((real(i, real64) - 0.5_real64)/2000.0_real64)
            w(i) = 1.0_real64 + 0.5_real64*real(mod(i, 5), real64)
        end do
        call spread_points(0.05_real64, 1.95_real64, t)
        call kb%fit(x, weights=w, weight_type="reliability", rule="silverman", lower=0.0_real64, &
            upper=2.0_real64, method="binned", ok=okb)
        call ke%fit(x, weights=w, weight_type="reliability", rule="silverman", lower=0.0_real64, &
            upper=2.0_real64, ok=oke)
        call check(error, okb .and. oke, "both weighted fits must be defined")
        if (allocated(error)) return
        call check(error, kb%sum_weights() == ke%sum_weights(), &
            "both methods must hold the same total weight")
        if (allocated(error)) return
        call kb%pdf(t, fb)
        call ke%pdf(t, fe)
        ! `test_binned_converges` pins the binning's error as the square of the cell width; the
        ! fit's own grid resolves the kernel to a sixteenth, so a twentieth of the peak is ample
        ! and a weight carried wrongly is not.
        write(msg, '(a,es12.4,a,es12.4)') "the binned and exact weighted estimates differ by at most ", &
            maxval(abs(fb - fe)), " against a peak of ", maxval(fe)
        call check(error, maxval(abs(fb - fe)) <= 0.05_real64*maxval(fe), trim(msg))

    end subroutine test_a_weighted_binned_fit_matches_the_exact_one

    !> `%curve(method="binned")` over an ADAPTIVE, WEIGHTED fit reads the fit's own per-point
    !> bandwidths and weights into one transform: the curve follows the exact curve at the same
    !> points and is never negative.
    subroutine test_a_binned_curve_of_an_adaptive_weighted_fit(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde) :: k
        real(real64) :: x(2000), w(2000), cb(128), fb(128), ce(128), fe(128)
        integer :: i
        logical :: ok
        character(len=200) :: msg

        do i = 1, 2000
            x(i) = 2.0_real64*sqrt((real(i, real64) - 0.5_real64)/2000.0_real64)
            w(i) = 1.0_real64 + 0.5_real64*real(mod(i, 5), real64)
        end do
        call k%fit(x, weights=w, weight_type="reliability", rule="silverman", adaptive=.true., ok=ok)
        call check(error, ok .and. k%is_adaptive(), "the adaptive weighted fit must be defined")
        if (allocated(error)) return
        call k%curve(cb, fb, method="binned")
        call k%curve(ce, fe, method="exact")
        call check(error, maxval(abs(cb - ce)) == 0.0_real64, &
            "both methods must put the curve on the same points")
        if (allocated(error)) return
        call check(error, minval(fb) >= 0.0_real64, "a binned curve is never negative")
        if (allocated(error)) return
        write(msg, '(a,es12.4,a,es12.4)') "the binned and exact curves differ by at most ", &
            maxval(abs(fb - fe)), " against a peak of ", maxval(fe)
        call check(error, maxval(abs(fb - fe)) <= 0.05_real64*maxval(fe), trim(msg))

    end subroutine test_a_binned_curve_of_an_adaptive_weighted_fit

    !> A `method="binned"` fit under `"linear"` with ONE bound widens its range to a whole kernel
    !> reach at the free edge, exactly as `pf_kde_grid%init`'s R2 requires of a grid; both sides
    !> are checked, since the two are separate arms of the same rule.
    subroutine test_a_binned_linear_fit_bounded_on_one_side(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde) :: klo, khi
        real(real64) :: x(2000), t(64), flo(64), fhi(64)
        integer :: i
        logical :: oklo, okhi
        character(len=200) :: msg

        ! An exponential tail piled against the bound, which is where a one-sided correction earns
        ! its cost; the upper arm is its mirror, so the two must answer the same numbers reversed.
        do i = 1, 2000
            x(i) = -log(1.0_real64 - (real(i, real64) - 0.5_real64)/2000.0_real64)
        end do
        call spread_points(0.01_real64, 3.0_real64, t)
        call klo%fit(x, rule="silverman", lower=0.0_real64, boundary="linear", method="binned", ok=oklo)
        call khi%fit(-x, rule="silverman", upper=0.0_real64, boundary="linear", method="binned", ok=okhi)
        call check(error, oklo .and. okhi, "both one-sided binned linear fits must be defined")
        if (allocated(error)) return
        call klo%pdf(t, flo)
        call khi%pdf(-t, fhi)
        write(msg, '(a,es12.4,a,es12.4)') "the lower- and upper-bounded arms differ by at most ", &
            maxval(abs(flo - fhi)), " against a peak of ", maxval(flo)
        call check(error, maxval(abs(flo - fhi)) <= 1.0e-12_real64*maxval(flo), trim(msg))

    end subroutine test_a_binned_linear_fit_bounded_on_one_side

    !> The Gaussian kernel is CUT at five standard deviations: a variate beyond it is drawn again,
    !> so the fitted kernel is the cut one and no draw ever lands past `5 h` from its point.
    !>
    !> Both samplers that apply the cut are exercised, each at a stream whose FIRST variate is
    !> beyond it (found by walking `pf_random_normal_at` over the streams of this seed, which is
    !> what the assertion re-derives): the corrected boundary's interior sampler, at draw three of
    !> the stream, and `"renormalise"`'s, at draw two. Each fit has ONE point and a bound its
    !> kernel cannot reach, so the point and the bandwidth are known and the draw is the next
    !> variate exactly.
    subroutine test_a_variate_beyond_the_gaussian_cut_is_redrawn(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        integer(int64), parameter :: SEED = 20260919_int64
        type(pf_kde) :: kr, ki
        real(real64) :: v(1), e1, e2
        integer(int64) :: key, sk
        character(len=200) :: msg

        key = pf_random_key(SEED, KDE_LABEL)

        ! ---- "renormalise": the point is chosen at draw one, the variate at draw two ----
        sk = pf_random_key(202069_int64, 1_int64)
        e1 = pf_random_normal_at(key, sk, 2_int64)
        e2 = pf_random_normal_at(key, sk, 3_int64)
        call check(error, abs(e1) > 5.0_real64, &
            "the first variate of this stream must lie beyond the cut, or nothing is redrawn")
        if (allocated(error)) return
        call kr%fit([0.0_real64], weights=[2.0_real64], weight_type="reliability", &
            bandwidth=1.0_real64, kernel="gaussian", lower=-50.0_real64, boundary="renormalise")
        call kr%sample(v, SEED, 202069)
        write(msg, '(a,es20.12,a,es20.12)') "the renormalised draw is ", v(1), &
            " against the second variate ", e2
        call check(error, v(1) == e2, trim(msg))
        if (allocated(error)) return

        ! ---- "linear", no zone reached: the zone is chosen at draw one, the point at draw two ----
        sk = pf_random_key(7351_int64, 1_int64)
        e1 = pf_random_normal_at(key, sk, 3_int64)
        e2 = pf_random_normal_at(key, sk, 4_int64)
        call check(error, abs(e1) > 5.0_real64, &
            "the first interior variate of this stream must lie beyond the cut")
        if (allocated(error)) return
        call ki%fit([2.0_real64], bandwidth=0.05_real64, kernel="gaussian", lower=0.0_real64, &
            upper=5.0_real64, boundary="linear")
        call ki%sample(v, SEED, 7351)
        write(msg, '(a,es20.12,a,es20.12)') "the interior draw is ", v(1), " against ", &
            2.0_real64 + 0.05_real64*e2
        call check(error, v(1) == 2.0_real64 + 0.05_real64*e2, trim(msg))

    end subroutine test_a_variate_beyond_the_gaussian_cut_is_redrawn

    !> `%sample` under `"linear"` draws from the UPPER zone as it does from the lower one: every
    !> draw stays inside the support, and the draws in the zone follow the clipped estimate there
    !> -- their share of the sample is the estimate's own mass over the zone, to five standard
    !> errors.
    subroutine test_the_upper_zone_sampler(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        integer(int64), parameter :: SEED = 20260919_int64
        integer, parameter :: N = 20000
        type(pf_kde) :: k
        real(real64) :: x(500), v(N), edge, p_edge, p_hi, want, got, se
        integer :: i
        logical :: ok
        character(len=200) :: msg

        ! The density `2(1 - x)` on `(-infinity, 1]`, piled against the bound.
        do i = 1, 500
            x(i) = 1.0_real64 - sqrt((real(i, real64) - 0.5_real64)/500.0_real64)
        end do
        call k%fit(x, bandwidth=0.2_real64, upper=1.0_real64, boundary="linear", ok=ok)
        call check(error, ok, "the upper-bounded linear fit must be defined")
        if (allocated(error)) return
        call k%sample(v, SEED, 5)
        call check(error, count(ieee_is_nan(v)) == 0 .and. maxval(v) <= 1.0_real64, &
            "every draw must be a number inside the support")
        if (allocated(error)) return
        ! The zone is the bound's own kernel reach: the B-spline's radius is `2 sqrt(3)`.
        edge = 1.0_real64 - 2.0_real64*sqrt(3.0_real64)*0.2_real64
        call k%cdf(edge, p_edge)
        call k%cdf(1.0_real64, p_hi)
        want = p_hi - p_edge
        got = real(count(v > edge), real64)/real(N, real64)
        se = sqrt(want*(1.0_real64 - want)/real(N, real64))
        write(msg, '(a,f9.6,a,f9.6,a,f9.6)') "the zone holds ", want, " of the mass and ", got, &
            " of the draws, against a standard error of ", se
        call check(error, abs(got - want) <= 5.0_real64*se, trim(msg))

    end subroutine test_the_upper_zone_sampler

    !> Where the kernel is wider than half the support the two corrected zones MEET: nothing lies
    !> between them, and a draw that rounds into the gap is placed in whichever zone is on rather
    !> than in an interior that does not exist. Every draw must still land inside the support.
    !>
    !> **`M` and `N` are small on purpose, and cost is the reason.** This is the most expensive
    !> shape the sampler has. Every term is cut at BOTH bounds, so each one is built by
    !> `centred_moments`' eight-point Gauss-Legendre quadrature rather than by a closed form; and
    !> with the zones meeting there is no interior to draw from, so every draw inverts a zone's own
    !> integral -- a root find whose every step sums the whole fit. The cost is therefore
    !> proportional to `M*N`, measured at 4.4e-5 s per unit on one core. Written first with
    !> `M = 500, N = 4000` it cost 89 s of one core, and in an instrumented build it drove gcov's
    !> per-line counts on those kernel primitives past the 2**32 above which gcovr refuses to read
    !> the gcov output at all. Nothing here needs a large sample: the assertion is that a draw lands
    !> inside the support, and a sampler that breaks that breaks it systematically, not rarely.
    subroutine test_a_sample_where_the_zones_meet(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        integer(int64), parameter :: SEED = 20260919_int64
        integer, parameter :: M = 25   !! points fitted; the cost of one draw is proportional to it
        integer, parameter :: N = 400  !! draws taken
        type(pf_kde) :: k
        real(real64) :: x(M), v(N)
        integer :: i
        logical :: ok
        character(len=200) :: msg

        do i = 1, M
            x(i) = (real(i, real64) - 0.5_real64)/real(M, real64)
        end do
        ! Ten times the support's width: each zone reaches well past the other bound, so they meet.
        call k%fit(x, bandwidth=2.0_real64, lower=0.0_real64, upper=1.0_real64, &
            boundary="linear", ok=ok)
        call check(error, ok, "the two-sided linear fit must be defined")
        if (allocated(error)) return
        call k%sample(v, SEED, 6)
        write(msg, '(a,es14.6,a,es14.6,a,i0)') "the draws run from ", minval(v), " to ", maxval(v), &
            ", with NaN count ", count(ieee_is_nan(v))
        call check(error, count(ieee_is_nan(v)) == 0 .and. minval(v) >= 0.0_real64 .and. &
            maxval(v) <= 1.0_real64, trim(msg))

    end subroutine test_a_sample_where_the_zones_meet

    !> The corrected boundary scan lays a binned grid over each zone where it can and reads the
    !> exact estimator everywhere else. A zone far wider than the support leaves that grid longer
    !> than its cells allow, so the scan DECLINES it and reads every sample exactly -- which the
    !> counters say, and which changes the answer nowhere: the same fit at `method="binned"`
    !> agrees with it.
    subroutine test_the_scans_grid_is_declined_when_it_is_too_long(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde) :: k
        real(real64) :: x(64), t(8), f(8)
        integer :: i
        integer(int64) :: steps, exact
        logical :: ok
        character(len=200) :: msg

        do i = 1, 64
            x(i) = (real(i, real64) - 0.5_real64)/64.0_real64
        end do
        call spread_points(0.05_real64, 0.95_real64, t)
        ! A hundred times the support's width: the zone's transform would need far more cells than
        ! the scan's grid may hold.
        call k%fit(x, bandwidth=10.0_real64, kernel="gaussian", lower=0.0_real64, &
            upper=1.0_real64, boundary="linear", ok=ok)
        call parquet_debug_kde_scan_counts(steps, exact)
        call check(error, ok, "the fit must be defined")
        if (allocated(error)) return
        write(msg, '(a,i0,a,i0,a)') "the scan took ", steps, " samples and read ", exact, " exactly"
        call check(error, steps > 0_int64 .and. exact == steps, trim(msg))
        if (allocated(error)) return
        call k%pdf(t, f)
        call check(error, all(f > 0.0_real64) .and. all(ieee_is_finite(f)), &
            "the estimate must still answer a positive density across the support")
        if (allocated(error)) return
        ! The same refusal from the other end of the geometry: at this bandwidth the pad and the
        ! cells fit inside what the transform may hold, and the POWER OF TWO they round up to does
        ! not -- which is the third of the three lengths `kde_binned_setup` tests.
        call k%fit(x, bandwidth=2.2_real64, kernel="gaussian", lower=0.0_real64, &
            upper=1.0_real64, boundary="linear", ok=ok)
        call parquet_debug_kde_scan_counts(steps, exact)
        write(msg, '(a,i0,a,i0,a)') "at the rounded length the scan took ", steps, &
            " samples and read ", exact, " exactly"
        call check(error, ok .and. steps > 0_int64 .and. exact == steps, trim(msg))
        if (allocated(error)) return
        call k%pdf(t, f)
        call check(error, all(f > 0.0_real64) .and. all(ieee_is_finite(f)), &
            "the estimate must answer a positive density there too")

    end subroutine test_the_scans_grid_is_declined_when_it_is_too_long

    !> The scan's step rule has two arms: where a kernel reaches the current point it steps by a
    !> fraction of the narrowest such bandwidth, and where NONE does it jumps by the widest
    !> kernel's whole reach. The second is taken here: the nearest point lies between one and two
    !> reaches above the bound, so the zone's own lower end is reached by no kernel at all, and the
    !> estimate is exactly zero there.
    subroutine test_the_scan_jumps_across_a_gap_at_the_bound(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde) :: k
        real(real64) :: x(60), t(200), f(200), reach, cdf_gap
        integer :: i
        logical :: ok
        character(len=200) :: msg

        ! The Epanechnikov kernel has no scan grid at all, so the scan steps by hand. At a
        ! bandwidth of `1/sqrt(5)` its reach is exactly one, and the nearest point sits at 1.51.
        reach = 1.0_real64
        do i = 1, 60
            x(i) = 1.5_real64 + 0.01_real64*real(i, real64)
        end do
        do i = 1, 200
            t(i) = 2.3_real64*(real(i, real64) - 0.5_real64)/200.0_real64
        end do
        call k%fit(x, bandwidth=1.0_real64/sqrt(5.0_real64), kernel="epanechnikov", &
            lower=0.0_real64, boundary="linear", ok=ok)
        call check(error, ok, "the fit must be defined")
        if (allocated(error)) return
        call k%pdf(t, f)
        call k%cdf(x(1) - reach, cdf_gap)
        write(msg, '(a,i0,a,es12.4)') "the estimate is zero at ", count(f == 0.0_real64), &
            " of the 200 probes and its distribution function reaches ", cdf_gap
        call check(error, count(f == 0.0_real64) > 0, trim(msg))
        if (allocated(error)) return
        call check(error, cdf_gap == 0.0_real64, &
            "no mass may lie below the first point's own kernel, which is what the jump steps over")

    end subroutine test_the_scan_jumps_across_a_gap_at_the_bound

    !> `parquet_debug_kde_fit_nanos` reports the phases of the LAST fit: a fit with an adaptive
    !> pilot pays for one, and a fixed fit over the same sample does not.
    subroutine test_the_fit_timer_reports_the_last_fit(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde) :: k
        real(real64) :: x(4000)
        integer(int64) :: sort_a, pilot_a, lookup_a, sort_b, pilot_b, lookup_b
        integer :: i
        character(len=200) :: msg

        do i = 1, 4000
            x(i) = 2.0_real64*sqrt((real(i, real64) - 0.5_real64)/4000.0_real64)
        end do
        call k%fit(x, rule="silverman", adaptive=.true., lower=0.0_real64, upper=2.0_real64)
        call parquet_debug_kde_fit_nanos(sort_a, pilot_a, lookup_a)
        call k%fit(x, rule="silverman", lower=0.0_real64, upper=2.0_real64)
        call parquet_debug_kde_fit_nanos(sort_b, pilot_b, lookup_b)
        call check(error, sort_a >= 0_int64 .and. pilot_a >= 0_int64 .and. lookup_a >= 0_int64 .and. &
            sort_b >= 0_int64 .and. pilot_b >= 0_int64 .and. lookup_b >= 0_int64, &
            "every phase must report a duration that is not negative")
        if (allocated(error)) return
        ! Structural, not a timing: the fixed fit builds no pilot, so its second phase spans one
        ! assignment and the adaptive one spans a pass over four thousand points into a grid.
        write(msg, '(a,i0,a,i0,a)') "the adaptive fit's pilot phase took ", pilot_a, &
            " ns and the fixed fit's ", pilot_b, " ns"
        call check(error, pilot_a > pilot_b, trim(msg))

    end subroutine test_the_fit_timer_reports_the_last_fit

    !> `parquet_debug_kde_lscv_at` takes weights or leaves them out, and equal reliability weights
    !> are no weights: the criterion is the same number either way, bit for bit.
    subroutine test_the_lscv_hook_reads_weights(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        real(real64) :: x(200), w(200), with_w, without_w
        integer :: i
        logical :: ok_w, ok_plain
        character(len=200) :: msg

        do i = 1, 200
            x(i) = sin(real(i, real64)*0.7_real64)
        end do
        w = 1.0_real64
        call parquet_debug_kde_lscv_at(x, 0.2_real64, without_w, ok_plain)
        call parquet_debug_kde_lscv_at(x, 0.2_real64, with_w, ok_w, weights=w)
        call check(error, ok_plain .and. ok_w, "the criterion must be formed on both arms")
        if (allocated(error)) return
        write(msg, '(a,es24.16,a,es24.16)') "with equal weights the criterion is ", with_w, &
            " and without them ", without_w
        call check(error, with_w == without_w, trim(msg))

    end subroutine test_the_lscv_hook_reads_weights

    !> The binned method sorts the adaptive bandwidths into a ladder of classes, each a fixed
    !> factor wider than the last, and the ladder is CAPPED: a pilot whose densities span enough
    !> decades asks for more classes than the cap allows and is held to it. The grid still answers
    !> a density -- the cap coarsens the bandwidths, it does not refuse them.
    subroutine test_the_binned_class_ladder_is_capped(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde_grid) :: p, g
        real(real64) :: x(600), d(256)
        integer :: i
        character(len=200) :: msg

        ! One point in six a long way out, the rest in a tight cluster: four decades of density
        ! between the two, which an alpha of one turns into four decades of bandwidth.
        do i = 1, 600
            if (mod(i, 6) == 0) then
                x(i) = 1.0e300_real64*real(i, real64)/600.0_real64
            else
                x(i) = 1.0e296_real64*(real(i, real64) - 0.5_real64)/600.0_real64
            end if
        end do
        call p%init(4096, 0.0_real64, 1.1e300_real64, 1.0e296_real64)
        call p%add(x)
        call p%finish()
        call g%init(256, 0.0_real64, 1.1e300_real64, 1.0e290_real64, pilot=p, alpha=1.0_real64, &
            spread_max=1.0e9_real64, method="binned")
        call g%add(x)
        call g%finish()
        call g%density(d)
        write(msg, '(a,es12.4,a,es12.4)') "the capped binned grid holds densities from ", &
            minval(d), " to ", maxval(d)
        call check(error, g%is_adaptive() .and. g%is_finished(), &
            "the grid must be a finished adaptive one")
        if (allocated(error)) return
        call check(error, count(ieee_is_nan(d)) == 0 .and. maxval(d) > 0.0_real64, trim(msg))

    end subroutine test_the_binned_class_ladder_is_capped

    !> A kernel far wider than a two-sided support keeps a reflected mass that UNDERFLOWS to zero,
    !> and the deposit would divide each point's shares by it: the grid poisons itself instead, as
    !> it does for a kept NaN, rather than returning having deposited nothing.
    subroutine test_a_reflecting_grid_whose_mass_underflows(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde_grid) :: g, control
        real(real64) :: d(1), dc(1)

        ! The support is one subnormal wide and the bandwidth is ten thousand billion: the two
        ! reflected images keep a mass of about `1e-324`, which is below the smallest number.
        call g%init(1, 0.0_real64, 1.0e-310_real64, 1.0e14_real64, lower=0.0_real64, &
            upper=1.0e-310_real64, boundary="reflect")
        call g%add([5.0e-311_real64])
        call g%finish()
        call g%density(d)
        call check(error, g%n_valid() == 1_int64, "the point must be counted as retained")
        if (allocated(error)) return
        call check(error, ieee_is_nan(d(1)), &
            "a grid whose reflected mass underflowed must answer NaN")
        if (allocated(error)) return
        ! The control: the same geometry at a bandwidth whose reflected mass is an ordinary number.
        ! Normal, not subnormal -- `%init` refuses a subnormal bandwidth in its own right.
        call control%init(1, 0.0_real64, 1.0e-310_real64, 1.0e-307_real64, lower=0.0_real64, &
            upper=1.0e-310_real64, boundary="reflect")
        call control%add([5.0e-311_real64])
        call control%finish()
        call control%density(dc)
        call check(error, .not. ieee_is_nan(dc(1)) .and. dc(1) > 0.0_real64, &
            "the same grid at a usable bandwidth must answer a density")

    end subroutine test_a_reflecting_grid_whose_mass_underflows

    !> A padded reflecting grid reads its images by GATHERING: each cell takes the plain estimate
    !> at its mirror, interpolated between the two padded centres around it. A mirror landing
    !> exactly ON the last padded centre has no centre after it to interpolate towards, and the
    !> value there is that centre's own.
    !>
    !> The geometry puts one there exactly: with the range `[0, 1]` in sixteen cells the mirror of
    !> centre `k` about the upper bound sits at padded index `2*nc + 2*pad - k`, every term a whole
    !> number of cell widths and every cell width a power of two, so the arithmetic is exact.
    subroutine test_a_reflected_image_on_the_last_padded_centre(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde_grid) :: gb, ge
        real(real64) :: x(500), db(16), de(16)
        integer :: i
        character(len=200) :: msg

        do i = 1, 500
            x(i) = (real(i, real64) - 0.5_real64)/500.0_real64
        end do
        call gb%init(16, 0.0_real64, 1.0_real64, 0.05_real64, upper=1.0_real64, &
            boundary="reflect", method="binned")
        call ge%init(16, 0.0_real64, 1.0_real64, 0.05_real64, upper=1.0_real64, boundary="reflect")
        call gb%add(x)
        call ge%add(x)
        call gb%finish()
        call ge%finish()
        call gb%density(db)
        call ge%density(de)
        write(msg, '(a,es12.4,a,es12.4)') "the binned and exact reflecting grids differ by at most ", &
            maxval(abs(db - de)), " against a peak of ", maxval(de)
        call check(error, count(ieee_is_nan(db)) == 0, "the binned grid must answer a density")
        if (allocated(error)) return
        call check(error, maxval(abs(db - de)) <= 0.05_real64*maxval(de), trim(msg))

    end subroutine test_a_reflected_image_on_the_last_padded_centre

    !> Where the two corrected zones CROSS, each spans the support from its own bound to the point
    !> they meet at, and a draw in either is taken by inverting that zone's own integral rather
    !> than by the rejection sampler -- which needs a one-sided term to bound, and there is none.
    !> Every draw must still land inside the support and follow the estimate: the share of them in
    !> the upper zone is the estimate's own mass there, to five standard errors.
    !>
    !> **`M` and `N` are sized by cost, and what that gives up is stated here rather than left to
    !> be found.** Every draw inverts a zone's integral, a root find whose every step sums the
    !> whole fit, and the terms reaching both bounds are each built by `centred_moments`'
    !> eight-point Gauss-Legendre quadrature; the cost is proportional to `M*N`, measured at
    !> 2.1e-5 s per unit on one core. Written first with `M = 500, N = 20000` it cost 197 s of one
    !> core -- the slowest test in the suite by a factor of two thousand -- and in an instrumented
    !> build it drove gcov's per-line counts on the kernel primitives past the 2**32 above which
    !> gcovr refuses to read the gcov output at all.
    !>
    !> `M` is free: the reference is `%cdf` of the SAME fit, so a smaller fit changes what is
    !> compared, not whether the comparison is exact, and the crossing itself is geometry -- the
    !> zone edges are `lower + reach` and `upper - reach` whatever `M` is. `N` is not free: the
    !> five-standard-error band is `5 sqrt(w(1-w)/N)`, so cutting it from 20000 to 1500 widens the
    !> band from about two parts in a hundred to about six. That is deliberate. A sampler that
    !> takes zone draws any other way -- from the interior term, from the wrong zone, from an
    !> unnormalised integral -- misplaces tens of parts in a hundred, so six is still far inside
    !> what this test exists to catch, and the precision the first version bought was never the
    !> point. Do not raise `N` back without raising the cost knowingly.
    subroutine test_a_draw_from_two_crossing_zones(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        integer(int64), parameter :: SEED = 20260919_int64
        integer, parameter :: M = 20   !! points fitted; the cost of one draw is proportional to it
        integer, parameter :: N = 1500 !! draws taken; the five-sigma band narrows as its root
        type(pf_kde) :: k
        real(real64) :: x(M), v(N), meet, p_meet, want, got, se
        integer :: i
        logical :: ok
        character(len=200) :: msg

        do i = 1, M
            x(i) = (real(i, real64) - 0.5_real64)/real(M, real64)
        end do
        ! The B-spline's reach at this bandwidth is `2 sqrt(3) * 0.2 = 0.69`, more than half the
        ! support and less than all of it: each zone's inner edge lies past the other's, so the two
        ! cross and meet at `lower + reach`.
        meet = 2.0_real64*sqrt(3.0_real64)*0.2_real64
        call k%fit(x, bandwidth=0.2_real64, lower=0.0_real64, upper=1.0_real64, &
            boundary="linear", ok=ok)
        call check(error, ok, "the crossing-zone fit must be defined")
        if (allocated(error)) return
        call k%sample(v, SEED, 9)
        call check(error, count(ieee_is_nan(v)) == 0 .and. minval(v) >= 0.0_real64 .and. &
            maxval(v) <= 1.0_real64, "every draw must be a number inside the support")
        if (allocated(error)) return
        call k%cdf(meet, p_meet)
        want = 1.0_real64 - p_meet
        got = real(count(v > meet), real64)/real(N, real64)
        se = sqrt(want*(1.0_real64 - want)/real(N, real64))
        write(msg, '(a,f9.6,a,f9.6,a,f9.6)') "the upper zone holds ", want, " of the mass and ", &
            got, " of the draws, against a standard error of ", se
        call check(error, abs(got - want) <= 5.0_real64*se, trim(msg))

    end subroutine test_a_draw_from_two_crossing_zones

    !> The adaptive rule's bandwidth is `h (p/g)**(-alpha)`, and the exponent is tested BEFORE it
    !> is raised so that a bandwidth too large to represent comes out as the largest number rather
    !> than as an overflow -- which would end the process under a compiler that halts on one. The
    !> ladder of binned classes then stops there: this grid's widest class is the cap, not a NaN.
    !!
    !! **The flags are saved and restored rather than asserted.** `class_ladder` forms the spread
    !! cap as `h * capf` before it compares it with `bandwidth_max`, and at this bandwidth that
    !! product overflows on its way to a right answer. When that is formed without overflowing,
    !! this test gains `call check(error, .not. any(raised), "building the ladder raises no flag")`.
    subroutine test_the_class_ladder_stops_at_the_widest_bandwidth(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(pf_kde_grid) :: p, g
        real(real64) :: x(600), d(256)
        integer :: i
        logical :: raised(size(ieee_usual)), before(size(ieee_usual))
        character(len=200) :: msg

        ! One point in six far out, the rest in a tight cluster a few decades below: the pilot's
        ! smallest density is far enough under its geometric mean that the rule's own exponent
        ! passes what `h` can be multiplied by.
        do i = 1, 600
            if (mod(i, 6) == 0) then
                x(i) = 5.0e306_real64*real(i, real64)/600.0_real64
            else
                x(i) = 1.0e303_real64*(real(i, real64) - 0.5_real64)/600.0_real64
            end if
        end do
        call p%init(4096, 0.0_real64, 5.1e306_real64, 1.0e303_real64)
        call p%add(x)
        call p%finish()
        call ieee_get_flag(ieee_usual, before)
        call ieee_set_flag(ieee_usual, .false.)
        call g%init(256, 0.0_real64, 5.0e306_real64, 1.0e307_real64, pilot=p, alpha=1.0_real64, &
            bandwidth_max=1.5e307_real64, method="binned")
        call g%add(x)
        call g%finish()
        call g%density(d)
        call ieee_get_flag(ieee_usual, raised)
        call ieee_set_flag(ieee_usual, before)
        write(msg, '(a,es12.4,a,es12.4)') "the grid's densities run from ", minval(d), " to ", maxval(d)
        call check(error, g%is_adaptive() .and. g%is_finished(), &
            "the grid must be a finished adaptive one")
        if (allocated(error)) return
        call check(error, count(ieee_is_nan(d)) == 0 .and. maxval(d) > 0.0_real64, trim(msg))

    end subroutine test_the_class_ladder_stops_at_the_widest_bandwidth

    !> `%cdf` inside a corrected zone reads each point in the query's window one of three ways: by
    !> its whole integral over the zone where its kernel lies wholly on the bound's side of the
    !> query, by a partial integral where it straddles it, and not at all where it lies beyond.
    !> All three arms need an ADAPTIVE fit at an UPPER bound: with one bandwidth the window's own
    !> edge is exactly where a kernel stops reaching, so only a point sitting on it takes the
    !> first arm, while narrow kernels inside a wide window take it for a whole interval of
    !> queries.
    !>
    !> The assertion is the one that holds whatever the three arms are: `%cdf` rises across the
    !> zone and integrates `%pdf` over it, to the quadrature's own tolerance.
    subroutine test_an_adaptive_upper_zone_reads_its_window_three_ways(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check
        type(kde_density) :: fn
        type(pf_integration_info) :: info
        real(real64) :: x(2000), t(64), c(64), edge, c_edge, c_hi, want
        integer :: i
        logical :: ok, conv
        character(len=200) :: msg

        ! The density `2(1 - x)` on `(-infinity, 1]`: dense at the bound and sparse well below it,
        ! so the rule gives the points near the bound much narrower kernels than the rest.
        do i = 1, 2000
            x(i) = 1.0_real64 - sqrt((real(i, real64) - 0.5_real64)/2000.0_real64)
        end do
        call fn%k%fit(x, rule="silverman", adaptive=.true., upper=1.0_real64, &
            boundary="linear", ok=ok)
        fn%power = 0
        call check(error, ok .and. fn%k%is_adaptive(), "the adaptive upper-bounded fit must be defined")
        if (allocated(error)) return
        ! Ascending, unlike `spread_points`'s low-discrepancy sequence: the first assertion is
        ! that the distribution function never falls as the query rises.
        do i = 1, 64
            t(i) = 0.1_real64 + 0.89_real64*(real(i, real64) - 0.5_real64)/64.0_real64
        end do
        call fn%k%cdf(t, c)
        do i = 2, 64
            if (c(i) >= c(i - 1)) cycle
            write(msg, '(a,i0,a,es14.6,a,es14.6)') "%cdf falls at probe ", i, ": ", c(i - 1), &
                " then ", c(i)
            call check(error, .false., trim(msg))
            return
        end do
        ! The zone is the bound's own kernel reach at the WIDEST bandwidth, so its inner edge is
        ! read from the fit rather than assumed; the mass across it is the integral of `%pdf`.
        edge = 0.5_real64
        call fn%k%cdf(edge, c_edge)
        call fn%k%cdf(1.0_real64, c_hi)
        want = pf_integrate(fn, edge, 1.0_real64, 1.0e-10_real64, converged=conv, info=info)
        write(msg, '(a,es14.6,a,es14.6,a,es10.3,a,l1)') "across the zone %cdf gives ", c_hi - c_edge, &
            " and the integral of %pdf ", want, ", differing by ", (c_hi - c_edge) - want, &
            "; converged ", conv
        call check(error, conv, "the reference integral must converge over the zone")
        if (allocated(error)) return
        call check(error, abs((c_hi - c_edge) - want) <= 1.0e-8_real64, trim(msg))

    end subroutine test_an_adaptive_upper_zone_reads_its_window_three_ways

end module test_kde
