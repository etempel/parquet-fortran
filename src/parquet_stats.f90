!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
! GENERATED FILE -- DO NOT EDIT BY HAND.
! Regenerate with:  tools/generate_parquet_stats.py
! The kind table lives in tools/generate_parquet_columns.py; edit it there, not here.
!
!> Array statistics over plain Fortran arrays -- the `pf_*` reduction family.
!!
!! **This module is Arrow-free and must stay that way.** Nothing in its closure may reach
!! `parquet_bindings`; `check_parquet_stats_stays_arrow_free` (tools/check_source_conventions.py)
!! walks the closure, submodules included, and fails if it ever does. That is what lets a program
!! summarise arrays it already has without compiling the C++ wrapper's dependencies into its build.
!!
!! **`use parquet_sorting` is the tier edge, and it is deliberate.** The statistics that need an
!! order -- every quantile, the median absolute deviation, the clipped estimators -- take their
!! ordering from `pf_argsort` and their selection from `pf_nth_element` rather than carrying a
!! second sorting implementation, and the `parquet_column` entry points take their storage access
!! from the same tier. `tools/module_footprints.txt` records what that import costs a consumer, and
!! `tools/check_module_footprints.sh` fails if it grows.
!!
!! **The optional-argument order is a permanent compatibility contract.** Every procedure here
!! declares its optional dummies as a SUBSEQUENCE of one canonical sequence --
!! `is_valid, weights, weight_type, ddof, bias, excess, skipnan, method, scale, center, out_valid,`
!! `converged, n_null, n_nan, ok, threads` -- so that a reader who has seen one signature has seen
!! them all. `check_stats_optional_argument_order` enforces it against this file.
!!
!! **The family's standing rules**, stated once here rather than in thirty doc-comments:
!!
!! * a **null is excluded** from the population (`skipna = .true.`, as pandas does), and a
!!   procedure that can report how many were excluded takes `n_null`;
!! * a **NaN is excluded too**, by default, on the kinds that can hold one -- matching `pf_minmax`,
!!   which has always skipped them, and matching pandas; `skipnan = .false.` restores numpy's
!!   propagating behaviour;
!! * **exclusion is ORDERED -- nullness, then NaN, then weight** -- so an element that is already
!!   out of the population never has its weight examined, and a weight column that is NaN exactly
!!   where the value column is null costs nothing;
!! * a **zero weight removes the element** from the population, which is how a caller says "drop
!!   this row"; a negative, NaN or infinite weight is a MISUSE and aborts;
!! * an **undefined answer is a quiet NaN**, never an abort. This module aborts on misuse and never
!!   on a data condition, because a per-group loop meets an empty group on real data.
!!
!! Design and staging: feature_pandas_S4.md. User guide: doc/pages/utilities/statistics.md.
module parquet_stats
    ! The tier edge. Unrestricted rather than `only:` -- this module's default accessibility is
    ! `private`, so nothing is re-exported, and the later phases reach `pf_argsort`,
    ! `pf_nth_element` and `pf_sort_threads` through it.
    use parquet_sorting
    ! **The second tier edge, and the only one that adds a file to the footprint.**
    ! `pf_normal_scores` maps a rank to a normal quantile, which is `Phi**(-1)`, and the only
    ! alternative to importing it is a second `Phi**(-1)` in this module. `feature_risks.md`
    ! Risk-252 records what two spellings of one number cost when the number is a single
    ! CONSTANT; a duplicated kernel is that defect with far more surface.
    !
    ! **`parquet_utils` is a LEAF** -- it imports the intrinsic `iso_fortran_env` and
    ! `ieee_arithmetic` and nothing else, no module of this library -- so the edge costs
    ! `tools/module_footprints.txt` exactly one file and adds nothing beneath it, and
    ! `check_parquet_stats_stays_arrow_free` is untouched because no Arrow is reachable from it.
    ! `only:` rather than bare, unlike `parquet_sorting` above: one name is needed, and a
    ! consumer who wants the probit itself should say `use parquet_utils` and get the whole
    ! family rather than the one specific this module happens to need.
    use parquet_utils, only : pf_probit
    ! `parquet_sorting` imports these with an `only:` list and does not re-export them, so this
    ! module names them itself. Nothing new enters the dependency graph: `parquet_columns` is
    ! already in it, through `parquet_sorting`'s own `pf_argsort` over a column.
    ! The thread rule, named directly for the same reason: `parquet_sorting` is private by default
    ! and re-exports `pf_sort_threads` but not `resolve_thread_count`, which is the procedure that
    ! honours an explicit `threads=`, refuses a nested team where libgomp would deadlock, and clamps
    ! to the processors actually available. Reusing it rather than re-deriving the rule here is what
    ! the design's settings analysis requires -- this module adds no thread knob of its own -- and
    ! `parquet_argsort` is already in the graph beneath `parquet_sorting`, so nothing new enters it.
    use parquet_argsort, only : resolve_thread_count
    use parquet_columns, only : parquet_column, parquet_kind_name, parquet_column_data_ptr, &
        PK_INT32, PK_INT64, PK_FLOAT32, PK_FLOAT64, PK_LOGICAL, &
        PK_INT32_VEC, PK_INT64_VEC, PK_FLOAT32_VEC, PK_FLOAT64_VEC, PK_LOGICAL_VEC
    ! `pf_mode` accepts a packed string column, and reaches it through the TYPED tier
    ! (`parquet_string_column_get` and friends) rather than through a type-bound procedure --
    ! CLAUDE.md's ifx descriptor rule, which costs 24 static stores per call on a shared `.bss`
    ! line and so gets worse rather than better under threads. `parquet_strings` is already in
    ! this module's closure, beneath `parquet_columns`, so nothing new enters the graph.
    use parquet_strings, only : parquet_string_column, parquet_string_column_get, &
        parquet_string_column_is_null, parquet_string_column_size, &
        parquet_string_column_null_count
    ! `%print` writes solicited output, and `verbosity = "silent"` governs solicited output
    ! wherever it lives. `parquet_sorting` re-exports the getters and setters but not this
    ! predicate, so it is named directly; `parquet_settings_base` is already in the closure.
    use parquet_settings_base, only : parquet_output_is_suppressed
    ! `output_unit`/`error_unit` are for `%print`, which resolves the `message_stream` setting's
    ! token to a unit the same way the library's own emitters do.
    use iso_fortran_env, only : int32, int64, real32, real64, output_unit, error_unit
    implicit none
    private

    !> The `int64` bit pattern of a `real64` whose exponent field is all ones.
    !!
    !! **The fast weight screen, and it is an INTEGER test on purpose.** Every `weights=` loop in
    !! this module has to reject a NaN, a negative and an infinity, and `stats_check_weight` is the
    !! procedure that says so with a message naming the element. It lives in another submodule, so
    !! calling it PER ELEMENT is an out-of-line call per element -- which is what it used to be.
    !!
    !! A valid weight is finite and non-negative, i.e. its bit pattern read as a signed `int64`
    !! lies in `[0, STATS_W_LIM)`: a negative has the sign bit set and so reads negative, and a NaN
    !! or `+Inf` reads at or above the limit. So `b < 0 .or. b >= STATS_W_LIM` is ONE branch that
    !! is true for every weight the validator could object to and false for every one it could not,
    !! which puts the call on the failure path where it belongs.
    !!
    !! It has to be an integer test rather than `w >= 0.0 .and. w <= huge(...)`, because `<` and
    !! `>=` are SIGNALLING comparisons: on a NaN they raise `IEEE_INVALID`, which nagfor's default
    !! `-ieee=stop` turns into a dead process -- the same reason `stats_check_weight`'s own three
    !! tests are separate statements in a fixed order. Reading the bits raises nothing at all.
    !!
    !! `-0.0` reads negative and so takes the slow path, where the validator accepts it and the
    !! caller's own `<= 0` test then excludes it as the zero weight it is: the same answer, one
    !! branch later, on a value that does not occur in practice.
    integer(int64), parameter :: STATS_W_LIM = int(z'7FF0000000000000', int64)

    public :: pf_count_valid
    public :: pf_sum, pf_mean, pf_variance, pf_stddev, pf_sem
    public :: pf_gmean, pf_hmean
    public :: pf_skewness, pf_kurtosis, pf_moments
    public :: pf_median, pf_quantile, pf_quantiles
    public :: pf_iqr, pf_trim_mean, pf_percentile_of_score
    public :: pf_mad, pf_mode, pf_describe
    public :: pf_cov, pf_corr, pf_zscore, pf_normal_scores
    public :: pf_sigma_clipped_stats
    public :: pf_cumsum, pf_cumprod, pf_cummax, pf_cummin
    public :: pf_bucketize, pf_histogram, pf_bin_edges
    public :: parquet_set_verbosity, parquet_get_verbosity
    public :: parquet_set_message_stream, parquet_get_message_stream
    public :: pf_stats
    public :: parquet_debug_stats_scans, parquet_debug_reset_stats_scans
    public :: parquet_debug_stats_team, parquet_debug_set_stats_min_per_thread
    public :: parquet_debug_stats_sorts, parquet_debug_reset_stats_sorts
    public :: parquet_debug_set_stats_quantile_sort_min
    !
    ! ---- The tier-A accumulator, and the object that holds one ----
    !
    !> Everything tier A holds about one population: the counts, the weight sums and the central
    !! moments about the mean.
    !!
    !! Private: it is the engine's currency, not part of the public surface.
    type :: stats_acc
        integer(int64) :: n_valid = 0_int64 !! elements that survived every exclusion.
        integer(int64) :: n_null = 0_int64  !! elements `is_valid` excluded.
        integer(int64) :: n_nan = 0_int64   !! excluded as NaN, and not already null.
        real(real64) :: w_sum = 0.0_real64  !! `sum(w)`; the population size when unweighted.
        real(real64) :: w_sq = 0.0_real64   !! `sum(w**2)`, which the reliability count needs.
        real(real64) :: vsum = 0.0_real64   !! `sum(w*x)`; exactly 0 for an empty population.
        real(real64) :: mean = 0.0_real64   !! `vsum / w_sum`.
        real(real64) :: m2 = 0.0_real64     !! `sum(w*(x-mean)**2)`.
        real(real64) :: m3 = 0.0_real64     !! `sum(w*(x-mean)**3)`.
        real(real64) :: m4 = 0.0_real64     !! `sum(w*(x-mean)**4)`.
        real(real64) :: vmin = 0.0_real64   !! the smallest surviving value.
        real(real64) :: vmax = 0.0_real64   !! the largest surviving value.
        logical :: empty = .true.           !! .true. when nothing survived; every moment is NaN.
        logical :: saw_nan = .false.        !! a NaN entered the population under skipnan=.false.
    end type stats_acc

    !> One population, summarised once and queried as often as you like.
    !>
    !> The reason to reach for this type rather than the one-shot `pf_*` procedures is that the
    !> work is done **once**: `%compute` traverses the values twice, and every tier-A query after
    !> that is an O(1) read of what it left behind. Asking for eight statistics costs what asking
    !> for one costs, and `parquet_debug_stats_scans()` is what that promise is asserted with.
    !>
    !> Three lifecycles, and the choice between them is `retain`:
    !>
    !> - **`%compute(values, ...)`** summarises a resident array. `retain = .true.` (the default)
    !>   keeps a copy of the surviving values, which is what the order statistics of a later phase
    !>   will need; `retain = .false.` keeps nothing but the accumulator.
    !> - **`%init(retain=.false.)` then a loop of `%update`** streams a population that never
    !>   exists in memory at once -- a row-group loop over a file larger than RAM. Each `%update`
    !>   is ONE traversal and O(1) memory, using the Chan/Pebay combination formulas.
    !> - **`%init` per thread, then `%merge`** accumulates in parallel, folded afterwards in index
    !>   order so the answer does not depend on which thread finished first.
    !>
    !> **In retained mode `%merge` and `%update` are EXACT**: the moments are recomputed by the
    !> same two-pass algorithm over the concatenated survivors, so `%merge` equals `%compute` over
    !> the concatenation bit for bit, not merely to the accuracy a combination formula claims. The
    !> recomputation is lazy, so folding k partials costs one recomputation rather than k. In
    !> streaming mode there is no buffer to re-walk and the formulas' own accuracy is what you get.
    !>
    !> **Memory.** `retain = .false.` is O(1) whatever the input size. `retain = .true.` costs 8
    !> bytes per surviving element, and 8 more when weighted. `%clear` gives all of it back.
    !>
    !> **Threading.** This is a value-like summary object and takes no lock, so a shared one must
    !> not be mutated concurrently. It also has allocatable components, which puts it in the class
    !> ifx miscompiles when declared in a `block` lexically inside a parallel region: declare an
    !> ARRAY of `pf_stats` before the region, one slot per thread, and index it by
    !> `omp_get_thread_num() + 1` -- the shape `%merge`'s array form exists to make natural.
    !>
    !> A query may complete a deferred recomputation, so every one of them takes the object as
    !> `intent(inout)` and none is `pure`. That is the same trade `parquet_table`'s value
    !> accessors make for lazy materialization.
    type :: pf_stats
        private
        type(stats_acc) :: acc                   !! the tier-A quantities.
        real(real64), allocatable :: keep(:)     !! retained survivors; `keep_n` of them are live.
        real(real64), allocatable :: keep_w(:)   !! their weights, allocated only when weighted.
        integer(int64) :: keep_n = 0_int64       !! live length of `keep`/`keep_w`.
        integer(int64) :: n_seen = 0_int64       !! elements offered, including every excluded one.
        integer(int64) :: c_valid = 0_int64      !! survivors, carried exactly across updates.
        integer(int64) :: c_null = 0_int64       !! nulls, likewise.
        integer(int64) :: c_nan = 0_int64        !! NaNs excluded, likewise.
        logical :: hold = .true.                 !! `retain`: whether survivors are kept.
        logical :: live = .false.                !! %init or %compute has run.
        logical :: freq = .false.                !! frequency rather than reliability weights.
        logical :: wtd = .false.                 !! any weights have been supplied.
        logical :: skip = .true.                 !! `skipnan`, fixed at %init/%compute.
        logical :: stale = .false.  !! retained moments need recomputing.
        logical :: ordered = .false.
        !! tier B is built: `keep` (and `keep_w` with it) is in ascending order. **The retained
        !! buffer is sorted IN PLACE**, so tier B costs no memory of its own -- which is also why
        !! nothing in the public API ever hands the retained values back in their original order,
        !! and why no binding should be added that does.
        real(real64) :: mad_raw = 0.0_real64 !! tier C: the UNSCALED median absolute deviation.
        real(real64) :: mad_c = 0.0_real64   !! the centre `mad_raw` was taken about.
        logical :: mad_ready = .false.
        !! tier C is built for centre `mad_c`. **The cache is a scalar, not the deviation buffer**:
        !! once the median of |x - c| is known the buffer has no further use, so tier C costs
        !! O(1) to hold and O(n) only while it is being built. A `%mad(center=)` whose centre
        !! differs from `mad_c` rebuilds rather than answering the wrong population's deviation --
        !! the equality is on the RESOLVED centre, so repeated default calls hit.
    contains
        procedure :: init => obj_init !! Arms an empty accumulator for a loop of `%update`.
        procedure :: clear => obj_clear !! Returns the object to its default-initialised state.
        procedure, private :: obj_compute_f64 !! `%compute` over a 64-bit real array.
        procedure, private :: obj_compute_i32 !! `%compute` over a 32-bit integer array.
        procedure, private :: obj_compute_i64 !! `%compute` over a 64-bit integer array.
        procedure, private :: obj_compute_f32 !! `%compute` over a 32-bit real array.
        procedure, private :: obj_compute_bool !! `%compute` over a logical array.
        procedure, private :: obj_compute_col !! `%compute` over a scalar numeric `parquet_column`.
        !> Summarises a resident array; the usual entry point. Accepts the same kinds the one-shot
        !> family does -- the four widened numeric arrays and a scalar numeric `parquet_column` --
        !> and every one of them reaches the same real64 engine.
        generic :: compute => obj_compute_f64, obj_compute_i32, obj_compute_i64, &
            obj_compute_f32, obj_compute_bool, obj_compute_col
        procedure, private :: obj_update_f64 !! `%update` over a 64-bit real array.
        procedure, private :: obj_update_i32 !! `%update` over a 32-bit integer array.
        procedure, private :: obj_update_i64 !! `%update` over a 64-bit integer array.
        procedure, private :: obj_update_f32 !! `%update` over a 32-bit real array.
        procedure, private :: obj_update_bool !! `%update` over a logical array.
        procedure, private :: obj_update_col !! `%update` over a scalar numeric `parquet_column`.
        !> Folds one more batch into the population, over any kind `%compute` accepts.
        generic :: update => obj_update_f64, obj_update_i32, obj_update_i64, &
            obj_update_f32, obj_update_bool, obj_update_col
        procedure, private :: obj_merge_one !! `%merge` over one other accumulator.
        procedure, private :: obj_merge_many !! `%merge` over an array of them, in index order.
        generic :: merge => obj_merge_one, obj_merge_many !! Folds other accumulators into this one.
        procedure :: is_computed => obj_is_computed !! Whether %compute or %init has run.
        procedure :: retains => obj_retains !! Whether the surviving values are being kept.
        procedure :: n => obj_n !! Elements offered, including every excluded one.
        procedure :: n_valid => obj_n_valid !! Elements in the population.
        procedure :: n_null => obj_n_null !! Elements `is_valid` excluded.
        procedure :: n_nan => obj_n_nan !! Elements excluded as NaN.
        procedure :: sum_weights => obj_sum_weights !! `sum(w)` over the population.
        procedure :: sum => obj_sum !! `sum(w*x)` over the population.
        procedure :: mean => obj_mean !! The weighted mean.
        procedure :: gmean => obj_gmean !! The weighted geometric mean; needs `retain`.
        procedure :: hmean => obj_hmean !! The weighted harmonic mean; needs `retain`.
        procedure :: variance => obj_variance !! The variance, at `ddof` degrees of freedom.
        procedure :: stddev => obj_stddev !! The standard deviation.
        procedure :: sem => obj_sem !! The standard error of the mean.
        procedure :: skewness => obj_skewness !! The third standardised moment.
        procedure :: kurtosis => obj_kurtosis !! The fourth standardised moment.
        procedure :: vmin => obj_vmin !! The smallest value in the population.
        procedure :: vmax => obj_vmax !! The largest value in the population.
        procedure :: range => obj_range !! `vmax - vmin`.
        procedure :: median => obj_median !! The interpolating median.
        procedure :: quantile => obj_quantile !! One quantile, on a 0-1 scale.
        procedure :: quantiles => obj_quantiles !! Several quantiles from the one ordering.
        procedure :: iqr => obj_iqr !! The interquartile range.
        procedure :: trim_mean => obj_trim_mean !! The mean with a share trimmed from each tail.
        procedure :: percentile_of_score => obj_percentile_of_score !! Where a value sits, 0-1.
        procedure :: mad => obj_mad !! The median absolute deviation, scaled by default.
        procedure :: print => obj_print !! Writes the describe() block to a unit.
        procedure :: prepare_order => obj_prepare_order !! Builds tier B now rather than lazily.
        procedure :: release_order => obj_release_order !! Frees tier B, keeping tier A.
        procedure :: is_ordered => obj_is_ordered !! Whether tier B is currently built.
        procedure :: has_deviation => obj_has_deviation !! Whether tier C is currently built.
    end type pf_stats

    !> How many elements of `values` are in the population -- pandas' `Series.count()`.
    !>
    !> An element is counted unless it is null (`is_valid(i)` is `.false.`), a NaN under the
    !> default `skipnan = .true.` on a kind that can hold one, or carries a zero weight. Those
    !> three exclusions are applied in that order, and they are exactly the ones every other
    !> reduction in this module applies -- so this answers the same `n_valid` a weighted
    !> `pf_mean` over the same arguments would report, which is why it takes `weights` at all.
    !>
    !> `n` is `integer(int64)`. An empty array, or one whose every element is excluded,
    !> answers `0`: this module never aborts on a data condition.
    interface pf_count_valid
        module procedure count_valid_i32
        module procedure count_valid_i64
        module procedure count_valid_f32
        module procedure count_valid_f64
        module procedure count_valid_bool
        module procedure count_valid_col
        module procedure count_valid_i32_i32
        module procedure count_valid_i64_i32
        module procedure count_valid_f32_i32
        module procedure count_valid_f64_i32
        module procedure count_valid_bool_i32
        module procedure count_valid_col_i32
    end interface pf_count_valid
    !
    ! ---- Counting ----
    interface
        !> `pf_count_valid` over a 32-bit integer array.
        !>
        !> `n_null`/`n_nan` report WHY elements left, which is the whole reason to
        !> reach for this procedure rather than `size(values)`: the three numbers
        !> together account for every element except the zero-weighted ones, and
        !> that difference is what a zero weight IS.
        module subroutine count_valid_i32(values, n, is_valid, weights, n_null)
            integer(int32), intent(in) :: values(:) !! the population to count.
            integer(int64), intent(out) :: n !! how many elements are in the population.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight. A zero weight REMOVES the element from the population, so a
            !! weighted count and an unweighted one over the same array legitimately differ.
            !! A negative, NaN or infinite weight aborts.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
        end subroutine count_valid_i32
        !> `pf_count_valid` over a 64-bit integer array.
        !>
        !> `n_null`/`n_nan` report WHY elements left, which is the whole reason to
        !> reach for this procedure rather than `size(values)`: the three numbers
        !> together account for every element except the zero-weighted ones, and
        !> that difference is what a zero weight IS.
        module subroutine count_valid_i64(values, n, is_valid, weights, n_null)
            integer(int64), intent(in) :: values(:) !! the population to count.
            integer(int64), intent(out) :: n !! how many elements are in the population.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight. A zero weight REMOVES the element from the population, so a
            !! weighted count and an unweighted one over the same array legitimately differ.
            !! A negative, NaN or infinite weight aborts.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
        end subroutine count_valid_i64
        !> `pf_count_valid` over a 32-bit real array.
        !>
        !> `n_null`/`n_nan` report WHY elements left, which is the whole reason to
        !> reach for this procedure rather than `size(values)`: the three numbers
        !> together account for every element except the zero-weighted ones, and
        !> that difference is what a zero weight IS.
        module subroutine count_valid_f32(values, n, is_valid, weights, skipnan, n_null, n_nan)
            real(real32), intent(in) :: values(:) !! the population to count.
            integer(int64), intent(out) :: n !! how many elements are in the population.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight. A zero weight REMOVES the element from the population, so a
            !! weighted count and an unweighted one over the same array legitimately differ.
            !! A negative, NaN or infinite weight aborts.
            logical, intent(in), optional :: skipnan
            !! .true. (the default) excludes a NaN from the population, as a null is excluded and
            !! as `pf_minmax` has always done; .false. counts it as an ordinary value.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            integer(int64), intent(out), optional :: n_nan
            !! how many were excluded as NaN and were not already null.
        end subroutine count_valid_f32
        !> `pf_count_valid` over a 64-bit real array.
        !>
        !> `n_null`/`n_nan` report WHY elements left, which is the whole reason to
        !> reach for this procedure rather than `size(values)`: the three numbers
        !> together account for every element except the zero-weighted ones, and
        !> that difference is what a zero weight IS.
        module subroutine count_valid_f64(values, n, is_valid, weights, skipnan, n_null, n_nan)
            real(real64), intent(in) :: values(:) !! the population to count.
            integer(int64), intent(out) :: n !! how many elements are in the population.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight. A zero weight REMOVES the element from the population, so a
            !! weighted count and an unweighted one over the same array legitimately differ.
            !! A negative, NaN or infinite weight aborts.
            logical, intent(in), optional :: skipnan
            !! .true. (the default) excludes a NaN from the population, as a null is excluded and
            !! as `pf_minmax` has always done; .false. counts it as an ordinary value.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            integer(int64), intent(out), optional :: n_nan
            !! how many were excluded as NaN and were not already null.
        end subroutine count_valid_f64
        !> `pf_count_valid` over a logical array.
        !>
        !> `n_null`/`n_nan` report WHY elements left, which is the whole reason to
        !> reach for this procedure rather than `size(values)`: the three numbers
        !> together account for every element except the zero-weighted ones, and
        !> that difference is what a zero weight IS.
        module subroutine count_valid_bool(values, n, is_valid, weights, n_null)
            logical, intent(in) :: values(:) !! the population to count.
            integer(int64), intent(out) :: n !! how many elements are in the population.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight. A zero weight REMOVES the element from the population, so a
            !! weighted count and an unweighted one over the same array legitimately differ.
            !! A negative, NaN or infinite weight aborts.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
        end subroutine count_valid_bool
        !> `pf_count_valid` over a scalar numeric `parquet_column`.
        !>
        !> Same dispatch and same three refusals as the moment family: numeric scalar kinds only,
        !> width 1 only, and `is_valid=` alongside a column aborts because the column carries its
        !> own validity.
        module subroutine count_valid_col(values, n, is_valid, weights, skipnan, n_null, n_nan)
            type(parquet_column), intent(in) :: values !! the column to count.
            integer(int64), intent(out) :: n !! how many elements are in the population.
            logical, intent(in), optional :: is_valid(:) !! must be absent; the column carries it.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight. A zero weight REMOVES the element from the population, so a
            !! weighted count and an unweighted one over the same column legitimately differ.
            !! A negative, NaN or infinite weight aborts.
            logical, intent(in), optional :: skipnan
            !! .true. (the default) excludes a NaN from the population, as a null is excluded and
            !! as `pf_minmax` has always done; .false. counts it as an ordinary value.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            integer(int64), intent(out), optional :: n_nan
            !! how many were excluded as NaN and were not already null.
        end subroutine count_valid_col
        !> `pf_count_valid` over a 32-bit integer array, reporting into `integer(int32)`.
        !>
        !> Identical to the `integer(int64)` form in every respect but the kind of the
        !> counts. `n` is REQUIRED, so its kind is what selects between the two, and
        !> `n_null`/`n_nan` must then match it -- a mixed call matches neither specific
        !> and does not compile. A count above `huge(int32)` aborts rather than wrapping.
        module subroutine count_valid_i32_i32(values, n, is_valid, weights, n_null)
            integer(int32), intent(in) :: values(:) !! the population to count.
            integer(int32), intent(out) :: n !! how many elements are in the population.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight. A zero weight REMOVES the element from the population, so a
            !! weighted count and an unweighted one over the same array legitimately differ.
            !! A negative, NaN or infinite weight aborts.
            integer(int32), intent(out), optional :: n_null !! how many elements were null.
        end subroutine count_valid_i32_i32
        !> `pf_count_valid` over a 64-bit integer array, reporting into `integer(int32)`.
        !>
        !> Identical to the `integer(int64)` form in every respect but the kind of the
        !> counts. `n` is REQUIRED, so its kind is what selects between the two, and
        !> `n_null`/`n_nan` must then match it -- a mixed call matches neither specific
        !> and does not compile. A count above `huge(int32)` aborts rather than wrapping.
        module subroutine count_valid_i64_i32(values, n, is_valid, weights, n_null)
            integer(int64), intent(in) :: values(:) !! the population to count.
            integer(int32), intent(out) :: n !! how many elements are in the population.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight. A zero weight REMOVES the element from the population, so a
            !! weighted count and an unweighted one over the same array legitimately differ.
            !! A negative, NaN or infinite weight aborts.
            integer(int32), intent(out), optional :: n_null !! how many elements were null.
        end subroutine count_valid_i64_i32
        !> `pf_count_valid` over a 32-bit real array, reporting into `integer(int32)`.
        !>
        !> Identical to the `integer(int64)` form in every respect but the kind of the
        !> counts. `n` is REQUIRED, so its kind is what selects between the two, and
        !> `n_null`/`n_nan` must then match it -- a mixed call matches neither specific
        !> and does not compile. A count above `huge(int32)` aborts rather than wrapping.
        module subroutine count_valid_f32_i32(values, n, is_valid, weights, skipnan, n_null, n_nan)
            real(real32), intent(in) :: values(:) !! the population to count.
            integer(int32), intent(out) :: n !! how many elements are in the population.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight. A zero weight REMOVES the element from the population, so a
            !! weighted count and an unweighted one over the same array legitimately differ.
            !! A negative, NaN or infinite weight aborts.
            logical, intent(in), optional :: skipnan
            !! .true. (the default) excludes a NaN from the population, as a null is excluded and
            !! as `pf_minmax` does; .false. counts it as an ordinary value.
            integer(int32), intent(out), optional :: n_null !! how many elements were null.
            integer(int32), intent(out), optional :: n_nan !! how many were NaN and skipped.
        end subroutine count_valid_f32_i32
        !> `pf_count_valid` over a 64-bit real array, reporting into `integer(int32)`.
        !>
        !> Identical to the `integer(int64)` form in every respect but the kind of the
        !> counts. `n` is REQUIRED, so its kind is what selects between the two, and
        !> `n_null`/`n_nan` must then match it -- a mixed call matches neither specific
        !> and does not compile. A count above `huge(int32)` aborts rather than wrapping.
        module subroutine count_valid_f64_i32(values, n, is_valid, weights, skipnan, n_null, n_nan)
            real(real64), intent(in) :: values(:) !! the population to count.
            integer(int32), intent(out) :: n !! how many elements are in the population.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight. A zero weight REMOVES the element from the population, so a
            !! weighted count and an unweighted one over the same array legitimately differ.
            !! A negative, NaN or infinite weight aborts.
            logical, intent(in), optional :: skipnan
            !! .true. (the default) excludes a NaN from the population, as a null is excluded and
            !! as `pf_minmax` does; .false. counts it as an ordinary value.
            integer(int32), intent(out), optional :: n_null !! how many elements were null.
            integer(int32), intent(out), optional :: n_nan !! how many were NaN and skipped.
        end subroutine count_valid_f64_i32
        !> `pf_count_valid` over a logical array, reporting into `integer(int32)`.
        !>
        !> Identical to the `integer(int64)` form in every respect but the kind of the
        !> counts. `n` is REQUIRED, so its kind is what selects between the two, and
        !> `n_null`/`n_nan` must then match it -- a mixed call matches neither specific
        !> and does not compile. A count above `huge(int32)` aborts rather than wrapping.
        module subroutine count_valid_bool_i32(values, n, is_valid, weights, n_null)
            logical, intent(in) :: values(:) !! the population to count.
            integer(int32), intent(out) :: n !! how many elements are in the population.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight. A zero weight REMOVES the element from the population, so a
            !! weighted count and an unweighted one over the same array legitimately differ.
            !! A negative, NaN or infinite weight aborts.
            integer(int32), intent(out), optional :: n_null !! how many elements were null.
        end subroutine count_valid_bool_i32
        !> `pf_count_valid` over a scalar numeric `parquet_column`, reporting into `integer(int32)`.
        !!
        !! The int32 counterpart of `count_valid_col`; see `count_valid_f64_i32` for why `n` is
        !! required and what a mixed-kind call does.
        module subroutine count_valid_col_i32(values, n, is_valid, weights, skipnan, n_null, n_nan)
            type(parquet_column), intent(in) :: values !! the column to count.
            integer(int32), intent(out) :: n !! how many elements are in the population.
            logical, intent(in), optional :: is_valid(:)
            !! REFUSED beside a column, which carries its own validity.
            real(real64), intent(in), optional :: weights(:) !! per element weight; zero removes it.
            logical, intent(in), optional :: skipnan !! .true. (the default) excludes a NaN.
            integer(int32), intent(out), optional :: n_null !! how many elements were null.
            integer(int32), intent(out), optional :: n_nan !! how many were NaN and skipped.
        end subroutine count_valid_col_i32
        !> Narrows a count to `int32`, aborting rather than wrapping.
        !!
        !! Reached only from the int32 `pf_count_valid` specifics. A population larger than
        !! `huge(int32)` is entirely reachable here -- this library reads billion-row files -- so a
        !! silent wrap would hand back a plausible wrong count, which is the one outcome worse than
        !! forcing the caller to declare an `int64`.
        module subroutine stats_narrow_count(value, noun, dst)
            integer(int64), intent(in) :: value  !! the count.
            character(len=*), intent(in) :: noun !! which count it is, for the message.
            integer(int32), intent(out) :: dst   !! the narrowed copy.
        end subroutine stats_narrow_count
    end interface
    !
    !> The sum of a population, computed PAIRWISE rather than left to right.
    !>
    !> Pairwise summation loses O(log n) relative accuracy against a naive sum's O(n),
    !> for the same number of additions and no extra memory -- and it is the same fixed
    !> block tree that makes every answer here independent of the thread count.
    !>
    !> Nulls, NaNs and zero-weight elements are excluded from the population, in that order, and
    !> `n_null`/`n_nan` report how many. An undefined answer is a **quiet NaN** with
    !> `ok = .false.`: this module never aborts on a data condition, because a per-group loop
    !> meets an empty group on real data. It aborts only on misuse -- a mismatched array size, an
    !> unrecognised token, or a negative, NaN or infinite weight.
    !>
    !> `values` may be a `real(real64)`, `real(real32)`, `integer(int32)`, `integer(int64)` or
    !> `logical` array, or a scalar numeric `type(parquet_column)`; every one of them widens to
    !> `real64` and reaches the same engine, so the answer does not depend on which was passed.
    !>
    !> The central-moment pass is threaded on a large population, and `threads=` overrides the
    !> automatic count. **That cannot change an answer**: the population is reduced over a fixed
    !> block tree determined by its size alone, so one thread, eight threads and a build without
    !> OpenMP return the identical bits.
    interface pf_sum
        module procedure sum_f64
        module procedure sum_i32
        module procedure sum_i64
        module procedure sum_f32
        module procedure sum_bool
        module procedure sum_col
    end interface pf_sum
    !
    !> The arithmetic mean of a population, weighted when `weights` is present.
    !>
    !> **Not literally `sum(w*x)/sum(w)`, and deliberately so.** That quotient is taken
    !> first and then REFINED by `sum(w*(x - mu))/sum(w)`, which is algebraically zero
    !> and in floating point is the rounding error left in it -- the standard two-pass
    !> refinement, and the more accurate answer. It is also the centre the variance and
    !> the higher moments are taken about, so it has to be this one. The visible
    !> consequence is that the result can differ from the naive quotient in the last
    !> bit: `pf_mean` over ten logicals of which three are `.true.` is one ulp below
    !> `0.3_real64`. Compare with a tolerance, or against another reduction of the same
    !> population, rather than against a decimal literal.
    !>
    !> Nulls, NaNs and zero-weight elements are excluded from the population, in that order, and
    !> `n_null`/`n_nan` report how many. An undefined answer is a **quiet NaN** with
    !> `ok = .false.`: this module never aborts on a data condition, because a per-group loop
    !> meets an empty group on real data. It aborts only on misuse -- a mismatched array size, an
    !> unrecognised token, or a negative, NaN or infinite weight.
    !>
    !> `values` may be a `real(real64)`, `real(real32)`, `integer(int32)`, `integer(int64)` or
    !> `logical` array, or a scalar numeric `type(parquet_column)`; every one of them widens to
    !> `real64` and reaches the same engine, so the answer does not depend on which was passed.
    !>
    !> The central-moment pass is threaded on a large population, and `threads=` overrides the
    !> automatic count. **That cannot change an answer**: the population is reduced over a fixed
    !> block tree determined by its size alone, so one thread, eight threads and a build without
    !> OpenMP return the identical bits.
    interface pf_mean
        module procedure mean_f64
        module procedure mean_i32
        module procedure mean_i64
        module procedure mean_f32
        module procedure mean_bool
        module procedure mean_col
    end interface pf_mean
    !
    !> The geometric mean -- `scipy.stats.gmean`, computed in log space.
    !>
    !> `exp(sum(w*log(x))/sum(w))`. In log space rather than as a product, because the
    !> product of a few thousand fluxes overflows a `real64` and its logarithm does
    !> not.
    !>
    !> **Two values outside the domain are answered rather than refused.** A value
    !> exactly `0` gives exactly `0`, which is the limit and is what scipy returns;
    !> any negative value gives a quiet NaN with `ok = .false.`, since the geometric
    !> mean of a sign-changing population is not defined. Neither aborts: both are
    !> data conditions.
    !>
    !> Nulls, NaNs and zero-weight elements are excluded from the population, in that order, and
    !> `n_null`/`n_nan` report how many. An undefined answer is a **quiet NaN** with
    !> `ok = .false.`: this module never aborts on a data condition, because a per-group loop
    !> meets an empty group on real data. It aborts only on misuse -- a mismatched array size, an
    !> unrecognised token, or a negative, NaN or infinite weight.
    !>
    !> `values` may be a `real(real64)`, `real(real32)`, `integer(int32)`, `integer(int64)` or
    !> `logical` array, or a scalar numeric `type(parquet_column)`; every one of them widens to
    !> `real64` and reaches the same engine, so the answer does not depend on which was passed.
    !>
    !> The central-moment pass is threaded on a large population, and `threads=` overrides the
    !> automatic count. **That cannot change an answer**: the population is reduced over a fixed
    !> block tree determined by its size alone, so one thread, eight threads and a build without
    !> OpenMP return the identical bits.
    interface pf_gmean
        module procedure gmean_f64
        module procedure gmean_i32
        module procedure gmean_i64
        module procedure gmean_f32
        module procedure gmean_bool
        module procedure gmean_col
    end interface pf_gmean
    !
    !> The harmonic mean -- `scipy.stats.hmean`.
    !>
    !> `sum(w)/sum(w/x)`, with the same domain rules as `pf_gmean`: a zero gives
    !> exactly `0`, a negative gives a quiet NaN.
    !>
    !> Nulls, NaNs and zero-weight elements are excluded from the population, in that order, and
    !> `n_null`/`n_nan` report how many. An undefined answer is a **quiet NaN** with
    !> `ok = .false.`: this module never aborts on a data condition, because a per-group loop
    !> meets an empty group on real data. It aborts only on misuse -- a mismatched array size, an
    !> unrecognised token, or a negative, NaN or infinite weight.
    !>
    !> `values` may be a `real(real64)`, `real(real32)`, `integer(int32)`, `integer(int64)` or
    !> `logical` array, or a scalar numeric `type(parquet_column)`; every one of them widens to
    !> `real64` and reaches the same engine, so the answer does not depend on which was passed.
    !>
    !> The central-moment pass is threaded on a large population, and `threads=` overrides the
    !> automatic count. **That cannot change an answer**: the population is reduced over a fixed
    !> block tree determined by its size alone, so one thread, eight threads and a build without
    !> OpenMP return the identical bits.
    interface pf_hmean
        module procedure hmean_f64
        module procedure hmean_i32
        module procedure hmean_i64
        module procedure hmean_f32
        module procedure hmean_bool
        module procedure hmean_col
    end interface pf_hmean
    !
    !> The variance of a population, in two passes so that it is shift-invariant.
    !>
    !> `ddof = 1` by default (the sample variance, as pandas returns); pass
    !> `ddof = 0` for numpy's default.
    !>
    !> Nulls, NaNs and zero-weight elements are excluded from the population, in that order, and
    !> `n_null`/`n_nan` report how many. An undefined answer is a **quiet NaN** with
    !> `ok = .false.`: this module never aborts on a data condition, because a per-group loop
    !> meets an empty group on real data. It aborts only on misuse -- a mismatched array size, an
    !> unrecognised token, or a negative, NaN or infinite weight.
    !>
    !> `values` may be a `real(real64)`, `real(real32)`, `integer(int32)`, `integer(int64)` or
    !> `logical` array, or a scalar numeric `type(parquet_column)`; every one of them widens to
    !> `real64` and reaches the same engine, so the answer does not depend on which was passed.
    !>
    !> The central-moment pass is threaded on a large population, and `threads=` overrides the
    !> automatic count. **That cannot change an answer**: the population is reduced over a fixed
    !> block tree determined by its size alone, so one thread, eight threads and a build without
    !> OpenMP return the identical bits.
    interface pf_variance
        module procedure variance_f64
        module procedure variance_i32
        module procedure variance_i64
        module procedure variance_f32
        module procedure variance_bool
        module procedure variance_col
    end interface pf_variance
    !
    !> The standard deviation: the square root of `pf_variance`, same arguments.
    !>
    !> Nulls, NaNs and zero-weight elements are excluded from the population, in that order, and
    !> `n_null`/`n_nan` report how many. An undefined answer is a **quiet NaN** with
    !> `ok = .false.`: this module never aborts on a data condition, because a per-group loop
    !> meets an empty group on real data. It aborts only on misuse -- a mismatched array size, an
    !> unrecognised token, or a negative, NaN or infinite weight.
    !>
    !> `values` may be a `real(real64)`, `real(real32)`, `integer(int32)`, `integer(int64)` or
    !> `logical` array, or a scalar numeric `type(parquet_column)`; every one of them widens to
    !> `real64` and reaches the same engine, so the answer does not depend on which was passed.
    !>
    !> The central-moment pass is threaded on a large population, and `threads=` overrides the
    !> automatic count. **That cannot change an answer**: the population is reduced over a fixed
    !> block tree determined by its size alone, so one thread, eight threads and a build without
    !> OpenMP return the identical bits.
    interface pf_stddev
        module procedure stddev_f64
        module procedure stddev_i32
        module procedure stddev_i64
        module procedure stddev_f32
        module procedure stddev_bool
        module procedure stddev_col
    end interface pf_stddev
    !
    !> The standard error of the mean.
    !>
    !> Nulls, NaNs and zero-weight elements are excluded from the population, in that order, and
    !> `n_null`/`n_nan` report how many. An undefined answer is a **quiet NaN** with
    !> `ok = .false.`: this module never aborts on a data condition, because a per-group loop
    !> meets an empty group on real data. It aborts only on misuse -- a mismatched array size, an
    !> unrecognised token, or a negative, NaN or infinite weight.
    !>
    !> `values` may be a `real(real64)`, `real(real32)`, `integer(int32)`, `integer(int64)` or
    !> `logical` array, or a scalar numeric `type(parquet_column)`; every one of them widens to
    !> `real64` and reaches the same engine, so the answer does not depend on which was passed.
    !>
    !> The central-moment pass is threaded on a large population, and `threads=` overrides the
    !> automatic count. **That cannot change an answer**: the population is reduced over a fixed
    !> block tree determined by its size alone, so one thread, eight threads and a build without
    !> OpenMP return the identical bits.
    interface pf_sem
        module procedure sem_f64
        module procedure sem_i32
        module procedure sem_i64
        module procedure sem_f32
        module procedure sem_bool
        module procedure sem_col
    end interface pf_sem
    !
    !> The skewness, bias-corrected by default (pandas' G1, not scipy's g1).
    !>
    !> Nulls, NaNs and zero-weight elements are excluded from the population, in that order, and
    !> `n_null`/`n_nan` report how many. An undefined answer is a **quiet NaN** with
    !> `ok = .false.`: this module never aborts on a data condition, because a per-group loop
    !> meets an empty group on real data. It aborts only on misuse -- a mismatched array size, an
    !> unrecognised token, or a negative, NaN or infinite weight.
    !>
    !> `values` may be a `real(real64)`, `real(real32)`, `integer(int32)`, `integer(int64)` or
    !> `logical` array, or a scalar numeric `type(parquet_column)`; every one of them widens to
    !> `real64` and reaches the same engine, so the answer does not depend on which was passed.
    !>
    !> The central-moment pass is threaded on a large population, and `threads=` overrides the
    !> automatic count. **That cannot change an answer**: the population is reduced over a fixed
    !> block tree determined by its size alone, so one thread, eight threads and a build without
    !> OpenMP return the identical bits.
    interface pf_skewness
        module procedure skewness_f64
        module procedure skewness_i32
        module procedure skewness_i64
        module procedure skewness_f32
        module procedure skewness_bool
        module procedure skewness_col
    end interface pf_skewness
    !
    !> The kurtosis, excess and bias-corrected by default (pandas' G2).
    !>
    !> Nulls, NaNs and zero-weight elements are excluded from the population, in that order, and
    !> `n_null`/`n_nan` report how many. An undefined answer is a **quiet NaN** with
    !> `ok = .false.`: this module never aborts on a data condition, because a per-group loop
    !> meets an empty group on real data. It aborts only on misuse -- a mismatched array size, an
    !> unrecognised token, or a negative, NaN or infinite weight.
    !>
    !> `values` may be a `real(real64)`, `real(real32)`, `integer(int32)`, `integer(int64)` or
    !> `logical` array, or a scalar numeric `type(parquet_column)`; every one of them widens to
    !> `real64` and reaches the same engine, so the answer does not depend on which was passed.
    !>
    !> The central-moment pass is threaded on a large population, and `threads=` overrides the
    !> automatic count. **That cannot change an answer**: the population is reduced over a fixed
    !> block tree determined by its size alone, so one thread, eight threads and a build without
    !> OpenMP return the identical bits.
    interface pf_kurtosis
        module procedure kurtosis_f64
        module procedure kurtosis_i32
        module procedure kurtosis_i64
        module procedure kurtosis_f32
        module procedure kurtosis_bool
        module procedure kurtosis_col
    end interface pf_kurtosis
    !
    !> Every tier-A statistic of a population in one pair of passes.
    !>
    !> The form a per-group loop should use: asking for eight statistics costs the
    !> same two traversals as asking for one.
    !>
    !> Nulls, NaNs and zero-weight elements are excluded from the population, in that order, and
    !> `n_null`/`n_nan` report how many. An undefined answer is a **quiet NaN** with
    !> `ok = .false.`: this module never aborts on a data condition, because a per-group loop
    !> meets an empty group on real data. It aborts only on misuse -- a mismatched array size, an
    !> unrecognised token, or a negative, NaN or infinite weight.
    !>
    !> `values` may be a `real(real64)`, `real(real32)`, `integer(int32)`, `integer(int64)` or
    !> `logical` array, or a scalar numeric `type(parquet_column)`; every one of them widens to
    !> `real64` and reaches the same engine, so the answer does not depend on which was passed.
    !>
    !> The central-moment pass is threaded on a large population, and `threads=` overrides the
    !> automatic count. **That cannot change an answer**: the population is reduced over a fixed
    !> block tree determined by its size alone, so one thread, eight threads and a build without
    !> OpenMP return the identical bits.
    interface pf_moments
        module procedure moments_f64
        module procedure moments_i32
        module procedure moments_i64
        module procedure moments_f32
        module procedure moments_bool
        module procedure moments_col
    end interface pf_moments
    !
    !> The median of a population: the INTERPOLATING one, not an element of the input.
    !>
    !> For an even-length population this is the mean of the two middle values, which is what
    !> numpy, pandas and every spreadsheet mean by `median` -- and is deliberately NOT what
    !> `pf_nth_quantile(v, 0.5)` in `parquet_sorting` returns, which is one of the two. That
    !> one answers `which element`, this one answers `what value`; both are useful and they
    !> are different questions.
    !>
    !> `method=` selects the rule (see `pf_quantile`); `pf_median` is exactly
    !> `pf_quantile(values, 0.5, med)`, and `pf_quantile`'s doc-comment sets out the three
    !> ways `pf_nth_quantile` differs from both.
    !>
    !> Nulls, NaNs and zero-weight elements leave the population first, in that order, exactly as
    !> they do for the moments, and `n_null`/`n_nan` report how many. An empty population gives a
    !> quiet NaN with `ok = .false.` rather than an abort. What DOES abort is misuse: a probability
    !> outside `[0, 1]`, an unrecognised `method`, a mismatched array size, or a weight that is
    !> negative, NaN or infinite.
    !>
    !> `values` may be a `real(real64)`, `real(real32)`, `integer(int32)`, `integer(int64)` or
    !> `logical` array, or a scalar numeric `type(parquet_column)`.
    !>
    !> **Reaching for several order statistics of one population? Use `pf_stats`.** It orders the
    !> values once and answers every later query off that ordering, which `parquet_debug_stats_sorts()`
    !> is what asserts.
    interface pf_median
        module procedure median_f64
        module procedure median_i32
        module procedure median_i64
        module procedure median_f32
        module procedure median_bool
        module procedure median_col
    end interface pf_median
    !
    !> One quantile of a population, interpolated between the bracketing order statistics.
    !>
    !> `p` is on a **0-1 scale**, not 0-100, and outside `[0, 1]` aborts. `method=` chooses how
    !> a fractional position is resolved: `linear` (the default, Hyndman-Fan type 7 -- numpy's
    !> and pandas' default), `lower`, `higher`, `nearest`, `midpoint`, or `inverted_cdf`.
    !>
    !> **Weighted, the position rule reduces EXACTLY to type 7 when the weights are equal**, so
    !> adding uniform weights to a working script does not move its numbers. No reference
    !> library implements an interpolating weighted quantile, so that invariant -- rather than
    !> a cross-check -- is what pins it; `method="inverted_cdf"` is the numpy-comparable
    !> escape hatch, and is what `weight_type="frequency"` selects by default.
    !>
    !> One call with one probability uses SELECTION rather than a sort where it can, so it is
    !> O(n). Asking for several probabilities should use `pf_quantiles`, which sorts once.
    !>
    !> **`pf_nth_quantile` in `parquet_sorting` is the other one**, and the two differ in three
    !> ways rather than one: it returns an ELEMENT of the input (with `index=` saying which)
    !> instead of an interpolated value, it accepts every element type this library sorts
    !> including `character` and the temporal kinds, and it takes no weights. Its `ok` also
    !> means the opposite of this module's -- omitting `ok` there makes a degenerate
    !> population ABORT, where omitting it here is the ordinary way to call. Reach for it when
    !> the question is *which row*, and for this one when the question is *what value*.
    !>
    !> Nulls, NaNs and zero-weight elements leave the population first, in that order, exactly as
    !> they do for the moments, and `n_null`/`n_nan` report how many. An empty population gives a
    !> quiet NaN with `ok = .false.` rather than an abort. What DOES abort is misuse: a probability
    !> outside `[0, 1]`, an unrecognised `method`, a mismatched array size, or a weight that is
    !> negative, NaN or infinite.
    !>
    !> `values` may be a `real(real64)`, `real(real32)`, `integer(int32)`, `integer(int64)` or
    !> `logical` array, or a scalar numeric `type(parquet_column)`.
    !>
    !> **Reaching for several order statistics of one population? Use `pf_stats`.** It orders the
    !> values once and answers every later query off that ordering, which `parquet_debug_stats_sorts()`
    !> is what asserts.
    interface pf_quantile
        module procedure quantile_f64
        module procedure quantile_i32
        module procedure quantile_i64
        module procedure quantile_f32
        module procedure quantile_bool
        module procedure quantile_col
    end interface pf_quantile
    !
    !> Several quantiles of one population, from ONE ordering of it.
    !>
    !> `probs(:)` on a 0-1 scale and `out(:)` the same size; `probs` need not be sorted and may
    !> repeat. This is the form to reach for whenever more than one quantile is wanted --
    !> `[0.16, 0.5, 0.84]` is one call and one sort, where three `pf_quantile` calls would
    !> traverse the population three times.
    !>
    !> Every argument means what it does on `pf_quantile`, including `method=` and the weighted
    !> position rule.
    !>
    !> Nulls, NaNs and zero-weight elements leave the population first, in that order, exactly as
    !> they do for the moments, and `n_null`/`n_nan` report how many. An empty population gives a
    !> quiet NaN with `ok = .false.` rather than an abort. What DOES abort is misuse: a probability
    !> outside `[0, 1]`, an unrecognised `method`, a mismatched array size, or a weight that is
    !> negative, NaN or infinite.
    !>
    !> `values` may be a `real(real64)`, `real(real32)`, `integer(int32)`, `integer(int64)` or
    !> `logical` array, or a scalar numeric `type(parquet_column)`.
    !>
    !> **Reaching for several order statistics of one population? Use `pf_stats`.** It orders the
    !> values once and answers every later query off that ordering, which `parquet_debug_stats_sorts()`
    !> is what asserts.
    interface pf_quantiles
        module procedure quantiles_f64
        module procedure quantiles_i32
        module procedure quantiles_i64
        module procedure quantiles_f32
        module procedure quantiles_bool
        module procedure quantiles_col
    end interface pf_quantiles
    !
    !> The interquartile range: `pf_quantile(v, 0.75) - pf_quantile(v, 0.25)`, from one sort.
    !>
    !> NaN when either quartile is undefined, which for this family means an empty population.
    !> `method=` is passed through to both quartiles, so the difference is always taken between
    !> two quantiles computed the same way.
    !>
    !> Nulls, NaNs and zero-weight elements leave the population first, in that order, exactly as
    !> they do for the moments, and `n_null`/`n_nan` report how many. An empty population gives a
    !> quiet NaN with `ok = .false.` rather than an abort. What DOES abort is misuse: a probability
    !> outside `[0, 1]`, an unrecognised `method`, a mismatched array size, or a weight that is
    !> negative, NaN or infinite.
    !>
    !> `values` may be a `real(real64)`, `real(real32)`, `integer(int32)`, `integer(int64)` or
    !> `logical` array, or a scalar numeric `type(parquet_column)`.
    !>
    !> **Reaching for several order statistics of one population? Use `pf_stats`.** It orders the
    !> values once and answers every later query off that ordering, which `parquet_debug_stats_sorts()`
    !> is what asserts.
    interface pf_iqr
        module procedure iqr_f64
        module procedure iqr_i32
        module procedure iqr_i64
        module procedure iqr_f32
        module procedure iqr_bool
        module procedure iqr_col
    end interface pf_iqr
    !
    !> The mean of the population with a share trimmed from EACH tail -- scipy's `trim_mean`.
    !>
    !> `prop` is the share removed at each end, so `prop=0.1` averages the middle 80%. It must
    !> satisfy `0 <= prop < 0.5`; anything else aborts, since trimming everything is a mistake
    !> rather than a request. `prop=0` is the ordinary mean and is allowed.
    !>
    !> The count trimmed from each end is `floor(prop * n_valid)`, matching scipy, so the
    !> result changes in steps as `prop` grows rather than continuously.
    !>
    !> **Weights are supported and are trimmed with their values**: the same elements leave the
    !> population, and the mean of what remains is weighted.
    !>
    !> Nulls, NaNs and zero-weight elements leave the population first, in that order, exactly as
    !> they do for the moments, and `n_null`/`n_nan` report how many. An empty population gives a
    !> quiet NaN with `ok = .false.` rather than an abort. What DOES abort is misuse: a probability
    !> outside `[0, 1]`, an unrecognised `method`, a mismatched array size, or a weight that is
    !> negative, NaN or infinite.
    !>
    !> `values` may be a `real(real64)`, `real(real32)`, `integer(int32)`, `integer(int64)` or
    !> `logical` array, or a scalar numeric `type(parquet_column)`.
    !>
    !> **Reaching for several order statistics of one population? Use `pf_stats`.** It orders the
    !> values once and answers every later query off that ordering, which `parquet_debug_stats_sorts()`
    !> is what asserts.
    interface pf_trim_mean
        module procedure trim_mean_f64
        module procedure trim_mean_i32
        module procedure trim_mean_i64
        module procedure trim_mean_f32
        module procedure trim_mean_bool
        module procedure trim_mean_col
    end interface pf_trim_mean
    !
    !> The share of the population at or below a given value -- scipy's `percentileofscore`,
    !> on a **0-1 scale** rather than 0-100, matching every other probability in this module.
    !>
    !> The inverse of `pf_quantile` in the loose sense: it answers `where does this value sit`
    !> rather than `what value sits here`. `kind=` chooses how a value exactly EQUAL to `score`
    !> is counted -- `rank` (the default; ties count half), `weak` (`<=`), `strict` (`<`), or
    !> `mean` (the average of weak and strict).
    !>
    !> A NaN or infinite `score` aborts: unlike a NaN in the population, which is an ordinary
    !> data condition this module excludes, a NaN score can only be the caller's own mistake.
    !>
    !> Nulls, NaNs and zero-weight elements leave the population first, in that order, exactly as
    !> they do for the moments, and `n_null`/`n_nan` report how many. An empty population gives a
    !> quiet NaN with `ok = .false.` rather than an abort. What DOES abort is misuse: a probability
    !> outside `[0, 1]`, an unrecognised `method`, a mismatched array size, or a weight that is
    !> negative, NaN or infinite.
    !>
    !> `values` may be a `real(real64)`, `real(real32)`, `integer(int32)`, `integer(int64)` or
    !> `logical` array, or a scalar numeric `type(parquet_column)`.
    !>
    !> **Reaching for several order statistics of one population? Use `pf_stats`.** It orders the
    !> values once and answers every later query off that ordering, which `parquet_debug_stats_sorts()`
    !> is what asserts.
    interface pf_percentile_of_score
        module procedure percentile_of_score_f64
        module procedure percentile_of_score_i32
        module procedure percentile_of_score_i64
        module procedure percentile_of_score_f32
        module procedure percentile_of_score_bool
        module procedure percentile_of_score_col
    end interface pf_percentile_of_score
    !
    !> The median absolute deviation -- scipy's `median_abs_deviation`, scaled by default.
    !>
    !> `median(|x - center|)`, where `center` is the population's own median unless one is
    !> supplied. `scale="normal"` (the default) multiplies by `1/Phi^-1(3/4)` =
    !> 1.4826022185056018, which makes the result
    !> a consistent estimator of the standard deviation for Gaussian data -- so on a large
    !> Gaussian sample `pf_mad` and `pf_stddev` agree to within sampling error, and on a sample
    !> with a few wild points they do not, which is the whole reason to reach for it.
    !> `scale="raw"` is the unscaled median.
    !>
    !> **Supplying `center` skips one selection, not the ordering.** The median of the
    !> deviations still has to be found, so this is a small saving rather than a different
    !> algorithm. A NaN or infinite `center` aborts.
    !>
    !> Both medians are taken with `linear` interpolation, because scipy reaches `np.median`;
    !> there is deliberately no `method=` here, since a token that made this disagree with
    !> scipy would be worse than no token at all.
    !>
    !> Nulls, NaNs and zero-weight elements leave the population first, in that order, exactly as
    !> they do for the moments, and `n_null`/`n_nan` report how many. An empty population gives a
    !> quiet NaN with `ok = .false.` rather than an abort. What DOES abort is misuse: a probability
    !> outside `[0, 1]`, an unrecognised `method`, a mismatched array size, or a weight that is
    !> negative, NaN or infinite.
    !>
    !> `values` may be a `real(real64)`, `real(real32)`, `integer(int32)`, `integer(int64)` or
    !> `logical` array, or a scalar numeric `type(parquet_column)`.
    !>
    !> **Reaching for several order statistics of one population? Use `pf_stats`.** It orders the
    !> values once and answers every later query off that ordering, which `parquet_debug_stats_sorts()`
    !> is what asserts.
    interface pf_mad
        module procedure mad_f64
        module procedure mad_i32
        module procedure mad_i64
        module procedure mad_f32
        module procedure mad_bool
        module procedure mad_col
    end interface pf_mad
    !
    !> Summarises a population into a `pf_stats`, ordered and ready -- pandas' `describe()`.
    !>
    !> Exactly `s%compute(values, ...)` followed by `s%prepare_order()`, which is the pairing
    !> worth a name: it is ONE pair of traversals and ONE ordering, after which the count, the
    !> mean, the standard deviation, the extremes and every quantile are reads. `s%print()`
    !> renders the usual eight-row block from it.
    !>
    !> Reach for `%compute` instead when the order statistics are not wanted -- this procedure
    !> pays for the ordering whether or not anything asks for it, which is the point.
    !>
    !> Nulls, NaNs and zero-weight elements leave the population exactly as they do for the
    !> moments, and the object reports how many through `%n_null()` and `%n_nan()`. An empty
    !> population is not an error: every statistic on the result is then a quiet NaN and
    !> `%n_valid()` is 0.
    !>
    !> `values` may be a `real(real64)`, `real(real32)`, `integer(int32)`, `integer(int64)` or
    !> `logical` array, or a scalar numeric `type(parquet_column)`.
    interface pf_describe
        module procedure describe_f64
        module procedure describe_i32
        module procedure describe_i64
        module procedure describe_f32
        module procedure describe_bool
        module procedure describe_col
    end interface pf_describe
    !
    !> The covariance of two samples -- pairwise-complete, as pandas is.
    !>
    !> A pair enters the population only when BOTH elements are usable: neither null, neither
    !> NaN, and the pair's weight non-zero. `n_null` counts pairs dropped because either side
    !> was null and `n_nan` those dropped for a NaN and not already null, so the two counts
    !> describe pairs rather than elements -- which is the only thing they could describe, since
    !> a covariance between vectors of different lengths is not a number.
    !>
    !> `ddof = 1` by default (the sample covariance, as pandas returns); pass `ddof = 0` for
    !> numpy's. `pf_cov(x, x)` is exactly `pf_variance(x)` at the same `ddof`, and the test
    !> suite asserts it.
    !>
    !> **Both arrays must be the same kind.** Six specifics rather than thirty-six: a caller
    !> mixing kinds writes `real(x, real64)`, which is one call and says what it does.
    !>
    !> Nulls, NaNs and zero-weight elements leave the population exactly as they do for the
    !> moments. An undefined answer is a **quiet NaN** with `ok = .false.`, never an abort: this
    !> module aborts on misuse and never on a data condition.
    !>
    !> Every argument may be a `real(real64)`, `real(real32)`, `integer(int32)`, `integer(int64)`
    !> or `logical` array, or a scalar numeric `type(parquet_column)`.
    interface pf_cov
        module procedure cov_f64
        module procedure cov_i32
        module procedure cov_i64
        module procedure cov_f32
        module procedure cov_bool
        module procedure cov_col
    end interface pf_cov
    !
    !> The correlation of two samples: Pearson by default, Spearman on request.
    !>
    !> `method="pearson"` is `cov(x, y) / (sd(x) sd(y))`. `method="spearman"` is the same
    !> formula over MIDRANKS -- each run of equal values receives the mean of the sorted
    !> positions it spans -- so it measures any monotone relationship rather than a linear one.
    !>
    !> Pairwise-complete, exactly as `pf_cov` is, and with the same pair-counting rule. There is
    !> deliberately **no `ddof`**: the one in the covariance and the two in the standard
    !> deviations cancel exactly, so the argument could never change the answer.
    !>
    !> `pf_corr(x, x)` is exactly `1` for any non-constant sample, under either method. A
    !> constant sample has zero variance and gives a quiet NaN with `ok = .false.`.
    !>
    !> Nulls, NaNs and zero-weight elements leave the population exactly as they do for the
    !> moments. An undefined answer is a **quiet NaN** with `ok = .false.`, never an abort: this
    !> module aborts on misuse and never on a data condition.
    !>
    !> Every argument may be a `real(real64)`, `real(real32)`, `integer(int32)`, `integer(int64)`
    !> or `logical` array, or a scalar numeric `type(parquet_column)`.
    interface pf_corr
        module procedure corr_f64
        module procedure corr_i32
        module procedure corr_i64
        module procedure corr_f32
        module procedure corr_bool
        module procedure corr_col
    end interface pf_corr
    !
    !> Standardises a population in place of the caller's own loop -- `scipy.stats.zscore`.
    !>
    !> `z(i) = (values(i) - mean) / stddev`, with `ddof = 1` by default. `z` is the same size as
    !> `values` and the mean and standard deviation are taken over the elements that survive the
    !> exclusion rules.
    !>
    !> **An excluded element has no standardised value**, and there are two ways to learn which:
    !> `out_valid(:)` marks them exactly, and without it they are written as quiet NaNs. `ok`
    !> comes back `.false.` in either case, and also when the population's variance is zero --
    !> in which case EVERY output is NaN. `n_null` separates the two: it is `0` for the second.
    !>
    !> The result has mean 0 and standard deviation 1 over the surviving elements, to a few ulp.
    !>
    !> Nulls, NaNs and zero-weight elements leave the population exactly as they do for the
    !> moments. An undefined answer is a **quiet NaN** with `ok = .false.`, never an abort: this
    !> module aborts on misuse and never on a data condition.
    !>
    !> Every argument may be a `real(real64)`, `real(real32)`, `integer(int32)`, `integer(int64)`
    !> or `logical` array, or a scalar numeric `type(parquet_column)`.
    interface pf_zscore
        module procedure zscore_f64
        module procedure zscore_i32
        module procedure zscore_i64
        module procedure zscore_f32
        module procedure zscore_bool
        module procedure zscore_col
    end interface pf_zscore
    !
    !> Replaces each value by the normal quantile its RANK marks -- rankits, normal scores, the
    !> van der Waerden transform; the x-axis of a Q-Q plot against a Gaussian.
    !>
    !> `s(i) = Phi**(-1)((r(i) - a)/(m + 1 - 2a))`, where `r(i)` is `values(i)`'s MIDRANK among
    !> the `m` elements that survive the exclusion rules -- each run of equal values receiving
    !> the mean of the sorted positions it spans, exactly as `pf_corr(method="spearman")`
    !> does -- and `a` comes from `method=`. `s` is the same size as `values` and is in the
    !> caller's original order.
    !>
    !> **The result depends on the input only through its ORDER**, so it is unchanged, bit for
    !> bit, by any strictly monotone transform of the values: `pf_normal_scores(x)` and
    !> `pf_normal_scores(exp(x))` are the same array. Ranks are taken over the SURVIVORS, so
    !> excluding a quarter of the population gives the remaining elements exactly the scores
    !> they would have had as a population of their own.
    !>
    !> **An excluded element has no score**, and there are two ways to learn which: `out_valid`
    !> marks them exactly, and without it they are written as quiet NaNs -- `pf_zscore`'s rule
    !> verbatim. `ok` is `.false.` in either case. A one-element population scores that element
    !> `Phi**(-1)(1/2) = 0`, which is the correct degenerate answer rather than a failure.
    !>
    !> There is deliberately **no `weights`** and **no `pf_stats` binding**: a weighted midrank
    !> is a definitional choice no reference library makes, and tier B sorts in place and never
    !> hands values back in the caller's order.
    !>
    !> Nulls, NaNs and zero-weight elements leave the population exactly as they do for the
    !> moments. An undefined answer is a **quiet NaN** with `ok = .false.`, never an abort: this
    !> module aborts on misuse and never on a data condition.
    !>
    !> Every argument may be a `real(real64)`, `real(real32)`, `integer(int32)`, `integer(int64)`
    !> or `logical` array, or a scalar numeric `type(parquet_column)`.
    interface pf_normal_scores
        module procedure normal_scores_f64
        module procedure normal_scores_i32
        module procedure normal_scores_i64
        module procedure normal_scores_f32
        module procedure normal_scores_bool
        module procedure normal_scores_col
    end interface pf_normal_scores
    !
    !> astropy's `sigma_clipped_stats`: iteratively drop the outliers, then summarise the rest.
    !>
    !> Each round computes a centre and a scale over the values still surviving and keeps
    !> `centre - sigma_lower*scale <= x <= centre + sigma_upper*scale`, inclusive at both ends,
    !> as astropy does. `cenfunc=` chooses the centre (`"median"` by default, or `"mean"`)
    !> and `stdfunc=` the scale (`"std"` by default, or `"mad_std"`, which is far less
    !> disturbed by the outliers being clipped). `sigma = 3.0` and `maxiters = 5` are astropy's
    !> defaults.
    !>
    !> **The whole run costs ONE ordering.** The keep condition is an interval, so the survivors
    !> are always a contiguous sub-range of the sorted order and each round only moves two
    !> indices inward.
    !>
    !> `maxiters <= 0` iterates until a round removes nothing, capped at 100 rounds. Reaching
    !> that cap stops and reports through `converged = .false.`; it does NOT abort, because each
    !> non-terminating round removes at least one point and a large catalogue can honestly want
    !> more rounds than any small constant.
    !>
    !> **A round that would leave fewer than two values stops instead**, and the previous round's
    !> survivors are what is reported. An empty population gives three NaNs with `ok = .false.`
    !> and `n_clipped = 0`.
    !>
    !> `keep(:)` optionally returns the surviving mask over the ORIGINAL array, which is what a
    !> caller needs in order to apply the same clip to a second column.
    !>
    !> **There is no `weights` argument**, which is stronger than refusing one at run time: a
    !> weighted scale estimator is a further definitional choice that no workload in evidence
    !> asks for, and its absence makes passing one a compile error.
    !>
    !> Nulls, NaNs and zero-weight elements leave the population exactly as they do for the
    !> moments. An undefined answer is a **quiet NaN** with `ok = .false.`, never an abort: this
    !> module aborts on misuse and never on a data condition.
    !>
    !> Every argument may be a `real(real64)`, `real(real32)`, `integer(int32)`, `integer(int64)`
    !> or `logical` array, or a scalar numeric `type(parquet_column)`.
    interface pf_sigma_clipped_stats
        module procedure sigma_clipped_stats_f64
        module procedure sigma_clipped_stats_i32
        module procedure sigma_clipped_stats_i64
        module procedure sigma_clipped_stats_f32
        module procedure sigma_clipped_stats_bool
        module procedure sigma_clipped_stats_col
    end interface pf_sigma_clipped_stats
    !
    !> The running sum of a population -- pandas' `Series.cumsum()`.
    !>
    !> **The last element is NOT `pf_sum(values)`, and that is arithmetic rather than a bug.**
    !> A running sum is sequential by definition, so it accumulates left to right; `pf_sum` is
    !> PAIRWISE over a fixed block tree. The two agree to within the accumulated rounding of a
    !> naive sum, which on a large or badly scaled population is several ulps and occasionally
    !> more. Compare them with a tolerance, or take the total from `pf_sum`.
    !>
    !> **An `integer(int64)` population widens to `real64` first**, so a value above `2**53` is
    !> rounded on the way in and the running sum inherits that -- see `pf_sum` for the bound
    !> and for what to do instead. int32 is unaffected.
    !>
    !> **An excluded element yields an excluded OUTPUT element, and the running value continues
    !> past it unchanged** -- which is what pandas does, and the whole reason this family needs a
    !> paragraph of its own. `pf_cumsum([1, null, 3])` is `[1, undefined, 4]`: the null contributes
    !> nothing and is not carried forward. "The output element is undefined" and "the rest of the
    !> output is undefined" are one careless line apart, and only the first is meant.
    !>
    !> An undefined output element is `.false.` in `out_valid` when that mask is present, and a
    !> **quiet NaN** when it is not -- there being no other way to say "no value" in a
    !> `real(real64)` array. `ok` comes back `.false.` when any element was excluded; `n_null` and
    !> `n_nan` say why. This module aborts on misuse (`out` the wrong size) and never on a data
    !> condition.
    !>
    !> `skipnan = .false.` is the one case where a NaN DOES poison what follows: it is then an
    !> ordinary value, the running sum of a NaN is a NaN, and every later element is one too.
    !> That is numpy's `np.cumsum` and pandas' `skipna=False`.
    !>
    !> `values` may be a `real(real64)`, `real(real32)`, `integer(int32)`, `integer(int64)` or
    !> `logical` array, or a scalar numeric `type(parquet_column)`; `out` is always
    !> `real(real64)`, so a running sum of 64-bit integers cannot silently wrap.
    !>
    !> **`out` must not be `values`.** There is no in-place form: passing one array to both
    !> arguments associates it with an `intent(in)` and an `intent(out)` dummy at once, which
    !> F2018 15.5.2.13 forbids and no compiler here diagnoses. The scan would in fact survive it,
    !> since it reads element `i` before writing element `i` -- which is exactly what makes the
    !> mistake worth naming rather than leaving to be discovered.
    interface pf_cumsum
        module procedure cumsum_f64
        module procedure cumsum_i32
        module procedure cumsum_i64
        module procedure cumsum_f32
        module procedure cumsum_bool
        module procedure cumsum_col
    end interface pf_cumsum
    !
    !> The running product of a population -- pandas' `Series.cumprod()`.
    !>
    !> Formed as a product and not in log space, deliberately: unlike `pf_gmean` this has to
    !> reproduce every intermediate value, including the sign, so a magnitude that overflows to
    !> infinity is the honest answer rather than something to be routed around.
    !>
    !> **An excluded element yields an excluded OUTPUT element, and the running value continues
    !> past it unchanged** -- which is what pandas does, and the whole reason this family needs a
    !> paragraph of its own. `pf_cumsum([1, null, 3])` is `[1, undefined, 4]`: the null contributes
    !> nothing and is not carried forward. "The output element is undefined" and "the rest of the
    !> output is undefined" are one careless line apart, and only the first is meant.
    !>
    !> An undefined output element is `.false.` in `out_valid` when that mask is present, and a
    !> **quiet NaN** when it is not -- there being no other way to say "no value" in a
    !> `real(real64)` array. `ok` comes back `.false.` when any element was excluded; `n_null` and
    !> `n_nan` say why. This module aborts on misuse (`out` the wrong size) and never on a data
    !> condition.
    !>
    !> `skipnan = .false.` is the one case where a NaN DOES poison what follows: it is then an
    !> ordinary value, the running sum of a NaN is a NaN, and every later element is one too.
    !> That is numpy's `np.cumsum` and pandas' `skipna=False`.
    !>
    !> `values` may be a `real(real64)`, `real(real32)`, `integer(int32)`, `integer(int64)` or
    !> `logical` array, or a scalar numeric `type(parquet_column)`; `out` is always
    !> `real(real64)`, so a running sum of 64-bit integers cannot silently wrap.
    !>
    !> **`out` must not be `values`.** There is no in-place form: passing one array to both
    !> arguments associates it with an `intent(in)` and an `intent(out)` dummy at once, which
    !> F2018 15.5.2.13 forbids and no compiler here diagnoses. The scan would in fact survive it,
    !> since it reads element `i` before writing element `i` -- which is exactly what makes the
    !> mistake worth naming rather than leaving to be discovered.
    interface pf_cumprod
        module procedure cumprod_f64
        module procedure cumprod_i32
        module procedure cumprod_i64
        module procedure cumprod_f32
        module procedure cumprod_bool
        module procedure cumprod_col
    end interface pf_cumprod
    !
    !> The running maximum of a population -- pandas' `Series.cummax()`.
    !>
    !> The comparison is IEEE `>`, so a negative zero and a positive zero compare equal and the
    !> first of them is kept -- which is what numpy and pandas do.
    !>
    !> **An excluded element yields an excluded OUTPUT element, and the running value continues
    !> past it unchanged** -- which is what pandas does, and the whole reason this family needs a
    !> paragraph of its own. `pf_cumsum([1, null, 3])` is `[1, undefined, 4]`: the null contributes
    !> nothing and is not carried forward. "The output element is undefined" and "the rest of the
    !> output is undefined" are one careless line apart, and only the first is meant.
    !>
    !> An undefined output element is `.false.` in `out_valid` when that mask is present, and a
    !> **quiet NaN** when it is not -- there being no other way to say "no value" in a
    !> `real(real64)` array. `ok` comes back `.false.` when any element was excluded; `n_null` and
    !> `n_nan` say why. This module aborts on misuse (`out` the wrong size) and never on a data
    !> condition.
    !>
    !> `skipnan = .false.` is the one case where a NaN DOES poison what follows: it is then an
    !> ordinary value, the running sum of a NaN is a NaN, and every later element is one too.
    !> That is numpy's `np.cumsum` and pandas' `skipna=False`.
    !>
    !> `values` may be a `real(real64)`, `real(real32)`, `integer(int32)`, `integer(int64)` or
    !> `logical` array, or a scalar numeric `type(parquet_column)`; `out` is always
    !> `real(real64)`, so a running sum of 64-bit integers cannot silently wrap.
    !>
    !> **`out` must not be `values`.** There is no in-place form: passing one array to both
    !> arguments associates it with an `intent(in)` and an `intent(out)` dummy at once, which
    !> F2018 15.5.2.13 forbids and no compiler here diagnoses. The scan would in fact survive it,
    !> since it reads element `i` before writing element `i` -- which is exactly what makes the
    !> mistake worth naming rather than leaving to be discovered.
    interface pf_cummax
        module procedure cummax_f64
        module procedure cummax_i32
        module procedure cummax_i64
        module procedure cummax_f32
        module procedure cummax_bool
        module procedure cummax_col
    end interface pf_cummax
    !
    !> The running minimum of a population -- pandas' `Series.cummin()`.
    !>
    !> The comparison is IEEE `<`, so a negative zero and a positive zero compare equal and the
    !> first of them is kept -- which is what numpy and pandas do.
    !>
    !> **An excluded element yields an excluded OUTPUT element, and the running value continues
    !> past it unchanged** -- which is what pandas does, and the whole reason this family needs a
    !> paragraph of its own. `pf_cumsum([1, null, 3])` is `[1, undefined, 4]`: the null contributes
    !> nothing and is not carried forward. "The output element is undefined" and "the rest of the
    !> output is undefined" are one careless line apart, and only the first is meant.
    !>
    !> An undefined output element is `.false.` in `out_valid` when that mask is present, and a
    !> **quiet NaN** when it is not -- there being no other way to say "no value" in a
    !> `real(real64)` array. `ok` comes back `.false.` when any element was excluded; `n_null` and
    !> `n_nan` say why. This module aborts on misuse (`out` the wrong size) and never on a data
    !> condition.
    !>
    !> `skipnan = .false.` is the one case where a NaN DOES poison what follows: it is then an
    !> ordinary value, the running sum of a NaN is a NaN, and every later element is one too.
    !> That is numpy's `np.cumsum` and pandas' `skipna=False`.
    !>
    !> `values` may be a `real(real64)`, `real(real32)`, `integer(int32)`, `integer(int64)` or
    !> `logical` array, or a scalar numeric `type(parquet_column)`; `out` is always
    !> `real(real64)`, so a running sum of 64-bit integers cannot silently wrap.
    !>
    !> **`out` must not be `values`.** There is no in-place form: passing one array to both
    !> arguments associates it with an `intent(in)` and an `intent(out)` dummy at once, which
    !> F2018 15.5.2.13 forbids and no compiler here diagnoses. The scan would in fact survive it,
    !> since it reads element `i` before writing element `i` -- which is exactly what makes the
    !> mistake worth naming rather than leaving to be discovered.
    interface pf_cummin
        module procedure cummin_f64
        module procedure cummin_i32
        module procedure cummin_i64
        module procedure cummin_f32
        module procedure cummin_bool
        module procedure cummin_col
    end interface pf_cummin
    !
    !> Which bin of a sorted edge array each value falls in -- pandas' `pd.cut`.
    !>
    !> The primitive behind a histogram, a grouped aggregation over ranges, and every
    !> hand-written `if (x < a) then ... else if (x < b)` ladder. `codes(i)` is the 1-based
    !> number of the bin holding `values(i)`, or **0** when it joined none: excluded by
    !> `is_valid`, a NaN, carrying a zero weight, or outside `[edges(1), edges(nbins+1)]`.
    !> `n_null`, `n_nan` and `n_outside` separate three of those four exactly; a zero-weighted
    !> element is deliberately in none of them and leaves `ok` alone, exactly as it does in
    !> `pf_histogram`, because it was removed from the POPULATION rather than failing to reach
    !> a bin. `pf_count_valid` over the same arguments is what reports it.
    !>
    !> The search is binary, so the cost is `O(n log nbins)` and an edge array with thousands
    !> of bins is as cheap as one with four.
    !>
    !> `edges` must be **strictly increasing** and hold at least two entries, or the call aborts
    !> naming the offending index -- an equal pair would describe a bin no value can reach, and a
    !> descending one is always a mistake. An infinite outer edge is allowed and is the way to
    !> ask for an open-ended first or last bin. A NaN edge aborts.
    !>
    !> Nothing here aborts on a data condition: a value outside the edges, a NaN, a null and an
    !> entirely empty population are all ordinary and are reported through the counts. `ok` is
    !> `.false.` when any element failed to reach a bin, for any of those reasons at once.
    !>
    !> `values` may be a `real(real64)`, `real(real32)`, `integer(int32)`, `integer(int64)` or
    !> `logical` array, or a scalar numeric `type(parquet_column)`. `edges` is always
    !> `real(real64)`: it states a rule rather than carrying data, and one type for it keeps the
    !> generic resolving on `values` alone.
    interface pf_bucketize
        module procedure bucketize_f64
        module procedure bucketize_i32
        module procedure bucketize_i64
        module procedure bucketize_f32
        module procedure bucketize_bool
        module procedure bucketize_col
    end interface pf_bucketize
    !
    !> How much of a population falls in each bin -- `np.histogram` with explicit `bins=`.
    !>
    !> `counts(k)` is the number of values in bin k, or the SUM OF THEIR WEIGHTS when `weights`
    !> is present, which is what `np.histogram(weights=)` returns and why the output is real
    !> rather than integer. Values outside the edge range join no bin and are reported through
    !> `n_outside`; numpy drops them silently, so this is strictly more information.
    !>
    !> This is `pf_bucketize` followed by a tally, and the two agree by construction -- the
    !> count in bin k is the total WEIGHT of the `codes` equal to k over the same arguments,
    !> which is why `pf_bucketize` takes `weights` even though a weight cannot change a bin
    !> number. Unweighted, that total weight is a count.
    !>
    !> `edges` must be **strictly increasing** and hold at least two entries, or the call aborts
    !> naming the offending index -- an equal pair would describe a bin no value can reach, and a
    !> descending one is always a mistake. An infinite outer edge is allowed and is the way to
    !> ask for an open-ended first or last bin. A NaN edge aborts.
    !>
    !> Nothing here aborts on a data condition: a value outside the edges, a NaN, a null and an
    !> entirely empty population are all ordinary and are reported through the counts. `ok` is
    !> `.false.` when any element failed to reach a bin, for any of those reasons at once.
    !>
    !> `values` may be a `real(real64)`, `real(real32)`, `integer(int32)`, `integer(int64)` or
    !> `logical` array, or a scalar numeric `type(parquet_column)`. `edges` is always
    !> `real(real64)`: it states a rule rather than carrying data, and one type for it keeps the
    !> generic resolving on `values` alone.
    interface pf_histogram
        module procedure histogram_f64
        module procedure histogram_i32
        module procedure histogram_i64
        module procedure histogram_f32
        module procedure histogram_bool
        module procedure histogram_col
    end interface pf_histogram
    !
    !> `nbins` equal-width bins spanning a population -- `np.histogram_bin_edges(bins=n)`.
    !>
    !> The half of `np.histogram(bins=10)` this library does not fold into `pf_histogram`: it finds
    !> the range and lays `nbins + 1` boundaries across it, ready to hand straight to
    !> `pf_histogram` or `pf_bucketize`. The population is the same one they would bin -- nulls,
    !> NaNs and zero-weight elements leave it by the module's usual rules -- so a zero-weighted
    !> extreme value does not stretch the range over data that will not be counted.
    !>
    !> **The edges are ALWAYS strictly increasing, which is a contract rather than an**
    !> **observation**: they exist to be passed to `pf_histogram`, which aborts on a pair that is
    !> not, so a degenerate population must not produce edges that abort one call later.
    !> `ok = .false.` says the edges do not describe the data's own range, and there are three
    !> ways to get it -- an EMPTY population, which falls back to `[0, 1]` as numpy does; a
    !> CONSTANT one, which widens to `[x - 0.5, x + 0.5]`, also as numpy does; and an `nbins`
    !> finer than double precision can resolve over the range, where the spacing would collapse
    !> and neighbouring edges are nudged apart instead. In all three the edges are still usable.
    !>
    !> **There is no explicit range pair.** A caller who already knows the bounds can write the
    !> `nbins + 1` values directly; what is worth a procedure is finding the range under this
    !> module's exclusion rules, which is the part that is easy to get subtly wrong.
    !>
    !> `nbins` is a plain default-kind `integer` and has no int64 form. It is bounded by the size
    !> of `edges`, so it can never legitimately exceed `huge(1_int32)`.
    !>
    !> `edges` must be **strictly increasing** and hold at least two entries, or the call aborts
    !> naming the offending index -- an equal pair would describe a bin no value can reach, and a
    !> descending one is always a mistake. An infinite outer edge is allowed and is the way to
    !> ask for an open-ended first or last bin. A NaN edge aborts.
    !>
    !> Nothing here aborts on a data condition: a value outside the edges, a NaN, a null and an
    !> entirely empty population are all ordinary and are reported through the counts. `ok` is
    !> `.false.` when any element failed to reach a bin, for any of those reasons at once.
    !>
    !> `values` may be a `real(real64)`, `real(real32)`, `integer(int32)`, `integer(int64)` or
    !> `logical` array, or a scalar numeric `type(parquet_column)`. `edges` is always
    !> `real(real64)`: it states a rule rather than carrying data, and one type for it keeps the
    !> generic resolving on `values` alone.
    interface pf_bin_edges
        module procedure bin_edges_f64
        module procedure bin_edges_i32
        module procedure bin_edges_i64
        module procedure bin_edges_f32
        module procedure bin_edges_bool
        module procedure bin_edges_col
    end interface pf_bin_edges
    !
    !> The most common value of a population -- pandas' `mode()`, scipy's `stats.mode`.
    !>
    !> **Integer, logical and string kinds only.** The real kinds are not overloads that abort at
    !> run time; they are absent from the generic, so `pf_mode` over a real array does not
    !> compile. The mode of a continuous variable is not a statistic, and floating-point equality
    !> is a trap rather than an answer -- `pf_bucketize` and `pf_histogram` are what that question
    !> actually wants.
    !>
    !> **Ties go to the SMALLEST value**, matching `scipy.stats.mode`, and never to the first
    !> occurrence: an answer that depended on input order would differ between an array and its
    !> own permutation, which is a reproducibility defect rather than a preference.
    !>
    !> **`modes` gives every tied value instead of just the smallest**, in ascending order, which
    !> is what pandas' `Series.mode()` returns. It is allocated to exactly the number that tie, so
    !> `size(modes)` says how many there were and `modes(1)` is always `m`. A population with one
    !> clear winner gives a one-element array, and an empty one gives a zero-length array rather
    !> than an unallocated result.
    !>
    !> `count` reports how many elements hold the modal value. Nulls are excluded and counted
    !> through `n_null`, as everywhere in this module; no kind here can hold a NaN, so there is
    !> deliberately no `skipnan` or `n_nan`.
    !>
    !> **Weights are supported**: the mode is then the value carrying the greatest total weight,
    !> ties still broken by the smallest value, and `count` still counts elements rather than
    !> weight. A zero weight removes the element, as everywhere else.
    !>
    !> An empty population gives `ok = .false.` and `count = 0`, and `m` must not be read.
    interface pf_mode
        module procedure mode_i32
        module procedure mode_i64
        module procedure mode_bool
        module procedure mode_chr
        module procedure mode_str
    end interface pf_mode
    !
    ! ---- Shared argument guards (implemented in parquet_stats_core) ----
    interface
        !> Renders an index for an error message without a deferred-length result.
        !!
        !! A fixed-length function result is fine; a `character(len=:), allocatable` one is the
        !! shape this project forbids outright, because gfortran's hidden length temporary for it
        !! is not reliably thread-local.
        pure module function stats_i2s(v) result(res)
            integer(int64), intent(in) :: v !! the value to render.
            character(len=24) :: res !! `v` in decimal, blank-padded.
        end function stats_i2s
        !> Aborts unless every present optional array matches `values` in size.
        module subroutine stats_check_sizes(nv, what, is_valid, weights)
            integer(int64), intent(in) :: nv !! the number of elements in `values`.
            character(len=*), intent(in) :: what !! the public procedure's name, for the message.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
            real(real64), intent(in), optional :: weights(:) !! per element weight.
        end subroutine stats_check_sizes
        !> Aborts unless `w` is a usable weight: finite and non-negative.
        !!
        !! The three tests are separate statements on purpose. Fortran does not short-circuit, so
        !! `w /= w .or. w < 0` would evaluate the `<` on a NaN -- and `<` on a NaN raises
        !! `IEEE_INVALID`, which nagfor's default `-ieee=stop` turns into a dead process.
        module subroutine stats_check_weight(w, i, what)
            real(real64), intent(in) :: w !! the weight to validate.
            integer(int64), intent(in) :: i !! its index, for the message.
            character(len=*), intent(in) :: what !! the public procedure's name, for the message.
        end subroutine stats_check_weight
    end interface
    !
    ! ---- The real64 moment core (implemented in parquet_stats_core) ----
    interface
        !> `pf_sum` over a 64-bit real array: the PAIRWISE sum of the population.
        !>
        !> Weighted, this is `sum(w*x)`. An empty population sums to exactly `0` with
        !> `ok = .true.`, which is the additive identity and what numpy and pandas return -- it
        !> is the one quantity in this family that an empty population still defines.
        !>
        !> **An `integer(int64)` population is summed in `real64`, so it is exact only while
        !> every value is**, i.e. while `abs(v) <= 2**53`. Above that the WIDENING loses the low
        !> bits of each element before any addition happens, so no summation order recovers
        !> them: `pf_sum([2**53+1, 1, 2**53+1, 1])` answers `1.8014398509481984e16` where the
        !> exact integer total is `18014398509481988`. numpy and pandas keep an int64 sum in
        !> int64 and stay exact (until they wrap). This module has one engine and one
        !> `real(real64)` result -- which is what makes every kind return the same bits -- so
        !> a population of genuinely huge integers has to be summed by the caller, in int64, or
        !> shifted and scaled before it gets here. int32 is unaffected: every int32 value is
        !> exactly representable.
        module subroutine sum_f64(values, s, is_valid, weights, skipnan, n_null, n_nan, ok, threads)
            real(real64), intent(in) :: values(:) !! the population.
            real(real64), intent(out) :: s !! the sum.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight, non-negative. A ZERO weight removes the element from the
            !! population; a negative, NaN or infinite one aborts. Absent means every weight is 1.
            logical, intent(in), optional :: skipnan
            !! .true. (the default) excludes a NaN from the population, as a null is excluded and
            !! as `pf_minmax` has always done; .false. restores numpy's propagating behaviour, in
            !! which one NaN makes every answer NaN.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            integer(int64), intent(out), optional :: n_nan
            !! how many were excluded as NaN and were not already null.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
            integer, intent(in), optional :: threads
            !! how many threads the central-moment pass may use. Absent takes the automatic rule:
            !! the `sort_threads` setting, capped by the processors actually available and
            !! by a measured work floor, and 1 inside a caller's own parallel region. **The answer
            !! does not depend on this argument** -- the block decomposition is a function of the
            !! population size alone, so 1, 8 and a build with no OpenMP at all return the same
            !! bits. It is a speed control and never an accuracy one.
        end subroutine sum_f64
        !> `pf_mean` over a 64-bit real array: `sum(w*x) / sum(w)`, REFINED.
        !>
        !> The quotient is taken first and corrected by `sum(w*(x - mu))/sum(w)`, so the result
        !> can sit one ulp from the naive quotient. See `pf_mean`'s own doc-comment for why that
        !> is the right answer and what it means for a test.
        module subroutine mean_f64(values, m, is_valid, weights, skipnan, n_null, n_nan, ok, threads)
            real(real64), intent(in) :: values(:) !! the population.
            real(real64), intent(out) :: m !! the mean; NaN when the population is empty.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight, non-negative. A ZERO weight removes the element from the
            !! population; a negative, NaN or infinite one aborts. Absent means every weight is 1.
            logical, intent(in), optional :: skipnan
            !! .true. (the default) excludes a NaN from the population, as a null is excluded and
            !! as `pf_minmax` has always done; .false. restores numpy's propagating behaviour, in
            !! which one NaN makes every answer NaN.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            integer(int64), intent(out), optional :: n_nan
            !! how many were excluded as NaN and were not already null.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
            integer, intent(in), optional :: threads
            !! how many threads the central-moment pass may use. Absent takes the automatic rule:
            !! the `sort_threads` setting, capped by the processors actually available and
            !! by a measured work floor, and 1 inside a caller's own parallel region. **The answer
            !! does not depend on this argument** -- the block decomposition is a function of the
            !! population size alone, so 1, 8 and a build with no OpenMP at all return the same
            !! bits. It is a speed control and never an accuracy one.
        end subroutine mean_f64
        !> `pf_gmean` over a 64-bit real array: the geometric mean.
        !>
        !> `exp(sum(w*log(x)) / sum(w))`, which is `scipy.stats.gmean`. Computed in log space
        !> rather than as a product, because the product of a few thousand magnitudes overflows
        !> and its logarithm does not.
        !>
        !> Two values outside the domain are answered rather than refused: a value **exactly 0**
        !> gives exactly `0` (the limit, and scipy's answer), and any **negative** value gives a
        !> quiet NaN with `ok = .false.`. Both are branched on BEFORE the logarithm is reached,
        !> which is a correctness requirement rather than tidiness -- `log(0)` raises
        !> `IEEE_DIVIDE_BY_ZERO` and nagfor unmasks the IEEE traps by default, so reaching it
        !> would terminate the process on that compiler while returning a plausible answer on
        !> every other one.
        module subroutine gmean_f64(values, g, is_valid, weights, skipnan, n_null, n_nan, ok, threads)
            real(real64), intent(in) :: values(:) !! the population.
            real(real64), intent(out) :: g !! the geometric mean; 0 for a zero, NaN for a negative or empty population.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight, non-negative. A ZERO weight removes the element from the
            !! population; a negative, NaN or infinite one aborts. Absent means every weight is 1.
            logical, intent(in), optional :: skipnan
            !! .true. (the default) excludes a NaN from the population, as a null is excluded and
            !! as `pf_minmax` has always done; .false. restores numpy's propagating behaviour, in
            !! which one NaN makes every answer NaN.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            integer(int64), intent(out), optional :: n_nan
            !! how many were excluded as NaN and were not already null.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
            integer, intent(in), optional :: threads
            !! how many threads the central-moment pass may use. Absent takes the automatic rule:
            !! the `sort_threads` setting, capped by the processors actually available and
            !! by a measured work floor, and 1 inside a caller's own parallel region. **The answer
            !! does not depend on this argument** -- the block decomposition is a function of the
            !! population size alone, so 1, 8 and a build with no OpenMP at all return the same
            !! bits. It is a speed control and never an accuracy one.
        end subroutine gmean_f64
        !> `pf_hmean` over a 64-bit real array: the harmonic mean.
        !>
        !> `sum(w) / sum(w/x)`, which is `scipy.stats.hmean`. Same domain rules as `pf_gmean`
        !> and for the same reason: a value exactly 0 gives exactly `0`, a negative one gives a
        !> quiet NaN, and both are branched on before the reciprocal, since `1/0` raises under
        !> nagfor's default trap policy.
        module subroutine hmean_f64(values, h, is_valid, weights, skipnan, n_null, n_nan, ok, threads)
            real(real64), intent(in) :: values(:) !! the population.
            real(real64), intent(out) :: h !! the harmonic mean; 0 for a zero, NaN for a negative or empty population.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight, non-negative. A ZERO weight removes the element from the
            !! population; a negative, NaN or infinite one aborts. Absent means every weight is 1.
            logical, intent(in), optional :: skipnan
            !! .true. (the default) excludes a NaN from the population, as a null is excluded and
            !! as `pf_minmax` has always done; .false. restores numpy's propagating behaviour, in
            !! which one NaN makes every answer NaN.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            integer(int64), intent(out), optional :: n_nan
            !! how many were excluded as NaN and were not already null.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
            integer, intent(in), optional :: threads
            !! how many threads the central-moment pass may use. Absent takes the automatic rule:
            !! the `sort_threads` setting, capped by the processors actually available and
            !! by a measured work floor, and 1 inside a caller's own parallel region. **The answer
            !! does not depend on this argument** -- the block decomposition is a function of the
            !! population size alone, so 1, 8 and a build with no OpenMP at all return the same
            !! bits. It is a speed control and never an accuracy one.
        end subroutine hmean_f64
        !> `pf_variance` over a 64-bit real array, computed in TWO passes.
        !>
        !> The mean is taken first and the central moment accumulated against it, so the result
        !> is shift-invariant to a few ulp: `pf_variance(x + 1e9)` agrees with `pf_variance(x)`,
        !> which the textbook `sum(x**2) - sum(x)**2/n` does not.
        module subroutine variance_f64(values, v, is_valid, weights, weight_type, ddof, skipnan, n_null, n_nan, ok, threads)
            real(real64), intent(in) :: values(:) !! the population.
            real(real64), intent(out) :: v !! the variance; NaN when `n_valid <= ddof`.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight, non-negative. A ZERO weight removes the element from the
            !! population; a negative, NaN or infinite one aborts. Absent means every weight is 1.
            character(len=*), intent(in), optional :: weight_type
            !! "reliability" (the default) or "frequency". The two differ only when the weights are
            !! unequal: a FREQUENCY weight of 3 says the value occurred three times, so the count
            !! `ddof` is charged against is `sum(weights)`; a RELIABILITY weight says the value is
            !! that much more precise, so the count is Kish's effective size
            !! `sum(weights)**2 / sum(weights**2)`. Any other token aborts, listing both.
            integer, intent(in), optional :: ddof
            !! delta degrees of freedom; **1 by default** -- the sample variance, as pandas
            !! returns. numpy's default is 0, so pass `ddof=0` to reproduce `np.var`/`np.std`.
            !! A plain default-kind integer deliberately: it is bounded by the population size,
            !! so there is no int64 form to provide. `ddof >= n_valid` gives NaN, not a division.
            logical, intent(in), optional :: skipnan
            !! .true. (the default) excludes a NaN from the population, as a null is excluded and
            !! as `pf_minmax` has always done; .false. restores numpy's propagating behaviour, in
            !! which one NaN makes every answer NaN.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            integer(int64), intent(out), optional :: n_nan
            !! how many were excluded as NaN and were not already null.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
            integer, intent(in), optional :: threads
            !! how many threads the central-moment pass may use. Absent takes the automatic rule:
            !! the `sort_threads` setting, capped by the processors actually available and
            !! by a measured work floor, and 1 inside a caller's own parallel region. **The answer
            !! does not depend on this argument** -- the block decomposition is a function of the
            !! population size alone, so 1, 8 and a build with no OpenMP at all return the same
            !! bits. It is a speed control and never an accuracy one.
        end subroutine variance_f64
        !> `pf_stddev` over a 64-bit real array: the square root of `pf_variance`.
        module subroutine stddev_f64(values, sd, is_valid, weights, weight_type, ddof, skipnan, n_null, n_nan, ok, threads)
            real(real64), intent(in) :: values(:) !! the population.
            real(real64), intent(out) :: sd !! the standard deviation; NaN when the variance is.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight, non-negative. A ZERO weight removes the element from the
            !! population; a negative, NaN or infinite one aborts. Absent means every weight is 1.
            character(len=*), intent(in), optional :: weight_type
            !! "reliability" (the default) or "frequency". The two differ only when the weights are
            !! unequal: a FREQUENCY weight of 3 says the value occurred three times, so the count
            !! `ddof` is charged against is `sum(weights)`; a RELIABILITY weight says the value is
            !! that much more precise, so the count is Kish's effective size
            !! `sum(weights)**2 / sum(weights**2)`. Any other token aborts, listing both.
            integer, intent(in), optional :: ddof
            !! delta degrees of freedom; **1 by default** -- the sample variance, as pandas
            !! returns. numpy's default is 0, so pass `ddof=0` to reproduce `np.var`/`np.std`.
            !! A plain default-kind integer deliberately: it is bounded by the population size,
            !! so there is no int64 form to provide. `ddof >= n_valid` gives NaN, not a division.
            logical, intent(in), optional :: skipnan
            !! .true. (the default) excludes a NaN from the population, as a null is excluded and
            !! as `pf_minmax` has always done; .false. restores numpy's propagating behaviour, in
            !! which one NaN makes every answer NaN.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            integer(int64), intent(out), optional :: n_nan
            !! how many were excluded as NaN and were not already null.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
            integer, intent(in), optional :: threads
            !! how many threads the central-moment pass may use. Absent takes the automatic rule:
            !! the `sort_threads` setting, capped by the processors actually available and
            !! by a measured work floor, and 1 inside a caller's own parallel region. **The answer
            !! does not depend on this argument** -- the block decomposition is a function of the
            !! population size alone, so 1, 8 and a build with no OpenMP at all return the same
            !! bits. It is a speed control and never an accuracy one.
        end subroutine stddev_f64
        !> `pf_sem` over a 64-bit real array: the standard error of the mean.
        !>
        !> `stddev / sqrt(n_eff)`, where `n_eff` is the population size unweighted and the count
        !> named by `weight_type` otherwise. Matches `scipy.stats.sem` at the default `ddof=1`.
        module subroutine sem_f64(values, se, is_valid, weights, weight_type, ddof, skipnan, n_null, n_nan, ok, threads)
            real(real64), intent(in) :: values(:) !! the population.
            real(real64), intent(out) :: se !! the standard error; NaN when the standard deviation is.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight, non-negative. A ZERO weight removes the element from the
            !! population; a negative, NaN or infinite one aborts. Absent means every weight is 1.
            character(len=*), intent(in), optional :: weight_type
            !! "reliability" (the default) or "frequency". The two differ only when the weights are
            !! unequal: a FREQUENCY weight of 3 says the value occurred three times, so the count
            !! `ddof` is charged against is `sum(weights)`; a RELIABILITY weight says the value is
            !! that much more precise, so the count is Kish's effective size
            !! `sum(weights)**2 / sum(weights**2)`. Any other token aborts, listing both.
            integer, intent(in), optional :: ddof
            !! delta degrees of freedom; **1 by default** -- the sample variance, as pandas
            !! returns. numpy's default is 0, so pass `ddof=0` to reproduce `np.var`/`np.std`.
            !! A plain default-kind integer deliberately: it is bounded by the population size,
            !! so there is no int64 form to provide. `ddof >= n_valid` gives NaN, not a division.
            logical, intent(in), optional :: skipnan
            !! .true. (the default) excludes a NaN from the population, as a null is excluded and
            !! as `pf_minmax` has always done; .false. restores numpy's propagating behaviour, in
            !! which one NaN makes every answer NaN.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            integer(int64), intent(out), optional :: n_nan
            !! how many were excluded as NaN and were not already null.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
            integer, intent(in), optional :: threads
            !! how many threads the central-moment pass may use. Absent takes the automatic rule:
            !! the `sort_threads` setting, capped by the processors actually available and
            !! by a measured work floor, and 1 inside a caller's own parallel region. **The answer
            !! does not depend on this argument** -- the block decomposition is a function of the
            !! population size alone, so 1, 8 and a build with no OpenMP at all return the same
            !! bits. It is a speed control and never an accuracy one.
        end subroutine sem_f64
        !> `pf_skewness` over a 64-bit real array: the third standardised moment.
        !>
        !> **`ddof` is deliberately absent**: no definition of skewness in pandas, numpy or scipy
        !> takes one, and an argument that can never do anything is worse than an absent one.
        !> `weight_type` is present because the bias correction counts through `n_eff`.
        !> NaN when the variance is zero, or when `n_valid < 3` and `bias` is .false.
        module subroutine skewness_f64(values, g, is_valid, weights, weight_type, bias, skipnan, n_null, n_nan, ok, threads)
            real(real64), intent(in) :: values(:) !! the population.
            real(real64), intent(out) :: g !! the skewness.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight, non-negative. A ZERO weight removes the element from the
            !! population; a negative, NaN or infinite one aborts. Absent means every weight is 1.
            character(len=*), intent(in), optional :: weight_type
            !! "reliability" (the default) or "frequency". The two differ only when the weights are
            !! unequal: a FREQUENCY weight of 3 says the value occurred three times, so the count
            !! `ddof` is charged against is `sum(weights)`; a RELIABILITY weight says the value is
            !! that much more precise, so the count is Kish's effective size
            !! `sum(weights)**2 / sum(weights**2)`. Any other token aborts, listing both.
            logical, intent(in), optional :: bias
            !! .false. by default: the bias-CORRECTED G1/G2, which is what pandas returns.
            !! .true. gives the uncorrected g1/g2, which is what scipy returns by default.
            logical, intent(in), optional :: skipnan
            !! .true. (the default) excludes a NaN from the population, as a null is excluded and
            !! as `pf_minmax` has always done; .false. restores numpy's propagating behaviour, in
            !! which one NaN makes every answer NaN.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            integer(int64), intent(out), optional :: n_nan
            !! how many were excluded as NaN and were not already null.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
            integer, intent(in), optional :: threads
            !! how many threads the central-moment pass may use. Absent takes the automatic rule:
            !! the `sort_threads` setting, capped by the processors actually available and
            !! by a measured work floor, and 1 inside a caller's own parallel region. **The answer
            !! does not depend on this argument** -- the block decomposition is a function of the
            !! population size alone, so 1, 8 and a build with no OpenMP at all return the same
            !! bits. It is a speed control and never an accuracy one.
        end subroutine skewness_f64
        !> `pf_kurtosis` over a 64-bit real array: the fourth standardised moment.
        !>
        !> `ddof` is absent for the same reason as on `pf_skewness`. NaN when the variance is
        !> zero, or when `n_valid < 4` and `bias` is .false.
        module subroutine kurtosis_f64(values, k, is_valid, weights, weight_type, bias, excess, skipnan, n_null, n_nan, ok, threads)
            real(real64), intent(in) :: values(:) !! the population.
            real(real64), intent(out) :: k !! the kurtosis, excess by default.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight, non-negative. A ZERO weight removes the element from the
            !! population; a negative, NaN or infinite one aborts. Absent means every weight is 1.
            character(len=*), intent(in), optional :: weight_type
            !! "reliability" (the default) or "frequency". The two differ only when the weights are
            !! unequal: a FREQUENCY weight of 3 says the value occurred three times, so the count
            !! `ddof` is charged against is `sum(weights)`; a RELIABILITY weight says the value is
            !! that much more precise, so the count is Kish's effective size
            !! `sum(weights)**2 / sum(weights**2)`. Any other token aborts, listing both.
            logical, intent(in), optional :: bias
            !! .false. by default: the bias-CORRECTED G1/G2, which is what pandas returns.
            !! .true. gives the uncorrected g1/g2, which is what scipy returns by default.
            logical, intent(in), optional :: excess
            !! .true. by default: EXCESS kurtosis, which is 0 for a normal population.
            !! .false. adds 3 back, giving the raw fourth-moment ratio.
            logical, intent(in), optional :: skipnan
            !! .true. (the default) excludes a NaN from the population, as a null is excluded and
            !! as `pf_minmax` has always done; .false. restores numpy's propagating behaviour, in
            !! which one NaN makes every answer NaN.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            integer(int64), intent(out), optional :: n_nan
            !! how many were excluded as NaN and were not already null.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
            integer, intent(in), optional :: threads
            !! how many threads the central-moment pass may use. Absent takes the automatic rule:
            !! the `sort_threads` setting, capped by the processors actually available and
            !! by a measured work floor, and 1 inside a caller's own parallel region. **The answer
            !! does not depend on this argument** -- the block decomposition is a function of the
            !! population size alone, so 1, 8 and a build with no OpenMP at all return the same
            !! bits. It is a speed control and never an accuracy one.
        end subroutine kurtosis_f64
        !> `pf_moments` over a 64-bit real array: every tier-A quantity in ONE pair of passes.
        !>
        !> This is the form to use in a per-group loop. Nothing is computed that was not asked
        !> for beyond the running moments themselves, and the population is traversed twice
        !> however many outputs are requested -- once for the mean, once for the central moments.
        !>
        !> **The outputs come before the common input block**, which is this family's one
        !> exception to the canonical optional order, and their own order is fixed here.
        module subroutine moments_f64(values, n_valid, mean, variance, stddev, sem, skewness, &
                kurtosis, vsum, vmin, vmax, is_valid, weights, weight_type, ddof, bias, excess, &
                skipnan, n_null, n_nan, ok, threads)
            real(real64), intent(in) :: values(:) !! the population.
            integer(int64), intent(out), optional :: n_valid !! how many elements were used.
            real(real64), intent(out), optional :: mean !! the mean; NaN when empty.
            real(real64), intent(out), optional :: variance !! the variance; NaN when undefined.
            real(real64), intent(out), optional :: stddev !! the standard deviation.
            real(real64), intent(out), optional :: sem !! the standard error of the mean.
            real(real64), intent(out), optional :: skewness !! the third standardised moment.
            real(real64), intent(out), optional :: kurtosis !! the fourth, excess by default.
            real(real64), intent(out), optional :: vsum !! the pairwise sum; 0 when empty.
            real(real64), intent(out), optional :: vmin !! the smallest value; NaN when empty.
            real(real64), intent(out), optional :: vmax !! the largest value; NaN when empty.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight, non-negative. A ZERO weight removes the element from the
            !! population; a negative, NaN or infinite one aborts. Absent means every weight is 1.
            character(len=*), intent(in), optional :: weight_type
            !! "reliability" (the default) or "frequency". The two differ only when the weights are
            !! unequal: a FREQUENCY weight of 3 says the value occurred three times, so the count
            !! `ddof` is charged against is `sum(weights)`; a RELIABILITY weight says the value is
            !! that much more precise, so the count is Kish's effective size
            !! `sum(weights)**2 / sum(weights**2)`. Any other token aborts, listing both.
            integer, intent(in), optional :: ddof
            !! delta degrees of freedom; **1 by default** -- the sample variance, as pandas
            !! returns. numpy's default is 0, so pass `ddof=0` to reproduce `np.var`/`np.std`.
            !! A plain default-kind integer deliberately: it is bounded by the population size,
            !! so there is no int64 form to provide. `ddof >= n_valid` gives NaN, not a division.
            logical, intent(in), optional :: bias
            !! .false. by default: the bias-CORRECTED G1/G2, which is what pandas returns.
            !! .true. gives the uncorrected g1/g2, which is what scipy returns by default.
            logical, intent(in), optional :: excess
            !! .true. by default: EXCESS kurtosis, which is 0 for a normal population.
            !! .false. adds 3 back, giving the raw fourth-moment ratio.
            logical, intent(in), optional :: skipnan
            !! .true. (the default) excludes a NaN from the population, as a null is excluded and
            !! as `pf_minmax` has always done; .false. restores numpy's propagating behaviour, in
            !! which one NaN makes every answer NaN.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            integer(int64), intent(out), optional :: n_nan
            !! how many were excluded as NaN and were not already null.
            logical, intent(out), optional :: ok
            !! .false. when any output that was ASKED for came back a quiet NaN -- an empty
            !! population, a `ddof` that leaves nothing to divide by, a constant population for
            !! the shape statistics, or a non-finite value under `skipnan = .false.`. Only the
            !! PRESENT outputs are tested, which is the only reading that works here: `vsum` over
            !! an empty population is a correct `0`, and testing an output the caller never asked
            !! for would report a failure that is not one. `n_valid`, `n_null` and `n_nan` are
            !! counts and are always defined, so they are not tested.
            !!
            !! An infinity is NOT a failure: `mean` answers `+Inf` over a population containing
            !! one, exactly as `pf_mean` and numpy do, and `ok` stays .true. for it while the
            !! variance -- genuinely a NaN there -- turns it .false. if it was asked for.
            integer, intent(in), optional :: threads
            !! how many threads the central-moment pass may use. Absent takes the automatic rule:
            !! the `sort_threads` setting, capped by the processors actually available and
            !! by a measured work floor, and 1 inside a caller's own parallel region. **The answer
            !! does not depend on this argument** -- the block decomposition is a function of the
            !! population size alone, so 1, 8 and a build with no OpenMP at all return the same
            !! bits. It is a speed control and never an accuracy one.
        end subroutine moments_f64
    end interface
    !
    ! ---- The real64 order core (implemented in parquet_stats_order) ----
    interface
        !> The median of a population: the INTERPOLATING one, not an element of the input.
        !>
        !> For an even-length population this is the mean of the two middle values, which is what
        !> numpy, pandas and every spreadsheet mean by `median` -- and is deliberately NOT what
        !> `pf_nth_quantile(v, 0.5)` in `parquet_sorting` returns, which is one of the two. That
        !> one answers `which element`, this one answers `what value`; both are useful and they
        !> are different questions.
        !>
        !> `method=` selects the rule (see `pf_quantile`); `pf_median` is exactly
        !> `pf_quantile(values, 0.5, med)`, and `pf_quantile`'s doc-comment sets out the three
        !> ways `pf_nth_quantile` differs from both.
        module subroutine median_f64(values, med, is_valid, weights, weight_type, skipnan, method, n_null, n_nan, &
                ok, threads)
            real(real64), intent(in) :: values(:) !! the population.
            real(real64), intent(out) :: med
            !! the median; NaN when the population is empty.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight, non-negative. A ZERO weight removes the element from the
            !! population; a negative, NaN or infinite one aborts. Absent means every weight is 1.
            character(len=*), intent(in), optional :: weight_type
            !! "reliability" (the default) or "frequency". The two differ only when the weights are
            !! unequal: a FREQUENCY weight of 3 says the value occurred three times, so the count
            !! `ddof` is charged against is `sum(weights)`; a RELIABILITY weight says the value is
            !! that much more precise, so the count is Kish's effective size
            !! `sum(weights)**2 / sum(weights**2)`. Any other token aborts, listing both.
            logical, intent(in), optional :: skipnan
            !! .true. (the default) excludes a NaN from the population, as a null is excluded and
            !! as `pf_minmax` has always done; .false. restores numpy's propagating behaviour, in
            !! which one NaN makes every answer NaN.
            character(len=*), intent(in), optional :: method
            !! how a fractional position between two order statistics is resolved, in numpy's
            !! spelling: "linear" (the default -- Hyndman-Fan type 7, what numpy and pandas do),
            !! "lower", "higher", "nearest", "midpoint", or "inverted_cdf" (a step function on the
            !! plain cumulative scale, and the only token numpy itself accepts weights with). Any
            !! other token aborts, listing all six. Matched case-insensitively.
            !! **`weight_type="frequency"` changes the default to "inverted_cdf"**, because that
            !! token IS frequency expansion; an explicit `method=` still wins.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            integer(int64), intent(out), optional :: n_nan
            !! how many were excluded as NaN and were not already null.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
            integer, intent(in), optional :: threads
            !! how many threads the ORDERING may use -- this is an order statistic and has no
            !! central-moment pass, so the work `threads=` divides is `pf_argsort`'s. Absent takes
            !! the automatic rule: the `sort_threads` setting, capped by the processors
            !! actually available and by a measured work floor, and 1 inside a caller's own
            !! parallel region. **The answer does not depend on this argument** -- a sort is a
            !! permutation and the selection off it is exact, so 1, 8 and a build with no OpenMP
            !! at all return the same bits. It is a speed control and never an accuracy one.
        end subroutine median_f64
        !> One quantile of a population, interpolated between the bracketing order statistics.
        !>
        !> `p` is on a **0-1 scale**, not 0-100, and outside `[0, 1]` aborts. `method=` chooses how
        !> a fractional position is resolved: `linear` (the default, Hyndman-Fan type 7 -- numpy's
        !> and pandas' default), `lower`, `higher`, `nearest`, `midpoint`, or `inverted_cdf`.
        !>
        !> **Weighted, the position rule reduces EXACTLY to type 7 when the weights are equal**, so
        !> adding uniform weights to a working script does not move its numbers. No reference
        !> library implements an interpolating weighted quantile, so that invariant -- rather than
        !> a cross-check -- is what pins it; `method="inverted_cdf"` is the numpy-comparable
        !> escape hatch, and is what `weight_type="frequency"` selects by default.
        !>
        !> One call with one probability uses SELECTION rather than a sort where it can, so it is
        !> O(n). Asking for several probabilities should use `pf_quantiles`, which sorts once.
        !>
        !> **`pf_nth_quantile` in `parquet_sorting` is the other one**, and the two differ in three
        !> ways rather than one: it returns an ELEMENT of the input (with `index=` saying which)
        !> instead of an interpolated value, it accepts every element type this library sorts
        !> including `character` and the temporal kinds, and it takes no weights. Its `ok` also
        !> means the opposite of this module's -- omitting `ok` there makes a degenerate
        !> population ABORT, where omitting it here is the ordinary way to call. Reach for it when
        !> the question is *which row*, and for this one when the question is *what value*.
        module subroutine quantile_f64(values, p, q, is_valid, weights, weight_type, skipnan, method, n_null, n_nan, &
                ok, threads)
            real(real64), intent(in) :: values(:) !! the population.
            real(real64), intent(in) :: p
            !! the probability, on a 0-1 scale rather than 0-100. Outside [0, 1] aborts.
            real(real64), intent(out) :: q
            !! the quantile; NaN when the population is empty.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight, non-negative. A ZERO weight removes the element from the
            !! population; a negative, NaN or infinite one aborts. Absent means every weight is 1.
            character(len=*), intent(in), optional :: weight_type
            !! "reliability" (the default) or "frequency". The two differ only when the weights are
            !! unequal: a FREQUENCY weight of 3 says the value occurred three times, so the count
            !! `ddof` is charged against is `sum(weights)`; a RELIABILITY weight says the value is
            !! that much more precise, so the count is Kish's effective size
            !! `sum(weights)**2 / sum(weights**2)`. Any other token aborts, listing both.
            logical, intent(in), optional :: skipnan
            !! .true. (the default) excludes a NaN from the population, as a null is excluded and
            !! as `pf_minmax` has always done; .false. restores numpy's propagating behaviour, in
            !! which one NaN makes every answer NaN.
            character(len=*), intent(in), optional :: method
            !! how a fractional position between two order statistics is resolved, in numpy's
            !! spelling: "linear" (the default -- Hyndman-Fan type 7, what numpy and pandas do),
            !! "lower", "higher", "nearest", "midpoint", or "inverted_cdf" (a step function on the
            !! plain cumulative scale, and the only token numpy itself accepts weights with). Any
            !! other token aborts, listing all six. Matched case-insensitively.
            !! **`weight_type="frequency"` changes the default to "inverted_cdf"**, because that
            !! token IS frequency expansion; an explicit `method=` still wins.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            integer(int64), intent(out), optional :: n_nan
            !! how many were excluded as NaN and were not already null.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
            integer, intent(in), optional :: threads
            !! how many threads the ORDERING may use -- this is an order statistic and has no
            !! central-moment pass, so the work `threads=` divides is `pf_argsort`'s. Absent takes
            !! the automatic rule: the `sort_threads` setting, capped by the processors
            !! actually available and by a measured work floor, and 1 inside a caller's own
            !! parallel region. **The answer does not depend on this argument** -- a sort is a
            !! permutation and the selection off it is exact, so 1, 8 and a build with no OpenMP
            !! at all return the same bits. It is a speed control and never an accuracy one.
        end subroutine quantile_f64
        !> Several quantiles of one population, from ONE ordering of it.
        !>
        !> `probs(:)` on a 0-1 scale and `out(:)` the same size; `probs` need not be sorted and may
        !> repeat. This is the form to reach for whenever more than one quantile is wanted --
        !> `[0.16, 0.5, 0.84]` is one call and one sort, where three `pf_quantile` calls would
        !> traverse the population three times.
        !>
        !> Every argument means what it does on `pf_quantile`, including `method=` and the weighted
        !> position rule.
        module subroutine quantiles_f64(values, probs, out, is_valid, weights, weight_type, skipnan, method, n_null, &
                n_nan, ok, threads)
            real(real64), intent(in) :: values(:) !! the population.
            real(real64), intent(in) :: probs(:)
            !! the probabilities, each on a 0-1 scale. Any outside [0, 1] aborts. Need not be sorted.
            real(real64), intent(out) :: out(:)
            !! one quantile per entry of `probs`, same size. ONE sort serves all of them.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight, non-negative. A ZERO weight removes the element from the
            !! population; a negative, NaN or infinite one aborts. Absent means every weight is 1.
            character(len=*), intent(in), optional :: weight_type
            !! "reliability" (the default) or "frequency". The two differ only when the weights are
            !! unequal: a FREQUENCY weight of 3 says the value occurred three times, so the count
            !! `ddof` is charged against is `sum(weights)`; a RELIABILITY weight says the value is
            !! that much more precise, so the count is Kish's effective size
            !! `sum(weights)**2 / sum(weights**2)`. Any other token aborts, listing both.
            logical, intent(in), optional :: skipnan
            !! .true. (the default) excludes a NaN from the population, as a null is excluded and
            !! as `pf_minmax` has always done; .false. restores numpy's propagating behaviour, in
            !! which one NaN makes every answer NaN.
            character(len=*), intent(in), optional :: method
            !! how a fractional position between two order statistics is resolved, in numpy's
            !! spelling: "linear" (the default -- Hyndman-Fan type 7, what numpy and pandas do),
            !! "lower", "higher", "nearest", "midpoint", or "inverted_cdf" (a step function on the
            !! plain cumulative scale, and the only token numpy itself accepts weights with). Any
            !! other token aborts, listing all six. Matched case-insensitively.
            !! **`weight_type="frequency"` changes the default to "inverted_cdf"**, because that
            !! token IS frequency expansion; an explicit `method=` still wins.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            integer(int64), intent(out), optional :: n_nan
            !! how many were excluded as NaN and were not already null.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
            integer, intent(in), optional :: threads
            !! how many threads the ORDERING may use -- this is an order statistic and has no
            !! central-moment pass, so the work `threads=` divides is `pf_argsort`'s. Absent takes
            !! the automatic rule: the `sort_threads` setting, capped by the processors
            !! actually available and by a measured work floor, and 1 inside a caller's own
            !! parallel region. **The answer does not depend on this argument** -- a sort is a
            !! permutation and the selection off it is exact, so 1, 8 and a build with no OpenMP
            !! at all return the same bits. It is a speed control and never an accuracy one.
        end subroutine quantiles_f64
        !> The interquartile range: `pf_quantile(v, 0.75) - pf_quantile(v, 0.25)`, from one sort.
        !>
        !> NaN when either quartile is undefined, which for this family means an empty population.
        !> `method=` is passed through to both quartiles, so the difference is always taken between
        !> two quantiles computed the same way.
        module subroutine iqr_f64(values, r, is_valid, weights, weight_type, skipnan, method, n_null, n_nan, &
                ok, threads)
            real(real64), intent(in) :: values(:) !! the population.
            real(real64), intent(out) :: r
            !! the interquartile range, `q(0.75) - q(0.25)`; NaN when either quartile is.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight, non-negative. A ZERO weight removes the element from the
            !! population; a negative, NaN or infinite one aborts. Absent means every weight is 1.
            character(len=*), intent(in), optional :: weight_type
            !! "reliability" (the default) or "frequency". The two differ only when the weights are
            !! unequal: a FREQUENCY weight of 3 says the value occurred three times, so the count
            !! `ddof` is charged against is `sum(weights)`; a RELIABILITY weight says the value is
            !! that much more precise, so the count is Kish's effective size
            !! `sum(weights)**2 / sum(weights**2)`. Any other token aborts, listing both.
            logical, intent(in), optional :: skipnan
            !! .true. (the default) excludes a NaN from the population, as a null is excluded and
            !! as `pf_minmax` has always done; .false. restores numpy's propagating behaviour, in
            !! which one NaN makes every answer NaN.
            character(len=*), intent(in), optional :: method
            !! how a fractional position between two order statistics is resolved, in numpy's
            !! spelling: "linear" (the default -- Hyndman-Fan type 7, what numpy and pandas do),
            !! "lower", "higher", "nearest", "midpoint", or "inverted_cdf" (a step function on the
            !! plain cumulative scale, and the only token numpy itself accepts weights with). Any
            !! other token aborts, listing all six. Matched case-insensitively.
            !! **`weight_type="frequency"` changes the default to "inverted_cdf"**, because that
            !! token IS frequency expansion; an explicit `method=` still wins.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            integer(int64), intent(out), optional :: n_nan
            !! how many were excluded as NaN and were not already null.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
            integer, intent(in), optional :: threads
            !! how many threads the ORDERING may use -- this is an order statistic and has no
            !! central-moment pass, so the work `threads=` divides is `pf_argsort`'s. Absent takes
            !! the automatic rule: the `sort_threads` setting, capped by the processors
            !! actually available and by a measured work floor, and 1 inside a caller's own
            !! parallel region. **The answer does not depend on this argument** -- a sort is a
            !! permutation and the selection off it is exact, so 1, 8 and a build with no OpenMP
            !! at all return the same bits. It is a speed control and never an accuracy one.
        end subroutine iqr_f64
        !> The mean of the population with a share trimmed from EACH tail -- scipy's `trim_mean`.
        !>
        !> `prop` is the share removed at each end, so `prop=0.1` averages the middle 80%. It must
        !> satisfy `0 <= prop < 0.5`; anything else aborts, since trimming everything is a mistake
        !> rather than a request. `prop=0` is the ordinary mean and is allowed.
        !>
        !> The count trimmed from each end is `floor(prop * n_valid)`, matching scipy, so the
        !> result changes in steps as `prop` grows rather than continuously.
        !>
        !> **Weights are supported and are trimmed with their values**: the same elements leave the
        !> population, and the mean of what remains is weighted.
        module subroutine trim_mean_f64(values, prop, m, is_valid, weights, skipnan, n_null, n_nan, ok, threads)
            real(real64), intent(in) :: values(:) !! the population.
            real(real64), intent(in) :: prop
            !! the share trimmed from EACH tail, so `prop=0.1` drops 10% at each end and averages the
            !! middle 80%. Must satisfy `0 <= prop < 0.5`; anything else aborts, because a caller who asks
            !! to trim everything has made a mistake rather than expressed a preference.
            real(real64), intent(out) :: m
            !! the trimmed mean; NaN when nothing survives the trim.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight, non-negative. A ZERO weight removes the element from the
            !! population; a negative, NaN or infinite one aborts. Absent means every weight is 1.
            logical, intent(in), optional :: skipnan
            !! .true. (the default) excludes a NaN from the population, as a null is excluded and
            !! as `pf_minmax` has always done; .false. restores numpy's propagating behaviour, in
            !! which one NaN makes every answer NaN.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            integer(int64), intent(out), optional :: n_nan
            !! how many were excluded as NaN and were not already null.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
            integer, intent(in), optional :: threads
            !! how many threads the ORDERING may use -- this is an order statistic and has no
            !! central-moment pass, so the work `threads=` divides is `pf_argsort`'s. Absent takes
            !! the automatic rule: the `sort_threads` setting, capped by the processors
            !! actually available and by a measured work floor, and 1 inside a caller's own
            !! parallel region. **The answer does not depend on this argument** -- a sort is a
            !! permutation and the selection off it is exact, so 1, 8 and a build with no OpenMP
            !! at all return the same bits. It is a speed control and never an accuracy one.
        end subroutine trim_mean_f64
        !> The share of the population at or below a given value -- scipy's `percentileofscore`,
        !> on a **0-1 scale** rather than 0-100, matching every other probability in this module.
        !>
        !> The inverse of `pf_quantile` in the loose sense: it answers `where does this value sit`
        !> rather than `what value sits here`. `kind=` chooses how a value exactly EQUAL to `score`
        !> is counted -- `rank` (the default; ties count half), `weak` (`<=`), `strict` (`<`), or
        !> `mean` (the average of weak and strict).
        !>
        !> A NaN or infinite `score` aborts: unlike a NaN in the population, which is an ordinary
        !> data condition this module excludes, a NaN score can only be the caller's own mistake.
        module subroutine percentile_of_score_f64(values, score, p, is_valid, weights, skipnan, kind, n_null, n_nan, ok, threads)
            real(real64), intent(in) :: values(:) !! the population.
            real(real64), intent(in) :: score
            !! the value to locate. NaN or infinite aborts -- unlike a NaN in the population, which is an
            !! ordinary data condition, a NaN score can only come from the caller's own arithmetic.
            real(real64), intent(out) :: p
            !! the share of the population at or below `score`, on a 0-1 scale; NaN when empty.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight, non-negative. A ZERO weight removes the element from the
            !! population; a negative, NaN or infinite one aborts. Absent means every weight is 1.
            logical, intent(in), optional :: skipnan
            !! .true. (the default) excludes a NaN from the population, as a null is excluded and
            !! as `pf_minmax` has always done; .false. restores numpy's propagating behaviour, in
            !! which one NaN makes every answer NaN.
            character(len=*), intent(in), optional :: kind
            !! which convention counts a value EQUAL to `score`, in scipy's spelling: "rank" (the
            !! default; ties count half), "weak" (a value <= score counts), "strict" (only < score)
            !! or "mean" (the average of weak and strict). Any other token aborts, listing all
            !! four. Matched case-insensitively.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            integer(int64), intent(out), optional :: n_nan
            !! how many were excluded as NaN and were not already null.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
            integer, intent(in), optional :: threads
            !! how many threads the ORDERING may use -- this is an order statistic and has no
            !! central-moment pass, so the work `threads=` divides is `pf_argsort`'s. Absent takes
            !! the automatic rule: the `sort_threads` setting, capped by the processors
            !! actually available and by a measured work floor, and 1 inside a caller's own
            !! parallel region. **The answer does not depend on this argument** -- a sort is a
            !! permutation and the selection off it is exact, so 1, 8 and a build with no OpenMP
            !! at all return the same bits. It is a speed control and never an accuracy one.
        end subroutine percentile_of_score_f64
        !> The median absolute deviation -- scipy's `median_abs_deviation`, scaled by default.
        !>
        !> `median(|x - center|)`, where `center` is the population's own median unless one is
        !> supplied. `scale="normal"` (the default) multiplies by `1/Phi^-1(3/4)` =
        !> 1.4826022185056018, which makes the result
        !> a consistent estimator of the standard deviation for Gaussian data -- so on a large
        !> Gaussian sample `pf_mad` and `pf_stddev` agree to within sampling error, and on a sample
        !> with a few wild points they do not, which is the whole reason to reach for it.
        !> `scale="raw"` is the unscaled median.
        !>
        !> **Supplying `center` skips one selection, not the ordering.** The median of the
        !> deviations still has to be found, so this is a small saving rather than a different
        !> algorithm. A NaN or infinite `center` aborts.
        !>
        !> Both medians are taken with `linear` interpolation, because scipy reaches `np.median`;
        !> there is deliberately no `method=` here, since a token that made this disagree with
        !> scipy would be worse than no token at all.
        module subroutine mad_f64(values, m, is_valid, weights, skipnan, scale, center, n_null, n_nan, ok, &
                threads)
            real(real64), intent(in) :: values(:) !! the population.
            real(real64), intent(out) :: m
            !! the median absolute deviation, scaled unless `scale="raw"`; NaN when the population is
            !! empty.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight, non-negative. A ZERO weight removes the element from the
            !! population; a negative, NaN or infinite one aborts. Absent means every weight is 1.
            logical, intent(in), optional :: skipnan
            !! .true. (the default) excludes a NaN from the population, as a null is excluded and
            !! as `pf_minmax` has always done; .false. restores numpy's propagating behaviour, in
            !! which one NaN makes every answer NaN.
            character(len=*), intent(in), optional :: scale
            !! "normal" (the default) divides the raw deviation by `Phi^-1(3/4)`, which makes the
            !! result a consistent estimator of the standard deviation for Gaussian data --
            !! scipy's `median_abs_deviation(scale='normal')`, and what an astronomy script means
            !! by "the MAD". That is a multiplication by 1.4826022185056018, and **not** by the
            !! rounded 1.4826 the textbooks quote, which differs by 1.5e-06 relative. "raw" is the
            !! unscaled median of |x - center|. Any other token aborts, naming both. Matched
            !! case-insensitively.
            real(real64), intent(in), optional :: center
            !! the centre the deviations are taken about. Absent uses the population's own median,
            !! which is what scipy does. Supplying one skips ONE selection and not the ordering:
            !! the median of |x - center| still has to be found. A NaN or infinite centre ABORTS
            !! -- unlike a NaN value, which is an ordinary data condition, a NaN centre can only
            !! come from the caller's own arithmetic.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            integer(int64), intent(out), optional :: n_nan
            !! how many were excluded as NaN and were not already null.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
            integer, intent(in), optional :: threads
            !! how many threads the ORDERING may use -- this is an order statistic and has no
            !! central-moment pass, so the work `threads=` divides is `pf_argsort`'s. Absent takes
            !! the automatic rule: the `sort_threads` setting, capped by the processors
            !! actually available and by a measured work floor, and 1 inside a caller's own
            !! parallel region. **The answer does not depend on this argument** -- a sort is a
            !! permutation and the selection off it is exact, so 1, 8 and a build with no OpenMP
            !! at all return the same bits. It is a speed control and never an accuracy one.
        end subroutine mad_f64
        !> Summarises a population into a `pf_stats`, ordered and ready -- pandas' `describe()`.
        !>
        !> Exactly `s%compute(values, ...)` followed by `s%prepare_order()`, which is the pairing
        !> worth a name: it is ONE pair of traversals and ONE ordering, after which the count, the
        !> mean, the standard deviation, the extremes and every quantile are reads. `s%print()`
        !> renders the usual eight-row block from it.
        !>
        !> Reach for `%compute` instead when the order statistics are not wanted -- this procedure
        !> pays for the ordering whether or not anything asks for it, which is the point.
        module subroutine describe_f64(values, s, is_valid, weights, weight_type, skipnan, ok, threads)
            real(real64), intent(in) :: values(:) !! the population.
            type(pf_stats), intent(out) :: s
            !! the filled summary object: tier A computed and tier B already ordered, so every query on it
            !! afterwards is a read.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight, non-negative. A ZERO weight removes the element from the
            !! population; a negative, NaN or infinite one aborts. Absent means every weight is 1.
            character(len=*), intent(in), optional :: weight_type
            !! "reliability" (the default) or "frequency". The two differ only when the weights are
            !! unequal: a FREQUENCY weight of 3 says the value occurred three times, so the count
            !! `ddof` is charged against is `sum(weights)`; a RELIABILITY weight says the value is
            !! that much more precise, so the count is Kish's effective size
            !! `sum(weights)**2 / sum(weights**2)`. Any other token aborts, listing both.
            logical, intent(in), optional :: skipnan
            !! .true. (the default) excludes a NaN from the population, as a null is excluded and
            !! as `pf_minmax` has always done; .false. restores numpy's propagating behaviour, in
            !! which one NaN makes every answer NaN.
            logical, intent(out), optional :: ok
            !! .false. when the object describes a population with no defined moments -- one that
            !! is empty after the exclusions, or that kept a NaN under `skipnan = .false.` In
            !! either case every tier-A query on `s` answers a quiet NaN. The object itself
            !! carries the counts (`%n_valid()`, `%n_null()`, `%n_nan()`), so this is the one
            !! thing about the result a caller cannot otherwise read without picking a statistic
            !! and NaN-testing it by hand.
            integer, intent(in), optional :: threads
            !! how many threads the central-moment pass and the ordering may use -- `pf_describe`
            !! does both, which no other entry point in this module does. Absent takes the
            !! automatic rule: the `sort_threads` setting, capped by the processors
            !! actually available and by a measured work floor, and 1 inside a caller's own
            !! parallel region. **The answer does not depend on this argument** -- the block
            !! decomposition is a function of the population size alone and a sort is a
            !! permutation, so 1, 8 and a build with no OpenMP at all return the same bits. It is
            !! a speed control and never an accuracy one.
        end subroutine describe_f64
        !> Applies the exclusion rules and hands back the surviving values, without the moments.
        !!
        !! Pass one of `stats_engine` and nothing else: an order statistic needs the survivors and
        !! their weights, and computing four central moments it will discard would roughly double
        !! the cost of every `pf_median`.
        module subroutine stats_compact(values, what, is_valid, weights, skipnan, keep_x, keep_w, &
                n_valid, n_null, n_nan, saw_nan)
            real(real64), intent(in) :: values(:) !! the population, before exclusions.
            character(len=*), intent(in) :: what !! the public procedure's name, for messages.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
            real(real64), intent(in), optional :: weights(:) !! per element weight.
            logical, intent(in), optional :: skipnan !! .true. (default) excludes a NaN.
            real(real64), allocatable, intent(out) :: keep_x(:)
            !! the survivors, in input order. `n_valid` of them are live and the rest of the array
            !! is spare capacity, exactly as `stats_engine`'s own hand-off leaves it.
            real(real64), allocatable, intent(out) :: keep_w(:)
            !! their weights, allocated only when `weights` was supplied.
            integer(int64), intent(out) :: n_valid !! how many survived.
            integer(int64), intent(out) :: n_null !! how many `is_valid` excluded.
            integer(int64), intent(out) :: n_nan !! how many were excluded as NaN.
            logical, intent(out) :: saw_nan
            !! .true. when `skipnan = .false.` and a NaN is still in `keep_x`. **Every order
            !! statistic must answer NaN when this is set**, exactly as the moments do: a NaN
            !! sorts to one end rather than poisoning the interpolation, so an order tier that
            !! ignored this flag would return a plausible number from a population the caller
            !! asked to have poisoned.
        end subroutine stats_compact
        !> A quiet NaN, the value every undefined statistic in this module returns.
        !!
        !! Built with `ieee_value` rather than by arithmetic: `0.0/0.0` and anything routed through
        !! `anint`/`int` raise under nagfor's default `-ieee=stop`, which terminates the process.
        pure module function stats_nan() result(res)
            real(real64) :: res !! a quiet NaN.
        end function stats_nan
        !> Resolves `weight_type` to the one bit the formulas actually branch on.
        module subroutine stats_weight_kind(what, weight_type, freq)
            character(len=*), intent(in) :: what !! the public procedure's name, for the message.
            character(len=*), intent(in), optional :: weight_type !! the caller's token, if any.
            logical, intent(out) :: freq !! .true. for frequency weights.
        end subroutine stats_weight_kind
        !> The two-sample two-pass, over pairs the caller has already compacted.
        !!
        !! **It exists so that `pf_cov(x, x)` is EXACTLY `pf_variance(x)`**, which it could not be
        !! if the covariance had its own accumulation loop: it is written here, in the same
        !! submodule as `stats_engine`, so that both reach the same `STATS_BLOCK` block tree, the
        !! same `pair_reduce`, the same refined mean and the same re-centring correction. A second
        !! loop elsewhere would agree to fifteen digits and differ in the sixteenth, and the
        !! identity is worth more than the duplication costs.
        module subroutine stats_pair_moments(kx, ky, kw, m, mean_x, mean_y, sxx, sxy, syy, &
                w_sum, w_sq)
            real(real64), intent(in) :: kx(:) !! the surviving first-sample values.
            real(real64), intent(in) :: ky(:) !! the surviving second-sample values, paired.
            real(real64), allocatable, intent(in) :: kw(:) !! their weights, or unallocated.
            integer(int64), intent(in) :: m !! how many pairs survived.
            real(real64), intent(out) :: mean_x !! the first sample's weighted mean.
            real(real64), intent(out) :: mean_y !! the second's.
            real(real64), intent(out) :: sxx !! `sum(w*(x-mx)**2)`, re-centred.
            real(real64), intent(out) :: sxy !! `sum(w*(x-mx)*(y-my))`, re-centred.
            real(real64), intent(out) :: syy !! `sum(w*(y-my)**2)`, re-centred.
            real(real64), intent(out) :: w_sum !! `sum(w)`.
            real(real64), intent(out) :: w_sq !! `sum(w**2)`, which the reliability `ddof` needs.
        end subroutine stats_pair_moments
        !> The mean and standard deviation of one population, from ONE engine run.
        !!
        !! `pf_zscore` needs both and would otherwise pay four traversals for two. Also reports
        !! `saw_nan`, so a vector-valued caller can answer all-NaN under `skipnan = .false.`
        !! exactly as the scalar family does.
        module subroutine stats_mean_sd(values, what, is_valid, ddof, skipnan, mean, sd, &
                n_valid, n_null, n_nan, saw_nan)
            real(real64), intent(in) :: values(:) !! the population, before exclusions.
            character(len=*), intent(in) :: what !! the public procedure's name, for messages.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
            integer, intent(in), optional :: ddof !! delta degrees of freedom; 1 by default.
            logical, intent(in), optional :: skipnan !! .true. (default) excludes a NaN.
            real(real64), intent(out) :: mean !! the mean, or NaN.
            real(real64), intent(out) :: sd !! the standard deviation, or NaN.
            integer(int64), intent(out) :: n_valid !! how many survived.
            integer(int64), intent(out) :: n_null !! how many `is_valid` excluded.
            integer(int64), intent(out) :: n_nan !! how many were excluded as NaN.
            logical, intent(out) :: saw_nan !! .true. when a NaN survived under skipnan=.false.
        end subroutine stats_mean_sd
        !> Completes a deferred tier-A recomputation, if one is pending.
        module subroutine stats_ensure(self)
            class(pf_stats), intent(inout) :: self !! the accumulator.
        end subroutine stats_ensure
    end interface
    !
    ! ---- The real64 cores of the relational and robust families ----
    interface
        !> `pf_cov` over two 64-bit real arrays: the PAIRWISE-COMPLETE covariance.
        !!
        !! A pair enters the population only when both elements are usable -- neither null, neither
        !! NaN, and the pair's weight non-zero. That is the only defensible rule for a two-sample
        !! statistic and it is what pandas does; handling nulls independently per array would
        !! produce a covariance between vectors of different lengths, which is not a number.
        module subroutine cov_f64(x, y, c, is_valid, weights, weight_type, ddof, n_null, &
                n_nan, ok)
            real(real64), intent(in) :: x(:) !! the first sample.
            real(real64), intent(in) :: y(:) !! the second sample, element for element.
            real(real64), intent(out) :: c !! the covariance; NaN when `n_valid <= ddof`.
            logical, intent(in), optional :: is_valid(:)
            !! per PAIR: .false. marks the pair as unusable. One mask over both arrays, because a
            !! two-sample statistic is taken over pairs; a caller holding a separate mask for each
            !! column passes `mask_x .and. mask_y`, which says what it does. A NaN on either side
            !! drops the pair as well, and is counted through `n_nan` rather than here.
            real(real64), intent(in), optional :: weights(:)
            !! per PAIR weight, non-negative. A ZERO weight removes the pair from the population;
            !! a negative, NaN or infinite one aborts. Absent means every weight is 1. Pearson
            !! only -- see `method`.
            character(len=*), intent(in), optional :: weight_type
            !! "reliability" (the default) or "frequency". The two differ only when the weights are
            !! unequal: a FREQUENCY weight of 3 says the value occurred three times, so the count
            !! `ddof` is charged against is `sum(weights)`; a RELIABILITY weight says the value is
            !! that much more precise, so the count is Kish's effective size
            !! `sum(weights)**2 / sum(weights**2)`. Any other token aborts, listing both.
            integer, intent(in), optional :: ddof
            !! delta degrees of freedom; **1 by default** -- the sample variance, as pandas
            !! returns. numpy's default is 0, so pass `ddof=0` to reproduce `np.var`/`np.std`.
            !! A plain default-kind integer deliberately: it is bounded by the population size,
            !! so there is no int64 form to provide. `ddof >= n_valid` gives NaN, not a division.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            integer(int64), intent(out), optional :: n_nan
            !! how many were excluded as NaN and were not already null.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
        end subroutine cov_f64
        !> `pf_corr` over two 64-bit real arrays.
        !!
        !! `cov(x, y) / (sd(x) * sd(y))` for Pearson, and the same over MIDRANKS for Spearman. The
        !! `ddof` in the covariance and the two in the standard deviations cancel exactly, which is
        !! why this procedure has no `ddof` argument to offer.
        module subroutine corr_f64(x, y, r, is_valid, weights, method, n_null, n_nan, ok)
            real(real64), intent(in) :: x(:) !! the first sample.
            real(real64), intent(in) :: y(:) !! the second sample, element for element.
            real(real64), intent(out) :: r !! the correlation in [-1, 1]; NaN when either sample is constant.
            logical, intent(in), optional :: is_valid(:)
            !! per PAIR: .false. marks the pair as unusable. One mask over both arrays, because a
            !! two-sample statistic is taken over pairs; a caller holding a separate mask for each
            !! column passes `mask_x .and. mask_y`, which says what it does. A NaN on either side
            !! drops the pair as well, and is counted through `n_nan` rather than here.
            real(real64), intent(in), optional :: weights(:)
            !! per PAIR weight, non-negative. A ZERO weight removes the pair from the population;
            !! a negative, NaN or infinite one aborts. Absent means every weight is 1. Pearson
            !! only -- see `method`.
            character(len=*), intent(in), optional :: method
            !! "pearson" (the default) or "spearman". Spearman is Pearson over MIDRANKS: each run
            !! of equal values receives the mean of the sorted positions it spans, so it measures
            !! any monotone relationship rather than a linear one. Any other token aborts, naming
            !! both. Matched case-insensitively. **`weights` with "spearman" aborts** -- a
            !! weighted midrank is a further definitional choice and no reference library makes
            !! it.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            integer(int64), intent(out), optional :: n_nan
            !! how many were excluded as NaN and were not already null.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
        end subroutine corr_f64
        !> `pf_zscore` over a 64-bit real array: `(x - mean) / stddev`, element by element.
        module subroutine zscore_f64(values, z, is_valid, ddof, skipnan, out_valid, n_null, ok)
            real(real64), intent(in) :: values(:) !! the population.
            real(real64), intent(out) :: z(:)
            !! the standardised values, same size as `values`.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            integer, intent(in), optional :: ddof
            !! delta degrees of freedom; **1 by default** -- the sample variance, as pandas
            !! returns. numpy's default is 0, so pass `ddof=0` to reproduce `np.var`/`np.std`.
            !! A plain default-kind integer deliberately: it is bounded by the population size,
            !! so there is no int64 form to provide. `ddof >= n_valid` gives NaN, not a division.
            logical, intent(in), optional :: skipnan
            !! .true. (the default) excludes a NaN from the population, as a null is excluded and
            !! as `pf_minmax` has always done; .false. restores numpy's propagating behaviour, in
            !! which one NaN makes every answer NaN.
            logical, intent(out), optional :: out_valid(:)
            !! per output element: .false. where the input was excluded and the result is
            !! therefore undefined. Same size as `values`. **Absent, such an element is written as
            !! a quiet NaN instead** -- which is loud in practice, since it propagates through
            !! whatever the caller does next.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
        end subroutine zscore_f64
        !> `pf_normal_scores` over a 64-bit real array: each element's rank, mapped through
        !! `Phi**(-1)`.
        !!
        !! **The scores depend on the input only through its ORDER.** That is the property worth
        !! knowing and the one that constrains the implementation: any strictly monotone transform
        !! of `values` must give bit-identical scores, so the value may be read only to sort it and
        !! never to compute with. The midranks are `pf_corr(method="spearman")`'s, over the
        !! SURVIVING elements rather than the original array.
        module subroutine normal_scores_f64(values, s, is_valid, skipnan, method, out_valid, &
                n_null, ok, threads)
            real(real64), intent(in) :: values(:) !! the population.
            real(real64), intent(out) :: s(:)
            !! the normal scores, same size as `values`, in the caller's original order.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            logical, intent(in), optional :: skipnan
            !! .true. (the default) excludes a NaN from the population, as a null is excluded and
            !! as `pf_minmax` has always done; .false. restores numpy's propagating behaviour, in
            !! which one NaN makes every answer NaN.
            character(len=*), intent(in), optional :: method
            !! the PLOTTING POSITION: which point of the distribution a rank is taken to mark.
            !! `s(i) = Phi**(-1)((r - a)/(m + 1 - 2a))` for a midrank `r` among `m` survivors,
            !! with `a` given by the token: "blom" (the default, `a = 3/8`, the standard rankit
            !! and what R's `qqnorm` uses), "weibull" (`a = 0`, `r/(m+1)`, the van der Waerden
            !! score), "tukey" (`a = 1/3`), "hazen" (`a = 1/2`) or "cunnane" (`a = 0.4`).
            !! "filliben" is the MEDIAN rank and not of that family at all: `1 - 0.5**(1/m)` at
            !! the first position, `0.5**(1/m)` at the last and `(i - 0.3175)/(m + 0.365)`
            !! between. Any other token aborts, naming all six. Matched case-insensitively.
            logical, intent(out), optional :: out_valid(:)
            !! per output element: .false. where the input was excluded and the element therefore
            !! has no score. Same size as `values`. **Absent, such an element is written as a
            !! quiet NaN instead** -- which is loud in practice, since it propagates through
            !! whatever the caller does next.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
            integer, intent(in), optional :: threads
            !! how many threads the midrank ordering may use, passed straight to `pf_argsort`.
            !! Absent takes the automatic rule. **The answer does not depend on this argument**:
            !! ties take the mean of the positions they span, so no tie-breaking the sort might
            !! choose can reach the result. It is a speed control and never an accuracy one.
        end subroutine normal_scores_f64
        !> `pf_sigma_clipped_stats` over a 64-bit real array: astropy's iterative clip.
        !!
        !! **One ordering serves the whole run**, and that is a property rather than an
        !! optimisation: the keep condition is an INTERVAL, so the survivors of every round are a
        !! contiguous sub-range of the sorted order and each later round only moves two indices
        !! inward. That is what turns an O(k n log n) routine into O(n log n + k n).
        module subroutine sigma_clipped_stats_f64(values, mean, median, stddev, sigma, &
                sigma_lower, sigma_upper, maxiters, cenfunc, stdfunc, n_clipped, keep, &
                converged, is_valid, skipnan, n_null, n_nan, ok, threads)
            real(real64), intent(in) :: values(:) !! the population, before exclusions.
            real(real64), intent(out) :: mean !! the mean of the surviving values; NaN when none survive.
            real(real64), intent(out) :: median !! their median.
            real(real64), intent(out) :: stddev !! their POPULATION standard deviation (`ddof = 0`), as astropy reports.
            real(real64), intent(in), optional :: sigma
            !! how many scale units a value may sit from the centre and still be kept. 3.0 by
            !! default, as astropy does. `sigma_lower` and `sigma_upper` each default to this, so
            !! setting it alone gives a symmetric clip.
            real(real64), intent(in), optional :: sigma_lower
            !! the lower half of the clip, overriding `sigma` below the centre.
            real(real64), intent(in), optional :: sigma_upper
            !! the upper half of the clip, overriding `sigma` above the centre.
            integer, intent(in), optional :: maxiters
            !! how many clipping rounds to run; 5 by default, as astropy does. **A non-positive
            !! value means iterate until nothing more is removed**, which is astropy's
            !! `maxiters=None` -- capped at a hard internal 100 rounds, and reaching that cap
            !! STOPS and reports through `converged` rather than aborting. Each non-terminating
            !! round removes at least one point, so a large catalogue can honestly want more
            !! rounds than any small constant.
            character(len=*), intent(in), optional :: cenfunc
            !! which centre each round clips about: "median" (the default, as astropy) or "mean".
            !! Any other token aborts, naming both. Matched case-insensitively.
            character(len=*), intent(in), optional :: stdfunc
            !! which scale each round measures: "std" (the default, as astropy -- the sample
            !! standard deviation at `ddof = 1`) or "mad_std", the normal-scaled median absolute
            !! deviation, which is far less disturbed by the outliers being clipped. Any other
            !! token aborts, naming both. Matched case-insensitively.
            integer(int64), intent(out), optional :: n_clipped
            !! how many elements of the population the clipping removed. `0` for an empty
            !! population, and `0` when the first round removes nothing.
            logical, intent(out), optional :: keep(:)
            !! per element of `values`: .true. for one that survived both the exclusion rules and
            !! every clipping round. Same size as `values`. This is what a caller needs in order
            !! to apply the same clip to a SECOND column.
            logical, intent(out), optional :: converged
            !! .false. when the iteration stopped at its round limit with points still being
            !! removed, rather than because a round removed nothing. The answer is still usable
            !! -- it is the last completed round's -- so `ok` stays .true. and this is the only
            !! way to tell the two endings apart.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            logical, intent(in), optional :: skipnan
            !! .true. (the default) excludes a NaN from the population, as a null is excluded and
            !! as `pf_minmax` has always done; .false. restores numpy's propagating behaviour, in
            !! which one NaN makes every answer NaN.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            integer(int64), intent(out), optional :: n_nan
            !! how many were excluded as NaN and were not already null.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
            integer, intent(in), optional :: threads
            !! how many threads the ORDERING may use -- this is an order statistic and has no
            !! central-moment pass, so the work `threads=` divides is `pf_argsort`'s. Absent takes
            !! the automatic rule: the `sort_threads` setting, capped by the processors
            !! actually available and by a measured work floor, and 1 inside a caller's own
            !! parallel region. **The answer does not depend on this argument** -- a sort is a
            !! permutation and the selection off it is exact, so 1, 8 and a build with no OpenMP
            !! at all return the same bits. It is a speed control and never an accuracy one.
        end subroutine sigma_clipped_stats_f64
        !> `pf_cumsum` over a 64-bit real array: the running sum, element by element.
        module subroutine cumsum_f64(values, out, is_valid, skipnan, out_valid, n_null, n_nan, ok)
            real(real64), intent(in) :: values(:) !! the population, in the order it is to be scanned.
            real(real64), intent(out) :: out(:) !! the running value, same size as `values`.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            logical, intent(in), optional :: skipnan
            !! .true. (the default) excludes a NaN from the population, as a null is excluded and
            !! as `pf_minmax` has always done; .false. restores numpy's propagating behaviour, in
            !! which one NaN makes every answer NaN.
            logical, intent(out), optional :: out_valid(:)
            !! per output element: .false. where the input was excluded and the result is
            !! therefore undefined. Same size as `values`. **Absent, such an element is written as
            !! a quiet NaN instead** -- which is loud in practice, since it propagates through
            !! whatever the caller does next.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            integer(int64), intent(out), optional :: n_nan
            !! how many were excluded as NaN and were not already null.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
        end subroutine cumsum_f64
        !> `pf_cumprod` over a 64-bit real array: the running product.
        module subroutine cumprod_f64(values, out, is_valid, skipnan, out_valid, n_null, n_nan, ok)
            real(real64), intent(in) :: values(:) !! the population, in the order it is to be scanned.
            real(real64), intent(out) :: out(:) !! the running value, same size as `values`.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            logical, intent(in), optional :: skipnan
            !! .true. (the default) excludes a NaN from the population, as a null is excluded and
            !! as `pf_minmax` has always done; .false. restores numpy's propagating behaviour, in
            !! which one NaN makes every answer NaN.
            logical, intent(out), optional :: out_valid(:)
            !! per output element: .false. where the input was excluded and the result is
            !! therefore undefined. Same size as `values`. **Absent, such an element is written as
            !! a quiet NaN instead** -- which is loud in practice, since it propagates through
            !! whatever the caller does next.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            integer(int64), intent(out), optional :: n_nan
            !! how many were excluded as NaN and were not already null.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
        end subroutine cumprod_f64
        !> `pf_cummax` over a 64-bit real array: the running maximum.
        module subroutine cummax_f64(values, out, is_valid, skipnan, out_valid, n_null, n_nan, ok)
            real(real64), intent(in) :: values(:) !! the population, in the order it is to be scanned.
            real(real64), intent(out) :: out(:) !! the running value, same size as `values`.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            logical, intent(in), optional :: skipnan
            !! .true. (the default) excludes a NaN from the population, as a null is excluded and
            !! as `pf_minmax` has always done; .false. restores numpy's propagating behaviour, in
            !! which one NaN makes every answer NaN.
            logical, intent(out), optional :: out_valid(:)
            !! per output element: .false. where the input was excluded and the result is
            !! therefore undefined. Same size as `values`. **Absent, such an element is written as
            !! a quiet NaN instead** -- which is loud in practice, since it propagates through
            !! whatever the caller does next.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            integer(int64), intent(out), optional :: n_nan
            !! how many were excluded as NaN and were not already null.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
        end subroutine cummax_f64
        !> `pf_cummin` over a 64-bit real array: the running minimum.
        module subroutine cummin_f64(values, out, is_valid, skipnan, out_valid, n_null, n_nan, ok)
            real(real64), intent(in) :: values(:) !! the population, in the order it is to be scanned.
            real(real64), intent(out) :: out(:) !! the running value, same size as `values`.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            logical, intent(in), optional :: skipnan
            !! .true. (the default) excludes a NaN from the population, as a null is excluded and
            !! as `pf_minmax` has always done; .false. restores numpy's propagating behaviour, in
            !! which one NaN makes every answer NaN.
            logical, intent(out), optional :: out_valid(:)
            !! per output element: .false. where the input was excluded and the result is
            !! therefore undefined. Same size as `values`. **Absent, such an element is written as
            !! a quiet NaN instead** -- which is loud in practice, since it propagates through
            !! whatever the caller does next.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            integer(int64), intent(out), optional :: n_nan
            !! how many were excluded as NaN and were not already null.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
        end subroutine cummin_f64
    end interface
    !
    ! ---- The real64 binning core (implemented in parquet_stats_bin) ----
    interface
        !> `pf_bucketize` over a 64-bit real array: which bin each value falls in.
        !!
        !! The bins are the intervals between consecutive `edges`, and the answer is a 1-based bin
        !! number or **0** for a value that joined none -- excluded, or outside the edge range.
        !! `pd.cut` spells that last case -1 over 0-based codes; 0 is the 1-based spelling of the
        !! same idea, and it is the value a Fortran caller can test without knowing the bin count.
        module subroutine bucketize_f64(values, edges, codes, right, is_valid, weights, &
                skipnan, n_null, n_nan, n_outside, ok)
            real(real64), intent(in) :: values(:) !! the values to classify.
            real(real64), intent(in) :: edges(:) !! the bin boundaries, strictly increasing, at least two.
            integer(int32), intent(out) :: codes(:) !! the 1-based bin of each value, or 0. Same size as `values`.
            logical, intent(in), optional :: right
            !! which side of a bin is closed. `.false.` by default, which is numpy's rule and the
            !! one `np.histogram`/`np.digitize` use: bin k is `[edges(k), edges(k+1))`, and the
            !! LAST bin closes at the top so that `edges(nbins+1)` itself lands in it. `.true.`
            !! mirrors it, which is `pd.cut(right=True, include_lowest=True)`: bin k is
            !! `(edges(k), edges(k+1)]`, and the FIRST bin closes at the bottom. Either way the
            !! closed range `[edges(1), edges(nbins+1)]` is covered exactly once, so the two
            !! conventions differ only in which bin a value sitting exactly on an interior edge
            !! joins.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight, non-negative. A ZERO weight removes the element from the
            !! population; a negative, NaN or infinite one aborts. Absent means every weight is 1.
            logical, intent(in), optional :: skipnan
            !! .true. (the default) excludes a NaN from the population, as a null is excluded and
            !! as `pf_minmax` has always done; .false. restores numpy's propagating behaviour, in
            !! which one NaN makes every answer NaN.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            integer(int64), intent(out), optional :: n_nan
            !! how many were excluded as NaN and were not already null.
            integer(int64), intent(out), optional :: n_outside
            !! how many usable elements fell outside `[edges(1), edges(nbins+1)]` and so joined no
            !! bin. This is an ordinary data condition rather than an error -- `np.histogram`
            !! drops such values silently -- but it is the one thing a caller cannot recover from
            !! the output, so it is reported. A NaN under `skipnan = .false.` is counted here: it
            !! is then a value, and it matches no bin.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
        end subroutine bucketize_f64
        !> `pf_histogram` over a 64-bit real array: how much weight lands in each bin.
        !!
        !! `np.histogram(values, bins=edges)`, and `np.histogram(..., weights=)` when `weights` is
        !! present. The counts are `real(real64)` for that reason: a weighted histogram sums
        !! weights rather than counting elements. Unweighted they are whole numbers, exactly, up
        !! to 2**53 elements in one bin.
        module subroutine histogram_f64(values, edges, counts, right, density, is_valid, &
                weights, skipnan, n_null, n_nan, n_outside, ok)
            real(real64), intent(in) :: values(:) !! the values to bin.
            real(real64), intent(in) :: edges(:) !! the bin boundaries, strictly increasing, at least two.
            real(real64), intent(out) :: counts(:)
            !! one entry per bin, so `size(edges) - 1` of them. A density when `density` is set.
            logical, intent(in), optional :: right
            !! which side of a bin is closed. `.false.` by default, which is numpy's rule and the
            !! one `np.histogram`/`np.digitize` use: bin k is `[edges(k), edges(k+1))`, and the
            !! LAST bin closes at the top so that `edges(nbins+1)` itself lands in it. `.true.`
            !! mirrors it, which is `pd.cut(right=True, include_lowest=True)`: bin k is
            !! `(edges(k), edges(k+1)]`, and the FIRST bin closes at the bottom. Either way the
            !! closed range `[edges(1), edges(nbins+1)]` is covered exactly once, so the two
            !! conventions differ only in which bin a value sitting exactly on an interior edge
            !! joins.
            logical, intent(in), optional :: density
            !! `.false.` by default. `.true.` divides each bin by its own WIDTH and by the total
            !! that was binned, giving a probability density -- `np.histogram(density=True)`, so
            !! `sum(counts * widths)` is 1 and an uneven edge spacing is accounted for. The
            !! normalisation base is what actually landed in a bin: values outside the edges were
            !! never counted, so they do not appear in it, which is numpy's rule too. When nothing
            !! was binned the density is undefined and every entry is a **quiet NaN** with
            !! `ok = .false.` -- note that an empty HISTOGRAM is perfectly well defined (all
            !! zeros, `ok = .true.`) and an empty DENSITY is not.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight, non-negative. A ZERO weight removes the element from the
            !! population; a negative, NaN or infinite one aborts. Absent means every weight is 1.
            logical, intent(in), optional :: skipnan
            !! .true. (the default) excludes a NaN from the population, as a null is excluded and
            !! as `pf_minmax` has always done; .false. restores numpy's propagating behaviour, in
            !! which one NaN makes every answer NaN.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            integer(int64), intent(out), optional :: n_nan
            !! how many were excluded as NaN and were not already null.
            integer(int64), intent(out), optional :: n_outside
            !! how many usable elements fell outside `[edges(1), edges(nbins+1)]` and so joined no
            !! bin. This is an ordinary data condition rather than an error -- `np.histogram`
            !! drops such values silently -- but it is the one thing a caller cannot recover from
            !! the output, so it is reported. A NaN under `skipnan = .false.` is counted here: it
            !! is then a value, and it matches no bin.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
        end subroutine histogram_f64
        !> `pf_bin_edges` over a 64-bit real array: `nbins` equal-width bins spanning the data.
        !!
        !! `np.histogram_bin_edges(values, bins=nbins)`, including its two degenerate answers: a
        !! CONSTANT population widens to `[x - 0.5, x + 0.5]` and an EMPTY one falls back to
        !! `[0, 1]`, both of which `ok = .false.` reports.
        module subroutine bin_edges_f64(values, nbins, edges, is_valid, weights, skipnan, n_null, &
                n_nan, ok)
            real(real64), intent(in) :: values(:) !! the population the range is taken over.
            integer, intent(in) :: nbins !! how many bins to describe; at least 1.
            real(real64), intent(out) :: edges(:) !! the `nbins + 1` boundaries, strictly increasing.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight, non-negative. A ZERO weight removes the element from the
            !! population; a negative, NaN or infinite one aborts. Absent means every weight is 1.
            logical, intent(in), optional :: skipnan
            !! .true. (the default) excludes a NaN from the population, as a null is excluded and
            !! as `pf_minmax` has always done; .false. restores numpy's propagating behaviour, in
            !! which one NaN makes every answer NaN.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            integer(int64), intent(out), optional :: n_nan
            !! how many were excluded as NaN and were not already null.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
        end subroutine bin_edges_f64
    end interface
    !
    ! ---- pf_mode, one specific per kind (implemented in parquet_stats_order) ----
    interface
        !> `pf_mode` over a 32-bit integer array.
        module subroutine mode_i32(values, m, count, modes, is_valid, weights, n_null, ok)
            integer(int32), intent(in) :: values(:) !! the population.
            integer(int32), intent(out) :: m
            !! the modal value; unchanged when the population is empty.
            integer(int64), intent(out), optional :: count
            !! how many elements hold the modal value. `0` when the population is empty. Counts
            !! ELEMENTS even when weights decided which value won, so a weighted mode reports the
            !! occurrences of the value carrying the greatest total weight.
            integer(int32), allocatable, intent(out), optional :: modes(:)
            !! **every** modal value, in ascending order, allocated to exactly the number that tie --
            !! which is pandas' `Series.mode()`, where `m` alone is scipy's `stats.mode`. `m` is always
            !! `modes(1)`, since both take the smallest of a tie, so asking for both costs one extra
            !! gather and no second pass. Allocated to size 0 for an empty population, never left
            !! unallocated: `size(modes)` is then the only test a caller needs, and an unallocated
            !! result would make it undefined behaviour instead. For the two character forms every
            !! element shares one length, which is the LONGEST tied value's -- a shorter one is
            !! blank-padded, so compare with `trim()`.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight, non-negative. A ZERO weight removes the element from the
            !! population; a negative, NaN or infinite one aborts. Absent means every weight is 1.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            logical, intent(out), optional :: ok
            !! .false. when the population is empty, and there is therefore no modal value. `m`
            !! is then untouched -- unallocated, for the two character forms -- and must not be
            !! read; `count` is 0.
        end subroutine mode_i32
        !> `pf_mode` over a 64-bit integer array.
        module subroutine mode_i64(values, m, count, modes, is_valid, weights, n_null, ok)
            integer(int64), intent(in) :: values(:) !! the population.
            integer(int64), intent(out) :: m
            !! the modal value; unchanged when the population is empty.
            integer(int64), intent(out), optional :: count
            !! how many elements hold the modal value. `0` when the population is empty. Counts
            !! ELEMENTS even when weights decided which value won, so a weighted mode reports the
            !! occurrences of the value carrying the greatest total weight.
            integer(int64), allocatable, intent(out), optional :: modes(:)
            !! **every** modal value, in ascending order, allocated to exactly the number that tie --
            !! which is pandas' `Series.mode()`, where `m` alone is scipy's `stats.mode`. `m` is always
            !! `modes(1)`, since both take the smallest of a tie, so asking for both costs one extra
            !! gather and no second pass. Allocated to size 0 for an empty population, never left
            !! unallocated: `size(modes)` is then the only test a caller needs, and an unallocated
            !! result would make it undefined behaviour instead. For the two character forms every
            !! element shares one length, which is the LONGEST tied value's -- a shorter one is
            !! blank-padded, so compare with `trim()`.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight, non-negative. A ZERO weight removes the element from the
            !! population; a negative, NaN or infinite one aborts. Absent means every weight is 1.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            logical, intent(out), optional :: ok
            !! .false. when the population is empty, and there is therefore no modal value. `m`
            !! is then untouched -- unallocated, for the two character forms -- and must not be
            !! read; `count` is 0.
        end subroutine mode_i64
        !> `pf_mode` over a logical array.
        module subroutine mode_bool(values, m, count, modes, is_valid, weights, n_null, ok)
            logical, intent(in) :: values(:) !! the population.
            logical, intent(out) :: m
            !! the modal value; unchanged when the population is empty. `.false.` sorts below `.true.`, so
            !! an even split answers `.false.`.
            integer(int64), intent(out), optional :: count
            !! how many elements hold the modal value. `0` when the population is empty. Counts
            !! ELEMENTS even when weights decided which value won, so a weighted mode reports the
            !! occurrences of the value carrying the greatest total weight.
            logical, allocatable, intent(out), optional :: modes(:)
            !! **every** modal value, in ascending order, allocated to exactly the number that tie --
            !! which is pandas' `Series.mode()`, where `m` alone is scipy's `stats.mode`. `m` is always
            !! `modes(1)`, since both take the smallest of a tie, so asking for both costs one extra
            !! gather and no second pass. Allocated to size 0 for an empty population, never left
            !! unallocated: `size(modes)` is then the only test a caller needs, and an unallocated
            !! result would make it undefined behaviour instead. For the two character forms every
            !! element shares one length, which is the LONGEST tied value's -- a shorter one is
            !! blank-padded, so compare with `trim()`.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight, non-negative. A ZERO weight removes the element from the
            !! population; a negative, NaN or infinite one aborts. Absent means every weight is 1.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            logical, intent(out), optional :: ok
            !! .false. when the population is empty, and there is therefore no modal value. `m`
            !! is then untouched -- unallocated, for the two character forms -- and must not be
            !! read; `count` is 0.
        end subroutine mode_bool
        !> `pf_mode` over a character array.
        module subroutine mode_chr(values, m, count, modes, is_valid, weights, n_null, ok)
            character(len=*), intent(in) :: values(:) !! the population; each element is trimmed.
            character(len=:), allocatable, intent(out) :: m
            !! the modal value, allocated to its own trimmed length. **Left unallocated when the population
            !! is empty**, which is the one shape in this module where `ok = .false.` and an unallocated
            !! result coincide -- test `ok`, or `allocated(m)`, before reading it.
            integer(int64), intent(out), optional :: count
            !! how many elements hold the modal value. `0` when the population is empty. Counts
            !! ELEMENTS even when weights decided which value won, so a weighted mode reports the
            !! occurrences of the value carrying the greatest total weight.
            character(len=:), allocatable, intent(out), optional :: modes(:)
            !! **every** modal value, in ascending order, allocated to exactly the number that tie --
            !! which is pandas' `Series.mode()`, where `m` alone is scipy's `stats.mode`. `m` is always
            !! `modes(1)`, since both take the smallest of a tie, so asking for both costs one extra
            !! gather and no second pass. Allocated to size 0 for an empty population, never left
            !! unallocated: `size(modes)` is then the only test a caller needs, and an unallocated
            !! result would make it undefined behaviour instead. For the two character forms every
            !! element shares one length, which is the LONGEST tied value's -- a shorter one is
            !! blank-padded, so compare with `trim()`.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight, non-negative. A ZERO weight removes the element from the
            !! population; a negative, NaN or infinite one aborts. Absent means every weight is 1.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            logical, intent(out), optional :: ok
            !! .false. when the population is empty, and there is therefore no modal value. `m`
            !! is then untouched -- unallocated, for the two character forms -- and must not be
            !! read; `count` is 0.
        end subroutine mode_chr
        !> `pf_mode` over a `parquet_string_column`.
        module subroutine mode_str(values, m, count, modes, is_valid, weights, n_null, ok)
            type(parquet_string_column), intent(in) :: values !! the population.
            character(len=:), allocatable, intent(out) :: m
            !! the modal value, allocated to its own length. Left unallocated when the population is empty.
            integer(int64), intent(out), optional :: count
            !! how many elements hold the modal value. `0` when the population is empty. Counts
            !! ELEMENTS even when weights decided which value won, so a weighted mode reports the
            !! occurrences of the value carrying the greatest total weight.
            character(len=:), allocatable, intent(out), optional :: modes(:)
            !! **every** modal value, in ascending order, allocated to exactly the number that tie --
            !! which is pandas' `Series.mode()`, where `m` alone is scipy's `stats.mode`. `m` is always
            !! `modes(1)`, since both take the smallest of a tie, so asking for both costs one extra
            !! gather and no second pass. Allocated to size 0 for an empty population, never left
            !! unallocated: `size(modes)` is then the only test a caller needs, and an unallocated
            !! result would make it undefined behaviour instead. For the two character forms every
            !! element shares one length, which is the LONGEST tied value's -- a shorter one is
            !! blank-padded, so compare with `trim()`.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight, non-negative. A ZERO weight removes the element from the
            !! population; a negative, NaN or infinite one aborts. Absent means every weight is 1.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            logical, intent(out), optional :: ok
            !! .false. when the population is empty, and there is therefore no modal value. `m`
            !! is then untouched -- unallocated, for the two character forms -- and must not be
            !! read; `count` is 0.
        end subroutine mode_str
    end interface
    !
    ! ---- The per-kind entry layer (implemented in parquet_stats_kernel) ----
    interface
        !> `pf_sum` over a 32-bit integer array.
        module subroutine sum_i32(values, s, is_valid, weights, n_null, ok, threads)
            integer(int32), intent(in) :: values(:) !! the population.
            real(real64), intent(out) :: s !! the sum.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight, non-negative. A ZERO weight removes the element from the
            !! population; a negative, NaN or infinite one aborts. Absent means every weight is 1.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
            integer, intent(in), optional :: threads
            !! how many threads the central-moment pass may use. Absent takes the automatic rule:
            !! the `sort_threads` setting, capped by the processors actually available and
            !! by a measured work floor, and 1 inside a caller's own parallel region. **The answer
            !! does not depend on this argument** -- the block decomposition is a function of the
            !! population size alone, so 1, 8 and a build with no OpenMP at all return the same
            !! bits. It is a speed control and never an accuracy one.
        end subroutine sum_i32
        !> `pf_mean` over a 32-bit integer array.
        module subroutine mean_i32(values, m, is_valid, weights, n_null, ok, threads)
            integer(int32), intent(in) :: values(:) !! the population.
            real(real64), intent(out) :: m !! the mean; NaN when the population is empty.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight, non-negative. A ZERO weight removes the element from the
            !! population; a negative, NaN or infinite one aborts. Absent means every weight is 1.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
            integer, intent(in), optional :: threads
            !! how many threads the central-moment pass may use. Absent takes the automatic rule:
            !! the `sort_threads` setting, capped by the processors actually available and
            !! by a measured work floor, and 1 inside a caller's own parallel region. **The answer
            !! does not depend on this argument** -- the block decomposition is a function of the
            !! population size alone, so 1, 8 and a build with no OpenMP at all return the same
            !! bits. It is a speed control and never an accuracy one.
        end subroutine mean_i32
        !> `pf_gmean` over a 32-bit integer array.
        module subroutine gmean_i32(values, g, is_valid, weights, n_null, ok, threads)
            integer(int32), intent(in) :: values(:) !! the population.
            real(real64), intent(out) :: g !! the geometric mean; 0 for a zero, NaN for a negative or empty population.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight, non-negative. A ZERO weight removes the element from the
            !! population; a negative, NaN or infinite one aborts. Absent means every weight is 1.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
            integer, intent(in), optional :: threads
            !! how many threads the central-moment pass may use. Absent takes the automatic rule:
            !! the `sort_threads` setting, capped by the processors actually available and
            !! by a measured work floor, and 1 inside a caller's own parallel region. **The answer
            !! does not depend on this argument** -- the block decomposition is a function of the
            !! population size alone, so 1, 8 and a build with no OpenMP at all return the same
            !! bits. It is a speed control and never an accuracy one.
        end subroutine gmean_i32
        !> `pf_hmean` over a 32-bit integer array.
        module subroutine hmean_i32(values, h, is_valid, weights, n_null, ok, threads)
            integer(int32), intent(in) :: values(:) !! the population.
            real(real64), intent(out) :: h !! the harmonic mean; 0 for a zero, NaN for a negative or empty population.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight, non-negative. A ZERO weight removes the element from the
            !! population; a negative, NaN or infinite one aborts. Absent means every weight is 1.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
            integer, intent(in), optional :: threads
            !! how many threads the central-moment pass may use. Absent takes the automatic rule:
            !! the `sort_threads` setting, capped by the processors actually available and
            !! by a measured work floor, and 1 inside a caller's own parallel region. **The answer
            !! does not depend on this argument** -- the block decomposition is a function of the
            !! population size alone, so 1, 8 and a build with no OpenMP at all return the same
            !! bits. It is a speed control and never an accuracy one.
        end subroutine hmean_i32
        !> `pf_variance` over a 32-bit integer array.
        module subroutine variance_i32(values, v, is_valid, weights, weight_type, ddof, n_null, ok, threads)
            integer(int32), intent(in) :: values(:) !! the population.
            real(real64), intent(out) :: v !! the variance; NaN when `n_valid <= ddof`.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight, non-negative. A ZERO weight removes the element from the
            !! population; a negative, NaN or infinite one aborts. Absent means every weight is 1.
            character(len=*), intent(in), optional :: weight_type
            !! "reliability" (the default) or "frequency". The two differ only when the weights are
            !! unequal: a FREQUENCY weight of 3 says the value occurred three times, so the count
            !! `ddof` is charged against is `sum(weights)`; a RELIABILITY weight says the value is
            !! that much more precise, so the count is Kish's effective size
            !! `sum(weights)**2 / sum(weights**2)`. Any other token aborts, listing both.
            integer, intent(in), optional :: ddof
            !! delta degrees of freedom; **1 by default** -- the sample variance, as pandas
            !! returns. numpy's default is 0, so pass `ddof=0` to reproduce `np.var`/`np.std`.
            !! A plain default-kind integer deliberately: it is bounded by the population size,
            !! so there is no int64 form to provide. `ddof >= n_valid` gives NaN, not a division.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
            integer, intent(in), optional :: threads
            !! how many threads the central-moment pass may use. Absent takes the automatic rule:
            !! the `sort_threads` setting, capped by the processors actually available and
            !! by a measured work floor, and 1 inside a caller's own parallel region. **The answer
            !! does not depend on this argument** -- the block decomposition is a function of the
            !! population size alone, so 1, 8 and a build with no OpenMP at all return the same
            !! bits. It is a speed control and never an accuracy one.
        end subroutine variance_i32
        !> `pf_stddev` over a 32-bit integer array.
        module subroutine stddev_i32(values, sd, is_valid, weights, weight_type, ddof, n_null, ok, threads)
            integer(int32), intent(in) :: values(:) !! the population.
            real(real64), intent(out) :: sd !! the standard deviation; NaN when the variance is.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight, non-negative. A ZERO weight removes the element from the
            !! population; a negative, NaN or infinite one aborts. Absent means every weight is 1.
            character(len=*), intent(in), optional :: weight_type
            !! "reliability" (the default) or "frequency". The two differ only when the weights are
            !! unequal: a FREQUENCY weight of 3 says the value occurred three times, so the count
            !! `ddof` is charged against is `sum(weights)`; a RELIABILITY weight says the value is
            !! that much more precise, so the count is Kish's effective size
            !! `sum(weights)**2 / sum(weights**2)`. Any other token aborts, listing both.
            integer, intent(in), optional :: ddof
            !! delta degrees of freedom; **1 by default** -- the sample variance, as pandas
            !! returns. numpy's default is 0, so pass `ddof=0` to reproduce `np.var`/`np.std`.
            !! A plain default-kind integer deliberately: it is bounded by the population size,
            !! so there is no int64 form to provide. `ddof >= n_valid` gives NaN, not a division.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
            integer, intent(in), optional :: threads
            !! how many threads the central-moment pass may use. Absent takes the automatic rule:
            !! the `sort_threads` setting, capped by the processors actually available and
            !! by a measured work floor, and 1 inside a caller's own parallel region. **The answer
            !! does not depend on this argument** -- the block decomposition is a function of the
            !! population size alone, so 1, 8 and a build with no OpenMP at all return the same
            !! bits. It is a speed control and never an accuracy one.
        end subroutine stddev_i32
        !> `pf_sem` over a 32-bit integer array.
        module subroutine sem_i32(values, se, is_valid, weights, weight_type, ddof, n_null, ok, threads)
            integer(int32), intent(in) :: values(:) !! the population.
            real(real64), intent(out) :: se !! the standard error; NaN when the standard deviation is.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight, non-negative. A ZERO weight removes the element from the
            !! population; a negative, NaN or infinite one aborts. Absent means every weight is 1.
            character(len=*), intent(in), optional :: weight_type
            !! "reliability" (the default) or "frequency". The two differ only when the weights are
            !! unequal: a FREQUENCY weight of 3 says the value occurred three times, so the count
            !! `ddof` is charged against is `sum(weights)`; a RELIABILITY weight says the value is
            !! that much more precise, so the count is Kish's effective size
            !! `sum(weights)**2 / sum(weights**2)`. Any other token aborts, listing both.
            integer, intent(in), optional :: ddof
            !! delta degrees of freedom; **1 by default** -- the sample variance, as pandas
            !! returns. numpy's default is 0, so pass `ddof=0` to reproduce `np.var`/`np.std`.
            !! A plain default-kind integer deliberately: it is bounded by the population size,
            !! so there is no int64 form to provide. `ddof >= n_valid` gives NaN, not a division.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
            integer, intent(in), optional :: threads
            !! how many threads the central-moment pass may use. Absent takes the automatic rule:
            !! the `sort_threads` setting, capped by the processors actually available and
            !! by a measured work floor, and 1 inside a caller's own parallel region. **The answer
            !! does not depend on this argument** -- the block decomposition is a function of the
            !! population size alone, so 1, 8 and a build with no OpenMP at all return the same
            !! bits. It is a speed control and never an accuracy one.
        end subroutine sem_i32
        !> `pf_skewness` over a 32-bit integer array.
        module subroutine skewness_i32(values, g, is_valid, weights, weight_type, bias, n_null, ok, threads)
            integer(int32), intent(in) :: values(:) !! the population.
            real(real64), intent(out) :: g !! the skewness.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight, non-negative. A ZERO weight removes the element from the
            !! population; a negative, NaN or infinite one aborts. Absent means every weight is 1.
            character(len=*), intent(in), optional :: weight_type
            !! "reliability" (the default) or "frequency". The two differ only when the weights are
            !! unequal: a FREQUENCY weight of 3 says the value occurred three times, so the count
            !! `ddof` is charged against is `sum(weights)`; a RELIABILITY weight says the value is
            !! that much more precise, so the count is Kish's effective size
            !! `sum(weights)**2 / sum(weights**2)`. Any other token aborts, listing both.
            logical, intent(in), optional :: bias
            !! .false. by default: the bias-CORRECTED G1/G2, which is what pandas returns.
            !! .true. gives the uncorrected g1/g2, which is what scipy returns by default.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
            integer, intent(in), optional :: threads
            !! how many threads the central-moment pass may use. Absent takes the automatic rule:
            !! the `sort_threads` setting, capped by the processors actually available and
            !! by a measured work floor, and 1 inside a caller's own parallel region. **The answer
            !! does not depend on this argument** -- the block decomposition is a function of the
            !! population size alone, so 1, 8 and a build with no OpenMP at all return the same
            !! bits. It is a speed control and never an accuracy one.
        end subroutine skewness_i32
        !> `pf_kurtosis` over a 32-bit integer array.
        module subroutine kurtosis_i32(values, k, is_valid, weights, weight_type, bias, excess, n_null, ok, threads)
            integer(int32), intent(in) :: values(:) !! the population.
            real(real64), intent(out) :: k !! the kurtosis, excess by default.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight, non-negative. A ZERO weight removes the element from the
            !! population; a negative, NaN or infinite one aborts. Absent means every weight is 1.
            character(len=*), intent(in), optional :: weight_type
            !! "reliability" (the default) or "frequency". The two differ only when the weights are
            !! unequal: a FREQUENCY weight of 3 says the value occurred three times, so the count
            !! `ddof` is charged against is `sum(weights)`; a RELIABILITY weight says the value is
            !! that much more precise, so the count is Kish's effective size
            !! `sum(weights)**2 / sum(weights**2)`. Any other token aborts, listing both.
            logical, intent(in), optional :: bias
            !! .false. by default: the bias-CORRECTED G1/G2, which is what pandas returns.
            !! .true. gives the uncorrected g1/g2, which is what scipy returns by default.
            logical, intent(in), optional :: excess
            !! .true. by default: EXCESS kurtosis, which is 0 for a normal population.
            !! .false. adds 3 back, giving the raw fourth-moment ratio.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
            integer, intent(in), optional :: threads
            !! how many threads the central-moment pass may use. Absent takes the automatic rule:
            !! the `sort_threads` setting, capped by the processors actually available and
            !! by a measured work floor, and 1 inside a caller's own parallel region. **The answer
            !! does not depend on this argument** -- the block decomposition is a function of the
            !! population size alone, so 1, 8 and a build with no OpenMP at all return the same
            !! bits. It is a speed control and never an accuracy one.
        end subroutine kurtosis_i32
        !> `pf_moments` over a 32-bit integer array: every tier-A quantity in one pair of passes.
        module subroutine moments_i32(values, n_valid, mean, variance, stddev, sem, skewness, kurtosis, vsum, vmin, &
                vmax, is_valid, weights, weight_type, ddof, bias, excess, &
                n_null, ok, threads)
            integer(int32), intent(in) :: values(:) !! the population.
            integer(int64), intent(out), optional :: n_valid !! how many elements were used.
            real(real64), intent(out), optional :: mean !! the mean; NaN when empty.
            real(real64), intent(out), optional :: variance !! the variance; NaN when undefined.
            real(real64), intent(out), optional :: stddev !! the standard deviation.
            real(real64), intent(out), optional :: sem !! the standard error of the mean.
            real(real64), intent(out), optional :: skewness !! the third standardised moment.
            real(real64), intent(out), optional :: kurtosis !! the fourth, excess by default.
            real(real64), intent(out), optional :: vsum !! the pairwise sum; 0 when empty.
            real(real64), intent(out), optional :: vmin !! the smallest value; NaN when empty.
            real(real64), intent(out), optional :: vmax !! the largest value; NaN when empty.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight, non-negative. A ZERO weight removes the element from the
            !! population; a negative, NaN or infinite one aborts. Absent means every weight is 1.
            character(len=*), intent(in), optional :: weight_type
            !! "reliability" (the default) or "frequency". The two differ only when the weights are
            !! unequal: a FREQUENCY weight of 3 says the value occurred three times, so the count
            !! `ddof` is charged against is `sum(weights)`; a RELIABILITY weight says the value is
            !! that much more precise, so the count is Kish's effective size
            !! `sum(weights)**2 / sum(weights**2)`. Any other token aborts, listing both.
            integer, intent(in), optional :: ddof
            !! delta degrees of freedom; **1 by default** -- the sample variance, as pandas
            !! returns. numpy's default is 0, so pass `ddof=0` to reproduce `np.var`/`np.std`.
            !! A plain default-kind integer deliberately: it is bounded by the population size,
            !! so there is no int64 form to provide. `ddof >= n_valid` gives NaN, not a division.
            logical, intent(in), optional :: bias
            !! .false. by default: the bias-CORRECTED G1/G2, which is what pandas returns.
            !! .true. gives the uncorrected g1/g2, which is what scipy returns by default.
            logical, intent(in), optional :: excess
            !! .true. by default: EXCESS kurtosis, which is 0 for a normal population.
            !! .false. adds 3 back, giving the raw fourth-moment ratio.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            logical, intent(out), optional :: ok
            !! .false. when any output that was ASKED for came back a quiet NaN -- an empty
            !! population, a `ddof` that leaves nothing to divide by, a constant population for
            !! the shape statistics, or a non-finite value under `skipnan = .false.`. Only the
            !! PRESENT outputs are tested, which is the only reading that works here: `vsum` over
            !! an empty population is a correct `0`, and testing an output the caller never asked
            !! for would report a failure that is not one. `n_valid`, `n_null` and `n_nan` are
            !! counts and are always defined, so they are not tested.
            !!
            !! An infinity is NOT a failure: `mean` answers `+Inf` over a population containing
            !! one, exactly as `pf_mean` and numpy do, and `ok` stays .true. for it while the
            !! variance -- genuinely a NaN there -- turns it .false. if it was asked for.
            integer, intent(in), optional :: threads
            !! how many threads the central-moment pass may use. Absent takes the automatic rule:
            !! the `sort_threads` setting, capped by the processors actually available and
            !! by a measured work floor, and 1 inside a caller's own parallel region. **The answer
            !! does not depend on this argument** -- the block decomposition is a function of the
            !! population size alone, so 1, 8 and a build with no OpenMP at all return the same
            !! bits. It is a speed control and never an accuracy one.
        end subroutine moments_i32
        !> `pf_median` over a 32-bit integer array.
        module subroutine median_i32(values, med, is_valid, weights, weight_type, method, n_null, ok, threads)
            integer(int32), intent(in) :: values(:) !! the population.
            real(real64), intent(out) :: med
            !! the median; NaN when the population is empty.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight, non-negative. A ZERO weight removes the element from the
            !! population; a negative, NaN or infinite one aborts. Absent means every weight is 1.
            character(len=*), intent(in), optional :: weight_type
            !! "reliability" (the default) or "frequency". The two differ only when the weights are
            !! unequal: a FREQUENCY weight of 3 says the value occurred three times, so the count
            !! `ddof` is charged against is `sum(weights)`; a RELIABILITY weight says the value is
            !! that much more precise, so the count is Kish's effective size
            !! `sum(weights)**2 / sum(weights**2)`. Any other token aborts, listing both.
            character(len=*), intent(in), optional :: method
            !! how a fractional position between two order statistics is resolved, in numpy's
            !! spelling: "linear" (the default -- Hyndman-Fan type 7, what numpy and pandas do),
            !! "lower", "higher", "nearest", "midpoint", or "inverted_cdf" (a step function on the
            !! plain cumulative scale, and the only token numpy itself accepts weights with). Any
            !! other token aborts, listing all six. Matched case-insensitively.
            !! **`weight_type="frequency"` changes the default to "inverted_cdf"**, because that
            !! token IS frequency expansion; an explicit `method=` still wins.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
            integer, intent(in), optional :: threads
            !! how many threads the ORDERING may use -- this is an order statistic and has no
            !! central-moment pass, so the work `threads=` divides is `pf_argsort`'s. Absent takes
            !! the automatic rule: the `sort_threads` setting, capped by the processors
            !! actually available and by a measured work floor, and 1 inside a caller's own
            !! parallel region. **The answer does not depend on this argument** -- a sort is a
            !! permutation and the selection off it is exact, so 1, 8 and a build with no OpenMP
            !! at all return the same bits. It is a speed control and never an accuracy one.
        end subroutine median_i32
        !> `pf_quantile` over a 32-bit integer array.
        module subroutine quantile_i32(values, p, q, is_valid, weights, weight_type, method, n_null, ok, threads)
            integer(int32), intent(in) :: values(:) !! the population.
            real(real64), intent(in) :: p
            !! the probability, on a 0-1 scale rather than 0-100. Outside [0, 1] aborts.
            real(real64), intent(out) :: q
            !! the quantile; NaN when the population is empty.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight, non-negative. A ZERO weight removes the element from the
            !! population; a negative, NaN or infinite one aborts. Absent means every weight is 1.
            character(len=*), intent(in), optional :: weight_type
            !! "reliability" (the default) or "frequency". The two differ only when the weights are
            !! unequal: a FREQUENCY weight of 3 says the value occurred three times, so the count
            !! `ddof` is charged against is `sum(weights)`; a RELIABILITY weight says the value is
            !! that much more precise, so the count is Kish's effective size
            !! `sum(weights)**2 / sum(weights**2)`. Any other token aborts, listing both.
            character(len=*), intent(in), optional :: method
            !! how a fractional position between two order statistics is resolved, in numpy's
            !! spelling: "linear" (the default -- Hyndman-Fan type 7, what numpy and pandas do),
            !! "lower", "higher", "nearest", "midpoint", or "inverted_cdf" (a step function on the
            !! plain cumulative scale, and the only token numpy itself accepts weights with). Any
            !! other token aborts, listing all six. Matched case-insensitively.
            !! **`weight_type="frequency"` changes the default to "inverted_cdf"**, because that
            !! token IS frequency expansion; an explicit `method=` still wins.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
            integer, intent(in), optional :: threads
            !! how many threads the ORDERING may use -- this is an order statistic and has no
            !! central-moment pass, so the work `threads=` divides is `pf_argsort`'s. Absent takes
            !! the automatic rule: the `sort_threads` setting, capped by the processors
            !! actually available and by a measured work floor, and 1 inside a caller's own
            !! parallel region. **The answer does not depend on this argument** -- a sort is a
            !! permutation and the selection off it is exact, so 1, 8 and a build with no OpenMP
            !! at all return the same bits. It is a speed control and never an accuracy one.
        end subroutine quantile_i32
        !> `pf_quantiles` over a 32-bit integer array.
        module subroutine quantiles_i32(values, probs, out, is_valid, weights, weight_type, method, n_null, ok, &
                threads)
            integer(int32), intent(in) :: values(:) !! the population.
            real(real64), intent(in) :: probs(:)
            !! the probabilities, each on a 0-1 scale. Any outside [0, 1] aborts. Need not be sorted.
            real(real64), intent(out) :: out(:)
            !! one quantile per entry of `probs`, same size. ONE sort serves all of them.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight, non-negative. A ZERO weight removes the element from the
            !! population; a negative, NaN or infinite one aborts. Absent means every weight is 1.
            character(len=*), intent(in), optional :: weight_type
            !! "reliability" (the default) or "frequency". The two differ only when the weights are
            !! unequal: a FREQUENCY weight of 3 says the value occurred three times, so the count
            !! `ddof` is charged against is `sum(weights)`; a RELIABILITY weight says the value is
            !! that much more precise, so the count is Kish's effective size
            !! `sum(weights)**2 / sum(weights**2)`. Any other token aborts, listing both.
            character(len=*), intent(in), optional :: method
            !! how a fractional position between two order statistics is resolved, in numpy's
            !! spelling: "linear" (the default -- Hyndman-Fan type 7, what numpy and pandas do),
            !! "lower", "higher", "nearest", "midpoint", or "inverted_cdf" (a step function on the
            !! plain cumulative scale, and the only token numpy itself accepts weights with). Any
            !! other token aborts, listing all six. Matched case-insensitively.
            !! **`weight_type="frequency"` changes the default to "inverted_cdf"**, because that
            !! token IS frequency expansion; an explicit `method=` still wins.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
            integer, intent(in), optional :: threads
            !! how many threads the ORDERING may use -- this is an order statistic and has no
            !! central-moment pass, so the work `threads=` divides is `pf_argsort`'s. Absent takes
            !! the automatic rule: the `sort_threads` setting, capped by the processors
            !! actually available and by a measured work floor, and 1 inside a caller's own
            !! parallel region. **The answer does not depend on this argument** -- a sort is a
            !! permutation and the selection off it is exact, so 1, 8 and a build with no OpenMP
            !! at all return the same bits. It is a speed control and never an accuracy one.
        end subroutine quantiles_i32
        !> `pf_iqr` over a 32-bit integer array.
        module subroutine iqr_i32(values, r, is_valid, weights, weight_type, method, n_null, ok, threads)
            integer(int32), intent(in) :: values(:) !! the population.
            real(real64), intent(out) :: r
            !! the interquartile range, `q(0.75) - q(0.25)`; NaN when either quartile is.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight, non-negative. A ZERO weight removes the element from the
            !! population; a negative, NaN or infinite one aborts. Absent means every weight is 1.
            character(len=*), intent(in), optional :: weight_type
            !! "reliability" (the default) or "frequency". The two differ only when the weights are
            !! unequal: a FREQUENCY weight of 3 says the value occurred three times, so the count
            !! `ddof` is charged against is `sum(weights)`; a RELIABILITY weight says the value is
            !! that much more precise, so the count is Kish's effective size
            !! `sum(weights)**2 / sum(weights**2)`. Any other token aborts, listing both.
            character(len=*), intent(in), optional :: method
            !! how a fractional position between two order statistics is resolved, in numpy's
            !! spelling: "linear" (the default -- Hyndman-Fan type 7, what numpy and pandas do),
            !! "lower", "higher", "nearest", "midpoint", or "inverted_cdf" (a step function on the
            !! plain cumulative scale, and the only token numpy itself accepts weights with). Any
            !! other token aborts, listing all six. Matched case-insensitively.
            !! **`weight_type="frequency"` changes the default to "inverted_cdf"**, because that
            !! token IS frequency expansion; an explicit `method=` still wins.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
            integer, intent(in), optional :: threads
            !! how many threads the ORDERING may use -- this is an order statistic and has no
            !! central-moment pass, so the work `threads=` divides is `pf_argsort`'s. Absent takes
            !! the automatic rule: the `sort_threads` setting, capped by the processors
            !! actually available and by a measured work floor, and 1 inside a caller's own
            !! parallel region. **The answer does not depend on this argument** -- a sort is a
            !! permutation and the selection off it is exact, so 1, 8 and a build with no OpenMP
            !! at all return the same bits. It is a speed control and never an accuracy one.
        end subroutine iqr_i32
        !> `pf_trim_mean` over a 32-bit integer array.
        module subroutine trim_mean_i32(values, prop, m, is_valid, weights, n_null, ok, threads)
            integer(int32), intent(in) :: values(:) !! the population.
            real(real64), intent(in) :: prop
            !! the share trimmed from EACH tail, so `prop=0.1` drops 10% at each end and averages the
            !! middle 80%. Must satisfy `0 <= prop < 0.5`; anything else aborts, because a caller who asks
            !! to trim everything has made a mistake rather than expressed a preference.
            real(real64), intent(out) :: m
            !! the trimmed mean; NaN when nothing survives the trim.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight, non-negative. A ZERO weight removes the element from the
            !! population; a negative, NaN or infinite one aborts. Absent means every weight is 1.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
            integer, intent(in), optional :: threads
            !! how many threads the ORDERING may use -- this is an order statistic and has no
            !! central-moment pass, so the work `threads=` divides is `pf_argsort`'s. Absent takes
            !! the automatic rule: the `sort_threads` setting, capped by the processors
            !! actually available and by a measured work floor, and 1 inside a caller's own
            !! parallel region. **The answer does not depend on this argument** -- a sort is a
            !! permutation and the selection off it is exact, so 1, 8 and a build with no OpenMP
            !! at all return the same bits. It is a speed control and never an accuracy one.
        end subroutine trim_mean_i32
        !> `pf_percentile_of_score` over a 32-bit integer array.
        module subroutine percentile_of_score_i32(values, score, p, is_valid, weights, kind, n_null, ok, threads)
            integer(int32), intent(in) :: values(:) !! the population.
            real(real64), intent(in) :: score
            !! the value to locate. NaN or infinite aborts -- unlike a NaN in the population, which is an
            !! ordinary data condition, a NaN score can only come from the caller's own arithmetic.
            real(real64), intent(out) :: p
            !! the share of the population at or below `score`, on a 0-1 scale; NaN when empty.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight, non-negative. A ZERO weight removes the element from the
            !! population; a negative, NaN or infinite one aborts. Absent means every weight is 1.
            character(len=*), intent(in), optional :: kind
            !! which convention counts a value EQUAL to `score`, in scipy's spelling: "rank" (the
            !! default; ties count half), "weak" (a value <= score counts), "strict" (only < score)
            !! or "mean" (the average of weak and strict). Any other token aborts, listing all
            !! four. Matched case-insensitively.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
            integer, intent(in), optional :: threads
            !! how many threads the ORDERING may use -- this is an order statistic and has no
            !! central-moment pass, so the work `threads=` divides is `pf_argsort`'s. Absent takes
            !! the automatic rule: the `sort_threads` setting, capped by the processors
            !! actually available and by a measured work floor, and 1 inside a caller's own
            !! parallel region. **The answer does not depend on this argument** -- a sort is a
            !! permutation and the selection off it is exact, so 1, 8 and a build with no OpenMP
            !! at all return the same bits. It is a speed control and never an accuracy one.
        end subroutine percentile_of_score_i32
        !> `pf_mad` over a 32-bit integer array.
        module subroutine mad_i32(values, m, is_valid, weights, scale, center, n_null, ok, threads)
            integer(int32), intent(in) :: values(:) !! the population.
            real(real64), intent(out) :: m
            !! the median absolute deviation, scaled unless `scale="raw"`; NaN when the population is
            !! empty.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight, non-negative. A ZERO weight removes the element from the
            !! population; a negative, NaN or infinite one aborts. Absent means every weight is 1.
            character(len=*), intent(in), optional :: scale
            !! "normal" (the default) divides the raw deviation by `Phi^-1(3/4)`, which makes the
            !! result a consistent estimator of the standard deviation for Gaussian data --
            !! scipy's `median_abs_deviation(scale='normal')`, and what an astronomy script means
            !! by "the MAD". That is a multiplication by 1.4826022185056018, and **not** by the
            !! rounded 1.4826 the textbooks quote, which differs by 1.5e-06 relative. "raw" is the
            !! unscaled median of |x - center|. Any other token aborts, naming both. Matched
            !! case-insensitively.
            real(real64), intent(in), optional :: center
            !! the centre the deviations are taken about. Absent uses the population's own median,
            !! which is what scipy does. Supplying one skips ONE selection and not the ordering:
            !! the median of |x - center| still has to be found. A NaN or infinite centre ABORTS
            !! -- unlike a NaN value, which is an ordinary data condition, a NaN centre can only
            !! come from the caller's own arithmetic.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
            integer, intent(in), optional :: threads
            !! how many threads the ORDERING may use -- this is an order statistic and has no
            !! central-moment pass, so the work `threads=` divides is `pf_argsort`'s. Absent takes
            !! the automatic rule: the `sort_threads` setting, capped by the processors
            !! actually available and by a measured work floor, and 1 inside a caller's own
            !! parallel region. **The answer does not depend on this argument** -- a sort is a
            !! permutation and the selection off it is exact, so 1, 8 and a build with no OpenMP
            !! at all return the same bits. It is a speed control and never an accuracy one.
        end subroutine mad_i32
        !> `pf_describe` over a 32-bit integer array.
        module subroutine describe_i32(values, s, is_valid, weights, weight_type, ok, threads)
            integer(int32), intent(in) :: values(:) !! the population.
            type(pf_stats), intent(out) :: s
            !! the filled summary object: tier A computed and tier B already ordered, so every query on it
            !! afterwards is a read.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight, non-negative. A ZERO weight removes the element from the
            !! population; a negative, NaN or infinite one aborts. Absent means every weight is 1.
            character(len=*), intent(in), optional :: weight_type
            !! "reliability" (the default) or "frequency". The two differ only when the weights are
            !! unequal: a FREQUENCY weight of 3 says the value occurred three times, so the count
            !! `ddof` is charged against is `sum(weights)`; a RELIABILITY weight says the value is
            !! that much more precise, so the count is Kish's effective size
            !! `sum(weights)**2 / sum(weights**2)`. Any other token aborts, listing both.
            logical, intent(out), optional :: ok
            !! .false. when the object describes a population with no defined moments -- one that
            !! is empty after the exclusions, or that kept a NaN under `skipnan = .false.` In
            !! either case every tier-A query on `s` answers a quiet NaN. The object itself
            !! carries the counts (`%n_valid()`, `%n_null()`, `%n_nan()`), so this is the one
            !! thing about the result a caller cannot otherwise read without picking a statistic
            !! and NaN-testing it by hand.
            integer, intent(in), optional :: threads
            !! how many threads the central-moment pass and the ordering may use -- `pf_describe`
            !! does both, which no other entry point in this module does. Absent takes the
            !! automatic rule: the `sort_threads` setting, capped by the processors
            !! actually available and by a measured work floor, and 1 inside a caller's own
            !! parallel region. **The answer does not depend on this argument** -- the block
            !! decomposition is a function of the population size alone and a sort is a
            !! permutation, so 1, 8 and a build with no OpenMP at all return the same bits. It is
            !! a speed control and never an accuracy one.
        end subroutine describe_i32
        !> `pf_cov` over a 32-bit integer array.
        module subroutine cov_i32(x, y, c, is_valid, weights, weight_type, ddof, n_null, n_nan, ok)
            integer(int32), intent(in) :: x(:) !! the first sample.
            integer(int32), intent(in) :: y(:) !! the second sample, element for element.
            real(real64), intent(out) :: c !! the covariance; NaN when `n_valid <= ddof`.
            logical, intent(in), optional :: is_valid(:)
            !! per PAIR: .false. marks the pair as unusable. One mask over both arrays, because a
            !! two-sample statistic is taken over pairs; a caller holding a separate mask for each
            !! column passes `mask_x .and. mask_y`, which says what it does. A NaN on either side
            !! drops the pair as well, and is counted through `n_nan` rather than here.
            real(real64), intent(in), optional :: weights(:)
            !! per PAIR weight, non-negative. A ZERO weight removes the pair from the population;
            !! a negative, NaN or infinite one aborts. Absent means every weight is 1. Pearson
            !! only -- see `method`.
            character(len=*), intent(in), optional :: weight_type
            !! "reliability" (the default) or "frequency". The two differ only when the weights are
            !! unequal: a FREQUENCY weight of 3 says the value occurred three times, so the count
            !! `ddof` is charged against is `sum(weights)`; a RELIABILITY weight says the value is
            !! that much more precise, so the count is Kish's effective size
            !! `sum(weights)**2 / sum(weights**2)`. Any other token aborts, listing both.
            integer, intent(in), optional :: ddof
            !! delta degrees of freedom; **1 by default** -- the sample variance, as pandas
            !! returns. numpy's default is 0, so pass `ddof=0` to reproduce `np.var`/`np.std`.
            !! A plain default-kind integer deliberately: it is bounded by the population size,
            !! so there is no int64 form to provide. `ddof >= n_valid` gives NaN, not a division.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            integer(int64), intent(out), optional :: n_nan
            !! how many were excluded as NaN and were not already null.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
        end subroutine cov_i32
        !> `pf_corr` over a 32-bit integer array.
        module subroutine corr_i32(x, y, r, is_valid, weights, method, n_null, n_nan, ok)
            integer(int32), intent(in) :: x(:) !! the first sample.
            integer(int32), intent(in) :: y(:) !! the second sample, element for element.
            real(real64), intent(out) :: r !! the correlation in [-1, 1]; NaN when either sample is constant.
            logical, intent(in), optional :: is_valid(:)
            !! per PAIR: .false. marks the pair as unusable. One mask over both arrays, because a
            !! two-sample statistic is taken over pairs; a caller holding a separate mask for each
            !! column passes `mask_x .and. mask_y`, which says what it does. A NaN on either side
            !! drops the pair as well, and is counted through `n_nan` rather than here.
            real(real64), intent(in), optional :: weights(:)
            !! per PAIR weight, non-negative. A ZERO weight removes the pair from the population;
            !! a negative, NaN or infinite one aborts. Absent means every weight is 1. Pearson
            !! only -- see `method`.
            character(len=*), intent(in), optional :: method
            !! "pearson" (the default) or "spearman". Spearman is Pearson over MIDRANKS: each run
            !! of equal values receives the mean of the sorted positions it spans, so it measures
            !! any monotone relationship rather than a linear one. Any other token aborts, naming
            !! both. Matched case-insensitively. **`weights` with "spearman" aborts** -- a
            !! weighted midrank is a further definitional choice and no reference library makes
            !! it.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            integer(int64), intent(out), optional :: n_nan
            !! how many were excluded as NaN and were not already null.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
        end subroutine corr_i32
        !> `pf_zscore` over a 32-bit integer array.
        module subroutine zscore_i32(values, z, is_valid, ddof, out_valid, n_null, ok)
            integer(int32), intent(in) :: values(:) !! the population.
            real(real64), intent(out) :: z(:)
            !! the standardised values, same size as `values`. An excluded
            !! element is a quiet NaN unless `out_valid` is present.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            integer, intent(in), optional :: ddof
            !! delta degrees of freedom; **1 by default** -- the sample variance, as pandas
            !! returns. numpy's default is 0, so pass `ddof=0` to reproduce `np.var`/`np.std`.
            !! A plain default-kind integer deliberately: it is bounded by the population size,
            !! so there is no int64 form to provide. `ddof >= n_valid` gives NaN, not a division.
            logical, intent(out), optional :: out_valid(:)
            !! per output element: .false. where the input was excluded and the result is
            !! therefore undefined. Same size as `values`. **Absent, such an element is written as
            !! a quiet NaN instead** -- which is loud in practice, since it propagates through
            !! whatever the caller does next.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
        end subroutine zscore_i32
        !> `pf_normal_scores` over a 32-bit integer array.
        module subroutine normal_scores_i32(values, s, is_valid, method, out_valid, n_null, ok, threads)
            integer(int32), intent(in) :: values(:) !! the population.
            real(real64), intent(out) :: s(:)
            !! the normal scores, same size as `values`, in the caller's
            !! original order. An excluded element is a quiet NaN unless
            !! `out_valid` is present.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            character(len=*), intent(in), optional :: method
            !! the PLOTTING POSITION: which point of the distribution a rank is taken to mark.
            !! `s(i) = Phi**(-1)((r - a)/(m + 1 - 2a))` for a midrank `r` among `m` survivors,
            !! with `a` given by the token: "blom" (the default, `a = 3/8`, the standard rankit
            !! and what R's `qqnorm` uses), "weibull" (`a = 0`, `r/(m+1)`, the van der Waerden
            !! score), "tukey" (`a = 1/3`), "hazen" (`a = 1/2`) or "cunnane" (`a = 0.4`).
            !! "filliben" is the MEDIAN rank and not of that family at all: `1 - 0.5**(1/m)` at
            !! the first position, `0.5**(1/m)` at the last and `(i - 0.3175)/(m + 0.365)`
            !! between. Any other token aborts, naming all six. Matched case-insensitively.
            logical, intent(out), optional :: out_valid(:)
            !! per output element: .false. where the input was excluded and the element therefore
            !! has no score. Same size as `values`. **Absent, such an element is written as a
            !! quiet NaN instead** -- which is loud in practice, since it propagates through
            !! whatever the caller does next.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
            integer, intent(in), optional :: threads
            !! how many threads the midrank ordering may use, passed straight to `pf_argsort`.
            !! Absent takes the automatic rule. **The answer does not depend on this argument**:
            !! ties take the mean of the positions they span, so no tie-breaking the sort might
            !! choose can reach the result. It is a speed control and never an accuracy one.
        end subroutine normal_scores_i32
        !> `pf_sigma_clipped_stats` over a 32-bit integer array.
        module subroutine sigma_clipped_stats_i32(values, mean, median, stddev, sigma, sigma_lower, sigma_upper, maxiters, &
                cenfunc, stdfunc, n_clipped, keep, converged, is_valid, &
                n_null, ok, threads)
            integer(int32), intent(in) :: values(:) !! the population.
            real(real64), intent(out) :: mean !! the mean of the surviving values; NaN when none survive.
            real(real64), intent(out) :: median !! their median.
            real(real64), intent(out) :: stddev !! their POPULATION standard deviation (`ddof = 0`), as astropy reports.
            real(real64), intent(in), optional :: sigma
            !! how many scale units a value may sit from the centre and still be kept. 3.0 by
            !! default, as astropy does. `sigma_lower` and `sigma_upper` each default to this, so
            !! setting it alone gives a symmetric clip.
            real(real64), intent(in), optional :: sigma_lower
            !! the lower half of the clip, overriding `sigma` below the centre.
            real(real64), intent(in), optional :: sigma_upper
            !! the upper half of the clip, overriding `sigma` above the centre.
            integer, intent(in), optional :: maxiters
            !! how many clipping rounds to run; 5 by default, as astropy does. **A non-positive
            !! value means iterate until nothing more is removed**, which is astropy's
            !! `maxiters=None` -- capped at a hard internal 100 rounds, and reaching that cap
            !! STOPS and reports through `converged` rather than aborting. Each non-terminating
            !! round removes at least one point, so a large catalogue can honestly want more
            !! rounds than any small constant.
            character(len=*), intent(in), optional :: cenfunc
            !! which centre each round clips about: "median" (the default, as astropy) or "mean".
            !! Any other token aborts, naming both. Matched case-insensitively.
            character(len=*), intent(in), optional :: stdfunc
            !! which scale each round measures: "std" (the default, as astropy -- the sample
            !! standard deviation at `ddof = 1`) or "mad_std", the normal-scaled median absolute
            !! deviation, which is far less disturbed by the outliers being clipped. Any other
            !! token aborts, naming both. Matched case-insensitively.
            integer(int64), intent(out), optional :: n_clipped
            !! how many elements of the population the clipping removed. `0` for an empty
            !! population, and `0` when the first round removes nothing.
            logical, intent(out), optional :: keep(:)
            !! per element of `values`: .true. for one that survived both the exclusion rules and
            !! every clipping round. Same size as `values`. This is what a caller needs in order
            !! to apply the same clip to a SECOND column.
            logical, intent(out), optional :: converged
            !! .false. when the iteration stopped at its round limit with points still being
            !! removed, rather than because a round removed nothing. The answer is still usable
            !! -- it is the last completed round's -- so `ok` stays .true. and this is the only
            !! way to tell the two endings apart.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
            integer, intent(in), optional :: threads
            !! how many threads the ORDERING may use -- this is an order statistic and has no
            !! central-moment pass, so the work `threads=` divides is `pf_argsort`'s. Absent takes
            !! the automatic rule: the `sort_threads` setting, capped by the processors
            !! actually available and by a measured work floor, and 1 inside a caller's own
            !! parallel region. **The answer does not depend on this argument** -- a sort is a
            !! permutation and the selection off it is exact, so 1, 8 and a build with no OpenMP
            !! at all return the same bits. It is a speed control and never an accuracy one.
        end subroutine sigma_clipped_stats_i32
        !> `pf_cumsum` over a 32-bit integer array.
        module subroutine cumsum_i32(values, out, is_valid, out_valid, n_null, ok)
            integer(int32), intent(in) :: values(:) !! the population.
            real(real64), intent(out) :: out(:)
            !! the running sum, same size as `values`. An excluded element is a quiet NaN unless
            !! `out_valid` is present.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            logical, intent(out), optional :: out_valid(:)
            !! per output element: .false. where the input was excluded and the result is
            !! therefore undefined. Same size as `values`. **Absent, such an element is written as
            !! a quiet NaN instead** -- which is loud in practice, since it propagates through
            !! whatever the caller does next.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
        end subroutine cumsum_i32
        !> `pf_cumprod` over a 32-bit integer array.
        module subroutine cumprod_i32(values, out, is_valid, out_valid, n_null, ok)
            integer(int32), intent(in) :: values(:) !! the population.
            real(real64), intent(out) :: out(:)
            !! the running product, same size as `values`. An excluded element is a quiet NaN unless
            !! `out_valid` is present.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            logical, intent(out), optional :: out_valid(:)
            !! per output element: .false. where the input was excluded and the result is
            !! therefore undefined. Same size as `values`. **Absent, such an element is written as
            !! a quiet NaN instead** -- which is loud in practice, since it propagates through
            !! whatever the caller does next.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
        end subroutine cumprod_i32
        !> `pf_cummax` over a 32-bit integer array.
        module subroutine cummax_i32(values, out, is_valid, out_valid, n_null, ok)
            integer(int32), intent(in) :: values(:) !! the population.
            real(real64), intent(out) :: out(:)
            !! the running maximum, same size as `values`. An excluded element is a quiet NaN unless
            !! `out_valid` is present.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            logical, intent(out), optional :: out_valid(:)
            !! per output element: .false. where the input was excluded and the result is
            !! therefore undefined. Same size as `values`. **Absent, such an element is written as
            !! a quiet NaN instead** -- which is loud in practice, since it propagates through
            !! whatever the caller does next.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
        end subroutine cummax_i32
        !> `pf_cummin` over a 32-bit integer array.
        module subroutine cummin_i32(values, out, is_valid, out_valid, n_null, ok)
            integer(int32), intent(in) :: values(:) !! the population.
            real(real64), intent(out) :: out(:)
            !! the running minimum, same size as `values`. An excluded element is a quiet NaN unless
            !! `out_valid` is present.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            logical, intent(out), optional :: out_valid(:)
            !! per output element: .false. where the input was excluded and the result is
            !! therefore undefined. Same size as `values`. **Absent, such an element is written as
            !! a quiet NaN instead** -- which is loud in practice, since it propagates through
            !! whatever the caller does next.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
        end subroutine cummin_i32
        !> `pf_bucketize` over a 32-bit integer array.
        module subroutine bucketize_i32(values, edges, codes, right, is_valid, weights, n_null, n_outside, ok)
            integer(int32), intent(in) :: values(:) !! the population.
            real(real64), intent(in) :: edges(:) !! the bin boundaries, strictly increasing, at least two.
            integer(int32), intent(out) :: codes(:)
            !! the 1-based bin of each value, or 0 for one that joined none. Same size as `values`.
            logical, intent(in), optional :: right
            !! which side of a bin is closed. `.false.` by default, which is numpy's rule and the
            !! one `np.histogram`/`np.digitize` use: bin k is `[edges(k), edges(k+1))`, and the
            !! LAST bin closes at the top so that `edges(nbins+1)` itself lands in it. `.true.`
            !! mirrors it, which is `pd.cut(right=True, include_lowest=True)`: bin k is
            !! `(edges(k), edges(k+1)]`, and the FIRST bin closes at the bottom. Either way the
            !! closed range `[edges(1), edges(nbins+1)]` is covered exactly once, so the two
            !! conventions differ only in which bin a value sitting exactly on an interior edge
            !! joins.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight, non-negative. A ZERO weight removes the element from the
            !! population; a negative, NaN or infinite one aborts. Absent means every weight is 1.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            integer(int64), intent(out), optional :: n_outside
            !! how many usable elements fell outside `[edges(1), edges(nbins+1)]` and so joined no
            !! bin. This is an ordinary data condition rather than an error -- `np.histogram`
            !! drops such values silently -- but it is the one thing a caller cannot recover from
            !! the output, so it is reported. A NaN under `skipnan = .false.` is counted here: it
            !! is then a value, and it matches no bin.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
        end subroutine bucketize_i32
        !> `pf_histogram` over a 32-bit integer array.
        module subroutine histogram_i32(values, edges, counts, right, density, is_valid, weights, n_null, n_outside, &
                ok)
            integer(int32), intent(in) :: values(:) !! the population.
            real(real64), intent(in) :: edges(:) !! the bin boundaries, strictly increasing, at least two.
            real(real64), intent(out) :: counts(:)
            !! one entry per bin, so `size(edges) - 1` of them.
            logical, intent(in), optional :: right
            !! which side of a bin is closed. `.false.` by default, which is numpy's rule and the
            !! one `np.histogram`/`np.digitize` use: bin k is `[edges(k), edges(k+1))`, and the
            !! LAST bin closes at the top so that `edges(nbins+1)` itself lands in it. `.true.`
            !! mirrors it, which is `pd.cut(right=True, include_lowest=True)`: bin k is
            !! `(edges(k), edges(k+1)]`, and the FIRST bin closes at the bottom. Either way the
            !! closed range `[edges(1), edges(nbins+1)]` is covered exactly once, so the two
            !! conventions differ only in which bin a value sitting exactly on an interior edge
            !! joins.
            logical, intent(in), optional :: density
            !! `.false.` by default. `.true.` divides each bin by its own WIDTH and by the total
            !! that was binned, giving a probability density -- `np.histogram(density=True)`, so
            !! `sum(counts * widths)` is 1 and an uneven edge spacing is accounted for. The
            !! normalisation base is what actually landed in a bin: values outside the edges were
            !! never counted, so they do not appear in it, which is numpy's rule too. When nothing
            !! was binned the density is undefined and every entry is a **quiet NaN** with
            !! `ok = .false.` -- note that an empty HISTOGRAM is perfectly well defined (all
            !! zeros, `ok = .true.`) and an empty DENSITY is not.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight, non-negative. A ZERO weight removes the element from the
            !! population; a negative, NaN or infinite one aborts. Absent means every weight is 1.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            integer(int64), intent(out), optional :: n_outside
            !! how many usable elements fell outside `[edges(1), edges(nbins+1)]` and so joined no
            !! bin. This is an ordinary data condition rather than an error -- `np.histogram`
            !! drops such values silently -- but it is the one thing a caller cannot recover from
            !! the output, so it is reported. A NaN under `skipnan = .false.` is counted here: it
            !! is then a value, and it matches no bin.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
        end subroutine histogram_i32
        !> `pf_bin_edges` over a 32-bit integer array.
        module subroutine bin_edges_i32(values, nbins, edges, is_valid, weights, n_null, ok)
            integer(int32), intent(in) :: values(:) !! the population.
            integer, intent(in) :: nbins !! how many bins to describe; at least 1.
            real(real64), intent(out) :: edges(:)
            !! the `nbins + 1` boundaries, strictly increasing.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight, non-negative. A ZERO weight removes the element from the
            !! population; a negative, NaN or infinite one aborts. Absent means every weight is 1.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
        end subroutine bin_edges_i32
        !> `pf_sum` over a 64-bit integer array.
        !>
        !> **An `integer(int64)` above 2**53 loses exactness** on the way into the real64 buffer
        !> every statistic here is computed in. That is unavoidable for a mean and irrelevant for
        !> the workloads this serves -- nobody averages an object ID -- but it is NOT irrelevant
        !> for a median, which `pf_nth_quantile` computes exactly on the original array.
        module subroutine sum_i64(values, s, is_valid, weights, n_null, ok, threads)
            integer(int64), intent(in) :: values(:) !! the population.
            real(real64), intent(out) :: s !! the sum.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight, non-negative. A ZERO weight removes the element from the
            !! population; a negative, NaN or infinite one aborts. Absent means every weight is 1.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
            integer, intent(in), optional :: threads
            !! how many threads the central-moment pass may use. Absent takes the automatic rule:
            !! the `sort_threads` setting, capped by the processors actually available and
            !! by a measured work floor, and 1 inside a caller's own parallel region. **The answer
            !! does not depend on this argument** -- the block decomposition is a function of the
            !! population size alone, so 1, 8 and a build with no OpenMP at all return the same
            !! bits. It is a speed control and never an accuracy one.
        end subroutine sum_i64
        !> `pf_mean` over a 64-bit integer array.
        !>
        !> **An `integer(int64)` above 2**53 loses exactness** on the way into the real64 buffer
        !> every statistic here is computed in. That is unavoidable for a mean and irrelevant for
        !> the workloads this serves -- nobody averages an object ID -- but it is NOT irrelevant
        !> for a median, which `pf_nth_quantile` computes exactly on the original array.
        module subroutine mean_i64(values, m, is_valid, weights, n_null, ok, threads)
            integer(int64), intent(in) :: values(:) !! the population.
            real(real64), intent(out) :: m !! the mean; NaN when the population is empty.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight, non-negative. A ZERO weight removes the element from the
            !! population; a negative, NaN or infinite one aborts. Absent means every weight is 1.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
            integer, intent(in), optional :: threads
            !! how many threads the central-moment pass may use. Absent takes the automatic rule:
            !! the `sort_threads` setting, capped by the processors actually available and
            !! by a measured work floor, and 1 inside a caller's own parallel region. **The answer
            !! does not depend on this argument** -- the block decomposition is a function of the
            !! population size alone, so 1, 8 and a build with no OpenMP at all return the same
            !! bits. It is a speed control and never an accuracy one.
        end subroutine mean_i64
        !> `pf_gmean` over a 64-bit integer array.
        !>
        !> **An `integer(int64)` above 2**53 loses exactness** on the way into the real64 buffer
        !> every statistic here is computed in. That is unavoidable for a mean and irrelevant for
        !> the workloads this serves -- nobody averages an object ID -- but it is NOT irrelevant
        !> for a median, which `pf_nth_quantile` computes exactly on the original array.
        module subroutine gmean_i64(values, g, is_valid, weights, n_null, ok, threads)
            integer(int64), intent(in) :: values(:) !! the population.
            real(real64), intent(out) :: g !! the geometric mean; 0 for a zero, NaN for a negative or empty population.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight, non-negative. A ZERO weight removes the element from the
            !! population; a negative, NaN or infinite one aborts. Absent means every weight is 1.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
            integer, intent(in), optional :: threads
            !! how many threads the central-moment pass may use. Absent takes the automatic rule:
            !! the `sort_threads` setting, capped by the processors actually available and
            !! by a measured work floor, and 1 inside a caller's own parallel region. **The answer
            !! does not depend on this argument** -- the block decomposition is a function of the
            !! population size alone, so 1, 8 and a build with no OpenMP at all return the same
            !! bits. It is a speed control and never an accuracy one.
        end subroutine gmean_i64
        !> `pf_hmean` over a 64-bit integer array.
        !>
        !> **An `integer(int64)` above 2**53 loses exactness** on the way into the real64 buffer
        !> every statistic here is computed in. That is unavoidable for a mean and irrelevant for
        !> the workloads this serves -- nobody averages an object ID -- but it is NOT irrelevant
        !> for a median, which `pf_nth_quantile` computes exactly on the original array.
        module subroutine hmean_i64(values, h, is_valid, weights, n_null, ok, threads)
            integer(int64), intent(in) :: values(:) !! the population.
            real(real64), intent(out) :: h !! the harmonic mean; 0 for a zero, NaN for a negative or empty population.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight, non-negative. A ZERO weight removes the element from the
            !! population; a negative, NaN or infinite one aborts. Absent means every weight is 1.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
            integer, intent(in), optional :: threads
            !! how many threads the central-moment pass may use. Absent takes the automatic rule:
            !! the `sort_threads` setting, capped by the processors actually available and
            !! by a measured work floor, and 1 inside a caller's own parallel region. **The answer
            !! does not depend on this argument** -- the block decomposition is a function of the
            !! population size alone, so 1, 8 and a build with no OpenMP at all return the same
            !! bits. It is a speed control and never an accuracy one.
        end subroutine hmean_i64
        !> `pf_variance` over a 64-bit integer array.
        !>
        !> **An `integer(int64)` above 2**53 loses exactness** on the way into the real64 buffer
        !> every statistic here is computed in. That is unavoidable for a mean and irrelevant for
        !> the workloads this serves -- nobody averages an object ID -- but it is NOT irrelevant
        !> for a median, which `pf_nth_quantile` computes exactly on the original array.
        module subroutine variance_i64(values, v, is_valid, weights, weight_type, ddof, n_null, ok, threads)
            integer(int64), intent(in) :: values(:) !! the population.
            real(real64), intent(out) :: v !! the variance; NaN when `n_valid <= ddof`.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight, non-negative. A ZERO weight removes the element from the
            !! population; a negative, NaN or infinite one aborts. Absent means every weight is 1.
            character(len=*), intent(in), optional :: weight_type
            !! "reliability" (the default) or "frequency". The two differ only when the weights are
            !! unequal: a FREQUENCY weight of 3 says the value occurred three times, so the count
            !! `ddof` is charged against is `sum(weights)`; a RELIABILITY weight says the value is
            !! that much more precise, so the count is Kish's effective size
            !! `sum(weights)**2 / sum(weights**2)`. Any other token aborts, listing both.
            integer, intent(in), optional :: ddof
            !! delta degrees of freedom; **1 by default** -- the sample variance, as pandas
            !! returns. numpy's default is 0, so pass `ddof=0` to reproduce `np.var`/`np.std`.
            !! A plain default-kind integer deliberately: it is bounded by the population size,
            !! so there is no int64 form to provide. `ddof >= n_valid` gives NaN, not a division.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
            integer, intent(in), optional :: threads
            !! how many threads the central-moment pass may use. Absent takes the automatic rule:
            !! the `sort_threads` setting, capped by the processors actually available and
            !! by a measured work floor, and 1 inside a caller's own parallel region. **The answer
            !! does not depend on this argument** -- the block decomposition is a function of the
            !! population size alone, so 1, 8 and a build with no OpenMP at all return the same
            !! bits. It is a speed control and never an accuracy one.
        end subroutine variance_i64
        !> `pf_stddev` over a 64-bit integer array.
        !>
        !> **An `integer(int64)` above 2**53 loses exactness** on the way into the real64 buffer
        !> every statistic here is computed in. That is unavoidable for a mean and irrelevant for
        !> the workloads this serves -- nobody averages an object ID -- but it is NOT irrelevant
        !> for a median, which `pf_nth_quantile` computes exactly on the original array.
        module subroutine stddev_i64(values, sd, is_valid, weights, weight_type, ddof, n_null, ok, threads)
            integer(int64), intent(in) :: values(:) !! the population.
            real(real64), intent(out) :: sd !! the standard deviation; NaN when the variance is.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight, non-negative. A ZERO weight removes the element from the
            !! population; a negative, NaN or infinite one aborts. Absent means every weight is 1.
            character(len=*), intent(in), optional :: weight_type
            !! "reliability" (the default) or "frequency". The two differ only when the weights are
            !! unequal: a FREQUENCY weight of 3 says the value occurred three times, so the count
            !! `ddof` is charged against is `sum(weights)`; a RELIABILITY weight says the value is
            !! that much more precise, so the count is Kish's effective size
            !! `sum(weights)**2 / sum(weights**2)`. Any other token aborts, listing both.
            integer, intent(in), optional :: ddof
            !! delta degrees of freedom; **1 by default** -- the sample variance, as pandas
            !! returns. numpy's default is 0, so pass `ddof=0` to reproduce `np.var`/`np.std`.
            !! A plain default-kind integer deliberately: it is bounded by the population size,
            !! so there is no int64 form to provide. `ddof >= n_valid` gives NaN, not a division.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
            integer, intent(in), optional :: threads
            !! how many threads the central-moment pass may use. Absent takes the automatic rule:
            !! the `sort_threads` setting, capped by the processors actually available and
            !! by a measured work floor, and 1 inside a caller's own parallel region. **The answer
            !! does not depend on this argument** -- the block decomposition is a function of the
            !! population size alone, so 1, 8 and a build with no OpenMP at all return the same
            !! bits. It is a speed control and never an accuracy one.
        end subroutine stddev_i64
        !> `pf_sem` over a 64-bit integer array.
        !>
        !> **An `integer(int64)` above 2**53 loses exactness** on the way into the real64 buffer
        !> every statistic here is computed in. That is unavoidable for a mean and irrelevant for
        !> the workloads this serves -- nobody averages an object ID -- but it is NOT irrelevant
        !> for a median, which `pf_nth_quantile` computes exactly on the original array.
        module subroutine sem_i64(values, se, is_valid, weights, weight_type, ddof, n_null, ok, threads)
            integer(int64), intent(in) :: values(:) !! the population.
            real(real64), intent(out) :: se !! the standard error; NaN when the standard deviation is.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight, non-negative. A ZERO weight removes the element from the
            !! population; a negative, NaN or infinite one aborts. Absent means every weight is 1.
            character(len=*), intent(in), optional :: weight_type
            !! "reliability" (the default) or "frequency". The two differ only when the weights are
            !! unequal: a FREQUENCY weight of 3 says the value occurred three times, so the count
            !! `ddof` is charged against is `sum(weights)`; a RELIABILITY weight says the value is
            !! that much more precise, so the count is Kish's effective size
            !! `sum(weights)**2 / sum(weights**2)`. Any other token aborts, listing both.
            integer, intent(in), optional :: ddof
            !! delta degrees of freedom; **1 by default** -- the sample variance, as pandas
            !! returns. numpy's default is 0, so pass `ddof=0` to reproduce `np.var`/`np.std`.
            !! A plain default-kind integer deliberately: it is bounded by the population size,
            !! so there is no int64 form to provide. `ddof >= n_valid` gives NaN, not a division.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
            integer, intent(in), optional :: threads
            !! how many threads the central-moment pass may use. Absent takes the automatic rule:
            !! the `sort_threads` setting, capped by the processors actually available and
            !! by a measured work floor, and 1 inside a caller's own parallel region. **The answer
            !! does not depend on this argument** -- the block decomposition is a function of the
            !! population size alone, so 1, 8 and a build with no OpenMP at all return the same
            !! bits. It is a speed control and never an accuracy one.
        end subroutine sem_i64
        !> `pf_skewness` over a 64-bit integer array.
        !>
        !> **An `integer(int64)` above 2**53 loses exactness** on the way into the real64 buffer
        !> every statistic here is computed in. That is unavoidable for a mean and irrelevant for
        !> the workloads this serves -- nobody averages an object ID -- but it is NOT irrelevant
        !> for a median, which `pf_nth_quantile` computes exactly on the original array.
        module subroutine skewness_i64(values, g, is_valid, weights, weight_type, bias, n_null, ok, threads)
            integer(int64), intent(in) :: values(:) !! the population.
            real(real64), intent(out) :: g !! the skewness.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight, non-negative. A ZERO weight removes the element from the
            !! population; a negative, NaN or infinite one aborts. Absent means every weight is 1.
            character(len=*), intent(in), optional :: weight_type
            !! "reliability" (the default) or "frequency". The two differ only when the weights are
            !! unequal: a FREQUENCY weight of 3 says the value occurred three times, so the count
            !! `ddof` is charged against is `sum(weights)`; a RELIABILITY weight says the value is
            !! that much more precise, so the count is Kish's effective size
            !! `sum(weights)**2 / sum(weights**2)`. Any other token aborts, listing both.
            logical, intent(in), optional :: bias
            !! .false. by default: the bias-CORRECTED G1/G2, which is what pandas returns.
            !! .true. gives the uncorrected g1/g2, which is what scipy returns by default.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
            integer, intent(in), optional :: threads
            !! how many threads the central-moment pass may use. Absent takes the automatic rule:
            !! the `sort_threads` setting, capped by the processors actually available and
            !! by a measured work floor, and 1 inside a caller's own parallel region. **The answer
            !! does not depend on this argument** -- the block decomposition is a function of the
            !! population size alone, so 1, 8 and a build with no OpenMP at all return the same
            !! bits. It is a speed control and never an accuracy one.
        end subroutine skewness_i64
        !> `pf_kurtosis` over a 64-bit integer array.
        !>
        !> **An `integer(int64)` above 2**53 loses exactness** on the way into the real64 buffer
        !> every statistic here is computed in. That is unavoidable for a mean and irrelevant for
        !> the workloads this serves -- nobody averages an object ID -- but it is NOT irrelevant
        !> for a median, which `pf_nth_quantile` computes exactly on the original array.
        module subroutine kurtosis_i64(values, k, is_valid, weights, weight_type, bias, excess, n_null, ok, threads)
            integer(int64), intent(in) :: values(:) !! the population.
            real(real64), intent(out) :: k !! the kurtosis, excess by default.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight, non-negative. A ZERO weight removes the element from the
            !! population; a negative, NaN or infinite one aborts. Absent means every weight is 1.
            character(len=*), intent(in), optional :: weight_type
            !! "reliability" (the default) or "frequency". The two differ only when the weights are
            !! unequal: a FREQUENCY weight of 3 says the value occurred three times, so the count
            !! `ddof` is charged against is `sum(weights)`; a RELIABILITY weight says the value is
            !! that much more precise, so the count is Kish's effective size
            !! `sum(weights)**2 / sum(weights**2)`. Any other token aborts, listing both.
            logical, intent(in), optional :: bias
            !! .false. by default: the bias-CORRECTED G1/G2, which is what pandas returns.
            !! .true. gives the uncorrected g1/g2, which is what scipy returns by default.
            logical, intent(in), optional :: excess
            !! .true. by default: EXCESS kurtosis, which is 0 for a normal population.
            !! .false. adds 3 back, giving the raw fourth-moment ratio.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
            integer, intent(in), optional :: threads
            !! how many threads the central-moment pass may use. Absent takes the automatic rule:
            !! the `sort_threads` setting, capped by the processors actually available and
            !! by a measured work floor, and 1 inside a caller's own parallel region. **The answer
            !! does not depend on this argument** -- the block decomposition is a function of the
            !! population size alone, so 1, 8 and a build with no OpenMP at all return the same
            !! bits. It is a speed control and never an accuracy one.
        end subroutine kurtosis_i64
        !> `pf_moments` over a 64-bit integer array: every tier-A quantity in one pair of passes.
        !>
        !> **An `integer(int64)` above 2**53 loses exactness** on the way into the real64 buffer
        !> every statistic here is computed in. That is unavoidable for a mean and irrelevant for
        !> the workloads this serves -- nobody averages an object ID -- but it is NOT irrelevant
        !> for a median, which `pf_nth_quantile` computes exactly on the original array.
        module subroutine moments_i64(values, n_valid, mean, variance, stddev, sem, skewness, kurtosis, vsum, vmin, &
                vmax, is_valid, weights, weight_type, ddof, bias, excess, &
                n_null, ok, threads)
            integer(int64), intent(in) :: values(:) !! the population.
            integer(int64), intent(out), optional :: n_valid !! how many elements were used.
            real(real64), intent(out), optional :: mean !! the mean; NaN when empty.
            real(real64), intent(out), optional :: variance !! the variance; NaN when undefined.
            real(real64), intent(out), optional :: stddev !! the standard deviation.
            real(real64), intent(out), optional :: sem !! the standard error of the mean.
            real(real64), intent(out), optional :: skewness !! the third standardised moment.
            real(real64), intent(out), optional :: kurtosis !! the fourth, excess by default.
            real(real64), intent(out), optional :: vsum !! the pairwise sum; 0 when empty.
            real(real64), intent(out), optional :: vmin !! the smallest value; NaN when empty.
            real(real64), intent(out), optional :: vmax !! the largest value; NaN when empty.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight, non-negative. A ZERO weight removes the element from the
            !! population; a negative, NaN or infinite one aborts. Absent means every weight is 1.
            character(len=*), intent(in), optional :: weight_type
            !! "reliability" (the default) or "frequency". The two differ only when the weights are
            !! unequal: a FREQUENCY weight of 3 says the value occurred three times, so the count
            !! `ddof` is charged against is `sum(weights)`; a RELIABILITY weight says the value is
            !! that much more precise, so the count is Kish's effective size
            !! `sum(weights)**2 / sum(weights**2)`. Any other token aborts, listing both.
            integer, intent(in), optional :: ddof
            !! delta degrees of freedom; **1 by default** -- the sample variance, as pandas
            !! returns. numpy's default is 0, so pass `ddof=0` to reproduce `np.var`/`np.std`.
            !! A plain default-kind integer deliberately: it is bounded by the population size,
            !! so there is no int64 form to provide. `ddof >= n_valid` gives NaN, not a division.
            logical, intent(in), optional :: bias
            !! .false. by default: the bias-CORRECTED G1/G2, which is what pandas returns.
            !! .true. gives the uncorrected g1/g2, which is what scipy returns by default.
            logical, intent(in), optional :: excess
            !! .true. by default: EXCESS kurtosis, which is 0 for a normal population.
            !! .false. adds 3 back, giving the raw fourth-moment ratio.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            logical, intent(out), optional :: ok
            !! .false. when any output that was ASKED for came back a quiet NaN -- an empty
            !! population, a `ddof` that leaves nothing to divide by, a constant population for
            !! the shape statistics, or a non-finite value under `skipnan = .false.`. Only the
            !! PRESENT outputs are tested, which is the only reading that works here: `vsum` over
            !! an empty population is a correct `0`, and testing an output the caller never asked
            !! for would report a failure that is not one. `n_valid`, `n_null` and `n_nan` are
            !! counts and are always defined, so they are not tested.
            !!
            !! An infinity is NOT a failure: `mean` answers `+Inf` over a population containing
            !! one, exactly as `pf_mean` and numpy do, and `ok` stays .true. for it while the
            !! variance -- genuinely a NaN there -- turns it .false. if it was asked for.
            integer, intent(in), optional :: threads
            !! how many threads the central-moment pass may use. Absent takes the automatic rule:
            !! the `sort_threads` setting, capped by the processors actually available and
            !! by a measured work floor, and 1 inside a caller's own parallel region. **The answer
            !! does not depend on this argument** -- the block decomposition is a function of the
            !! population size alone, so 1, 8 and a build with no OpenMP at all return the same
            !! bits. It is a speed control and never an accuracy one.
        end subroutine moments_i64
        !> `pf_median` over a 64-bit integer array.
        !>
        !> **An `integer(int64)` above 2**53 loses exactness** on the way into the real64 buffer
        !> every statistic here is computed in. That is unavoidable for a mean and irrelevant for
        !> the workloads this serves -- nobody averages an object ID -- but it is NOT irrelevant
        !> for a median, which `pf_nth_quantile` computes exactly on the original array.
        module subroutine median_i64(values, med, is_valid, weights, weight_type, method, n_null, ok, threads)
            integer(int64), intent(in) :: values(:) !! the population.
            real(real64), intent(out) :: med
            !! the median; NaN when the population is empty.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight, non-negative. A ZERO weight removes the element from the
            !! population; a negative, NaN or infinite one aborts. Absent means every weight is 1.
            character(len=*), intent(in), optional :: weight_type
            !! "reliability" (the default) or "frequency". The two differ only when the weights are
            !! unequal: a FREQUENCY weight of 3 says the value occurred three times, so the count
            !! `ddof` is charged against is `sum(weights)`; a RELIABILITY weight says the value is
            !! that much more precise, so the count is Kish's effective size
            !! `sum(weights)**2 / sum(weights**2)`. Any other token aborts, listing both.
            character(len=*), intent(in), optional :: method
            !! how a fractional position between two order statistics is resolved, in numpy's
            !! spelling: "linear" (the default -- Hyndman-Fan type 7, what numpy and pandas do),
            !! "lower", "higher", "nearest", "midpoint", or "inverted_cdf" (a step function on the
            !! plain cumulative scale, and the only token numpy itself accepts weights with). Any
            !! other token aborts, listing all six. Matched case-insensitively.
            !! **`weight_type="frequency"` changes the default to "inverted_cdf"**, because that
            !! token IS frequency expansion; an explicit `method=` still wins.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
            integer, intent(in), optional :: threads
            !! how many threads the ORDERING may use -- this is an order statistic and has no
            !! central-moment pass, so the work `threads=` divides is `pf_argsort`'s. Absent takes
            !! the automatic rule: the `sort_threads` setting, capped by the processors
            !! actually available and by a measured work floor, and 1 inside a caller's own
            !! parallel region. **The answer does not depend on this argument** -- a sort is a
            !! permutation and the selection off it is exact, so 1, 8 and a build with no OpenMP
            !! at all return the same bits. It is a speed control and never an accuracy one.
        end subroutine median_i64
        !> `pf_quantile` over a 64-bit integer array.
        !>
        !> **An `integer(int64)` above 2**53 loses exactness** on the way into the real64 buffer
        !> every statistic here is computed in. That is unavoidable for a mean and irrelevant for
        !> the workloads this serves -- nobody averages an object ID -- but it is NOT irrelevant
        !> for a median, which `pf_nth_quantile` computes exactly on the original array.
        module subroutine quantile_i64(values, p, q, is_valid, weights, weight_type, method, n_null, ok, threads)
            integer(int64), intent(in) :: values(:) !! the population.
            real(real64), intent(in) :: p
            !! the probability, on a 0-1 scale rather than 0-100. Outside [0, 1] aborts.
            real(real64), intent(out) :: q
            !! the quantile; NaN when the population is empty.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight, non-negative. A ZERO weight removes the element from the
            !! population; a negative, NaN or infinite one aborts. Absent means every weight is 1.
            character(len=*), intent(in), optional :: weight_type
            !! "reliability" (the default) or "frequency". The two differ only when the weights are
            !! unequal: a FREQUENCY weight of 3 says the value occurred three times, so the count
            !! `ddof` is charged against is `sum(weights)`; a RELIABILITY weight says the value is
            !! that much more precise, so the count is Kish's effective size
            !! `sum(weights)**2 / sum(weights**2)`. Any other token aborts, listing both.
            character(len=*), intent(in), optional :: method
            !! how a fractional position between two order statistics is resolved, in numpy's
            !! spelling: "linear" (the default -- Hyndman-Fan type 7, what numpy and pandas do),
            !! "lower", "higher", "nearest", "midpoint", or "inverted_cdf" (a step function on the
            !! plain cumulative scale, and the only token numpy itself accepts weights with). Any
            !! other token aborts, listing all six. Matched case-insensitively.
            !! **`weight_type="frequency"` changes the default to "inverted_cdf"**, because that
            !! token IS frequency expansion; an explicit `method=` still wins.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
            integer, intent(in), optional :: threads
            !! how many threads the ORDERING may use -- this is an order statistic and has no
            !! central-moment pass, so the work `threads=` divides is `pf_argsort`'s. Absent takes
            !! the automatic rule: the `sort_threads` setting, capped by the processors
            !! actually available and by a measured work floor, and 1 inside a caller's own
            !! parallel region. **The answer does not depend on this argument** -- a sort is a
            !! permutation and the selection off it is exact, so 1, 8 and a build with no OpenMP
            !! at all return the same bits. It is a speed control and never an accuracy one.
        end subroutine quantile_i64
        !> `pf_quantiles` over a 64-bit integer array.
        !>
        !> **An `integer(int64)` above 2**53 loses exactness** on the way into the real64 buffer
        !> every statistic here is computed in. That is unavoidable for a mean and irrelevant for
        !> the workloads this serves -- nobody averages an object ID -- but it is NOT irrelevant
        !> for a median, which `pf_nth_quantile` computes exactly on the original array.
        module subroutine quantiles_i64(values, probs, out, is_valid, weights, weight_type, method, n_null, ok, &
                threads)
            integer(int64), intent(in) :: values(:) !! the population.
            real(real64), intent(in) :: probs(:)
            !! the probabilities, each on a 0-1 scale. Any outside [0, 1] aborts. Need not be sorted.
            real(real64), intent(out) :: out(:)
            !! one quantile per entry of `probs`, same size. ONE sort serves all of them.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight, non-negative. A ZERO weight removes the element from the
            !! population; a negative, NaN or infinite one aborts. Absent means every weight is 1.
            character(len=*), intent(in), optional :: weight_type
            !! "reliability" (the default) or "frequency". The two differ only when the weights are
            !! unequal: a FREQUENCY weight of 3 says the value occurred three times, so the count
            !! `ddof` is charged against is `sum(weights)`; a RELIABILITY weight says the value is
            !! that much more precise, so the count is Kish's effective size
            !! `sum(weights)**2 / sum(weights**2)`. Any other token aborts, listing both.
            character(len=*), intent(in), optional :: method
            !! how a fractional position between two order statistics is resolved, in numpy's
            !! spelling: "linear" (the default -- Hyndman-Fan type 7, what numpy and pandas do),
            !! "lower", "higher", "nearest", "midpoint", or "inverted_cdf" (a step function on the
            !! plain cumulative scale, and the only token numpy itself accepts weights with). Any
            !! other token aborts, listing all six. Matched case-insensitively.
            !! **`weight_type="frequency"` changes the default to "inverted_cdf"**, because that
            !! token IS frequency expansion; an explicit `method=` still wins.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
            integer, intent(in), optional :: threads
            !! how many threads the ORDERING may use -- this is an order statistic and has no
            !! central-moment pass, so the work `threads=` divides is `pf_argsort`'s. Absent takes
            !! the automatic rule: the `sort_threads` setting, capped by the processors
            !! actually available and by a measured work floor, and 1 inside a caller's own
            !! parallel region. **The answer does not depend on this argument** -- a sort is a
            !! permutation and the selection off it is exact, so 1, 8 and a build with no OpenMP
            !! at all return the same bits. It is a speed control and never an accuracy one.
        end subroutine quantiles_i64
        !> `pf_iqr` over a 64-bit integer array.
        !>
        !> **An `integer(int64)` above 2**53 loses exactness** on the way into the real64 buffer
        !> every statistic here is computed in. That is unavoidable for a mean and irrelevant for
        !> the workloads this serves -- nobody averages an object ID -- but it is NOT irrelevant
        !> for a median, which `pf_nth_quantile` computes exactly on the original array.
        module subroutine iqr_i64(values, r, is_valid, weights, weight_type, method, n_null, ok, threads)
            integer(int64), intent(in) :: values(:) !! the population.
            real(real64), intent(out) :: r
            !! the interquartile range, `q(0.75) - q(0.25)`; NaN when either quartile is.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight, non-negative. A ZERO weight removes the element from the
            !! population; a negative, NaN or infinite one aborts. Absent means every weight is 1.
            character(len=*), intent(in), optional :: weight_type
            !! "reliability" (the default) or "frequency". The two differ only when the weights are
            !! unequal: a FREQUENCY weight of 3 says the value occurred three times, so the count
            !! `ddof` is charged against is `sum(weights)`; a RELIABILITY weight says the value is
            !! that much more precise, so the count is Kish's effective size
            !! `sum(weights)**2 / sum(weights**2)`. Any other token aborts, listing both.
            character(len=*), intent(in), optional :: method
            !! how a fractional position between two order statistics is resolved, in numpy's
            !! spelling: "linear" (the default -- Hyndman-Fan type 7, what numpy and pandas do),
            !! "lower", "higher", "nearest", "midpoint", or "inverted_cdf" (a step function on the
            !! plain cumulative scale, and the only token numpy itself accepts weights with). Any
            !! other token aborts, listing all six. Matched case-insensitively.
            !! **`weight_type="frequency"` changes the default to "inverted_cdf"**, because that
            !! token IS frequency expansion; an explicit `method=` still wins.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
            integer, intent(in), optional :: threads
            !! how many threads the ORDERING may use -- this is an order statistic and has no
            !! central-moment pass, so the work `threads=` divides is `pf_argsort`'s. Absent takes
            !! the automatic rule: the `sort_threads` setting, capped by the processors
            !! actually available and by a measured work floor, and 1 inside a caller's own
            !! parallel region. **The answer does not depend on this argument** -- a sort is a
            !! permutation and the selection off it is exact, so 1, 8 and a build with no OpenMP
            !! at all return the same bits. It is a speed control and never an accuracy one.
        end subroutine iqr_i64
        !> `pf_trim_mean` over a 64-bit integer array.
        !>
        !> **An `integer(int64)` above 2**53 loses exactness** on the way into the real64 buffer
        !> every statistic here is computed in. That is unavoidable for a mean and irrelevant for
        !> the workloads this serves -- nobody averages an object ID -- but it is NOT irrelevant
        !> for a median, which `pf_nth_quantile` computes exactly on the original array.
        module subroutine trim_mean_i64(values, prop, m, is_valid, weights, n_null, ok, threads)
            integer(int64), intent(in) :: values(:) !! the population.
            real(real64), intent(in) :: prop
            !! the share trimmed from EACH tail, so `prop=0.1` drops 10% at each end and averages the
            !! middle 80%. Must satisfy `0 <= prop < 0.5`; anything else aborts, because a caller who asks
            !! to trim everything has made a mistake rather than expressed a preference.
            real(real64), intent(out) :: m
            !! the trimmed mean; NaN when nothing survives the trim.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight, non-negative. A ZERO weight removes the element from the
            !! population; a negative, NaN or infinite one aborts. Absent means every weight is 1.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
            integer, intent(in), optional :: threads
            !! how many threads the ORDERING may use -- this is an order statistic and has no
            !! central-moment pass, so the work `threads=` divides is `pf_argsort`'s. Absent takes
            !! the automatic rule: the `sort_threads` setting, capped by the processors
            !! actually available and by a measured work floor, and 1 inside a caller's own
            !! parallel region. **The answer does not depend on this argument** -- a sort is a
            !! permutation and the selection off it is exact, so 1, 8 and a build with no OpenMP
            !! at all return the same bits. It is a speed control and never an accuracy one.
        end subroutine trim_mean_i64
        !> `pf_percentile_of_score` over a 64-bit integer array.
        !>
        !> **An `integer(int64)` above 2**53 loses exactness** on the way into the real64 buffer
        !> every statistic here is computed in. That is unavoidable for a mean and irrelevant for
        !> the workloads this serves -- nobody averages an object ID -- but it is NOT irrelevant
        !> for a median, which `pf_nth_quantile` computes exactly on the original array.
        module subroutine percentile_of_score_i64(values, score, p, is_valid, weights, kind, n_null, ok, threads)
            integer(int64), intent(in) :: values(:) !! the population.
            real(real64), intent(in) :: score
            !! the value to locate. NaN or infinite aborts -- unlike a NaN in the population, which is an
            !! ordinary data condition, a NaN score can only come from the caller's own arithmetic.
            real(real64), intent(out) :: p
            !! the share of the population at or below `score`, on a 0-1 scale; NaN when empty.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight, non-negative. A ZERO weight removes the element from the
            !! population; a negative, NaN or infinite one aborts. Absent means every weight is 1.
            character(len=*), intent(in), optional :: kind
            !! which convention counts a value EQUAL to `score`, in scipy's spelling: "rank" (the
            !! default; ties count half), "weak" (a value <= score counts), "strict" (only < score)
            !! or "mean" (the average of weak and strict). Any other token aborts, listing all
            !! four. Matched case-insensitively.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
            integer, intent(in), optional :: threads
            !! how many threads the ORDERING may use -- this is an order statistic and has no
            !! central-moment pass, so the work `threads=` divides is `pf_argsort`'s. Absent takes
            !! the automatic rule: the `sort_threads` setting, capped by the processors
            !! actually available and by a measured work floor, and 1 inside a caller's own
            !! parallel region. **The answer does not depend on this argument** -- a sort is a
            !! permutation and the selection off it is exact, so 1, 8 and a build with no OpenMP
            !! at all return the same bits. It is a speed control and never an accuracy one.
        end subroutine percentile_of_score_i64
        !> `pf_mad` over a 64-bit integer array.
        !>
        !> **An `integer(int64)` above 2**53 loses exactness** on the way into the real64 buffer
        !> every statistic here is computed in. That is unavoidable for a mean and irrelevant for
        !> the workloads this serves -- nobody averages an object ID -- but it is NOT irrelevant
        !> for a median, which `pf_nth_quantile` computes exactly on the original array.
        module subroutine mad_i64(values, m, is_valid, weights, scale, center, n_null, ok, threads)
            integer(int64), intent(in) :: values(:) !! the population.
            real(real64), intent(out) :: m
            !! the median absolute deviation, scaled unless `scale="raw"`; NaN when the population is
            !! empty.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight, non-negative. A ZERO weight removes the element from the
            !! population; a negative, NaN or infinite one aborts. Absent means every weight is 1.
            character(len=*), intent(in), optional :: scale
            !! "normal" (the default) divides the raw deviation by `Phi^-1(3/4)`, which makes the
            !! result a consistent estimator of the standard deviation for Gaussian data --
            !! scipy's `median_abs_deviation(scale='normal')`, and what an astronomy script means
            !! by "the MAD". That is a multiplication by 1.4826022185056018, and **not** by the
            !! rounded 1.4826 the textbooks quote, which differs by 1.5e-06 relative. "raw" is the
            !! unscaled median of |x - center|. Any other token aborts, naming both. Matched
            !! case-insensitively.
            real(real64), intent(in), optional :: center
            !! the centre the deviations are taken about. Absent uses the population's own median,
            !! which is what scipy does. Supplying one skips ONE selection and not the ordering:
            !! the median of |x - center| still has to be found. A NaN or infinite centre ABORTS
            !! -- unlike a NaN value, which is an ordinary data condition, a NaN centre can only
            !! come from the caller's own arithmetic.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
            integer, intent(in), optional :: threads
            !! how many threads the ORDERING may use -- this is an order statistic and has no
            !! central-moment pass, so the work `threads=` divides is `pf_argsort`'s. Absent takes
            !! the automatic rule: the `sort_threads` setting, capped by the processors
            !! actually available and by a measured work floor, and 1 inside a caller's own
            !! parallel region. **The answer does not depend on this argument** -- a sort is a
            !! permutation and the selection off it is exact, so 1, 8 and a build with no OpenMP
            !! at all return the same bits. It is a speed control and never an accuracy one.
        end subroutine mad_i64
        !> `pf_describe` over a 64-bit integer array.
        !>
        !> **An `integer(int64)` above 2**53 loses exactness** on the way into the real64 buffer
        !> every statistic here is computed in. That is unavoidable for a mean and irrelevant for
        !> the workloads this serves -- nobody averages an object ID -- but it is NOT irrelevant
        !> for a median, which `pf_nth_quantile` computes exactly on the original array.
        module subroutine describe_i64(values, s, is_valid, weights, weight_type, ok, threads)
            integer(int64), intent(in) :: values(:) !! the population.
            type(pf_stats), intent(out) :: s
            !! the filled summary object: tier A computed and tier B already ordered, so every query on it
            !! afterwards is a read.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight, non-negative. A ZERO weight removes the element from the
            !! population; a negative, NaN or infinite one aborts. Absent means every weight is 1.
            character(len=*), intent(in), optional :: weight_type
            !! "reliability" (the default) or "frequency". The two differ only when the weights are
            !! unequal: a FREQUENCY weight of 3 says the value occurred three times, so the count
            !! `ddof` is charged against is `sum(weights)`; a RELIABILITY weight says the value is
            !! that much more precise, so the count is Kish's effective size
            !! `sum(weights)**2 / sum(weights**2)`. Any other token aborts, listing both.
            logical, intent(out), optional :: ok
            !! .false. when the object describes a population with no defined moments -- one that
            !! is empty after the exclusions, or that kept a NaN under `skipnan = .false.` In
            !! either case every tier-A query on `s` answers a quiet NaN. The object itself
            !! carries the counts (`%n_valid()`, `%n_null()`, `%n_nan()`), so this is the one
            !! thing about the result a caller cannot otherwise read without picking a statistic
            !! and NaN-testing it by hand.
            integer, intent(in), optional :: threads
            !! how many threads the central-moment pass and the ordering may use -- `pf_describe`
            !! does both, which no other entry point in this module does. Absent takes the
            !! automatic rule: the `sort_threads` setting, capped by the processors
            !! actually available and by a measured work floor, and 1 inside a caller's own
            !! parallel region. **The answer does not depend on this argument** -- the block
            !! decomposition is a function of the population size alone and a sort is a
            !! permutation, so 1, 8 and a build with no OpenMP at all return the same bits. It is
            !! a speed control and never an accuracy one.
        end subroutine describe_i64
        !> `pf_cov` over a 64-bit integer array.
        module subroutine cov_i64(x, y, c, is_valid, weights, weight_type, ddof, n_null, n_nan, ok)
            integer(int64), intent(in) :: x(:) !! the first sample.
            integer(int64), intent(in) :: y(:) !! the second sample, element for element.
            real(real64), intent(out) :: c !! the covariance; NaN when `n_valid <= ddof`.
            logical, intent(in), optional :: is_valid(:)
            !! per PAIR: .false. marks the pair as unusable. One mask over both arrays, because a
            !! two-sample statistic is taken over pairs; a caller holding a separate mask for each
            !! column passes `mask_x .and. mask_y`, which says what it does. A NaN on either side
            !! drops the pair as well, and is counted through `n_nan` rather than here.
            real(real64), intent(in), optional :: weights(:)
            !! per PAIR weight, non-negative. A ZERO weight removes the pair from the population;
            !! a negative, NaN or infinite one aborts. Absent means every weight is 1. Pearson
            !! only -- see `method`.
            character(len=*), intent(in), optional :: weight_type
            !! "reliability" (the default) or "frequency". The two differ only when the weights are
            !! unequal: a FREQUENCY weight of 3 says the value occurred three times, so the count
            !! `ddof` is charged against is `sum(weights)`; a RELIABILITY weight says the value is
            !! that much more precise, so the count is Kish's effective size
            !! `sum(weights)**2 / sum(weights**2)`. Any other token aborts, listing both.
            integer, intent(in), optional :: ddof
            !! delta degrees of freedom; **1 by default** -- the sample variance, as pandas
            !! returns. numpy's default is 0, so pass `ddof=0` to reproduce `np.var`/`np.std`.
            !! A plain default-kind integer deliberately: it is bounded by the population size,
            !! so there is no int64 form to provide. `ddof >= n_valid` gives NaN, not a division.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            integer(int64), intent(out), optional :: n_nan
            !! how many were excluded as NaN and were not already null.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
        end subroutine cov_i64
        !> `pf_corr` over a 64-bit integer array.
        module subroutine corr_i64(x, y, r, is_valid, weights, method, n_null, n_nan, ok)
            integer(int64), intent(in) :: x(:) !! the first sample.
            integer(int64), intent(in) :: y(:) !! the second sample, element for element.
            real(real64), intent(out) :: r !! the correlation in [-1, 1]; NaN when either sample is constant.
            logical, intent(in), optional :: is_valid(:)
            !! per PAIR: .false. marks the pair as unusable. One mask over both arrays, because a
            !! two-sample statistic is taken over pairs; a caller holding a separate mask for each
            !! column passes `mask_x .and. mask_y`, which says what it does. A NaN on either side
            !! drops the pair as well, and is counted through `n_nan` rather than here.
            real(real64), intent(in), optional :: weights(:)
            !! per PAIR weight, non-negative. A ZERO weight removes the pair from the population;
            !! a negative, NaN or infinite one aborts. Absent means every weight is 1. Pearson
            !! only -- see `method`.
            character(len=*), intent(in), optional :: method
            !! "pearson" (the default) or "spearman". Spearman is Pearson over MIDRANKS: each run
            !! of equal values receives the mean of the sorted positions it spans, so it measures
            !! any monotone relationship rather than a linear one. Any other token aborts, naming
            !! both. Matched case-insensitively. **`weights` with "spearman" aborts** -- a
            !! weighted midrank is a further definitional choice and no reference library makes
            !! it.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            integer(int64), intent(out), optional :: n_nan
            !! how many were excluded as NaN and were not already null.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
        end subroutine corr_i64
        !> `pf_zscore` over a 64-bit integer array.
        module subroutine zscore_i64(values, z, is_valid, ddof, out_valid, n_null, ok)
            integer(int64), intent(in) :: values(:) !! the population.
            real(real64), intent(out) :: z(:)
            !! the standardised values, same size as `values`. An excluded
            !! element is a quiet NaN unless `out_valid` is present.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            integer, intent(in), optional :: ddof
            !! delta degrees of freedom; **1 by default** -- the sample variance, as pandas
            !! returns. numpy's default is 0, so pass `ddof=0` to reproduce `np.var`/`np.std`.
            !! A plain default-kind integer deliberately: it is bounded by the population size,
            !! so there is no int64 form to provide. `ddof >= n_valid` gives NaN, not a division.
            logical, intent(out), optional :: out_valid(:)
            !! per output element: .false. where the input was excluded and the result is
            !! therefore undefined. Same size as `values`. **Absent, such an element is written as
            !! a quiet NaN instead** -- which is loud in practice, since it propagates through
            !! whatever the caller does next.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
        end subroutine zscore_i64
        !> `pf_normal_scores` over a 64-bit integer array.
        module subroutine normal_scores_i64(values, s, is_valid, method, out_valid, n_null, ok, threads)
            integer(int64), intent(in) :: values(:) !! the population.
            real(real64), intent(out) :: s(:)
            !! the normal scores, same size as `values`, in the caller's
            !! original order. An excluded element is a quiet NaN unless
            !! `out_valid` is present.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            character(len=*), intent(in), optional :: method
            !! the PLOTTING POSITION: which point of the distribution a rank is taken to mark.
            !! `s(i) = Phi**(-1)((r - a)/(m + 1 - 2a))` for a midrank `r` among `m` survivors,
            !! with `a` given by the token: "blom" (the default, `a = 3/8`, the standard rankit
            !! and what R's `qqnorm` uses), "weibull" (`a = 0`, `r/(m+1)`, the van der Waerden
            !! score), "tukey" (`a = 1/3`), "hazen" (`a = 1/2`) or "cunnane" (`a = 0.4`).
            !! "filliben" is the MEDIAN rank and not of that family at all: `1 - 0.5**(1/m)` at
            !! the first position, `0.5**(1/m)` at the last and `(i - 0.3175)/(m + 0.365)`
            !! between. Any other token aborts, naming all six. Matched case-insensitively.
            logical, intent(out), optional :: out_valid(:)
            !! per output element: .false. where the input was excluded and the element therefore
            !! has no score. Same size as `values`. **Absent, such an element is written as a
            !! quiet NaN instead** -- which is loud in practice, since it propagates through
            !! whatever the caller does next.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
            integer, intent(in), optional :: threads
            !! how many threads the midrank ordering may use, passed straight to `pf_argsort`.
            !! Absent takes the automatic rule. **The answer does not depend on this argument**:
            !! ties take the mean of the positions they span, so no tie-breaking the sort might
            !! choose can reach the result. It is a speed control and never an accuracy one.
        end subroutine normal_scores_i64
        !> `pf_sigma_clipped_stats` over a 64-bit integer array.
        module subroutine sigma_clipped_stats_i64(values, mean, median, stddev, sigma, sigma_lower, sigma_upper, maxiters, &
                cenfunc, stdfunc, n_clipped, keep, converged, is_valid, &
                n_null, ok, threads)
            integer(int64), intent(in) :: values(:) !! the population.
            real(real64), intent(out) :: mean !! the mean of the surviving values; NaN when none survive.
            real(real64), intent(out) :: median !! their median.
            real(real64), intent(out) :: stddev !! their POPULATION standard deviation (`ddof = 0`), as astropy reports.
            real(real64), intent(in), optional :: sigma
            !! how many scale units a value may sit from the centre and still be kept. 3.0 by
            !! default, as astropy does. `sigma_lower` and `sigma_upper` each default to this, so
            !! setting it alone gives a symmetric clip.
            real(real64), intent(in), optional :: sigma_lower
            !! the lower half of the clip, overriding `sigma` below the centre.
            real(real64), intent(in), optional :: sigma_upper
            !! the upper half of the clip, overriding `sigma` above the centre.
            integer, intent(in), optional :: maxiters
            !! how many clipping rounds to run; 5 by default, as astropy does. **A non-positive
            !! value means iterate until nothing more is removed**, which is astropy's
            !! `maxiters=None` -- capped at a hard internal 100 rounds, and reaching that cap
            !! STOPS and reports through `converged` rather than aborting. Each non-terminating
            !! round removes at least one point, so a large catalogue can honestly want more
            !! rounds than any small constant.
            character(len=*), intent(in), optional :: cenfunc
            !! which centre each round clips about: "median" (the default, as astropy) or "mean".
            !! Any other token aborts, naming both. Matched case-insensitively.
            character(len=*), intent(in), optional :: stdfunc
            !! which scale each round measures: "std" (the default, as astropy -- the sample
            !! standard deviation at `ddof = 1`) or "mad_std", the normal-scaled median absolute
            !! deviation, which is far less disturbed by the outliers being clipped. Any other
            !! token aborts, naming both. Matched case-insensitively.
            integer(int64), intent(out), optional :: n_clipped
            !! how many elements of the population the clipping removed. `0` for an empty
            !! population, and `0` when the first round removes nothing.
            logical, intent(out), optional :: keep(:)
            !! per element of `values`: .true. for one that survived both the exclusion rules and
            !! every clipping round. Same size as `values`. This is what a caller needs in order
            !! to apply the same clip to a SECOND column.
            logical, intent(out), optional :: converged
            !! .false. when the iteration stopped at its round limit with points still being
            !! removed, rather than because a round removed nothing. The answer is still usable
            !! -- it is the last completed round's -- so `ok` stays .true. and this is the only
            !! way to tell the two endings apart.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
            integer, intent(in), optional :: threads
            !! how many threads the ORDERING may use -- this is an order statistic and has no
            !! central-moment pass, so the work `threads=` divides is `pf_argsort`'s. Absent takes
            !! the automatic rule: the `sort_threads` setting, capped by the processors
            !! actually available and by a measured work floor, and 1 inside a caller's own
            !! parallel region. **The answer does not depend on this argument** -- a sort is a
            !! permutation and the selection off it is exact, so 1, 8 and a build with no OpenMP
            !! at all return the same bits. It is a speed control and never an accuracy one.
        end subroutine sigma_clipped_stats_i64
        !> `pf_cumsum` over a 64-bit integer array.
        module subroutine cumsum_i64(values, out, is_valid, out_valid, n_null, ok)
            integer(int64), intent(in) :: values(:) !! the population.
            real(real64), intent(out) :: out(:)
            !! the running sum, same size as `values`. An excluded element is a quiet NaN unless
            !! `out_valid` is present.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            logical, intent(out), optional :: out_valid(:)
            !! per output element: .false. where the input was excluded and the result is
            !! therefore undefined. Same size as `values`. **Absent, such an element is written as
            !! a quiet NaN instead** -- which is loud in practice, since it propagates through
            !! whatever the caller does next.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
        end subroutine cumsum_i64
        !> `pf_cumprod` over a 64-bit integer array.
        module subroutine cumprod_i64(values, out, is_valid, out_valid, n_null, ok)
            integer(int64), intent(in) :: values(:) !! the population.
            real(real64), intent(out) :: out(:)
            !! the running product, same size as `values`. An excluded element is a quiet NaN unless
            !! `out_valid` is present.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            logical, intent(out), optional :: out_valid(:)
            !! per output element: .false. where the input was excluded and the result is
            !! therefore undefined. Same size as `values`. **Absent, such an element is written as
            !! a quiet NaN instead** -- which is loud in practice, since it propagates through
            !! whatever the caller does next.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
        end subroutine cumprod_i64
        !> `pf_cummax` over a 64-bit integer array.
        module subroutine cummax_i64(values, out, is_valid, out_valid, n_null, ok)
            integer(int64), intent(in) :: values(:) !! the population.
            real(real64), intent(out) :: out(:)
            !! the running maximum, same size as `values`. An excluded element is a quiet NaN unless
            !! `out_valid` is present.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            logical, intent(out), optional :: out_valid(:)
            !! per output element: .false. where the input was excluded and the result is
            !! therefore undefined. Same size as `values`. **Absent, such an element is written as
            !! a quiet NaN instead** -- which is loud in practice, since it propagates through
            !! whatever the caller does next.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
        end subroutine cummax_i64
        !> `pf_cummin` over a 64-bit integer array.
        module subroutine cummin_i64(values, out, is_valid, out_valid, n_null, ok)
            integer(int64), intent(in) :: values(:) !! the population.
            real(real64), intent(out) :: out(:)
            !! the running minimum, same size as `values`. An excluded element is a quiet NaN unless
            !! `out_valid` is present.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            logical, intent(out), optional :: out_valid(:)
            !! per output element: .false. where the input was excluded and the result is
            !! therefore undefined. Same size as `values`. **Absent, such an element is written as
            !! a quiet NaN instead** -- which is loud in practice, since it propagates through
            !! whatever the caller does next.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
        end subroutine cummin_i64
        !> `pf_bucketize` over a 64-bit integer array.
        module subroutine bucketize_i64(values, edges, codes, right, is_valid, weights, n_null, n_outside, ok)
            integer(int64), intent(in) :: values(:) !! the population.
            real(real64), intent(in) :: edges(:) !! the bin boundaries, strictly increasing, at least two.
            integer(int32), intent(out) :: codes(:)
            !! the 1-based bin of each value, or 0 for one that joined none. Same size as `values`.
            logical, intent(in), optional :: right
            !! which side of a bin is closed. `.false.` by default, which is numpy's rule and the
            !! one `np.histogram`/`np.digitize` use: bin k is `[edges(k), edges(k+1))`, and the
            !! LAST bin closes at the top so that `edges(nbins+1)` itself lands in it. `.true.`
            !! mirrors it, which is `pd.cut(right=True, include_lowest=True)`: bin k is
            !! `(edges(k), edges(k+1)]`, and the FIRST bin closes at the bottom. Either way the
            !! closed range `[edges(1), edges(nbins+1)]` is covered exactly once, so the two
            !! conventions differ only in which bin a value sitting exactly on an interior edge
            !! joins.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight, non-negative. A ZERO weight removes the element from the
            !! population; a negative, NaN or infinite one aborts. Absent means every weight is 1.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            integer(int64), intent(out), optional :: n_outside
            !! how many usable elements fell outside `[edges(1), edges(nbins+1)]` and so joined no
            !! bin. This is an ordinary data condition rather than an error -- `np.histogram`
            !! drops such values silently -- but it is the one thing a caller cannot recover from
            !! the output, so it is reported. A NaN under `skipnan = .false.` is counted here: it
            !! is then a value, and it matches no bin.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
        end subroutine bucketize_i64
        !> `pf_histogram` over a 64-bit integer array.
        module subroutine histogram_i64(values, edges, counts, right, density, is_valid, weights, n_null, n_outside, &
                ok)
            integer(int64), intent(in) :: values(:) !! the population.
            real(real64), intent(in) :: edges(:) !! the bin boundaries, strictly increasing, at least two.
            real(real64), intent(out) :: counts(:)
            !! one entry per bin, so `size(edges) - 1` of them.
            logical, intent(in), optional :: right
            !! which side of a bin is closed. `.false.` by default, which is numpy's rule and the
            !! one `np.histogram`/`np.digitize` use: bin k is `[edges(k), edges(k+1))`, and the
            !! LAST bin closes at the top so that `edges(nbins+1)` itself lands in it. `.true.`
            !! mirrors it, which is `pd.cut(right=True, include_lowest=True)`: bin k is
            !! `(edges(k), edges(k+1)]`, and the FIRST bin closes at the bottom. Either way the
            !! closed range `[edges(1), edges(nbins+1)]` is covered exactly once, so the two
            !! conventions differ only in which bin a value sitting exactly on an interior edge
            !! joins.
            logical, intent(in), optional :: density
            !! `.false.` by default. `.true.` divides each bin by its own WIDTH and by the total
            !! that was binned, giving a probability density -- `np.histogram(density=True)`, so
            !! `sum(counts * widths)` is 1 and an uneven edge spacing is accounted for. The
            !! normalisation base is what actually landed in a bin: values outside the edges were
            !! never counted, so they do not appear in it, which is numpy's rule too. When nothing
            !! was binned the density is undefined and every entry is a **quiet NaN** with
            !! `ok = .false.` -- note that an empty HISTOGRAM is perfectly well defined (all
            !! zeros, `ok = .true.`) and an empty DENSITY is not.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight, non-negative. A ZERO weight removes the element from the
            !! population; a negative, NaN or infinite one aborts. Absent means every weight is 1.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            integer(int64), intent(out), optional :: n_outside
            !! how many usable elements fell outside `[edges(1), edges(nbins+1)]` and so joined no
            !! bin. This is an ordinary data condition rather than an error -- `np.histogram`
            !! drops such values silently -- but it is the one thing a caller cannot recover from
            !! the output, so it is reported. A NaN under `skipnan = .false.` is counted here: it
            !! is then a value, and it matches no bin.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
        end subroutine histogram_i64
        !> `pf_bin_edges` over a 64-bit integer array.
        module subroutine bin_edges_i64(values, nbins, edges, is_valid, weights, n_null, ok)
            integer(int64), intent(in) :: values(:) !! the population.
            integer, intent(in) :: nbins !! how many bins to describe; at least 1.
            real(real64), intent(out) :: edges(:)
            !! the `nbins + 1` boundaries, strictly increasing.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight, non-negative. A ZERO weight removes the element from the
            !! population; a negative, NaN or infinite one aborts. Absent means every weight is 1.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
        end subroutine bin_edges_i64
        !> `pf_sum` over a 32-bit real array.
        module subroutine sum_f32(values, s, is_valid, weights, skipnan, n_null, n_nan, ok, threads)
            real(real32), intent(in) :: values(:) !! the population.
            real(real64), intent(out) :: s !! the sum.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight, non-negative. A ZERO weight removes the element from the
            !! population; a negative, NaN or infinite one aborts. Absent means every weight is 1.
            logical, intent(in), optional :: skipnan
            !! .true. (the default) excludes a NaN from the population, as a null is excluded and
            !! as `pf_minmax` has always done; .false. restores numpy's propagating behaviour, in
            !! which one NaN makes every answer NaN.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            integer(int64), intent(out), optional :: n_nan
            !! how many were excluded as NaN and were not already null.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
            integer, intent(in), optional :: threads
            !! how many threads the central-moment pass may use. Absent takes the automatic rule:
            !! the `sort_threads` setting, capped by the processors actually available and
            !! by a measured work floor, and 1 inside a caller's own parallel region. **The answer
            !! does not depend on this argument** -- the block decomposition is a function of the
            !! population size alone, so 1, 8 and a build with no OpenMP at all return the same
            !! bits. It is a speed control and never an accuracy one.
        end subroutine sum_f32
        !> `pf_mean` over a 32-bit real array.
        module subroutine mean_f32(values, m, is_valid, weights, skipnan, n_null, n_nan, ok, threads)
            real(real32), intent(in) :: values(:) !! the population.
            real(real64), intent(out) :: m !! the mean; NaN when the population is empty.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight, non-negative. A ZERO weight removes the element from the
            !! population; a negative, NaN or infinite one aborts. Absent means every weight is 1.
            logical, intent(in), optional :: skipnan
            !! .true. (the default) excludes a NaN from the population, as a null is excluded and
            !! as `pf_minmax` has always done; .false. restores numpy's propagating behaviour, in
            !! which one NaN makes every answer NaN.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            integer(int64), intent(out), optional :: n_nan
            !! how many were excluded as NaN and were not already null.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
            integer, intent(in), optional :: threads
            !! how many threads the central-moment pass may use. Absent takes the automatic rule:
            !! the `sort_threads` setting, capped by the processors actually available and
            !! by a measured work floor, and 1 inside a caller's own parallel region. **The answer
            !! does not depend on this argument** -- the block decomposition is a function of the
            !! population size alone, so 1, 8 and a build with no OpenMP at all return the same
            !! bits. It is a speed control and never an accuracy one.
        end subroutine mean_f32
        !> `pf_gmean` over a 32-bit real array.
        module subroutine gmean_f32(values, g, is_valid, weights, skipnan, n_null, n_nan, ok, threads)
            real(real32), intent(in) :: values(:) !! the population.
            real(real64), intent(out) :: g !! the geometric mean; 0 for a zero, NaN for a negative or empty population.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight, non-negative. A ZERO weight removes the element from the
            !! population; a negative, NaN or infinite one aborts. Absent means every weight is 1.
            logical, intent(in), optional :: skipnan
            !! .true. (the default) excludes a NaN from the population, as a null is excluded and
            !! as `pf_minmax` has always done; .false. restores numpy's propagating behaviour, in
            !! which one NaN makes every answer NaN.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            integer(int64), intent(out), optional :: n_nan
            !! how many were excluded as NaN and were not already null.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
            integer, intent(in), optional :: threads
            !! how many threads the central-moment pass may use. Absent takes the automatic rule:
            !! the `sort_threads` setting, capped by the processors actually available and
            !! by a measured work floor, and 1 inside a caller's own parallel region. **The answer
            !! does not depend on this argument** -- the block decomposition is a function of the
            !! population size alone, so 1, 8 and a build with no OpenMP at all return the same
            !! bits. It is a speed control and never an accuracy one.
        end subroutine gmean_f32
        !> `pf_hmean` over a 32-bit real array.
        module subroutine hmean_f32(values, h, is_valid, weights, skipnan, n_null, n_nan, ok, threads)
            real(real32), intent(in) :: values(:) !! the population.
            real(real64), intent(out) :: h !! the harmonic mean; 0 for a zero, NaN for a negative or empty population.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight, non-negative. A ZERO weight removes the element from the
            !! population; a negative, NaN or infinite one aborts. Absent means every weight is 1.
            logical, intent(in), optional :: skipnan
            !! .true. (the default) excludes a NaN from the population, as a null is excluded and
            !! as `pf_minmax` has always done; .false. restores numpy's propagating behaviour, in
            !! which one NaN makes every answer NaN.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            integer(int64), intent(out), optional :: n_nan
            !! how many were excluded as NaN and were not already null.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
            integer, intent(in), optional :: threads
            !! how many threads the central-moment pass may use. Absent takes the automatic rule:
            !! the `sort_threads` setting, capped by the processors actually available and
            !! by a measured work floor, and 1 inside a caller's own parallel region. **The answer
            !! does not depend on this argument** -- the block decomposition is a function of the
            !! population size alone, so 1, 8 and a build with no OpenMP at all return the same
            !! bits. It is a speed control and never an accuracy one.
        end subroutine hmean_f32
        !> `pf_variance` over a 32-bit real array.
        module subroutine variance_f32(values, v, is_valid, weights, weight_type, ddof, skipnan, n_null, n_nan, ok, threads)
            real(real32), intent(in) :: values(:) !! the population.
            real(real64), intent(out) :: v !! the variance; NaN when `n_valid <= ddof`.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight, non-negative. A ZERO weight removes the element from the
            !! population; a negative, NaN or infinite one aborts. Absent means every weight is 1.
            character(len=*), intent(in), optional :: weight_type
            !! "reliability" (the default) or "frequency". The two differ only when the weights are
            !! unequal: a FREQUENCY weight of 3 says the value occurred three times, so the count
            !! `ddof` is charged against is `sum(weights)`; a RELIABILITY weight says the value is
            !! that much more precise, so the count is Kish's effective size
            !! `sum(weights)**2 / sum(weights**2)`. Any other token aborts, listing both.
            integer, intent(in), optional :: ddof
            !! delta degrees of freedom; **1 by default** -- the sample variance, as pandas
            !! returns. numpy's default is 0, so pass `ddof=0` to reproduce `np.var`/`np.std`.
            !! A plain default-kind integer deliberately: it is bounded by the population size,
            !! so there is no int64 form to provide. `ddof >= n_valid` gives NaN, not a division.
            logical, intent(in), optional :: skipnan
            !! .true. (the default) excludes a NaN from the population, as a null is excluded and
            !! as `pf_minmax` has always done; .false. restores numpy's propagating behaviour, in
            !! which one NaN makes every answer NaN.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            integer(int64), intent(out), optional :: n_nan
            !! how many were excluded as NaN and were not already null.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
            integer, intent(in), optional :: threads
            !! how many threads the central-moment pass may use. Absent takes the automatic rule:
            !! the `sort_threads` setting, capped by the processors actually available and
            !! by a measured work floor, and 1 inside a caller's own parallel region. **The answer
            !! does not depend on this argument** -- the block decomposition is a function of the
            !! population size alone, so 1, 8 and a build with no OpenMP at all return the same
            !! bits. It is a speed control and never an accuracy one.
        end subroutine variance_f32
        !> `pf_stddev` over a 32-bit real array.
        module subroutine stddev_f32(values, sd, is_valid, weights, weight_type, ddof, skipnan, n_null, n_nan, ok, threads)
            real(real32), intent(in) :: values(:) !! the population.
            real(real64), intent(out) :: sd !! the standard deviation; NaN when the variance is.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight, non-negative. A ZERO weight removes the element from the
            !! population; a negative, NaN or infinite one aborts. Absent means every weight is 1.
            character(len=*), intent(in), optional :: weight_type
            !! "reliability" (the default) or "frequency". The two differ only when the weights are
            !! unequal: a FREQUENCY weight of 3 says the value occurred three times, so the count
            !! `ddof` is charged against is `sum(weights)`; a RELIABILITY weight says the value is
            !! that much more precise, so the count is Kish's effective size
            !! `sum(weights)**2 / sum(weights**2)`. Any other token aborts, listing both.
            integer, intent(in), optional :: ddof
            !! delta degrees of freedom; **1 by default** -- the sample variance, as pandas
            !! returns. numpy's default is 0, so pass `ddof=0` to reproduce `np.var`/`np.std`.
            !! A plain default-kind integer deliberately: it is bounded by the population size,
            !! so there is no int64 form to provide. `ddof >= n_valid` gives NaN, not a division.
            logical, intent(in), optional :: skipnan
            !! .true. (the default) excludes a NaN from the population, as a null is excluded and
            !! as `pf_minmax` has always done; .false. restores numpy's propagating behaviour, in
            !! which one NaN makes every answer NaN.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            integer(int64), intent(out), optional :: n_nan
            !! how many were excluded as NaN and were not already null.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
            integer, intent(in), optional :: threads
            !! how many threads the central-moment pass may use. Absent takes the automatic rule:
            !! the `sort_threads` setting, capped by the processors actually available and
            !! by a measured work floor, and 1 inside a caller's own parallel region. **The answer
            !! does not depend on this argument** -- the block decomposition is a function of the
            !! population size alone, so 1, 8 and a build with no OpenMP at all return the same
            !! bits. It is a speed control and never an accuracy one.
        end subroutine stddev_f32
        !> `pf_sem` over a 32-bit real array.
        module subroutine sem_f32(values, se, is_valid, weights, weight_type, ddof, skipnan, n_null, n_nan, ok, threads)
            real(real32), intent(in) :: values(:) !! the population.
            real(real64), intent(out) :: se !! the standard error; NaN when the standard deviation is.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight, non-negative. A ZERO weight removes the element from the
            !! population; a negative, NaN or infinite one aborts. Absent means every weight is 1.
            character(len=*), intent(in), optional :: weight_type
            !! "reliability" (the default) or "frequency". The two differ only when the weights are
            !! unequal: a FREQUENCY weight of 3 says the value occurred three times, so the count
            !! `ddof` is charged against is `sum(weights)`; a RELIABILITY weight says the value is
            !! that much more precise, so the count is Kish's effective size
            !! `sum(weights)**2 / sum(weights**2)`. Any other token aborts, listing both.
            integer, intent(in), optional :: ddof
            !! delta degrees of freedom; **1 by default** -- the sample variance, as pandas
            !! returns. numpy's default is 0, so pass `ddof=0` to reproduce `np.var`/`np.std`.
            !! A plain default-kind integer deliberately: it is bounded by the population size,
            !! so there is no int64 form to provide. `ddof >= n_valid` gives NaN, not a division.
            logical, intent(in), optional :: skipnan
            !! .true. (the default) excludes a NaN from the population, as a null is excluded and
            !! as `pf_minmax` has always done; .false. restores numpy's propagating behaviour, in
            !! which one NaN makes every answer NaN.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            integer(int64), intent(out), optional :: n_nan
            !! how many were excluded as NaN and were not already null.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
            integer, intent(in), optional :: threads
            !! how many threads the central-moment pass may use. Absent takes the automatic rule:
            !! the `sort_threads` setting, capped by the processors actually available and
            !! by a measured work floor, and 1 inside a caller's own parallel region. **The answer
            !! does not depend on this argument** -- the block decomposition is a function of the
            !! population size alone, so 1, 8 and a build with no OpenMP at all return the same
            !! bits. It is a speed control and never an accuracy one.
        end subroutine sem_f32
        !> `pf_skewness` over a 32-bit real array.
        module subroutine skewness_f32(values, g, is_valid, weights, weight_type, bias, skipnan, n_null, n_nan, ok, threads)
            real(real32), intent(in) :: values(:) !! the population.
            real(real64), intent(out) :: g !! the skewness.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight, non-negative. A ZERO weight removes the element from the
            !! population; a negative, NaN or infinite one aborts. Absent means every weight is 1.
            character(len=*), intent(in), optional :: weight_type
            !! "reliability" (the default) or "frequency". The two differ only when the weights are
            !! unequal: a FREQUENCY weight of 3 says the value occurred three times, so the count
            !! `ddof` is charged against is `sum(weights)`; a RELIABILITY weight says the value is
            !! that much more precise, so the count is Kish's effective size
            !! `sum(weights)**2 / sum(weights**2)`. Any other token aborts, listing both.
            logical, intent(in), optional :: bias
            !! .false. by default: the bias-CORRECTED G1/G2, which is what pandas returns.
            !! .true. gives the uncorrected g1/g2, which is what scipy returns by default.
            logical, intent(in), optional :: skipnan
            !! .true. (the default) excludes a NaN from the population, as a null is excluded and
            !! as `pf_minmax` has always done; .false. restores numpy's propagating behaviour, in
            !! which one NaN makes every answer NaN.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            integer(int64), intent(out), optional :: n_nan
            !! how many were excluded as NaN and were not already null.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
            integer, intent(in), optional :: threads
            !! how many threads the central-moment pass may use. Absent takes the automatic rule:
            !! the `sort_threads` setting, capped by the processors actually available and
            !! by a measured work floor, and 1 inside a caller's own parallel region. **The answer
            !! does not depend on this argument** -- the block decomposition is a function of the
            !! population size alone, so 1, 8 and a build with no OpenMP at all return the same
            !! bits. It is a speed control and never an accuracy one.
        end subroutine skewness_f32
        !> `pf_kurtosis` over a 32-bit real array.
        module subroutine kurtosis_f32(values, k, is_valid, weights, weight_type, bias, excess, skipnan, n_null, n_nan, ok, threads)
            real(real32), intent(in) :: values(:) !! the population.
            real(real64), intent(out) :: k !! the kurtosis, excess by default.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight, non-negative. A ZERO weight removes the element from the
            !! population; a negative, NaN or infinite one aborts. Absent means every weight is 1.
            character(len=*), intent(in), optional :: weight_type
            !! "reliability" (the default) or "frequency". The two differ only when the weights are
            !! unequal: a FREQUENCY weight of 3 says the value occurred three times, so the count
            !! `ddof` is charged against is `sum(weights)`; a RELIABILITY weight says the value is
            !! that much more precise, so the count is Kish's effective size
            !! `sum(weights)**2 / sum(weights**2)`. Any other token aborts, listing both.
            logical, intent(in), optional :: bias
            !! .false. by default: the bias-CORRECTED G1/G2, which is what pandas returns.
            !! .true. gives the uncorrected g1/g2, which is what scipy returns by default.
            logical, intent(in), optional :: excess
            !! .true. by default: EXCESS kurtosis, which is 0 for a normal population.
            !! .false. adds 3 back, giving the raw fourth-moment ratio.
            logical, intent(in), optional :: skipnan
            !! .true. (the default) excludes a NaN from the population, as a null is excluded and
            !! as `pf_minmax` has always done; .false. restores numpy's propagating behaviour, in
            !! which one NaN makes every answer NaN.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            integer(int64), intent(out), optional :: n_nan
            !! how many were excluded as NaN and were not already null.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
            integer, intent(in), optional :: threads
            !! how many threads the central-moment pass may use. Absent takes the automatic rule:
            !! the `sort_threads` setting, capped by the processors actually available and
            !! by a measured work floor, and 1 inside a caller's own parallel region. **The answer
            !! does not depend on this argument** -- the block decomposition is a function of the
            !! population size alone, so 1, 8 and a build with no OpenMP at all return the same
            !! bits. It is a speed control and never an accuracy one.
        end subroutine kurtosis_f32
        !> `pf_moments` over a 32-bit real array: every tier-A quantity in one pair of passes.
        module subroutine moments_f32(values, n_valid, mean, variance, stddev, sem, skewness, kurtosis, vsum, vmin, &
                vmax, is_valid, weights, weight_type, ddof, bias, excess, &
                skipnan, n_null, n_nan, ok, threads)
            real(real32), intent(in) :: values(:) !! the population.
            integer(int64), intent(out), optional :: n_valid !! how many elements were used.
            real(real64), intent(out), optional :: mean !! the mean; NaN when empty.
            real(real64), intent(out), optional :: variance !! the variance; NaN when undefined.
            real(real64), intent(out), optional :: stddev !! the standard deviation.
            real(real64), intent(out), optional :: sem !! the standard error of the mean.
            real(real64), intent(out), optional :: skewness !! the third standardised moment.
            real(real64), intent(out), optional :: kurtosis !! the fourth, excess by default.
            real(real64), intent(out), optional :: vsum !! the pairwise sum; 0 when empty.
            real(real64), intent(out), optional :: vmin !! the smallest value; NaN when empty.
            real(real64), intent(out), optional :: vmax !! the largest value; NaN when empty.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight, non-negative. A ZERO weight removes the element from the
            !! population; a negative, NaN or infinite one aborts. Absent means every weight is 1.
            character(len=*), intent(in), optional :: weight_type
            !! "reliability" (the default) or "frequency". The two differ only when the weights are
            !! unequal: a FREQUENCY weight of 3 says the value occurred three times, so the count
            !! `ddof` is charged against is `sum(weights)`; a RELIABILITY weight says the value is
            !! that much more precise, so the count is Kish's effective size
            !! `sum(weights)**2 / sum(weights**2)`. Any other token aborts, listing both.
            integer, intent(in), optional :: ddof
            !! delta degrees of freedom; **1 by default** -- the sample variance, as pandas
            !! returns. numpy's default is 0, so pass `ddof=0` to reproduce `np.var`/`np.std`.
            !! A plain default-kind integer deliberately: it is bounded by the population size,
            !! so there is no int64 form to provide. `ddof >= n_valid` gives NaN, not a division.
            logical, intent(in), optional :: bias
            !! .false. by default: the bias-CORRECTED G1/G2, which is what pandas returns.
            !! .true. gives the uncorrected g1/g2, which is what scipy returns by default.
            logical, intent(in), optional :: excess
            !! .true. by default: EXCESS kurtosis, which is 0 for a normal population.
            !! .false. adds 3 back, giving the raw fourth-moment ratio.
            logical, intent(in), optional :: skipnan
            !! .true. (the default) excludes a NaN from the population, as a null is excluded and
            !! as `pf_minmax` has always done; .false. restores numpy's propagating behaviour, in
            !! which one NaN makes every answer NaN.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            integer(int64), intent(out), optional :: n_nan
            !! how many were excluded as NaN and were not already null.
            logical, intent(out), optional :: ok
            !! .false. when any output that was ASKED for came back a quiet NaN -- an empty
            !! population, a `ddof` that leaves nothing to divide by, a constant population for
            !! the shape statistics, or a non-finite value under `skipnan = .false.`. Only the
            !! PRESENT outputs are tested, which is the only reading that works here: `vsum` over
            !! an empty population is a correct `0`, and testing an output the caller never asked
            !! for would report a failure that is not one. `n_valid`, `n_null` and `n_nan` are
            !! counts and are always defined, so they are not tested.
            !!
            !! An infinity is NOT a failure: `mean` answers `+Inf` over a population containing
            !! one, exactly as `pf_mean` and numpy do, and `ok` stays .true. for it while the
            !! variance -- genuinely a NaN there -- turns it .false. if it was asked for.
            integer, intent(in), optional :: threads
            !! how many threads the central-moment pass may use. Absent takes the automatic rule:
            !! the `sort_threads` setting, capped by the processors actually available and
            !! by a measured work floor, and 1 inside a caller's own parallel region. **The answer
            !! does not depend on this argument** -- the block decomposition is a function of the
            !! population size alone, so 1, 8 and a build with no OpenMP at all return the same
            !! bits. It is a speed control and never an accuracy one.
        end subroutine moments_f32
        !> `pf_median` over a 32-bit real array.
        module subroutine median_f32(values, med, is_valid, weights, weight_type, skipnan, method, n_null, n_nan, &
                ok, threads)
            real(real32), intent(in) :: values(:) !! the population.
            real(real64), intent(out) :: med
            !! the median; NaN when the population is empty.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight, non-negative. A ZERO weight removes the element from the
            !! population; a negative, NaN or infinite one aborts. Absent means every weight is 1.
            character(len=*), intent(in), optional :: weight_type
            !! "reliability" (the default) or "frequency". The two differ only when the weights are
            !! unequal: a FREQUENCY weight of 3 says the value occurred three times, so the count
            !! `ddof` is charged against is `sum(weights)`; a RELIABILITY weight says the value is
            !! that much more precise, so the count is Kish's effective size
            !! `sum(weights)**2 / sum(weights**2)`. Any other token aborts, listing both.
            logical, intent(in), optional :: skipnan
            !! .true. (the default) excludes a NaN from the population, as a null is excluded and
            !! as `pf_minmax` has always done; .false. restores numpy's propagating behaviour, in
            !! which one NaN makes every answer NaN.
            character(len=*), intent(in), optional :: method
            !! how a fractional position between two order statistics is resolved, in numpy's
            !! spelling: "linear" (the default -- Hyndman-Fan type 7, what numpy and pandas do),
            !! "lower", "higher", "nearest", "midpoint", or "inverted_cdf" (a step function on the
            !! plain cumulative scale, and the only token numpy itself accepts weights with). Any
            !! other token aborts, listing all six. Matched case-insensitively.
            !! **`weight_type="frequency"` changes the default to "inverted_cdf"**, because that
            !! token IS frequency expansion; an explicit `method=` still wins.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            integer(int64), intent(out), optional :: n_nan
            !! how many were excluded as NaN and were not already null.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
            integer, intent(in), optional :: threads
            !! how many threads the ORDERING may use -- this is an order statistic and has no
            !! central-moment pass, so the work `threads=` divides is `pf_argsort`'s. Absent takes
            !! the automatic rule: the `sort_threads` setting, capped by the processors
            !! actually available and by a measured work floor, and 1 inside a caller's own
            !! parallel region. **The answer does not depend on this argument** -- a sort is a
            !! permutation and the selection off it is exact, so 1, 8 and a build with no OpenMP
            !! at all return the same bits. It is a speed control and never an accuracy one.
        end subroutine median_f32
        !> `pf_quantile` over a 32-bit real array.
        module subroutine quantile_f32(values, p, q, is_valid, weights, weight_type, skipnan, method, n_null, n_nan, &
                ok, threads)
            real(real32), intent(in) :: values(:) !! the population.
            real(real64), intent(in) :: p
            !! the probability, on a 0-1 scale rather than 0-100. Outside [0, 1] aborts.
            real(real64), intent(out) :: q
            !! the quantile; NaN when the population is empty.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight, non-negative. A ZERO weight removes the element from the
            !! population; a negative, NaN or infinite one aborts. Absent means every weight is 1.
            character(len=*), intent(in), optional :: weight_type
            !! "reliability" (the default) or "frequency". The two differ only when the weights are
            !! unequal: a FREQUENCY weight of 3 says the value occurred three times, so the count
            !! `ddof` is charged against is `sum(weights)`; a RELIABILITY weight says the value is
            !! that much more precise, so the count is Kish's effective size
            !! `sum(weights)**2 / sum(weights**2)`. Any other token aborts, listing both.
            logical, intent(in), optional :: skipnan
            !! .true. (the default) excludes a NaN from the population, as a null is excluded and
            !! as `pf_minmax` has always done; .false. restores numpy's propagating behaviour, in
            !! which one NaN makes every answer NaN.
            character(len=*), intent(in), optional :: method
            !! how a fractional position between two order statistics is resolved, in numpy's
            !! spelling: "linear" (the default -- Hyndman-Fan type 7, what numpy and pandas do),
            !! "lower", "higher", "nearest", "midpoint", or "inverted_cdf" (a step function on the
            !! plain cumulative scale, and the only token numpy itself accepts weights with). Any
            !! other token aborts, listing all six. Matched case-insensitively.
            !! **`weight_type="frequency"` changes the default to "inverted_cdf"**, because that
            !! token IS frequency expansion; an explicit `method=` still wins.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            integer(int64), intent(out), optional :: n_nan
            !! how many were excluded as NaN and were not already null.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
            integer, intent(in), optional :: threads
            !! how many threads the ORDERING may use -- this is an order statistic and has no
            !! central-moment pass, so the work `threads=` divides is `pf_argsort`'s. Absent takes
            !! the automatic rule: the `sort_threads` setting, capped by the processors
            !! actually available and by a measured work floor, and 1 inside a caller's own
            !! parallel region. **The answer does not depend on this argument** -- a sort is a
            !! permutation and the selection off it is exact, so 1, 8 and a build with no OpenMP
            !! at all return the same bits. It is a speed control and never an accuracy one.
        end subroutine quantile_f32
        !> `pf_quantiles` over a 32-bit real array.
        module subroutine quantiles_f32(values, probs, out, is_valid, weights, weight_type, skipnan, method, n_null, &
                n_nan, ok, threads)
            real(real32), intent(in) :: values(:) !! the population.
            real(real64), intent(in) :: probs(:)
            !! the probabilities, each on a 0-1 scale. Any outside [0, 1] aborts. Need not be sorted.
            real(real64), intent(out) :: out(:)
            !! one quantile per entry of `probs`, same size. ONE sort serves all of them.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight, non-negative. A ZERO weight removes the element from the
            !! population; a negative, NaN or infinite one aborts. Absent means every weight is 1.
            character(len=*), intent(in), optional :: weight_type
            !! "reliability" (the default) or "frequency". The two differ only when the weights are
            !! unequal: a FREQUENCY weight of 3 says the value occurred three times, so the count
            !! `ddof` is charged against is `sum(weights)`; a RELIABILITY weight says the value is
            !! that much more precise, so the count is Kish's effective size
            !! `sum(weights)**2 / sum(weights**2)`. Any other token aborts, listing both.
            logical, intent(in), optional :: skipnan
            !! .true. (the default) excludes a NaN from the population, as a null is excluded and
            !! as `pf_minmax` has always done; .false. restores numpy's propagating behaviour, in
            !! which one NaN makes every answer NaN.
            character(len=*), intent(in), optional :: method
            !! how a fractional position between two order statistics is resolved, in numpy's
            !! spelling: "linear" (the default -- Hyndman-Fan type 7, what numpy and pandas do),
            !! "lower", "higher", "nearest", "midpoint", or "inverted_cdf" (a step function on the
            !! plain cumulative scale, and the only token numpy itself accepts weights with). Any
            !! other token aborts, listing all six. Matched case-insensitively.
            !! **`weight_type="frequency"` changes the default to "inverted_cdf"**, because that
            !! token IS frequency expansion; an explicit `method=` still wins.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            integer(int64), intent(out), optional :: n_nan
            !! how many were excluded as NaN and were not already null.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
            integer, intent(in), optional :: threads
            !! how many threads the ORDERING may use -- this is an order statistic and has no
            !! central-moment pass, so the work `threads=` divides is `pf_argsort`'s. Absent takes
            !! the automatic rule: the `sort_threads` setting, capped by the processors
            !! actually available and by a measured work floor, and 1 inside a caller's own
            !! parallel region. **The answer does not depend on this argument** -- a sort is a
            !! permutation and the selection off it is exact, so 1, 8 and a build with no OpenMP
            !! at all return the same bits. It is a speed control and never an accuracy one.
        end subroutine quantiles_f32
        !> `pf_iqr` over a 32-bit real array.
        module subroutine iqr_f32(values, r, is_valid, weights, weight_type, skipnan, method, n_null, n_nan, &
                ok, threads)
            real(real32), intent(in) :: values(:) !! the population.
            real(real64), intent(out) :: r
            !! the interquartile range, `q(0.75) - q(0.25)`; NaN when either quartile is.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight, non-negative. A ZERO weight removes the element from the
            !! population; a negative, NaN or infinite one aborts. Absent means every weight is 1.
            character(len=*), intent(in), optional :: weight_type
            !! "reliability" (the default) or "frequency". The two differ only when the weights are
            !! unequal: a FREQUENCY weight of 3 says the value occurred three times, so the count
            !! `ddof` is charged against is `sum(weights)`; a RELIABILITY weight says the value is
            !! that much more precise, so the count is Kish's effective size
            !! `sum(weights)**2 / sum(weights**2)`. Any other token aborts, listing both.
            logical, intent(in), optional :: skipnan
            !! .true. (the default) excludes a NaN from the population, as a null is excluded and
            !! as `pf_minmax` has always done; .false. restores numpy's propagating behaviour, in
            !! which one NaN makes every answer NaN.
            character(len=*), intent(in), optional :: method
            !! how a fractional position between two order statistics is resolved, in numpy's
            !! spelling: "linear" (the default -- Hyndman-Fan type 7, what numpy and pandas do),
            !! "lower", "higher", "nearest", "midpoint", or "inverted_cdf" (a step function on the
            !! plain cumulative scale, and the only token numpy itself accepts weights with). Any
            !! other token aborts, listing all six. Matched case-insensitively.
            !! **`weight_type="frequency"` changes the default to "inverted_cdf"**, because that
            !! token IS frequency expansion; an explicit `method=` still wins.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            integer(int64), intent(out), optional :: n_nan
            !! how many were excluded as NaN and were not already null.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
            integer, intent(in), optional :: threads
            !! how many threads the ORDERING may use -- this is an order statistic and has no
            !! central-moment pass, so the work `threads=` divides is `pf_argsort`'s. Absent takes
            !! the automatic rule: the `sort_threads` setting, capped by the processors
            !! actually available and by a measured work floor, and 1 inside a caller's own
            !! parallel region. **The answer does not depend on this argument** -- a sort is a
            !! permutation and the selection off it is exact, so 1, 8 and a build with no OpenMP
            !! at all return the same bits. It is a speed control and never an accuracy one.
        end subroutine iqr_f32
        !> `pf_trim_mean` over a 32-bit real array.
        module subroutine trim_mean_f32(values, prop, m, is_valid, weights, skipnan, n_null, n_nan, ok, threads)
            real(real32), intent(in) :: values(:) !! the population.
            real(real64), intent(in) :: prop
            !! the share trimmed from EACH tail, so `prop=0.1` drops 10% at each end and averages the
            !! middle 80%. Must satisfy `0 <= prop < 0.5`; anything else aborts, because a caller who asks
            !! to trim everything has made a mistake rather than expressed a preference.
            real(real64), intent(out) :: m
            !! the trimmed mean; NaN when nothing survives the trim.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight, non-negative. A ZERO weight removes the element from the
            !! population; a negative, NaN or infinite one aborts. Absent means every weight is 1.
            logical, intent(in), optional :: skipnan
            !! .true. (the default) excludes a NaN from the population, as a null is excluded and
            !! as `pf_minmax` has always done; .false. restores numpy's propagating behaviour, in
            !! which one NaN makes every answer NaN.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            integer(int64), intent(out), optional :: n_nan
            !! how many were excluded as NaN and were not already null.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
            integer, intent(in), optional :: threads
            !! how many threads the ORDERING may use -- this is an order statistic and has no
            !! central-moment pass, so the work `threads=` divides is `pf_argsort`'s. Absent takes
            !! the automatic rule: the `sort_threads` setting, capped by the processors
            !! actually available and by a measured work floor, and 1 inside a caller's own
            !! parallel region. **The answer does not depend on this argument** -- a sort is a
            !! permutation and the selection off it is exact, so 1, 8 and a build with no OpenMP
            !! at all return the same bits. It is a speed control and never an accuracy one.
        end subroutine trim_mean_f32
        !> `pf_percentile_of_score` over a 32-bit real array.
        module subroutine percentile_of_score_f32(values, score, p, is_valid, weights, skipnan, kind, n_null, n_nan, ok, threads)
            real(real32), intent(in) :: values(:) !! the population.
            real(real64), intent(in) :: score
            !! the value to locate. NaN or infinite aborts -- unlike a NaN in the population, which is an
            !! ordinary data condition, a NaN score can only come from the caller's own arithmetic.
            real(real64), intent(out) :: p
            !! the share of the population at or below `score`, on a 0-1 scale; NaN when empty.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight, non-negative. A ZERO weight removes the element from the
            !! population; a negative, NaN or infinite one aborts. Absent means every weight is 1.
            logical, intent(in), optional :: skipnan
            !! .true. (the default) excludes a NaN from the population, as a null is excluded and
            !! as `pf_minmax` has always done; .false. restores numpy's propagating behaviour, in
            !! which one NaN makes every answer NaN.
            character(len=*), intent(in), optional :: kind
            !! which convention counts a value EQUAL to `score`, in scipy's spelling: "rank" (the
            !! default; ties count half), "weak" (a value <= score counts), "strict" (only < score)
            !! or "mean" (the average of weak and strict). Any other token aborts, listing all
            !! four. Matched case-insensitively.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            integer(int64), intent(out), optional :: n_nan
            !! how many were excluded as NaN and were not already null.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
            integer, intent(in), optional :: threads
            !! how many threads the ORDERING may use -- this is an order statistic and has no
            !! central-moment pass, so the work `threads=` divides is `pf_argsort`'s. Absent takes
            !! the automatic rule: the `sort_threads` setting, capped by the processors
            !! actually available and by a measured work floor, and 1 inside a caller's own
            !! parallel region. **The answer does not depend on this argument** -- a sort is a
            !! permutation and the selection off it is exact, so 1, 8 and a build with no OpenMP
            !! at all return the same bits. It is a speed control and never an accuracy one.
        end subroutine percentile_of_score_f32
        !> `pf_mad` over a 32-bit real array.
        module subroutine mad_f32(values, m, is_valid, weights, skipnan, scale, center, n_null, n_nan, ok, &
                threads)
            real(real32), intent(in) :: values(:) !! the population.
            real(real64), intent(out) :: m
            !! the median absolute deviation, scaled unless `scale="raw"`; NaN when the population is
            !! empty.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight, non-negative. A ZERO weight removes the element from the
            !! population; a negative, NaN or infinite one aborts. Absent means every weight is 1.
            logical, intent(in), optional :: skipnan
            !! .true. (the default) excludes a NaN from the population, as a null is excluded and
            !! as `pf_minmax` has always done; .false. restores numpy's propagating behaviour, in
            !! which one NaN makes every answer NaN.
            character(len=*), intent(in), optional :: scale
            !! "normal" (the default) divides the raw deviation by `Phi^-1(3/4)`, which makes the
            !! result a consistent estimator of the standard deviation for Gaussian data --
            !! scipy's `median_abs_deviation(scale='normal')`, and what an astronomy script means
            !! by "the MAD". That is a multiplication by 1.4826022185056018, and **not** by the
            !! rounded 1.4826 the textbooks quote, which differs by 1.5e-06 relative. "raw" is the
            !! unscaled median of |x - center|. Any other token aborts, naming both. Matched
            !! case-insensitively.
            real(real64), intent(in), optional :: center
            !! the centre the deviations are taken about. Absent uses the population's own median,
            !! which is what scipy does. Supplying one skips ONE selection and not the ordering:
            !! the median of |x - center| still has to be found. A NaN or infinite centre ABORTS
            !! -- unlike a NaN value, which is an ordinary data condition, a NaN centre can only
            !! come from the caller's own arithmetic.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            integer(int64), intent(out), optional :: n_nan
            !! how many were excluded as NaN and were not already null.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
            integer, intent(in), optional :: threads
            !! how many threads the ORDERING may use -- this is an order statistic and has no
            !! central-moment pass, so the work `threads=` divides is `pf_argsort`'s. Absent takes
            !! the automatic rule: the `sort_threads` setting, capped by the processors
            !! actually available and by a measured work floor, and 1 inside a caller's own
            !! parallel region. **The answer does not depend on this argument** -- a sort is a
            !! permutation and the selection off it is exact, so 1, 8 and a build with no OpenMP
            !! at all return the same bits. It is a speed control and never an accuracy one.
        end subroutine mad_f32
        !> `pf_describe` over a 32-bit real array.
        module subroutine describe_f32(values, s, is_valid, weights, weight_type, skipnan, ok, threads)
            real(real32), intent(in) :: values(:) !! the population.
            type(pf_stats), intent(out) :: s
            !! the filled summary object: tier A computed and tier B already ordered, so every query on it
            !! afterwards is a read.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight, non-negative. A ZERO weight removes the element from the
            !! population; a negative, NaN or infinite one aborts. Absent means every weight is 1.
            character(len=*), intent(in), optional :: weight_type
            !! "reliability" (the default) or "frequency". The two differ only when the weights are
            !! unequal: a FREQUENCY weight of 3 says the value occurred three times, so the count
            !! `ddof` is charged against is `sum(weights)`; a RELIABILITY weight says the value is
            !! that much more precise, so the count is Kish's effective size
            !! `sum(weights)**2 / sum(weights**2)`. Any other token aborts, listing both.
            logical, intent(in), optional :: skipnan
            !! .true. (the default) excludes a NaN from the population, as a null is excluded and
            !! as `pf_minmax` has always done; .false. restores numpy's propagating behaviour, in
            !! which one NaN makes every answer NaN.
            logical, intent(out), optional :: ok
            !! .false. when the object describes a population with no defined moments -- one that
            !! is empty after the exclusions, or that kept a NaN under `skipnan = .false.` In
            !! either case every tier-A query on `s` answers a quiet NaN. The object itself
            !! carries the counts (`%n_valid()`, `%n_null()`, `%n_nan()`), so this is the one
            !! thing about the result a caller cannot otherwise read without picking a statistic
            !! and NaN-testing it by hand.
            integer, intent(in), optional :: threads
            !! how many threads the central-moment pass and the ordering may use -- `pf_describe`
            !! does both, which no other entry point in this module does. Absent takes the
            !! automatic rule: the `sort_threads` setting, capped by the processors
            !! actually available and by a measured work floor, and 1 inside a caller's own
            !! parallel region. **The answer does not depend on this argument** -- the block
            !! decomposition is a function of the population size alone and a sort is a
            !! permutation, so 1, 8 and a build with no OpenMP at all return the same bits. It is
            !! a speed control and never an accuracy one.
        end subroutine describe_f32
        !> `pf_cov` over a 32-bit real array.
        module subroutine cov_f32(x, y, c, is_valid, weights, weight_type, ddof, n_null, n_nan, ok)
            real(real32), intent(in) :: x(:) !! the first sample.
            real(real32), intent(in) :: y(:) !! the second sample, element for element.
            real(real64), intent(out) :: c !! the covariance; NaN when `n_valid <= ddof`.
            logical, intent(in), optional :: is_valid(:)
            !! per PAIR: .false. marks the pair as unusable. One mask over both arrays, because a
            !! two-sample statistic is taken over pairs; a caller holding a separate mask for each
            !! column passes `mask_x .and. mask_y`, which says what it does. A NaN on either side
            !! drops the pair as well, and is counted through `n_nan` rather than here.
            real(real64), intent(in), optional :: weights(:)
            !! per PAIR weight, non-negative. A ZERO weight removes the pair from the population;
            !! a negative, NaN or infinite one aborts. Absent means every weight is 1. Pearson
            !! only -- see `method`.
            character(len=*), intent(in), optional :: weight_type
            !! "reliability" (the default) or "frequency". The two differ only when the weights are
            !! unequal: a FREQUENCY weight of 3 says the value occurred three times, so the count
            !! `ddof` is charged against is `sum(weights)`; a RELIABILITY weight says the value is
            !! that much more precise, so the count is Kish's effective size
            !! `sum(weights)**2 / sum(weights**2)`. Any other token aborts, listing both.
            integer, intent(in), optional :: ddof
            !! delta degrees of freedom; **1 by default** -- the sample variance, as pandas
            !! returns. numpy's default is 0, so pass `ddof=0` to reproduce `np.var`/`np.std`.
            !! A plain default-kind integer deliberately: it is bounded by the population size,
            !! so there is no int64 form to provide. `ddof >= n_valid` gives NaN, not a division.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            integer(int64), intent(out), optional :: n_nan
            !! how many were excluded as NaN and were not already null.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
        end subroutine cov_f32
        !> `pf_corr` over a 32-bit real array.
        module subroutine corr_f32(x, y, r, is_valid, weights, method, n_null, n_nan, ok)
            real(real32), intent(in) :: x(:) !! the first sample.
            real(real32), intent(in) :: y(:) !! the second sample, element for element.
            real(real64), intent(out) :: r !! the correlation in [-1, 1]; NaN when either sample is constant.
            logical, intent(in), optional :: is_valid(:)
            !! per PAIR: .false. marks the pair as unusable. One mask over both arrays, because a
            !! two-sample statistic is taken over pairs; a caller holding a separate mask for each
            !! column passes `mask_x .and. mask_y`, which says what it does. A NaN on either side
            !! drops the pair as well, and is counted through `n_nan` rather than here.
            real(real64), intent(in), optional :: weights(:)
            !! per PAIR weight, non-negative. A ZERO weight removes the pair from the population;
            !! a negative, NaN or infinite one aborts. Absent means every weight is 1. Pearson
            !! only -- see `method`.
            character(len=*), intent(in), optional :: method
            !! "pearson" (the default) or "spearman". Spearman is Pearson over MIDRANKS: each run
            !! of equal values receives the mean of the sorted positions it spans, so it measures
            !! any monotone relationship rather than a linear one. Any other token aborts, naming
            !! both. Matched case-insensitively. **`weights` with "spearman" aborts** -- a
            !! weighted midrank is a further definitional choice and no reference library makes
            !! it.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            integer(int64), intent(out), optional :: n_nan
            !! how many were excluded as NaN and were not already null.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
        end subroutine corr_f32
        !> `pf_zscore` over a 32-bit real array.
        module subroutine zscore_f32(values, z, is_valid, ddof, skipnan, out_valid, n_null, ok)
            real(real32), intent(in) :: values(:) !! the population.
            real(real64), intent(out) :: z(:)
            !! the standardised values, same size as `values`. An excluded
            !! element is a quiet NaN unless `out_valid` is present.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            integer, intent(in), optional :: ddof
            !! delta degrees of freedom; **1 by default** -- the sample variance, as pandas
            !! returns. numpy's default is 0, so pass `ddof=0` to reproduce `np.var`/`np.std`.
            !! A plain default-kind integer deliberately: it is bounded by the population size,
            !! so there is no int64 form to provide. `ddof >= n_valid` gives NaN, not a division.
            logical, intent(in), optional :: skipnan
            !! .true. (the default) excludes a NaN from the population, as a null is excluded and
            !! as `pf_minmax` has always done; .false. restores numpy's propagating behaviour, in
            !! which one NaN makes every answer NaN.
            logical, intent(out), optional :: out_valid(:)
            !! per output element: .false. where the input was excluded and the result is
            !! therefore undefined. Same size as `values`. **Absent, such an element is written as
            !! a quiet NaN instead** -- which is loud in practice, since it propagates through
            !! whatever the caller does next.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
        end subroutine zscore_f32
        !> `pf_normal_scores` over a 32-bit real array.
        module subroutine normal_scores_f32(values, s, is_valid, skipnan, method, out_valid, n_null, ok, threads)
            real(real32), intent(in) :: values(:) !! the population.
            real(real64), intent(out) :: s(:)
            !! the normal scores, same size as `values`, in the caller's
            !! original order. An excluded element is a quiet NaN unless
            !! `out_valid` is present.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            logical, intent(in), optional :: skipnan
            !! .true. (the default) excludes a NaN from the population, as a null is excluded and
            !! as `pf_minmax` has always done; .false. restores numpy's propagating behaviour, in
            !! which one NaN makes every answer NaN.
            character(len=*), intent(in), optional :: method
            !! the PLOTTING POSITION: which point of the distribution a rank is taken to mark.
            !! `s(i) = Phi**(-1)((r - a)/(m + 1 - 2a))` for a midrank `r` among `m` survivors,
            !! with `a` given by the token: "blom" (the default, `a = 3/8`, the standard rankit
            !! and what R's `qqnorm` uses), "weibull" (`a = 0`, `r/(m+1)`, the van der Waerden
            !! score), "tukey" (`a = 1/3`), "hazen" (`a = 1/2`) or "cunnane" (`a = 0.4`).
            !! "filliben" is the MEDIAN rank and not of that family at all: `1 - 0.5**(1/m)` at
            !! the first position, `0.5**(1/m)` at the last and `(i - 0.3175)/(m + 0.365)`
            !! between. Any other token aborts, naming all six. Matched case-insensitively.
            logical, intent(out), optional :: out_valid(:)
            !! per output element: .false. where the input was excluded and the element therefore
            !! has no score. Same size as `values`. **Absent, such an element is written as a
            !! quiet NaN instead** -- which is loud in practice, since it propagates through
            !! whatever the caller does next.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
            integer, intent(in), optional :: threads
            !! how many threads the midrank ordering may use, passed straight to `pf_argsort`.
            !! Absent takes the automatic rule. **The answer does not depend on this argument**:
            !! ties take the mean of the positions they span, so no tie-breaking the sort might
            !! choose can reach the result. It is a speed control and never an accuracy one.
        end subroutine normal_scores_f32
        !> `pf_sigma_clipped_stats` over a 32-bit real array.
        module subroutine sigma_clipped_stats_f32(values, mean, median, stddev, sigma, sigma_lower, sigma_upper, maxiters, &
                cenfunc, stdfunc, n_clipped, keep, converged, is_valid, &
                skipnan, n_null, n_nan, ok, threads)
            real(real32), intent(in) :: values(:) !! the population.
            real(real64), intent(out) :: mean !! the mean of the surviving values; NaN when none survive.
            real(real64), intent(out) :: median !! their median.
            real(real64), intent(out) :: stddev !! their POPULATION standard deviation (`ddof = 0`), as astropy reports.
            real(real64), intent(in), optional :: sigma
            !! how many scale units a value may sit from the centre and still be kept. 3.0 by
            !! default, as astropy does. `sigma_lower` and `sigma_upper` each default to this, so
            !! setting it alone gives a symmetric clip.
            real(real64), intent(in), optional :: sigma_lower
            !! the lower half of the clip, overriding `sigma` below the centre.
            real(real64), intent(in), optional :: sigma_upper
            !! the upper half of the clip, overriding `sigma` above the centre.
            integer, intent(in), optional :: maxiters
            !! how many clipping rounds to run; 5 by default, as astropy does. **A non-positive
            !! value means iterate until nothing more is removed**, which is astropy's
            !! `maxiters=None` -- capped at a hard internal 100 rounds, and reaching that cap
            !! STOPS and reports through `converged` rather than aborting. Each non-terminating
            !! round removes at least one point, so a large catalogue can honestly want more
            !! rounds than any small constant.
            character(len=*), intent(in), optional :: cenfunc
            !! which centre each round clips about: "median" (the default, as astropy) or "mean".
            !! Any other token aborts, naming both. Matched case-insensitively.
            character(len=*), intent(in), optional :: stdfunc
            !! which scale each round measures: "std" (the default, as astropy -- the sample
            !! standard deviation at `ddof = 1`) or "mad_std", the normal-scaled median absolute
            !! deviation, which is far less disturbed by the outliers being clipped. Any other
            !! token aborts, naming both. Matched case-insensitively.
            integer(int64), intent(out), optional :: n_clipped
            !! how many elements of the population the clipping removed. `0` for an empty
            !! population, and `0` when the first round removes nothing.
            logical, intent(out), optional :: keep(:)
            !! per element of `values`: .true. for one that survived both the exclusion rules and
            !! every clipping round. Same size as `values`. This is what a caller needs in order
            !! to apply the same clip to a SECOND column.
            logical, intent(out), optional :: converged
            !! .false. when the iteration stopped at its round limit with points still being
            !! removed, rather than because a round removed nothing. The answer is still usable
            !! -- it is the last completed round's -- so `ok` stays .true. and this is the only
            !! way to tell the two endings apart.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            logical, intent(in), optional :: skipnan
            !! .true. (the default) excludes a NaN from the population, as a null is excluded and
            !! as `pf_minmax` has always done; .false. restores numpy's propagating behaviour, in
            !! which one NaN makes every answer NaN.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            integer(int64), intent(out), optional :: n_nan
            !! how many were excluded as NaN and were not already null.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
            integer, intent(in), optional :: threads
            !! how many threads the ORDERING may use -- this is an order statistic and has no
            !! central-moment pass, so the work `threads=` divides is `pf_argsort`'s. Absent takes
            !! the automatic rule: the `sort_threads` setting, capped by the processors
            !! actually available and by a measured work floor, and 1 inside a caller's own
            !! parallel region. **The answer does not depend on this argument** -- a sort is a
            !! permutation and the selection off it is exact, so 1, 8 and a build with no OpenMP
            !! at all return the same bits. It is a speed control and never an accuracy one.
        end subroutine sigma_clipped_stats_f32
        !> `pf_cumsum` over a 32-bit real array.
        module subroutine cumsum_f32(values, out, is_valid, skipnan, out_valid, n_null, n_nan, ok)
            real(real32), intent(in) :: values(:) !! the population.
            real(real64), intent(out) :: out(:)
            !! the running sum, same size as `values`. An excluded element is a quiet NaN unless
            !! `out_valid` is present.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            logical, intent(in), optional :: skipnan
            !! .true. (the default) excludes a NaN from the population, as a null is excluded and
            !! as `pf_minmax` has always done; .false. restores numpy's propagating behaviour, in
            !! which one NaN makes every answer NaN.
            logical, intent(out), optional :: out_valid(:)
            !! per output element: .false. where the input was excluded and the result is
            !! therefore undefined. Same size as `values`. **Absent, such an element is written as
            !! a quiet NaN instead** -- which is loud in practice, since it propagates through
            !! whatever the caller does next.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            integer(int64), intent(out), optional :: n_nan
            !! how many were excluded as NaN and were not already null.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
        end subroutine cumsum_f32
        !> `pf_cumprod` over a 32-bit real array.
        module subroutine cumprod_f32(values, out, is_valid, skipnan, out_valid, n_null, n_nan, ok)
            real(real32), intent(in) :: values(:) !! the population.
            real(real64), intent(out) :: out(:)
            !! the running product, same size as `values`. An excluded element is a quiet NaN unless
            !! `out_valid` is present.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            logical, intent(in), optional :: skipnan
            !! .true. (the default) excludes a NaN from the population, as a null is excluded and
            !! as `pf_minmax` has always done; .false. restores numpy's propagating behaviour, in
            !! which one NaN makes every answer NaN.
            logical, intent(out), optional :: out_valid(:)
            !! per output element: .false. where the input was excluded and the result is
            !! therefore undefined. Same size as `values`. **Absent, such an element is written as
            !! a quiet NaN instead** -- which is loud in practice, since it propagates through
            !! whatever the caller does next.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            integer(int64), intent(out), optional :: n_nan
            !! how many were excluded as NaN and were not already null.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
        end subroutine cumprod_f32
        !> `pf_cummax` over a 32-bit real array.
        module subroutine cummax_f32(values, out, is_valid, skipnan, out_valid, n_null, n_nan, ok)
            real(real32), intent(in) :: values(:) !! the population.
            real(real64), intent(out) :: out(:)
            !! the running maximum, same size as `values`. An excluded element is a quiet NaN unless
            !! `out_valid` is present.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            logical, intent(in), optional :: skipnan
            !! .true. (the default) excludes a NaN from the population, as a null is excluded and
            !! as `pf_minmax` has always done; .false. restores numpy's propagating behaviour, in
            !! which one NaN makes every answer NaN.
            logical, intent(out), optional :: out_valid(:)
            !! per output element: .false. where the input was excluded and the result is
            !! therefore undefined. Same size as `values`. **Absent, such an element is written as
            !! a quiet NaN instead** -- which is loud in practice, since it propagates through
            !! whatever the caller does next.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            integer(int64), intent(out), optional :: n_nan
            !! how many were excluded as NaN and were not already null.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
        end subroutine cummax_f32
        !> `pf_cummin` over a 32-bit real array.
        module subroutine cummin_f32(values, out, is_valid, skipnan, out_valid, n_null, n_nan, ok)
            real(real32), intent(in) :: values(:) !! the population.
            real(real64), intent(out) :: out(:)
            !! the running minimum, same size as `values`. An excluded element is a quiet NaN unless
            !! `out_valid` is present.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            logical, intent(in), optional :: skipnan
            !! .true. (the default) excludes a NaN from the population, as a null is excluded and
            !! as `pf_minmax` has always done; .false. restores numpy's propagating behaviour, in
            !! which one NaN makes every answer NaN.
            logical, intent(out), optional :: out_valid(:)
            !! per output element: .false. where the input was excluded and the result is
            !! therefore undefined. Same size as `values`. **Absent, such an element is written as
            !! a quiet NaN instead** -- which is loud in practice, since it propagates through
            !! whatever the caller does next.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            integer(int64), intent(out), optional :: n_nan
            !! how many were excluded as NaN and were not already null.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
        end subroutine cummin_f32
        !> `pf_bucketize` over a 32-bit real array.
        module subroutine bucketize_f32(values, edges, codes, right, is_valid, weights, skipnan, n_null, n_nan, &
                n_outside, ok)
            real(real32), intent(in) :: values(:) !! the population.
            real(real64), intent(in) :: edges(:) !! the bin boundaries, strictly increasing, at least two.
            integer(int32), intent(out) :: codes(:)
            !! the 1-based bin of each value, or 0 for one that joined none. Same size as `values`.
            logical, intent(in), optional :: right
            !! which side of a bin is closed. `.false.` by default, which is numpy's rule and the
            !! one `np.histogram`/`np.digitize` use: bin k is `[edges(k), edges(k+1))`, and the
            !! LAST bin closes at the top so that `edges(nbins+1)` itself lands in it. `.true.`
            !! mirrors it, which is `pd.cut(right=True, include_lowest=True)`: bin k is
            !! `(edges(k), edges(k+1)]`, and the FIRST bin closes at the bottom. Either way the
            !! closed range `[edges(1), edges(nbins+1)]` is covered exactly once, so the two
            !! conventions differ only in which bin a value sitting exactly on an interior edge
            !! joins.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight, non-negative. A ZERO weight removes the element from the
            !! population; a negative, NaN or infinite one aborts. Absent means every weight is 1.
            logical, intent(in), optional :: skipnan
            !! .true. (the default) excludes a NaN from the population, as a null is excluded and
            !! as `pf_minmax` has always done; .false. restores numpy's propagating behaviour, in
            !! which one NaN makes every answer NaN.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            integer(int64), intent(out), optional :: n_nan
            !! how many were excluded as NaN and were not already null.
            integer(int64), intent(out), optional :: n_outside
            !! how many usable elements fell outside `[edges(1), edges(nbins+1)]` and so joined no
            !! bin. This is an ordinary data condition rather than an error -- `np.histogram`
            !! drops such values silently -- but it is the one thing a caller cannot recover from
            !! the output, so it is reported. A NaN under `skipnan = .false.` is counted here: it
            !! is then a value, and it matches no bin.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
        end subroutine bucketize_f32
        !> `pf_histogram` over a 32-bit real array.
        module subroutine histogram_f32(values, edges, counts, right, density, is_valid, weights, skipnan, n_null, &
                n_nan, n_outside, ok)
            real(real32), intent(in) :: values(:) !! the population.
            real(real64), intent(in) :: edges(:) !! the bin boundaries, strictly increasing, at least two.
            real(real64), intent(out) :: counts(:)
            !! one entry per bin, so `size(edges) - 1` of them.
            logical, intent(in), optional :: right
            !! which side of a bin is closed. `.false.` by default, which is numpy's rule and the
            !! one `np.histogram`/`np.digitize` use: bin k is `[edges(k), edges(k+1))`, and the
            !! LAST bin closes at the top so that `edges(nbins+1)` itself lands in it. `.true.`
            !! mirrors it, which is `pd.cut(right=True, include_lowest=True)`: bin k is
            !! `(edges(k), edges(k+1)]`, and the FIRST bin closes at the bottom. Either way the
            !! closed range `[edges(1), edges(nbins+1)]` is covered exactly once, so the two
            !! conventions differ only in which bin a value sitting exactly on an interior edge
            !! joins.
            logical, intent(in), optional :: density
            !! `.false.` by default. `.true.` divides each bin by its own WIDTH and by the total
            !! that was binned, giving a probability density -- `np.histogram(density=True)`, so
            !! `sum(counts * widths)` is 1 and an uneven edge spacing is accounted for. The
            !! normalisation base is what actually landed in a bin: values outside the edges were
            !! never counted, so they do not appear in it, which is numpy's rule too. When nothing
            !! was binned the density is undefined and every entry is a **quiet NaN** with
            !! `ok = .false.` -- note that an empty HISTOGRAM is perfectly well defined (all
            !! zeros, `ok = .true.`) and an empty DENSITY is not.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight, non-negative. A ZERO weight removes the element from the
            !! population; a negative, NaN or infinite one aborts. Absent means every weight is 1.
            logical, intent(in), optional :: skipnan
            !! .true. (the default) excludes a NaN from the population, as a null is excluded and
            !! as `pf_minmax` has always done; .false. restores numpy's propagating behaviour, in
            !! which one NaN makes every answer NaN.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            integer(int64), intent(out), optional :: n_nan
            !! how many were excluded as NaN and were not already null.
            integer(int64), intent(out), optional :: n_outside
            !! how many usable elements fell outside `[edges(1), edges(nbins+1)]` and so joined no
            !! bin. This is an ordinary data condition rather than an error -- `np.histogram`
            !! drops such values silently -- but it is the one thing a caller cannot recover from
            !! the output, so it is reported. A NaN under `skipnan = .false.` is counted here: it
            !! is then a value, and it matches no bin.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
        end subroutine histogram_f32
        !> `pf_bin_edges` over a 32-bit real array.
        module subroutine bin_edges_f32(values, nbins, edges, is_valid, weights, skipnan, n_null, n_nan, ok)
            real(real32), intent(in) :: values(:) !! the population.
            integer, intent(in) :: nbins !! how many bins to describe; at least 1.
            real(real64), intent(out) :: edges(:)
            !! the `nbins + 1` boundaries, strictly increasing.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight, non-negative. A ZERO weight removes the element from the
            !! population; a negative, NaN or infinite one aborts. Absent means every weight is 1.
            logical, intent(in), optional :: skipnan
            !! .true. (the default) excludes a NaN from the population, as a null is excluded and
            !! as `pf_minmax` has always done; .false. restores numpy's propagating behaviour, in
            !! which one NaN makes every answer NaN.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            integer(int64), intent(out), optional :: n_nan
            !! how many were excluded as NaN and were not already null.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
        end subroutine bin_edges_f32
        !> `pf_sum` over a logical array.
        !>
        !> `.true.` widens to 1 and `.false.` to 0, so the mean of a logical array is the
        !> fraction that are true.
        module subroutine sum_bool(values, s, is_valid, weights, n_null, ok, threads)
            logical, intent(in) :: values(:) !! the population; .true. is 1 and .false. is 0.
            real(real64), intent(out) :: s !! the sum.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight, non-negative. A ZERO weight removes the element from the
            !! population; a negative, NaN or infinite one aborts. Absent means every weight is 1.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
            integer, intent(in), optional :: threads
            !! how many threads the central-moment pass may use. Absent takes the automatic rule:
            !! the `sort_threads` setting, capped by the processors actually available and
            !! by a measured work floor, and 1 inside a caller's own parallel region. **The answer
            !! does not depend on this argument** -- the block decomposition is a function of the
            !! population size alone, so 1, 8 and a build with no OpenMP at all return the same
            !! bits. It is a speed control and never an accuracy one.
        end subroutine sum_bool
        !> `pf_mean` over a logical array.
        !>
        !> `.true.` widens to 1 and `.false.` to 0, so the mean of a logical array is the
        !> fraction that are true.
        module subroutine mean_bool(values, m, is_valid, weights, n_null, ok, threads)
            logical, intent(in) :: values(:) !! the population; .true. is 1 and .false. is 0.
            real(real64), intent(out) :: m !! the mean; NaN when the population is empty.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight, non-negative. A ZERO weight removes the element from the
            !! population; a negative, NaN or infinite one aborts. Absent means every weight is 1.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
            integer, intent(in), optional :: threads
            !! how many threads the central-moment pass may use. Absent takes the automatic rule:
            !! the `sort_threads` setting, capped by the processors actually available and
            !! by a measured work floor, and 1 inside a caller's own parallel region. **The answer
            !! does not depend on this argument** -- the block decomposition is a function of the
            !! population size alone, so 1, 8 and a build with no OpenMP at all return the same
            !! bits. It is a speed control and never an accuracy one.
        end subroutine mean_bool
        !> `pf_gmean` over a logical array.
        !>
        !> `.true.` widens to 1 and `.false.` to 0, so the mean of a logical array is the
        !> fraction that are true.
        module subroutine gmean_bool(values, g, is_valid, weights, n_null, ok, threads)
            logical, intent(in) :: values(:) !! the population; .true. is 1 and .false. is 0.
            real(real64), intent(out) :: g !! the geometric mean; 0 for a zero, NaN for a negative or empty population.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight, non-negative. A ZERO weight removes the element from the
            !! population; a negative, NaN or infinite one aborts. Absent means every weight is 1.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
            integer, intent(in), optional :: threads
            !! how many threads the central-moment pass may use. Absent takes the automatic rule:
            !! the `sort_threads` setting, capped by the processors actually available and
            !! by a measured work floor, and 1 inside a caller's own parallel region. **The answer
            !! does not depend on this argument** -- the block decomposition is a function of the
            !! population size alone, so 1, 8 and a build with no OpenMP at all return the same
            !! bits. It is a speed control and never an accuracy one.
        end subroutine gmean_bool
        !> `pf_hmean` over a logical array.
        !>
        !> `.true.` widens to 1 and `.false.` to 0, so the mean of a logical array is the
        !> fraction that are true.
        module subroutine hmean_bool(values, h, is_valid, weights, n_null, ok, threads)
            logical, intent(in) :: values(:) !! the population; .true. is 1 and .false. is 0.
            real(real64), intent(out) :: h !! the harmonic mean; 0 for a zero, NaN for a negative or empty population.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight, non-negative. A ZERO weight removes the element from the
            !! population; a negative, NaN or infinite one aborts. Absent means every weight is 1.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
            integer, intent(in), optional :: threads
            !! how many threads the central-moment pass may use. Absent takes the automatic rule:
            !! the `sort_threads` setting, capped by the processors actually available and
            !! by a measured work floor, and 1 inside a caller's own parallel region. **The answer
            !! does not depend on this argument** -- the block decomposition is a function of the
            !! population size alone, so 1, 8 and a build with no OpenMP at all return the same
            !! bits. It is a speed control and never an accuracy one.
        end subroutine hmean_bool
        !> `pf_variance` over a logical array.
        !>
        !> `.true.` widens to 1 and `.false.` to 0, so the mean of a logical array is the
        !> fraction that are true.
        module subroutine variance_bool(values, v, is_valid, weights, weight_type, ddof, n_null, ok, threads)
            logical, intent(in) :: values(:) !! the population; .true. is 1 and .false. is 0.
            real(real64), intent(out) :: v !! the variance; NaN when `n_valid <= ddof`.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight, non-negative. A ZERO weight removes the element from the
            !! population; a negative, NaN or infinite one aborts. Absent means every weight is 1.
            character(len=*), intent(in), optional :: weight_type
            !! "reliability" (the default) or "frequency". The two differ only when the weights are
            !! unequal: a FREQUENCY weight of 3 says the value occurred three times, so the count
            !! `ddof` is charged against is `sum(weights)`; a RELIABILITY weight says the value is
            !! that much more precise, so the count is Kish's effective size
            !! `sum(weights)**2 / sum(weights**2)`. Any other token aborts, listing both.
            integer, intent(in), optional :: ddof
            !! delta degrees of freedom; **1 by default** -- the sample variance, as pandas
            !! returns. numpy's default is 0, so pass `ddof=0` to reproduce `np.var`/`np.std`.
            !! A plain default-kind integer deliberately: it is bounded by the population size,
            !! so there is no int64 form to provide. `ddof >= n_valid` gives NaN, not a division.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
            integer, intent(in), optional :: threads
            !! how many threads the central-moment pass may use. Absent takes the automatic rule:
            !! the `sort_threads` setting, capped by the processors actually available and
            !! by a measured work floor, and 1 inside a caller's own parallel region. **The answer
            !! does not depend on this argument** -- the block decomposition is a function of the
            !! population size alone, so 1, 8 and a build with no OpenMP at all return the same
            !! bits. It is a speed control and never an accuracy one.
        end subroutine variance_bool
        !> `pf_stddev` over a logical array.
        !>
        !> `.true.` widens to 1 and `.false.` to 0, so the mean of a logical array is the
        !> fraction that are true.
        module subroutine stddev_bool(values, sd, is_valid, weights, weight_type, ddof, n_null, ok, threads)
            logical, intent(in) :: values(:) !! the population; .true. is 1 and .false. is 0.
            real(real64), intent(out) :: sd !! the standard deviation; NaN when the variance is.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight, non-negative. A ZERO weight removes the element from the
            !! population; a negative, NaN or infinite one aborts. Absent means every weight is 1.
            character(len=*), intent(in), optional :: weight_type
            !! "reliability" (the default) or "frequency". The two differ only when the weights are
            !! unequal: a FREQUENCY weight of 3 says the value occurred three times, so the count
            !! `ddof` is charged against is `sum(weights)`; a RELIABILITY weight says the value is
            !! that much more precise, so the count is Kish's effective size
            !! `sum(weights)**2 / sum(weights**2)`. Any other token aborts, listing both.
            integer, intent(in), optional :: ddof
            !! delta degrees of freedom; **1 by default** -- the sample variance, as pandas
            !! returns. numpy's default is 0, so pass `ddof=0` to reproduce `np.var`/`np.std`.
            !! A plain default-kind integer deliberately: it is bounded by the population size,
            !! so there is no int64 form to provide. `ddof >= n_valid` gives NaN, not a division.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
            integer, intent(in), optional :: threads
            !! how many threads the central-moment pass may use. Absent takes the automatic rule:
            !! the `sort_threads` setting, capped by the processors actually available and
            !! by a measured work floor, and 1 inside a caller's own parallel region. **The answer
            !! does not depend on this argument** -- the block decomposition is a function of the
            !! population size alone, so 1, 8 and a build with no OpenMP at all return the same
            !! bits. It is a speed control and never an accuracy one.
        end subroutine stddev_bool
        !> `pf_sem` over a logical array.
        !>
        !> `.true.` widens to 1 and `.false.` to 0, so the mean of a logical array is the
        !> fraction that are true.
        module subroutine sem_bool(values, se, is_valid, weights, weight_type, ddof, n_null, ok, threads)
            logical, intent(in) :: values(:) !! the population; .true. is 1 and .false. is 0.
            real(real64), intent(out) :: se !! the standard error; NaN when the standard deviation is.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight, non-negative. A ZERO weight removes the element from the
            !! population; a negative, NaN or infinite one aborts. Absent means every weight is 1.
            character(len=*), intent(in), optional :: weight_type
            !! "reliability" (the default) or "frequency". The two differ only when the weights are
            !! unequal: a FREQUENCY weight of 3 says the value occurred three times, so the count
            !! `ddof` is charged against is `sum(weights)`; a RELIABILITY weight says the value is
            !! that much more precise, so the count is Kish's effective size
            !! `sum(weights)**2 / sum(weights**2)`. Any other token aborts, listing both.
            integer, intent(in), optional :: ddof
            !! delta degrees of freedom; **1 by default** -- the sample variance, as pandas
            !! returns. numpy's default is 0, so pass `ddof=0` to reproduce `np.var`/`np.std`.
            !! A plain default-kind integer deliberately: it is bounded by the population size,
            !! so there is no int64 form to provide. `ddof >= n_valid` gives NaN, not a division.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
            integer, intent(in), optional :: threads
            !! how many threads the central-moment pass may use. Absent takes the automatic rule:
            !! the `sort_threads` setting, capped by the processors actually available and
            !! by a measured work floor, and 1 inside a caller's own parallel region. **The answer
            !! does not depend on this argument** -- the block decomposition is a function of the
            !! population size alone, so 1, 8 and a build with no OpenMP at all return the same
            !! bits. It is a speed control and never an accuracy one.
        end subroutine sem_bool
        !> `pf_skewness` over a logical array.
        !>
        !> `.true.` widens to 1 and `.false.` to 0, so the mean of a logical array is the
        !> fraction that are true.
        module subroutine skewness_bool(values, g, is_valid, weights, weight_type, bias, n_null, ok, threads)
            logical, intent(in) :: values(:) !! the population; .true. is 1 and .false. is 0.
            real(real64), intent(out) :: g !! the skewness.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight, non-negative. A ZERO weight removes the element from the
            !! population; a negative, NaN or infinite one aborts. Absent means every weight is 1.
            character(len=*), intent(in), optional :: weight_type
            !! "reliability" (the default) or "frequency". The two differ only when the weights are
            !! unequal: a FREQUENCY weight of 3 says the value occurred three times, so the count
            !! `ddof` is charged against is `sum(weights)`; a RELIABILITY weight says the value is
            !! that much more precise, so the count is Kish's effective size
            !! `sum(weights)**2 / sum(weights**2)`. Any other token aborts, listing both.
            logical, intent(in), optional :: bias
            !! .false. by default: the bias-CORRECTED G1/G2, which is what pandas returns.
            !! .true. gives the uncorrected g1/g2, which is what scipy returns by default.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
            integer, intent(in), optional :: threads
            !! how many threads the central-moment pass may use. Absent takes the automatic rule:
            !! the `sort_threads` setting, capped by the processors actually available and
            !! by a measured work floor, and 1 inside a caller's own parallel region. **The answer
            !! does not depend on this argument** -- the block decomposition is a function of the
            !! population size alone, so 1, 8 and a build with no OpenMP at all return the same
            !! bits. It is a speed control and never an accuracy one.
        end subroutine skewness_bool
        !> `pf_kurtosis` over a logical array.
        !>
        !> `.true.` widens to 1 and `.false.` to 0, so the mean of a logical array is the
        !> fraction that are true.
        module subroutine kurtosis_bool(values, k, is_valid, weights, weight_type, bias, excess, n_null, ok, threads)
            logical, intent(in) :: values(:) !! the population; .true. is 1 and .false. is 0.
            real(real64), intent(out) :: k !! the kurtosis, excess by default.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight, non-negative. A ZERO weight removes the element from the
            !! population; a negative, NaN or infinite one aborts. Absent means every weight is 1.
            character(len=*), intent(in), optional :: weight_type
            !! "reliability" (the default) or "frequency". The two differ only when the weights are
            !! unequal: a FREQUENCY weight of 3 says the value occurred three times, so the count
            !! `ddof` is charged against is `sum(weights)`; a RELIABILITY weight says the value is
            !! that much more precise, so the count is Kish's effective size
            !! `sum(weights)**2 / sum(weights**2)`. Any other token aborts, listing both.
            logical, intent(in), optional :: bias
            !! .false. by default: the bias-CORRECTED G1/G2, which is what pandas returns.
            !! .true. gives the uncorrected g1/g2, which is what scipy returns by default.
            logical, intent(in), optional :: excess
            !! .true. by default: EXCESS kurtosis, which is 0 for a normal population.
            !! .false. adds 3 back, giving the raw fourth-moment ratio.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
            integer, intent(in), optional :: threads
            !! how many threads the central-moment pass may use. Absent takes the automatic rule:
            !! the `sort_threads` setting, capped by the processors actually available and
            !! by a measured work floor, and 1 inside a caller's own parallel region. **The answer
            !! does not depend on this argument** -- the block decomposition is a function of the
            !! population size alone, so 1, 8 and a build with no OpenMP at all return the same
            !! bits. It is a speed control and never an accuracy one.
        end subroutine kurtosis_bool
        !> `pf_moments` over a logical array: every tier-A quantity in one pair of passes.
        !>
        !> `.true.` widens to 1 and `.false.` to 0, so the mean of a logical array is the
        !> fraction that are true.
        module subroutine moments_bool(values, n_valid, mean, variance, stddev, sem, skewness, kurtosis, vsum, vmin, &
                vmax, is_valid, weights, weight_type, ddof, bias, excess, &
                n_null, ok, threads)
            logical, intent(in) :: values(:) !! the population; .true. is 1 and .false. is 0.
            integer(int64), intent(out), optional :: n_valid !! how many elements were used.
            real(real64), intent(out), optional :: mean !! the mean; NaN when empty.
            real(real64), intent(out), optional :: variance !! the variance; NaN when undefined.
            real(real64), intent(out), optional :: stddev !! the standard deviation.
            real(real64), intent(out), optional :: sem !! the standard error of the mean.
            real(real64), intent(out), optional :: skewness !! the third standardised moment.
            real(real64), intent(out), optional :: kurtosis !! the fourth, excess by default.
            real(real64), intent(out), optional :: vsum !! the pairwise sum; 0 when empty.
            real(real64), intent(out), optional :: vmin !! the smallest value; NaN when empty.
            real(real64), intent(out), optional :: vmax !! the largest value; NaN when empty.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight, non-negative. A ZERO weight removes the element from the
            !! population; a negative, NaN or infinite one aborts. Absent means every weight is 1.
            character(len=*), intent(in), optional :: weight_type
            !! "reliability" (the default) or "frequency". The two differ only when the weights are
            !! unequal: a FREQUENCY weight of 3 says the value occurred three times, so the count
            !! `ddof` is charged against is `sum(weights)`; a RELIABILITY weight says the value is
            !! that much more precise, so the count is Kish's effective size
            !! `sum(weights)**2 / sum(weights**2)`. Any other token aborts, listing both.
            integer, intent(in), optional :: ddof
            !! delta degrees of freedom; **1 by default** -- the sample variance, as pandas
            !! returns. numpy's default is 0, so pass `ddof=0` to reproduce `np.var`/`np.std`.
            !! A plain default-kind integer deliberately: it is bounded by the population size,
            !! so there is no int64 form to provide. `ddof >= n_valid` gives NaN, not a division.
            logical, intent(in), optional :: bias
            !! .false. by default: the bias-CORRECTED G1/G2, which is what pandas returns.
            !! .true. gives the uncorrected g1/g2, which is what scipy returns by default.
            logical, intent(in), optional :: excess
            !! .true. by default: EXCESS kurtosis, which is 0 for a normal population.
            !! .false. adds 3 back, giving the raw fourth-moment ratio.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            logical, intent(out), optional :: ok
            !! .false. when any output that was ASKED for came back a quiet NaN -- an empty
            !! population, a `ddof` that leaves nothing to divide by, a constant population for
            !! the shape statistics, or a non-finite value under `skipnan = .false.`. Only the
            !! PRESENT outputs are tested, which is the only reading that works here: `vsum` over
            !! an empty population is a correct `0`, and testing an output the caller never asked
            !! for would report a failure that is not one. `n_valid`, `n_null` and `n_nan` are
            !! counts and are always defined, so they are not tested.
            !!
            !! An infinity is NOT a failure: `mean` answers `+Inf` over a population containing
            !! one, exactly as `pf_mean` and numpy do, and `ok` stays .true. for it while the
            !! variance -- genuinely a NaN there -- turns it .false. if it was asked for.
            integer, intent(in), optional :: threads
            !! how many threads the central-moment pass may use. Absent takes the automatic rule:
            !! the `sort_threads` setting, capped by the processors actually available and
            !! by a measured work floor, and 1 inside a caller's own parallel region. **The answer
            !! does not depend on this argument** -- the block decomposition is a function of the
            !! population size alone, so 1, 8 and a build with no OpenMP at all return the same
            !! bits. It is a speed control and never an accuracy one.
        end subroutine moments_bool
        !> `pf_median` over a logical array.
        !>
        !> `.true.` widens to 1 and `.false.` to 0, so the mean of a logical array is the
        !> fraction that are true.
        module subroutine median_bool(values, med, is_valid, weights, weight_type, method, n_null, ok, threads)
            logical, intent(in) :: values(:) !! the population; .true. is 1 and .false. is 0.
            real(real64), intent(out) :: med
            !! the median; NaN when the population is empty.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight, non-negative. A ZERO weight removes the element from the
            !! population; a negative, NaN or infinite one aborts. Absent means every weight is 1.
            character(len=*), intent(in), optional :: weight_type
            !! "reliability" (the default) or "frequency". The two differ only when the weights are
            !! unequal: a FREQUENCY weight of 3 says the value occurred three times, so the count
            !! `ddof` is charged against is `sum(weights)`; a RELIABILITY weight says the value is
            !! that much more precise, so the count is Kish's effective size
            !! `sum(weights)**2 / sum(weights**2)`. Any other token aborts, listing both.
            character(len=*), intent(in), optional :: method
            !! how a fractional position between two order statistics is resolved, in numpy's
            !! spelling: "linear" (the default -- Hyndman-Fan type 7, what numpy and pandas do),
            !! "lower", "higher", "nearest", "midpoint", or "inverted_cdf" (a step function on the
            !! plain cumulative scale, and the only token numpy itself accepts weights with). Any
            !! other token aborts, listing all six. Matched case-insensitively.
            !! **`weight_type="frequency"` changes the default to "inverted_cdf"**, because that
            !! token IS frequency expansion; an explicit `method=` still wins.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
            integer, intent(in), optional :: threads
            !! how many threads the ORDERING may use -- this is an order statistic and has no
            !! central-moment pass, so the work `threads=` divides is `pf_argsort`'s. Absent takes
            !! the automatic rule: the `sort_threads` setting, capped by the processors
            !! actually available and by a measured work floor, and 1 inside a caller's own
            !! parallel region. **The answer does not depend on this argument** -- a sort is a
            !! permutation and the selection off it is exact, so 1, 8 and a build with no OpenMP
            !! at all return the same bits. It is a speed control and never an accuracy one.
        end subroutine median_bool
        !> `pf_quantile` over a logical array.
        !>
        !> `.true.` widens to 1 and `.false.` to 0, so the mean of a logical array is the
        !> fraction that are true.
        module subroutine quantile_bool(values, p, q, is_valid, weights, weight_type, method, n_null, ok, threads)
            logical, intent(in) :: values(:) !! the population; .true. is 1 and .false. is 0.
            real(real64), intent(in) :: p
            !! the probability, on a 0-1 scale rather than 0-100. Outside [0, 1] aborts.
            real(real64), intent(out) :: q
            !! the quantile; NaN when the population is empty.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight, non-negative. A ZERO weight removes the element from the
            !! population; a negative, NaN or infinite one aborts. Absent means every weight is 1.
            character(len=*), intent(in), optional :: weight_type
            !! "reliability" (the default) or "frequency". The two differ only when the weights are
            !! unequal: a FREQUENCY weight of 3 says the value occurred three times, so the count
            !! `ddof` is charged against is `sum(weights)`; a RELIABILITY weight says the value is
            !! that much more precise, so the count is Kish's effective size
            !! `sum(weights)**2 / sum(weights**2)`. Any other token aborts, listing both.
            character(len=*), intent(in), optional :: method
            !! how a fractional position between two order statistics is resolved, in numpy's
            !! spelling: "linear" (the default -- Hyndman-Fan type 7, what numpy and pandas do),
            !! "lower", "higher", "nearest", "midpoint", or "inverted_cdf" (a step function on the
            !! plain cumulative scale, and the only token numpy itself accepts weights with). Any
            !! other token aborts, listing all six. Matched case-insensitively.
            !! **`weight_type="frequency"` changes the default to "inverted_cdf"**, because that
            !! token IS frequency expansion; an explicit `method=` still wins.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
            integer, intent(in), optional :: threads
            !! how many threads the ORDERING may use -- this is an order statistic and has no
            !! central-moment pass, so the work `threads=` divides is `pf_argsort`'s. Absent takes
            !! the automatic rule: the `sort_threads` setting, capped by the processors
            !! actually available and by a measured work floor, and 1 inside a caller's own
            !! parallel region. **The answer does not depend on this argument** -- a sort is a
            !! permutation and the selection off it is exact, so 1, 8 and a build with no OpenMP
            !! at all return the same bits. It is a speed control and never an accuracy one.
        end subroutine quantile_bool
        !> `pf_quantiles` over a logical array.
        !>
        !> `.true.` widens to 1 and `.false.` to 0, so the mean of a logical array is the
        !> fraction that are true.
        module subroutine quantiles_bool(values, probs, out, is_valid, weights, weight_type, method, n_null, ok, &
                threads)
            logical, intent(in) :: values(:) !! the population; .true. is 1 and .false. is 0.
            real(real64), intent(in) :: probs(:)
            !! the probabilities, each on a 0-1 scale. Any outside [0, 1] aborts. Need not be sorted.
            real(real64), intent(out) :: out(:)
            !! one quantile per entry of `probs`, same size. ONE sort serves all of them.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight, non-negative. A ZERO weight removes the element from the
            !! population; a negative, NaN or infinite one aborts. Absent means every weight is 1.
            character(len=*), intent(in), optional :: weight_type
            !! "reliability" (the default) or "frequency". The two differ only when the weights are
            !! unequal: a FREQUENCY weight of 3 says the value occurred three times, so the count
            !! `ddof` is charged against is `sum(weights)`; a RELIABILITY weight says the value is
            !! that much more precise, so the count is Kish's effective size
            !! `sum(weights)**2 / sum(weights**2)`. Any other token aborts, listing both.
            character(len=*), intent(in), optional :: method
            !! how a fractional position between two order statistics is resolved, in numpy's
            !! spelling: "linear" (the default -- Hyndman-Fan type 7, what numpy and pandas do),
            !! "lower", "higher", "nearest", "midpoint", or "inverted_cdf" (a step function on the
            !! plain cumulative scale, and the only token numpy itself accepts weights with). Any
            !! other token aborts, listing all six. Matched case-insensitively.
            !! **`weight_type="frequency"` changes the default to "inverted_cdf"**, because that
            !! token IS frequency expansion; an explicit `method=` still wins.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
            integer, intent(in), optional :: threads
            !! how many threads the ORDERING may use -- this is an order statistic and has no
            !! central-moment pass, so the work `threads=` divides is `pf_argsort`'s. Absent takes
            !! the automatic rule: the `sort_threads` setting, capped by the processors
            !! actually available and by a measured work floor, and 1 inside a caller's own
            !! parallel region. **The answer does not depend on this argument** -- a sort is a
            !! permutation and the selection off it is exact, so 1, 8 and a build with no OpenMP
            !! at all return the same bits. It is a speed control and never an accuracy one.
        end subroutine quantiles_bool
        !> `pf_iqr` over a logical array.
        !>
        !> `.true.` widens to 1 and `.false.` to 0, so the mean of a logical array is the
        !> fraction that are true.
        module subroutine iqr_bool(values, r, is_valid, weights, weight_type, method, n_null, ok, threads)
            logical, intent(in) :: values(:) !! the population; .true. is 1 and .false. is 0.
            real(real64), intent(out) :: r
            !! the interquartile range, `q(0.75) - q(0.25)`; NaN when either quartile is.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight, non-negative. A ZERO weight removes the element from the
            !! population; a negative, NaN or infinite one aborts. Absent means every weight is 1.
            character(len=*), intent(in), optional :: weight_type
            !! "reliability" (the default) or "frequency". The two differ only when the weights are
            !! unequal: a FREQUENCY weight of 3 says the value occurred three times, so the count
            !! `ddof` is charged against is `sum(weights)`; a RELIABILITY weight says the value is
            !! that much more precise, so the count is Kish's effective size
            !! `sum(weights)**2 / sum(weights**2)`. Any other token aborts, listing both.
            character(len=*), intent(in), optional :: method
            !! how a fractional position between two order statistics is resolved, in numpy's
            !! spelling: "linear" (the default -- Hyndman-Fan type 7, what numpy and pandas do),
            !! "lower", "higher", "nearest", "midpoint", or "inverted_cdf" (a step function on the
            !! plain cumulative scale, and the only token numpy itself accepts weights with). Any
            !! other token aborts, listing all six. Matched case-insensitively.
            !! **`weight_type="frequency"` changes the default to "inverted_cdf"**, because that
            !! token IS frequency expansion; an explicit `method=` still wins.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
            integer, intent(in), optional :: threads
            !! how many threads the ORDERING may use -- this is an order statistic and has no
            !! central-moment pass, so the work `threads=` divides is `pf_argsort`'s. Absent takes
            !! the automatic rule: the `sort_threads` setting, capped by the processors
            !! actually available and by a measured work floor, and 1 inside a caller's own
            !! parallel region. **The answer does not depend on this argument** -- a sort is a
            !! permutation and the selection off it is exact, so 1, 8 and a build with no OpenMP
            !! at all return the same bits. It is a speed control and never an accuracy one.
        end subroutine iqr_bool
        !> `pf_trim_mean` over a logical array.
        !>
        !> `.true.` widens to 1 and `.false.` to 0, so the mean of a logical array is the
        !> fraction that are true.
        module subroutine trim_mean_bool(values, prop, m, is_valid, weights, n_null, ok, threads)
            logical, intent(in) :: values(:) !! the population; .true. is 1 and .false. is 0.
            real(real64), intent(in) :: prop
            !! the share trimmed from EACH tail, so `prop=0.1` drops 10% at each end and averages the
            !! middle 80%. Must satisfy `0 <= prop < 0.5`; anything else aborts, because a caller who asks
            !! to trim everything has made a mistake rather than expressed a preference.
            real(real64), intent(out) :: m
            !! the trimmed mean; NaN when nothing survives the trim.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight, non-negative. A ZERO weight removes the element from the
            !! population; a negative, NaN or infinite one aborts. Absent means every weight is 1.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
            integer, intent(in), optional :: threads
            !! how many threads the ORDERING may use -- this is an order statistic and has no
            !! central-moment pass, so the work `threads=` divides is `pf_argsort`'s. Absent takes
            !! the automatic rule: the `sort_threads` setting, capped by the processors
            !! actually available and by a measured work floor, and 1 inside a caller's own
            !! parallel region. **The answer does not depend on this argument** -- a sort is a
            !! permutation and the selection off it is exact, so 1, 8 and a build with no OpenMP
            !! at all return the same bits. It is a speed control and never an accuracy one.
        end subroutine trim_mean_bool
        !> `pf_percentile_of_score` over a logical array.
        !>
        !> `.true.` widens to 1 and `.false.` to 0, so the mean of a logical array is the
        !> fraction that are true.
        module subroutine percentile_of_score_bool(values, score, p, is_valid, weights, kind, n_null, ok, threads)
            logical, intent(in) :: values(:) !! the population; .true. is 1 and .false. is 0.
            real(real64), intent(in) :: score
            !! the value to locate. NaN or infinite aborts -- unlike a NaN in the population, which is an
            !! ordinary data condition, a NaN score can only come from the caller's own arithmetic.
            real(real64), intent(out) :: p
            !! the share of the population at or below `score`, on a 0-1 scale; NaN when empty.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight, non-negative. A ZERO weight removes the element from the
            !! population; a negative, NaN or infinite one aborts. Absent means every weight is 1.
            character(len=*), intent(in), optional :: kind
            !! which convention counts a value EQUAL to `score`, in scipy's spelling: "rank" (the
            !! default; ties count half), "weak" (a value <= score counts), "strict" (only < score)
            !! or "mean" (the average of weak and strict). Any other token aborts, listing all
            !! four. Matched case-insensitively.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
            integer, intent(in), optional :: threads
            !! how many threads the ORDERING may use -- this is an order statistic and has no
            !! central-moment pass, so the work `threads=` divides is `pf_argsort`'s. Absent takes
            !! the automatic rule: the `sort_threads` setting, capped by the processors
            !! actually available and by a measured work floor, and 1 inside a caller's own
            !! parallel region. **The answer does not depend on this argument** -- a sort is a
            !! permutation and the selection off it is exact, so 1, 8 and a build with no OpenMP
            !! at all return the same bits. It is a speed control and never an accuracy one.
        end subroutine percentile_of_score_bool
        !> `pf_mad` over a logical array.
        !>
        !> `.true.` widens to 1 and `.false.` to 0, so the mean of a logical array is the
        !> fraction that are true.
        module subroutine mad_bool(values, m, is_valid, weights, scale, center, n_null, ok, threads)
            logical, intent(in) :: values(:) !! the population; .true. is 1 and .false. is 0.
            real(real64), intent(out) :: m
            !! the median absolute deviation, scaled unless `scale="raw"`; NaN when the population is
            !! empty.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight, non-negative. A ZERO weight removes the element from the
            !! population; a negative, NaN or infinite one aborts. Absent means every weight is 1.
            character(len=*), intent(in), optional :: scale
            !! "normal" (the default) divides the raw deviation by `Phi^-1(3/4)`, which makes the
            !! result a consistent estimator of the standard deviation for Gaussian data --
            !! scipy's `median_abs_deviation(scale='normal')`, and what an astronomy script means
            !! by "the MAD". That is a multiplication by 1.4826022185056018, and **not** by the
            !! rounded 1.4826 the textbooks quote, which differs by 1.5e-06 relative. "raw" is the
            !! unscaled median of |x - center|. Any other token aborts, naming both. Matched
            !! case-insensitively.
            real(real64), intent(in), optional :: center
            !! the centre the deviations are taken about. Absent uses the population's own median,
            !! which is what scipy does. Supplying one skips ONE selection and not the ordering:
            !! the median of |x - center| still has to be found. A NaN or infinite centre ABORTS
            !! -- unlike a NaN value, which is an ordinary data condition, a NaN centre can only
            !! come from the caller's own arithmetic.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
            integer, intent(in), optional :: threads
            !! how many threads the ORDERING may use -- this is an order statistic and has no
            !! central-moment pass, so the work `threads=` divides is `pf_argsort`'s. Absent takes
            !! the automatic rule: the `sort_threads` setting, capped by the processors
            !! actually available and by a measured work floor, and 1 inside a caller's own
            !! parallel region. **The answer does not depend on this argument** -- a sort is a
            !! permutation and the selection off it is exact, so 1, 8 and a build with no OpenMP
            !! at all return the same bits. It is a speed control and never an accuracy one.
        end subroutine mad_bool
        !> `pf_describe` over a logical array.
        !>
        !> `.true.` widens to 1 and `.false.` to 0, so the mean of a logical array is the
        !> fraction that are true.
        module subroutine describe_bool(values, s, is_valid, weights, weight_type, ok, threads)
            logical, intent(in) :: values(:) !! the population; .true. is 1 and .false. is 0.
            type(pf_stats), intent(out) :: s
            !! the filled summary object: tier A computed and tier B already ordered, so every query on it
            !! afterwards is a read.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight, non-negative. A ZERO weight removes the element from the
            !! population; a negative, NaN or infinite one aborts. Absent means every weight is 1.
            character(len=*), intent(in), optional :: weight_type
            !! "reliability" (the default) or "frequency". The two differ only when the weights are
            !! unequal: a FREQUENCY weight of 3 says the value occurred three times, so the count
            !! `ddof` is charged against is `sum(weights)`; a RELIABILITY weight says the value is
            !! that much more precise, so the count is Kish's effective size
            !! `sum(weights)**2 / sum(weights**2)`. Any other token aborts, listing both.
            logical, intent(out), optional :: ok
            !! .false. when the object describes a population with no defined moments -- one that
            !! is empty after the exclusions, or that kept a NaN under `skipnan = .false.` In
            !! either case every tier-A query on `s` answers a quiet NaN. The object itself
            !! carries the counts (`%n_valid()`, `%n_null()`, `%n_nan()`), so this is the one
            !! thing about the result a caller cannot otherwise read without picking a statistic
            !! and NaN-testing it by hand.
            integer, intent(in), optional :: threads
            !! how many threads the central-moment pass and the ordering may use -- `pf_describe`
            !! does both, which no other entry point in this module does. Absent takes the
            !! automatic rule: the `sort_threads` setting, capped by the processors
            !! actually available and by a measured work floor, and 1 inside a caller's own
            !! parallel region. **The answer does not depend on this argument** -- the block
            !! decomposition is a function of the population size alone and a sort is a
            !! permutation, so 1, 8 and a build with no OpenMP at all return the same bits. It is
            !! a speed control and never an accuracy one.
        end subroutine describe_bool
        !> `pf_cov` over a logical array.
        module subroutine cov_bool(x, y, c, is_valid, weights, weight_type, ddof, n_null, n_nan, ok)
            logical, intent(in) :: x(:) !! the first sample.
            logical, intent(in) :: y(:) !! the second sample, element for element.
            real(real64), intent(out) :: c !! the covariance; NaN when `n_valid <= ddof`.
            logical, intent(in), optional :: is_valid(:)
            !! per PAIR: .false. marks the pair as unusable. One mask over both arrays, because a
            !! two-sample statistic is taken over pairs; a caller holding a separate mask for each
            !! column passes `mask_x .and. mask_y`, which says what it does. A NaN on either side
            !! drops the pair as well, and is counted through `n_nan` rather than here.
            real(real64), intent(in), optional :: weights(:)
            !! per PAIR weight, non-negative. A ZERO weight removes the pair from the population;
            !! a negative, NaN or infinite one aborts. Absent means every weight is 1. Pearson
            !! only -- see `method`.
            character(len=*), intent(in), optional :: weight_type
            !! "reliability" (the default) or "frequency". The two differ only when the weights are
            !! unequal: a FREQUENCY weight of 3 says the value occurred three times, so the count
            !! `ddof` is charged against is `sum(weights)`; a RELIABILITY weight says the value is
            !! that much more precise, so the count is Kish's effective size
            !! `sum(weights)**2 / sum(weights**2)`. Any other token aborts, listing both.
            integer, intent(in), optional :: ddof
            !! delta degrees of freedom; **1 by default** -- the sample variance, as pandas
            !! returns. numpy's default is 0, so pass `ddof=0` to reproduce `np.var`/`np.std`.
            !! A plain default-kind integer deliberately: it is bounded by the population size,
            !! so there is no int64 form to provide. `ddof >= n_valid` gives NaN, not a division.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            integer(int64), intent(out), optional :: n_nan
            !! how many were excluded as NaN and were not already null.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
        end subroutine cov_bool
        !> `pf_corr` over a logical array.
        module subroutine corr_bool(x, y, r, is_valid, weights, method, n_null, n_nan, ok)
            logical, intent(in) :: x(:) !! the first sample.
            logical, intent(in) :: y(:) !! the second sample, element for element.
            real(real64), intent(out) :: r !! the correlation in [-1, 1]; NaN when either sample is constant.
            logical, intent(in), optional :: is_valid(:)
            !! per PAIR: .false. marks the pair as unusable. One mask over both arrays, because a
            !! two-sample statistic is taken over pairs; a caller holding a separate mask for each
            !! column passes `mask_x .and. mask_y`, which says what it does. A NaN on either side
            !! drops the pair as well, and is counted through `n_nan` rather than here.
            real(real64), intent(in), optional :: weights(:)
            !! per PAIR weight, non-negative. A ZERO weight removes the pair from the population;
            !! a negative, NaN or infinite one aborts. Absent means every weight is 1. Pearson
            !! only -- see `method`.
            character(len=*), intent(in), optional :: method
            !! "pearson" (the default) or "spearman". Spearman is Pearson over MIDRANKS: each run
            !! of equal values receives the mean of the sorted positions it spans, so it measures
            !! any monotone relationship rather than a linear one. Any other token aborts, naming
            !! both. Matched case-insensitively. **`weights` with "spearman" aborts** -- a
            !! weighted midrank is a further definitional choice and no reference library makes
            !! it.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            integer(int64), intent(out), optional :: n_nan
            !! how many were excluded as NaN and were not already null.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
        end subroutine corr_bool
        !> `pf_zscore` over a logical array.
        module subroutine zscore_bool(values, z, is_valid, ddof, out_valid, n_null, ok)
            logical, intent(in) :: values(:) !! the population; .true. is 1 and .false. is 0.
            real(real64), intent(out) :: z(:)
            !! the standardised values, same size as `values`. An excluded
            !! element is a quiet NaN unless `out_valid` is present.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            integer, intent(in), optional :: ddof
            !! delta degrees of freedom; **1 by default** -- the sample variance, as pandas
            !! returns. numpy's default is 0, so pass `ddof=0` to reproduce `np.var`/`np.std`.
            !! A plain default-kind integer deliberately: it is bounded by the population size,
            !! so there is no int64 form to provide. `ddof >= n_valid` gives NaN, not a division.
            logical, intent(out), optional :: out_valid(:)
            !! per output element: .false. where the input was excluded and the result is
            !! therefore undefined. Same size as `values`. **Absent, such an element is written as
            !! a quiet NaN instead** -- which is loud in practice, since it propagates through
            !! whatever the caller does next.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
        end subroutine zscore_bool
        !> `pf_normal_scores` over a logical array.
        module subroutine normal_scores_bool(values, s, is_valid, method, out_valid, n_null, ok, threads)
            logical, intent(in) :: values(:) !! the population; .true. is 1 and .false. is 0.
            real(real64), intent(out) :: s(:)
            !! the normal scores, same size as `values`, in the caller's
            !! original order. An excluded element is a quiet NaN unless
            !! `out_valid` is present.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            character(len=*), intent(in), optional :: method
            !! the PLOTTING POSITION: which point of the distribution a rank is taken to mark.
            !! `s(i) = Phi**(-1)((r - a)/(m + 1 - 2a))` for a midrank `r` among `m` survivors,
            !! with `a` given by the token: "blom" (the default, `a = 3/8`, the standard rankit
            !! and what R's `qqnorm` uses), "weibull" (`a = 0`, `r/(m+1)`, the van der Waerden
            !! score), "tukey" (`a = 1/3`), "hazen" (`a = 1/2`) or "cunnane" (`a = 0.4`).
            !! "filliben" is the MEDIAN rank and not of that family at all: `1 - 0.5**(1/m)` at
            !! the first position, `0.5**(1/m)` at the last and `(i - 0.3175)/(m + 0.365)`
            !! between. Any other token aborts, naming all six. Matched case-insensitively.
            logical, intent(out), optional :: out_valid(:)
            !! per output element: .false. where the input was excluded and the element therefore
            !! has no score. Same size as `values`. **Absent, such an element is written as a
            !! quiet NaN instead** -- which is loud in practice, since it propagates through
            !! whatever the caller does next.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
            integer, intent(in), optional :: threads
            !! how many threads the midrank ordering may use, passed straight to `pf_argsort`.
            !! Absent takes the automatic rule. **The answer does not depend on this argument**:
            !! ties take the mean of the positions they span, so no tie-breaking the sort might
            !! choose can reach the result. It is a speed control and never an accuracy one.
        end subroutine normal_scores_bool
        !> `pf_sigma_clipped_stats` over a logical array.
        module subroutine sigma_clipped_stats_bool(values, mean, median, stddev, sigma, sigma_lower, sigma_upper, maxiters, &
                cenfunc, stdfunc, n_clipped, keep, converged, is_valid, &
                n_null, ok, threads)
            logical, intent(in) :: values(:) !! the population; .true. is 1 and .false. is 0.
            real(real64), intent(out) :: mean !! the mean of the surviving values; NaN when none survive.
            real(real64), intent(out) :: median !! their median.
            real(real64), intent(out) :: stddev !! their POPULATION standard deviation (`ddof = 0`), as astropy reports.
            real(real64), intent(in), optional :: sigma
            !! how many scale units a value may sit from the centre and still be kept. 3.0 by
            !! default, as astropy does. `sigma_lower` and `sigma_upper` each default to this, so
            !! setting it alone gives a symmetric clip.
            real(real64), intent(in), optional :: sigma_lower
            !! the lower half of the clip, overriding `sigma` below the centre.
            real(real64), intent(in), optional :: sigma_upper
            !! the upper half of the clip, overriding `sigma` above the centre.
            integer, intent(in), optional :: maxiters
            !! how many clipping rounds to run; 5 by default, as astropy does. **A non-positive
            !! value means iterate until nothing more is removed**, which is astropy's
            !! `maxiters=None` -- capped at a hard internal 100 rounds, and reaching that cap
            !! STOPS and reports through `converged` rather than aborting. Each non-terminating
            !! round removes at least one point, so a large catalogue can honestly want more
            !! rounds than any small constant.
            character(len=*), intent(in), optional :: cenfunc
            !! which centre each round clips about: "median" (the default, as astropy) or "mean".
            !! Any other token aborts, naming both. Matched case-insensitively.
            character(len=*), intent(in), optional :: stdfunc
            !! which scale each round measures: "std" (the default, as astropy -- the sample
            !! standard deviation at `ddof = 1`) or "mad_std", the normal-scaled median absolute
            !! deviation, which is far less disturbed by the outliers being clipped. Any other
            !! token aborts, naming both. Matched case-insensitively.
            integer(int64), intent(out), optional :: n_clipped
            !! how many elements of the population the clipping removed. `0` for an empty
            !! population, and `0` when the first round removes nothing.
            logical, intent(out), optional :: keep(:)
            !! per element of `values`: .true. for one that survived both the exclusion rules and
            !! every clipping round. Same size as `values`. This is what a caller needs in order
            !! to apply the same clip to a SECOND column.
            logical, intent(out), optional :: converged
            !! .false. when the iteration stopped at its round limit with points still being
            !! removed, rather than because a round removed nothing. The answer is still usable
            !! -- it is the last completed round's -- so `ok` stays .true. and this is the only
            !! way to tell the two endings apart.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
            integer, intent(in), optional :: threads
            !! how many threads the ORDERING may use -- this is an order statistic and has no
            !! central-moment pass, so the work `threads=` divides is `pf_argsort`'s. Absent takes
            !! the automatic rule: the `sort_threads` setting, capped by the processors
            !! actually available and by a measured work floor, and 1 inside a caller's own
            !! parallel region. **The answer does not depend on this argument** -- a sort is a
            !! permutation and the selection off it is exact, so 1, 8 and a build with no OpenMP
            !! at all return the same bits. It is a speed control and never an accuracy one.
        end subroutine sigma_clipped_stats_bool
        !> `pf_cumsum` over a logical array.
        module subroutine cumsum_bool(values, out, is_valid, out_valid, n_null, ok)
            logical, intent(in) :: values(:) !! the population; .true. is 1 and .false. is 0.
            real(real64), intent(out) :: out(:)
            !! the running sum, same size as `values`. An excluded element is a quiet NaN unless
            !! `out_valid` is present.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            logical, intent(out), optional :: out_valid(:)
            !! per output element: .false. where the input was excluded and the result is
            !! therefore undefined. Same size as `values`. **Absent, such an element is written as
            !! a quiet NaN instead** -- which is loud in practice, since it propagates through
            !! whatever the caller does next.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
        end subroutine cumsum_bool
        !> `pf_cumprod` over a logical array.
        module subroutine cumprod_bool(values, out, is_valid, out_valid, n_null, ok)
            logical, intent(in) :: values(:) !! the population; .true. is 1 and .false. is 0.
            real(real64), intent(out) :: out(:)
            !! the running product, same size as `values`. An excluded element is a quiet NaN unless
            !! `out_valid` is present.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            logical, intent(out), optional :: out_valid(:)
            !! per output element: .false. where the input was excluded and the result is
            !! therefore undefined. Same size as `values`. **Absent, such an element is written as
            !! a quiet NaN instead** -- which is loud in practice, since it propagates through
            !! whatever the caller does next.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
        end subroutine cumprod_bool
        !> `pf_cummax` over a logical array.
        module subroutine cummax_bool(values, out, is_valid, out_valid, n_null, ok)
            logical, intent(in) :: values(:) !! the population; .true. is 1 and .false. is 0.
            real(real64), intent(out) :: out(:)
            !! the running maximum, same size as `values`. An excluded element is a quiet NaN unless
            !! `out_valid` is present.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            logical, intent(out), optional :: out_valid(:)
            !! per output element: .false. where the input was excluded and the result is
            !! therefore undefined. Same size as `values`. **Absent, such an element is written as
            !! a quiet NaN instead** -- which is loud in practice, since it propagates through
            !! whatever the caller does next.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
        end subroutine cummax_bool
        !> `pf_cummin` over a logical array.
        module subroutine cummin_bool(values, out, is_valid, out_valid, n_null, ok)
            logical, intent(in) :: values(:) !! the population; .true. is 1 and .false. is 0.
            real(real64), intent(out) :: out(:)
            !! the running minimum, same size as `values`. An excluded element is a quiet NaN unless
            !! `out_valid` is present.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            logical, intent(out), optional :: out_valid(:)
            !! per output element: .false. where the input was excluded and the result is
            !! therefore undefined. Same size as `values`. **Absent, such an element is written as
            !! a quiet NaN instead** -- which is loud in practice, since it propagates through
            !! whatever the caller does next.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
        end subroutine cummin_bool
        !> `pf_bucketize` over a logical array.
        module subroutine bucketize_bool(values, edges, codes, right, is_valid, weights, n_null, n_outside, ok)
            logical, intent(in) :: values(:) !! the population; .true. is 1 and .false. is 0.
            real(real64), intent(in) :: edges(:) !! the bin boundaries, strictly increasing, at least two.
            integer(int32), intent(out) :: codes(:)
            !! the 1-based bin of each value, or 0 for one that joined none. Same size as `values`.
            logical, intent(in), optional :: right
            !! which side of a bin is closed. `.false.` by default, which is numpy's rule and the
            !! one `np.histogram`/`np.digitize` use: bin k is `[edges(k), edges(k+1))`, and the
            !! LAST bin closes at the top so that `edges(nbins+1)` itself lands in it. `.true.`
            !! mirrors it, which is `pd.cut(right=True, include_lowest=True)`: bin k is
            !! `(edges(k), edges(k+1)]`, and the FIRST bin closes at the bottom. Either way the
            !! closed range `[edges(1), edges(nbins+1)]` is covered exactly once, so the two
            !! conventions differ only in which bin a value sitting exactly on an interior edge
            !! joins.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight, non-negative. A ZERO weight removes the element from the
            !! population; a negative, NaN or infinite one aborts. Absent means every weight is 1.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            integer(int64), intent(out), optional :: n_outside
            !! how many usable elements fell outside `[edges(1), edges(nbins+1)]` and so joined no
            !! bin. This is an ordinary data condition rather than an error -- `np.histogram`
            !! drops such values silently -- but it is the one thing a caller cannot recover from
            !! the output, so it is reported. A NaN under `skipnan = .false.` is counted here: it
            !! is then a value, and it matches no bin.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
        end subroutine bucketize_bool
        !> `pf_histogram` over a logical array.
        module subroutine histogram_bool(values, edges, counts, right, density, is_valid, weights, n_null, n_outside, &
                ok)
            logical, intent(in) :: values(:) !! the population; .true. is 1 and .false. is 0.
            real(real64), intent(in) :: edges(:) !! the bin boundaries, strictly increasing, at least two.
            real(real64), intent(out) :: counts(:)
            !! one entry per bin, so `size(edges) - 1` of them.
            logical, intent(in), optional :: right
            !! which side of a bin is closed. `.false.` by default, which is numpy's rule and the
            !! one `np.histogram`/`np.digitize` use: bin k is `[edges(k), edges(k+1))`, and the
            !! LAST bin closes at the top so that `edges(nbins+1)` itself lands in it. `.true.`
            !! mirrors it, which is `pd.cut(right=True, include_lowest=True)`: bin k is
            !! `(edges(k), edges(k+1)]`, and the FIRST bin closes at the bottom. Either way the
            !! closed range `[edges(1), edges(nbins+1)]` is covered exactly once, so the two
            !! conventions differ only in which bin a value sitting exactly on an interior edge
            !! joins.
            logical, intent(in), optional :: density
            !! `.false.` by default. `.true.` divides each bin by its own WIDTH and by the total
            !! that was binned, giving a probability density -- `np.histogram(density=True)`, so
            !! `sum(counts * widths)` is 1 and an uneven edge spacing is accounted for. The
            !! normalisation base is what actually landed in a bin: values outside the edges were
            !! never counted, so they do not appear in it, which is numpy's rule too. When nothing
            !! was binned the density is undefined and every entry is a **quiet NaN** with
            !! `ok = .false.` -- note that an empty HISTOGRAM is perfectly well defined (all
            !! zeros, `ok = .true.`) and an empty DENSITY is not.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight, non-negative. A ZERO weight removes the element from the
            !! population; a negative, NaN or infinite one aborts. Absent means every weight is 1.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            integer(int64), intent(out), optional :: n_outside
            !! how many usable elements fell outside `[edges(1), edges(nbins+1)]` and so joined no
            !! bin. This is an ordinary data condition rather than an error -- `np.histogram`
            !! drops such values silently -- but it is the one thing a caller cannot recover from
            !! the output, so it is reported. A NaN under `skipnan = .false.` is counted here: it
            !! is then a value, and it matches no bin.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
        end subroutine histogram_bool
        !> `pf_bin_edges` over a logical array.
        module subroutine bin_edges_bool(values, nbins, edges, is_valid, weights, n_null, ok)
            logical, intent(in) :: values(:) !! the population; .true. is 1 and .false. is 0.
            integer, intent(in) :: nbins !! how many bins to describe; at least 1.
            real(real64), intent(out) :: edges(:)
            !! the `nbins + 1` boundaries, strictly increasing.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight, non-negative. A ZERO weight removes the element from the
            !! population; a negative, NaN or infinite one aborts. Absent means every weight is 1.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
        end subroutine bin_edges_bool
        !> `pf_sum` over a scalar numeric `parquet_column`.
        !>
        !> Dispatched on the column's kind at run time, and **scalar numeric columns only**:
        !> int32, int64, float32, float64 and logical. A string, temporal or container column
        !> aborts naming its kind, and so does a column more than one element wide -- flattening
        !> a vector column into one population is a different statistic and nobody should get it
        !> by accident. **The column's own validity is the only source of nullness**, so passing
        !> `is_valid=` alongside one aborts rather than silently preferring one of two sources.
        module subroutine sum_col(values, s, is_valid, weights, skipnan, n_null, n_nan, ok, threads)
            type(parquet_column), intent(in) :: values !! the column; scalar numeric kinds only.
            real(real64), intent(out) :: s !! the sum.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight, non-negative. A ZERO weight removes the element from the
            !! population; a negative, NaN or infinite one aborts. Absent means every weight is 1.
            logical, intent(in), optional :: skipnan
            !! .true. (the default) excludes a NaN from the population, as a null is excluded and
            !! as `pf_minmax` has always done; .false. restores numpy's propagating behaviour, in
            !! which one NaN makes every answer NaN.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            integer(int64), intent(out), optional :: n_nan
            !! how many were excluded as NaN and were not already null.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
            integer, intent(in), optional :: threads
            !! how many threads the central-moment pass may use. Absent takes the automatic rule:
            !! the `sort_threads` setting, capped by the processors actually available and
            !! by a measured work floor, and 1 inside a caller's own parallel region. **The answer
            !! does not depend on this argument** -- the block decomposition is a function of the
            !! population size alone, so 1, 8 and a build with no OpenMP at all return the same
            !! bits. It is a speed control and never an accuracy one.
        end subroutine sum_col
        !> `pf_mean` over a scalar numeric `parquet_column`.
        !>
        !> Dispatched on the column's kind at run time, and **scalar numeric columns only**:
        !> int32, int64, float32, float64 and logical. A string, temporal or container column
        !> aborts naming its kind, and so does a column more than one element wide -- flattening
        !> a vector column into one population is a different statistic and nobody should get it
        !> by accident. **The column's own validity is the only source of nullness**, so passing
        !> `is_valid=` alongside one aborts rather than silently preferring one of two sources.
        module subroutine mean_col(values, m, is_valid, weights, skipnan, n_null, n_nan, ok, threads)
            type(parquet_column), intent(in) :: values !! the column; scalar numeric kinds only.
            real(real64), intent(out) :: m !! the mean; NaN when the population is empty.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight, non-negative. A ZERO weight removes the element from the
            !! population; a negative, NaN or infinite one aborts. Absent means every weight is 1.
            logical, intent(in), optional :: skipnan
            !! .true. (the default) excludes a NaN from the population, as a null is excluded and
            !! as `pf_minmax` has always done; .false. restores numpy's propagating behaviour, in
            !! which one NaN makes every answer NaN.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            integer(int64), intent(out), optional :: n_nan
            !! how many were excluded as NaN and were not already null.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
            integer, intent(in), optional :: threads
            !! how many threads the central-moment pass may use. Absent takes the automatic rule:
            !! the `sort_threads` setting, capped by the processors actually available and
            !! by a measured work floor, and 1 inside a caller's own parallel region. **The answer
            !! does not depend on this argument** -- the block decomposition is a function of the
            !! population size alone, so 1, 8 and a build with no OpenMP at all return the same
            !! bits. It is a speed control and never an accuracy one.
        end subroutine mean_col
        !> `pf_gmean` over a scalar numeric `parquet_column`.
        !>
        !> Dispatched on the column's kind at run time, and **scalar numeric columns only**:
        !> int32, int64, float32, float64 and logical. A string, temporal or container column
        !> aborts naming its kind, and so does a column more than one element wide -- flattening
        !> a vector column into one population is a different statistic and nobody should get it
        !> by accident. **The column's own validity is the only source of nullness**, so passing
        !> `is_valid=` alongside one aborts rather than silently preferring one of two sources.
        module subroutine gmean_col(values, g, is_valid, weights, skipnan, n_null, n_nan, ok, threads)
            type(parquet_column), intent(in) :: values !! the column; scalar numeric kinds only.
            real(real64), intent(out) :: g !! the geometric mean; 0 for a zero, NaN for a negative or empty population.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight, non-negative. A ZERO weight removes the element from the
            !! population; a negative, NaN or infinite one aborts. Absent means every weight is 1.
            logical, intent(in), optional :: skipnan
            !! .true. (the default) excludes a NaN from the population, as a null is excluded and
            !! as `pf_minmax` has always done; .false. restores numpy's propagating behaviour, in
            !! which one NaN makes every answer NaN.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            integer(int64), intent(out), optional :: n_nan
            !! how many were excluded as NaN and were not already null.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
            integer, intent(in), optional :: threads
            !! how many threads the central-moment pass may use. Absent takes the automatic rule:
            !! the `sort_threads` setting, capped by the processors actually available and
            !! by a measured work floor, and 1 inside a caller's own parallel region. **The answer
            !! does not depend on this argument** -- the block decomposition is a function of the
            !! population size alone, so 1, 8 and a build with no OpenMP at all return the same
            !! bits. It is a speed control and never an accuracy one.
        end subroutine gmean_col
        !> `pf_hmean` over a scalar numeric `parquet_column`.
        !>
        !> Dispatched on the column's kind at run time, and **scalar numeric columns only**:
        !> int32, int64, float32, float64 and logical. A string, temporal or container column
        !> aborts naming its kind, and so does a column more than one element wide -- flattening
        !> a vector column into one population is a different statistic and nobody should get it
        !> by accident. **The column's own validity is the only source of nullness**, so passing
        !> `is_valid=` alongside one aborts rather than silently preferring one of two sources.
        module subroutine hmean_col(values, h, is_valid, weights, skipnan, n_null, n_nan, ok, threads)
            type(parquet_column), intent(in) :: values !! the column; scalar numeric kinds only.
            real(real64), intent(out) :: h !! the harmonic mean; 0 for a zero, NaN for a negative or empty population.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight, non-negative. A ZERO weight removes the element from the
            !! population; a negative, NaN or infinite one aborts. Absent means every weight is 1.
            logical, intent(in), optional :: skipnan
            !! .true. (the default) excludes a NaN from the population, as a null is excluded and
            !! as `pf_minmax` has always done; .false. restores numpy's propagating behaviour, in
            !! which one NaN makes every answer NaN.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            integer(int64), intent(out), optional :: n_nan
            !! how many were excluded as NaN and were not already null.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
            integer, intent(in), optional :: threads
            !! how many threads the central-moment pass may use. Absent takes the automatic rule:
            !! the `sort_threads` setting, capped by the processors actually available and
            !! by a measured work floor, and 1 inside a caller's own parallel region. **The answer
            !! does not depend on this argument** -- the block decomposition is a function of the
            !! population size alone, so 1, 8 and a build with no OpenMP at all return the same
            !! bits. It is a speed control and never an accuracy one.
        end subroutine hmean_col
        !> `pf_variance` over a scalar numeric `parquet_column`.
        !>
        !> Dispatched on the column's kind at run time, and **scalar numeric columns only**:
        !> int32, int64, float32, float64 and logical. A string, temporal or container column
        !> aborts naming its kind, and so does a column more than one element wide -- flattening
        !> a vector column into one population is a different statistic and nobody should get it
        !> by accident. **The column's own validity is the only source of nullness**, so passing
        !> `is_valid=` alongside one aborts rather than silently preferring one of two sources.
        module subroutine variance_col(values, v, is_valid, weights, weight_type, ddof, skipnan, n_null, n_nan, ok, threads)
            type(parquet_column), intent(in) :: values !! the column; scalar numeric kinds only.
            real(real64), intent(out) :: v !! the variance; NaN when `n_valid <= ddof`.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight, non-negative. A ZERO weight removes the element from the
            !! population; a negative, NaN or infinite one aborts. Absent means every weight is 1.
            character(len=*), intent(in), optional :: weight_type
            !! "reliability" (the default) or "frequency". The two differ only when the weights are
            !! unequal: a FREQUENCY weight of 3 says the value occurred three times, so the count
            !! `ddof` is charged against is `sum(weights)`; a RELIABILITY weight says the value is
            !! that much more precise, so the count is Kish's effective size
            !! `sum(weights)**2 / sum(weights**2)`. Any other token aborts, listing both.
            integer, intent(in), optional :: ddof
            !! delta degrees of freedom; **1 by default** -- the sample variance, as pandas
            !! returns. numpy's default is 0, so pass `ddof=0` to reproduce `np.var`/`np.std`.
            !! A plain default-kind integer deliberately: it is bounded by the population size,
            !! so there is no int64 form to provide. `ddof >= n_valid` gives NaN, not a division.
            logical, intent(in), optional :: skipnan
            !! .true. (the default) excludes a NaN from the population, as a null is excluded and
            !! as `pf_minmax` has always done; .false. restores numpy's propagating behaviour, in
            !! which one NaN makes every answer NaN.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            integer(int64), intent(out), optional :: n_nan
            !! how many were excluded as NaN and were not already null.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
            integer, intent(in), optional :: threads
            !! how many threads the central-moment pass may use. Absent takes the automatic rule:
            !! the `sort_threads` setting, capped by the processors actually available and
            !! by a measured work floor, and 1 inside a caller's own parallel region. **The answer
            !! does not depend on this argument** -- the block decomposition is a function of the
            !! population size alone, so 1, 8 and a build with no OpenMP at all return the same
            !! bits. It is a speed control and never an accuracy one.
        end subroutine variance_col
        !> `pf_stddev` over a scalar numeric `parquet_column`.
        !>
        !> Dispatched on the column's kind at run time, and **scalar numeric columns only**:
        !> int32, int64, float32, float64 and logical. A string, temporal or container column
        !> aborts naming its kind, and so does a column more than one element wide -- flattening
        !> a vector column into one population is a different statistic and nobody should get it
        !> by accident. **The column's own validity is the only source of nullness**, so passing
        !> `is_valid=` alongside one aborts rather than silently preferring one of two sources.
        module subroutine stddev_col(values, sd, is_valid, weights, weight_type, ddof, skipnan, n_null, n_nan, ok, threads)
            type(parquet_column), intent(in) :: values !! the column; scalar numeric kinds only.
            real(real64), intent(out) :: sd !! the standard deviation; NaN when the variance is.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight, non-negative. A ZERO weight removes the element from the
            !! population; a negative, NaN or infinite one aborts. Absent means every weight is 1.
            character(len=*), intent(in), optional :: weight_type
            !! "reliability" (the default) or "frequency". The two differ only when the weights are
            !! unequal: a FREQUENCY weight of 3 says the value occurred three times, so the count
            !! `ddof` is charged against is `sum(weights)`; a RELIABILITY weight says the value is
            !! that much more precise, so the count is Kish's effective size
            !! `sum(weights)**2 / sum(weights**2)`. Any other token aborts, listing both.
            integer, intent(in), optional :: ddof
            !! delta degrees of freedom; **1 by default** -- the sample variance, as pandas
            !! returns. numpy's default is 0, so pass `ddof=0` to reproduce `np.var`/`np.std`.
            !! A plain default-kind integer deliberately: it is bounded by the population size,
            !! so there is no int64 form to provide. `ddof >= n_valid` gives NaN, not a division.
            logical, intent(in), optional :: skipnan
            !! .true. (the default) excludes a NaN from the population, as a null is excluded and
            !! as `pf_minmax` has always done; .false. restores numpy's propagating behaviour, in
            !! which one NaN makes every answer NaN.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            integer(int64), intent(out), optional :: n_nan
            !! how many were excluded as NaN and were not already null.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
            integer, intent(in), optional :: threads
            !! how many threads the central-moment pass may use. Absent takes the automatic rule:
            !! the `sort_threads` setting, capped by the processors actually available and
            !! by a measured work floor, and 1 inside a caller's own parallel region. **The answer
            !! does not depend on this argument** -- the block decomposition is a function of the
            !! population size alone, so 1, 8 and a build with no OpenMP at all return the same
            !! bits. It is a speed control and never an accuracy one.
        end subroutine stddev_col
        !> `pf_sem` over a scalar numeric `parquet_column`.
        !>
        !> Dispatched on the column's kind at run time, and **scalar numeric columns only**:
        !> int32, int64, float32, float64 and logical. A string, temporal or container column
        !> aborts naming its kind, and so does a column more than one element wide -- flattening
        !> a vector column into one population is a different statistic and nobody should get it
        !> by accident. **The column's own validity is the only source of nullness**, so passing
        !> `is_valid=` alongside one aborts rather than silently preferring one of two sources.
        module subroutine sem_col(values, se, is_valid, weights, weight_type, ddof, skipnan, n_null, n_nan, ok, threads)
            type(parquet_column), intent(in) :: values !! the column; scalar numeric kinds only.
            real(real64), intent(out) :: se !! the standard error; NaN when the standard deviation is.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight, non-negative. A ZERO weight removes the element from the
            !! population; a negative, NaN or infinite one aborts. Absent means every weight is 1.
            character(len=*), intent(in), optional :: weight_type
            !! "reliability" (the default) or "frequency". The two differ only when the weights are
            !! unequal: a FREQUENCY weight of 3 says the value occurred three times, so the count
            !! `ddof` is charged against is `sum(weights)`; a RELIABILITY weight says the value is
            !! that much more precise, so the count is Kish's effective size
            !! `sum(weights)**2 / sum(weights**2)`. Any other token aborts, listing both.
            integer, intent(in), optional :: ddof
            !! delta degrees of freedom; **1 by default** -- the sample variance, as pandas
            !! returns. numpy's default is 0, so pass `ddof=0` to reproduce `np.var`/`np.std`.
            !! A plain default-kind integer deliberately: it is bounded by the population size,
            !! so there is no int64 form to provide. `ddof >= n_valid` gives NaN, not a division.
            logical, intent(in), optional :: skipnan
            !! .true. (the default) excludes a NaN from the population, as a null is excluded and
            !! as `pf_minmax` has always done; .false. restores numpy's propagating behaviour, in
            !! which one NaN makes every answer NaN.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            integer(int64), intent(out), optional :: n_nan
            !! how many were excluded as NaN and were not already null.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
            integer, intent(in), optional :: threads
            !! how many threads the central-moment pass may use. Absent takes the automatic rule:
            !! the `sort_threads` setting, capped by the processors actually available and
            !! by a measured work floor, and 1 inside a caller's own parallel region. **The answer
            !! does not depend on this argument** -- the block decomposition is a function of the
            !! population size alone, so 1, 8 and a build with no OpenMP at all return the same
            !! bits. It is a speed control and never an accuracy one.
        end subroutine sem_col
        !> `pf_skewness` over a scalar numeric `parquet_column`.
        !>
        !> Dispatched on the column's kind at run time, and **scalar numeric columns only**:
        !> int32, int64, float32, float64 and logical. A string, temporal or container column
        !> aborts naming its kind, and so does a column more than one element wide -- flattening
        !> a vector column into one population is a different statistic and nobody should get it
        !> by accident. **The column's own validity is the only source of nullness**, so passing
        !> `is_valid=` alongside one aborts rather than silently preferring one of two sources.
        module subroutine skewness_col(values, g, is_valid, weights, weight_type, bias, skipnan, n_null, n_nan, ok, threads)
            type(parquet_column), intent(in) :: values !! the column; scalar numeric kinds only.
            real(real64), intent(out) :: g !! the skewness.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight, non-negative. A ZERO weight removes the element from the
            !! population; a negative, NaN or infinite one aborts. Absent means every weight is 1.
            character(len=*), intent(in), optional :: weight_type
            !! "reliability" (the default) or "frequency". The two differ only when the weights are
            !! unequal: a FREQUENCY weight of 3 says the value occurred three times, so the count
            !! `ddof` is charged against is `sum(weights)`; a RELIABILITY weight says the value is
            !! that much more precise, so the count is Kish's effective size
            !! `sum(weights)**2 / sum(weights**2)`. Any other token aborts, listing both.
            logical, intent(in), optional :: bias
            !! .false. by default: the bias-CORRECTED G1/G2, which is what pandas returns.
            !! .true. gives the uncorrected g1/g2, which is what scipy returns by default.
            logical, intent(in), optional :: skipnan
            !! .true. (the default) excludes a NaN from the population, as a null is excluded and
            !! as `pf_minmax` has always done; .false. restores numpy's propagating behaviour, in
            !! which one NaN makes every answer NaN.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            integer(int64), intent(out), optional :: n_nan
            !! how many were excluded as NaN and were not already null.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
            integer, intent(in), optional :: threads
            !! how many threads the central-moment pass may use. Absent takes the automatic rule:
            !! the `sort_threads` setting, capped by the processors actually available and
            !! by a measured work floor, and 1 inside a caller's own parallel region. **The answer
            !! does not depend on this argument** -- the block decomposition is a function of the
            !! population size alone, so 1, 8 and a build with no OpenMP at all return the same
            !! bits. It is a speed control and never an accuracy one.
        end subroutine skewness_col
        !> `pf_kurtosis` over a scalar numeric `parquet_column`.
        !>
        !> Dispatched on the column's kind at run time, and **scalar numeric columns only**:
        !> int32, int64, float32, float64 and logical. A string, temporal or container column
        !> aborts naming its kind, and so does a column more than one element wide -- flattening
        !> a vector column into one population is a different statistic and nobody should get it
        !> by accident. **The column's own validity is the only source of nullness**, so passing
        !> `is_valid=` alongside one aborts rather than silently preferring one of two sources.
        module subroutine kurtosis_col(values, k, is_valid, weights, weight_type, bias, excess, skipnan, n_null, n_nan, ok, threads)
            type(parquet_column), intent(in) :: values !! the column; scalar numeric kinds only.
            real(real64), intent(out) :: k !! the kurtosis, excess by default.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight, non-negative. A ZERO weight removes the element from the
            !! population; a negative, NaN or infinite one aborts. Absent means every weight is 1.
            character(len=*), intent(in), optional :: weight_type
            !! "reliability" (the default) or "frequency". The two differ only when the weights are
            !! unequal: a FREQUENCY weight of 3 says the value occurred three times, so the count
            !! `ddof` is charged against is `sum(weights)`; a RELIABILITY weight says the value is
            !! that much more precise, so the count is Kish's effective size
            !! `sum(weights)**2 / sum(weights**2)`. Any other token aborts, listing both.
            logical, intent(in), optional :: bias
            !! .false. by default: the bias-CORRECTED G1/G2, which is what pandas returns.
            !! .true. gives the uncorrected g1/g2, which is what scipy returns by default.
            logical, intent(in), optional :: excess
            !! .true. by default: EXCESS kurtosis, which is 0 for a normal population.
            !! .false. adds 3 back, giving the raw fourth-moment ratio.
            logical, intent(in), optional :: skipnan
            !! .true. (the default) excludes a NaN from the population, as a null is excluded and
            !! as `pf_minmax` has always done; .false. restores numpy's propagating behaviour, in
            !! which one NaN makes every answer NaN.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            integer(int64), intent(out), optional :: n_nan
            !! how many were excluded as NaN and were not already null.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
            integer, intent(in), optional :: threads
            !! how many threads the central-moment pass may use. Absent takes the automatic rule:
            !! the `sort_threads` setting, capped by the processors actually available and
            !! by a measured work floor, and 1 inside a caller's own parallel region. **The answer
            !! does not depend on this argument** -- the block decomposition is a function of the
            !! population size alone, so 1, 8 and a build with no OpenMP at all return the same
            !! bits. It is a speed control and never an accuracy one.
        end subroutine kurtosis_col
        !> `pf_moments` over a scalar numeric `parquet_column`: every tier-A quantity in one pair of passes.
        !>
        !> Dispatched on the column's kind at run time, and **scalar numeric columns only**:
        !> int32, int64, float32, float64 and logical. A string, temporal or container column
        !> aborts naming its kind, and so does a column more than one element wide -- flattening
        !> a vector column into one population is a different statistic and nobody should get it
        !> by accident. **The column's own validity is the only source of nullness**, so passing
        !> `is_valid=` alongside one aborts rather than silently preferring one of two sources.
        module subroutine moments_col(values, n_valid, mean, variance, stddev, sem, skewness, kurtosis, vsum, vmin, &
                vmax, is_valid, weights, weight_type, ddof, bias, excess, &
                skipnan, n_null, n_nan, ok, threads)
            type(parquet_column), intent(in) :: values !! the column; scalar numeric kinds only.
            integer(int64), intent(out), optional :: n_valid !! how many elements were used.
            real(real64), intent(out), optional :: mean !! the mean; NaN when empty.
            real(real64), intent(out), optional :: variance !! the variance; NaN when undefined.
            real(real64), intent(out), optional :: stddev !! the standard deviation.
            real(real64), intent(out), optional :: sem !! the standard error of the mean.
            real(real64), intent(out), optional :: skewness !! the third standardised moment.
            real(real64), intent(out), optional :: kurtosis !! the fourth, excess by default.
            real(real64), intent(out), optional :: vsum !! the pairwise sum; 0 when empty.
            real(real64), intent(out), optional :: vmin !! the smallest value; NaN when empty.
            real(real64), intent(out), optional :: vmax !! the largest value; NaN when empty.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight, non-negative. A ZERO weight removes the element from the
            !! population; a negative, NaN or infinite one aborts. Absent means every weight is 1.
            character(len=*), intent(in), optional :: weight_type
            !! "reliability" (the default) or "frequency". The two differ only when the weights are
            !! unequal: a FREQUENCY weight of 3 says the value occurred three times, so the count
            !! `ddof` is charged against is `sum(weights)`; a RELIABILITY weight says the value is
            !! that much more precise, so the count is Kish's effective size
            !! `sum(weights)**2 / sum(weights**2)`. Any other token aborts, listing both.
            integer, intent(in), optional :: ddof
            !! delta degrees of freedom; **1 by default** -- the sample variance, as pandas
            !! returns. numpy's default is 0, so pass `ddof=0` to reproduce `np.var`/`np.std`.
            !! A plain default-kind integer deliberately: it is bounded by the population size,
            !! so there is no int64 form to provide. `ddof >= n_valid` gives NaN, not a division.
            logical, intent(in), optional :: bias
            !! .false. by default: the bias-CORRECTED G1/G2, which is what pandas returns.
            !! .true. gives the uncorrected g1/g2, which is what scipy returns by default.
            logical, intent(in), optional :: excess
            !! .true. by default: EXCESS kurtosis, which is 0 for a normal population.
            !! .false. adds 3 back, giving the raw fourth-moment ratio.
            logical, intent(in), optional :: skipnan
            !! .true. (the default) excludes a NaN from the population, as a null is excluded and
            !! as `pf_minmax` has always done; .false. restores numpy's propagating behaviour, in
            !! which one NaN makes every answer NaN.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            integer(int64), intent(out), optional :: n_nan
            !! how many were excluded as NaN and were not already null.
            logical, intent(out), optional :: ok
            !! .false. when any output that was ASKED for came back a quiet NaN -- an empty
            !! population, a `ddof` that leaves nothing to divide by, a constant population for
            !! the shape statistics, or a non-finite value under `skipnan = .false.`. Only the
            !! PRESENT outputs are tested, which is the only reading that works here: `vsum` over
            !! an empty population is a correct `0`, and testing an output the caller never asked
            !! for would report a failure that is not one. `n_valid`, `n_null` and `n_nan` are
            !! counts and are always defined, so they are not tested.
            !!
            !! An infinity is NOT a failure: `mean` answers `+Inf` over a population containing
            !! one, exactly as `pf_mean` and numpy do, and `ok` stays .true. for it while the
            !! variance -- genuinely a NaN there -- turns it .false. if it was asked for.
            integer, intent(in), optional :: threads
            !! how many threads the central-moment pass may use. Absent takes the automatic rule:
            !! the `sort_threads` setting, capped by the processors actually available and
            !! by a measured work floor, and 1 inside a caller's own parallel region. **The answer
            !! does not depend on this argument** -- the block decomposition is a function of the
            !! population size alone, so 1, 8 and a build with no OpenMP at all return the same
            !! bits. It is a speed control and never an accuracy one.
        end subroutine moments_col
        !> `pf_median` over a scalar numeric `parquet_column`.
        !>
        !> Dispatched on the column's kind at run time, and **scalar numeric columns only**:
        !> int32, int64, float32, float64 and logical. A string, temporal or container column
        !> aborts naming its kind, and so does a column more than one element wide -- flattening
        !> a vector column into one population is a different statistic and nobody should get it
        !> by accident. **The column's own validity is the only source of nullness**, so passing
        !> `is_valid=` alongside one aborts rather than silently preferring one of two sources.
        module subroutine median_col(values, med, is_valid, weights, weight_type, skipnan, method, n_null, n_nan, &
                ok, threads)
            type(parquet_column), intent(in) :: values !! the column; scalar numeric kinds only.
            real(real64), intent(out) :: med
            !! the median; NaN when the population is empty.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight, non-negative. A ZERO weight removes the element from the
            !! population; a negative, NaN or infinite one aborts. Absent means every weight is 1.
            character(len=*), intent(in), optional :: weight_type
            !! "reliability" (the default) or "frequency". The two differ only when the weights are
            !! unequal: a FREQUENCY weight of 3 says the value occurred three times, so the count
            !! `ddof` is charged against is `sum(weights)`; a RELIABILITY weight says the value is
            !! that much more precise, so the count is Kish's effective size
            !! `sum(weights)**2 / sum(weights**2)`. Any other token aborts, listing both.
            logical, intent(in), optional :: skipnan
            !! .true. (the default) excludes a NaN from the population, as a null is excluded and
            !! as `pf_minmax` has always done; .false. restores numpy's propagating behaviour, in
            !! which one NaN makes every answer NaN.
            character(len=*), intent(in), optional :: method
            !! how a fractional position between two order statistics is resolved, in numpy's
            !! spelling: "linear" (the default -- Hyndman-Fan type 7, what numpy and pandas do),
            !! "lower", "higher", "nearest", "midpoint", or "inverted_cdf" (a step function on the
            !! plain cumulative scale, and the only token numpy itself accepts weights with). Any
            !! other token aborts, listing all six. Matched case-insensitively.
            !! **`weight_type="frequency"` changes the default to "inverted_cdf"**, because that
            !! token IS frequency expansion; an explicit `method=` still wins.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            integer(int64), intent(out), optional :: n_nan
            !! how many were excluded as NaN and were not already null.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
            integer, intent(in), optional :: threads
            !! how many threads the ORDERING may use -- this is an order statistic and has no
            !! central-moment pass, so the work `threads=` divides is `pf_argsort`'s. Absent takes
            !! the automatic rule: the `sort_threads` setting, capped by the processors
            !! actually available and by a measured work floor, and 1 inside a caller's own
            !! parallel region. **The answer does not depend on this argument** -- a sort is a
            !! permutation and the selection off it is exact, so 1, 8 and a build with no OpenMP
            !! at all return the same bits. It is a speed control and never an accuracy one.
        end subroutine median_col
        !> `pf_quantile` over a scalar numeric `parquet_column`.
        !>
        !> Dispatched on the column's kind at run time, and **scalar numeric columns only**:
        !> int32, int64, float32, float64 and logical. A string, temporal or container column
        !> aborts naming its kind, and so does a column more than one element wide -- flattening
        !> a vector column into one population is a different statistic and nobody should get it
        !> by accident. **The column's own validity is the only source of nullness**, so passing
        !> `is_valid=` alongside one aborts rather than silently preferring one of two sources.
        module subroutine quantile_col(values, p, q, is_valid, weights, weight_type, skipnan, method, n_null, n_nan, &
                ok, threads)
            type(parquet_column), intent(in) :: values !! the column; scalar numeric kinds only.
            real(real64), intent(in) :: p
            !! the probability, on a 0-1 scale rather than 0-100. Outside [0, 1] aborts.
            real(real64), intent(out) :: q
            !! the quantile; NaN when the population is empty.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight, non-negative. A ZERO weight removes the element from the
            !! population; a negative, NaN or infinite one aborts. Absent means every weight is 1.
            character(len=*), intent(in), optional :: weight_type
            !! "reliability" (the default) or "frequency". The two differ only when the weights are
            !! unequal: a FREQUENCY weight of 3 says the value occurred three times, so the count
            !! `ddof` is charged against is `sum(weights)`; a RELIABILITY weight says the value is
            !! that much more precise, so the count is Kish's effective size
            !! `sum(weights)**2 / sum(weights**2)`. Any other token aborts, listing both.
            logical, intent(in), optional :: skipnan
            !! .true. (the default) excludes a NaN from the population, as a null is excluded and
            !! as `pf_minmax` has always done; .false. restores numpy's propagating behaviour, in
            !! which one NaN makes every answer NaN.
            character(len=*), intent(in), optional :: method
            !! how a fractional position between two order statistics is resolved, in numpy's
            !! spelling: "linear" (the default -- Hyndman-Fan type 7, what numpy and pandas do),
            !! "lower", "higher", "nearest", "midpoint", or "inverted_cdf" (a step function on the
            !! plain cumulative scale, and the only token numpy itself accepts weights with). Any
            !! other token aborts, listing all six. Matched case-insensitively.
            !! **`weight_type="frequency"` changes the default to "inverted_cdf"**, because that
            !! token IS frequency expansion; an explicit `method=` still wins.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            integer(int64), intent(out), optional :: n_nan
            !! how many were excluded as NaN and were not already null.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
            integer, intent(in), optional :: threads
            !! how many threads the ORDERING may use -- this is an order statistic and has no
            !! central-moment pass, so the work `threads=` divides is `pf_argsort`'s. Absent takes
            !! the automatic rule: the `sort_threads` setting, capped by the processors
            !! actually available and by a measured work floor, and 1 inside a caller's own
            !! parallel region. **The answer does not depend on this argument** -- a sort is a
            !! permutation and the selection off it is exact, so 1, 8 and a build with no OpenMP
            !! at all return the same bits. It is a speed control and never an accuracy one.
        end subroutine quantile_col
        !> `pf_quantiles` over a scalar numeric `parquet_column`.
        !>
        !> Dispatched on the column's kind at run time, and **scalar numeric columns only**:
        !> int32, int64, float32, float64 and logical. A string, temporal or container column
        !> aborts naming its kind, and so does a column more than one element wide -- flattening
        !> a vector column into one population is a different statistic and nobody should get it
        !> by accident. **The column's own validity is the only source of nullness**, so passing
        !> `is_valid=` alongside one aborts rather than silently preferring one of two sources.
        module subroutine quantiles_col(values, probs, out, is_valid, weights, weight_type, skipnan, method, n_null, &
                n_nan, ok, threads)
            type(parquet_column), intent(in) :: values !! the column; scalar numeric kinds only.
            real(real64), intent(in) :: probs(:)
            !! the probabilities, each on a 0-1 scale. Any outside [0, 1] aborts. Need not be sorted.
            real(real64), intent(out) :: out(:)
            !! one quantile per entry of `probs`, same size. ONE sort serves all of them.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight, non-negative. A ZERO weight removes the element from the
            !! population; a negative, NaN or infinite one aborts. Absent means every weight is 1.
            character(len=*), intent(in), optional :: weight_type
            !! "reliability" (the default) or "frequency". The two differ only when the weights are
            !! unequal: a FREQUENCY weight of 3 says the value occurred three times, so the count
            !! `ddof` is charged against is `sum(weights)`; a RELIABILITY weight says the value is
            !! that much more precise, so the count is Kish's effective size
            !! `sum(weights)**2 / sum(weights**2)`. Any other token aborts, listing both.
            logical, intent(in), optional :: skipnan
            !! .true. (the default) excludes a NaN from the population, as a null is excluded and
            !! as `pf_minmax` has always done; .false. restores numpy's propagating behaviour, in
            !! which one NaN makes every answer NaN.
            character(len=*), intent(in), optional :: method
            !! how a fractional position between two order statistics is resolved, in numpy's
            !! spelling: "linear" (the default -- Hyndman-Fan type 7, what numpy and pandas do),
            !! "lower", "higher", "nearest", "midpoint", or "inverted_cdf" (a step function on the
            !! plain cumulative scale, and the only token numpy itself accepts weights with). Any
            !! other token aborts, listing all six. Matched case-insensitively.
            !! **`weight_type="frequency"` changes the default to "inverted_cdf"**, because that
            !! token IS frequency expansion; an explicit `method=` still wins.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            integer(int64), intent(out), optional :: n_nan
            !! how many were excluded as NaN and were not already null.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
            integer, intent(in), optional :: threads
            !! how many threads the ORDERING may use -- this is an order statistic and has no
            !! central-moment pass, so the work `threads=` divides is `pf_argsort`'s. Absent takes
            !! the automatic rule: the `sort_threads` setting, capped by the processors
            !! actually available and by a measured work floor, and 1 inside a caller's own
            !! parallel region. **The answer does not depend on this argument** -- a sort is a
            !! permutation and the selection off it is exact, so 1, 8 and a build with no OpenMP
            !! at all return the same bits. It is a speed control and never an accuracy one.
        end subroutine quantiles_col
        !> `pf_iqr` over a scalar numeric `parquet_column`.
        !>
        !> Dispatched on the column's kind at run time, and **scalar numeric columns only**:
        !> int32, int64, float32, float64 and logical. A string, temporal or container column
        !> aborts naming its kind, and so does a column more than one element wide -- flattening
        !> a vector column into one population is a different statistic and nobody should get it
        !> by accident. **The column's own validity is the only source of nullness**, so passing
        !> `is_valid=` alongside one aborts rather than silently preferring one of two sources.
        module subroutine iqr_col(values, r, is_valid, weights, weight_type, skipnan, method, n_null, n_nan, &
                ok, threads)
            type(parquet_column), intent(in) :: values !! the column; scalar numeric kinds only.
            real(real64), intent(out) :: r
            !! the interquartile range, `q(0.75) - q(0.25)`; NaN when either quartile is.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight, non-negative. A ZERO weight removes the element from the
            !! population; a negative, NaN or infinite one aborts. Absent means every weight is 1.
            character(len=*), intent(in), optional :: weight_type
            !! "reliability" (the default) or "frequency". The two differ only when the weights are
            !! unequal: a FREQUENCY weight of 3 says the value occurred three times, so the count
            !! `ddof` is charged against is `sum(weights)`; a RELIABILITY weight says the value is
            !! that much more precise, so the count is Kish's effective size
            !! `sum(weights)**2 / sum(weights**2)`. Any other token aborts, listing both.
            logical, intent(in), optional :: skipnan
            !! .true. (the default) excludes a NaN from the population, as a null is excluded and
            !! as `pf_minmax` has always done; .false. restores numpy's propagating behaviour, in
            !! which one NaN makes every answer NaN.
            character(len=*), intent(in), optional :: method
            !! how a fractional position between two order statistics is resolved, in numpy's
            !! spelling: "linear" (the default -- Hyndman-Fan type 7, what numpy and pandas do),
            !! "lower", "higher", "nearest", "midpoint", or "inverted_cdf" (a step function on the
            !! plain cumulative scale, and the only token numpy itself accepts weights with). Any
            !! other token aborts, listing all six. Matched case-insensitively.
            !! **`weight_type="frequency"` changes the default to "inverted_cdf"**, because that
            !! token IS frequency expansion; an explicit `method=` still wins.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            integer(int64), intent(out), optional :: n_nan
            !! how many were excluded as NaN and were not already null.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
            integer, intent(in), optional :: threads
            !! how many threads the ORDERING may use -- this is an order statistic and has no
            !! central-moment pass, so the work `threads=` divides is `pf_argsort`'s. Absent takes
            !! the automatic rule: the `sort_threads` setting, capped by the processors
            !! actually available and by a measured work floor, and 1 inside a caller's own
            !! parallel region. **The answer does not depend on this argument** -- a sort is a
            !! permutation and the selection off it is exact, so 1, 8 and a build with no OpenMP
            !! at all return the same bits. It is a speed control and never an accuracy one.
        end subroutine iqr_col
        !> `pf_trim_mean` over a scalar numeric `parquet_column`.
        !>
        !> Dispatched on the column's kind at run time, and **scalar numeric columns only**:
        !> int32, int64, float32, float64 and logical. A string, temporal or container column
        !> aborts naming its kind, and so does a column more than one element wide -- flattening
        !> a vector column into one population is a different statistic and nobody should get it
        !> by accident. **The column's own validity is the only source of nullness**, so passing
        !> `is_valid=` alongside one aborts rather than silently preferring one of two sources.
        module subroutine trim_mean_col(values, prop, m, is_valid, weights, skipnan, n_null, n_nan, ok, threads)
            type(parquet_column), intent(in) :: values !! the column; scalar numeric kinds only.
            real(real64), intent(in) :: prop
            !! the share trimmed from EACH tail, so `prop=0.1` drops 10% at each end and averages the
            !! middle 80%. Must satisfy `0 <= prop < 0.5`; anything else aborts, because a caller who asks
            !! to trim everything has made a mistake rather than expressed a preference.
            real(real64), intent(out) :: m
            !! the trimmed mean; NaN when nothing survives the trim.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight, non-negative. A ZERO weight removes the element from the
            !! population; a negative, NaN or infinite one aborts. Absent means every weight is 1.
            logical, intent(in), optional :: skipnan
            !! .true. (the default) excludes a NaN from the population, as a null is excluded and
            !! as `pf_minmax` has always done; .false. restores numpy's propagating behaviour, in
            !! which one NaN makes every answer NaN.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            integer(int64), intent(out), optional :: n_nan
            !! how many were excluded as NaN and were not already null.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
            integer, intent(in), optional :: threads
            !! how many threads the ORDERING may use -- this is an order statistic and has no
            !! central-moment pass, so the work `threads=` divides is `pf_argsort`'s. Absent takes
            !! the automatic rule: the `sort_threads` setting, capped by the processors
            !! actually available and by a measured work floor, and 1 inside a caller's own
            !! parallel region. **The answer does not depend on this argument** -- a sort is a
            !! permutation and the selection off it is exact, so 1, 8 and a build with no OpenMP
            !! at all return the same bits. It is a speed control and never an accuracy one.
        end subroutine trim_mean_col
        !> `pf_percentile_of_score` over a scalar numeric `parquet_column`.
        !>
        !> Dispatched on the column's kind at run time, and **scalar numeric columns only**:
        !> int32, int64, float32, float64 and logical. A string, temporal or container column
        !> aborts naming its kind, and so does a column more than one element wide -- flattening
        !> a vector column into one population is a different statistic and nobody should get it
        !> by accident. **The column's own validity is the only source of nullness**, so passing
        !> `is_valid=` alongside one aborts rather than silently preferring one of two sources.
        module subroutine percentile_of_score_col(values, score, p, is_valid, weights, skipnan, kind, n_null, n_nan, ok, threads)
            type(parquet_column), intent(in) :: values !! the column; scalar numeric kinds only.
            real(real64), intent(in) :: score
            !! the value to locate. NaN or infinite aborts -- unlike a NaN in the population, which is an
            !! ordinary data condition, a NaN score can only come from the caller's own arithmetic.
            real(real64), intent(out) :: p
            !! the share of the population at or below `score`, on a 0-1 scale; NaN when empty.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight, non-negative. A ZERO weight removes the element from the
            !! population; a negative, NaN or infinite one aborts. Absent means every weight is 1.
            logical, intent(in), optional :: skipnan
            !! .true. (the default) excludes a NaN from the population, as a null is excluded and
            !! as `pf_minmax` has always done; .false. restores numpy's propagating behaviour, in
            !! which one NaN makes every answer NaN.
            character(len=*), intent(in), optional :: kind
            !! which convention counts a value EQUAL to `score`, in scipy's spelling: "rank" (the
            !! default; ties count half), "weak" (a value <= score counts), "strict" (only < score)
            !! or "mean" (the average of weak and strict). Any other token aborts, listing all
            !! four. Matched case-insensitively.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            integer(int64), intent(out), optional :: n_nan
            !! how many were excluded as NaN and were not already null.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
            integer, intent(in), optional :: threads
            !! how many threads the ORDERING may use -- this is an order statistic and has no
            !! central-moment pass, so the work `threads=` divides is `pf_argsort`'s. Absent takes
            !! the automatic rule: the `sort_threads` setting, capped by the processors
            !! actually available and by a measured work floor, and 1 inside a caller's own
            !! parallel region. **The answer does not depend on this argument** -- a sort is a
            !! permutation and the selection off it is exact, so 1, 8 and a build with no OpenMP
            !! at all return the same bits. It is a speed control and never an accuracy one.
        end subroutine percentile_of_score_col
        !> `pf_mad` over a scalar numeric `parquet_column`.
        !>
        !> Dispatched on the column's kind at run time, and **scalar numeric columns only**:
        !> int32, int64, float32, float64 and logical. A string, temporal or container column
        !> aborts naming its kind, and so does a column more than one element wide -- flattening
        !> a vector column into one population is a different statistic and nobody should get it
        !> by accident. **The column's own validity is the only source of nullness**, so passing
        !> `is_valid=` alongside one aborts rather than silently preferring one of two sources.
        module subroutine mad_col(values, m, is_valid, weights, skipnan, scale, center, n_null, n_nan, ok, &
                threads)
            type(parquet_column), intent(in) :: values !! the column; scalar numeric kinds only.
            real(real64), intent(out) :: m
            !! the median absolute deviation, scaled unless `scale="raw"`; NaN when the population is
            !! empty.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight, non-negative. A ZERO weight removes the element from the
            !! population; a negative, NaN or infinite one aborts. Absent means every weight is 1.
            logical, intent(in), optional :: skipnan
            !! .true. (the default) excludes a NaN from the population, as a null is excluded and
            !! as `pf_minmax` has always done; .false. restores numpy's propagating behaviour, in
            !! which one NaN makes every answer NaN.
            character(len=*), intent(in), optional :: scale
            !! "normal" (the default) divides the raw deviation by `Phi^-1(3/4)`, which makes the
            !! result a consistent estimator of the standard deviation for Gaussian data --
            !! scipy's `median_abs_deviation(scale='normal')`, and what an astronomy script means
            !! by "the MAD". That is a multiplication by 1.4826022185056018, and **not** by the
            !! rounded 1.4826 the textbooks quote, which differs by 1.5e-06 relative. "raw" is the
            !! unscaled median of |x - center|. Any other token aborts, naming both. Matched
            !! case-insensitively.
            real(real64), intent(in), optional :: center
            !! the centre the deviations are taken about. Absent uses the population's own median,
            !! which is what scipy does. Supplying one skips ONE selection and not the ordering:
            !! the median of |x - center| still has to be found. A NaN or infinite centre ABORTS
            !! -- unlike a NaN value, which is an ordinary data condition, a NaN centre can only
            !! come from the caller's own arithmetic.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            integer(int64), intent(out), optional :: n_nan
            !! how many were excluded as NaN and were not already null.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
            integer, intent(in), optional :: threads
            !! how many threads the ORDERING may use -- this is an order statistic and has no
            !! central-moment pass, so the work `threads=` divides is `pf_argsort`'s. Absent takes
            !! the automatic rule: the `sort_threads` setting, capped by the processors
            !! actually available and by a measured work floor, and 1 inside a caller's own
            !! parallel region. **The answer does not depend on this argument** -- a sort is a
            !! permutation and the selection off it is exact, so 1, 8 and a build with no OpenMP
            !! at all return the same bits. It is a speed control and never an accuracy one.
        end subroutine mad_col
        !> `pf_describe` over a scalar numeric `parquet_column`.
        !>
        !> Dispatched on the column's kind at run time, and **scalar numeric columns only**:
        !> int32, int64, float32, float64 and logical. A string, temporal or container column
        !> aborts naming its kind, and so does a column more than one element wide -- flattening
        !> a vector column into one population is a different statistic and nobody should get it
        !> by accident. **The column's own validity is the only source of nullness**, so passing
        !> `is_valid=` alongside one aborts rather than silently preferring one of two sources.
        module subroutine describe_col(values, s, is_valid, weights, weight_type, skipnan, ok, threads)
            type(parquet_column), intent(in) :: values !! the column; scalar numeric kinds only.
            type(pf_stats), intent(out) :: s
            !! the filled summary object: tier A computed and tier B already ordered, so every query on it
            !! afterwards is a read.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight, non-negative. A ZERO weight removes the element from the
            !! population; a negative, NaN or infinite one aborts. Absent means every weight is 1.
            character(len=*), intent(in), optional :: weight_type
            !! "reliability" (the default) or "frequency". The two differ only when the weights are
            !! unequal: a FREQUENCY weight of 3 says the value occurred three times, so the count
            !! `ddof` is charged against is `sum(weights)`; a RELIABILITY weight says the value is
            !! that much more precise, so the count is Kish's effective size
            !! `sum(weights)**2 / sum(weights**2)`. Any other token aborts, listing both.
            logical, intent(in), optional :: skipnan
            !! .true. (the default) excludes a NaN from the population, as a null is excluded and
            !! as `pf_minmax` has always done; .false. restores numpy's propagating behaviour, in
            !! which one NaN makes every answer NaN.
            logical, intent(out), optional :: ok
            !! .false. when the object describes a population with no defined moments -- one that
            !! is empty after the exclusions, or that kept a NaN under `skipnan = .false.` In
            !! either case every tier-A query on `s` answers a quiet NaN. The object itself
            !! carries the counts (`%n_valid()`, `%n_null()`, `%n_nan()`), so this is the one
            !! thing about the result a caller cannot otherwise read without picking a statistic
            !! and NaN-testing it by hand.
            integer, intent(in), optional :: threads
            !! how many threads the central-moment pass and the ordering may use -- `pf_describe`
            !! does both, which no other entry point in this module does. Absent takes the
            !! automatic rule: the `sort_threads` setting, capped by the processors
            !! actually available and by a measured work floor, and 1 inside a caller's own
            !! parallel region. **The answer does not depend on this argument** -- the block
            !! decomposition is a function of the population size alone and a sort is a
            !! permutation, so 1, 8 and a build with no OpenMP at all return the same bits. It is
            !! a speed control and never an accuracy one.
        end subroutine describe_col
        !> `pf_cov` over a scalar numeric `parquet_column`.
        module subroutine cov_col(x, y, c, is_valid, weights, weight_type, ddof, n_null, n_nan, ok)
            type(parquet_column), intent(in) :: x !! the first sample.
            type(parquet_column), intent(in) :: y !! the second sample, element for element.
            real(real64), intent(out) :: c !! the covariance; NaN when `n_valid <= ddof`.
            logical, intent(in), optional :: is_valid(:)
            !! per PAIR: .false. marks the pair as unusable. One mask over both arrays, because a
            !! two-sample statistic is taken over pairs; a caller holding a separate mask for each
            !! column passes `mask_x .and. mask_y`, which says what it does. A NaN on either side
            !! drops the pair as well, and is counted through `n_nan` rather than here.
            real(real64), intent(in), optional :: weights(:)
            !! per PAIR weight, non-negative. A ZERO weight removes the pair from the population;
            !! a negative, NaN or infinite one aborts. Absent means every weight is 1. Pearson
            !! only -- see `method`.
            character(len=*), intent(in), optional :: weight_type
            !! "reliability" (the default) or "frequency". The two differ only when the weights are
            !! unequal: a FREQUENCY weight of 3 says the value occurred three times, so the count
            !! `ddof` is charged against is `sum(weights)`; a RELIABILITY weight says the value is
            !! that much more precise, so the count is Kish's effective size
            !! `sum(weights)**2 / sum(weights**2)`. Any other token aborts, listing both.
            integer, intent(in), optional :: ddof
            !! delta degrees of freedom; **1 by default** -- the sample variance, as pandas
            !! returns. numpy's default is 0, so pass `ddof=0` to reproduce `np.var`/`np.std`.
            !! A plain default-kind integer deliberately: it is bounded by the population size,
            !! so there is no int64 form to provide. `ddof >= n_valid` gives NaN, not a division.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            integer(int64), intent(out), optional :: n_nan
            !! how many were excluded as NaN and were not already null.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
        end subroutine cov_col
        !> `pf_corr` over a scalar numeric `parquet_column`.
        module subroutine corr_col(x, y, r, is_valid, weights, method, n_null, n_nan, ok)
            type(parquet_column), intent(in) :: x !! the first sample.
            type(parquet_column), intent(in) :: y !! the second sample, element for element.
            real(real64), intent(out) :: r !! the correlation in [-1, 1]; NaN when either sample is constant.
            logical, intent(in), optional :: is_valid(:)
            !! per PAIR: .false. marks the pair as unusable. One mask over both arrays, because a
            !! two-sample statistic is taken over pairs; a caller holding a separate mask for each
            !! column passes `mask_x .and. mask_y`, which says what it does. A NaN on either side
            !! drops the pair as well, and is counted through `n_nan` rather than here.
            real(real64), intent(in), optional :: weights(:)
            !! per PAIR weight, non-negative. A ZERO weight removes the pair from the population;
            !! a negative, NaN or infinite one aborts. Absent means every weight is 1. Pearson
            !! only -- see `method`.
            character(len=*), intent(in), optional :: method
            !! "pearson" (the default) or "spearman". Spearman is Pearson over MIDRANKS: each run
            !! of equal values receives the mean of the sorted positions it spans, so it measures
            !! any monotone relationship rather than a linear one. Any other token aborts, naming
            !! both. Matched case-insensitively. **`weights` with "spearman" aborts** -- a
            !! weighted midrank is a further definitional choice and no reference library makes
            !! it.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            integer(int64), intent(out), optional :: n_nan
            !! how many were excluded as NaN and were not already null.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
        end subroutine corr_col
        !> `pf_zscore` over a scalar numeric `parquet_column`.
        module subroutine zscore_col(values, z, is_valid, ddof, skipnan, out_valid, n_null, ok)
            type(parquet_column), intent(in) :: values !! the column; scalar numeric kinds only.
            real(real64), intent(out) :: z(:)
            !! the standardised values, same size as `values`. An excluded
            !! element is a quiet NaN unless `out_valid` is present.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            integer, intent(in), optional :: ddof
            !! delta degrees of freedom; **1 by default** -- the sample variance, as pandas
            !! returns. numpy's default is 0, so pass `ddof=0` to reproduce `np.var`/`np.std`.
            !! A plain default-kind integer deliberately: it is bounded by the population size,
            !! so there is no int64 form to provide. `ddof >= n_valid` gives NaN, not a division.
            logical, intent(in), optional :: skipnan
            !! .true. (the default) excludes a NaN from the population, as a null is excluded and
            !! as `pf_minmax` has always done; .false. restores numpy's propagating behaviour, in
            !! which one NaN makes every answer NaN.
            logical, intent(out), optional :: out_valid(:)
            !! per output element: .false. where the input was excluded and the result is
            !! therefore undefined. Same size as `values`. **Absent, such an element is written as
            !! a quiet NaN instead** -- which is loud in practice, since it propagates through
            !! whatever the caller does next.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
        end subroutine zscore_col
        !> `pf_normal_scores` over a scalar numeric `parquet_column`.
        module subroutine normal_scores_col(values, s, is_valid, skipnan, method, out_valid, n_null, ok, threads)
            type(parquet_column), intent(in) :: values !! the column; scalar numeric kinds only.
            real(real64), intent(out) :: s(:)
            !! the normal scores, same size as `values`, in the caller's
            !! original order. An excluded element is a quiet NaN unless
            !! `out_valid` is present.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            logical, intent(in), optional :: skipnan
            !! .true. (the default) excludes a NaN from the population, as a null is excluded and
            !! as `pf_minmax` has always done; .false. restores numpy's propagating behaviour, in
            !! which one NaN makes every answer NaN.
            character(len=*), intent(in), optional :: method
            !! the PLOTTING POSITION: which point of the distribution a rank is taken to mark.
            !! `s(i) = Phi**(-1)((r - a)/(m + 1 - 2a))` for a midrank `r` among `m` survivors,
            !! with `a` given by the token: "blom" (the default, `a = 3/8`, the standard rankit
            !! and what R's `qqnorm` uses), "weibull" (`a = 0`, `r/(m+1)`, the van der Waerden
            !! score), "tukey" (`a = 1/3`), "hazen" (`a = 1/2`) or "cunnane" (`a = 0.4`).
            !! "filliben" is the MEDIAN rank and not of that family at all: `1 - 0.5**(1/m)` at
            !! the first position, `0.5**(1/m)` at the last and `(i - 0.3175)/(m + 0.365)`
            !! between. Any other token aborts, naming all six. Matched case-insensitively.
            logical, intent(out), optional :: out_valid(:)
            !! per output element: .false. where the input was excluded and the element therefore
            !! has no score. Same size as `values`. **Absent, such an element is written as a
            !! quiet NaN instead** -- which is loud in practice, since it propagates through
            !! whatever the caller does next.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
            integer, intent(in), optional :: threads
            !! how many threads the midrank ordering may use, passed straight to `pf_argsort`.
            !! Absent takes the automatic rule. **The answer does not depend on this argument**:
            !! ties take the mean of the positions they span, so no tie-breaking the sort might
            !! choose can reach the result. It is a speed control and never an accuracy one.
        end subroutine normal_scores_col
        !> `pf_sigma_clipped_stats` over a scalar numeric `parquet_column`.
        module subroutine sigma_clipped_stats_col(values, mean, median, stddev, sigma, sigma_lower, sigma_upper, maxiters, &
                cenfunc, stdfunc, n_clipped, keep, converged, is_valid, &
                skipnan, n_null, n_nan, ok, threads)
            type(parquet_column), intent(in) :: values !! the column; scalar numeric kinds only.
            real(real64), intent(out) :: mean !! the mean of the surviving values; NaN when none survive.
            real(real64), intent(out) :: median !! their median.
            real(real64), intent(out) :: stddev !! their POPULATION standard deviation (`ddof = 0`), as astropy reports.
            real(real64), intent(in), optional :: sigma
            !! how many scale units a value may sit from the centre and still be kept. 3.0 by
            !! default, as astropy does. `sigma_lower` and `sigma_upper` each default to this, so
            !! setting it alone gives a symmetric clip.
            real(real64), intent(in), optional :: sigma_lower
            !! the lower half of the clip, overriding `sigma` below the centre.
            real(real64), intent(in), optional :: sigma_upper
            !! the upper half of the clip, overriding `sigma` above the centre.
            integer, intent(in), optional :: maxiters
            !! how many clipping rounds to run; 5 by default, as astropy does. **A non-positive
            !! value means iterate until nothing more is removed**, which is astropy's
            !! `maxiters=None` -- capped at a hard internal 100 rounds, and reaching that cap
            !! STOPS and reports through `converged` rather than aborting. Each non-terminating
            !! round removes at least one point, so a large catalogue can honestly want more
            !! rounds than any small constant.
            character(len=*), intent(in), optional :: cenfunc
            !! which centre each round clips about: "median" (the default, as astropy) or "mean".
            !! Any other token aborts, naming both. Matched case-insensitively.
            character(len=*), intent(in), optional :: stdfunc
            !! which scale each round measures: "std" (the default, as astropy -- the sample
            !! standard deviation at `ddof = 1`) or "mad_std", the normal-scaled median absolute
            !! deviation, which is far less disturbed by the outliers being clipped. Any other
            !! token aborts, naming both. Matched case-insensitively.
            integer(int64), intent(out), optional :: n_clipped
            !! how many elements of the population the clipping removed. `0` for an empty
            !! population, and `0` when the first round removes nothing.
            logical, intent(out), optional :: keep(:)
            !! per element of `values`: .true. for one that survived both the exclusion rules and
            !! every clipping round. Same size as `values`. This is what a caller needs in order
            !! to apply the same clip to a SECOND column.
            logical, intent(out), optional :: converged
            !! .false. when the iteration stopped at its round limit with points still being
            !! removed, rather than because a round removed nothing. The answer is still usable
            !! -- it is the last completed round's -- so `ok` stays .true. and this is the only
            !! way to tell the two endings apart.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            logical, intent(in), optional :: skipnan
            !! .true. (the default) excludes a NaN from the population, as a null is excluded and
            !! as `pf_minmax` has always done; .false. restores numpy's propagating behaviour, in
            !! which one NaN makes every answer NaN.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            integer(int64), intent(out), optional :: n_nan
            !! how many were excluded as NaN and were not already null.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
            integer, intent(in), optional :: threads
            !! how many threads the ORDERING may use -- this is an order statistic and has no
            !! central-moment pass, so the work `threads=` divides is `pf_argsort`'s. Absent takes
            !! the automatic rule: the `sort_threads` setting, capped by the processors
            !! actually available and by a measured work floor, and 1 inside a caller's own
            !! parallel region. **The answer does not depend on this argument** -- a sort is a
            !! permutation and the selection off it is exact, so 1, 8 and a build with no OpenMP
            !! at all return the same bits. It is a speed control and never an accuracy one.
        end subroutine sigma_clipped_stats_col
        !> `pf_cumsum` over a scalar numeric `parquet_column`.
        module subroutine cumsum_col(values, out, is_valid, skipnan, out_valid, n_null, n_nan, ok)
            type(parquet_column), intent(in) :: values !! the column; scalar numeric kinds only.
            real(real64), intent(out) :: out(:)
            !! the running sum, same size as `values`. An excluded element is a quiet NaN unless
            !! `out_valid` is present.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            logical, intent(in), optional :: skipnan
            !! .true. (the default) excludes a NaN from the population, as a null is excluded and
            !! as `pf_minmax` has always done; .false. restores numpy's propagating behaviour, in
            !! which one NaN makes every answer NaN.
            logical, intent(out), optional :: out_valid(:)
            !! per output element: .false. where the input was excluded and the result is
            !! therefore undefined. Same size as `values`. **Absent, such an element is written as
            !! a quiet NaN instead** -- which is loud in practice, since it propagates through
            !! whatever the caller does next.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            integer(int64), intent(out), optional :: n_nan
            !! how many were excluded as NaN and were not already null.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
        end subroutine cumsum_col
        !> `pf_cumprod` over a scalar numeric `parquet_column`.
        module subroutine cumprod_col(values, out, is_valid, skipnan, out_valid, n_null, n_nan, ok)
            type(parquet_column), intent(in) :: values !! the column; scalar numeric kinds only.
            real(real64), intent(out) :: out(:)
            !! the running product, same size as `values`. An excluded element is a quiet NaN unless
            !! `out_valid` is present.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            logical, intent(in), optional :: skipnan
            !! .true. (the default) excludes a NaN from the population, as a null is excluded and
            !! as `pf_minmax` has always done; .false. restores numpy's propagating behaviour, in
            !! which one NaN makes every answer NaN.
            logical, intent(out), optional :: out_valid(:)
            !! per output element: .false. where the input was excluded and the result is
            !! therefore undefined. Same size as `values`. **Absent, such an element is written as
            !! a quiet NaN instead** -- which is loud in practice, since it propagates through
            !! whatever the caller does next.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            integer(int64), intent(out), optional :: n_nan
            !! how many were excluded as NaN and were not already null.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
        end subroutine cumprod_col
        !> `pf_cummax` over a scalar numeric `parquet_column`.
        module subroutine cummax_col(values, out, is_valid, skipnan, out_valid, n_null, n_nan, ok)
            type(parquet_column), intent(in) :: values !! the column; scalar numeric kinds only.
            real(real64), intent(out) :: out(:)
            !! the running maximum, same size as `values`. An excluded element is a quiet NaN unless
            !! `out_valid` is present.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            logical, intent(in), optional :: skipnan
            !! .true. (the default) excludes a NaN from the population, as a null is excluded and
            !! as `pf_minmax` has always done; .false. restores numpy's propagating behaviour, in
            !! which one NaN makes every answer NaN.
            logical, intent(out), optional :: out_valid(:)
            !! per output element: .false. where the input was excluded and the result is
            !! therefore undefined. Same size as `values`. **Absent, such an element is written as
            !! a quiet NaN instead** -- which is loud in practice, since it propagates through
            !! whatever the caller does next.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            integer(int64), intent(out), optional :: n_nan
            !! how many were excluded as NaN and were not already null.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
        end subroutine cummax_col
        !> `pf_cummin` over a scalar numeric `parquet_column`.
        module subroutine cummin_col(values, out, is_valid, skipnan, out_valid, n_null, n_nan, ok)
            type(parquet_column), intent(in) :: values !! the column; scalar numeric kinds only.
            real(real64), intent(out) :: out(:)
            !! the running minimum, same size as `values`. An excluded element is a quiet NaN unless
            !! `out_valid` is present.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            logical, intent(in), optional :: skipnan
            !! .true. (the default) excludes a NaN from the population, as a null is excluded and
            !! as `pf_minmax` has always done; .false. restores numpy's propagating behaviour, in
            !! which one NaN makes every answer NaN.
            logical, intent(out), optional :: out_valid(:)
            !! per output element: .false. where the input was excluded and the result is
            !! therefore undefined. Same size as `values`. **Absent, such an element is written as
            !! a quiet NaN instead** -- which is loud in practice, since it propagates through
            !! whatever the caller does next.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            integer(int64), intent(out), optional :: n_nan
            !! how many were excluded as NaN and were not already null.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
        end subroutine cummin_col
        !> `pf_bucketize` over a scalar numeric `parquet_column`.
        module subroutine bucketize_col(values, edges, codes, right, is_valid, weights, skipnan, n_null, n_nan, &
                n_outside, ok)
            type(parquet_column), intent(in) :: values !! the column; scalar numeric kinds only.
            real(real64), intent(in) :: edges(:) !! the bin boundaries, strictly increasing, at least two.
            integer(int32), intent(out) :: codes(:)
            !! the 1-based bin of each value, or 0 for one that joined none. Same size as `values`.
            logical, intent(in), optional :: right
            !! which side of a bin is closed. `.false.` by default, which is numpy's rule and the
            !! one `np.histogram`/`np.digitize` use: bin k is `[edges(k), edges(k+1))`, and the
            !! LAST bin closes at the top so that `edges(nbins+1)` itself lands in it. `.true.`
            !! mirrors it, which is `pd.cut(right=True, include_lowest=True)`: bin k is
            !! `(edges(k), edges(k+1)]`, and the FIRST bin closes at the bottom. Either way the
            !! closed range `[edges(1), edges(nbins+1)]` is covered exactly once, so the two
            !! conventions differ only in which bin a value sitting exactly on an interior edge
            !! joins.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight, non-negative. A ZERO weight removes the element from the
            !! population; a negative, NaN or infinite one aborts. Absent means every weight is 1.
            logical, intent(in), optional :: skipnan
            !! .true. (the default) excludes a NaN from the population, as a null is excluded and
            !! as `pf_minmax` has always done; .false. restores numpy's propagating behaviour, in
            !! which one NaN makes every answer NaN.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            integer(int64), intent(out), optional :: n_nan
            !! how many were excluded as NaN and were not already null.
            integer(int64), intent(out), optional :: n_outside
            !! how many usable elements fell outside `[edges(1), edges(nbins+1)]` and so joined no
            !! bin. This is an ordinary data condition rather than an error -- `np.histogram`
            !! drops such values silently -- but it is the one thing a caller cannot recover from
            !! the output, so it is reported. A NaN under `skipnan = .false.` is counted here: it
            !! is then a value, and it matches no bin.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
        end subroutine bucketize_col
        !> `pf_histogram` over a scalar numeric `parquet_column`.
        module subroutine histogram_col(values, edges, counts, right, density, is_valid, weights, skipnan, n_null, &
                n_nan, n_outside, ok)
            type(parquet_column), intent(in) :: values !! the column; scalar numeric kinds only.
            real(real64), intent(in) :: edges(:) !! the bin boundaries, strictly increasing, at least two.
            real(real64), intent(out) :: counts(:)
            !! one entry per bin, so `size(edges) - 1` of them.
            logical, intent(in), optional :: right
            !! which side of a bin is closed. `.false.` by default, which is numpy's rule and the
            !! one `np.histogram`/`np.digitize` use: bin k is `[edges(k), edges(k+1))`, and the
            !! LAST bin closes at the top so that `edges(nbins+1)` itself lands in it. `.true.`
            !! mirrors it, which is `pd.cut(right=True, include_lowest=True)`: bin k is
            !! `(edges(k), edges(k+1)]`, and the FIRST bin closes at the bottom. Either way the
            !! closed range `[edges(1), edges(nbins+1)]` is covered exactly once, so the two
            !! conventions differ only in which bin a value sitting exactly on an interior edge
            !! joins.
            logical, intent(in), optional :: density
            !! `.false.` by default. `.true.` divides each bin by its own WIDTH and by the total
            !! that was binned, giving a probability density -- `np.histogram(density=True)`, so
            !! `sum(counts * widths)` is 1 and an uneven edge spacing is accounted for. The
            !! normalisation base is what actually landed in a bin: values outside the edges were
            !! never counted, so they do not appear in it, which is numpy's rule too. When nothing
            !! was binned the density is undefined and every entry is a **quiet NaN** with
            !! `ok = .false.` -- note that an empty HISTOGRAM is perfectly well defined (all
            !! zeros, `ok = .true.`) and an empty DENSITY is not.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight, non-negative. A ZERO weight removes the element from the
            !! population; a negative, NaN or infinite one aborts. Absent means every weight is 1.
            logical, intent(in), optional :: skipnan
            !! .true. (the default) excludes a NaN from the population, as a null is excluded and
            !! as `pf_minmax` has always done; .false. restores numpy's propagating behaviour, in
            !! which one NaN makes every answer NaN.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            integer(int64), intent(out), optional :: n_nan
            !! how many were excluded as NaN and were not already null.
            integer(int64), intent(out), optional :: n_outside
            !! how many usable elements fell outside `[edges(1), edges(nbins+1)]` and so joined no
            !! bin. This is an ordinary data condition rather than an error -- `np.histogram`
            !! drops such values silently -- but it is the one thing a caller cannot recover from
            !! the output, so it is reported. A NaN under `skipnan = .false.` is counted here: it
            !! is then a value, and it matches no bin.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
        end subroutine histogram_col
        !> `pf_bin_edges` over a scalar numeric `parquet_column`.
        module subroutine bin_edges_col(values, nbins, edges, is_valid, weights, skipnan, n_null, n_nan, ok)
            type(parquet_column), intent(in) :: values !! the column; scalar numeric kinds only.
            integer, intent(in) :: nbins !! how many bins to describe; at least 1.
            real(real64), intent(out) :: edges(:)
            !! the `nbins + 1` boundaries, strictly increasing.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight, non-negative. A ZERO weight removes the element from the
            !! population; a negative, NaN or infinite one aborts. Absent means every weight is 1.
            logical, intent(in), optional :: skipnan
            !! .true. (the default) excludes a NaN from the population, as a null is excluded and
            !! as `pf_minmax` has always done; .false. restores numpy's propagating behaviour, in
            !! which one NaN makes every answer NaN.
            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded.
            integer(int64), intent(out), optional :: n_nan
            !! how many were excluded as NaN and were not already null.
            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure.
        end subroutine bin_edges_col
        !> `%compute` over a 32-bit integer array.
        module subroutine obj_compute_i32(self, values, retain, is_valid, weights, weight_type, threads)
            class(pf_stats), intent(inout) :: self
            !! the accumulator; any previous contents are discarded.
            integer(int32), intent(in) :: values(:) !! the population.
            logical, intent(in), optional :: retain
            !! .true. by default: keep a copy of the surviving values, which costs 8 bytes per
            !! survivor (16 when weighted) and is what makes `%update` and `%merge` EXACT rather
            !! than approximate. .false. keeps nothing but the accumulator, which is O(1) whatever
            !! the population size and is what a row-group loop over a file larger than RAM wants.
            !! Fixed for the object's lifetime; every accumulator in one `%merge` must agree.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight, non-negative. A ZERO weight removes the element from the
            !! population; a negative, NaN or infinite one aborts. Absent means every weight is 1.
            character(len=*), intent(in), optional :: weight_type
            !! "reliability" (the default) or "frequency". The two differ only when the weights are
            !! unequal: a FREQUENCY weight of 3 says the value occurred three times, so the count
            !! `ddof` is charged against is `sum(weights)`; a RELIABILITY weight says the value is
            !! that much more precise, so the count is Kish's effective size
            !! `sum(weights)**2 / sum(weights**2)`. Any other token aborts, listing both.
            integer, intent(in), optional :: threads
            !! how many threads the central-moment pass may use. Absent takes the automatic rule:
            !! the `sort_threads` setting, capped by the processors actually available and
            !! by a measured work floor, and 1 inside a caller's own parallel region. **The answer
            !! does not depend on this argument** -- the block decomposition is a function of the
            !! population size alone, so 1, 8 and a build with no OpenMP at all return the same
            !! bits. It is a speed control and never an accuracy one.
        end subroutine obj_compute_i32
        !> `%update` over a 32-bit integer array.
        module subroutine obj_update_i32(self, values, is_valid, weights)
            class(pf_stats), intent(inout) :: self !! the accumulator.
            integer(int32), intent(in) :: values(:) !! the batch.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight, non-negative. A ZERO weight removes the element from the
            !! population; a negative, NaN or infinite one aborts. Absent means every weight is 1.
        end subroutine obj_update_i32
        !> `%compute` over a 64-bit integer array.
        !>
        !> **An `integer(int64)` above 2**53 loses exactness** on the way into the real64 buffer
        !> every statistic here is computed in. That is unavoidable for a mean and irrelevant for
        !> the workloads this serves -- nobody averages an object ID -- but it is NOT irrelevant
        !> for a median, which `pf_nth_quantile` computes exactly on the original array.
        module subroutine obj_compute_i64(self, values, retain, is_valid, weights, weight_type, threads)
            class(pf_stats), intent(inout) :: self
            !! the accumulator; any previous contents are discarded.
            integer(int64), intent(in) :: values(:) !! the population.
            logical, intent(in), optional :: retain
            !! .true. by default: keep a copy of the surviving values, which costs 8 bytes per
            !! survivor (16 when weighted) and is what makes `%update` and `%merge` EXACT rather
            !! than approximate. .false. keeps nothing but the accumulator, which is O(1) whatever
            !! the population size and is what a row-group loop over a file larger than RAM wants.
            !! Fixed for the object's lifetime; every accumulator in one `%merge` must agree.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight, non-negative. A ZERO weight removes the element from the
            !! population; a negative, NaN or infinite one aborts. Absent means every weight is 1.
            character(len=*), intent(in), optional :: weight_type
            !! "reliability" (the default) or "frequency". The two differ only when the weights are
            !! unequal: a FREQUENCY weight of 3 says the value occurred three times, so the count
            !! `ddof` is charged against is `sum(weights)`; a RELIABILITY weight says the value is
            !! that much more precise, so the count is Kish's effective size
            !! `sum(weights)**2 / sum(weights**2)`. Any other token aborts, listing both.
            integer, intent(in), optional :: threads
            !! how many threads the central-moment pass may use. Absent takes the automatic rule:
            !! the `sort_threads` setting, capped by the processors actually available and
            !! by a measured work floor, and 1 inside a caller's own parallel region. **The answer
            !! does not depend on this argument** -- the block decomposition is a function of the
            !! population size alone, so 1, 8 and a build with no OpenMP at all return the same
            !! bits. It is a speed control and never an accuracy one.
        end subroutine obj_compute_i64
        !> `%update` over a 64-bit integer array.
        module subroutine obj_update_i64(self, values, is_valid, weights)
            class(pf_stats), intent(inout) :: self !! the accumulator.
            integer(int64), intent(in) :: values(:) !! the batch.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight, non-negative. A ZERO weight removes the element from the
            !! population; a negative, NaN or infinite one aborts. Absent means every weight is 1.
        end subroutine obj_update_i64
        !> `%compute` over a 32-bit real array.
        module subroutine obj_compute_f32(self, values, retain, is_valid, weights, weight_type, skipnan, threads)
            class(pf_stats), intent(inout) :: self
            !! the accumulator; any previous contents are discarded.
            real(real32), intent(in) :: values(:) !! the population.
            logical, intent(in), optional :: retain
            !! .true. by default: keep a copy of the surviving values, which costs 8 bytes per
            !! survivor (16 when weighted) and is what makes `%update` and `%merge` EXACT rather
            !! than approximate. .false. keeps nothing but the accumulator, which is O(1) whatever
            !! the population size and is what a row-group loop over a file larger than RAM wants.
            !! Fixed for the object's lifetime; every accumulator in one `%merge` must agree.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight, non-negative. A ZERO weight removes the element from the
            !! population; a negative, NaN or infinite one aborts. Absent means every weight is 1.
            character(len=*), intent(in), optional :: weight_type
            !! "reliability" (the default) or "frequency". The two differ only when the weights are
            !! unequal: a FREQUENCY weight of 3 says the value occurred three times, so the count
            !! `ddof` is charged against is `sum(weights)`; a RELIABILITY weight says the value is
            !! that much more precise, so the count is Kish's effective size
            !! `sum(weights)**2 / sum(weights**2)`. Any other token aborts, listing both.
            logical, intent(in), optional :: skipnan
            !! .true. (the default) excludes a NaN from the population, as a null is excluded and
            !! as `pf_minmax` has always done; .false. restores numpy's propagating behaviour, in
            !! which one NaN makes every answer NaN.
            integer, intent(in), optional :: threads
            !! how many threads the central-moment pass may use. Absent takes the automatic rule:
            !! the `sort_threads` setting, capped by the processors actually available and
            !! by a measured work floor, and 1 inside a caller's own parallel region. **The answer
            !! does not depend on this argument** -- the block decomposition is a function of the
            !! population size alone, so 1, 8 and a build with no OpenMP at all return the same
            !! bits. It is a speed control and never an accuracy one.
        end subroutine obj_compute_f32
        !> `%update` over a 32-bit real array.
        module subroutine obj_update_f32(self, values, is_valid, weights)
            class(pf_stats), intent(inout) :: self !! the accumulator.
            real(real32), intent(in) :: values(:) !! the batch.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight, non-negative. A ZERO weight removes the element from the
            !! population; a negative, NaN or infinite one aborts. Absent means every weight is 1.
        end subroutine obj_update_f32
        !> `%compute` over a logical array.
        !>
        !> `.true.` widens to 1 and `.false.` to 0, so the mean of a logical array is the
        !> fraction that are true.
        module subroutine obj_compute_bool(self, values, retain, is_valid, weights, weight_type, threads)
            class(pf_stats), intent(inout) :: self
            !! the accumulator; any previous contents are discarded.
            logical, intent(in) :: values(:) !! the population; .true. is 1 and .false. is 0.
            logical, intent(in), optional :: retain
            !! .true. by default: keep a copy of the surviving values, which costs 8 bytes per
            !! survivor (16 when weighted) and is what makes `%update` and `%merge` EXACT rather
            !! than approximate. .false. keeps nothing but the accumulator, which is O(1) whatever
            !! the population size and is what a row-group loop over a file larger than RAM wants.
            !! Fixed for the object's lifetime; every accumulator in one `%merge` must agree.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight, non-negative. A ZERO weight removes the element from the
            !! population; a negative, NaN or infinite one aborts. Absent means every weight is 1.
            character(len=*), intent(in), optional :: weight_type
            !! "reliability" (the default) or "frequency". The two differ only when the weights are
            !! unequal: a FREQUENCY weight of 3 says the value occurred three times, so the count
            !! `ddof` is charged against is `sum(weights)`; a RELIABILITY weight says the value is
            !! that much more precise, so the count is Kish's effective size
            !! `sum(weights)**2 / sum(weights**2)`. Any other token aborts, listing both.
            integer, intent(in), optional :: threads
            !! how many threads the central-moment pass may use. Absent takes the automatic rule:
            !! the `sort_threads` setting, capped by the processors actually available and
            !! by a measured work floor, and 1 inside a caller's own parallel region. **The answer
            !! does not depend on this argument** -- the block decomposition is a function of the
            !! population size alone, so 1, 8 and a build with no OpenMP at all return the same
            !! bits. It is a speed control and never an accuracy one.
        end subroutine obj_compute_bool
        !> `%update` over a logical array.
        module subroutine obj_update_bool(self, values, is_valid, weights)
            class(pf_stats), intent(inout) :: self !! the accumulator.
            logical, intent(in) :: values(:) !! the population; .true. is 1 and .false. is 0.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight, non-negative. A ZERO weight removes the element from the
            !! population; a negative, NaN or infinite one aborts. Absent means every weight is 1.
        end subroutine obj_update_bool
        !> `%compute` over a scalar numeric `parquet_column`.
        !>
        !> Dispatched on the column's kind at run time, and **scalar numeric columns only**:
        !> int32, int64, float32, float64 and logical. A string, temporal or container column
        !> aborts naming its kind, and so does a column more than one element wide -- flattening
        !> a vector column into one population is a different statistic and nobody should get it
        !> by accident. **The column's own validity is the only source of nullness**, so passing
        !> `is_valid=` alongside one aborts rather than silently preferring one of two sources.
        module subroutine obj_compute_col(self, values, retain, is_valid, weights, weight_type, skipnan, threads)
            class(pf_stats), intent(inout) :: self
            !! the accumulator; any previous contents are discarded.
            type(parquet_column), intent(in) :: values !! the column; scalar numeric kinds only.
            logical, intent(in), optional :: retain
            !! .true. by default: keep a copy of the surviving values, which costs 8 bytes per
            !! survivor (16 when weighted) and is what makes `%update` and `%merge` EXACT rather
            !! than approximate. .false. keeps nothing but the accumulator, which is O(1) whatever
            !! the population size and is what a row-group loop over a file larger than RAM wants.
            !! Fixed for the object's lifetime; every accumulator in one `%merge` must agree.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight, non-negative. A ZERO weight removes the element from the
            !! population; a negative, NaN or infinite one aborts. Absent means every weight is 1.
            character(len=*), intent(in), optional :: weight_type
            !! "reliability" (the default) or "frequency". The two differ only when the weights are
            !! unequal: a FREQUENCY weight of 3 says the value occurred three times, so the count
            !! `ddof` is charged against is `sum(weights)`; a RELIABILITY weight says the value is
            !! that much more precise, so the count is Kish's effective size
            !! `sum(weights)**2 / sum(weights**2)`. Any other token aborts, listing both.
            logical, intent(in), optional :: skipnan
            !! .true. (the default) excludes a NaN from the population, as a null is excluded and
            !! as `pf_minmax` has always done; .false. restores numpy's propagating behaviour, in
            !! which one NaN makes every answer NaN.
            integer, intent(in), optional :: threads
            !! how many threads the central-moment pass may use. Absent takes the automatic rule:
            !! the `sort_threads` setting, capped by the processors actually available and
            !! by a measured work floor, and 1 inside a caller's own parallel region. **The answer
            !! does not depend on this argument** -- the block decomposition is a function of the
            !! population size alone, so 1, 8 and a build with no OpenMP at all return the same
            !! bits. It is a speed control and never an accuracy one.
        end subroutine obj_compute_col
        !> `%update` over a scalar numeric `parquet_column`.
        module subroutine obj_update_col(self, values, is_valid, weights)
            class(pf_stats), intent(inout) :: self !! the accumulator.
            type(parquet_column), intent(in) :: values !! the batch; scalar numeric kinds only.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight, non-negative. A ZERO weight removes the element from the
            !! population; a negative, NaN or infinite one aborts. Absent means every weight is 1.
        end subroutine obj_update_col
    end interface
    !
    ! ---- pf_stats (implemented in parquet_stats_core) ----
    interface
        !> Summarises a resident array: the usual way to build a `pf_stats`.
        !!
        !! Two traversals, whatever is asked of the result afterwards. Any previous contents of
        !! `self` are discarded, so an object may be reused across a loop of groups without
        !! `%clear` in between.
        module subroutine obj_compute_f64(self, values, retain, is_valid, weights, weight_type, &
                skipnan, threads)
            class(pf_stats), intent(inout) :: self
            !! the accumulator; any previous contents are discarded.
            real(real64), intent(in) :: values(:) !! the population, before exclusions.
            logical, intent(in), optional :: retain
            !! .true. by default: keep a copy of the surviving values, which costs 8 bytes per
            !! survivor (16 when weighted) and is what makes `%update` and `%merge` EXACT rather
            !! than approximate. .false. keeps nothing but the accumulator, which is O(1) whatever
            !! the population size and is what a row-group loop over a file larger than RAM wants.
            !! Fixed for the object's lifetime; every accumulator in one `%merge` must agree.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight, non-negative. A ZERO weight removes the element from the
            !! population; a negative, NaN or infinite one aborts. Absent means every weight is 1.
            character(len=*), intent(in), optional :: weight_type
            !! "reliability" (the default) or "frequency". The two differ only when the weights are
            !! unequal: a FREQUENCY weight of 3 says the value occurred three times, so the count
            !! `ddof` is charged against is `sum(weights)`; a RELIABILITY weight says the value is
            !! that much more precise, so the count is Kish's effective size
            !! `sum(weights)**2 / sum(weights**2)`. Any other token aborts, listing both.
            logical, intent(in), optional :: skipnan
            !! .true. (the default) excludes a NaN from the population, as a null is excluded and
            !! as `pf_minmax` has always done; .false. restores numpy's propagating behaviour, in
            !! which one NaN makes every answer NaN.
            integer, intent(in), optional :: threads
            !! how many threads the central-moment pass may use. Absent takes the automatic rule:
            !! the `sort_threads` setting, capped by the processors actually available and
            !! by a measured work floor, and 1 inside a caller's own parallel region. **The answer
            !! does not depend on this argument** -- the block decomposition is a function of the
            !! population size alone, so 1, 8 and a build with no OpenMP at all return the same
            !! bits. It is a speed control and never an accuracy one.
        end subroutine obj_compute_f64
        !> Arms an empty accumulator for a loop of `%update`, or for a `%merge`.
        !!
        !! The population is EMPTY afterwards, which is a different state from uncomputed: every
        !! count answers `0`, every moment is NaN, and `%is_computed()` is `.true.`. `weight_type`
        !! and `skipnan` are fixed here because the queries do not take them.
        module subroutine obj_init(self, retain, weight_type, skipnan)
            class(pf_stats), intent(inout) :: self
            !! the accumulator; any previous contents are discarded.
            logical, intent(in), optional :: retain
            !! .true. by default: keep a copy of the surviving values, which costs 8 bytes per
            !! survivor (16 when weighted) and is what makes `%update` and `%merge` EXACT rather
            !! than approximate. .false. keeps nothing but the accumulator, which is O(1) whatever
            !! the population size and is what a row-group loop over a file larger than RAM wants.
            !! Fixed for the object's lifetime; every accumulator in one `%merge` must agree.
            character(len=*), intent(in), optional :: weight_type
            !! "reliability" (the default) or "frequency". The two differ only when the weights are
            !! unequal: a FREQUENCY weight of 3 says the value occurred three times, so the count
            !! `ddof` is charged against is `sum(weights)`; a RELIABILITY weight says the value is
            !! that much more precise, so the count is Kish's effective size
            !! `sum(weights)**2 / sum(weights**2)`. Any other token aborts, listing both.
            logical, intent(in), optional :: skipnan
            !! .true. (the default) excludes a NaN from the population, as a null is excluded and
            !! as `pf_minmax` has always done; .false. restores numpy's propagating behaviour, in
            !! which one NaN makes every answer NaN.
        end subroutine obj_init
        !> Folds one more batch of values into the population.
        !!
        !! In retained mode the survivors are appended and the moments are marked for
        !! recomputation, so a loop of k updates costs k traversals and ONE recomputation --
        !! and the result is bit for bit what `%compute` over the concatenated batches gives.
        !! In streaming mode this is one traversal and O(1) memory, combining by the Chan/Pebay
        !! formulas, and the accuracy is theirs.
        module subroutine obj_update_f64(self, values, is_valid, weights)
            class(pf_stats), intent(inout) :: self !! the accumulator.
            real(real64), intent(in) :: values(:) !! the batch, before exclusions.
            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight, non-negative. A ZERO weight removes the element from the
            !! population; a negative, NaN or infinite one aborts. Absent means every weight is 1.
        end subroutine obj_update_f64
        !> Folds one other accumulator into this one.
        !!
        !! Both must agree on all THREE of the policies fixed at `%compute`/`%init` -- `retain`,
        !! `weight_type` and `skipnan` -- and the source must have been computed; a mismatch
        !! aborts, naming which. A source with no elements at all is a no-op.
        module subroutine obj_merge_one(self, other, consume)
            class(pf_stats), intent(inout) :: self !! the destination.
            type(pf_stats), intent(inout) :: other
            !! the source. `intent(inout)` because `consume` clears it; it is unchanged otherwise.
            logical, intent(in), optional :: consume
            !! .false. by default: the sources are left intact. .true. clears each source as it is
            !! folded in, which is what the threaded shape wants -- the partials are dead after the
            !! region, and keeping them doubles the peak memory for nothing.
        end subroutine obj_merge_one
        !> Folds an array of accumulators into this one, **in index order**.
        !!
        !! This is the form threading should use: the fold order is the array's, not the order the
        !! threads happened to finish in, so the answer is reproducible. Folding k retained
        !! partials costs one recomputation rather than k, because it is deferred until something
        !! is asked of the result.
        module subroutine obj_merge_many(self, others, consume)
            class(pf_stats), intent(inout) :: self !! the destination.
            type(pf_stats), intent(inout) :: others(:) !! the sources, folded in ascending index.
            logical, intent(in), optional :: consume
            !! .false. by default: the sources are left intact. .true. clears each source as it is
            !! folded in, which is what the threaded shape wants -- the partials are dead after the
            !! region, and keeping them doubles the peak memory for nothing.
        end subroutine obj_merge_many
        !> Returns the object to its default-initialised state, freeing everything it held.
        !!
        !! `%is_computed()` is `.false.` afterwards. Calling it on an object that was never
        !! computed is legal and does nothing.
        module subroutine obj_clear(self)
            class(pf_stats), intent(inout) :: self !! the accumulator.
        end subroutine obj_clear
        !> Whether this accumulator holds a population yet.
        !>
        !> `.false.` only for a default-initialised object and for one `%clear` has reset. An
        !> accumulator armed by `%init` and never updated holds an EMPTY population, which is a
        !> different thing: it answers `.true.` here and `0` to every count.
        module function obj_is_computed(self) result(res)
            class(pf_stats), intent(inout) :: self
            !! the accumulator; a query may complete a deferred recomputation.
            logical :: res !! `.true.` once `%compute` or `%init` has run.
        end function obj_is_computed
        !> Whether this accumulator keeps the surviving values.
        !>
        !> Fixed by `retain` at `%compute`/`%init` and never changes. It decides whether `%merge` and
        !> `%update` are exact or approximate, and whether the order statistics of a later phase are
        !> available at all.
        module function obj_retains(self) result(res)
            class(pf_stats), intent(inout) :: self
            !! the accumulator; a query may complete a deferred recomputation.
            logical :: res !! `.true.` when the surviving values are being kept.
        end function obj_retains
        !> How many elements have been offered to this accumulator.
        !>
        !> Counts nulls, NaNs and zero-weight elements too, so `%n() - %n_valid()` is everything the
        !> exclusion rules removed. pandas' `size` rather than its `count`.
        module function obj_n(self) result(res)
            class(pf_stats), intent(inout) :: self
            !! the accumulator; a query may complete a deferred recomputation.
            integer(int64) :: res !! elements offered, including every excluded one.
        end function obj_n
        !> How many elements are in the population -- pandas' `count`.
        !>
        !> Carried across `%update` and `%merge` as an integer rather than re-derived, so it is exact
        !> and needs no recomputation to answer.
        module function obj_n_valid(self) result(res)
            class(pf_stats), intent(inout) :: self
            !! the accumulator; a query may complete a deferred recomputation.
            integer(int64) :: res !! elements in the population.
        end function obj_n_valid
        !> How many elements were excluded as null.
        module function obj_n_null(self) result(res)
            class(pf_stats), intent(inout) :: self
            !! the accumulator; a query may complete a deferred recomputation.
            integer(int64) :: res !! elements `is_valid` excluded.
        end function obj_n_null
        !> How many elements were excluded as NaN and were not already null.
        !>
        !> Always `0` under `skipnan = .false.`, where a NaN stays in the population.
        module function obj_n_nan(self) result(res)
            class(pf_stats), intent(inout) :: self
            !! the accumulator; a query may complete a deferred recomputation.
            integer(int64) :: res !! elements excluded as NaN.
        end function obj_n_nan
        !> The total weight of the population.
        !>
        !> Exactly `real(%n_valid())` when no weights were supplied. This is `sum(w)` and NOT the
        !> effective size the reliability convention charges `ddof` against; those differ whenever the
        !> weights are unequal.
        module function obj_sum_weights(self) result(res)
            class(pf_stats), intent(inout) :: self
            !! the accumulator; a query may complete a deferred recomputation.
            real(real64) :: res !! `sum(w)` over the population.
        end function obj_sum_weights
        !> The sum of the population.
        !>
        !> Weighted, this is `sum(w*x)`. An empty population sums to exactly `0`: it is the one
        !> quantity here that emptiness still defines, and it is what numpy and pandas both return.
        module function obj_sum(self) result(res)
            class(pf_stats), intent(inout) :: self
            !! the accumulator; a query may complete a deferred recomputation.
            real(real64) :: res !! `sum(w*x)`; exactly 0 for an empty population.
        end function obj_sum
        !> The mean of the population.
        module function obj_mean(self) result(res)
            class(pf_stats), intent(inout) :: self
            !! the accumulator; a query may complete a deferred recomputation.
            real(real64) :: res !! the weighted mean, or NaN for an empty population.
        end function obj_mean
        !> The geometric mean of the population -- `exp(sum(w*log(x)) / sum(w))`.
        !>
        !> **Needs the retained values, so it aborts on a streaming accumulator**
        !> (`retain = .false.`), exactly as the order statistics do. It is not derivable from the
        !> four central moments the accumulator carries -- a log-sum is a fifth quantity, and
        !> accumulating it in the hot loop would charge a transcendental per element to every
        !> population that never asks for one. Unlike `%median` it orders nothing.
        !>
        !> NaN for an empty population and for one holding a negative value; **exactly 0 when any
        !> value is 0**, which is the limit and what scipy returns.
        module function obj_gmean(self) result(res)
            class(pf_stats), intent(inout) :: self
            !! the accumulator; a query may complete a deferred recomputation.
            real(real64) :: res !! the weighted geometric mean, or NaN when it is undefined.
        end function obj_gmean
        !> The harmonic mean of the population -- `sum(w) / sum(w/x)`.
        !>
        !> Aborts on a streaming accumulator for the reason `%gmean` does, and orders nothing.
        !>
        !> NaN for an empty population and for one holding a negative value; **exactly 0 when any
        !> value is 0**, as scipy returns.
        module function obj_hmean(self) result(res)
            class(pf_stats), intent(inout) :: self
            !! the accumulator; a query may complete a deferred recomputation.
            real(real64) :: res !! the weighted harmonic mean, or NaN when it is undefined.
        end function obj_hmean
        !> The variance of the population.
        !>
        !> `ddof` at or above the effective size gives a NaN rather than a division -- including the
        !> single-element case at the default `ddof = 1`, which is a data condition, not an error.
        module function obj_variance(self, ddof) result(res)
            class(pf_stats), intent(inout) :: self
            !! the accumulator; a query may complete a deferred recomputation.
            integer, intent(in), optional :: ddof
            !! delta degrees of freedom; **1 by default** -- the sample variance, as pandas
            !! returns. numpy's default is 0, so pass `ddof=0` to reproduce `np.var`/`np.std`.
            !! A plain default-kind integer deliberately: it is bounded by the population size,
            !! so there is no int64 form to provide. `ddof >= n_valid` gives NaN, not a division.
            real(real64) :: res !! the variance, or NaN when it is undefined.
        end function obj_variance
        !> The standard deviation: the square root of `%variance(ddof)`.
        module function obj_stddev(self, ddof) result(res)
            class(pf_stats), intent(inout) :: self
            !! the accumulator; a query may complete a deferred recomputation.
            integer, intent(in), optional :: ddof
            !! delta degrees of freedom; **1 by default** -- the sample variance, as pandas
            !! returns. numpy's default is 0, so pass `ddof=0` to reproduce `np.var`/`np.std`.
            !! A plain default-kind integer deliberately: it is bounded by the population size,
            !! so there is no int64 form to provide. `ddof >= n_valid` gives NaN, not a division.
            real(real64) :: res !! the standard deviation, or NaN when it is undefined.
        end function obj_stddev
        !> The standard error of the mean: `%stddev(ddof)` over the root of the effective size.
        !>
        !> `scipy.stats.sem` at its default `ddof = 1`, which is this library's default too.
        module function obj_sem(self, ddof) result(res)
            class(pf_stats), intent(inout) :: self
            !! the accumulator; a query may complete a deferred recomputation.
            integer, intent(in), optional :: ddof
            !! delta degrees of freedom; **1 by default** -- the sample variance, as pandas
            !! returns. numpy's default is 0, so pass `ddof=0` to reproduce `np.var`/`np.std`.
            !! A plain default-kind integer deliberately: it is bounded by the population size,
            !! so there is no int64 form to provide. `ddof >= n_valid` gives NaN, not a division.
            real(real64) :: res !! the standard error, or NaN when it is undefined.
        end function obj_sem
        !> The skewness of the population.
        !>
        !> NaN for a constant population, and -- bias-corrected, which is the default -- for one with
        !> fewer than three elements.
        module function obj_skewness(self, bias) result(res)
            class(pf_stats), intent(inout) :: self
            !! the accumulator; a query may complete a deferred recomputation.
            logical, intent(in), optional :: bias
            !! .false. by default: the bias-CORRECTED G1/G2, which is what pandas returns.
            !! .true. gives the uncorrected g1/g2, which is what scipy returns by default.
            real(real64) :: res !! the skewness, or NaN when it is undefined.
        end function obj_skewness
        !> The kurtosis of the population.
        !>
        !> NaN for a constant population, and -- bias-corrected, which is the default -- for one with
        !> fewer than four elements. Excess by default, so a normal population gives 0.
        module function obj_kurtosis(self, bias, excess) result(res)
            class(pf_stats), intent(inout) :: self
            !! the accumulator; a query may complete a deferred recomputation.
            logical, intent(in), optional :: bias
            !! .false. by default: the bias-CORRECTED G1/G2, which is what pandas returns.
            !! .true. gives the uncorrected g1/g2, which is what scipy returns by default.
            logical, intent(in), optional :: excess
            !! .true. by default: EXCESS kurtosis, which is 0 for a normal population.
            !! .false. adds 3 back, giving the raw fourth-moment ratio.
            real(real64) :: res !! the kurtosis, or NaN when it is undefined.
        end function obj_kurtosis
        !> The smallest value in the population.
        module function obj_vmin(self) result(res)
            class(pf_stats), intent(inout) :: self
            !! the accumulator; a query may complete a deferred recomputation.
            real(real64) :: res !! the smallest value, or NaN for an empty population.
        end function obj_vmin
        !> The largest value in the population.
        module function obj_vmax(self) result(res)
            class(pf_stats), intent(inout) :: self
            !! the accumulator; a query may complete a deferred recomputation.
            real(real64) :: res !! the largest value, or NaN for an empty population.
        end function obj_vmax
        !> The spread of the population: `%vmax() - %vmin()`.
        module function obj_range(self) result(res)
            class(pf_stats), intent(inout) :: self
            !! the accumulator; a query may complete a deferred recomputation.
            real(real64) :: res !! `vmax - vmin`, or NaN for an empty population.
        end function obj_range
        !> How many full traversals of a population's values this process has performed.
        !!
        !! Test-only, and nothing in the library reads it. It counts TRAVERSALS, not calls:
        !! `%compute` costs **two** -- one to apply the exclusion rules and find the mean, one to
        !! accumulate the central moments about it -- and every tier-A query after that costs
        !! none, which is the promise this type exists to make. A streaming `%update` costs
        !! **one**. A retained `%update` costs one to append and defers the two-traversal
        !! recomputation until something is asked of the object, so folding k batches and then
        !! querying costs `k + 2` rather than `3k`.
        !!
        !! Process-global, which is why the `stats` suite is excluded from the per-suite
        !! parallelism in `test/run_tester.f90`.
        module function parquet_debug_stats_scans() result(res)
            integer(int64) :: res !! traversals since the last reset.
        end function parquet_debug_stats_scans
        !> Resets the traversal counter to zero.
        !!
        !! A counter is only usable as an assertion if a test can re-arm it: a once-per-process
        !! observable with no reset makes every test after the first vacuous.
        module subroutine parquet_debug_reset_stats_scans()
        end subroutine parquet_debug_reset_stats_scans
        !> The team size pass two last opened; 1 means it ran serially.
        !!
        !! Test-only, and the negative control for every threading assertion in this module. An
        !! A/B comparing `threads=1` against `threads=8` passes just as happily against an engine
        !! that opened no team at all -- the answers are bit-identical either way, which is the
        !! whole point of the decomposition -- so an equality test alone proves nothing about
        !! threading. This is what tells the two apart.
        !!
        !! Process-global and unsynchronised, like the traversal counter beside it.
        module function parquet_debug_stats_team() result(res)
            integer(int64) :: res !! threads pass two used on the most recent engine call.
        end function parquet_debug_stats_team
        !> Overrides the survivors-per-thread floor pass two applies before opening a team.
        !!
        !! Test-only. The measured floor is tens of thousands of elements per thread (see
        !! `STATS_MIN_PER_THREAD` in `src/parquet_stats_core.f90`), which no unit-test fixture
        !! reaches -- so without this hook every test would take the serial branch and the threaded
        !! one would ship untested. CLAUDE.md's size-threshold rule requires reaching both sides.
        !!
        !! `0` teams up at any size; a negative value restores the measured floor.
        module subroutine parquet_debug_set_stats_min_per_thread(n)
            integer(int64), intent(in) :: n !! survivors per thread; 0 forces, negative restores.
        end subroutine parquet_debug_set_stats_min_per_thread
        !> How many times this module has ORDERED a population.
        !!
        !! Test-only, and counted at the one choke point every ordering passes through -- so it
        !! covers a `pf_stats` building its sorted buffer AND a one-shot procedure sorting for
        !! itself. Those are the two things it has to distinguish costs for:
        !!
        !! * for the object it is the **only** observable that can tell a working order cache from
        !!   one that never invalidates. Every equality assertion about a median passes just as
        !!   happily against a stale answer -- a stale median is a plausible number -- so
        !!   `sorts == 1` across four order statistics, and `sorts == 2` after an intervening
        !!   `%update`, is what the caching contract is actually pinned by.
        !! * for `pf_sigma_clipped_stats` it is the only observable at all. Sorting once per round
        !!   returns the identical numbers and differs solely in cost, so `sorts == 1` for a whole
        !!   clipping run is what makes "one ordering serves every round" a claim rather than a
        !!   comment.
        !!
        !! Process-global and unsynchronised, which is why the `stats` suite is excluded from the
        !! per-suite parallelism in `test/run_tester.f90`.
        module function parquet_debug_stats_sorts() result(res)
            integer(int64) :: res !! orderings performed since the last reset.
        end function parquet_debug_stats_sorts
        !> Resets the ordering counter to zero.
        module subroutine parquet_debug_reset_stats_sorts()
        end subroutine parquet_debug_reset_stats_sorts
        !> Overrides the probability count at which `pf_quantiles` sorts instead of selecting.
        !!
        !! Test-only. The shipped threshold is small but not 1, so a unit test asking for two or
        !! three probabilities would only ever exercise one of the two paths; this is what reaches
        !! the other (CLAUDE.md's size-threshold rule). `0` always sorts, a huge value always
        !! selects, and a negative value restores the shipped threshold.
        module subroutine parquet_debug_set_stats_quantile_sort_min(n)
            integer(int64), intent(in) :: n !! probabilities; 0 forces the sort, negative restores.
        end subroutine parquet_debug_set_stats_quantile_sort_min
    end interface
    !
    ! ---- pf_stats tier B (implemented in parquet_stats_order) ----
    interface
        !> The interpolating median of the population.
        !!
        !! Builds tier B on first use and reads it thereafter, so a median followed by an IQR and
        !! three quantiles is ONE ordering; `parquet_debug_stats_sorts()` is what asserts it.
        !!
        !! **Aborts on a streaming accumulator** (`retain = .false.`), because no values were
        !! kept and there is nothing to order. The message says so and names the fix.
        module function obj_median(self, method) result(res)
            class(pf_stats), intent(inout) :: self
            !! the accumulator; the first order statistic builds its sorted buffer.
            character(len=*), intent(in), optional :: method
            !! how a fractional position between two order statistics is resolved, in numpy's
            !! spelling: "linear" (the default -- Hyndman-Fan type 7, what numpy and pandas do),
            !! "lower", "higher", "nearest", "midpoint", or "inverted_cdf" (a step function on the
            !! plain cumulative scale, and the only token numpy itself accepts weights with). Any
            !! other token aborts, listing all six. Matched case-insensitively.
            !! **`weight_type="frequency"` changes the default to "inverted_cdf"**, because that
            !! token IS frequency expansion; an explicit `method=` still wins.
            real(real64) :: res !! the median, or NaN for an empty population.
        end function obj_median
        !> One quantile of the population, on a 0-1 scale.
        module function obj_quantile(self, p, method) result(res)
            class(pf_stats), intent(inout) :: self
            !! the accumulator; the first order statistic builds its sorted buffer.
            real(real64), intent(in) :: p !! the probability, 0 to 1. Outside that aborts.
            character(len=*), intent(in), optional :: method
            !! how a fractional position between two order statistics is resolved, in numpy's
            !! spelling: "linear" (the default -- Hyndman-Fan type 7, what numpy and pandas do),
            !! "lower", "higher", "nearest", "midpoint", or "inverted_cdf" (a step function on the
            !! plain cumulative scale, and the only token numpy itself accepts weights with). Any
            !! other token aborts, listing all six. Matched case-insensitively.
            !! **`weight_type="frequency"` changes the default to "inverted_cdf"**, because that
            !! token IS frequency expansion; an explicit `method=` still wins.
            real(real64) :: res !! the quantile, or NaN for an empty population.
        end function obj_quantile
        !> Several quantiles of the population, from the one ordering.
        !!
        !! A subroutine rather than a function because the result is an array whose size the
        !! caller already knows; `out` must be the same size as `probs`.
        module subroutine obj_quantiles(self, probs, out, method)
            class(pf_stats), intent(inout) :: self
            !! the accumulator; the first order statistic builds its sorted buffer.
            real(real64), intent(in) :: probs(:) !! the probabilities, each 0 to 1.
            real(real64), intent(out) :: out(:) !! one quantile each; same size as `probs`.
            character(len=*), intent(in), optional :: method
            !! how a fractional position between two order statistics is resolved, in numpy's
            !! spelling: "linear" (the default -- Hyndman-Fan type 7, what numpy and pandas do),
            !! "lower", "higher", "nearest", "midpoint", or "inverted_cdf" (a step function on the
            !! plain cumulative scale, and the only token numpy itself accepts weights with). Any
            !! other token aborts, listing all six. Matched case-insensitively.
            !! **`weight_type="frequency"` changes the default to "inverted_cdf"**, because that
            !! token IS frequency expansion; an explicit `method=` still wins.
        end subroutine obj_quantiles
        !> The interquartile range of the population.
        module function obj_iqr(self, method) result(res)
            class(pf_stats), intent(inout) :: self
            !! the accumulator; the first order statistic builds its sorted buffer.
            character(len=*), intent(in), optional :: method
            !! how a fractional position between two order statistics is resolved, in numpy's
            !! spelling: "linear" (the default -- Hyndman-Fan type 7, what numpy and pandas do),
            !! "lower", "higher", "nearest", "midpoint", or "inverted_cdf" (a step function on the
            !! plain cumulative scale, and the only token numpy itself accepts weights with). Any
            !! other token aborts, listing all six. Matched case-insensitively.
            !! **`weight_type="frequency"` changes the default to "inverted_cdf"**, because that
            !! token IS frequency expansion; an explicit `method=` still wins.
            real(real64) :: res !! `q(0.75) - q(0.25)`, or NaN for an empty population.
        end function obj_iqr
        !> The mean with `prop` trimmed from each tail.
        module function obj_trim_mean(self, prop) result(res)
            class(pf_stats), intent(inout) :: self
            !! the accumulator; the first order statistic builds its sorted buffer.
            real(real64), intent(in) :: prop
            !! the share trimmed from EACH tail; `0 <= prop < 0.5`, anything else aborts.
            real(real64) :: res !! the trimmed mean, or NaN when nothing survives the trim.
        end function obj_trim_mean
        !> Where `score` sits in the population, on a 0-1 scale.
        module function obj_percentile_of_score(self, score, kind) result(res)
            class(pf_stats), intent(inout) :: self
            !! the accumulator; the first order statistic builds its sorted buffer.
            real(real64), intent(in) :: score !! the value to locate. NaN or infinite aborts.
            character(len=*), intent(in), optional :: kind
            !! which convention counts a value EQUAL to `score`, in scipy's spelling: "rank" (the
            !! default; ties count half), "weak" (a value <= score counts), "strict" (only < score)
            !! or "mean" (the average of weak and strict). Any other token aborts, listing all
            !! four. Matched case-insensitively.
            real(real64) :: res !! the share at or below `score`, or NaN for an empty population.
        end function obj_percentile_of_score
        !> Builds tier B now, rather than leaving it to the first order statistic.
        !!
        !! **The escape hatch for concurrency, and the reason it exists.** The lazy fill is NOT
        !! thread-safe on a SHARED accumulator: two threads calling `%median()` on the same object
        !! both find tier B absent and both build it. This type takes no lock -- it is a value-like
        !! summary, not a container with an ownership model -- so either the object is
        !! thread-private, or the caller runs this before the parallel region. That is exactly the
        !! trade `parquet_column%ensure_validity` makes, for exactly the same reason.
        !!
        !! Idempotent, and free when tier B is already built.
        module subroutine obj_prepare_order(self)
            class(pf_stats), intent(inout) :: self !! the accumulator.
        end subroutine obj_prepare_order
        !> Frees tier B, keeping tier A.
        !!
        !! The retained values are kept -- they ARE tier A's exactness -- but the object stops
        !! claiming they are ordered, so the next order statistic sorts again. Use it when a
        !! long-lived summary has had the quantiles taken off it and only the moments are wanted
        !! afterwards; `%clear` is what gives the memory back.
        module subroutine obj_release_order(self)
            class(pf_stats), intent(inout) :: self !! the accumulator.
        end subroutine obj_release_order
        !> Whether tier B is currently built.
        !!
        !! Test-facing more than user-facing: it is how `%update` dropping the ordering is
        !! asserted without reading the sort counter.
        module function obj_is_ordered(self) result(res)
            class(pf_stats), intent(in) :: self !! the accumulator.
            logical :: res !! .true. when the retained values are in ascending order.
        end function obj_is_ordered
        !> The median absolute deviation of the population -- tier C.
        !!
        !! `median(|x - center|)`, divided by `Phi^-1(3/4)` unless `scale="raw"`. The RAW deviation is
        !! cached with the centre it was taken about, so a second `%mad()` at the same centre --
        !! including a second call at the default centre -- is a scalar multiply. Changing the
        !! centre rebuilds it.
        !!
        !! Builds tier B first (the default centre is the population's median, and the deviations
        !! have to be ordered too), so this aborts on a streaming accumulator exactly as the
        !! tier-B bindings do.
        module function obj_mad(self, scale, center) result(res)
            class(pf_stats), intent(inout) :: self
            !! the accumulator; the first `%mad` builds tiers B and C.
            character(len=*), intent(in), optional :: scale
            !! "normal" (the default) divides the raw deviation by `Phi^-1(3/4)`, which makes the
            !! result a consistent estimator of the standard deviation for Gaussian data --
            !! scipy's `median_abs_deviation(scale='normal')`, and what an astronomy script means
            !! by "the MAD". That is a multiplication by 1.4826022185056018, and **not** by the
            !! rounded 1.4826 the textbooks quote, which differs by 1.5e-06 relative. "raw" is the
            !! unscaled median of |x - center|. Any other token aborts, naming both. Matched
            !! case-insensitively.
            real(real64), intent(in), optional :: center
            !! the centre the deviations are taken about. Absent uses the population's own median,
            !! which is what scipy does. Supplying one skips ONE selection and not the ordering:
            !! the median of |x - center| still has to be found. A NaN or infinite centre ABORTS
            !! -- unlike a NaN value, which is an ordinary data condition, a NaN centre can only
            !! come from the caller's own arithmetic.
            real(real64) :: res !! the deviation, or NaN for an empty population.
        end function obj_mad
        !> Whether tier C is currently built.
        !!
        !! Test-facing, like `%is_ordered`: it is how `%update` dropping the cached deviation is
        !! asserted without reading a counter.
        module function obj_has_deviation(self) result(res)
            class(pf_stats), intent(in) :: self !! the accumulator.
            logical :: res !! .true. when a raw deviation is cached.
        end function obj_has_deviation
        !> Writes the population's summary block to a unit -- pandas' `describe()`, formatted.
        !!
        !! Eight rows: the counts, the mean, the standard deviation, the extremes and the three
        !! quartiles. Reading them costs nothing that has already been paid for, so this is the
        !! natural partner of `pf_describe`; on an object that has not been ordered it builds
        !! tier B, and on a streaming one it prints the tier-A rows and says why the rest are
        !! absent rather than aborting -- a printer that killed the process would be a poor way
        !! to find out what an object holds.
        !!
        !! **Silenced by `verbosity = "silent"`**, like every other solicited printer in this
        !! library: the call returns having written nothing.
        module subroutine obj_print(self, unit, name)
            class(pf_stats), intent(inout) :: self
            !! the accumulator; printing may build tier B.
            integer, intent(in), optional :: unit
            !! where to write; the default is the `message_stream` setting's unit, so a program
            !! that has redirected the library's output gets this with it.
            character(len=*), intent(in), optional :: name
            !! a label for the block's heading. Absent prints an unnamed heading.
        end subroutine obj_print
    end interface
    !
end module parquet_stats ! GCOVR_EXCL_LINE
