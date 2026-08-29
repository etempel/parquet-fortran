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
    ! `parquet_sorting` imports these with an `only:` list and does not re-export them, so this
    ! module names them itself. Nothing new enters the dependency graph: `parquet_columns` is
    ! already in it, through `parquet_sorting`'s own `pf_argsort` over a column.
    use parquet_columns, only : parquet_column, parquet_kind_name, parquet_column_data_ptr, &
        PK_INT32, PK_INT64, PK_FLOAT32, PK_FLOAT64, PK_LOGICAL, &
        PK_INT32_VEC, PK_INT64_VEC, PK_FLOAT32_VEC, PK_FLOAT64_VEC, PK_LOGICAL_VEC
    use iso_fortran_env, only : int32, int64, real32, real64
    implicit none
    private

    public :: pf_count_valid
    public :: pf_sum, pf_mean, pf_variance, pf_stddev, pf_sem
    public :: pf_skewness, pf_kurtosis, pf_moments
    public :: pf_stats
    public :: parquet_debug_stats_scans, parquet_debug_reset_stats_scans
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
        logical :: stale = .false.               !! retained moments need recomputing.
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
        procedure :: variance => obj_variance !! The variance, at `ddof` degrees of freedom.
        procedure :: stddev => obj_stddev !! The standard deviation.
        procedure :: sem => obj_sem !! The standard error of the mean.
        procedure :: skewness => obj_skewness !! The third standardised moment.
        procedure :: kurtosis => obj_kurtosis !! The fourth standardised moment.
        procedure :: vmin => obj_vmin !! The smallest value in the population.
        procedure :: vmax => obj_vmax !! The largest value in the population.
        procedure :: range => obj_range !! `vmax - vmin`.
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
    end interface pf_count_valid
    !
    ! ---- Counting ----
    interface
        !> `pf_count_valid` over a 32-bit integer array.
        module subroutine count_valid_i32(values, n, is_valid, weights)
            integer(int32), intent(in) :: values(:) !! the population to count.
            integer(int64), intent(out) :: n !! how many elements are in the population.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight. A zero weight REMOVES the element from the population, so a
            !! weighted count and an unweighted one over the same array legitimately differ.
            !! A negative, NaN or infinite weight aborts.
        end subroutine count_valid_i32
        !> `pf_count_valid` over a 64-bit integer array.
        module subroutine count_valid_i64(values, n, is_valid, weights)
            integer(int64), intent(in) :: values(:) !! the population to count.
            integer(int64), intent(out) :: n !! how many elements are in the population.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight. A zero weight REMOVES the element from the population, so a
            !! weighted count and an unweighted one over the same array legitimately differ.
            !! A negative, NaN or infinite weight aborts.
        end subroutine count_valid_i64
        !> `pf_count_valid` over a 32-bit real array.
        module subroutine count_valid_f32(values, n, is_valid, weights, skipnan)
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
        end subroutine count_valid_f32
        !> `pf_count_valid` over a 64-bit real array.
        module subroutine count_valid_f64(values, n, is_valid, weights, skipnan)
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
        end subroutine count_valid_f64
        !> `pf_count_valid` over a logical array.
        module subroutine count_valid_bool(values, n, is_valid, weights)
            logical, intent(in) :: values(:) !! the population to count.
            integer(int64), intent(out) :: n !! how many elements are in the population.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
            real(real64), intent(in), optional :: weights(:)
            !! per element weight. A zero weight REMOVES the element from the population, so a
            !! weighted count and an unweighted one over the same array legitimately differ.
            !! A negative, NaN or infinite weight aborts.
        end subroutine count_valid_bool
        !> `pf_count_valid` over a scalar numeric `parquet_column`.
        !>
        !> Same dispatch and same three refusals as the moment family: numeric scalar kinds only,
        !> width 1 only, and `is_valid=` alongside a column aborts because the column carries its
        !> own validity.
        module subroutine count_valid_col(values, n, is_valid, weights, skipnan)
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
        end subroutine count_valid_col
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
    !> `real(real64)` only for now; P4 adds the other numeric kinds and `type(parquet_column)` to
    !> this same generic, which is source-compatible for every existing call.
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
    !> `real(real64)` only for now; P4 adds the other numeric kinds and `type(parquet_column)` to
    !> this same generic, which is source-compatible for every existing call.
    interface pf_mean
        module procedure mean_f64
        module procedure mean_i32
        module procedure mean_i64
        module procedure mean_f32
        module procedure mean_bool
        module procedure mean_col
    end interface pf_mean
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
    !> `real(real64)` only for now; P4 adds the other numeric kinds and `type(parquet_column)` to
    !> this same generic, which is source-compatible for every existing call.
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
    !> `real(real64)` only for now; P4 adds the other numeric kinds and `type(parquet_column)` to
    !> this same generic, which is source-compatible for every existing call.
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
    !> `real(real64)` only for now; P4 adds the other numeric kinds and `type(parquet_column)` to
    !> this same generic, which is source-compatible for every existing call.
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
    !> `real(real64)` only for now; P4 adds the other numeric kinds and `type(parquet_column)` to
    !> this same generic, which is source-compatible for every existing call.
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
    !> `real(real64)` only for now; P4 adds the other numeric kinds and `type(parquet_column)` to
    !> this same generic, which is source-compatible for every existing call.
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
    !> `real(real64)` only for now; P4 adds the other numeric kinds and `type(parquet_column)` to
    !> this same generic, which is source-compatible for every existing call.
    interface pf_moments
        module procedure moments_f64
        module procedure moments_i32
        module procedure moments_i64
        module procedure moments_f32
        module procedure moments_bool
        module procedure moments_col
    end interface pf_moments
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
        module subroutine sum_f64(values, s, is_valid, weights, skipnan, n_null, n_nan, ok)
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
        end subroutine sum_f64
        !> `pf_mean` over a 64-bit real array: `sum(w*x) / sum(w)`, REFINED.
        !>
        !> The quotient is taken first and corrected by `sum(w*(x - mu))/sum(w)`, so the result
        !> can sit one ulp from the naive quotient. See `pf_mean`'s own doc-comment for why that
        !> is the right answer and what it means for a test.
        module subroutine mean_f64(values, m, is_valid, weights, skipnan, n_null, n_nan, ok)
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
        end subroutine mean_f64
        !> `pf_variance` over a 64-bit real array, computed in TWO passes.
        !>
        !> The mean is taken first and the central moment accumulated against it, so the result
        !> is shift-invariant to a few ulp: `pf_variance(x + 1e9)` agrees with `pf_variance(x)`,
        !> which the textbook `sum(x**2) - sum(x)**2/n` does not.
        module subroutine variance_f64(values, v, is_valid, weights, weight_type, ddof, skipnan, n_null, n_nan, ok)
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
        end subroutine variance_f64
        !> `pf_stddev` over a 64-bit real array: the square root of `pf_variance`.
        module subroutine stddev_f64(values, sd, is_valid, weights, weight_type, ddof, skipnan, n_null, n_nan, ok)
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
        end subroutine stddev_f64
        !> `pf_sem` over a 64-bit real array: the standard error of the mean.
        !>
        !> `stddev / sqrt(n_eff)`, where `n_eff` is the population size unweighted and the count
        !> named by `weight_type` otherwise. Matches `scipy.stats.sem` at the default `ddof=1`.
        module subroutine sem_f64(values, se, is_valid, weights, weight_type, ddof, skipnan, n_null, n_nan, ok)
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
        end subroutine sem_f64
        !> `pf_skewness` over a 64-bit real array: the third standardised moment.
        !>
        !> **`ddof` is deliberately absent**: no definition of skewness in pandas, numpy or scipy
        !> takes one, and an argument that can never do anything is worse than an absent one.
        !> `weight_type` is present because the bias correction counts through `n_eff`.
        !> NaN when the variance is zero, or when `n_valid < 3` and `bias` is .false.
        module subroutine skewness_f64(values, g, is_valid, weights, weight_type, bias, skipnan, n_null, n_nan, ok)
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
        end subroutine skewness_f64
        !> `pf_kurtosis` over a 64-bit real array: the fourth standardised moment.
        !>
        !> `ddof` is absent for the same reason as on `pf_skewness`. NaN when the variance is
        !> zero, or when `n_valid < 4` and `bias` is .false.
        module subroutine kurtosis_f64(values, k, is_valid, weights, weight_type, bias, excess, skipnan, n_null, n_nan, ok)
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
                skipnan, n_null, n_nan)
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
        end subroutine moments_f64
    end interface
    !
    ! ---- The per-kind entry layer (implemented in parquet_stats_kernel) ----
    interface
        !> `pf_sum` over a 32-bit integer array.
        module subroutine sum_i32(values, s, is_valid, weights, n_null, ok)
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
        end subroutine sum_i32
        !> `pf_mean` over a 32-bit integer array.
        module subroutine mean_i32(values, m, is_valid, weights, n_null, ok)
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
        end subroutine mean_i32
        !> `pf_variance` over a 32-bit integer array.
        module subroutine variance_i32(values, v, is_valid, weights, weight_type, ddof, n_null, ok)
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
        end subroutine variance_i32
        !> `pf_stddev` over a 32-bit integer array.
        module subroutine stddev_i32(values, sd, is_valid, weights, weight_type, ddof, n_null, ok)
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
        end subroutine stddev_i32
        !> `pf_sem` over a 32-bit integer array.
        module subroutine sem_i32(values, se, is_valid, weights, weight_type, ddof, n_null, ok)
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
        end subroutine sem_i32
        !> `pf_skewness` over a 32-bit integer array.
        module subroutine skewness_i32(values, g, is_valid, weights, weight_type, bias, n_null, ok)
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
        end subroutine skewness_i32
        !> `pf_kurtosis` over a 32-bit integer array.
        module subroutine kurtosis_i32(values, k, is_valid, weights, weight_type, bias, excess, n_null, ok)
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
        end subroutine kurtosis_i32
        !> `pf_moments` over a 32-bit integer array: every tier-A quantity in one pair of passes.
        module subroutine moments_i32(values, n_valid, mean, variance, stddev, sem, skewness, kurtosis, vsum, vmin, &
                vmax, is_valid, weights, weight_type, ddof, bias, excess, &
                n_null)
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
        end subroutine moments_i32
        !> `pf_sum` over a 64-bit integer array.
        !>
        !> **An `integer(int64)` above 2**53 loses exactness** on the way into the real64 buffer
        !> every statistic here is computed in. That is unavoidable for a mean and irrelevant for
        !> the workloads this serves -- nobody averages an object ID -- but it is NOT irrelevant
        !> for a median, which `pf_nth_quantile` computes exactly on the original array.
        module subroutine sum_i64(values, s, is_valid, weights, n_null, ok)
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
        end subroutine sum_i64
        !> `pf_mean` over a 64-bit integer array.
        !>
        !> **An `integer(int64)` above 2**53 loses exactness** on the way into the real64 buffer
        !> every statistic here is computed in. That is unavoidable for a mean and irrelevant for
        !> the workloads this serves -- nobody averages an object ID -- but it is NOT irrelevant
        !> for a median, which `pf_nth_quantile` computes exactly on the original array.
        module subroutine mean_i64(values, m, is_valid, weights, n_null, ok)
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
        end subroutine mean_i64
        !> `pf_variance` over a 64-bit integer array.
        !>
        !> **An `integer(int64)` above 2**53 loses exactness** on the way into the real64 buffer
        !> every statistic here is computed in. That is unavoidable for a mean and irrelevant for
        !> the workloads this serves -- nobody averages an object ID -- but it is NOT irrelevant
        !> for a median, which `pf_nth_quantile` computes exactly on the original array.
        module subroutine variance_i64(values, v, is_valid, weights, weight_type, ddof, n_null, ok)
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
        end subroutine variance_i64
        !> `pf_stddev` over a 64-bit integer array.
        !>
        !> **An `integer(int64)` above 2**53 loses exactness** on the way into the real64 buffer
        !> every statistic here is computed in. That is unavoidable for a mean and irrelevant for
        !> the workloads this serves -- nobody averages an object ID -- but it is NOT irrelevant
        !> for a median, which `pf_nth_quantile` computes exactly on the original array.
        module subroutine stddev_i64(values, sd, is_valid, weights, weight_type, ddof, n_null, ok)
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
        end subroutine stddev_i64
        !> `pf_sem` over a 64-bit integer array.
        !>
        !> **An `integer(int64)` above 2**53 loses exactness** on the way into the real64 buffer
        !> every statistic here is computed in. That is unavoidable for a mean and irrelevant for
        !> the workloads this serves -- nobody averages an object ID -- but it is NOT irrelevant
        !> for a median, which `pf_nth_quantile` computes exactly on the original array.
        module subroutine sem_i64(values, se, is_valid, weights, weight_type, ddof, n_null, ok)
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
        end subroutine sem_i64
        !> `pf_skewness` over a 64-bit integer array.
        !>
        !> **An `integer(int64)` above 2**53 loses exactness** on the way into the real64 buffer
        !> every statistic here is computed in. That is unavoidable for a mean and irrelevant for
        !> the workloads this serves -- nobody averages an object ID -- but it is NOT irrelevant
        !> for a median, which `pf_nth_quantile` computes exactly on the original array.
        module subroutine skewness_i64(values, g, is_valid, weights, weight_type, bias, n_null, ok)
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
        end subroutine skewness_i64
        !> `pf_kurtosis` over a 64-bit integer array.
        !>
        !> **An `integer(int64)` above 2**53 loses exactness** on the way into the real64 buffer
        !> every statistic here is computed in. That is unavoidable for a mean and irrelevant for
        !> the workloads this serves -- nobody averages an object ID -- but it is NOT irrelevant
        !> for a median, which `pf_nth_quantile` computes exactly on the original array.
        module subroutine kurtosis_i64(values, k, is_valid, weights, weight_type, bias, excess, n_null, ok)
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
        end subroutine kurtosis_i64
        !> `pf_moments` over a 64-bit integer array: every tier-A quantity in one pair of passes.
        !>
        !> **An `integer(int64)` above 2**53 loses exactness** on the way into the real64 buffer
        !> every statistic here is computed in. That is unavoidable for a mean and irrelevant for
        !> the workloads this serves -- nobody averages an object ID -- but it is NOT irrelevant
        !> for a median, which `pf_nth_quantile` computes exactly on the original array.
        module subroutine moments_i64(values, n_valid, mean, variance, stddev, sem, skewness, kurtosis, vsum, vmin, &
                vmax, is_valid, weights, weight_type, ddof, bias, excess, &
                n_null)
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
        end subroutine moments_i64
        !> `pf_sum` over a 32-bit real array.
        module subroutine sum_f32(values, s, is_valid, weights, skipnan, n_null, n_nan, ok)
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
        end subroutine sum_f32
        !> `pf_mean` over a 32-bit real array.
        module subroutine mean_f32(values, m, is_valid, weights, skipnan, n_null, n_nan, ok)
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
        end subroutine mean_f32
        !> `pf_variance` over a 32-bit real array.
        module subroutine variance_f32(values, v, is_valid, weights, weight_type, ddof, skipnan, n_null, n_nan, ok)
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
        end subroutine variance_f32
        !> `pf_stddev` over a 32-bit real array.
        module subroutine stddev_f32(values, sd, is_valid, weights, weight_type, ddof, skipnan, n_null, n_nan, ok)
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
        end subroutine stddev_f32
        !> `pf_sem` over a 32-bit real array.
        module subroutine sem_f32(values, se, is_valid, weights, weight_type, ddof, skipnan, n_null, n_nan, ok)
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
        end subroutine sem_f32
        !> `pf_skewness` over a 32-bit real array.
        module subroutine skewness_f32(values, g, is_valid, weights, weight_type, bias, skipnan, n_null, n_nan, ok)
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
        end subroutine skewness_f32
        !> `pf_kurtosis` over a 32-bit real array.
        module subroutine kurtosis_f32(values, k, is_valid, weights, weight_type, bias, excess, skipnan, n_null, n_nan, ok)
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
        end subroutine kurtosis_f32
        !> `pf_moments` over a 32-bit real array: every tier-A quantity in one pair of passes.
        module subroutine moments_f32(values, n_valid, mean, variance, stddev, sem, skewness, kurtosis, vsum, vmin, &
                vmax, is_valid, weights, weight_type, ddof, bias, excess, &
                skipnan, n_null, n_nan)
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
        end subroutine moments_f32
        !> `pf_sum` over a logical array.
        !>
        !> `.true.` widens to 1 and `.false.` to 0, so the mean of a logical array is the
        !> fraction that are true.
        module subroutine sum_bool(values, s, is_valid, weights, n_null, ok)
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
        end subroutine sum_bool
        !> `pf_mean` over a logical array.
        !>
        !> `.true.` widens to 1 and `.false.` to 0, so the mean of a logical array is the
        !> fraction that are true.
        module subroutine mean_bool(values, m, is_valid, weights, n_null, ok)
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
        end subroutine mean_bool
        !> `pf_variance` over a logical array.
        !>
        !> `.true.` widens to 1 and `.false.` to 0, so the mean of a logical array is the
        !> fraction that are true.
        module subroutine variance_bool(values, v, is_valid, weights, weight_type, ddof, n_null, ok)
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
        end subroutine variance_bool
        !> `pf_stddev` over a logical array.
        !>
        !> `.true.` widens to 1 and `.false.` to 0, so the mean of a logical array is the
        !> fraction that are true.
        module subroutine stddev_bool(values, sd, is_valid, weights, weight_type, ddof, n_null, ok)
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
        end subroutine stddev_bool
        !> `pf_sem` over a logical array.
        !>
        !> `.true.` widens to 1 and `.false.` to 0, so the mean of a logical array is the
        !> fraction that are true.
        module subroutine sem_bool(values, se, is_valid, weights, weight_type, ddof, n_null, ok)
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
        end subroutine sem_bool
        !> `pf_skewness` over a logical array.
        !>
        !> `.true.` widens to 1 and `.false.` to 0, so the mean of a logical array is the
        !> fraction that are true.
        module subroutine skewness_bool(values, g, is_valid, weights, weight_type, bias, n_null, ok)
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
        end subroutine skewness_bool
        !> `pf_kurtosis` over a logical array.
        !>
        !> `.true.` widens to 1 and `.false.` to 0, so the mean of a logical array is the
        !> fraction that are true.
        module subroutine kurtosis_bool(values, k, is_valid, weights, weight_type, bias, excess, n_null, ok)
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
        end subroutine kurtosis_bool
        !> `pf_moments` over a logical array: every tier-A quantity in one pair of passes.
        !>
        !> `.true.` widens to 1 and `.false.` to 0, so the mean of a logical array is the
        !> fraction that are true.
        module subroutine moments_bool(values, n_valid, mean, variance, stddev, sem, skewness, kurtosis, vsum, vmin, &
                vmax, is_valid, weights, weight_type, ddof, bias, excess, &
                n_null)
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
        end subroutine moments_bool
        !> `pf_sum` over a scalar numeric `parquet_column`.
        !>
        !> Dispatched on the column's kind at run time, and **scalar numeric columns only**:
        !> int32, int64, float32, float64 and logical. A string, temporal or container column
        !> aborts naming its kind, and so does a column more than one element wide -- flattening
        !> a vector column into one population is a different statistic and nobody should get it
        !> by accident. **The column's own validity is the only source of nullness**, so passing
        !> `is_valid=` alongside one aborts rather than silently preferring one of two sources.
        module subroutine sum_col(values, s, is_valid, weights, skipnan, n_null, n_nan, ok)
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
        end subroutine sum_col
        !> `pf_mean` over a scalar numeric `parquet_column`.
        !>
        !> Dispatched on the column's kind at run time, and **scalar numeric columns only**:
        !> int32, int64, float32, float64 and logical. A string, temporal or container column
        !> aborts naming its kind, and so does a column more than one element wide -- flattening
        !> a vector column into one population is a different statistic and nobody should get it
        !> by accident. **The column's own validity is the only source of nullness**, so passing
        !> `is_valid=` alongside one aborts rather than silently preferring one of two sources.
        module subroutine mean_col(values, m, is_valid, weights, skipnan, n_null, n_nan, ok)
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
        end subroutine mean_col
        !> `pf_variance` over a scalar numeric `parquet_column`.
        !>
        !> Dispatched on the column's kind at run time, and **scalar numeric columns only**:
        !> int32, int64, float32, float64 and logical. A string, temporal or container column
        !> aborts naming its kind, and so does a column more than one element wide -- flattening
        !> a vector column into one population is a different statistic and nobody should get it
        !> by accident. **The column's own validity is the only source of nullness**, so passing
        !> `is_valid=` alongside one aborts rather than silently preferring one of two sources.
        module subroutine variance_col(values, v, is_valid, weights, weight_type, ddof, skipnan, n_null, n_nan, ok)
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
        end subroutine variance_col
        !> `pf_stddev` over a scalar numeric `parquet_column`.
        !>
        !> Dispatched on the column's kind at run time, and **scalar numeric columns only**:
        !> int32, int64, float32, float64 and logical. A string, temporal or container column
        !> aborts naming its kind, and so does a column more than one element wide -- flattening
        !> a vector column into one population is a different statistic and nobody should get it
        !> by accident. **The column's own validity is the only source of nullness**, so passing
        !> `is_valid=` alongside one aborts rather than silently preferring one of two sources.
        module subroutine stddev_col(values, sd, is_valid, weights, weight_type, ddof, skipnan, n_null, n_nan, ok)
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
        end subroutine stddev_col
        !> `pf_sem` over a scalar numeric `parquet_column`.
        !>
        !> Dispatched on the column's kind at run time, and **scalar numeric columns only**:
        !> int32, int64, float32, float64 and logical. A string, temporal or container column
        !> aborts naming its kind, and so does a column more than one element wide -- flattening
        !> a vector column into one population is a different statistic and nobody should get it
        !> by accident. **The column's own validity is the only source of nullness**, so passing
        !> `is_valid=` alongside one aborts rather than silently preferring one of two sources.
        module subroutine sem_col(values, se, is_valid, weights, weight_type, ddof, skipnan, n_null, n_nan, ok)
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
        end subroutine sem_col
        !> `pf_skewness` over a scalar numeric `parquet_column`.
        !>
        !> Dispatched on the column's kind at run time, and **scalar numeric columns only**:
        !> int32, int64, float32, float64 and logical. A string, temporal or container column
        !> aborts naming its kind, and so does a column more than one element wide -- flattening
        !> a vector column into one population is a different statistic and nobody should get it
        !> by accident. **The column's own validity is the only source of nullness**, so passing
        !> `is_valid=` alongside one aborts rather than silently preferring one of two sources.
        module subroutine skewness_col(values, g, is_valid, weights, weight_type, bias, skipnan, n_null, n_nan, ok)
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
        end subroutine skewness_col
        !> `pf_kurtosis` over a scalar numeric `parquet_column`.
        !>
        !> Dispatched on the column's kind at run time, and **scalar numeric columns only**:
        !> int32, int64, float32, float64 and logical. A string, temporal or container column
        !> aborts naming its kind, and so does a column more than one element wide -- flattening
        !> a vector column into one population is a different statistic and nobody should get it
        !> by accident. **The column's own validity is the only source of nullness**, so passing
        !> `is_valid=` alongside one aborts rather than silently preferring one of two sources.
        module subroutine kurtosis_col(values, k, is_valid, weights, weight_type, bias, excess, skipnan, n_null, n_nan, ok)
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
                skipnan, n_null, n_nan)
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
        end subroutine moments_col
        !> `%compute` over a 32-bit integer array.
        module subroutine obj_compute_i32(self, values, retain, is_valid, weights, weight_type)
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
        module subroutine obj_compute_i64(self, values, retain, is_valid, weights, weight_type)
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
        module subroutine obj_compute_f32(self, values, retain, is_valid, weights, weight_type, skipnan)
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
        module subroutine obj_compute_bool(self, values, retain, is_valid, weights, weight_type)
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
        module subroutine obj_compute_col(self, values, retain, is_valid, weights, weight_type, skipnan)
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
        module subroutine obj_compute_f64(self, values, retain, is_valid, weights, weight_type, skipnan)
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
        !! Both must agree on `retain` and on the weight convention, and the source must have been
        !! computed; a mismatch aborts, naming which. A source with no elements at all is a no-op.
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
    end interface
    !
end module parquet_stats ! GCOVR_EXCL_LINE
