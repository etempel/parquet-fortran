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
    use iso_fortran_env, only : int32, int64, real32, real64
    implicit none
    private

    public :: pf_count_valid
    public :: pf_sum, pf_mean, pf_variance, pf_stddev, pf_sem
    public :: pf_skewness, pf_kurtosis, pf_moments

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
    end interface pf_sum
    !
    !> The arithmetic mean of a population, weighted when `weights` is present.
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
        !> `pf_mean` over a 64-bit real array: `sum(w*x) / sum(w)`.
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
end module parquet_stats ! GCOVR_EXCL_LINE
