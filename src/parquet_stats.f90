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
end module parquet_stats ! GCOVR_EXCL_LINE
