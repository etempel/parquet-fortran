#!/usr/bin/env python3
"""Generate the per-kind blocks of the `parquet_stats` module.

`parquet_stats` is this library's array-statistics API: reductions over plain Fortran arrays,
under `pf_*` names, with no reader state and no Arrow. Every public entry point is the same
handful of statements repeated across the numeric element kinds -- widen into a real64 view,
apply the null/NaN/weight policy, call the core -- which is what this script emits, each with
its own `!>`/`!!` doc-comments and its own interface body.

Emitted (both COMMITTED to the repository, exactly like the other generators' output -- nothing
is generated at build time, so the fpm build stays dependency-free):

  src/parquet_stats.f90          module spec: the public generics and the interface bodies for
                                 every submodule-implemented procedure.
  src/parquet_stats_kernel.f90   the per-kind bodies plus the shared argument guards.

THE KIND TABLE: the numeric scalar rows are imported from tools/generate_parquet_columns.py --
the single place a supported column kind is declared -- rather than copied. `--self-test`
cross-checks the imported set, so a change over there fails here rather than silently changing
what this module summarises.

SCOPE, and why it is this small: this is phase P1 of feature_pandas_S4.md, whose whole job is the
module, its tier and its wiring -- deliberately with no statistics in it. `pf_count_valid` is the
one procedure, chosen because it exercises every convention the later phases inherit (the
canonical optional-argument order, the null/NaN/weight exclusion ordering, the abort-on-misuse
rule) while computing nothing that needs a numerical contract. The `parquet_column` form arrives
in P4 with the ingress rules that govern it; the real work arrives in P2.

Usage:  tools/generate_parquet_stats.py [--check] [--self-test]

  --check      regenerate into memory and compare with the committed files; exit 1 on any
               difference (so CI, or a reviewer, can prove the committed output is current).
  --self-test  verify the imported kind table still has the shape this script assumes.

Maintainer-only: tools/ is an allow-list in tools/prep_fpm_publish.sh, so this script is stripped
from the published package automatically and needs no TOOLS_KEEP entry.
"""

import argparse
import importlib.util
import pathlib
import sys

REPO_ROOT = pathlib.Path(__file__).resolve().parent.parent


def _load_numeric_kinds():
    """Import the numeric SCALAR rows of the columns generator's kind table -- one source of truth."""
    path = REPO_ROOT / "tools" / "generate_parquet_columns.py"
    spec = importlib.util.spec_from_file_location("_pq_columns_gen", path)
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return [k for k in mod.KINDS if k[4] == 1 and k[5] == "num"]


#: Tags of the imported numeric scalar kinds, in order. --self-test asserts this still holds; the
#: emitted module's type coverage is defined by it.
EXPECTED_NUMERIC_TAGS = ["i32", "i64", "f32", "f64", "bool"]

# --------------------------------------------------------------------------------------
# The type table. One row per element kind this module summarises.
#
#   tag    short name used in every specific's name (count_valid_<tag>)
#   decl   the Fortran declaration of ONE element
#   what   human-readable name, used in doc-comments
#   real   .true. when the kind can hold a NaN -- which is what decides whether the specific
#          carries `skipnan`/`n_nan` at all. An integer or logical array has no NaN to skip, and
#          an argument that can never do anything is worse than an absent one
#          (feature_pandas_S4.md, F5).
# --------------------------------------------------------------------------------------
TYPES = [
    ("i32",  "integer(int32)", "32-bit integer", False),
    ("i64",  "integer(int64)", "64-bit integer", False),
    ("f32",  "real(real32)",   "32-bit real",    True),
    ("f64",  "real(real64)",   "64-bit real",    True),
    ("bool", "logical",        "logical",        False),
]

BANNER = """!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
! GENERATED FILE -- DO NOT EDIT BY HAND.
! Regenerate with:  tools/generate_parquet_stats.py
! The kind table lives in tools/generate_parquet_columns.py; edit it there, not here.
!
"""

SPEC_HEAD = BANNER + """!> Array statistics over plain Fortran arrays -- the `pf_*` reduction family.
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

"""

KERNEL_HEAD = BANNER + """!> The per-kind entry points of `parquet_stats`.
!!
!! Each specific is the same shape: check the optional arguments against `values`, then walk the
!! array applying the family's exclusion order -- nullness, then NaN, then weight.
!!
!! **The guards these bodies call live in `parquet_stats_core`, not here**, because that submodule
!! needs them too and two sibling submodules cannot reach each other's contained procedures. Their
!! interfaces are declared in the module spec for exactly that reason; a private helper contained
!! directly in the module would compile and then fail to link (CLAUDE.md).
submodule (parquet_stats) parquet_stats_kernel
    implicit none

contains

"""


# ======================================================================================
# The real64 moment core (phase P2). Hand-written bodies live in src/parquet_stats_core.f90;
# only the declarations are here, because the module spec is what this script owns.
# ======================================================================================

#: Optional-dummy fragments, in the canonical order. Composed rather than repeated so that eight
#: interface bodies cannot drift from each other on the rules that govern all of them.
D = {}
D["retain"] = """            logical, intent(in), optional :: retain
            !! .true. by default: keep a copy of the surviving values, which costs 8 bytes per
            !! survivor (16 when weighted) and is what makes `%update` and `%merge` EXACT rather
            !! than approximate. .false. keeps nothing but the accumulator, which is O(1) whatever
            !! the population size and is what a row-group loop over a file larger than RAM wants.
            !! Fixed for the object's lifetime; every accumulator in one `%merge` must agree."""
D["consume"] = """            logical, intent(in), optional :: consume
            !! .false. by default: the sources are left intact. .true. clears each source as it is
            !! folded in, which is what the threaded shape wants -- the partials are dead after the
            !! region, and keeping them doubles the peak memory for nothing."""
D["is_valid"] = """            logical, intent(in), optional :: is_valid(:)
            !! per element: .false. marks a null. Absent means no element is null."""
D["weights"] = """            real(real64), intent(in), optional :: weights(:)
            !! per element weight, non-negative. A ZERO weight removes the element from the
            !! population; a negative, NaN or infinite one aborts. Absent means every weight is 1."""
D["weight_type"] = """            character(len=*), intent(in), optional :: weight_type
            !! "reliability" (the default) or "frequency". The two differ only when the weights are
            !! unequal: a FREQUENCY weight of 3 says the value occurred three times, so the count
            !! `ddof` is charged against is `sum(weights)`; a RELIABILITY weight says the value is
            !! that much more precise, so the count is Kish's effective size
            !! `sum(weights)**2 / sum(weights**2)`. Any other token aborts, listing both."""
D["ddof"] = """            integer, intent(in), optional :: ddof
            !! delta degrees of freedom; **1 by default** -- the sample variance, as pandas
            !! returns. numpy's default is 0, so pass `ddof=0` to reproduce `np.var`/`np.std`.
            !! A plain default-kind integer deliberately: it is bounded by the population size,
            !! so there is no int64 form to provide. `ddof >= n_valid` gives NaN, not a division."""
D["bias"] = """            logical, intent(in), optional :: bias
            !! .false. by default: the bias-CORRECTED G1/G2, which is what pandas returns.
            !! .true. gives the uncorrected g1/g2, which is what scipy returns by default."""
D["excess"] = """            logical, intent(in), optional :: excess
            !! .true. by default: EXCESS kurtosis, which is 0 for a normal population.
            !! .false. adds 3 back, giving the raw fourth-moment ratio."""
D["skipnan"] = """            logical, intent(in), optional :: skipnan
            !! .true. (the default) excludes a NaN from the population, as a null is excluded and
            !! as `pf_minmax` has always done; .false. restores numpy's propagating behaviour, in
            !! which one NaN makes every answer NaN."""
D["method"] = """            character(len=*), intent(in), optional :: method
            !! how a fractional position between two order statistics is resolved, in numpy's
            !! spelling: "linear" (the default -- Hyndman-Fan type 7, what numpy and pandas do),
            !! "lower", "higher", "nearest", "midpoint", or "inverted_cdf" (a step function on the
            !! plain cumulative scale, and the only token numpy itself accepts weights with). Any
            !! other token aborts, listing all six. Matched case-insensitively.
            !! **`weight_type="frequency"` changes the default to "inverted_cdf"**, because that
            !! token IS frequency expansion; an explicit `method=` still wins."""
D["kind"] = """            character(len=*), intent(in), optional :: kind
            !! which convention counts a value EQUAL to `score`, in scipy's spelling: "rank" (the
            !! default; ties count half), "weak" (a value <= score counts), "strict" (only < score)
            !! or "mean" (the average of weak and strict). Any other token aborts, listing all
            !! four. Matched case-insensitively."""
D["scale"] = """            character(len=*), intent(in), optional :: scale
            !! "normal" (the default) divides the raw deviation by `Phi^-1(3/4)`, which makes the
            !! result a consistent estimator of the standard deviation for Gaussian data --
            !! scipy's `median_abs_deviation(scale='normal')`, and what an astronomy script means
            !! by "the MAD". That is a multiplication by 1.482602218505602, and **not** by the
            !! rounded 1.4826 the textbooks quote, which differs by 1.5e-06 relative. "raw" is the
            !! unscaled median of |x - center|. Any other token aborts, naming both. Matched
            !! case-insensitively."""
D["center"] = """            real(real64), intent(in), optional :: center
            !! the centre the deviations are taken about. Absent uses the population's own median,
            !! which is what scipy does. Supplying one skips ONE selection and not the ordering:
            !! the median of |x - center| still has to be found. A NaN or infinite centre ABORTS
            !! -- unlike a NaN value, which is an ordinary data condition, a NaN centre can only
            !! come from the caller's own arithmetic."""
D["count"] = """            integer(int64), intent(out), optional :: count
            !! how many elements hold the modal value. `0` when the population is empty. Counts
            !! ELEMENTS even when weights decided which value won, so a weighted mode reports the
            !! occurrences of the value carrying the greatest total weight."""
D["threads"] = """            integer, intent(in), optional :: threads
            !! how many threads the central-moment pass may use. Absent takes the automatic rule:
            !! the `parquet_sort_threads` setting, capped by the processors actually available and
            !! by a measured work floor, and 1 inside a caller's own parallel region. **The answer
            !! does not depend on this argument** -- the block decomposition is a function of the
            !! population size alone, so 1, 8 and a build with no OpenMP at all return the same
            !! bits. It is a speed control and never an accuracy one."""
D["n_null"] = """            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded."""
D["n_nan"] = """            integer(int64), intent(out), optional :: n_nan
            !! how many were excluded as NaN and were not already null."""
D["ok"] = """            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure."""


#: The two type bodies the spec gains for `pf_stats`. Emitted as one block because the ordering
#: is a language constraint rather than a preference: `pf_stats` has a component of `stats_acc`,
#: so `stats_acc` has to be declared first.
#:
#: `stats_acc` lives HERE rather than in `parquet_stats_core` for the same reason the argument
#: guards do -- a sibling submodule cannot see a type declared in another submodule, and tier B
#: will need it from `parquet_stats_order`.
TYPE_BLOCK = """    !
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
@@compute_bindings@@
        !> Summarises a resident array; the usual entry point. Accepts the same kinds the one-shot
        !> family does -- the four widened numeric arrays and a scalar numeric `parquet_column` --
        !> and every one of them reaches the same real64 engine.
        generic :: compute => obj_compute_f64, @@compute_list@@
        procedure, private :: obj_update_f64 !! `%update` over a 64-bit real array.
@@update_bindings@@
        !> Folds one more batch into the population, over any kind `%compute` accepts.
        generic :: update => obj_update_f64, @@update_list@@
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
    end type pf_stats"""


#: The `pf_stats` lifecycle bindings. Literal rather than table-driven: each of the five has a
#: different argument list and a different contract, so a table would carry one row per procedure
#: and buy nothing.
LIFECYCLE_IFACES = """        !> Summarises a resident array: the usual way to build a `pf_stats`.
        !!
        !! Two traversals, whatever is asked of the result afterwards. Any previous contents of
        !! `self` are discarded, so an object may be reused across a loop of groups without
        !! `%clear` in between.
        module subroutine obj_compute_f64(self, values, retain, is_valid, weights, weight_type, &
                skipnan, threads)
@@compute@@
        end subroutine obj_compute_f64
        !> Arms an empty accumulator for a loop of `%update`, or for a `%merge`.
        !!
        !! The population is EMPTY afterwards, which is a different state from uncomputed: every
        !! count answers `0`, every moment is NaN, and `%is_computed()` is `.true.`. `weight_type`
        !! and `skipnan` are fixed here because the queries do not take them.
        module subroutine obj_init(self, retain, weight_type, skipnan)
@@init@@
        end subroutine obj_init
        !> Folds one more batch of values into the population.
        !!
        !! In retained mode the survivors are appended and the moments are marked for
        !! recomputation, so a loop of k updates costs k traversals and ONE recomputation --
        !! and the result is bit for bit what `%compute` over the concatenated batches gives.
        !! In streaming mode this is one traversal and O(1) memory, combining by the Chan/Pebay
        !! formulas, and the accuracy is theirs.
        module subroutine obj_update_f64(self, values, is_valid, weights)
@@update@@
        end subroutine obj_update_f64
        !> Folds one other accumulator into this one.
        !!
        !! Both must agree on `retain` and on the weight convention, and the source must have been
        !! computed; a mismatch aborts, naming which. A source with no elements at all is a no-op.
        module subroutine obj_merge_one(self, other, consume)
            class(pf_stats), intent(inout) :: self !! the destination.
            type(pf_stats), intent(inout) :: other
            !! the source. `intent(inout)` because `consume` clears it; it is unchanged otherwise.
@@consume@@
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
@@consume@@
        end subroutine obj_merge_many
        !> Returns the object to its default-initialised state, freeing everything it held.
        !!
        !! `%is_computed()` is `.false.` afterwards. Calling it on an object that was never
        !! computed is legal and does nothing.
        module subroutine obj_clear(self)
            class(pf_stats), intent(inout) :: self !! the accumulator.
        end subroutine obj_clear"""


#: The tier-A queries: one row each, because they differ only in name, result and doc. Adding a
#: query is a row here plus a `procedure ::` line in TYPE_BLOCK and a body in the core.
#: (implementation, result declaration, result doc, extra optionals, doc lines)
QUERIES = [
    ("obj_is_computed", "logical", "`.true.` once `%compute` or `%init` has run.", [],
     ["Whether this accumulator holds a population yet.",
      "",
      "`.false.` only for a default-initialised object and for one `%clear` has reset. An",
      "accumulator armed by `%init` and never updated holds an EMPTY population, which is a",
      "different thing: it answers `.true.` here and `0` to every count."]),
    ("obj_retains", "logical", "`.true.` when the surviving values are being kept.", [],
     ["Whether this accumulator keeps the surviving values.",
      "",
      "Fixed by `retain` at `%compute`/`%init` and never changes. It decides whether `%merge` and",
      "`%update` are exact or approximate, and whether the order statistics of a later phase are",
      "available at all."]),
    ("obj_n", "integer(int64)", "elements offered, including every excluded one.", [],
     ["How many elements have been offered to this accumulator.",
      "",
      "Counts nulls, NaNs and zero-weight elements too, so `%n() - %n_valid()` is everything the",
      "exclusion rules removed. pandas' `size` rather than its `count`."]),
    ("obj_n_valid", "integer(int64)", "elements in the population.", [],
     ["How many elements are in the population -- pandas' `count`.",
      "",
      "Carried across `%update` and `%merge` as an integer rather than re-derived, so it is exact",
      "and needs no recomputation to answer."]),
    ("obj_n_null", "integer(int64)", "elements `is_valid` excluded.", [],
     ["How many elements were excluded as null."]),
    ("obj_n_nan", "integer(int64)", "elements excluded as NaN.", [],
     ["How many elements were excluded as NaN and were not already null.",
      "",
      "Always `0` under `skipnan = .false.`, where a NaN stays in the population."]),
    ("obj_sum_weights", "real(real64)", "`sum(w)` over the population.", [],
     ["The total weight of the population.",
      "",
      "Exactly `real(%n_valid())` when no weights were supplied. This is `sum(w)` and NOT the",
      "effective size the reliability convention charges `ddof` against; those differ whenever the",
      "weights are unequal."]),
    ("obj_sum", "real(real64)", "`sum(w*x)`; exactly 0 for an empty population.", [],
     ["The sum of the population.",
      "",
      "Weighted, this is `sum(w*x)`. An empty population sums to exactly `0`: it is the one",
      "quantity here that emptiness still defines, and it is what numpy and pandas both return."]),
    ("obj_mean", "real(real64)", "the weighted mean, or NaN for an empty population.", [],
     ["The mean of the population."]),
    ("obj_variance", "real(real64)", "the variance, or NaN when it is undefined.", ["ddof"],
     ["The variance of the population.",
      "",
      "`ddof` at or above the effective size gives a NaN rather than a division -- including the",
      "single-element case at the default `ddof = 1`, which is a data condition, not an error."]),
    ("obj_stddev", "real(real64)", "the standard deviation, or NaN when it is undefined.", ["ddof"],
     ["The standard deviation: the square root of `%variance(ddof)`."]),
    ("obj_sem", "real(real64)", "the standard error, or NaN when it is undefined.", ["ddof"],
     ["The standard error of the mean: `%stddev(ddof)` over the root of the effective size.",
      "",
      "`scipy.stats.sem` at its default `ddof = 1`, which is this library's default too."]),
    ("obj_skewness", "real(real64)", "the skewness, or NaN when it is undefined.", ["bias"],
     ["The skewness of the population.",
      "",
      "NaN for a constant population, and -- bias-corrected, which is the default -- for one with",
      "fewer than three elements."]),
    ("obj_kurtosis", "real(real64)", "the kurtosis, or NaN when it is undefined.", ["bias", "excess"],
     ["The kurtosis of the population.",
      "",
      "NaN for a constant population, and -- bias-corrected, which is the default -- for one with",
      "fewer than four elements. Excess by default, so a normal population gives 0."]),
    ("obj_vmin", "real(real64)", "the smallest value, or NaN for an empty population.", [],
     ["The smallest value in the population."]),
    ("obj_vmax", "real(real64)", "the largest value, or NaN for an empty population.", [],
     ["The largest value in the population."]),
    ("obj_range", "real(real64)", "`vmax - vmin`, or NaN for an empty population.", [],
     ["The spread of the population: `%vmax() - %vmin()`."]),
]


#: The traversal counter. Public because it must be: `parquet_stats` reaches no `bind(C)` surface,
#: so the C++-side debug-hook convention this project prefers is unavailable to it.
DEBUG_IFACES = """        !> How many full traversals of a population's values this process has performed.
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
        !> How many times any `pf_stats` has built its sorted buffer.
        !!
        !! Test-only, and the **only** observable that can tell a working order cache from one
        !! that never invalidates. Every equality assertion about a median passes just as happily
        !! against a stale answer -- a stale median is a plausible number -- so `sorts == 1` across
        !! four different order statistics, and `sorts == 2` after an intervening `%update`, is
        !! what the caching contract is actually pinned by.
        !!
        !! Process-global and unsynchronised, which is why the `stats` suite is excluded from the
        !! per-suite parallelism in `test/run_tester.f90`.
        module function parquet_debug_stats_sorts() result(res)
            integer(int64) :: res !! sorted-buffer builds since the last reset.
        end function parquet_debug_stats_sorts
        !> Resets the sorted-buffer counter to zero.
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
        end subroutine parquet_debug_set_stats_quantile_sort_min"""


def query_iface(impl, res_decl, res_doc, opts, doc):
    """One interface body for a tier-A query."""
    args = ", ".join(["self"] + opts)
    lines = [("        !> " + line).rstrip() for line in doc]
    lines.append("        module function %s(%s) result(res)" % (impl, args))
    lines.append("            class(pf_stats), intent(inout) :: self")
    lines.append("            !! the accumulator; a query may complete a deferred recomputation.")
    for key in opts:
        lines.append(D[key])
    lines.append("            %s :: res !! %s" % (res_decl, res_doc))
    lines.append("        end function %s" % impl)
    return "\n".join(lines)


def object_ifaces():
    """Every `pf_stats` binding implementation, plus the two debug hooks."""
    # `%` formatting is unusable here: the emitted Fortran is full of `%compute`, `%merge`, ...
    parts = {
        "compute": "\n".join([
            "            class(pf_stats), intent(inout) :: self",
            "            !! the accumulator; any previous contents are discarded.",
            "            real(real64), intent(in) :: values(:) !! the population, before exclusions."]
            + [D[k] for k in ("retain", "is_valid", "weights", "weight_type", "skipnan", "threads")]),
        "init": "\n".join([
            "            class(pf_stats), intent(inout) :: self",
            "            !! the accumulator; any previous contents are discarded."]
            + [D[k] for k in ("retain", "weight_type", "skipnan")]),
        "update": "\n".join([
            "            class(pf_stats), intent(inout) :: self !! the accumulator.",
            "            real(real64), intent(in) :: values(:) !! the batch, before exclusions."]
            + [D[k] for k in ("is_valid", "weights")]),
        "consume": D["consume"],
    }
    text = LIFECYCLE_IFACES
    for key, value in parts.items():
        text = text.replace("@@" + key + "@@", value)
    out = ["    !",
           "    ! ---- pf_stats (implemented in parquet_stats_core) ----",
           "    interface",
           text]
    out.extend(query_iface(*q) for q in QUERIES)
    out.append(DEBUG_IFACES)
    out.append("    end interface")
    out.append("    !")
    out.append("    ! ---- pf_stats tier B (implemented in parquet_stats_order) ----")
    out.append("    interface")
    tierb = OBJ_ORDER_IFACES
    tierb = tierb.replace("@@method@@", D["method"])
    tierb = tierb.replace("@@kind@@", D["kind"])
    tierb = tierb.replace("@@scale@@", D["scale"])
    tierb = tierb.replace("@@center@@", D["center"])
    out.append(tierb.rstrip("\n"))
    out.append("    end interface")
    return "\n".join(out)


#: The tier-B bindings. Literal rather than table-driven for the same reason the lifecycle block
#: is: each has a different argument list, and a table would carry one row per procedure.
#:
#: Every one takes `self` as `intent(inout)` and none is `pure`, because the FIRST of them to run
#: builds the sorted buffer -- that is the whole point of the type, and it is the same trade the
#: tier-A queries already make for the deferred recomputation.
OBJ_ORDER_IFACES = """        !> The interpolating median of the population.
        !!
        !! Builds tier B on first use and reads it thereafter, so a median followed by an IQR and
        !! three quantiles is ONE ordering; `parquet_debug_stats_sorts()` is what asserts it.
        !!
        !! **Aborts on a streaming accumulator** (`retain = .false.`), because no values were
        !! kept and there is nothing to order. The message says so and names the fix.
        module function obj_median(self, method) result(res)
            class(pf_stats), intent(inout) :: self
            !! the accumulator; the first order statistic builds its sorted buffer.
@@method@@
            real(real64) :: res !! the median, or NaN for an empty population.
        end function obj_median
        !> One quantile of the population, on a 0-1 scale.
        module function obj_quantile(self, p, method) result(res)
            class(pf_stats), intent(inout) :: self
            !! the accumulator; the first order statistic builds its sorted buffer.
            real(real64), intent(in) :: p !! the probability, 0 to 1. Outside that aborts.
@@method@@
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
@@method@@
        end subroutine obj_quantiles
        !> The interquartile range of the population.
        module function obj_iqr(self, method) result(res)
            class(pf_stats), intent(inout) :: self
            !! the accumulator; the first order statistic builds its sorted buffer.
@@method@@
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
@@kind@@
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
@@scale@@
@@center@@
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
        end subroutine obj_print"""


#: The shared argument guards. They are separate module procedures, implemented once in
#: `parquet_stats_core`, because BOTH submodules call them and two siblings cannot reach each
#: other's contained procedures -- see the kernel's own header for why a module-contained helper
#: is not an option either.
GUARD_SPEC = """    !
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
    end interface"""


#: The tail every moment generic's doc-comment shares. Stated once so that eight generics cannot
#: drift from each other on the rules that govern all of them.
_COMMON = """    !>
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
    !> OpenMP return the identical bits."""


def iface(name, doc, out_decl, opts):
    """One interface body for a real64 core procedure."""
    args = ", ".join(["values", out_decl[0]] + opts)
    lines = ["        !> " + line for line in doc]
    lines.append("        module subroutine %s(%s)" % (name, args))
    lines.append("            real(real64), intent(in) :: values(:) !! the population.")
    lines.append("            real(real64), intent(out) :: %s !! %s" % out_decl)
    for key in opts:
        lines.append(D[key])
    lines.append("        end subroutine %s" % name)
    return "\n".join(lines)


MOMENT_OPTS = ["is_valid", "weights", "weight_type", "ddof", "skipnan", "n_null", "n_nan", "ok",
               "threads"]
PLAIN_OPTS = ["is_valid", "weights", "skipnan", "n_null", "n_nan", "ok", "threads"]
SKEW_OPTS = ["is_valid", "weights", "weight_type", "bias", "skipnan", "n_null", "n_nan", "ok",
             "threads"]
KURT_OPTS = ["is_valid", "weights", "weight_type", "bias", "excess", "skipnan", "n_null", "n_nan",
             "ok", "threads"]
MOMENTS_OPTS = ["is_valid", "weights", "weight_type", "ddof", "bias", "excess", "skipnan",
                "n_null", "n_nan", "threads"]

#: The order family's option lists. Every one is a subsequence of the canonical order that
#: `check_stats_optional_argument_order` enforces, with `kind` sitting in `method`'s block -- both
#: are token arguments naming a rule, and no procedure takes both.
ORDER_OPTS = ["is_valid", "weights", "weight_type", "skipnan", "method", "n_null", "n_nan", "ok",
              "threads"]
TRIM_OPTS = ["is_valid", "weights", "skipnan", "n_null", "n_nan", "ok", "threads"]
PCTS_OPTS = ["is_valid", "weights", "skipnan", "kind", "n_null", "n_nan", "ok", "threads"]
#: `pf_mad` -- the tier-C pair `scale`/`center` in their canonical slots, and no `method`: the
#: deviation median is numpy's linear interpolation because scipy's `median_abs_deviation` reaches
#: `np.median`, and offering a token that would silently disagree with scipy is worse than none.
MAD_OPTS = ["is_valid", "weights", "skipnan", "scale", "center", "n_null", "n_nan", "ok", "threads"]
#: `pf_describe` -- it FILLS an object rather than answering, so `n_null`/`n_nan`/`ok` are absent:
#: the object carries all three and answering them twice would let the two copies disagree.
DESCRIBE_OPTS = ["is_valid", "weights", "weight_type", "skipnan", "threads"]
#: `pf_mode` -- no `skipnan`/`n_nan` on ANY specific, because no kind it accepts can hold a NaN,
#: and no `threads`, because the ordering it does is `pf_argsort`'s own and takes the same
#: automatic rule every other unthreaded caller does.
MODE_OPTS = ["count", "is_valid", "weights", "n_null", "ok"]

CORE_IFACES = [
    iface("sum_f64",
          ["`pf_sum` over a 64-bit real array: the PAIRWISE sum of the population.",
           "",
           "Weighted, this is `sum(w*x)`. An empty population sums to exactly `0` with",
           "`ok = .true.`, which is the additive identity and what numpy and pandas return -- it",
           "is the one quantity in this family that an empty population still defines."],
          ("s", "the sum."), PLAIN_OPTS),
    iface("mean_f64",
          ["`pf_mean` over a 64-bit real array: `sum(w*x) / sum(w)`, REFINED.",
           "",
           "The quotient is taken first and corrected by `sum(w*(x - mu))/sum(w)`, so the result",
           "can sit one ulp from the naive quotient. See `pf_mean`'s own doc-comment for why that",
           "is the right answer and what it means for a test."],
          ("m", "the mean; NaN when the population is empty."), PLAIN_OPTS),
    iface("variance_f64",
          ["`pf_variance` over a 64-bit real array, computed in TWO passes.",
           "",
           "The mean is taken first and the central moment accumulated against it, so the result",
           "is shift-invariant to a few ulp: `pf_variance(x + 1e9)` agrees with `pf_variance(x)`,",
           "which the textbook `sum(x**2) - sum(x)**2/n` does not."],
          ("v", "the variance; NaN when `n_valid <= ddof`."), MOMENT_OPTS),
    iface("stddev_f64",
          ["`pf_stddev` over a 64-bit real array: the square root of `pf_variance`."],
          ("sd", "the standard deviation; NaN when the variance is."), MOMENT_OPTS),
    iface("sem_f64",
          ["`pf_sem` over a 64-bit real array: the standard error of the mean.",
           "",
           "`stddev / sqrt(n_eff)`, where `n_eff` is the population size unweighted and the count",
           "named by `weight_type` otherwise. Matches `scipy.stats.sem` at the default `ddof=1`."],
          ("se", "the standard error; NaN when the standard deviation is."), MOMENT_OPTS),
    iface("skewness_f64",
          ["`pf_skewness` over a 64-bit real array: the third standardised moment.",
           "",
           "**`ddof` is deliberately absent**: no definition of skewness in pandas, numpy or scipy",
           "takes one, and an argument that can never do anything is worse than an absent one.",
           "`weight_type` is present because the bias correction counts through `n_eff`.",
           "NaN when the variance is zero, or when `n_valid < 3` and `bias` is .false."],
          ("g", "the skewness."), SKEW_OPTS),
    iface("kurtosis_f64",
          ["`pf_kurtosis` over a 64-bit real array: the fourth standardised moment.",
           "",
           "`ddof` is absent for the same reason as on `pf_skewness`. NaN when the variance is",
           "zero, or when `n_valid < 4` and `bias` is .false."],
          ("k", "the kurtosis, excess by default."), KURT_OPTS),
]

MOMENTS_IFACE = '''        !> `pf_moments` over a 64-bit real array: every tier-A quantity in ONE pair of passes.
        !>
        !> This is the form to use in a per-group loop. Nothing is computed that was not asked
        !> for beyond the running moments themselves, and the population is traversed twice
        !> however many outputs are requested -- once for the mean, once for the central moments.
        !>
        !> **The outputs come before the common input block**, which is this family's one
        !> exception to the canonical optional order, and their own order is fixed here.
        module subroutine moments_f64(values, n_valid, mean, variance, stddev, sem, skewness, &
                kurtosis, vsum, vmin, vmax, is_valid, weights, weight_type, ddof, bias, excess, &
                skipnan, n_null, n_nan, threads)
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
%s
        end subroutine moments_f64'''


#: `%compute` and `%update` accept the same kinds the one-shot family does, so the object is not
#: the one place in the module that is real64-only. Each specific widens and delegates to the
#: real64 form, exactly as `sum_i32` delegates to `sum_f64`.
OBJ_COMPUTE_OPTS = ["retain", "is_valid", "weights", "weight_type", "skipnan", "threads"]
OBJ_UPDATE_OPTS = ["is_valid", "weights"]


def obj_entry_ifaces():
    """`%compute` and `%update`'s interface bodies, one pair per widened kind."""
    out = []
    for tag, decl, _, has_nan, kindword in MOMENT_KINDS:
        mine = kind_opts(OBJ_COMPUTE_OPTS, has_nan)
        lines = ["        !> `%%compute` over %s." % kindword]
        for line in KIND_NOTE.get(tag, []):
            lines.append(("        !> " + line).rstrip())
        lines.append("        module subroutine obj_compute_%s(self, %s)"
                     % (tag, ", ".join(["values"] + mine)))
        lines.append("            class(pf_stats), intent(inout) :: self")
        lines.append("            !! the accumulator; any previous contents are discarded.")
        lines.append("            " + decl.strip())
        for key in mine:
            lines.append(D[key])
        lines.append("        end subroutine obj_compute_%s" % tag)
        out.append("\n".join(lines))

        lines = ["        !> `%%update` over %s." % kindword]
        lines.append("        module subroutine obj_update_%s(self, %s)"
                     % (tag, ", ".join(["values"] + OBJ_UPDATE_OPTS)))
        lines.append("            class(pf_stats), intent(inout) :: self !! the accumulator.")
        lines.append("            " + decl.strip().replace("the population.", "the batch.")
                     .replace("the column; scalar numeric kinds only.",
                              "the batch; scalar numeric kinds only."))
        for key in OBJ_UPDATE_OPTS:
            lines.append(D[key])
        lines.append("        end subroutine obj_update_%s" % tag)
        out.append("\n".join(lines))
    return "\n".join(out)


def obj_entry_bodies():
    """`%compute` and `%update`'s bodies, one pair per widened kind."""
    out = []
    for tag, _, widen, has_nan, _ in MOMENT_KINDS:
        for what, opts, target in (("compute", kind_opts(OBJ_COMPUTE_OPTS, has_nan), "obj_compute_f64"),
                                   ("update", OBJ_UPDATE_OPTS, "obj_update_f64")):
            lines = ["    module procedure obj_%s_%s" % (what, tag)]
            lines.append("        real(real64), allocatable :: wide(:)")
            if tag == "col":
                lines.append("        logical, allocatable :: mask(:)")
                lines.append('        call col_to_real64(values, "pf_stats%%%s", is_valid, wide, mask)'
                             % what)
            else:
                lines.append("        allocate(wide(size(values, kind=int64)))")
                lines.append("        wide = %s" % widen)
            call = ["self", "wide"] + ["%s=%s" % (o, "mask" if (o == "is_valid" and tag == "col") else o)
                                       for o in opts]
            lines.append("        call " + wrap_call(target, call))
            lines.append("    end procedure obj_%s_%s" % (what, tag))
            out.append("\n".join(lines))
    return "\n\n".join(out)


#: `pf_count_valid` over a column. Written out rather than folded into `spec_iface`/`body`, which
#: are shaped for a plain array: this one widens and delegates, exactly as the moment family's own
#: `*_col` entry points do, so the three refusals are stated in one place for the whole module.
COUNT_COL_IFACE = """        !> `pf_count_valid` over a scalar numeric `parquet_column`.
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
        end subroutine count_valid_col"""

COUNT_COL_BODY = """    module procedure count_valid_col
        real(real64), allocatable :: wide(:)
        logical, allocatable :: mask(:)
        call col_to_real64(values, "pf_count_valid", is_valid, wide, mask)
        call count_valid_f64(wide, n, mask, weights, skipnan)
    end procedure count_valid_col"""


#: The one place a `parquet_column` becomes a population. Every `*_col` entry point goes through
#: it, so the three refusals and the nullness rule are stated once rather than eight times.
COL_WIDEN = '''    !> Widens a scalar numeric `parquet_column` into a `real64` population and its validity mask.
    !!
    !! Three refusals, and each is a wrong answer rather than an inconvenience if it is dropped:
    !!
    !! * a **string, temporal or container** column has no numeric statistics, and the message names
    !!   the kind it actually found;
    !! * a column **wider than one element** aborts, because flattening a width-16 column into one
    !!   population is a DIFFERENT statistic -- a caller who wants one element position across all
    !!   rows has `%get_elem`, and one who genuinely wants the flattened population can pass the
    !!   flattened array and thereby say so;
    !! * `is_valid=` **alongside** a column aborts, because the column carries its own validity and
    !!   two sources of truth that can disagree is a shape this repository has been bitten by.
    !!
    !! `mask` comes back UNALLOCATED for a null-free column, which is not a detail: an unallocated
    !! allocatable passed to an `optional` dummy is ABSENT, so the core takes its own no-mask fast
    !! path with no branch at the call site.
    !!
    !! The bulk read goes through `parquet_column_data_ptr` -- the typed accessor tier, never a
    !! type-bound procedure -- so ifx does not build a class descriptor per call. The scalar
    !! metadata queries below have no typed twin and are called once per column, which is the same
    !! trade `parquet_sorting` already makes for its key extraction.
    subroutine col_to_real64(col, what, is_valid, v, mask)
        type(parquet_column), intent(in), target :: col
        !! the column. `target` because `parquet_column_data_ptr` requires it; the pointer never
        !! leaves this procedure, so the caller's actual argument needs no `target` of its own.
        character(len=*), intent(in) :: what                  !! the public procedure's name.
        logical, intent(in), optional :: is_valid(:)          !! must be absent; see above.
        real(real64), allocatable, intent(out) :: v(:)        !! the widened population.
        logical, allocatable, intent(out) :: mask(:)          !! its validity, or unallocated.
        integer(int32), pointer :: p32(:)
        integer(int64), pointer :: p64(:)
        real(real32), pointer :: r32(:)
        real(real64), pointer :: r64(:)
        logical, pointer :: pb(:)
        character(len=:), allocatable :: kname
        integer :: k
        integer(int64) :: n

        if (present(is_valid)) error stop what // ": is_valid= cannot be given alongside a " // &
            "parquet_column; the column carries its own validity, and two sources that can " // &
            "disagree is exactly what this refuses"
        k = col%kindof()
        select case (k)
        case (PK_INT32, PK_INT64, PK_FLOAT32, PK_FLOAT64, PK_LOGICAL)
            continue
        case (PK_INT32_VEC, PK_INT64_VEC, PK_FLOAT32_VEC, PK_FLOAT64_VEC, PK_LOGICAL_VEC)
            ! The VECTOR kinds get their own arm, and the message names the width rather than the
            ! kind, because "a column of kind PK_INT32_VEC has no numeric statistics" would be
            ! misleading -- the ELEMENTS are perfectly numeric and it is the shape that is refused.
            ! Width cannot be tested separately: `%init` already guarantees a scalar kind is width
            ! 1 and a vector kind is wider, so a `colwidth() /= 1` check would be dead code.
            error stop what // ": this column is " // trim(stats_i2s(int(col%colwidth(), int64))) // &
                " elements wide; flattening a vector column into one population is a different " // &
                "statistic, so use %get_elem for one element position or pass the flattened " // &
                "array yourself if that is what you mean"
        case default
            call parquet_kind_name(k, kname)
            error stop what // ": a column of kind " // kname // " has no numeric statistics; " // &
                "this family accepts int32, int64, float32, float64 and logical columns"
        end select

        ! The `(1:n)` bounds below are BELT AND BRACES, not what makes this correct, and saying so
        ! is worth a line: `parquet_column_data_ptr` already returns `col%i32(1:col%nrows)`, so the
        ! pointer never spans the slack that geometric growth leaves beyond the row count. Verified
        ! by mutation -- reading the whole pointer instead changes no answer. The bound stays
        ! because it states the intent and would survive a `data_ptr` that stopped slicing.
        n = col%length()
        allocate(v(n))
        select case (k)
        case (PK_INT32)
            call parquet_column_data_ptr(col, p32)
            v = real(p32(1:n), real64)
        case (PK_INT64)
            call parquet_column_data_ptr(col, p64)
            v = real(p64(1:n), real64)
        case (PK_FLOAT32)
            call parquet_column_data_ptr(col, r32)
            v = real(r32(1:n), real64)
        case (PK_FLOAT64)
            call parquet_column_data_ptr(col, r64)
            v = r64(1:n)
        case default
            call parquet_column_data_ptr(col, pb)
            v = merge(1.0_real64, 0.0_real64, pb(1:n))
        end select
        call col%row_validity(mask)
    end subroutine col_to_real64'''


#: The output block `pf_moments` declares before its input block, shared by every kind's entry
#: point so the ten names and their order cannot drift between them.
MOMENTS_OUT_DECLS = """            integer(int64), intent(out), optional :: n_valid !! how many elements were used.
            real(real64), intent(out), optional :: mean !! the mean; NaN when empty.
            real(real64), intent(out), optional :: variance !! the variance; NaN when undefined.
            real(real64), intent(out), optional :: stddev !! the standard deviation.
            real(real64), intent(out), optional :: sem !! the standard error of the mean.
            real(real64), intent(out), optional :: skewness !! the third standardised moment.
            real(real64), intent(out), optional :: kurtosis !! the fourth, excess by default.
            real(real64), intent(out), optional :: vsum !! the pairwise sum; 0 when empty.
            real(real64), intent(out), optional :: vmin !! the smallest value; NaN when empty.
            real(real64), intent(out), optional :: vmax !! the largest value; NaN when empty."""

MOMENTS_OUT_NAMES = ["n_valid", "mean", "variance", "stddev", "sem", "skewness", "kurtosis",
                     "vsum", "vmin", "vmax"]


#: The kinds the moment family accepts as `values` besides `real(real64)`, which the core already
#: serves directly. Each entry point widens into a `real64` buffer and calls that core, so there
#: is exactly one implementation of every statistic and a new kind cannot answer differently.
#:
#: `has_nan` drives F5: a kind that cannot hold a NaN gets no `skipnan` and no `n_nan`, because an
#: argument that can never do anything is worse than an absent one. The subsequence rule the lint
#: check enforces is unaffected -- omission is what a subsequence permits.
#:
#: (tag, dummy declaration, how to widen `values` into `v`, has_nan, what the doc calls it)
MOMENT_KINDS = [
    ("i32", "integer(int32), intent(in) :: values(:) !! the population.",
     "real(values, real64)", False, "a 32-bit integer array"),
    ("i64", "integer(int64), intent(in) :: values(:) !! the population.",
     "real(values, real64)", False, "a 64-bit integer array"),
    ("f32", "real(real32), intent(in) :: values(:) !! the population.",
     "real(values, real64)", True, "a 32-bit real array"),
    ("bool", "logical, intent(in) :: values(:) !! the population; .true. is 1 and .false. is 0.",
     "merge(1.0_real64, 0.0_real64, values)", False, "a logical array"),
    ("col", "type(parquet_column), intent(in) :: values !! the column; scalar numeric kinds only.",
     None, True, "a scalar numeric `parquet_column`"),
]

#: The extra doc paragraph a kind needs beyond "<statistic> over a <kind> array".
KIND_NOTE = {
    "i64": ["",
            "**An `integer(int64)` above 2**53 loses exactness** on the way into the real64 buffer",
            "every statistic here is computed in. That is unavoidable for a mean and irrelevant for",
            "the workloads this serves -- nobody averages an object ID -- but it is NOT irrelevant",
            "for a median, which `pf_nth_quantile` computes exactly on the original array."],
    "bool": ["",
             "`.true.` widens to 1 and `.false.` to 0, so the mean of a logical array is the",
             "fraction that are true."],
    "col": ["",
            "Dispatched on the column's kind at run time, and **scalar numeric columns only**:",
            "int32, int64, float32, float64 and logical. A string, temporal or container column",
            "aborts naming its kind, and so does a column more than one element wide -- flattening",
            "a vector column into one population is a different statistic and nobody should get it",
            "by accident. **The column's own validity is the only source of nullness**, so passing",
            "`is_valid=` alongside one aborts rather than silently preferring one of two sources."],
}

#: The seven one-output procedures, one row each: (base, result dummy, its doc, its option list).
#: The real64 core's own interfaces above are built from the same option constants, so the two
#: cannot drift on the half that is a permanent compatibility contract.
ENTRY_FAMILY = [
    ("sum", "s", "the sum.", PLAIN_OPTS),
    ("mean", "m", "the mean; NaN when the population is empty.", PLAIN_OPTS),
    ("variance", "v", "the variance; NaN when `n_valid <= ddof`.", MOMENT_OPTS),
    ("stddev", "sd", "the standard deviation; NaN when the variance is.", MOMENT_OPTS),
    ("sem", "se", "the standard error; NaN when the standard deviation is.", MOMENT_OPTS),
    ("skewness", "g", "the skewness.", SKEW_OPTS),
    ("kurtosis", "k", "the kurtosis, excess by default.", KURT_OPTS),
]


#: The order family: (base, extra required inputs, out dummy, out declaration, out doc, options).
#: The "extra inputs" slot is what distinguishes these from the moment family -- each sits between
#: `values` and the output, in the position a reader of `pf_quantile(v, p, q)` expects.
#: The tail every ORDER generic's doc-comment shares, stated once for the same reason `_COMMON` is.
_ORDER_COMMON = """    !>
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
    !> is what asserts."""

#: The one interface the order tier genuinely needs from its sibling.
#:
#: `parquet_stats_order` must apply the same exclusion rules and get the surviving values back, and
#: that logic is pass one of `stats_engine`, contained in `parquet_stats_core`. Two SIBLING
#: submodules cannot reach each other's contained procedures, so this is declared here and
#: implemented there -- the "cross-subtree private-helper interfaces" mechanism `parquet_core.f90`
#: uses for exactly this shape. Re-deriving the exclusion order in a second place is the
#: alternative, and it is the one that eventually disagrees with itself.
#:
#: The order tier's OWN helpers -- the method-token resolver and the interpolator -- are ordinary
#: contained procedures of `parquet_stats_order`, because only that submodule calls them. That also
#: keeps them clear of nagfor's rule that a separate module procedure must be implemented before it
#: is called within one submodule.
ORDER_HELPER_IFACES = """        !> Applies the exclusion rules and hands back the surviving values, without the moments.
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
        !> Completes a deferred tier-A recomputation, if one is pending.
        module subroutine stats_ensure(self)
            class(pf_stats), intent(inout) :: self !! the accumulator.
        end subroutine stats_ensure"""

ORDER_FAMILY = [
    ("median", [], "med", "real(real64), intent(out) :: med",
     "the median; NaN when the population is empty.", ORDER_OPTS),
    ("quantile",
     [("p", "real(real64), intent(in) :: p",
       "the probability, on a 0-1 scale rather than 0-100. Outside [0, 1] aborts.")],
     "q", "real(real64), intent(out) :: q",
     "the quantile; NaN when the population is empty.", ORDER_OPTS),
    ("quantiles",
     [("probs", "real(real64), intent(in) :: probs(:)",
       "the probabilities, each on a 0-1 scale. Any outside [0, 1] aborts. Need not be sorted.")],
     "out", "real(real64), intent(out) :: out(:)",
     "one quantile per entry of `probs`, same size. ONE sort serves all of them.",
     ORDER_OPTS),
    ("iqr", [], "r", "real(real64), intent(out) :: r",
     "the interquartile range, `q(0.75) - q(0.25)`; NaN when either quartile is.", ORDER_OPTS),
    ("trim_mean",
     [("prop", "real(real64), intent(in) :: prop",
       "the share trimmed from EACH tail, so `prop=0.1` drops 10% at each end and averages the "
       "middle 80%. Must satisfy `0 <= prop < 0.5`; anything else aborts, because a caller who "
       "asks to trim everything has made a mistake rather than expressed a preference.")],
     "m", "real(real64), intent(out) :: m",
     "the trimmed mean; NaN when nothing survives the trim.", TRIM_OPTS),
    ("percentile_of_score",
     [("score", "real(real64), intent(in) :: score",
       "the value to locate. NaN or infinite aborts -- unlike a NaN in the population, which is "
       "an ordinary data condition, a NaN score can only come from the caller's own arithmetic.")],
     "p", "real(real64), intent(out) :: p",
     "the share of the population at or below `score`, on a 0-1 scale; NaN when empty.",
     PCTS_OPTS),
    ("mad", [], "m", "real(real64), intent(out) :: m",
     "the median absolute deviation, scaled unless `scale=\"raw\"`; NaN when the population is "
     "empty.", MAD_OPTS),
    ("describe", [], "s", "type(pf_stats), intent(out) :: s",
     "the filled summary object: tier A computed and tier B already ordered, so every query on "
     "it afterwards is a read.", DESCRIBE_OPTS),
]


#: `pf_describe`'s own doc tail. Short, because the object it fills documents itself.
_DESCRIBE_COMMON = """    !>
    !> Nulls, NaNs and zero-weight elements leave the population exactly as they do for the
    !> moments, and the object reports how many through `%n_null()` and `%n_nan()`. An empty
    !> population is not an error: every statistic on the result is then a quiet NaN and
    !> `%n_valid()` is 0.
    !>
    !> `values` may be a `real(real64)`, `real(real32)`, `integer(int32)`, `integer(int64)` or
    !> `logical` array, or a scalar numeric `type(parquet_column)`."""

#: `pf_mode`'s kinds: (tag, `values` declaration, result declaration, its doc, the kind in prose).
#:
#: **The real kinds are absent on purpose and the omission is the specification.** The mode of a
#: continuous variable is not a statistic -- equality on floats is a trap rather than an answer --
#: so a real array does not merely go unsupported, it fails to compile against this generic, which
#: is the earliest and cheapest place to say so. `pf_bucketize`/`pf_histogram` (P9) are the answer
#: to the question a caller reaching for it is actually asking.
#:
#: `type(parquet_column)` is absent too, and for a different reason -- see feature_pandas_S4.md's
#: P7 open questions. A column's kind is not known until run time, so a `parquet_column` specific
#: could only be disambiguated by the type of the RESULT argument, which turns a kind mismatch
#: from a compile error into a runtime abort. Every other family in this module resolves on
#: `values` alone, and one that did not would be the odd one out for the sake of one call shape.
MODE_KINDS = [
    ("i32", "integer(int32), intent(in) :: values(:) !! the population.",
     "integer(int32), intent(out) :: m", "the modal value; unchanged when the population is empty.",
     "a 32-bit integer array"),
    ("i64", "integer(int64), intent(in) :: values(:) !! the population.",
     "integer(int64), intent(out) :: m", "the modal value; unchanged when the population is empty.",
     "a 64-bit integer array"),
    ("bool", "logical, intent(in) :: values(:) !! the population.",
     "logical, intent(out) :: m",
     "the modal value; unchanged when the population is empty. `.false.` sorts below `.true.`, "
     "so an even split answers `.false.`.",
     "a logical array"),
    ("chr", "character(len=*), intent(in) :: values(:) !! the population; each element is trimmed.",
     "character(len=:), allocatable, intent(out) :: m",
     "the modal value, allocated to its own trimmed length. **Left unallocated when the "
     "population is empty**, which is the one shape in this module where `ok = .false.` and an "
     "unallocated result coincide -- test `ok`, or `allocated(m)`, before reading it.",
     "a character array"),
    ("str", "type(parquet_string_column), intent(in) :: values !! the population.",
     "character(len=:), allocatable, intent(out) :: m",
     "the modal value, allocated to its own length. Left unallocated when the population is "
     "empty.",
     "a `parquet_string_column`"),
]

#: `pf_mode`'s generic doc-comment. Not shared with `_ORDER_COMMON`: this generic accepts a
#: different set of kinds, cannot meet a NaN, and returns something other than a `real(real64)`.
MODE_DOC = [
    "The most common value of a population -- pandas' `mode()`, scipy's `stats.mode`.",
    "",
    "**Integer, logical and string kinds only.** The real kinds are not overloads that abort at",
    "run time; they are absent from the generic, so `pf_mode` over a real array does not",
    "compile. The mode of a continuous variable is not a statistic, and floating-point equality",
    "is a trap rather than an answer -- `pf_bucketize` and `pf_histogram` are what that question",
    "actually wants.",
    "",
    "**Ties go to the SMALLEST value**, matching `scipy.stats.mode`, and never to the first",
    "occurrence: an answer that depended on input order would differ between an array and its",
    "own permutation, which is a reproducibility defect rather than a preference.",
    "",
    "`count` reports how many elements hold the modal value. Nulls are excluded and counted",
    "through `n_null`, as everywhere in this module; no kind here can hold a NaN, so there is",
    "deliberately no `skipnan` or `n_nan`.",
    "",
    "**Weights are supported**: the mode is then the value carrying the greatest total weight,",
    "ties still broken by the smallest value, and `count` still counts elements rather than",
    "weight. A zero weight removes the element, as everywhere else.",
    "",
    "An empty population gives `ok = .false.` and `count = 0`, and `m` must not be read.",
]


#: `ok`'s doc for `pf_mode`, whose result is not a `real(real64)` and so cannot be a quiet NaN.
#: An override rather than a second entry in the canonical sequence: the DUMMY is still `ok` in
#: the same slot, which is what `check_stats_optional_argument_order` reads.
MODE_D = dict(D)
MODE_D["ok"] = """            logical, intent(out), optional :: ok
            !! .false. when the population is empty, and there is therefore no modal value. `m`
            !! is then untouched -- unallocated, for the two character forms -- and must not be
            !! read; `count` is 0."""


def mode_iface(tag, decl, res_decl, res_doc, kindword):
    """One interface body for a `pf_mode` specific."""
    args = wrap_args(["values", "m"] + MODE_OPTS)
    lines = ["        !> `pf_mode` over %s." % kindword]
    lines.append("        module subroutine mode_%s(%s)" % (tag, args))
    lines.append("            " + decl.strip())
    lines.append("            %s" % res_decl)
    for line in _wrap_doc(res_doc):
        lines.append("            !! " + line)
    for key in MODE_OPTS:
        lines.append(MODE_D[key])
    lines.append("        end subroutine mode_%s" % tag)
    return "\n".join(lines)


def order_extra_decls(extra):
    """The declaration lines for an order procedure's extra required inputs."""
    out = []
    for name, decl, doc in extra:
        out.append("            %s" % decl)
        for line in _wrap_doc(doc):
            out.append("            !! " + line)
    return out


def _wrap_doc(text, width=92):
    """Wraps a doc-comment body so no emitted line passes the 132-column limit."""
    words, line, out = text.split(), "", []
    for w in words:
        if line and len(line) + 1 + len(w) > width:
            out.append(line)
            line = w
        else:
            line = (line + " " + w).strip()
    if line:
        out.append(line)
    return out


def kind_opts(opts, has_nan):
    """This kind's option list: the family's, minus what a NaN-free kind cannot use."""
    if has_nan:
        return list(opts)
    return [o for o in opts if o not in ("skipnan", "n_nan")]


def entry_iface(base, out_name, out_doc, opts, tag, decl, has_nan, kindword):
    """One interface body for a per-kind entry point."""
    mine = kind_opts(opts, has_nan)
    lines = ["        !> `pf_%s` over %s." % (base, kindword)]
    for line in KIND_NOTE.get(tag, []):
        lines.append(("        !> " + line).rstrip())
    lines.append("        module subroutine %s_%s(values, %s)"
                 % (base, tag, ", ".join([out_name] + mine)))
    lines.append("            " + decl.strip())
    lines.append("            real(real64), intent(out) :: %s !! %s" % (out_name, out_doc))
    for key in mine:
        lines.append(D[key])
    lines.append("        end subroutine %s_%s" % (base, tag))
    return "\n".join(lines)


def entry_moments_iface(tag, decl, has_nan, kindword):
    """`pf_moments`' interface body for one kind."""
    mine = kind_opts(MOMENTS_OPTS, has_nan)
    args = ["values"] + MOMENTS_OUT_NAMES + mine
    lines = ["        !> `pf_moments` over %s: every tier-A quantity in one pair of passes."
             % kindword]
    for line in KIND_NOTE.get(tag, []):
        lines.append(("        !> " + line).rstrip())
    lines.append("        module subroutine moments_%s(%s)" % (tag, wrap_args(args)))
    lines.append("            " + decl.strip())
    lines.append(MOMENTS_OUT_DECLS)
    for key in mine:
        lines.append(D[key])
    lines.append("        end subroutine moments_%s" % tag)
    return "\n".join(lines)


def wrap_args(args):
    """Renders an argument list with Fortran continuations, inside the 132-column limit."""
    out, line = [], ""
    for i, a in enumerate(args):
        piece = a + (", " if i < len(args) - 1 else "")
        if len(line) + len(piece) > 78:
            out.append(line + "&")
            line = "                "
        line += piece
    out.append(line)
    return "\n".join(out)


def entry_body(base, out_name, opts, tag, widen, has_nan):
    """One `module procedure` body for a per-kind entry point."""
    mine = kind_opts(opts, has_nan)
    lines = ["    module procedure %s_%s" % (base, tag)]
    lines.append("        real(real64), allocatable :: wide(:)")
    if tag == "col":
        lines.append("        logical, allocatable :: mask(:)")
        lines.append('        call col_to_real64(values, "pf_%s", is_valid, wide, mask)' % base)
    else:
        lines.append("        allocate(wide(size(values, kind=int64)))")
        lines.append("        wide = %s" % widen)
    call = ["wide", out_name] + ["%s=%s" % (o, "mask" if (o == "is_valid" and tag == "col") else o)
                              for o in mine]
    lines.append("        call " + wrap_call("%s_f64" % base, call))
    lines.append("    end procedure %s_%s" % (base, tag))
    return "\n".join(lines)


def entry_moments_body(tag, widen, has_nan):
    """`pf_moments`' body for one kind."""
    mine = kind_opts(MOMENTS_OPTS, has_nan)
    lines = ["    module procedure moments_%s" % tag]
    lines.append("        real(real64), allocatable :: wide(:)")
    if tag == "col":
        lines.append("        logical, allocatable :: mask(:)")
        lines.append('        call col_to_real64(values, "pf_moments", is_valid, wide, mask)')
    else:
        lines.append("        allocate(wide(size(values, kind=int64)))")
        lines.append("        wide = %s" % widen)
    call = ["wide"] + ["%s=%s" % (o, o) for o in MOMENTS_OUT_NAMES] \
        + ["%s=%s" % (o, "mask" if (o == "is_valid" and tag == "col") else o) for o in mine]
    lines.append("        call " + wrap_call("moments_f64", call))
    lines.append("    end procedure moments_%s" % tag)
    return "\n".join(lines)


def order_core_iface(base, extra, out_name, out_decl, out_doc, opts, doc):
    """One interface body for an order procedure's real64 core."""
    args = ["values"] + [e[0] for e in extra] + [out_name] + opts
    lines = [("        !> " + line).rstrip() for line in doc]
    lines.append("        module subroutine %s_f64(%s)" % (base, wrap_args(args)))
    lines.append("            real(real64), intent(in) :: values(:) !! the population.")
    lines.extend(order_extra_decls(extra))
    lines.append("            " + out_decl)
    for line in _wrap_doc(out_doc):
        lines.append("            !! " + line)
    for key in opts:
        lines.append(D[key])
    lines.append("        end subroutine %s_f64" % base)
    return "\n".join(lines)


def order_entry_iface(base, extra, out_name, out_decl, out_doc, opts, tag, decl, has_nan, kindword):
    """One interface body for an order procedure's per-kind entry point."""
    mine = kind_opts(opts, has_nan)
    args = ["values"] + [e[0] for e in extra] + [out_name] + mine
    lines = ["        !> `pf_%s` over %s." % (base, kindword)]
    for line in KIND_NOTE.get(tag, []):
        lines.append(("        !> " + line).rstrip())
    lines.append("        module subroutine %s_%s(%s)" % (base, tag, wrap_args(args)))
    lines.append("            " + decl.strip())
    lines.extend(order_extra_decls(extra))
    lines.append("            " + out_decl)
    for line in _wrap_doc(out_doc):
        lines.append("            !! " + line)
    for key in mine:
        lines.append(D[key])
    lines.append("        end subroutine %s_%s" % (base, tag))
    return "\n".join(lines)


def order_entry_body(base, extra, out_name, opts, tag, widen, has_nan):
    """One `module procedure` body for an order procedure's per-kind entry point."""
    mine = kind_opts(opts, has_nan)
    lines = ["    module procedure %s_%s" % (base, tag)]
    lines.append("        real(real64), allocatable :: wide(:)")
    if tag == "col":
        lines.append("        logical, allocatable :: mask(:)")
        lines.append('        call col_to_real64(values, "pf_%s", is_valid, wide, mask)' % base)
    else:
        lines.append("        allocate(wide(size(values, kind=int64)))")
        lines.append("        wide = %s" % widen)
    call = ["wide"] + [e[0] for e in extra] + [out_name] \
        + ["%s=%s" % (o, "mask" if (o == "is_valid" and tag == "col") else o) for o in mine]
    lines.append("        call " + wrap_call("%s_f64" % base, call))
    lines.append("    end procedure %s_%s" % (base, tag))
    return "\n".join(lines)


def wrap_call(name, args):
    """Renders a call's argument list with continuations, inside the 132-column limit."""
    out, line = [], name + "("
    for i, a in enumerate(args):
        piece = a + (", " if i < len(args) - 1 else ")")
        if len(line) + len(piece) > 96:
            out.append(line + "&")
            line = "            "
        line += piece
    out.append(line)
    return "\n".join(out)


GENERIC_DOC = {
    "pf_sum": ["The sum of a population, computed PAIRWISE rather than left to right.",
               "",
               "Pairwise summation loses O(log n) relative accuracy against a naive sum's O(n),",
               "for the same number of additions and no extra memory -- and it is the same fixed",
               "block tree that makes every answer here independent of the thread count."],
    "pf_mean": ["The arithmetic mean of a population, weighted when `weights` is present.",
                "",
                "**Not literally `sum(w*x)/sum(w)`, and deliberately so.** That quotient is taken",
                "first and then REFINED by `sum(w*(x - mu))/sum(w)`, which is algebraically zero",
                "and in floating point is the rounding error left in it -- the standard two-pass",
                "refinement, and the more accurate answer. It is also the centre the variance and",
                "the higher moments are taken about, so it has to be this one. The visible",
                "consequence is that the result can differ from the naive quotient in the last",
                "bit: `pf_mean` over ten logicals of which three are `.true.` is one ulp below",
                "`0.3_real64`. Compare with a tolerance, or against another reduction of the same",
                "population, rather than against a decimal literal."],
    "pf_variance": ["The variance of a population, in two passes so that it is shift-invariant.",
                    "",
                    "`ddof = 1` by default (the sample variance, as pandas returns); pass",
                    "`ddof = 0` for numpy's default."],
    "pf_stddev": ["The standard deviation: the square root of `pf_variance`, same arguments."],
    "pf_sem": ["The standard error of the mean."],
    "pf_skewness": ["The skewness, bias-corrected by default (pandas' G1, not scipy's g1)."],
    "pf_kurtosis": ["The kurtosis, excess and bias-corrected by default (pandas' G2)."],
    "pf_moments": ["Every tier-A statistic of a population in one pair of passes.",
                   "",
                   "The form a per-group loop should use: asking for eight statistics costs the",
                   "same two traversals as asking for one."],
}

#: What each order generic's own doc-comment says. FORD renders this on the generic's page, which
#: is the only place a multi-specific generic's arguments are described at all (see CLAUDE.md's
#: FORD notes), so each spells out its arguments in prose.
ORDER_DOC = {
    "pf_median": [
        "The median of a population: the INTERPOLATING one, not an element of the input.",
        "",
        "For an even-length population this is the mean of the two middle values, which is what",
        "numpy, pandas and every spreadsheet mean by `median` -- and is deliberately NOT what",
        "`pf_nth_quantile(v, 0.5)` in `parquet_sorting` returns, which is one of the two. That",
        "one answers `which element`, this one answers `what value`; both are useful and they",
        "are different questions.",
        "",
        "`method=` selects the rule (see `pf_quantile`); `pf_median` is exactly",
        "`pf_quantile(values, 0.5, med)`."],
    "pf_quantile": [
        "One quantile of a population, interpolated between the bracketing order statistics.",
        "",
        "`p` is on a **0-1 scale**, not 0-100, and outside `[0, 1]` aborts. `method=` chooses how",
        "a fractional position is resolved: `linear` (the default, Hyndman-Fan type 7 -- numpy's",
        "and pandas' default), `lower`, `higher`, `nearest`, `midpoint`, or `inverted_cdf`.",
        "",
        "**Weighted, the position rule reduces EXACTLY to type 7 when the weights are equal**, so",
        "adding uniform weights to a working script does not move its numbers. No reference",
        "library implements an interpolating weighted quantile, so that invariant -- rather than",
        "a cross-check -- is what pins it; `method=\"inverted_cdf\"` is the numpy-comparable",
        "escape hatch, and is what `weight_type=\"frequency\"` selects by default.",
        "",
        "One call with one probability uses SELECTION rather than a sort where it can, so it is",
        "O(n). Asking for several probabilities should use `pf_quantiles`, which sorts once."],
    "pf_quantiles": [
        "Several quantiles of one population, from ONE ordering of it.",
        "",
        "`probs(:)` on a 0-1 scale and `out(:)` the same size; `probs` need not be sorted and may",
        "repeat. This is the form to reach for whenever more than one quantile is wanted --",
        "`[0.16, 0.5, 0.84]` is one call and one sort, where three `pf_quantile` calls would",
        "traverse the population three times.",
        "",
        "Every argument means what it does on `pf_quantile`, including `method=` and the weighted",
        "position rule."],
    "pf_iqr": [
        "The interquartile range: `pf_quantile(v, 0.75) - pf_quantile(v, 0.25)`, from one sort.",
        "",
        "NaN when either quartile is undefined, which for this family means an empty population.",
        "`method=` is passed through to both quartiles, so the difference is always taken between",
        "two quantiles computed the same way."],
    "pf_trim_mean": [
        "The mean of the population with a share trimmed from EACH tail -- scipy's `trim_mean`.",
        "",
        "`prop` is the share removed at each end, so `prop=0.1` averages the middle 80%. It must",
        "satisfy `0 <= prop < 0.5`; anything else aborts, since trimming everything is a mistake",
        "rather than a request. `prop=0` is the ordinary mean and is allowed.",
        "",
        "The count trimmed from each end is `floor(prop * n_valid)`, matching scipy, so the",
        "result changes in steps as `prop` grows rather than continuously.",
        "",
        "**Weights are supported and are trimmed with their values**: the same elements leave the",
        "population, and the mean of what remains is weighted."],
    "pf_percentile_of_score": [
        "The share of the population at or below a given value -- scipy's `percentileofscore`,",
        "on a **0-1 scale** rather than 0-100, matching every other probability in this module.",
        "",
        "The inverse of `pf_quantile` in the loose sense: it answers `where does this value sit`",
        "rather than `what value sits here`. `kind=` chooses how a value exactly EQUAL to `score`",
        "is counted -- `rank` (the default; ties count half), `weak` (`<=`), `strict` (`<`), or",
        "`mean` (the average of weak and strict).",
        "",
        "A NaN or infinite `score` aborts: unlike a NaN in the population, which is an ordinary",
        "data condition this module excludes, a NaN score can only be the caller's own mistake."],
    "pf_mad": [
        "The median absolute deviation -- scipy's `median_abs_deviation`, scaled by default.",
        "",
        "`median(|x - center|)`, where `center` is the population's own median unless one is",
        "supplied. `scale=\"normal\"` (the default) divides by `Phi^-1(3/4)`, which makes the result",
        "a consistent estimator of the standard deviation for Gaussian data -- so on a large",
        "Gaussian sample `pf_mad` and `pf_stddev` agree to within sampling error, and on a sample",
        "with a few wild points they do not, which is the whole reason to reach for it.",
        "`scale=\"raw\"` is the unscaled median.",
        "",
        "**Supplying `center` skips one selection, not the ordering.** The median of the",
        "deviations still has to be found, so this is a small saving rather than a different",
        "algorithm. A NaN or infinite `center` aborts.",
        "",
        "Both medians are taken with `linear` interpolation, because scipy reaches `np.median`;",
        "there is deliberately no `method=` here, since a token that made this disagree with",
        "scipy would be worse than no token at all."],
    "pf_describe": [
        "Summarises a population into a `pf_stats`, ordered and ready -- pandas' `describe()`.",
        "",
        "Exactly `s%compute(values, ...)` followed by `s%prepare_order()`, which is the pairing",
        "worth a name: it is ONE pair of traversals and ONE ordering, after which the count, the",
        "mean, the standard deviation, the extremes and every quantile are reads. `s%print()`",
        "renders the usual eight-row block from it.",
        "",
        "Reach for `%compute` instead when the order statistics are not wanted -- this procedure",
        "pays for the ordering whether or not anything asks for it, which is the point."],
}

#: The order family's members, derived the same way the moment family's are.
_ORDER_BASES = [b for b, _, _, _, _, _ in ORDER_FAMILY]
GENERIC_SPECIFICS_ORDER = {
    "pf_" + base: ["%s_f64" % base] + ["%s_%s" % (base, k[0]) for k in MOMENT_KINDS]
    for base in _ORDER_BASES
}

#: Every generic's members: the real64 core first, then one entry point per widened kind. Derived
#: rather than listed, so adding a kind to MOMENT_KINDS adds it to all eight generics at once.
_MOMENT_BASES = [b for b, _, _, _ in ENTRY_FAMILY] + ["moments"]
GENERIC_SPECIFICS = {
    "pf_" + base: ["%s_f64" % base] + ["%s_%s" % (base, k[0]) for k in MOMENT_KINDS]
    for base in _MOMENT_BASES
}


def spec_iface(tag, decl, what, is_real):
    """One interface body for count_valid_<tag>."""
    out = []
    out.append("        !> `pf_count_valid` over a %s array." % what)
    out.append("        module subroutine count_valid_%s(values, n, is_valid, weights%s)"
               % (tag, ", skipnan" if is_real else ""))
    out.append("            %s, intent(in) :: values(:) !! the population to count." % decl)
    out.append("            integer(int64), intent(out) :: n !! how many elements are in the population.")
    out.append("            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.")
    out.append("            real(real64), intent(in), optional :: weights(:)")
    out.append("            !! per element weight. A zero weight REMOVES the element from the population, so a")
    out.append("            !! weighted count and an unweighted one over the same array legitimately differ.")
    out.append("            !! A negative, NaN or infinite weight aborts.")
    if is_real:
        out.append("            logical, intent(in), optional :: skipnan")
        out.append("            !! .true. (the default) excludes a NaN from the population, as a null is excluded and")
        out.append("            !! as `pf_minmax` has always done; .false. counts it as an ordinary value.")
    out.append("        end subroutine count_valid_%s" % tag)
    return "\n".join(out)


def body(tag, decl, what, is_real):
    """One implementation of count_valid_<tag>."""
    out = []
    out.append("    module procedure count_valid_%s" % tag)
    out.append("        integer(int64) :: i, nv")
    if is_real:
        out.append("        logical :: skip")
    out.append("")
    out.append("        nv = size(values, kind=int64)")
    out.append('        call stats_check_sizes(nv, "pf_count_valid", is_valid, weights)')
    if is_real:
        out.append("        skip = .true.")
        out.append("        if (present(skipnan)) skip = skipnan")
    out.append("        n = 0_int64")
    out.append("        do i = 1_int64, nv")
    out.append("            ! The family's exclusion order: nullness, then NaN, then weight. A weight is")
    out.append("            ! examined only for an element still in the population, which is what keeps a NaN")
    out.append("            ! weight beside a null value from aborting.")
    out.append("            if (present(is_valid)) then")
    out.append("                if (.not. is_valid(i)) cycle")
    out.append("            end if")
    if is_real:
        out.append("            if (skip) then")
        out.append("                ! `x /= x` rather than `ieee_is_nan`: this is a per-element path, and")
        out.append("                ! `ieee_is_nan` is a runtime call on ifx and nagfor. Both are quiet on a")
        out.append("                ! quiet NaN, so the only cost is a -Wcompare-reals warning under -Wall.")
        out.append("                if (values(i) /= values(i)) cycle")
        out.append("            end if")
    out.append("            if (present(weights)) then")
    out.append('                call stats_check_weight(weights(i), i, "pf_count_valid")')
    out.append("                if (weights(i) <= 0.0_real64) cycle")
    out.append("            end if")
    out.append("            n = n + 1_int64")
    out.append("        end do")
    out.append("    end procedure count_valid_%s" % tag)
    return "\n".join(out)


def type_block():
    """TYPE_BLOCK with the per-kind `%compute`/`%update` binding lists filled in."""
    tags = [k[0] for k in MOMENT_KINDS]
    text = TYPE_BLOCK
    for what in ("compute", "update"):
        binds = "\n".join(
            "        procedure, private :: obj_%s_%s !! `%%%s` over %s." % (what, k[0], what, k[4])
            for k in MOMENT_KINDS)
        text = text.replace("@@%s_bindings@@" % what, binds)
        names = ["obj_%s_%s" % (what, t) for t in tags]
        text = text.replace("@@%s_list@@" % what,
                            ", ".join(names[:2]) + ", &\n            " + ", ".join(names[2:]))
    return text


def gen_spec():
    out = [SPEC_HEAD.rstrip("\n") + "\n"]
    out.append("    public :: pf_count_valid")
    out.append("    public :: pf_sum, pf_mean, pf_variance, pf_stddev, pf_sem")
    out.append("    public :: pf_skewness, pf_kurtosis, pf_moments")
    out.append("    public :: pf_median, pf_quantile, pf_quantiles")
    out.append("    public :: pf_iqr, pf_trim_mean, pf_percentile_of_score")
    out.append("    public :: pf_mad, pf_mode, pf_describe")
    # `%print` writes solicited output, so this module READS `verbosity` and `message_stream` --
    # and CLAUDE.md's standing rule is that a module re-exports, getter and setter both, every
    # knob its own code reads, so that a narrow `use parquet_stats` program can silence it
    # without also importing `parquet_settings` and putting the C++ boundary back in its build.
    # They arrive through `parquet_sorting`, which already re-exports them; nothing new enters
    # the dependency graph. FORD 7.0.13 cannot resolve a use-association accessibility statement
    # and reports each of these as an `Unknown entity`, which is expected (CLAUDE.md).
    out.append("    public :: parquet_set_verbosity, parquet_get_verbosity")
    out.append("    public :: parquet_set_message_stream, parquet_get_message_stream")
    out.append("    public :: pf_stats")
    out.append("    public :: parquet_debug_stats_scans, parquet_debug_reset_stats_scans")
    out.append("    public :: parquet_debug_stats_team, parquet_debug_set_stats_min_per_thread")
    out.append("    public :: parquet_debug_stats_sorts, parquet_debug_reset_stats_sorts")
    out.append("    public :: parquet_debug_set_stats_quantile_sort_min")
    out.append(type_block())
    out.append("")
    out.append("    !> How many elements of `values` are in the population -- pandas' `Series.count()`.")
    out.append("    !>")
    out.append("    !> An element is counted unless it is null (`is_valid(i)` is `.false.`), a NaN under the")
    out.append("    !> default `skipnan = .true.` on a kind that can hold one, or carries a zero weight. Those")
    out.append("    !> three exclusions are applied in that order, and they are exactly the ones every other")
    out.append("    !> reduction in this module applies -- so this answers the same `n_valid` a weighted")
    out.append("    !> `pf_mean` over the same arguments would report, which is why it takes `weights` at all.")
    out.append("    !>")
    out.append("    !> `n` is `integer(int64)`. An empty array, or one whose every element is excluded,")
    out.append("    !> answers `0`: this module never aborts on a data condition.")
    out.append("    interface pf_count_valid")
    for tag, _, _, _ in TYPES:
        out.append("        module procedure count_valid_%s" % tag)
    out.append("        module procedure count_valid_col")
    out.append("    end interface pf_count_valid")
    out.append("    !")
    out.append("    ! ---- Counting ----")
    out.append("    interface")
    out.append("\n".join(spec_iface(*t) for t in TYPES))
    out.append(COUNT_COL_IFACE)
    out.append("    end interface")
    for name in ("pf_sum", "pf_mean", "pf_variance", "pf_stddev", "pf_sem", "pf_skewness",
                 "pf_kurtosis", "pf_moments"):
        out.append("    !")
        for line in GENERIC_DOC[name]:
            out.append(("    !> " + line).rstrip())
        for line in _COMMON.split("\n"):
            out.append(line.rstrip())
        out.append("    interface %s" % name)
        for spec in GENERIC_SPECIFICS[name]:
            out.append("        module procedure %s" % spec)
        out.append("    end interface %s" % name)
    for name in ("pf_median", "pf_quantile", "pf_quantiles", "pf_iqr", "pf_trim_mean",
                 "pf_percentile_of_score", "pf_mad"):
        out.append("    !")
        for line in ORDER_DOC[name]:
            out.append(("    !> " + line).rstrip())
        for line in _ORDER_COMMON.split("\n"):
            out.append(line.rstrip())
        out.append("    interface %s" % name)
        for spec in GENERIC_SPECIFICS_ORDER[name]:
            out.append("        module procedure %s" % spec)
        out.append("    end interface %s" % name)
    # `pf_describe` gets its own tail rather than `_ORDER_COMMON`'s: it answers nothing, so a
    # paragraph about quiet NaNs and `ok = .false.` would describe arguments it does not have.
    out.append("    !")
    for line in ORDER_DOC["pf_describe"]:
        out.append(("    !> " + line).rstrip())
    for line in _DESCRIBE_COMMON.split("\n"):
        out.append(line.rstrip())
    out.append("    interface pf_describe")
    for spec in GENERIC_SPECIFICS_ORDER["pf_describe"]:
        out.append("        module procedure %s" % spec)
    out.append("    end interface pf_describe")
    out.append("    !")
    for line in MODE_DOC:
        out.append(("    !> " + line).rstrip())
    out.append("    interface pf_mode")
    for tag, _, _, _, _ in MODE_KINDS:
        out.append("        module procedure mode_%s" % tag)
    out.append("    end interface pf_mode")
    out.append(GUARD_SPEC.rstrip("\n"))
    out.append("    !")
    out.append("    ! ---- The real64 moment core (implemented in parquet_stats_core) ----")
    out.append("    interface")
    for text in CORE_IFACES:
        out.append("\n".join(line.rstrip() for line in text.split("\n")))
    out.append(MOMENTS_IFACE % "\n".join(D[k] for k in MOMENTS_OPTS))
    out.append("    end interface")
    out.append("    !")
    out.append("    ! ---- The real64 order core (implemented in parquet_stats_order) ----")
    out.append("    interface")
    for base, extra, out_name, out_decl, out_doc, opts in ORDER_FAMILY:
        out.append(order_core_iface(base, extra, out_name, out_decl, out_doc, opts,
                                    ORDER_DOC["pf_" + base]))
    out.append(ORDER_HELPER_IFACES.rstrip("\n"))
    out.append("    end interface")
    out.append("    !")
    out.append("    ! ---- pf_mode, one specific per kind (implemented in parquet_stats_order) ----")
    out.append("    interface")
    out.append("\n".join(mode_iface(*k) for k in MODE_KINDS))
    out.append("    end interface")
    out.append("    !")
    out.append("    ! ---- The per-kind entry layer (implemented in parquet_stats_kernel) ----")
    out.append("    interface")
    for tag, decl, _, has_nan, kindword in MOMENT_KINDS:
        for base, out_name, out_doc, opts in ENTRY_FAMILY:
            out.append(entry_iface(base, out_name, out_doc, opts, tag, decl, has_nan, kindword))
        out.append(entry_moments_iface(tag, decl, has_nan, kindword))
        for base, extra, out_name, out_decl, out_doc, opts in ORDER_FAMILY:
            out.append(order_entry_iface(base, extra, out_name, out_decl, out_doc, opts,
                                         tag, decl, has_nan, kindword))
    out.append(obj_entry_ifaces())
    out.append("    end interface")
    out.append(object_ifaces())
    out.append("    !")
    out.append("end module parquet_stats ! GCOVR_EXCL_LINE")
    return "\n".join(out) + "\n"


def gen_kernel():
    out = [KERNEL_HEAD.rstrip("\n")]
    out.append("")
    out.append(COL_WIDEN)
    out.append("")
    out.append("\n\n".join(body(*t) for t in TYPES))
    out.append("")
    out.append(COUNT_COL_BODY)
    out.append("")
    bodies = []
    for tag, _, widen, has_nan, _ in MOMENT_KINDS:
        for base, out_name, _, opts in ENTRY_FAMILY:
            bodies.append(entry_body(base, out_name, opts, tag, widen, has_nan))
        bodies.append(entry_moments_body(tag, widen, has_nan))
        for base, extra, out_name, _, _, opts in ORDER_FAMILY:
            bodies.append(order_entry_body(base, extra, out_name, opts, tag, widen, has_nan))
    out.append("\n\n".join(bodies))
    out.append("")
    out.append(obj_entry_bodies())
    out.append("")
    out.append("end submodule parquet_stats_kernel ! GCOVR_EXCL_LINE")
    return "\n".join(out) + "\n"


def main():
    ap = argparse.ArgumentParser(description="Generate the per-kind blocks of parquet_stats.")
    ap.add_argument("--check", action="store_true",
                    help="compare a fresh generation with the committed files; exit 1 on drift")
    ap.add_argument("--self-test", action="store_true",
                    help="verify the imported kind table still has the shape this script assumes")
    args = ap.parse_args()

    if args.self_test:
        numeric = _load_numeric_kinds()
        tags = [k[0] for k in numeric]
        if tags != EXPECTED_NUMERIC_TAGS:
            print("generate_parquet_stats.py: expected numeric scalar kinds %s, got %s.\n"
                  "Update TYPES (and EXPECTED_NUMERIC_TAGS) here to match, then re-run --check."
                  % (EXPECTED_NUMERIC_TAGS, tags), file=sys.stderr)
            return 1
        ours = [t[0] for t in TYPES]
        if ours != tags:
            print("generate_parquet_stats.py: TYPES %s does not match the imported kind table %s."
                  % (ours, tags), file=sys.stderr)
            return 1
        for (tag, _, decl, _, _, _), (our_tag, our_decl, _, _) in zip(numeric, TYPES):
            if decl != our_decl:
                print("generate_parquet_stats.py: kind '%s' declares '%s' upstream but '%s' here."
                      % (tag, decl, our_decl), file=sys.stderr)
                return 1
        print("generate_parquet_stats.py: self-test OK (%d numeric scalar kinds imported and emitted)."
              % len(TYPES))
        return 0

    outputs = {
        REPO_ROOT / "src" / "parquet_stats.f90": gen_spec(),
        REPO_ROOT / "src" / "parquet_stats_kernel.f90": gen_kernel(),
    }

    over = [(p.name, i + 1, len(ln)) for p, text in outputs.items()
            for i, ln in enumerate(text.split("\n")) if len(ln) > 132]
    if over:
        print("generate_parquet_stats.py: %d emitted line(s) exceed 132 columns:" % len(over), file=sys.stderr)
        for name, ln, w in over[:10]:
            print("  %s:%d (%d columns)" % (name, ln, w), file=sys.stderr)
        return 1

    if args.check:
        drift = [p.relative_to(REPO_ROOT) for p, text in outputs.items()
                 if not p.exists() or p.read_text() != text]
        if drift:
            print("generate_parquet_stats.py --check: %d file(s) differ from a fresh generation:"
                  % len(drift), file=sys.stderr)
            for d in drift:
                print("  %s" % d, file=sys.stderr)
            print("Re-run tools/generate_parquet_stats.py to regenerate.", file=sys.stderr)
            return 1
        print("generate_parquet_stats.py --check: %d file(s) are current." % len(outputs))
        return 0

    for path, text in outputs.items():
        path.write_text(text)
        print("wrote %s (%d lines)" % (path.relative_to(REPO_ROOT), text.count("\n")))
    return 0


if __name__ == "__main__":
    sys.exit(main())
