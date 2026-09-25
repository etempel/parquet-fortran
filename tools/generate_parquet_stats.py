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

Maintainer-only: nothing outside this repository runs it. (Contrast a CONSUMER-FACING generator
such as generate_parquet_maml.sh, which a downstream project runs on its own schemas and which
CONTRIBUTING.md's index table marks as such.)
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
!! declares its optional dummies as a SUBSEQUENCE of one canonical sequence, whose main run is
!! `is_valid, weights, weight_type, ddof, bias, excess, skipnan, method, kind, scale, center,`
!! `out_valid, n_null, n_nan, n_outside, ok, threads` -- so that a reader who has seen one
!! signature has seen them all. Four smaller blocks sit around it: the output prefix
!! `n_valid, mean, variance, stddev, sem, skewness, kurtosis, vsum, vmin, vmax, count, modes, corr`,
!! the object-lifecycle pair `retain, consume`, the rule block
!! `sigma, sigma_lower, sigma_upper, maxiters, cenfunc, stdfunc, n_clipped, keep, converged, right,`
!! `density, prob`, and `%print`'s `unit, name`. `STATS_OPTIONAL_ORDER` in
!! `tools/check_source_conventions.py` is the sequence itself;
!! `check_stats_optional_argument_order` enforces it against this file and
!! `check_stats_optional_order_documented` against `doc/pages/utilities/statistics.md`.
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
    ! `only:` rather than bare, unlike `parquet_sorting` above: two names are needed, and a
    ! consumer who wants the normal family itself should say `use parquet_utils` and get all four
    ! of them rather than the specifics this module happens to need.
    !
    ! `pf_norm_cdf` is the return leg of `pf_probit_mean`, which averages on the probit scale and
    ! maps back -- and it must be the LIBRARY's `Phi`, not `0.5*erfc(-z/sqrt(2))` written out
    ! again here, or the round trip `pf_norm_cdf(pf_probit(p)) == p` would hold for the kernel and
    ! not for this module.
    use parquet_utils, only : pf_probit, pf_norm_cdf
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
    ! wherever it lives; with no `unit=` it writes where `parquet_message_unit` says, which is the
    ! one resolver the library's own emit channels use too. `parquet_sorting` re-exports the
    ! getters and setters but neither of these, so both are named directly; `parquet_settings_base`
    ! is already in the closure.
    use parquet_settings_base, only : parquet_output_is_suppressed, parquet_message_unit
    use iso_fortran_env, only : int32, int64, real32, real64
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
            !! by "the MAD". That is a multiplication by 1.4826022185056018, and **not** by the
            !! rounded 1.4826 the textbooks quote, which differs by 1.5e-06 relative. "raw" is the
            !! unscaled median of |x - center|. Any other token aborts, naming both. Matched
            !! case-insensitively."""
D["center"] = """            real(real64), intent(in), optional :: center
            !! the centre the deviations are taken about. Absent uses the population's own median,
            !! which is what scipy does. Supplying one skips ONE selection and not the ordering:
            !! the median of |x - center| still has to be found. A NaN or infinite centre ABORTS
            !! -- unlike a NaN value, which is an ordinary data condition, a NaN centre can only
            !! come from the caller's own arithmetic."""
D["sigma"] = """            real(real64), intent(in), optional :: sigma
            !! how many scale units a value may sit from the centre and still be kept. 3.0 by
            !! default, as astropy does. `sigma_lower` and `sigma_upper` each default to this, so
            !! setting it alone gives a symmetric clip."""
D["sigma_lower"] = """            real(real64), intent(in), optional :: sigma_lower
            !! the lower half of the clip, overriding `sigma` below the centre."""
D["sigma_upper"] = """            real(real64), intent(in), optional :: sigma_upper
            !! the upper half of the clip, overriding `sigma` above the centre."""
D["maxiters"] = """            integer, intent(in), optional :: maxiters
            !! how many clipping rounds to run; 5 by default, as astropy does. **A non-positive
            !! value means iterate until nothing more is removed**, which is astropy's
            !! `maxiters=None` -- capped at a hard internal 100 rounds, and reaching that cap
            !! STOPS and reports through `converged` rather than aborting. Each non-terminating
            !! round removes at least one point, so a large catalogue can honestly want more
            !! rounds than any small constant."""
D["cenfunc"] = """            character(len=*), intent(in), optional :: cenfunc
            !! which centre each round clips about: "median" (the default, as astropy) or "mean".
            !! Any other token aborts, naming both. Matched case-insensitively."""
D["stdfunc"] = """            character(len=*), intent(in), optional :: stdfunc
            !! which scale each round measures: "std" (the default, as astropy -- the sample
            !! standard deviation at `ddof = 1`) or "mad_std", the normal-scaled median absolute
            !! deviation, which is far less disturbed by the outliers being clipped. Any other
            !! token aborts, naming both. Matched case-insensitively."""
D["out_valid"] = """            logical, intent(out), optional :: out_valid(:)
            !! per output element: .false. where the input was excluded and the result is
            !! therefore undefined. Same size as `values`. **Absent, such an element is written as
            !! a quiet NaN instead** -- which is loud in practice, since it propagates through
            !! whatever the caller does next."""
D["n_clipped"] = """            integer(int64), intent(out), optional :: n_clipped
            !! how many elements of the population the clipping removed. `0` for an empty
            !! population, and `0` when the first round removes nothing."""
D["keep"] = """            logical, intent(out), optional :: keep(:)
            !! per element of `values`: .true. for one that survived both the exclusion rules and
            !! every clipping round. Same size as `values`. This is what a caller needs in order
            !! to apply the same clip to a SECOND column."""
D["converged"] = """            logical, intent(out), optional :: converged
            !! .false. when the iteration stopped at its round limit with points still being
            !! removed, rather than because a round removed nothing. The answer is still usable
            !! -- it is the last completed round's -- so `ok` stays .true. and this is the only
            !! way to tell the two endings apart."""
D["right"] = """            logical, intent(in), optional :: right
            !! which side of a bin is closed. `.false.` by default, which is numpy's rule and the
            !! one `np.histogram`/`np.digitize` use: bin k is `[edges(k), edges(k+1))`, and the
            !! LAST bin closes at the top so that `edges(nbins+1)` itself lands in it. `.true.`
            !! mirrors it, which is `pd.cut(right=True, include_lowest=True)`: bin k is
            !! `(edges(k), edges(k+1)]`, and the FIRST bin closes at the bottom. Either way the
            !! closed range `[edges(1), edges(nbins+1)]` is covered exactly once, so the two
            !! conventions differ only in which bin a value sitting exactly on an interior edge
            !! joins."""
D["density"] = """            logical, intent(in), optional :: density
            !! `.false.` by default. `.true.` divides each bin by its own WIDTH and by the total
            !! that was binned, giving a probability density -- `np.histogram(density=True)`, so
            !! `sum(counts * widths)` is 1 and an uneven edge spacing is accounted for. The
            !! normalisation base is what actually landed in a bin: values outside the edges were
            !! never counted, so they do not appear in it, which is numpy's rule too. When nothing
            !! was binned the density is undefined and every entry is a **quiet NaN** with
            !! `ok = .false.` -- note that an empty HISTOGRAM is perfectly well defined (all
            !! zeros, `ok = .true.`) and an empty DENSITY is not."""
D["corr"] = """            real(real64), intent(out), optional :: corr
            !! the PROBABILITY-PLOT CORRELATION COEFFICIENT of the fitted line -- how straight the
            !! normal-probability plot actually is, in [-1, 1] and near 1 for a Gaussian sample.
            !! It is free once the line is fitted, since the same three sums answer it, so this is
            !! the normality diagnostic the fit already contains rather than a second pass over
            !! the data. Under `method="filliben"` it is Filliben's statistic exactly, which is
            !! also `scipy.stats.probplot(x, fit=True)`'s third return value -- scipy's plotting
            !! positions are the median ranks, not this generic's default. **NaN for a CONSTANT
            !! population**, where the sorted values have no variance to correlate -- and that is
            !! the one case where `loc` and `sigma` are both perfectly well defined while this is
            !! not, so it is reported rather than inferred from `ok`."""
D["prob"] = """            real(real64), intent(in), optional :: prob
            !! the LOWER tail probability of the symmetric quantile pair, `0.25` by default --
            !! which makes the answer `pf_iqr` divided by 1.3489795003921634. The other value in
            !! daily use is `0.15865525393145705`, the 16th/84th percentile pair, whose divisor is
            !! exactly 2. Must satisfy `0 < prob < 0.5`; anything else aborts, because `prob=0.5`
            !! asks for `0/0` and `prob=0` asks for the whole range over an infinity. Both are
            !! mistakes rather than preferences, which is the same reading `pf_trim_mean` gives
            !! `prop`.
            !!
            !! **A probability, not a percentile**: `0.25`, never `25`.
            !! `prob` and `1 - prob` are the two quantiles taken, so it does not matter which half
            !! of the pair a caller thinks in -- but the argument is the LOWER one."""
D["n_outside"] = """            integer(int64), intent(out), optional :: n_outside
            !! how many usable elements fell outside `[edges(1), edges(nbins+1)]` and so joined no
            !! bin. This is an ordinary data condition rather than an error -- `np.histogram`
            !! drops such values silently -- but it is the one thing a caller cannot recover from
            !! the output, so it is reported. A NaN under `skipnan = .false.` is counted here: it
            !! is then a value, and it matches no bin."""
D["count"] = """            integer(int64), intent(out), optional :: count
            !! how many elements hold the modal value. `0` when the population is empty. Counts
            !! ELEMENTS even when weights decided which value won, so a weighted mode reports the
            !! occurrences of the value carrying the greatest total weight."""
D["threads"] = """            integer, intent(in), optional :: threads
            !! how many threads the central-moment pass may use. Absent takes the automatic rule:
            !! the `sort_threads` setting, capped by the processors actually available and
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
        procedure :: gmean => obj_gmean !! The weighted geometric mean; needs `retain`.
        procedure :: hmean => obj_hmean !! The weighted harmonic mean; needs `retain`.
        procedure :: probit_mean => obj_probit_mean !! The probit-space mean; needs `retain`.
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
        procedure :: probit_fit => obj_probit_fit !! The normal-probability line: loc, sigma, corr.
        procedure :: probit_scale => obj_probit_scale !! The scale a quantile pair implies.
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
        !! Both must agree on all THREE of the policies fixed at `%compute`/`%init` -- `retain`,
        !! `weight_type` and `skipnan` -- and the source must have been computed; a mismatch
        !! aborts, naming which. A source with no elements at all is a no-op.
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
    ("obj_gmean", "real(real64)", "the weighted geometric mean, or NaN when it is undefined.",
     [],
     ["The geometric mean of the population -- `exp(sum(w*log(x)) / sum(w))`.",
      "",
      "**Needs the retained values, so it aborts on a streaming accumulator**",
      "(`retain = .false.`), exactly as the order statistics do. It is not derivable from the",
      "four central moments the accumulator carries -- a log-sum is a fifth quantity, and",
      "accumulating it in the hot loop would charge a transcendental per element to every",
      "population that never asks for one. Unlike `%median` it orders nothing.",
      "",
      "NaN for an empty population and for one holding a negative value; **exactly 0 when any",
      "value is 0**, which is the limit and what scipy returns."]),
    ("obj_hmean", "real(real64)", "the weighted harmonic mean, or NaN when it is undefined.",
     [],
     ["The harmonic mean of the population -- `sum(w) / sum(w/x)`.",
      "",
      "Aborts on a streaming accumulator for the reason `%gmean` does, and orders nothing.",
      "",
      "NaN for an empty population and for one holding a negative value; **exactly 0 when any",
      "value is 0**, as scipy returns."]),
    ("obj_probit_mean", "real(real64)",
     "the probit-space mean, or NaN when it is undefined.", [],
     ["The mean of the population ON THE PROBIT SCALE -- `Phi(sum(w*Phi**(-1)(p)) / sum(w))`.",
      "",
      "**Needs the retained values, so it aborts on a streaming accumulator**",
      "(`retain = .false.`), exactly as `%gmean` does and for the same reason: a probit-sum is a",
      "fifth accumulated quantity, and charging a transcendental per element in the hot loop to",
      "every population that never asks for one is the wrong trade. It orders nothing.",
      "",
      "The population is read as PROBABILITIES. NaN for an empty one and for one holding a value",
      "outside `[0, 1]`; **exactly 0 when any value is 0** and **exactly 1 when any value is 1**,",
      "both being the limits; NaN when it holds both, since that limit is `-Inf + Inf`."]),
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
    tierb = tierb.replace("@@nscore_method@@", NSCORE_D["method"])
    tierb = tierb.replace("@@corr@@", D["corr"])
    tierb = tierb.replace("@@prob@@", D["prob"])
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
        !> The normal-probability line of the population: `pf_probit_fit` off the ordering.
        !!
        !! A subroutine rather than a function because it answers two quantities and optionally a
        !! third, and a caller wanting only the scale should reach for `%probit_scale` instead of
        !! discarding two results.
        !!
        !! Reads tier B and orders nothing that has not already been ordered, so a `%median`
        !! followed by this is ONE sort; `parquet_debug_stats_sorts()` is what asserts it. Aborts
        !! on a streaming accumulator, as every tier-B binding does.
        !!
        !! **Unweighted, whatever the accumulator carries.** The one-shot `pf_probit_fit` has no
        !! `weights` argument at all, and a binding that quietly used the object's weights would
        !! answer a different question under the same name; the weights are ignored here and the
        !! guide says so.
        module subroutine obj_probit_fit(self, loc, sigma, corr, method)
            class(pf_stats), intent(inout) :: self
            !! the accumulator; the first order statistic builds its sorted buffer.
            real(real64), intent(out) :: loc !! the intercept: the mean of the population.
            real(real64), intent(out) :: sigma !! the slope: the implied scale.
@@corr@@
@@nscore_method@@
        end subroutine obj_probit_fit
        !> The scale a symmetric quantile pair implies -- `pf_probit_scale` off the ordering.
        module function obj_probit_scale(self, prob, method) result(res)
            class(pf_stats), intent(inout) :: self
            !! the accumulator; the first order statistic builds its sorted buffer.
@@prob@@
@@method@@
            real(real64) :: res !! the implied scale, or NaN for an empty population.
        end function obj_probit_scale
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
#: `pf_moments` -- `ok` here means "every quantity you ASKED for came back defined", which is the
#: only reading available on a procedure with nine optional outputs: testing all nine would report
#: `.false.` for a caller who asked for the sum of an empty population and got the correct `0`.
MOMENTS_OPTS = ["is_valid", "weights", "weight_type", "ddof", "bias", "excess", "skipnan",
                "n_null", "n_nan", "ok", "threads"]

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
#: `pf_describe` -- it FILLS an object rather than answering, so `n_null`/`n_nan` are absent: the
#: object carries both and answering them twice would let the two copies disagree. `ok` is not in
#: that class, because the object has no `%ok()` to disagree with -- it is the one thing a caller
#: cannot read off the result without picking a statistic and NaN-testing it by hand.
DESCRIBE_OPTS = ["is_valid", "weights", "weight_type", "skipnan", "ok", "threads"]
#: `pf_mode` -- no `skipnan`/`n_nan` on ANY specific, because no kind it accepts can hold a NaN,
#: and no `threads`, because the ordering it does is `pf_argsort`'s own and takes the same
#: automatic rule every other unthreaded caller does.
MODE_OPTS = ["count", "modes", "is_valid", "weights", "n_null", "ok"]
#: `pf_zscore` -- a VECTOR-valued procedure, so `out_valid` reports which output elements are
#: undefined. No `weights`: a weighted z-score would need a weighted mean and a weighted scale,
#: and then the standardised values would not have unit variance under any convention worth
#: choosing between.
ZSCORE_OPTS = ["is_valid", "ddof", "skipnan", "out_valid", "n_null", "ok"]
#: `pf_normal_scores` -- `pf_zscore`'s shape with `method` in place of `ddof`, plus `threads`.
#:
#: **No `weights`, and the omission is a COMPILE error rather than a run-time one.** A weighted
#: midrank is a further definitional choice that no reference library makes; `pf_corr` already
#: refuses one at run time (`corr_method`), but a procedure that never had the argument refuses
#: it at compile time instead, which is strictly better and is what `pf_sigma_clipped_stats`
#: does. `threads` IS here, unlike `pf_zscore`'s: the midrank walk is `pf_argsort`'s, and that
#: sort takes a thread count.
NSCORE_OPTS = ["is_valid", "skipnan", "method", "out_valid", "n_null", "ok", "threads"]
#: `pf_probit_fit` -- `corr` is an OUTPUT and sits in the tier-A output-prefix block, ahead of
#: every population argument, which is where the procedure declares it. That position is the one
#: `check_stats_optional_argument_order` requires: an optional output a caller reaches for by
#: keyword still has to keep every other list a subsequence of the one canonical sequence.
#:
#: **No `weights`, for `pf_normal_scores`' reason**: the plotting position of a weighted order
#: statistic is a further definitional choice that no reference library makes, and a procedure
#: that never had the argument refuses one at COMPILE time rather than at run time.
PFIT_OPTS = ["corr", "is_valid", "skipnan", "method", "n_null", "n_nan", "ok", "threads"]
#: `pf_probit_scale` -- `ORDER_OPTS` with `prob` in front of it. `prob` says what the statistic IS
#: rather than which elements are in the population, so it goes at the head of the population
#: block, exactly as `pf_sigma_clipped_stats` puts its clipping rule and `pf_histogram` its `right`
#: and `density`. It takes `weights` and `method` where `pf_probit_fit` takes neither, and that is
#: not an inconsistency: this procedure is two `pf_quantile` probes off one ordering, so it offers
#: precisely what `pf_quantile` offers.
PSCALE_OPTS = ["prob", "is_valid", "weights", "weight_type", "skipnan", "method", "n_null",
               "n_nan", "ok", "threads"]
#: `pf_cov` -- no `skipnan`, because a NaN in EITHER array always drops the pair: a two-sample
#: statistic has no meaning over two populations of different length, so there is nothing for
#: `skipnan = .false.` to select.
#:
#: **`weight_type` IS here, for the same reason `pf_variance` has it.** `pf_cov(x, x, ddof=d)` is
#: `pf_variance(x, ddof=d)` by definition, so a covariance that offered only the reliability
#: convention would silently disagree with the variance whenever the caller asked `pf_variance`
#: for frequency weights -- the same `ddof`, the same weights, two different denominators.
COV_OPTS = ["is_valid", "weights", "weight_type", "ddof", "n_null", "n_nan", "ok"]
#: `pf_corr` -- as `pf_cov` but with `method` and WITHOUT `ddof` or `weight_type`. The `ddof` in
#: the covariance and the two in the standard deviations cancel exactly, and the two conventions
#: differ only in the denominator `ddof` is charged against, so neither argument could ever change
#: the answer -- and an argument that can never do anything is worse than an absent one.
CORR_OPTS = ["is_valid", "weights", "method", "n_null", "n_nan", "ok"]
#: `pf_sigma_clipped_stats` -- the clipping rule first, then its own outputs, then the population
#: block. No `weights`: a weighted scale estimator is a further definitional choice and passing
#: `weights` here aborts rather than picking one silently.
SIGCLIP_OPTS = ["sigma", "sigma_lower", "sigma_upper", "maxiters", "cenfunc", "stdfunc",
                "n_clipped", "keep", "converged", "is_valid", "skipnan", "n_null", "n_nan", "ok",
                "threads"]

#: The cumulative family -- `pf_cumsum`, `pf_cumprod`, `pf_cummax`, `pf_cummin`. VECTOR-valued,
#: so `out_valid` again, and no `weights`: a weighted running sum is `pf_cumsum(w*x)` and the
#: other three have no weighted meaning worth choosing between. No `threads` either -- a prefix
#: scan is sequential by definition, so an argument here could only ever be ignored.
CUM_OPTS = ["is_valid", "skipnan", "out_valid", "n_null", "n_nan", "ok"]
#: The binning family. The bin rule comes first, before the population block, exactly as
#: `pf_sigma_clipped_stats`' clipping rule does -- `right` says what the bins ARE, so it belongs
#: with `edges` rather than among the exclusions.
#:
#: **`pf_bucketize` takes `weights` for one reason: without it the documented
#: `pf_histogram == pf_bucketize`-tallied identity is FALSE whenever `pf_histogram` is weighted.**
#: A zero weight removes an element from the population everywhere else in this module, so a
#: bucketize that could not see weights assigned that element an ordinary bin code while the
#: histogram over the same arguments left it out. The weight never scales a code -- a code is a
#: bin number -- it only decides membership, which is what makes the identity hold again.
BUCKETIZE_OPTS = ["right", "is_valid", "weights", "skipnan", "n_null", "n_nan", "n_outside", "ok"]
#: `pf_histogram` adds `weights`, which is why `counts` is a real array: `np.histogram(weights=)`
#: sums the weights of the values in each bin rather than counting them, and an integer output
#: could not hold that.
HISTOGRAM_OPTS = ["right", "density", "is_valid", "weights", "skipnan", "n_null", "n_nan",
                  "n_outside", "ok"]
#: `pf_bin_edges` -- the population block and nothing else. No `right`: it produces edges rather
#: than using them, so the convention is the CALLER's later choice and an argument here could only
#: be ignored. No explicit range pair either -- see its doc-comment for why.
BIN_EDGES_OPTS = ["is_valid", "weights", "skipnan", "n_null", "n_nan", "ok"]
#: `pf_bin_linear` -- `pf_histogram`'s population block without its rule pair. No `right`: a value
#: exactly on an interior grid point gives that point its whole weight whichever cell it is taken
#: to belong to, so there is no side to close. No `density`: the share of the axis belonging to a
#: grid point is undefined at the two end points, which is where a density would be compared.
BIN_LINEAR_OPTS = ["is_valid", "weights", "skipnan", "n_null", "n_nan", "n_outside", "ok"]

CORE_IFACES = [
    iface("sum_f64",
          ["`pf_sum` over a 64-bit real array: the PAIRWISE sum of the population.",
           "",
           "Weighted, this is `sum(w*x)`. An empty population sums to exactly `0` with",
           "`ok = .true.`, which is the additive identity and what numpy and pandas return -- it",
           "is the one quantity in this family that an empty population still defines.",
           "",
           "**An `integer(int64)` population is summed in `real64`, so it is exact only while",
           "every value is**, i.e. while `abs(v) <= 2**53`. Above that the WIDENING loses the low",
           "bits of each element before any addition happens, so no summation order recovers",
           "them: `pf_sum([2**53+1, 1, 2**53+1, 1])` answers `1.8014398509481984e16` where the",
           "exact integer total is `18014398509481988`. numpy and pandas keep an int64 sum in",
           "int64 and stay exact (until they wrap). This module has one engine and one",
           "`real(real64)` result -- which is what makes every kind return the same bits -- so",
           "a population of genuinely huge integers has to be summed by the caller, in int64, or",
           "shifted and scaled before it gets here. int32 is unaffected: every int32 value is",
           "exactly representable."],
          ("s", "the sum."), PLAIN_OPTS),
    iface("mean_f64",
          ["`pf_mean` over a 64-bit real array: `sum(w*x) / sum(w)`, REFINED.",
           "",
           "The quotient is taken first and corrected by `sum(w*(x - mu))/sum(w)`, so the result",
           "can sit one ulp from the naive quotient. See `pf_mean`'s own doc-comment for why that",
           "is the right answer and what it means for a test."],
          ("m", "the mean; NaN when the population is empty."), PLAIN_OPTS),
    iface("gmean_f64",
          ["`pf_gmean` over a 64-bit real array: the geometric mean.",
           "",
           "`exp(sum(w*log(x)) / sum(w))`, which is `scipy.stats.gmean`. Computed in log space",
           "rather than as a product, because the product of a few thousand magnitudes overflows",
           "and its logarithm does not.",
           "",
           "Two values outside the domain are answered rather than refused: a value **exactly 0**",
           "gives exactly `0` (the limit, and scipy's answer), and any **negative** value gives a",
           "quiet NaN with `ok = .false.`. Both are branched on BEFORE the logarithm is reached,",
           "which is a correctness requirement rather than tidiness -- `log(0)` raises",
           "`IEEE_DIVIDE_BY_ZERO` and nagfor unmasks the IEEE traps by default, so reaching it",
           "would terminate the process on that compiler while returning a plausible answer on",
           "every other one."],
          ("g", "the geometric mean; 0 for a zero, NaN for a negative or empty population."),
          PLAIN_OPTS),
    iface("hmean_f64",
          ["`pf_hmean` over a 64-bit real array: the harmonic mean.",
           "",
           "`sum(w) / sum(w/x)`, which is `scipy.stats.hmean`. Same domain rules as `pf_gmean`",
           "and for the same reason: a value exactly 0 gives exactly `0`, a negative one gives a",
           "quiet NaN, and both are branched on before the reciprocal, since `1/0` raises under",
           "nagfor's default trap policy."],
          ("h", "the harmonic mean; 0 for a zero, NaN for a negative or empty population."),
          PLAIN_OPTS),
    iface("probit_mean_f64",
          ["`pf_probit_mean` over a 64-bit real array: the mean ON THE PROBIT SCALE.",
           "",
           "`Phi(sum(w*Phi**(-1)(p)) / sum(w))`. It is to `Phi`/`Phi**(-1)` exactly what",
           "`pf_gmean` is to `exp`/`log`, and it lives beside it for that reason.",
           "",
           "The domain rules are `pf_gmean`'s in shape and its own in detail. A value outside",
           "`[0, 1]` gives a quiet NaN with `ok = .false.`; a `0` in the population gives exactly",
           "`0` and a `1` gives exactly `1`, both being the limits; a population holding **both**",
           "gives a NaN, because that limit is `-Inf + Inf`.",
           "",
           "**Those three answers arrive from the infinities rather than from a special case**,",
           "and a guard that clamped `p` away from `{0, 1}` to avoid meeting one would silently",
           "replace all three with finite numbers near the boundary. The NaN screen comes FIRST",
           "and separately, because `p >= 0 .and. p <= 1` is false for a NaN and would count one",
           "as an out-of-domain value -- and the two have different answers under",
           "`skipnan = .false.`"],
          ("m", "the probit-space mean, back on a 0-1 scale; NaN when it is undefined."),
          PLAIN_OPTS),
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
@@count_col_counts@@
        end subroutine count_valid_col"""

COUNT_COL_BODY = """    module procedure count_valid_col
        real(real64), allocatable :: wide(:)
        logical, allocatable :: mask(:)
        call col_to_real64(values, "pf_count_valid", is_valid, wide, mask)
        call count_valid_f64(wide, n, mask, weights, skipnan, n_null, n_nan)
    end procedure count_valid_col"""


#: The one place a `parquet_column` becomes a population. Every `*_col` entry point goes through
#: it, so the three refusals and the nullness rule are stated once rather than eight times. It is a
#: separate module procedure, its interface in the spec (`COL_WIDEN_SPEC`), because `parquet_kde`'s
#: column forms call it too; its body stays FIRST in this submodule, above every call to it, which
#: nagfor requires of a separate module procedure.
COL_WIDEN = '''    !> Combines two columns' validity masks into the one a PAIRWISE statistic needs.
    !!
    !! Either input may be unallocated, which `col_to_real64` uses to mean "this column has no
    !! nulls". When BOTH are, `both` is left unallocated too -- and an unallocated allocatable
    !! passed to an `optional` dummy is ABSENT, so the core takes its no-mask fast path with no
    !! branch at the call site. That is the same idiom `col_to_real64` documents for one column.
    subroutine pair_mask(a, b, n, both)
        logical, allocatable, intent(in) :: a(:)          !! the first column's validity, or none.
        logical, allocatable, intent(in) :: b(:)          !! the second's, or none.
        integer(int64), intent(in) :: n                   !! how many pairs there are.
        logical, allocatable, intent(out) :: both(:)      !! their conjunction, or unallocated.

        if (.not. allocated(a) .and. .not. allocated(b)) return
        allocate(both(n))
        both = .true.
        if (allocated(a)) both = both .and. a
        if (allocated(b)) both = both .and. b
    end subroutine pair_mask

    module procedure col_to_real64
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
    end procedure col_to_real64'''


#: The column widener's interface, published for `parquet_kde`'s column forms and implemented FIRST
#: in `parquet_stats_kernel` (see `COL_WIDEN`).
COL_WIDEN_SPEC = """    !
    ! ---- The column widener (implemented in parquet_stats_kernel) ----
    interface
        !> Widens a scalar numeric `parquet_column` into a `real64` population and its validity mask.
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
        !! metadata queries it makes have no typed twin and are called once per column, which is the
        !! same trade `parquet_sorting` already makes for its key extraction.
        !!
        !! Published as plumbing, beside the exclusion pass and the checkers, so that `parquet_kde`'s
        !! column forms widen a column exactly as this family does; the `parquet` facade hides it.
        module subroutine col_to_real64(col, what, is_valid, v, mask)
            type(parquet_column), intent(in), target :: col
            !! the column. `target` because `parquet_column_data_ptr` requires it; the pointer never
            !! leaves this procedure, so the caller's actual argument needs no `target` of its own.
            character(len=*), intent(in) :: what              !! the public procedure's name.
            logical, intent(in), optional :: is_valid(:)      !! must be absent; see above.
            real(real64), allocatable, intent(out) :: v(:)    !! the widened population.
            logical, allocatable, intent(out) :: mask(:)      !! its validity, or unallocated.
        end subroutine col_to_real64
    end interface"""


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

#: The kinds `pf_bin_linear` offers: every row above but `logical`. Depositing `.true.` and
#: `.false.` onto a continuous grid answers no question, so the array form is absent -- a compile
#: error rather than a run-time one -- and the column form refuses a logical column by name
#: (`bin_linear_body`). Every other generic here carries all six forms, which is why the generic's
#: own doc-comment says so: removing that sentence would make the gap read as an oversight.
BIN_LINEAR_KINDS = [k for k in MOMENT_KINDS if k[0] != "bool"]

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
    ("gmean", "g", "the geometric mean; 0 for a zero, NaN for a negative or empty population.",
     PLAIN_OPTS),
    ("hmean", "h", "the harmonic mean; 0 for a zero, NaN for a negative or empty population.",
     PLAIN_OPTS),
    ("probit_mean", "m",
     "the probit-space mean; 0 for a zero, 1 for a one, NaN for both or out of range.",
     PLAIN_OPTS),
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
        !> The two-sample two-pass, over pairs the caller has already compacted.
        !!
        !! Written in the same submodule as `stats_engine` so that both reach the same
        !! `STATS_BLOCK` block tree, the same `pair_reduce`, the same refined mean and the same
        !! re-centring correction.
        !!
        !! **`pf_cov(x, x)` does not come through here**: `cov_f64` answers a diagonal pair by
        !! calling `variance_f64`, since the identity `pf_cov(x, x) == pf_variance(x)` is
        !! documented as exact and no arrangement of mirrored source can deliver that under a
        !! compiler free to rewrite arithmetic -- see the note at that call site. The diagonal
        !! fork in the body remains for `pf_corr(x, x) == 1`, and is written out there.
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
        !> Is this pair the diagonal -- `kx(i) == ky(i)` for every surviving pair?
        !!
        !! One copy, because two callers ask: `cov_f64`, which hands a diagonal pair to
        !! `variance_f64`, and `stats_pair_moments`, whose own diagonal fork keeps the three
        !! centred sums exactly equal. Neither array can hold a NaN (the pair compaction in
        !! parquet_stats_relate.f90 drops any pair holding one), so `/=` is an ordinary
        !! comparison; an infinity compares equal to itself and raises nothing.
        module function stats_pair_is_diagonal(kx, ky, m) result(res)
            real(real64), intent(in) :: kx(:) !! the surviving first-sample values.
            real(real64), intent(in) :: ky(:) !! the surviving second-sample values, paired.
            integer(int64), intent(in) :: m !! how many pairs survived.
            logical :: res !! .true. when every pair is diagonal, .false. on the first that is not.
        end function stats_pair_is_diagonal
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
        end subroutine stats_ensure"""

#: The real64 cores of the P8 families, and the generic doc each carries. Literal rather than
#: table-driven: the three shapes -- two samples in, an array out, three scalars out -- share
#: nothing but the module's exclusion rules, so a table would carry one row per procedure.
RELATE_IFACES = """        !> `pf_cov` over two 64-bit real arrays: the PAIRWISE-COMPLETE covariance.
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
@@cov_opts@@
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
@@corr_opts@@
        end subroutine corr_f64
        !> `pf_zscore` over a 64-bit real array: `(x - mean) / stddev`, element by element.
        module subroutine zscore_f64(values, z, is_valid, ddof, skipnan, out_valid, n_null, ok)
            real(real64), intent(in) :: values(:) !! the population.
            real(real64), intent(out) :: z(:)
            !! the standardised values, same size as `values`.
@@zscore_opts@@
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
@@nscore_opts@@
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
@@sigclip_opts@@
        end subroutine sigma_clipped_stats_f64
        !> `pf_probit_fit` over a 64-bit real array: the least-squares normal-probability line.
        !!
        !! **One ordering, and the fit is read straight off it.** The sorted survivors are the
        !! `y` of the line and their plotting positions' normal scores are the `x`, so once the
        !! values are in order the whole estimator is three running sums.
        !!
        !! Declared in this block rather than with the order family because it has TWO mandatory
        !! outputs and a third optional one, which the single-output `ORDER_FAMILY` shape cannot
        !! express -- exactly the reason `pf_sigma_clipped_stats` sits here. Both are implemented
        !! in `parquet_stats_order` all the same.
        module subroutine probit_fit_f64(values, loc, sigma, corr, is_valid, skipnan, method, &
                n_null, n_nan, ok, threads)
            real(real64), intent(in) :: values(:) !! the population, before exclusions.
            real(real64), intent(out) :: loc
            !! the INTERCEPT of the line: a location estimate. Every one of the six plotting
            !! positions is symmetric about the median, so the normal scores sum to zero and this
            !! is the MEAN of the surviving values -- computed as the mean, not as a regression
            !! intercept that ought to equal one. It is the mean rather than `pf_mean`'s last
            !! bit: this is a plain sum over the sorted survivors and `pf_mean` refines its
            !! quotient, so compare the two with a tolerance. NaN when fewer than two survive.
            real(real64), intent(out) :: sigma
            !! the SLOPE of the line: a scale estimate that reads the whole sample rather than
            !! two quantiles of it, and is far less disturbed by a heavy tail than `pf_stddev`.
            !! Exactly `0` for a constant population; NaN when fewer than two elements survive.
@@pfit_opts@@
        end subroutine probit_fit_f64
        !> Resolves a plotting-position `method=` token, or aborts naming all six.
        !!
        !! **Declared here so that ONE resolver serves two submodules.** `pf_normal_scores` lives
        !! in `parquet_stats_relate` and `pf_probit_fit` in `parquet_stats_order`, sibling
        !! submodules that cannot reach each other's contained procedures -- and the alternative
        !! is two copies of the same six-token `select case`, which is how `method="cunnane"`
        !! comes to mean two different constants in one library. Implemented in
        !! `parquet_stats_relate`, the same "cross-subtree private-helper interfaces" mechanism
        !! `stats_compact` uses in the other direction.
        module subroutine nscore_method(what, method, a, filliben)
            character(len=*), intent(in) :: what !! the public procedure's name, for messages.
            character(len=*), intent(in), optional :: method !! the caller's token, if any.
            real(real64), intent(out) :: a !! the plotting-position constant `a`.
            logical, intent(out) :: filliben !! .true. for Filliben's median rank, which is not
            !! of the `a`-family at all and so cannot come back as one.
        end subroutine nscore_method
        !> The probability a rank marks: what `Phi**(-1)` is actually asked for.
        !!
        !! Shared for `nscore_method`'s reason, and the pairing matters more than either half:
        !! a token and the formula it selects that lived in different submodules could resolve
        !! consistently and still be applied differently.
        pure module function nscore_position(r, m, a, den, filliben) result(p)
            real(real64), intent(in) :: r !! the rank, 1-based; a half-integer for a midrank.
            integer(int64), intent(in) :: m !! how many elements survived.
            real(real64), intent(in) :: a !! the plotting-position constant.
            real(real64), intent(in) :: den !! `m + 1 - 2a`, formed once by the caller.
            logical, intent(in) :: filliben !! .true. for Filliben's median rank.
            real(real64) :: p !! the probability; never 0 and never 1, so no clamp is needed.
        end function nscore_position"""

CUM_IFACES = """        !> `pf_cumsum` over a 64-bit real array: the running sum, element by element.
        module subroutine cumsum_f64(values, out, is_valid, skipnan, out_valid, n_null, n_nan, ok)
            real(real64), intent(in) :: values(:) !! the population, in the order it is to be scanned.
            real(real64), intent(out) :: out(:) !! the running value, same size as `values`.
@@cum_opts@@
        end subroutine cumsum_f64
        !> `pf_cumprod` over a 64-bit real array: the running product.
        module subroutine cumprod_f64(values, out, is_valid, skipnan, out_valid, n_null, n_nan, ok)
            real(real64), intent(in) :: values(:) !! the population, in the order it is to be scanned.
            real(real64), intent(out) :: out(:) !! the running value, same size as `values`.
@@cum_opts@@
        end subroutine cumprod_f64
        !> `pf_cummax` over a 64-bit real array: the running maximum.
        module subroutine cummax_f64(values, out, is_valid, skipnan, out_valid, n_null, n_nan, ok)
            real(real64), intent(in) :: values(:) !! the population, in the order it is to be scanned.
            real(real64), intent(out) :: out(:) !! the running value, same size as `values`.
@@cum_opts@@
        end subroutine cummax_f64
        !> `pf_cummin` over a 64-bit real array: the running minimum.
        module subroutine cummin_f64(values, out, is_valid, skipnan, out_valid, n_null, n_nan, ok)
            real(real64), intent(in) :: values(:) !! the population, in the order it is to be scanned.
            real(real64), intent(out) :: out(:) !! the running value, same size as `values`.
@@cum_opts@@
        end subroutine cummin_f64"""

BIN_IFACES = """        !> `pf_bucketize` over a 64-bit real array: which bin each value falls in.
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
@@bucketize_opts@@
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
@@histogram_opts@@
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
@@bin_edges_opts@@
        end subroutine bin_edges_f64
        !> `pf_bin_linear` over a 64-bit real array: each value's weight split between the two grid
        !! points around it.
        !!
        !! Every other specific widens its values to `real64` and calls this one, so the split is
        !! written once and the kinds cannot disagree about it.
        module subroutine bin_linear_f64(values, grid, mass, is_valid, weights, skipnan, n_null, &
                n_nan, n_outside, ok)
            real(real64), intent(in) :: values(:) !! the values to deposit.
            real(real64), intent(in) :: grid(:) !! the grid POINTS: finite, strictly increasing, at least two.
            real(real64), intent(out) :: mass(:)
            !! the weight deposited on each grid point, so `size(grid)` entries -- one per POINT,
            !! where `pf_histogram`'s `counts` has one per interval.
@@bin_linear_opts@@
        end subroutine bin_linear_f64"""

#: What each P9 generic's own doc-comment says, on the generic's page.
CUM_DOC = {
    "pf_cumsum": [
        "The running sum of a population -- pandas' `Series.cumsum()`.",
        "",
        "**The last element is NOT `pf_sum(values)`, and that is arithmetic rather than a bug.**",
        "A running sum is sequential by definition, so it accumulates left to right; `pf_sum` is",
        "PAIRWISE over a fixed block tree. The two agree to within the accumulated rounding of a",
        "naive sum, which on a large or badly scaled population is several ulps and occasionally",
        "more. Compare them with a tolerance, or take the total from `pf_sum`.",
        "",
        "**An `integer(int64)` population widens to `real64` first**, so a value above `2**53` is",
        "rounded on the way in and the running sum inherits that -- see `pf_sum` for the bound",
        "and for what to do instead. int32 is unaffected."],
    "pf_cumprod": [
        "The running product of a population -- pandas' `Series.cumprod()`.",
        "",
        "Formed as a product and not in log space, deliberately: unlike `pf_gmean` this has to",
        "reproduce every intermediate value, including the sign, so a magnitude that overflows to",
        "infinity is the honest answer rather than something to be routed around."],
    "pf_cummax": [
        "The running maximum of a population -- pandas' `Series.cummax()`.",
        "",
        "The comparison is IEEE `>`, so a negative zero and a positive zero compare equal and the",
        "first of them is kept -- which is what numpy and pandas do."],
    "pf_cummin": [
        "The running minimum of a population -- pandas' `Series.cummin()`.",
        "",
        "The comparison is IEEE `<`, so a negative zero and a positive zero compare equal and the",
        "first of them is kept -- which is what numpy and pandas do."],
}

#: The tail the four cumulative generics share. Their null rule is NOT the reduction family's and
#: has to be stated rather than inherited, which is why this text exists separately.
_CUM_COMMON = """    !>
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
    !> mistake worth naming rather than leaving to be discovered."""

BIN_DOC = {
    "pf_bucketize": [
        "Which bin of a sorted edge array each value falls in -- pandas' `pd.cut`.",
        "",
        "The primitive behind a histogram, a grouped aggregation over ranges, and every",
        "hand-written `if (x < a) then ... else if (x < b)` ladder. `codes(i)` is the 1-based",
        "number of the bin holding `values(i)`, or **0** when it joined none: excluded by",
        "`is_valid`, a NaN, carrying a zero weight, or outside `[edges(1), edges(nbins+1)]`.",
        "`n_null`, `n_nan` and `n_outside` separate three of those four exactly; a zero-weighted",
        "element is deliberately in none of them and leaves `ok` alone, exactly as it does in",
        "`pf_histogram`, because it was removed from the POPULATION rather than failing to reach",
        "a bin. `pf_count_valid` over the same arguments is what reports it.",
        "",
        "The search is binary, so the cost is `O(n log nbins)` and an edge array with thousands",
        "of bins is as cheap as one with four."],
    "pf_histogram": [
        "How much of a population falls in each bin -- `np.histogram` with explicit `bins=`.",
        "",
        "`counts(k)` is the number of values in bin k, or the SUM OF THEIR WEIGHTS when `weights`",
        "is present, which is what `np.histogram(weights=)` returns and why the output is real",
        "rather than integer. Values outside the edge range join no bin and are reported through",
        "`n_outside`; numpy drops them silently, so this is strictly more information.",
        "",
        "This is `pf_bucketize` followed by a tally, and the two agree by construction -- the",
        "count in bin k is the total WEIGHT of the `codes` equal to k over the same arguments,",
        "which is why `pf_bucketize` takes `weights` even though a weight cannot change a bin",
        "number. Unweighted, that total weight is a count."],
}

BIN_DOC["pf_bin_edges"] = [
    "`nbins` equal-width bins spanning a population -- `np.histogram_bin_edges(bins=n)`.",
    "",
    "The half of `np.histogram(bins=10)` this library does not fold into `pf_histogram`: it finds",
    "the range and lays `nbins + 1` boundaries across it, ready to hand straight to",
    "`pf_histogram` or `pf_bucketize`. The population is the same one they would bin -- nulls,",
    "NaNs and zero-weight elements leave it by the module's usual rules -- so a zero-weighted",
    "extreme value does not stretch the range over data that will not be counted.",
    "",
    "**The edges are ALWAYS strictly increasing, which is a contract rather than an**",
    "**observation**: they exist to be passed to `pf_histogram`, which aborts on a pair that is",
    "not, so a degenerate population must not produce edges that abort one call later.",
    "`ok = .false.` says the edges do not describe the data's own range, and there are four",
    "ways to get it -- an EMPTY population, which falls back to `[0, 1]` as numpy does; a",
    "CONSTANT one, which widens to `[x - 0.5, x + 0.5]`, also as numpy does; an `nbins` finer",
    "than double precision can resolve over the range, where the spacing would collapse and",
    "neighbouring edges are nudged apart instead; and a population holding an infinity, or a",
    "NaN kept by `skipnan = .false.`, whose edges span its finite values alone. In all four the",
    "edges are still usable.",
    "",
    "**There is no explicit range pair.** A caller who already knows the bounds can write the",
    "`nbins + 1` values directly; what is worth a procedure is finding the range under this",
    "module's exclusion rules, which is the part that is easy to get subtly wrong.",
    "",
    "`nbins` is a plain default-kind `integer` and has no int64 form. It is bounded by the size",
    "of `edges`, so it can never legitimately exceed `huge(1_int32)`."]

#: The tail the two binning generics share.
_BIN_COMMON = """    !>
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
    !> generic resolving on `values` alone."""

#: The binning family's `ok` and `skipnan` tags, in its own words. The module-wide ones describe a
#: REDUCTION -- an undefined statistic comes back a quiet NaN, partial nullness is no failure, and a
#: kept NaN makes every answer NaN -- and none of that is what a binner does: `pf_bucketize` and
#: `pf_histogram` report every element that failed to reach a bin, nulls included, and a kept NaN
#: reaches no bin rather than poisoning one; `pf_bin_edges` always returns usable edges, and its
#: `ok` says whether they span the population's own range. Each tag restates its procedure's body.
BUCKETIZE_D = dict(D)
BUCKETIZE_D["skipnan"] = """            logical, intent(in), optional :: skipnan
            !! .true. (the default) excludes a NaN from the population and counts it in `n_nan`;
            !! .false. keeps it as a value, which matches no bin and is counted in `n_outside`
            !! instead. Either way a NaN reaches no bin: the argument decides only which count it
            !! lands in."""
BUCKETIZE_D["ok"] = """            logical, intent(out), optional :: ok
            !! .false. when any element failed to reach a bin -- a null, a NaN or a value outside
            !! the edges, which `n_null`, `n_nan` and `n_outside` separate. A zero-weighted element
            !! left the population rather than failing, so it leaves this alone."""
HISTOGRAM_D = dict(BUCKETIZE_D)
HISTOGRAM_D["ok"] = """            logical, intent(out), optional :: ok
            !! .false. when any element failed to reach a bin -- a null, a NaN or a value outside
            !! the edges, which `n_null`, `n_nan` and `n_outside` separate. A zero-weighted element
            !! left the population rather than failing, so it leaves this alone. With `density`,
            !! also .false. when nothing was binned, the density then being undefined."""
EDGES_D = dict(D)
EDGES_D["skipnan"] = """            logical, intent(in), optional :: skipnan
            !! .true. (the default) excludes a NaN from the population and counts it in `n_nan`;
            !! .false. keeps it in the population, where, like an infinity, it is left out of the
            !! range the edges span and turns `ok` false. Either way the edges stay finite and
            !! strictly increasing."""
EDGES_D["ok"] = """            logical, intent(out), optional :: ok
            !! .false. when the edges do not span the population's own range: it is empty (the
            !! edges are then `[0, 1]` split `nbins` ways) or constant (widened by 0.5 either side),
            !! `nbins` is finer than double precision resolves over it (neighbouring edges nudged
            !! apart), or it holds an infinity or a NaN kept by `skipnan = .false.` (the edges span
            !! its finite values). The edges are strictly increasing and usable in every case. A
            !! null or a zero-weighted element affects this only by leaving the population empty."""
#: Which tag table each `BIN_FAMILY` procedure's per-kind interfaces read.
BIN_TAGS = {"bucketize": BUCKETIZE_D, "histogram": HISTOGRAM_D}

#: What `pf_bin_linear`'s own doc-comment says, on the generic's page. It carries no share of
#: `_BIN_COMMON`, which says two things that are false here: that an infinite outer edge is
#: allowed, and that a `logical` array is accepted.
BIN_LINEAR_DOC = [
    "Deposits each value's weight onto the two grid points around it, in proportion to how near",
    "it lies to each -- linear binning, which particle-mesh and power-spectrum work calls",
    "cloud-in-cell (CIC) assignment.",
    "",
    "A value `v` in the cell between `grid(i)` and `grid(i+1)` lies a fraction",
    "`t = (v - grid(i)) / (grid(i+1) - grid(i))` of the way across it, and gives `w*(1 - t)` of",
    "its weight `w` to `grid(i)` and `w*t` to `grid(i+1)`, so a value exactly on a grid point",
    "gives that point everything. `mass(k)` is the total deposited on `grid(k)`; unweighted,",
    "every `w` is 1. This is the mass-conserving adjoint of linear interpolation:",
    "`pf_interp_1d` reads a grid at a point, and this writes a point onto a grid.",
    "",
    "**The two shares of one value sum to its weight EXACTLY**, not to within a rounding: the",
    "larger share is the product and the smaller is what the weight leaves, and that subtraction",
    "is exact in floating point. Nothing is lost or created in the split, so `sum(mass)` differs",
    "from the total weight deposited only by the rounding of the additions into each grid point.",
    "",
    "**`grid` holds POINTS, not bin edges**, which is the difference from `pf_histogram` a caller",
    "will get wrong: `mass` has `size(grid)` entries, one per point, where `counts` has",
    "`size(edges) - 1`, one per interval. There is no `right=` -- a value exactly on an interior",
    "point gives it everything whichever cell it is taken to be in -- and no `density=`, because",
    "the two end points have no defined outer half-cell. A caller who wants a density divides by",
    "`sum(mass)` and by their own spacing.",
    "",
    "`grid` must hold at least two points and be **strictly increasing** and **finite**, or the",
    "call aborts naming the offending index. **An infinite point is refused**, although",
    "`pf_histogram` accepts an infinite outer edge: an open-ended bin is meaningful, a share of",
    "the distance to an infinite point is not. Two neighbours so far apart that their difference",
    "overflows a `real64` abort too, since the fraction `t` cannot be formed between them, and so",
    "does a NaN point.",
    "",
    "The optional arguments are `pf_histogram`'s and mean what they mean there. `is_valid`",
    "marks nulls; `weights` are non-negative, a zero weight removing the element and a negative,",
    "NaN or infinite one aborting; `skipnan` (`.true.` by default) excludes a NaN. `n_null`,",
    "`n_nan` and `n_outside` count the elements that reached no grid point, one count per reason,",
    "and `ok` is `.false.` when any element did. Nothing here aborts on a data condition: a value",
    "outside `[grid(1), grid(size(grid))]`, a NaN, a null and an empty population are all",
    "reported through those counts. A sample with nothing in it, or whose every weight is zero,",
    "leaves `mass` all zeros with `ok` `.true.`; an all-null one leaves `mass` all zeros too, but",
    "its nulls are counted, so `ok` is `.false.`. A zero-weighted element is in none of the counts",
    "and leaves `ok` alone, as in `pf_histogram`: it left the population rather than failing to",
    "land.",
    "",
    "`values` may be a `real(real64)`, `real(real32)`, `integer(int32)` or `integer(int64)`",
    "array, or a scalar numeric `type(parquet_column)`. **There is no `logical` form**:",
    "depositing `.true.` and `.false.` onto a continuous grid answers no question, so a logical",
    "array does not compile and a logical column aborts naming its kind. `grid` and `mass` are",
    "always `real(real64)`. The search is binary, so the cost is `O(n log size(grid))`, in one",
    "pass over the values."]

#: The argument tags `pf_bin_linear` needs in its own words. Three of the module-wide ones describe
#: something else here: `n_outside`'s names edges and bins, `ok`'s speaks of an undefined
#: statistic, and `skipnan`'s says a kept NaN makes every answer NaN -- a kept NaN here is
#: deposited nowhere and poisons nothing.
BIN_LINEAR_D = dict(D)
BIN_LINEAR_D["skipnan"] = """            logical, intent(in), optional :: skipnan
            !! .true. (the default) excludes a NaN from the population and counts it in `n_nan`;
            !! .false. keeps it as a value, which lies between no two grid points and is counted
            !! in `n_outside` instead. Either way a NaN is deposited nowhere."""
BIN_LINEAR_D["n_outside"] = """            integer(int64), intent(out), optional :: n_outside
            !! how many usable elements fell outside `[grid(1), grid(size(grid))]` and so were
            !! deposited nowhere. An ordinary data condition rather than an error, but the one
            !! thing a caller cannot recover from `mass`, so it is reported. An infinity is
            !! counted here, and so is a NaN under `skipnan = .false.`."""
BIN_LINEAR_D["ok"] = """            logical, intent(out), optional :: ok
            !! .false. when any element failed to be deposited -- a null, a NaN or a value outside
            !! the grid, which `n_null`, `n_nan` and `n_outside` separate. A zero-weighted element
            !! left the population instead and leaves this alone. A sample with nothing in it is
            !! `.true.`: depositing nothing is well defined, and `mass` is then all zeros."""

#: The guard `pf_bin_linear`'s column form runs before widening. `col_to_real64` widens a logical
#: column to 0s and 1s, which every other generic here accepts; this one has no logical form, and
#: without the guard the column form would quietly deposit the 0s and 1s the absent array form
#: exists to refuse. It runs before the widening so that nothing is copied first.
BIN_LINEAR_LOGICAL_COLUMN = [
    "        ! `col_to_real64` widens a logical column to 0s and 1s, which every other generic here",
    "        ! accepts. This one has no logical form, so its column form refuses the kind by name.",
    "        if (values%kindof() == PK_LOGICAL) &",
    '            error stop "pf_bin_linear: a logical column has no position on a grid to be split " // &',
    '                "between two points; pf_bin_linear accepts int32, int64, float32 and float64 columns"',
]

#: What each P8 generic's own doc-comment says, on the generic's page.
RELATE_DOC = {
    "pf_cov": [
        "The covariance of two samples -- pairwise-complete, as pandas is.",
        "",
        "A pair enters the population only when BOTH elements are usable: neither null, neither",
        "NaN, and the pair's weight non-zero. `n_null` counts pairs dropped because either side",
        "was null and `n_nan` those dropped for a NaN and not already null, so the two counts",
        "describe pairs rather than elements -- which is the only thing they could describe, since",
        "a covariance between vectors of different lengths is not a number.",
        "",
        "`ddof = 1` by default (the sample covariance, as pandas returns); pass `ddof = 0` for",
        "numpy's. `pf_cov(x, x)` is exactly `pf_variance(x)` at the same `ddof`, and the test",
        "suite asserts it.",
        "",
        "**Both arrays must be the same kind.** Six specifics rather than thirty-six: a caller",
        "mixing kinds writes `real(x, real64)`, which is one call and says what it does."],
    "pf_corr": [
        "The correlation of two samples: Pearson by default, Spearman on request.",
        "",
        "`method=\"pearson\"` is `cov(x, y) / (sd(x) sd(y))`. `method=\"spearman\"` is the same",
        "formula over MIDRANKS -- each run of equal values receives the mean of the sorted",
        "positions it spans -- so it measures any monotone relationship rather than a linear one.",
        "",
        "Pairwise-complete, exactly as `pf_cov` is, and with the same pair-counting rule. There is",
        "deliberately **no `ddof`**: the one in the covariance and the two in the standard",
        "deviations cancel exactly, so the argument could never change the answer.",
        "",
        "`pf_corr(x, x)` is exactly `1`, and `pf_corr(x, -x)` exactly `-1`, for any finite",
        "non-constant sample, under either method and at any SCALE: both are answered from the",
        "pairing itself rather than computed, so neither depends on the centred sums being",
        "representable. A constant sample has zero variance and gives a quiet NaN with",
        "`ok = .false.`, and so does one holding an infinity, which leaves the whole pair",
        "undefined."],
    "pf_zscore": [
        "Standardises a population in place of the caller's own loop -- `scipy.stats.zscore`.",
        "",
        "`z(i) = (values(i) - mean) / stddev`, with `ddof = 1` by default. `z` is the same size as",
        "`values` and the mean and standard deviation are taken over the elements that survive the",
        "exclusion rules.",
        "",
        "**An excluded element has no standardised value**, and there are two ways to learn which:",
        "`out_valid(:)` marks them exactly, and without it they are written as quiet NaNs. `ok`",
        "comes back `.false.` in either case, and also when the population's variance is zero --",
        "in which case EVERY output is NaN. `n_null` separates the two: it is `0` for the second.",
        "",
        "The result has mean 0 and standard deviation 1 over the surviving elements, to a few ulp."],
    "pf_normal_scores": [
        "Replaces each value by the normal quantile its RANK marks -- rankits, normal scores, the",
        "van der Waerden transform; the x-axis of a Q-Q plot against a Gaussian.",
        "",
        "`s(i) = Phi**(-1)((r(i) - a)/(m + 1 - 2a))`, where `r(i)` is `values(i)`'s MIDRANK among",
        "the `m` elements that survive the exclusion rules -- each run of equal values receiving",
        "the mean of the sorted positions it spans, exactly as `pf_corr(method=\"spearman\")`",
        "does -- and `a` comes from `method=`. `s` is the same size as `values` and is in the",
        "caller's original order.",
        "",
        "**The result depends on the input only through its ORDER**, so it is unchanged, bit for",
        "bit, by any strictly monotone transform of the values: `pf_normal_scores(x)` and",
        "`pf_normal_scores(exp(x))` are the same array. Ranks are taken over the SURVIVORS, so",
        "excluding a quarter of the population gives the remaining elements exactly the scores",
        "they would have had as a population of their own.",
        "",
        "**An excluded element has no score**, and there are two ways to learn which: `out_valid`",
        "marks them exactly, and without it they are written as quiet NaNs -- `pf_zscore`'s rule",
        "verbatim. `ok` is `.false.` in either case. A one-element population scores that element",
        "`Phi**(-1)(1/2) = 0`, which is the correct degenerate answer rather than a failure.",
        "",
        "There is deliberately **no `weights`** and **no `pf_stats` binding**: a weighted midrank",
        "is a definitional choice no reference library makes, and tier B sorts in place and never",
        "hands values back in the caller's order."],
    "pf_probit_fit": [
        "The least-squares line of a NORMAL-PROBABILITY PLOT: a location, a scale, and how",
        "straight the plot is -- `scipy.stats.probplot(x, fit=True)`.",
        "",
        "Sort the survivors, give order statistic `r` the normal score of its plotting position",
        "`(r - a)/(m + 1 - 2a)`, and fit `y = loc + sigma*Phi**(-1)(pp)` by least squares. `loc`",
        "is the intercept, `sigma` the slope, and the optional `corr` is the correlation",
        "coefficient of that fit -- the probability-plot correlation coefficient, which is a",
        "normality diagnostic and costs nothing extra.",
        "",
        "**Why reach for this rather than `pf_mean` and `pf_stddev`?** The slope reads the whole",
        "sample through its order statistics, so a few wild points move it far less than they",
        "move a standard deviation, while a clean Gaussian sample gives all three of `sigma`,",
        "`pf_stddev` and `pf_probit_scale` the same answer to within sampling error. And `corr`",
        "says whether the Gaussian assumption behind that reading holds at all.",
        "",
        "**The ranks are ORDINAL here, not the midranks `pf_normal_scores` uses**, which is what",
        "makes this scipy's `probplot` and what keeps the plotting positions symmetric about the",
        "median -- so their normal scores sum to zero and `loc` is the MEAN of the survivors,",
        "evaluated as a mean rather than as a regression intercept. Ties therefore take DIFFERENT",
        "plotting positions at equal `y`, which is the same line either way and is what the",
        "reference implementation does.",
        "",
        "`method=` chooses the plotting position from `pf_normal_scores`' six tokens -- `\"blom\"`",
        "(the default), `\"weibull\"`, `\"tukey\"`, `\"hazen\"`, `\"cunnane\"` or `\"filliben\"`.",
        "",
        "**`method=\"filliben\"` is what reproduces `scipy.stats.probplot`**, whose plotting",
        "positions are the median ranks rather than this generic's default -- and under that token",
        "`corr` is Filliben's statistic exactly. The default here is `\"blom\"`, which is the",
        "position `pf_normal_scores` defaults to and the one the literature quotes; the two differ",
        "in the third digit of `sigma` on a thousand points, so a comparison against scipy has to",
        "name the token.",
        "",
        "Fewer than **two** survivors gives three NaNs with `ok = .false.` -- a line needs two",
        "points. A CONSTANT population gives `sigma = 0`, `loc` equal to that constant, and `corr`",
        "a NaN, since a constant has no variance to correlate; `ok` is `.false.` there only if",
        "`corr` was asked for.",
        "",
        "There is deliberately **no `weights`**, for `pf_normal_scores`' reason: a weighted",
        "plotting position is a definitional choice no reference library makes."],
    "pf_sigma_clipped_stats": [
        "astropy's `sigma_clipped_stats`: iteratively drop the outliers, then summarise the rest.",
        "",
        "Each round computes a centre and a scale over the values still surviving and keeps",
        "`centre - sigma_lower*scale <= x <= centre + sigma_upper*scale`, inclusive at both ends,",
        "as astropy does. `cenfunc=` chooses the centre (`\"median\"` by default, or `\"mean\"`)",
        "and `stdfunc=` the scale (`\"std\"` by default, or `\"mad_std\"`, which is far less",
        "disturbed by the outliers being clipped). `sigma = 3.0` and `maxiters = 5` are astropy's",
        "defaults.",
        "",
        "**The whole run costs ONE ordering.** The keep condition is an interval, so the survivors",
        "are always a contiguous sub-range of the sorted order and each round only moves two",
        "indices inward.",
        "",
        "`maxiters <= 0` iterates until a round removes nothing, capped at 100 rounds. Reaching",
        "that cap stops and reports through `converged = .false.`; it does NOT abort, because each",
        "non-terminating round removes at least one point and a large catalogue can honestly want",
        "more rounds than any small constant.",
        "",
        "**A round that would leave fewer than two values stops instead**, and the previous round's",
        "survivors are what is reported. An empty population gives three NaNs with `ok = .false.`",
        "and `n_clipped = 0`.",
        "",
        "`keep(:)` optionally returns the surviving mask over the ORIGINAL array, which is what a",
        "caller needs in order to apply the same clip to a second column.",
        "",
        "**There is no `weights` argument**, which is stronger than refusing one at run time: a",
        "weighted scale estimator is a further definitional choice that no workload in evidence",
        "asks for, and its absence makes passing one a compile error."],
}

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
    ("probit_scale", [], "sigma", "real(real64), intent(out) :: sigma",
     "the scale a symmetric quantile pair implies for Gaussian data; NaN when the population is "
     "empty, and `0` for a constant one.", PSCALE_OPTS),
    ("describe", [], "s", "type(pf_stats), intent(out) :: s",
     "the filled summary object: tier A computed and tier B already ordered, so every query on "
     "it afterwards is a read.", DESCRIBE_OPTS),
]


#: The tail the four P8 generics share. Shorter than `_ORDER_COMMON`'s, because two of the four
#: are pairwise and one is vector-valued, so a paragraph written for a scalar reduction over one
#: population would be wrong for three of them.
_RELATE_COMMON = """    !>
    !> Nulls, NaNs and zero-weight elements leave the population exactly as they do for the
    !> moments. An undefined answer is a **quiet NaN** with `ok = .false.`, never an abort: this
    !> module aborts on misuse and never on a data condition.
    !>
    !> Every argument may be a `real(real64)`, `real(real32)`, `integer(int32)`, `integer(int64)`
    !> or `logical` array, or a scalar numeric `type(parquet_column)`."""

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
     "a 32-bit integer array",
     "integer(int32), allocatable, intent(out) :: modes(:)"),
    ("i64", "integer(int64), intent(in) :: values(:) !! the population.",
     "integer(int64), intent(out) :: m", "the modal value; unchanged when the population is empty.",
     "a 64-bit integer array",
     "integer(int64), allocatable, intent(out) :: modes(:)"),
    ("bool", "logical, intent(in) :: values(:) !! the population.",
     "logical, intent(out) :: m",
     "the modal value; unchanged when the population is empty. `.false.` sorts below `.true.`, "
     "so an even split answers `.false.`.",
     "a logical array",
     "logical, allocatable, intent(out) :: modes(:)"),
    ("chr", "character(len=*), intent(in) :: values(:) !! the population; each element is trimmed.",
     "character(len=:), allocatable, intent(out) :: m",
     "the modal value, allocated to its own trimmed length. **Left unallocated when the "
     "population is empty**, which is the one shape in this module where `ok = .false.` and an "
     "unallocated result coincide -- test `ok`, or `allocated(m)`, before reading it.",
     "a character array",
     "character(len=:), allocatable, intent(out) :: modes(:)"),
    ("str", "type(parquet_string_column), intent(in) :: values !! the population.",
     "character(len=:), allocatable, intent(out) :: m",
     "the modal value, allocated to its own length. Left unallocated when the population is "
     "empty.",
     "a `parquet_string_column`",
     "character(len=:), allocatable, intent(out) :: modes(:)"),
]

#: `modes`' shared doc-comment. Per-kind only in its DECLARATION -- the element type is the
#: result's -- so the prose is written once here and the type line comes from the kind row.
MODES_DOC = [
    "**every** modal value, in ascending order, allocated to exactly the number that tie --",
    "which is pandas' `Series.mode()`, where `m` alone is scipy's `stats.mode`. `m` is always",
    "`modes(1)`, since both take the smallest of a tie, so asking for both costs one extra",
    "gather and no second pass. Allocated to size 0 for an empty population, never left",
    "unallocated: `size(modes)` is then the only test a caller needs, and an unallocated",
    "result would make it undefined behaviour instead. For the two character forms every",
    "element shares one length, which is the LONGEST tied value's -- a shorter one is",
    "blank-padded, so compare with `trim()`.",
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
    "**`modes` gives every tied value instead of just the smallest**, in ascending order, which",
    "is what pandas' `Series.mode()` returns. It is allocated to exactly the number that tie, so",
    "`size(modes)` says how many there were and `modes(1)` is always `m`. A population with one",
    "clear winner gives a one-element array, and an empty one gives a zero-length array rather",
    "than an unallocated result.",
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


#: `method`'s doc for `pf_corr`, whose token names a correlation rather than an interpolation.
#: An override rather than a second entry in the canonical sequence -- the DUMMY is still `method`
#: in the same slot, which is what `check_stats_optional_argument_order` reads.
#: `pf_normal_scores`' own `method` and `out_valid`. `method` here selects a PLOTTING POSITION,
#: which is a different thing entirely from the quantile-interpolation `method` the order family
#: takes and from `pf_corr`'s pearson/spearman -- three unrelated vocabularies behind one argument
#: name, so each spells its own out in full rather than sharing a sentence.
NSCORE_D = dict(D)
NSCORE_D["method"] = """            character(len=*), intent(in), optional :: method
            !! the PLOTTING POSITION: which point of the distribution a rank is taken to mark.
            !! `s(i) = Phi**(-1)((r - a)/(m + 1 - 2a))` for a midrank `r` among `m` survivors,
            !! with `a` given by the token: "blom" (the default, `a = 3/8`, the standard rankit
            !! and what R's `qqnorm` uses), "weibull" (`a = 0`, `r/(m+1)`, the van der Waerden
            !! score), "tukey" (`a = 1/3`), "hazen" (`a = 1/2`) or "cunnane" (`a = 0.4`).
            !! "filliben" is the MEDIAN rank and not of that family at all: `1 - 0.5**(1/m)` at
            !! the first position, `0.5**(1/m)` at the last and `(i - 0.3175)/(m + 0.365)`
            !! between. Any other token aborts, naming all six. Matched case-insensitively."""
NSCORE_D["out_valid"] = """            logical, intent(out), optional :: out_valid(:)
            !! per output element: .false. where the input was excluded and the element therefore
            !! has no score. Same size as `values`. **Absent, such an element is written as a
            !! quiet NaN instead** -- which is loud in practice, since it propagates through
            !! whatever the caller does next."""
NSCORE_D["threads"] = """            integer, intent(in), optional :: threads
            !! how many threads the midrank ordering may use, passed straight to `pf_argsort`.
            !! Absent takes the automatic rule. **The answer does not depend on this argument**:
            !! ties take the mean of the positions they span, so no tie-breaking the sort might
            !! choose can reach the result. It is a speed control and never an accuracy one."""
CORR_D = dict(D)
CORR_D["method"] = """            character(len=*), intent(in), optional :: method
            !! "pearson" (the default) or "spearman". Spearman is Pearson over MIDRANKS: each run
            !! of equal values receives the mean of the sorted positions it spans, so it measures
            !! any monotone relationship rather than a linear one. Any other token aborts, naming
            !! both. Matched case-insensitively. **`weights` with "spearman" aborts** -- a
            !! weighted midrank is a further definitional choice and no reference library makes
            !! it."""
CORR_D["weights"] = """            real(real64), intent(in), optional :: weights(:)
            !! per PAIR weight, non-negative. A ZERO weight removes the pair from the population;
            !! a negative, NaN or infinite one aborts. Absent means every weight is 1. Pearson
            !! only -- see `method`."""
CORR_D["is_valid"] = """            logical, intent(in), optional :: is_valid(:)
            !! per PAIR: .false. marks the pair as unusable. One mask over both arrays, because a
            !! two-sample statistic is taken over pairs; a caller holding a separate mask for each
            !! column passes `mask_x .and. mask_y`, which says what it does. A NaN on either side
            !! drops the pair as well, and is counted through `n_nan` rather than here."""


#: The ORDER family's `threads` doc. The shared `D["threads"]` text names the central-moment pass,
#: which `pf_median` and its siblings do not have -- their `threads=` reaches `pf_argsort`. Two
#: texts rather than one vaguer one, because the reproducibility ARGUMENT differs: the moments are
#: reproducible because the block tree is a function of the population size, and an order statistic
#: is reproducible because a sort is a permutation.
ORDER_D = dict(D)
ORDER_D["threads"] = """            integer, intent(in), optional :: threads
            !! how many threads the ORDERING may use -- this is an order statistic and has no
            !! central-moment pass, so the work `threads=` divides is `pf_argsort`'s. Absent takes
            !! the automatic rule: the `sort_threads` setting, capped by the processors
            !! actually available and by a measured work floor, and 1 inside a caller's own
            !! parallel region. **The answer does not depend on this argument** -- a sort is a
            !! permutation and the selection off it is exact, so 1, 8 and a build with no OpenMP
            !! at all return the same bits. It is a speed control and never an accuracy one."""

#: `pf_describe` alone does both, so it gets its own text rather than either of the other two.
DESCRIBE_D = dict(D)
DESCRIBE_D["threads"] = """            integer, intent(in), optional :: threads
            !! how many threads the central-moment pass and the ordering may use -- `pf_describe`
            !! does both, which no other entry point in this module does. Absent takes the
            !! automatic rule: the `sort_threads` setting, capped by the processors
            !! actually available and by a measured work floor, and 1 inside a caller's own
            !! parallel region. **The answer does not depend on this argument** -- the block
            !! decomposition is a function of the population size alone and a sort is a
            !! permutation, so 1, 8 and a build with no OpenMP at all return the same bits. It is
            !! a speed control and never an accuracy one."""


#: `pf_moments`' and `pf_describe`'s own `ok` texts. Both are aggregate entry points, so the shared
#: "the result is then a quiet NaN" wording does not say which result.
MOMENTS_D = dict(D)
MOMENTS_D["ok"] = """            logical, intent(out), optional :: ok
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
            !! variance -- genuinely a NaN there -- turns it .false. if it was asked for."""

DESCRIBE_D["ok"] = """            logical, intent(out), optional :: ok
            !! .false. when the object describes a population with no defined moments -- one that
            !! is empty after the exclusions, or that kept a NaN under `skipnan = .false.` In
            !! either case every tier-A query on `s` answers a quiet NaN. The object itself
            !! carries the counts (`%n_valid()`, `%n_null()`, `%n_nan()`), so this is the one
            !! thing about the result a caller cannot otherwise read without picking a statistic
            !! and NaN-testing it by hand."""


#: `pf_probit_fit`'s doc map. Its `method=` names a PLOTTING POSITION -- the same six tokens
#: `pf_normal_scores` takes -- and not an interpolation rule, so it borrows `NSCORE_D`'s text.
#: **`pf_probit_scale` keeps `ORDER_D`'s**, and the two really do mean different things by one
#: argument name: that procedure is two `pf_quantile` probes off one ordering, so its `method=` is
#: `pf_quantile`'s. The guide says so in both sections, because a caller who assumes otherwise
#: gets a plausible number rather than an abort.
PFIT_D = dict(ORDER_D)
PFIT_D["method"] = NSCORE_D["method"]


def order_dmap(base):
    """Which `threads` doc an ORDER_FAMILY row gets: `pf_describe` alone does both passes."""
    return DESCRIBE_D if base == "describe" else ORDER_D


def pair_iface(base, out_name, out_doc, opts, tag, decl, kindword, doc):
    """One interface body for a two-sample procedure's per-kind entry point."""
    args = wrap_args(["x", "y", out_name] + opts)
    lines = ["        !> `pf_%s` over %s." % (base, kindword)]
    for line in doc:
        lines.append(("        !> " + line).rstrip())
    lines.append("        module subroutine %s_%s(%s)" % (base, tag, args))
    # Both arrays are the SAME kind. Thirty-six mixed-kind specifics would be absurd for a
    # question a caller answers with one `real(x, real64)`, and the restriction is a compile
    # error rather than anything subtle.
    for who, what in (("x", "the first sample."), ("y", "the second sample, element for element.")):
        lines.append("            " + decl.strip().replace("values", who).split("!!")[0].rstrip()
                     + " !! " + what)
    lines.append("            real(real64), intent(out) :: %s !! %s" % (out_name, out_doc))
    for key in opts:
        lines.append(CORR_D[key])
    lines.append("        end subroutine %s_%s" % (base, tag))
    return "\n".join(lines)


def pair_body(base, out_name, opts, tag, widen):
    """One `module procedure` body for a two-sample procedure's per-kind entry point."""
    lines = ["    module procedure %s_%s" % (base, tag)]
    lines.append("        real(real64), allocatable :: wx(:), wy(:)")
    if tag == "col":
        lines.append("        logical, allocatable :: mx(:), my(:), both(:)")
        # `is_valid` goes to the FIRST widening, whose own guard aborts if it is present -- so the
        # refusal is stated once, in `col_to_real64`, rather than repeated here.
        lines.append('        call col_to_real64(x, "pf_%s", is_valid, wx, mx)' % base)
        lines.append('        call col_to_real64(y, "pf_%s", v=wy, mask=my)' % base)
        # Both columns carry their own validity and a PAIR needs both, so the two masks are
        # combined here rather than by the caller.
        lines.append("        call pair_mask(mx, my, size(wx, kind=int64), both)")
        call = ["wx", "wy", out_name, "is_valid=both"] \
            + ["%s=%s" % (o, o) for o in opts if o != "is_valid"]
        lines.append("        call " + wrap_call("%s_f64" % base, call))
    else:
        lines.append("        allocate(wx(size(x, kind=int64)), wy(size(y, kind=int64)))")
        lines.append("        wx = %s" % widen.replace("values", "x"))
        lines.append("        wy = %s" % widen.replace("values", "y"))
        call = ["wx", "wy", out_name] + ["%s=%s" % (o, o) for o in opts]
        lines.append("        call " + wrap_call("%s_f64" % base, call))
    lines.append("    end procedure %s_%s" % (base, tag))
    return "\n".join(lines)


def zscore_iface(tag, decl, has_nan, kindword):
    """One interface body for `pf_zscore`'s per-kind entry point."""
    mine = kind_opts(ZSCORE_OPTS, has_nan)
    args = wrap_args(["values", "z"] + mine)
    lines = ["        !> `pf_zscore` over %s." % kindword]
    lines.append("        module subroutine zscore_%s(%s)" % (tag, args))
    lines.append("            " + decl.strip())
    lines.append("            real(real64), intent(out) :: z(:)")
    lines.append("            !! the standardised values, same size as `values`. An excluded")
    lines.append("            !! element is a quiet NaN unless `out_valid` is present.")
    for key in mine:
        lines.append(D[key])
    lines.append("        end subroutine zscore_%s" % tag)
    return "\n".join(lines)


def zscore_body(tag, widen, has_nan):
    """One `module procedure` body for `pf_zscore`'s per-kind entry point."""
    mine = kind_opts(ZSCORE_OPTS, has_nan)
    lines = ["    module procedure zscore_%s" % tag]
    lines.append("        real(real64), allocatable :: wide(:)")
    if tag == "col":
        lines.append("        logical, allocatable :: mask(:)")
        lines.append('        call col_to_real64(values, "pf_zscore", is_valid, wide, mask)')
    else:
        lines.append("        allocate(wide(size(values, kind=int64)))")
        lines.append("        wide = %s" % widen)
    call = ["wide", "z"] + ["%s=%s" % (o, "mask" if (o == "is_valid" and tag == "col") else o)
                            for o in mine]
    lines.append("        call " + wrap_call("zscore_f64", call))
    lines.append("    end procedure zscore_%s" % tag)
    return "\n".join(lines)


def nscore_iface(tag, decl, has_nan, kindword):
    """One interface body for `pf_normal_scores`' per-kind entry point."""
    mine = kind_opts(NSCORE_OPTS, has_nan)
    args = wrap_args(["values", "s"] + mine)
    lines = ["        !> `pf_normal_scores` over %s." % kindword]
    lines.append("        module subroutine normal_scores_%s(%s)" % (tag, args))
    lines.append("            " + decl.strip())
    lines.append("            real(real64), intent(out) :: s(:)")
    lines.append("            !! the normal scores, same size as `values`, in the caller's")
    lines.append("            !! original order. An excluded element is a quiet NaN unless")
    lines.append("            !! `out_valid` is present.")
    for key in mine:
        lines.append(NSCORE_D[key])
    lines.append("        end subroutine normal_scores_%s" % tag)
    return "\n".join(lines)


def nscore_body(tag, widen, has_nan):
    """One `module procedure` body for `pf_normal_scores`' per-kind entry point."""
    mine = kind_opts(NSCORE_OPTS, has_nan)
    lines = ["    module procedure normal_scores_%s" % tag]
    lines.append("        real(real64), allocatable :: wide(:)")
    if tag == "col":
        lines.append("        logical, allocatable :: mask(:)")
        lines.append('        call col_to_real64(values, "pf_normal_scores", is_valid, wide, mask)')
    else:
        lines.append("        allocate(wide(size(values, kind=int64)))")
        lines.append("        wide = %s" % widen)
    call = ["wide", "s"] + ["%s=%s" % (o, "mask" if (o == "is_valid" and tag == "col") else o)
                            for o in mine]
    lines.append("        call " + wrap_call("normal_scores_f64", call))
    lines.append("    end procedure normal_scores_%s" % tag)
    return "\n".join(lines)


def pfit_iface(tag, decl, has_nan, kindword):
    """One interface body for `pf_probit_fit`'s per-kind entry point."""
    mine = kind_opts(PFIT_OPTS, has_nan)
    args = wrap_args(["values", "loc", "sigma"] + mine)
    lines = ["        !> `pf_probit_fit` over %s." % kindword]
    lines.append("        module subroutine probit_fit_%s(%s)" % (tag, args))
    lines.append("            " + decl.strip())
    lines.append("            real(real64), intent(out) :: loc")
    lines.append("            !! the intercept, which every token makes the MEAN of the")
    lines.append("            !! survivors. NaN when fewer than two survive.")
    lines.append("            real(real64), intent(out) :: sigma")
    lines.append("            !! the slope: the scale the normal-probability line implies.")
    for key in mine:
        lines.append(PFIT_D[key])
    lines.append("        end subroutine probit_fit_%s" % tag)
    return "\n".join(lines)


def pfit_body(tag, widen, has_nan):
    """One `module procedure` body for `pf_probit_fit`'s per-kind entry point."""
    mine = kind_opts(PFIT_OPTS, has_nan)
    lines = ["    module procedure probit_fit_%s" % tag]
    lines.append("        real(real64), allocatable :: wide(:)")
    if tag == "col":
        lines.append("        logical, allocatable :: mask(:)")
        lines.append('        call col_to_real64(values, "pf_probit_fit", is_valid, wide, mask)')
    else:
        lines.append("        allocate(wide(size(values, kind=int64)))")
        lines.append("        wide = %s" % widen)
    call = ["wide", "loc", "sigma"] \
        + ["%s=%s" % (o, "mask" if (o == "is_valid" and tag == "col") else o) for o in mine]
    lines.append("        call " + wrap_call("probit_fit_f64", call))
    lines.append("    end procedure probit_fit_%s" % tag)
    return "\n".join(lines)


def sigclip_iface(tag, decl, has_nan, kindword):
    """One interface body for `pf_sigma_clipped_stats`' per-kind entry point."""
    mine = kind_opts(SIGCLIP_OPTS, has_nan)
    args = wrap_args(["values", "mean", "median", "stddev"] + mine)
    lines = ["        !> `pf_sigma_clipped_stats` over %s." % kindword]
    lines.append("        module subroutine sigma_clipped_stats_%s(%s)" % (tag, args))
    lines.append("            " + decl.strip())
    for name, doc in (("mean", "the mean of the surviving values; NaN when none survive."),
                      ("median", "their median."),
                      ("stddev", "their POPULATION standard deviation (`ddof = 0`), as astropy reports.")):
        lines.append("            real(real64), intent(out) :: %s !! %s" % (name, doc))
    for key in mine:
        lines.append(ORDER_D[key])
    lines.append("        end subroutine sigma_clipped_stats_%s" % tag)
    return "\n".join(lines)


def sigclip_body(tag, widen, has_nan):
    """One `module procedure` body for `pf_sigma_clipped_stats`' per-kind entry point."""
    mine = kind_opts(SIGCLIP_OPTS, has_nan)
    lines = ["    module procedure sigma_clipped_stats_%s" % tag]
    lines.append("        real(real64), allocatable :: wide(:)")
    if tag == "col":
        lines.append("        logical, allocatable :: mask(:)")
        lines.append('        call col_to_real64(values, "pf_sigma_clipped_stats", is_valid, '
                     "wide, mask)")
    else:
        lines.append("        allocate(wide(size(values, kind=int64)))")
        lines.append("        wide = %s" % widen)
    call = ["wide", "mean", "median", "stddev"] \
        + ["%s=%s" % (o, "mask" if (o == "is_valid" and tag == "col") else o) for o in mine]
    lines.append("        call " + wrap_call("sigma_clipped_stats_f64", call))
    lines.append("    end procedure sigma_clipped_stats_%s" % tag)
    return "\n".join(lines)


def cum_iface(base, kindword_doc, tag, decl, has_nan, kindword):
    """One interface body for a cumulative procedure's per-kind entry point."""
    mine = kind_opts(CUM_OPTS, has_nan)
    args = wrap_args(["values", "out"] + mine)
    lines = ["        !> `pf_%s` over %s." % (base, kindword)]
    lines.append("        module subroutine %s_%s(%s)" % (base, tag, args))
    lines.append("            " + decl.strip())
    lines.append("            real(real64), intent(out) :: out(:)")
    for line in _wrap_doc(kindword_doc):
        lines.append("            !! " + line)
    for key in mine:
        lines.append(D[key])
    lines.append("        end subroutine %s_%s" % (base, tag))
    return "\n".join(lines)


def cum_body(base, tag, widen, has_nan):
    """One `module procedure` body for a cumulative procedure's per-kind entry point."""
    mine = kind_opts(CUM_OPTS, has_nan)
    lines = ["    module procedure %s_%s" % (base, tag)]
    lines.append("        real(real64), allocatable :: wide(:)")
    if tag == "col":
        lines.append("        logical, allocatable :: mask(:)")
        lines.append('        call col_to_real64(values, "pf_%s", is_valid, wide, mask)' % base)
    else:
        lines.append("        allocate(wide(size(values, kind=int64)))")
        lines.append("        wide = %s" % widen)
    call = ["wide", "out"] + ["%s=%s" % (o, "mask" if (o == "is_valid" and tag == "col") else o)
                              for o in mine]
    lines.append("        call " + wrap_call("%s_f64" % base, call))
    lines.append("    end procedure %s_%s" % (base, tag))
    return "\n".join(lines)


#: The four cumulative procedures and the noun each one's `out` argument is described with.
CUM_WORDS = [("cumsum", "sum"), ("cumprod", "product"),
             ("cummax", "maximum"), ("cummin", "minimum")]


#: The binning family's two shapes: (base, result argument, its declaration, its doc, options).
BIN_FAMILY = [
    ("bucketize", "codes", "integer(int32), intent(out) :: codes(:)",
     "the 1-based bin of each value, or 0 for one that joined none. Same size as `values`.",
     BUCKETIZE_OPTS),
    ("histogram", "counts", "real(real64), intent(out) :: counts(:)",
     "one entry per bin, so `size(edges) - 1` of them.", HISTOGRAM_OPTS),
]


def bin_iface(base, out_name, out_decl, out_doc, opts, tag, decl, has_nan, kindword):
    """One interface body for a binning procedure's per-kind entry point."""
    mine = kind_opts(opts, has_nan)
    args = wrap_args(["values", "edges", out_name] + mine)
    lines = ["        !> `pf_%s` over %s." % (base, kindword)]
    lines.append("        module subroutine %s_%s(%s)" % (base, tag, args))
    lines.append("            " + decl.strip())
    lines.append("            real(real64), intent(in) :: edges(:) "
                 "!! the bin boundaries, strictly increasing, at least two.")
    lines.append("            " + out_decl)
    for line in _wrap_doc(out_doc):
        lines.append("            !! " + line)
    for key in mine:
        lines.append(BIN_TAGS[base][key])
    lines.append("        end subroutine %s_%s" % (base, tag))
    return "\n".join(lines)


def bin_body(base, out_name, opts, tag, widen, has_nan):
    """One `module procedure` body for a binning procedure's per-kind entry point."""
    mine = kind_opts(opts, has_nan)
    lines = ["    module procedure %s_%s" % (base, tag)]
    lines.append("        real(real64), allocatable :: wide(:)")
    if tag == "col":
        lines.append("        logical, allocatable :: mask(:)")
        lines.append('        call col_to_real64(values, "pf_%s", is_valid, wide, mask)' % base)
    else:
        lines.append("        allocate(wide(size(values, kind=int64)))")
        lines.append("        wide = %s" % widen)
    call = ["wide", "edges", out_name] \
        + ["%s=%s" % (o, "mask" if (o == "is_valid" and tag == "col") else o) for o in mine]
    lines.append("        call " + wrap_call("%s_f64" % base, call))
    lines.append("    end procedure %s_%s" % (base, tag))
    return "\n".join(lines)


def edges_iface(tag, decl, has_nan, kindword):
    """One interface body for `pf_bin_edges`' per-kind entry point."""
    mine = kind_opts(BIN_EDGES_OPTS, has_nan)
    args = wrap_args(["values", "nbins", "edges"] + mine)
    lines = ["        !> `pf_bin_edges` over %s." % kindword]
    lines.append("        module subroutine bin_edges_%s(%s)" % (tag, args))
    lines.append("            " + decl.strip())
    lines.append("            integer, intent(in) :: nbins !! how many bins to describe; at least 1.")
    lines.append("            real(real64), intent(out) :: edges(:)")
    lines.append("            !! the `nbins + 1` boundaries, strictly increasing.")
    for key in mine:
        lines.append(EDGES_D[key])
    lines.append("        end subroutine bin_edges_%s" % tag)
    return "\n".join(lines)


def edges_body(tag, widen, has_nan):
    """One `module procedure` body for `pf_bin_edges`' per-kind entry point."""
    mine = kind_opts(BIN_EDGES_OPTS, has_nan)
    lines = ["    module procedure bin_edges_%s" % tag]
    lines.append("        real(real64), allocatable :: wide(:)")
    if tag == "col":
        lines.append("        logical, allocatable :: mask(:)")
        lines.append('        call col_to_real64(values, "pf_bin_edges", is_valid, wide, mask)')
    else:
        lines.append("        allocate(wide(size(values, kind=int64)))")
        lines.append("        wide = %s" % widen)
    call = ["wide", "nbins", "edges"] \
        + ["%s=%s" % (o, "mask" if (o == "is_valid" and tag == "col") else o) for o in mine]
    lines.append("        call " + wrap_call("bin_edges_f64", call))
    lines.append("    end procedure bin_edges_%s" % tag)
    return "\n".join(lines)


def bin_linear_iface(tag, decl, has_nan, kindword):
    """One interface body for `pf_bin_linear`'s per-kind entry point."""
    mine = kind_opts(BIN_LINEAR_OPTS, has_nan)
    args = wrap_args(["values", "grid", "mass"] + mine)
    if tag == "col":
        decl = ("type(parquet_column), intent(in) :: values "
                "!! the column; int32, int64, float32 or float64 -- a logical one aborts.")
    lines = ["        !> `pf_bin_linear` over %s." % kindword]
    lines.append("        module subroutine bin_linear_%s(%s)" % (tag, args))
    lines.append("            " + decl.strip())
    lines.append("            real(real64), intent(in) :: grid(:) "
                 "!! the grid POINTS: finite, strictly increasing, at least two.")
    lines.append("            real(real64), intent(out) :: mass(:)")
    lines.append("            !! the weight deposited on each grid point, so `size(grid)` of them.")
    for key in mine:
        lines.append(BIN_LINEAR_D[key])
    lines.append("        end subroutine bin_linear_%s" % tag)
    return "\n".join(lines)


def bin_linear_body(tag, widen, has_nan):
    """One `module procedure` body for `pf_bin_linear`'s per-kind entry point."""
    mine = kind_opts(BIN_LINEAR_OPTS, has_nan)
    lines = ["    module procedure bin_linear_%s" % tag]
    lines.append("        real(real64), allocatable :: wide(:)")
    if tag == "col":
        lines.append("        logical, allocatable :: mask(:)")
        lines.extend(BIN_LINEAR_LOGICAL_COLUMN)
        lines.append('        call col_to_real64(values, "pf_bin_linear", is_valid, wide, mask)')
    else:
        lines.append("        allocate(wide(size(values, kind=int64)))")
        lines.append("        wide = %s" % widen)
    call = ["wide", "grid", "mass"] \
        + ["%s=%s" % (o, "mask" if (o == "is_valid" and tag == "col") else o) for o in mine]
    lines.append("        call " + wrap_call("bin_linear_f64", call))
    lines.append("    end procedure bin_linear_%s" % tag)
    return "\n".join(lines)


def mode_iface(tag, decl, res_decl, res_doc, kindword, modes_decl):
    """One interface body for a `pf_mode` specific."""
    args = wrap_args(["values", "m"] + MODE_OPTS)
    lines = ["        !> `pf_mode` over %s." % kindword]
    lines.append("        module subroutine mode_%s(%s)" % (tag, args))
    lines.append("            " + decl.strip())
    lines.append("            %s" % res_decl)
    for line in _wrap_doc(res_doc):
        lines.append("            !! " + line)
    for key in MODE_OPTS:
        if key == "modes":
            lines.append("            " + modes_decl.replace("intent(out)", "intent(out), optional"))
            for line in MODES_DOC:
                lines.append("            !! " + line)
        else:
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
        lines.append(MOMENTS_D[key])
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


def order_core_iface(base, extra, out_name, out_decl, out_doc, opts, doc, dmap=None):
    """One interface body for an order procedure's real64 core."""
    dmap = dmap or D
    args = ["values"] + [e[0] for e in extra] + [out_name] + opts
    lines = [("        !> " + line).rstrip() for line in doc]
    lines.append("        module subroutine %s_f64(%s)" % (base, wrap_args(args)))
    lines.append("            real(real64), intent(in) :: values(:) !! the population.")
    lines.extend(order_extra_decls(extra))
    lines.append("            " + out_decl)
    for line in _wrap_doc(out_doc):
        lines.append("            !! " + line)
    for key in opts:
        lines.append(dmap[key])
    lines.append("        end subroutine %s_f64" % base)
    return "\n".join(lines)


def order_entry_iface(base, extra, out_name, out_decl, out_doc, opts, tag, decl, has_nan,
                      kindword, dmap=None):
    """One interface body for an order procedure's per-kind entry point."""
    dmap = dmap or D
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
        lines.append(dmap[key])
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
    "pf_gmean": ["The geometric mean -- `scipy.stats.gmean`, computed in log space.",
                 "",
                 "`exp(sum(w*log(x))/sum(w))`. In log space rather than as a product, because the",
                 "product of a few thousand fluxes overflows a `real64` and its logarithm does",
                 "not.",
                 "",
                 "**Two values outside the domain are answered rather than refused.** A value",
                 "exactly `0` gives exactly `0`, which is the limit and is what scipy returns;",
                 "any negative value gives a quiet NaN with `ok = .false.`, since the geometric",
                 "mean of a sign-changing population is not defined. Neither aborts: both are",
                 "data conditions."],
    "pf_hmean": ["The harmonic mean -- `scipy.stats.hmean`.",
                 "",
                 "`sum(w)/sum(w/x)`, with the same domain rules as `pf_gmean`: a zero gives",
                 "exactly `0`, a negative gives a quiet NaN."],
    "pf_probit_mean": [
        "The mean of a set of PROBABILITIES, averaged on the probit scale.",
        "",
        "`Phi(sum(w*Phi**(-1)(p)) / sum(w))`: map each probability to the z it marks, average",
        "the z's, map back. It is to `Phi`/`Phi**(-1)` exactly what `pf_gmean` is to",
        "`exp`/`log`.",
        "",
        "**Why not `pf_mean`?** An arithmetic mean of probabilities is not wrong, but it is the",
        "wrong average whenever the probabilities are the output of a threshold process -- a",
        "selection function, a completeness fraction, a detection rate. Averaging in probit space",
        "is what keeps the answer inside `[0, 1]` by construction, and what makes the average of",
        "a uniformly shifted set of selection probabilities the shift of their average.",
        "",
        "**The domain, in full.** A value outside `[0, 1]` gives a quiet NaN with `ok = .false.`",
        "-- it is not a probability. A `0` in the population gives exactly `0` and a `1` gives",
        "exactly `1`, which are the limits: `Phi**(-1)(0)` is `-Inf` and one `-Inf` drags the",
        "whole sum there. A population holding both a `0` and a `1` gives a NaN, because that sum",
        "is `-Inf + Inf`. None of the four aborts; all are data conditions.",
        "",
        "Needs the retained values on the object (`%probit_mean`), for `%gmean`'s reason: a",
        "probit-sum is a fifth accumulated quantity and charging every population a transcendental",
        "per element in the hot loop to serve the few that ask is the wrong trade. It orders",
        "nothing."],
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
        "`pf_quantile(values, 0.5, med)`, and `pf_quantile`\'s doc-comment sets out the three",
        "ways `pf_nth_quantile` differs from both."],
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
        "O(n). Asking for several probabilities should use `pf_quantiles`, which sorts once.",
        "",
        "**`pf_nth_quantile` in `parquet_sorting` is the other one**, and the two differ in three",
        "ways rather than one: it returns an ELEMENT of the input (with `index=` saying which)",
        "instead of an interpolated value, it accepts every element type this library sorts",
        "including `character` and the temporal kinds, and it takes no weights. Its `ok` also",
        "means the opposite of this module\'s -- omitting `ok` there makes a degenerate",
        "population ABORT, where omitting it here is the ordinary way to call. Reach for it when",
        "the question is *which row*, and for this one when the question is *what value*."],
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
        "supplied. `scale=\"normal\"` (the default) multiplies by `1/Phi^-1(3/4)` =",
        "1.4826022185056018, which makes the result",
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
    "pf_probit_scale": [
        "The scale a SYMMETRIC QUANTILE PAIR implies for Gaussian data.",
        "",
        "`sigma = (Q(1-prob) - Q(prob)) / (2 * Phi^-1(1-prob))`, with `prob = 0.25` by default --",
        "so the default answer is `pf_iqr` divided by 1.3489795003921634. The divisor is what",
        "turns a range into a standard deviation: for a normal population those two quantiles sit",
        "`Phi^-1(1-prob)` scale units either side of the centre, so dividing the gap between them",
        "by twice that recovers the scale, whatever `prob` was.",
        "",
        "**This GENERALISES `pf_mad(scale=\"normal\")`.** That procedure is the `Phi^-1(3/4)` case",
        "taken about the median of the DEVIATIONS; this one is the same consistency factor taken",
        "between two quantiles of the VALUES. Both answer \"sigma, robustly\", and on clean",
        "Gaussian data `pf_stddev`, `pf_mad` and `pf_probit_scale` agree to within sampling error",
        "-- on a sample with a few wild points the first parts company with the other two, which",
        "is the whole reason to reach for either.",
        "",
        "`prob = 0.15865525393145705` is the 16th/84th percentile pair, the other value in daily",
        "use and the one whose divisor is exactly 2. `prob` outside `(0, 0.5)` ABORTS: `0.5` asks",
        "for `0/0` and `0` asks for the whole range over an infinity.",
        "",
        "**`method=` here is `pf_quantile`'s interpolation rule**, since this is two `pf_quantile`",
        "probes off one ordering -- NOT `pf_probit_fit`'s plotting-position token, which shares",
        "the argument name and means something else entirely.",
        "",
        "A CONSTANT population gives exactly `0`, which is the right scale for it and not a",
        "failure: `ok` stays .true. An empty one gives a quiet NaN with `ok = .false.`"],
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
    out.append("        !>")
    out.append("        !> `n_null`/`n_nan` report WHY elements left, which is the whole reason to")
    out.append("        !> reach for this procedure rather than `size(values)`: the three numbers")
    out.append("        !> together account for every element except the zero-weighted ones, and")
    out.append("        !> that difference is what a zero weight IS.")
    out.append("        module subroutine count_valid_%s(values, n, is_valid, weights%s, n_null%s)"
               % (tag, ", skipnan" if is_real else "", ", n_nan" if is_real else ""))
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
    out.append(D["n_null"])
    if is_real:
        out.append(D["n_nan"])
    out.append("        end subroutine count_valid_%s" % tag)
    return "\n".join(out)


def body(tag, decl, what, is_real):
    """One implementation of count_valid_<tag>."""
    out = []
    out.append("    module procedure count_valid_%s" % tag)
    out.append("        integer(int64) :: i, nv, nnull%s, wbits" % (", nnan" if is_real else ""))
    if is_real:
        out.append("        logical :: skip")
    out.append("")
    out.append("        nv = size(values, kind=int64)")
    out.append('        call stats_check_sizes(nv, "pf_count_valid", is_valid, weights)')
    if is_real:
        out.append("        skip = .true.")
        out.append("        if (present(skipnan)) skip = skipnan")
    out.append("        n = 0_int64")
    out.append("        nnull = 0_int64")
    if is_real:
        out.append("        nnan = 0_int64")
    out.append("        do i = 1_int64, nv")
    out.append("            ! The family's exclusion order: nullness, then NaN, then weight. A weight is")
    out.append("            ! examined only for an element still in the population, which is what keeps a NaN")
    out.append("            ! weight beside a null value from aborting.")
    out.append("            if (present(is_valid)) then")
    out.append("                if (.not. is_valid(i)) then")
    out.append("                    nnull = nnull + 1_int64")
    out.append("                    cycle")
    out.append("                end if")
    out.append("            end if")
    if is_real:
        out.append("            if (skip) then")
        out.append("                ! `x /= x` rather than `ieee_is_nan`: this is a per-element path, and")
        out.append("                ! `ieee_is_nan` is a runtime call on ifx and nagfor. Both are quiet on a")
        out.append("                ! quiet NaN, so the only cost is a -Wcompare-reals warning under -Wall.")
        out.append("                if (values(i) /= values(i)) then")
        out.append("                    nnan = nnan + 1_int64")
        out.append("                    cycle")
        out.append("                end if")
        out.append("            end if")
    out.append("            if (present(weights)) then")
    out.append("                ! One integer compare on the happy path; see STATS_W_LIM. The")
    out.append("                ! validator is reached only by a weight that cannot be valid.")
    out.append("                wbits = transfer(weights(i), 0_int64)")
    out.append("                if (wbits < 0_int64 .or. wbits >= STATS_W_LIM) then")
    out.append('                    call stats_check_weight(weights(i), i, "pf_count_valid")')
    out.append("                    if (weights(i) <= 0.0_real64) cycle")
    out.append("                else if (wbits == 0_int64) then")
    out.append("                    cycle")
    out.append("                end if")
    out.append("            end if")
    out.append("            n = n + 1_int64")
    out.append("        end do")
    out.append("        if (present(n_null)) n_null = nnull")
    if is_real:
        out.append("        if (present(n_nan)) n_nan = nnan")
    out.append("    end procedure count_valid_%s" % tag)
    return "\n".join(out)


#: The int32 counterpart of `count_valid_<tag>`, and the narrowing guard behind it.
#:
#: **`n` is REQUIRED, and that is the whole reason this pair can exist.** A generic's specifics must
#: be distinguishable, and an OPTIONAL dummy cannot distinguish them -- two specifics differing only
#: in the kind of an optional `n_null` are rejected at the DECLARATION ("Ambiguous interfaces in
#: generic interface"), not at the call. `pf_count_valid` is the one procedure in this module whose
#: reporting integer is required, so `n`'s kind resolves the call and `n_null`/`n_nan` follow it.
#: Every other generic here has nothing but optional integers and therefore cannot have this pair --
#: see feature_stats_int32.md for the census and the two compiler probes.
#:
#: **Mixed kinds are a COMPILE error, which is the point.** `n` int32 with `n_null` int64 matches
#: neither specific, so the "no mixed arguments" rule is enforced by the language rather than by a
#: runtime check.
NARROW_IFACE = """        !> Narrows a count to `int32`, aborting rather than wrapping.
        !!
        !! Reached only from the int32 `pf_count_valid` specifics. A population larger than
        !! `huge(int32)` is entirely reachable here -- this library reads billion-row files -- so a
        !! silent wrap would hand back a plausible wrong count, which is the one outcome worse than
        !! forcing the caller to declare an `int64`.
        module subroutine stats_narrow_count(value, noun, dst)
            integer(int64), intent(in) :: value  !! the count.
            character(len=*), intent(in) :: noun !! which count it is, for the message.
            integer(int32), intent(out) :: dst   !! the narrowed copy.
        end subroutine stats_narrow_count"""

NARROW_BODY = """    module procedure stats_narrow_count
        ! GCOVR_EXCL_START
        ! **Unreachable from any test, and deliberately left that way.** Tripping it needs a
        ! population of more than huge(int32) ELEMENTS -- 2**31 real64 values is about 17 GB -- so
        ! no fixture the suite can build gets here, and a debug override would be a second way to
        ! reach a guard whose whole job is to refuse one impossible input. Excluded rather than
        ! chased with an unbuildable fixture, per CLAUDE.md's rule for a defensive branch.
        if (value > int(huge(0_int32), int64)) &
            error stop "pf_count_valid: " // noun // " is " // trim(stats_i2s(value)) // &
                ", which does not fit an integer(int32); declare it integer(int64) instead"
        ! GCOVR_EXCL_STOP
        dst = int(value, int32)
    end procedure stats_narrow_count"""


def spec_iface_i32(tag, decl, what, is_real):
    """One interface body for count_valid_<tag>_i32 -- every integer argument int32."""
    out = []
    out.append("        !> `pf_count_valid` over a %s array, reporting into `integer(int32)`." % what)
    out.append("        !>")
    out.append("        !> Identical to the `integer(int64)` form in every respect but the kind of the")
    out.append("        !> counts. `n` is REQUIRED, so its kind is what selects between the two, and")
    out.append("        !> `n_null`/`n_nan` must then match it -- a mixed call matches neither specific")
    out.append("        !> and does not compile. A count above `huge(int32)` aborts rather than wrapping.")
    out.append("        module subroutine count_valid_%s_i32(values, n, is_valid, weights%s, n_null%s)"
               % (tag, ", skipnan" if is_real else "", ", n_nan" if is_real else ""))
    out.append("            %s, intent(in) :: values(:) !! the population to count." % decl)
    out.append("            integer(int32), intent(out) :: n !! how many elements are in the population.")
    out.append("            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.")
    out.append("            real(real64), intent(in), optional :: weights(:)")
    out.append("            !! per element weight. A zero weight REMOVES the element from the population, so a")
    out.append("            !! weighted count and an unweighted one over the same array legitimately differ.")
    out.append("            !! A negative, NaN or infinite weight aborts.")
    if is_real:
        out.append("            logical, intent(in), optional :: skipnan")
        out.append("            !! .true. (the default) excludes a NaN from the population, as a null is excluded and")
        out.append("            !! as `pf_minmax` does; .false. counts it as an ordinary value.")
    out.append("            integer(int32), intent(out), optional :: n_null !! how many elements were null.")
    if is_real:
        out.append("            integer(int32), intent(out), optional :: n_nan !! how many were NaN and skipped.")
    out.append("        end subroutine count_valid_%s_i32" % tag)
    return "\n".join(out)


def body_i32(tag, decl, what, is_real):
    """One implementation of count_valid_<tag>_i32: the int64 specific, then narrowed."""
    out = []
    out.append("    module procedure count_valid_%s_i32" % tag)
    out.append("        integer(int64) :: n64, nnull64%s" % (", nnan64" if is_real else ""))
    out.append("")
    out.append("        ! Delegated rather than duplicated: one counting loop, so the two kinds cannot")
    out.append("        ! come to disagree about what the population is.")
    out.append("        call count_valid_%s(values, n64, is_valid, weights%s, nnull64%s)"
               % (tag, ", skipnan" if is_real else "", ", nnan64" if is_real else ""))
    out.append('        call stats_narrow_count(n64, "n", n)')
    out.append("        if (present(n_null)) call stats_narrow_count(nnull64, \"n_null\", n_null)")
    if is_real:
        out.append("        if (present(n_nan)) call stats_narrow_count(nnan64, \"n_nan\", n_nan)")
    out.append("    end procedure count_valid_%s_i32" % tag)
    return "\n".join(out)


COUNT_COL_I32_IFACE = """        !> `pf_count_valid` over a scalar numeric `parquet_column`, reporting into `integer(int32)`.
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
        end subroutine count_valid_col_i32"""

COUNT_COL_I32_BODY = """    module procedure count_valid_col_i32
        integer(int64) :: n64, nnull64, nnan64
        call count_valid_col(values, n64, is_valid, weights, skipnan, nnull64, nnan64)
        call stats_narrow_count(n64, "n", n)
        if (present(n_null)) call stats_narrow_count(nnull64, "n_null", n_null)
        if (present(n_nan)) call stats_narrow_count(nnan64, "n_nan", n_nan)
    end procedure count_valid_col_i32"""


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
    out.append("    public :: pf_gmean, pf_hmean, pf_probit_mean")
    out.append("    public :: pf_skewness, pf_kurtosis, pf_moments")
    out.append("    public :: pf_median, pf_quantile, pf_quantiles")
    out.append("    public :: pf_iqr, pf_trim_mean, pf_percentile_of_score")
    out.append("    public :: pf_mad, pf_mode, pf_describe")
    out.append("    public :: pf_cov, pf_corr, pf_zscore, pf_normal_scores")
    out.append("    public :: pf_probit_fit, pf_probit_scale")
    out.append("    public :: pf_sigma_clipped_stats")
    out.append("    public :: pf_cumsum, pf_cumprod, pf_cummax, pf_cummin")
    out.append("    public :: pf_bucketize, pf_histogram, pf_bin_edges, pf_bin_linear")
    out.append("    ! Plumbing for parquet_kde, not API: the family's exclusion pass, argument checkers and column")
    out.append("    ! widener, so that a sibling module applies the family's population rules, widens a column as")
    out.append("    ! the family does and aborts with its texts. The `parquet` facade hides them.")
    out.append("    public :: stats_compact, stats_check_sizes, stats_check_weight, stats_weight_kind")
    out.append("    public :: col_to_real64")
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
    # The int32 forms. `n` is required, so its kind selects between each pair; see NARROW_IFACE.
    for tag, _, _, _ in TYPES:
        out.append("        module procedure count_valid_%s_i32" % tag)
    out.append("        module procedure count_valid_col_i32")
    out.append("    end interface pf_count_valid")
    out.append("    !")
    out.append("    ! ---- Counting ----")
    out.append("    interface")
    out.append("\n".join(spec_iface(*t) for t in TYPES))
    out.append(COUNT_COL_IFACE.replace("@@count_col_counts@@",
                                       D["n_null"] + "\n" + D["n_nan"]))
    out.append("\n".join(spec_iface_i32(*t) for t in TYPES))
    out.append(COUNT_COL_I32_IFACE)
    out.append(NARROW_IFACE)
    out.append("    end interface")
    for name in ("pf_sum", "pf_mean", "pf_gmean", "pf_hmean", "pf_probit_mean", "pf_variance",
                 "pf_stddev", "pf_sem", "pf_skewness", "pf_kurtosis", "pf_moments"):
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
                 "pf_percentile_of_score", "pf_mad", "pf_probit_scale"):
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
    for name in ("pf_cov", "pf_corr", "pf_zscore", "pf_normal_scores", "pf_probit_fit",
                 "pf_sigma_clipped_stats"):
        out.append("    !")
        for line in RELATE_DOC[name]:
            out.append(("    !> " + line).rstrip())
        for line in _RELATE_COMMON.split("\n"):
            out.append(line.rstrip())
        out.append("    interface %s" % name)
        base = name[3:]
        out.append("        module procedure %s_f64" % base)
        for tag, _, _, _, _ in MOMENT_KINDS:
            out.append("        module procedure %s_%s" % (base, tag))
        out.append("    end interface %s" % name)
    for name in ("pf_cumsum", "pf_cumprod", "pf_cummax", "pf_cummin"):
        out.append("    !")
        for line in CUM_DOC[name]:
            out.append(("    !> " + line).rstrip())
        for line in _CUM_COMMON.split("\n"):
            out.append(line.rstrip())
        out.append("    interface %s" % name)
        out.append("        module procedure %s_f64" % name[3:])
        for tag, _, _, _, _ in MOMENT_KINDS:
            out.append("        module procedure %s_%s" % (name[3:], tag))
        out.append("    end interface %s" % name)
    for name in ("pf_bucketize", "pf_histogram", "pf_bin_edges"):
        out.append("    !")
        for line in BIN_DOC[name]:
            out.append(("    !> " + line).rstrip())
        for line in _BIN_COMMON.split("\n"):
            out.append(line.rstrip())
        out.append("    interface %s" % name)
        out.append("        module procedure %s_f64" % name[3:])
        for tag, _, _, _, _ in MOMENT_KINDS:
            out.append("        module procedure %s_%s" % (name[3:], tag))
        out.append("    end interface %s" % name)
    out.append("    !")
    for line in BIN_LINEAR_DOC:
        out.append(("    !> " + line).rstrip())
    out.append("    interface pf_bin_linear")
    out.append("        module procedure bin_linear_f64")
    for tag, _, _, _, _ in BIN_LINEAR_KINDS:
        out.append("        module procedure bin_linear_%s" % tag)
    out.append("    end interface pf_bin_linear")
    out.append("    !")
    for line in MODE_DOC:
        out.append(("    !> " + line).rstrip())
    out.append("    interface pf_mode")
    for tag, _, _, _, _, _ in MODE_KINDS:
        out.append("        module procedure mode_%s" % tag)
    out.append("    end interface pf_mode")
    out.append(GUARD_SPEC.rstrip("\n"))
    out.append(COL_WIDEN_SPEC.rstrip("\n"))
    out.append("    !")
    out.append("    ! ---- The real64 moment core (implemented in parquet_stats_core) ----")
    out.append("    interface")
    for text in CORE_IFACES:
        out.append("\n".join(line.rstrip() for line in text.split("\n")))
    out.append(MOMENTS_IFACE % "\n".join(MOMENTS_D[k] for k in MOMENTS_OPTS))
    out.append("    end interface")
    out.append("    !")
    out.append("    ! ---- The real64 order core (implemented in parquet_stats_order) ----")
    out.append("    interface")
    for base, extra, out_name, out_decl, out_doc, opts in ORDER_FAMILY:
        out.append(order_core_iface(base, extra, out_name, out_decl, out_doc, opts,
                                    ORDER_DOC["pf_" + base], dmap=order_dmap(base)))
    out.append(ORDER_HELPER_IFACES.rstrip("\n"))
    out.append("    end interface")
    out.append("    !")
    out.append("    ! ---- The real64 cores of the relational and robust families ----")
    out.append("    interface")
    relate = RELATE_IFACES
    relate = relate.replace("@@cov_opts@@", "\n".join(CORR_D[k] for k in COV_OPTS))
    relate = relate.replace("@@corr_opts@@", "\n".join(CORR_D[k] for k in CORR_OPTS))
    relate = relate.replace("@@zscore_opts@@", "\n".join(D[k] for k in ZSCORE_OPTS))
    relate = relate.replace("@@nscore_opts@@", "\n".join(NSCORE_D[k] for k in NSCORE_OPTS))
    relate = relate.replace("@@pfit_opts@@", "\n".join(PFIT_D[k] for k in PFIT_OPTS))
    relate = relate.replace("@@sigclip_opts@@", "\n".join(ORDER_D[k] for k in SIGCLIP_OPTS))
    out.append(relate)
    cum = CUM_IFACES.replace("@@cum_opts@@", "\n".join(D[k] for k in CUM_OPTS))
    out.append(cum)
    out.append("    end interface")
    out.append("    !")
    out.append("    ! ---- The real64 binning core (implemented in parquet_stats_bin) ----")
    out.append("    interface")
    binning = BIN_IFACES
    binning = binning.replace("@@bucketize_opts@@",
                              "\n".join(BUCKETIZE_D[k] for k in BUCKETIZE_OPTS))
    binning = binning.replace("@@histogram_opts@@",
                              "\n".join(HISTOGRAM_D[k] for k in HISTOGRAM_OPTS))
    binning = binning.replace("@@bin_edges_opts@@", "\n".join(EDGES_D[k] for k in BIN_EDGES_OPTS))
    binning = binning.replace("@@bin_linear_opts@@",
                              "\n".join(BIN_LINEAR_D[k] for k in BIN_LINEAR_OPTS))
    out.append(binning)
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
                                         tag, decl, has_nan, kindword,
                                         dmap=order_dmap(base)))
        out.append(pair_iface("cov", "c", "the covariance; NaN when `n_valid <= ddof`.",
                              COV_OPTS, tag, decl, kindword, []))
        out.append(pair_iface("corr", "r",
                              "the correlation in [-1, 1]; NaN when either sample is constant.",
                              CORR_OPTS, tag, decl, kindword, []))
        out.append(zscore_iface(tag, decl, has_nan, kindword))
        out.append(nscore_iface(tag, decl, has_nan, kindword))
        out.append(pfit_iface(tag, decl, has_nan, kindword))
        out.append(sigclip_iface(tag, decl, has_nan, kindword))
        for base, word in CUM_WORDS:
            out.append(cum_iface(base, "the running %s, same size as `values`. An excluded "
                                 "element is a quiet NaN unless `out_valid` is present." % word,
                                 tag, decl, has_nan, kindword))
        for base, out_name, out_decl, out_doc, opts in BIN_FAMILY:
            out.append(bin_iface(base, out_name, out_decl, out_doc, opts,
                                 tag, decl, has_nan, kindword))
        out.append(edges_iface(tag, decl, has_nan, kindword))
        if tag in [k[0] for k in BIN_LINEAR_KINDS]:
            out.append(bin_linear_iface(tag, decl, has_nan, kindword))
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
    out.append("\n\n".join(body_i32(*t) for t in TYPES))
    out.append("")
    out.append(COUNT_COL_I32_BODY)
    out.append("")
    out.append(NARROW_BODY)
    out.append("")
    bodies = []
    for tag, _, widen, has_nan, _ in MOMENT_KINDS:
        for base, out_name, _, opts in ENTRY_FAMILY:
            bodies.append(entry_body(base, out_name, opts, tag, widen, has_nan))
        bodies.append(entry_moments_body(tag, widen, has_nan))
        for base, extra, out_name, _, _, opts in ORDER_FAMILY:
            bodies.append(order_entry_body(base, extra, out_name, opts, tag, widen, has_nan))
        bodies.append(pair_body("cov", "c", COV_OPTS, tag, widen))
        bodies.append(pair_body("corr", "r", CORR_OPTS, tag, widen))
        bodies.append(zscore_body(tag, widen, has_nan))
        bodies.append(nscore_body(tag, widen, has_nan))
        bodies.append(pfit_body(tag, widen, has_nan))
        bodies.append(sigclip_body(tag, widen, has_nan))
        for base, _ in CUM_WORDS:
            bodies.append(cum_body(base, tag, widen, has_nan))
        for base, out_name, _, _, opts in BIN_FAMILY:
            bodies.append(bin_body(base, out_name, opts, tag, widen, has_nan))
        bodies.append(edges_body(tag, widen, has_nan))
        if tag in [k[0] for k in BIN_LINEAR_KINDS]:
            bodies.append(bin_linear_body(tag, widen, has_nan))
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
