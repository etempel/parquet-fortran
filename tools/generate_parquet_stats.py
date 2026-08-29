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
    use iso_fortran_env, only : int32, int64, real32, real64
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
D["n_null"] = """            integer(int64), intent(out), optional :: n_null
            !! how many elements `is_valid` excluded."""
D["n_nan"] = """            integer(int64), intent(out), optional :: n_nan
            !! how many were excluded as NaN and were not already null."""
D["ok"] = """            logical, intent(out), optional :: ok
            !! .false. when the statistic is undefined for this population -- the result is then a
            !! quiet NaN and must not be relied on. Partial nullness is not a failure."""


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
    !> `real(real64)` only for now; P4 adds the other numeric kinds and `type(parquet_column)` to
    !> this same generic, which is source-compatible for every existing call."""


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


MOMENT_OPTS = ["is_valid", "weights", "weight_type", "ddof", "skipnan", "n_null", "n_nan", "ok"]
PLAIN_OPTS = ["is_valid", "weights", "skipnan", "n_null", "n_nan", "ok"]

CORE_IFACES = [
    iface("sum_f64",
          ["`pf_sum` over a 64-bit real array: the PAIRWISE sum of the population.",
           "",
           "Weighted, this is `sum(w*x)`. An empty population sums to exactly `0` with",
           "`ok = .true.`, which is the additive identity and what numpy and pandas return -- it",
           "is the one quantity in this family that an empty population still defines."],
          ("s", "the sum."), PLAIN_OPTS),
    iface("mean_f64",
          ["`pf_mean` over a 64-bit real array: `sum(w*x) / sum(w)`."],
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
          ("g", "the skewness."),
          ["is_valid", "weights", "weight_type", "bias", "skipnan", "n_null", "n_nan", "ok"]),
    iface("kurtosis_f64",
          ["`pf_kurtosis` over a 64-bit real array: the fourth standardised moment.",
           "",
           "`ddof` is absent for the same reason as on `pf_skewness`. NaN when the variance is",
           "zero, or when `n_valid < 4` and `bias` is .false."],
          ("k", "the kurtosis, excess by default."),
          ["is_valid", "weights", "weight_type", "bias", "excess", "skipnan", "n_null", "n_nan",
           "ok"]),
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
%s
        end subroutine moments_f64'''


GENERIC_DOC = {
    "pf_sum": ["The sum of a population, computed PAIRWISE rather than left to right.",
               "",
               "Pairwise summation loses O(log n) relative accuracy against a naive sum's O(n),",
               "for the same number of additions and no extra memory -- and it is the same fixed",
               "block tree that makes every answer here independent of the thread count."],
    "pf_mean": ["The arithmetic mean of a population, weighted when `weights` is present."],
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

GENERIC_SPECIFICS = {
    "pf_sum": ["sum_f64"], "pf_mean": ["mean_f64"], "pf_variance": ["variance_f64"],
    "pf_stddev": ["stddev_f64"], "pf_sem": ["sem_f64"], "pf_skewness": ["skewness_f64"],
    "pf_kurtosis": ["kurtosis_f64"], "pf_moments": ["moments_f64"],
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


def gen_spec():
    out = [SPEC_HEAD.rstrip("\n") + "\n"]
    out.append("    public :: pf_count_valid")
    out.append("    public :: pf_sum, pf_mean, pf_variance, pf_stddev, pf_sem")
    out.append("    public :: pf_skewness, pf_kurtosis, pf_moments\n")
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
    out.append("    end interface pf_count_valid")
    out.append("    !")
    out.append("    ! ---- Counting ----")
    out.append("    interface")
    out.append("\n".join(spec_iface(*t) for t in TYPES))
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
    out.append(GUARD_SPEC.rstrip("\n"))
    out.append("    !")
    out.append("    ! ---- The real64 moment core (implemented in parquet_stats_core) ----")
    out.append("    interface")
    for text in CORE_IFACES:
        out.append("\n".join(line.rstrip() for line in text.split("\n")))
    out.append(MOMENTS_IFACE % "\n".join(
        D[k] for k in ("is_valid", "weights", "weight_type", "ddof", "bias", "excess", "skipnan",
                       "n_null", "n_nan")))
    out.append("    end interface")
    out.append("    !")
    out.append("end module parquet_stats ! GCOVR_EXCL_LINE")
    return "\n".join(out) + "\n"


def gen_kernel():
    out = [KERNEL_HEAD.rstrip("\n")]
    out.append("")
    out.append("\n\n".join(body(*t) for t in TYPES))
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
