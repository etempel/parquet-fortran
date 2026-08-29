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

KERNEL_HEAD = BANNER + """!> The per-kind entry points of `parquet_stats`, and the argument guards they share.
!!
!! Each specific is the same shape: check the optional arguments against `values`, then walk the
!! array applying the family's exclusion order -- nullness, then NaN, then weight. The guards live
!! here, once, rather than being emitted into every body.
submodule (parquet_stats) parquet_stats_kernel
    implicit none

contains

    !> Renders an index for an error message without a deferred-length result.
    !!
    !! A fixed-length function result is fine; a `character(len=:), allocatable` one is the shape
    !! this project forbids outright, because gfortran's hidden length temporary for it is not
    !! reliably thread-local.
    pure function stats_i2s(v) result(res)
        integer(int64), intent(in) :: v !! the value to render.
        character(len=24) :: res !! `v` in decimal, blank-padded.
        write(res, '(i0)') v
    end function stats_i2s

    !> Aborts unless every present optional array matches `values` in size.
    subroutine stats_check_sizes(nv, what, is_valid, weights)
        integer(int64), intent(in) :: nv !! the number of elements in `values`.
        character(len=*), intent(in) :: what !! the public procedure's name, for the message.
        logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        real(real64), intent(in), optional :: weights(:) !! per element weight.

        if (present(is_valid)) then
            if (size(is_valid, kind=int64) /= nv) &
                error stop what // ": is_valid has " // trim(stats_i2s(size(is_valid, kind=int64))) // &
                    " elements but values has " // trim(stats_i2s(nv))
        end if
        if (present(weights)) then
            if (size(weights, kind=int64) /= nv) &
                error stop what // ": weights has " // trim(stats_i2s(size(weights, kind=int64))) // &
                    " elements but values has " // trim(stats_i2s(nv))
        end if
    end subroutine stats_check_sizes

    !> Aborts unless `w` is a usable weight: finite and non-negative.
    !!
    !! **The three tests are separate statements on purpose.** Fortran does not short-circuit, so
    !! `w /= w .or. w < 0` would evaluate the `<` on a NaN -- and a NaN comparison with `<` raises
    !! `IEEE_INVALID`, which nagfor's default `-ieee=stop` turns into a dead process. Only `==` and
    !! `/=` are quiet, so the NaN test has to run first and alone.
    subroutine stats_check_weight(w, i, what)
        real(real64), intent(in) :: w !! the weight to validate.
        integer(int64), intent(in) :: i !! its index, for the message.
        character(len=*), intent(in) :: what !! the public procedure's name, for the message.

        if (w /= w) error stop what // ": weight " // trim(stats_i2s(i)) // &
            " is NaN; weights must be finite and non-negative"
        if (w < 0.0_real64) error stop what // ": weight " // trim(stats_i2s(i)) // &
            " is negative; weights must be finite and non-negative"
        if (w > huge(0.0_real64)) error stop what // ": weight " // trim(stats_i2s(i)) // &
            " is infinite; weights must be finite and non-negative"
    end subroutine stats_check_weight

"""


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
    out.append("    public :: pf_count_valid\n")
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
