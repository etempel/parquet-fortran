#!/usr/bin/env python3
"""Apply the stage-0b COMPILE-OUT LADDER to `table_resolve`, for measurement branches only.

Stage 0 of the F1 campaign (`feature_colindex.md`) found that `table_resolve`'s prologue is
**82-96% of what a per-cell `%get_element` costs**, and that the figure varies by 3-6x between
gfortran, ifx and flang while the column layer underneath it varies by ~1.5x. Nothing in that
campaign says WHICH part of the prologue it is. This script makes that measurable.

WHY A LADDER AND NOT PHASE TIMERS
---------------------------------
The obvious instrument is this project's own `parquet_debug_get_*_nanos` phase counters -- how the
filter's "93% of it is one helper's parameter type" finding was made. They cannot be used here.
`table_resolve` is the per-element path: the whole call is ~37 ns and a `steady_clock::now()` pair
costs 20-25 ns, so the timer would cost more than the thing timed and would perturb the code under
study. CLAUDE.md states the rule directly -- a debug hook may sit on a coarse operation, never on a
per-row or per-element path.

So instead: put each phase behind a cpp macro, build one binary per rung, and time `%get_element`
with one phase removed at a time. Each rung's DIFFERENCE from the full build is that phase's cost.
No timer goes near the hot path. This reuses the pattern
`tools/generate_parquet_columns.py --bench-guards` established for the column layer, and the same
rules apply to it.

WHAT THE LADDER MEASURES, AND WHAT IT DOES NOT
----------------------------------------------
* It measures REMOVAL, not attribution. A phase whose removal lets the compiler improve another
  will over-report. Good for finding a dominant term; not for apportioning the last nanosecond.
* Every rung is a REBUILD, so a re-run noise floor is the wrong instrument -- the screening
  campaign saw an untouched arm move 16% purely from code layout between two builds that generate
  identical code for it. Quote a cross-build floor and carry a control arm.
* Several rungs are NOT SEMANTICALLY NEUTRAL. `PF_BENCH_NO_LOOKUP` resolves a slot by hashing one
  byte of the name, so it reads the WRONG COLUMN by construction; `PF_BENCH_NO_ROWINDEX_CMP` drops
  the automatic row-index column. Answers and checksums are not comparable across rungs, only
  timings are. This is why the branch is throwaway.

USAGE
-----
    tools/bench_resolve_ladder.py --list       # the rungs and what each removes
    tools/bench_resolve_ladder.py --check      # is the scaffolding applied to the working tree?
    tools/bench_resolve_ladder.py --apply      # rewrite src/parquet_tables_query.f90 in place

There is deliberately no --revert: use `git checkout src/parquet_tables_query.f90`. A hand-rolled
reverse edit is a second thing that can be subtly wrong, and git already does it exactly.

MEASUREMENT BRANCHES ONLY. Never commit the rewritten source to main. With no macro defined the
scaffolded source compiles to exactly today's behaviour, but the file still carries #ifdefs that
have no business in a shipped library.
"""

import argparse
import pathlib
import sys

TARGET = pathlib.Path("src/parquet_tables_query.f90")

MARKER = "PF_BENCH_NO_APPEND_CHECK"

# (macro, what it removes, where the cost goes)
RUNGS = [
    ("(none)", "the full shipped path", "the ladder's baseline; every rung is read against it"),
    ("PF_BENCH_NO_OPEN_CHECK", "the `table_check_open` call",
     "one pointer test plus a call; included mainly as a floor for the ladder itself"),
    ("PF_BENCH_NO_APPEND_CHECK", "the `table_check_no_append` call",
     "an !$omp atomic read plus a call; a plausible ifx suspect"),
    ("PF_BENCH_NO_ROWINDEX_CMP", "the `name == PARQUET_ROW_INDEX` comparison",
     "a string comparison on every accessor call"),
    ("PF_BENCH_NO_LENTRIM", "`cache_find`'s `len_trim`, using `len` instead",
     "valid only when names are exactly sized, which the harness's are"),
    ("PF_BENCH_NO_LOOKUP", "`table_resolve`'s whole name lookup (len_trim + name_sort_key + bisect)",
     "the upper bound on the name-handling half -- the prime suspect, collapsed into one rung. "
     "Read path only: cache_find is also add_column's duplicate check"),
    ("PF_BENCH_NO_RESIDENCY", "the `supported` and `residency` tests",
     "two loads and two comparisons"),
    ("PF_BENCH_NO_REQUIRE_ROW", "`table_require_row`'s body (the call remains)",
     "measures the work, not the call -- the call site is in generated code"),
]

# Each entry: (anchor that must occur exactly once, replacement).
#
# Anchored on the full statement plus enough context to be unique, and every one is asserted to
# occur exactly once before anything is written. CLAUDE.md's rule about never splicing a file with
# an unanchored search applies to source as much as to prose: an edit that silently matches the
# wrong place here would compile and produce plausible numbers.
EDITS = [
    # --- table_resolve: the open check -------------------------------------------------------
    (
        "        call table_check_open(self, proc)\n"
        "        ! Every value accessor -- %col, %get, %set, %is_null, %get_element, a row handle's %get,",
        "#ifndef PF_BENCH_NO_OPEN_CHECK\n"
        "        call table_check_open(self, proc)\n"
        "#endif\n"
        "        ! Every value accessor -- %col, %get, %set, %is_null, %get_element, a row handle's %get,",
    ),
    # --- table_resolve: the append check -----------------------------------------------------
    (
        "        call table_check_no_append(self%cache, proc)\n",
        "#ifndef PF_BENCH_NO_APPEND_CHECK\n"
        "        call table_check_no_append(self%cache, proc)\n"
        "#endif\n",
    ),
    # --- table_resolve: the reserved-name comparison ------------------------------------------
    #
    # Stage 0c nested this `if` (it was one `.and.`-ed condition when the ladder was written), so
    # the baseline the ladder now measures ALREADY has ifx's double lookup removed -- which is the
    # point of the fix, and means a NO_ROWINDEX_CMP rung run after it is not comparable with one
    # run before it. See feature_colindex.md §0.
    (
        "        if (name == PARQUET_ROW_INDEX) then\n",
        "#if !defined(PF_BENCH_NO_ROWINDEX_CMP) && !defined(PF_BENCH_NO_LOOKUP)\n"
        "        if (name == PARQUET_ROW_INDEX) then\n",
    ),
    # Closing the reserved-name guard AND replacing the lookup, in ONE edit, because they are
    # adjacent and cannot be anchored separately: `idx = table_find(self, name)` followed by
    # `if (idx == 0) then` occurs identically in `table_lookup_or_fail`, so the only unique context
    # is the meta_keys line above it -- which the previous edit would otherwise have consumed.
    (
        "                if (allocated(self%cache%meta_keys)) call table_make_row_index(self)\n"
        "            end if\n"
        "        end if\n"
        "        idx = table_find(self, name)\n",
        "                if (allocated(self%cache%meta_keys)) call table_make_row_index(self)\n"
        "            end if\n"
        "        end if\n"
        "#endif\n"
        "#ifdef PF_BENCH_NO_LOOKUP\n"
        "        ! Wrong on purpose -- some other column's slot, in O(1). Timings only.\n"
        "        idx = 1 + iand(iachar(name(len(name):len(name))), 3)\n"
        "        if (idx > self%cache%ncols) idx = self%cache%ncols\n"
        "#else\n"
        "        idx = table_find(self, name)\n"
        "#endif\n",
    ),
    # --- cache_find: len_trim only ------------------------------------------------------------
    #
    # NO_LOOKUP deliberately does NOT live here. cache_find is also how `table_new_slot` detects a
    # duplicate column name, so bypassing it here makes every %add_column believe the name is
    # already taken and the harness cannot even build its table -- found by running it. The rung
    # belongs on the READ path only, which is `table_resolve`'s own `table_find` call below.
    (
        "        nq = len_trim(name)\n",
        "#ifdef PF_BENCH_NO_LENTRIM\n"
        "        nq = len(name)\n"
        "#else\n"
        "        nq = len_trim(name)\n"
        "#endif\n",
    ),
    # --- table_resolve: supported + residency -------------------------------------------------
    (
        "        if (.not. self%cache%cols(idx)%supported) then\n",
        "#ifndef PF_BENCH_NO_RESIDENCY\n"
        "        if (.not. self%cache%cols(idx)%supported) then\n",
    ),
    (
        "            call table_read_enter(self%cache, proc)\n"
        "            call table_touch(self%cache, table_scope_of(self), idx, proc)\n"
        "            call table_read_exit(self%cache)\n"
        "        end if\n",
        "            call table_read_enter(self%cache, proc)\n"
        "            call table_touch(self%cache, table_scope_of(self), idx, proc)\n"
        "            call table_read_exit(self%cache)\n"
        "        end if\n"
        "#endif\n",
    ),
    # --- table_require_row: the body --------------------------------------------------------
    (
        "        if (i >= 1_int64 .and. i <= self%row_count) return\n",
        "#ifdef PF_BENCH_NO_REQUIRE_ROW\n"
        "        return\n"
        "#endif\n"
        "        if (i >= 1_int64 .and. i <= self%row_count) return\n",
    ),
]


def die(msg):
    print(f"bench_resolve_ladder.py: {msg}", file=sys.stderr)
    return 1


def main():
    ap = argparse.ArgumentParser(description=__doc__,
                                 formatter_class=argparse.RawDescriptionHelpFormatter)
    g = ap.add_mutually_exclusive_group(required=True)
    g.add_argument("--apply", action="store_true", help="rewrite the target in place")
    g.add_argument("--check", action="store_true", help="report whether the scaffolding is applied")
    g.add_argument("--list", action="store_true", help="list the rungs")
    args = ap.parse_args()

    root = pathlib.Path(__file__).resolve().parent.parent
    target = root / TARGET

    if args.list:
        print("Stage-0b ladder rungs -- build one binary per rung and time %get_element:\n")
        w = max(len(m) for m, _, _ in RUNGS)
        for macro, removes, note in RUNGS:
            print(f"  {macro:<{w}}  {removes}")
            print(f"  {'':<{w}}    {note}")
        print("\nEach rung's DIFFERENCE from the (none) baseline is that phase's cost.")
        print("PF_BENCH_NO_LOOKUP and PF_BENCH_NO_ROWINDEX_CMP change ANSWERS -- timings only.")
        return 0

    if not target.exists():
        return die(f"{TARGET} not found (run from anywhere in the repository)")
    text = target.read_text()
    applied = MARKER in text

    if args.check:
        print(f"bench_resolve_ladder.py: scaffolding is "
              f"{'APPLIED' if applied else 'absent'} in {TARGET}")
        return 0 if applied else 1

    if applied:
        return die(f"{TARGET} already carries the scaffolding. Restore it first with\n"
                   f"  git checkout {TARGET}\n"
                   "and apply again if you meant to.")

    # Validate EVERY anchor before writing ANYTHING. A partially applied ladder is the worst
    # outcome available: it compiles, it runs, and it silently measures a configuration nobody
    # asked for.
    problems = []
    for i, (anchor, _) in enumerate(EDITS, start=1):
        n = text.count(anchor)
        if n != 1:
            problems.append(f"  edit {i}: anchor occurs {n} times, expected exactly 1\n"
                            f"    first line: {anchor.splitlines()[0].strip()!r}")
    if problems:
        print("bench_resolve_ladder.py: the source has moved under this script -- refusing to "
              "edit.", file=sys.stderr)
        print("\n".join(problems), file=sys.stderr)
        print("\nRe-derive the anchors from the current src/parquet_tables_query.f90 and update "
              "EDITS.", file=sys.stderr)
        return 1

    for anchor, replacement in EDITS:
        text = text.replace(anchor, replacement, 1)

    over = [f"  line {n}: {len(line)} columns"
            for n, line in enumerate(text.splitlines(), start=1) if len(line) > 132]
    if over:
        print("bench_resolve_ladder.py: emitted lines exceed the 132-column limit:", file=sys.stderr)
        print("\n".join(over), file=sys.stderr)
        return 1

    target.write_text(text)
    print(f"bench_resolve_ladder.py: applied {len(EDITS)} edits to {TARGET}")
    print("  Build one binary per rung, e.g.:")
    print("      LADDER=PF_BENCH_NO_LOOKUP tools/benchmark_colindex.sh --mode=decompose")
    print(f"  Restore with:  git checkout {TARGET}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
