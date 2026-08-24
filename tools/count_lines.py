#!/usr/bin/env python3
"""Counts code/comment/blank lines for this repository's Fortran (.f90),
C++ (.cpp/.hpp/.h) and script (.py/.sh) sources.

A line is classified as:
  - blank:    empty after stripping whitespace.
  - comment:  every non-whitespace character on the line is part of a
              comment (Fortran `!...`; C++ `//...` or inside a `/* ... */`
              block, including block-comment delimiter lines; Python/shell
              `#...`, shebang included).
  - code:     anything else, including a line that mixes code and a
              trailing comment (e.g. `x = 1  ! set x`) -- it has real
              code on it, so it counts as code, not comment.

Known limitation, deliberate: a Python docstring counts as CODE, since
only `#` marks a comment here. Treating triple-quoted strings as comments
would badly misreport the generators under tools/, whose emitted Fortran
lives in exactly such strings and is their whole payload -- overstating a
script's code by its own docstring is much the smaller error.

Usage:
    tools/count_lines.py [paths...]

With no arguments, reports four independent summaries: src/ (the library
itself), test/ (the test suite), app/ (the manual, never-`fpm test`
programs) and tools/ (the maintenance tooling) -- each counted and totaled
on its own, not combined, so no group's size inflates another's. Pass
explicit paths (files or directories) to instead report a single
independent summary over just those paths, e.g.:
    tools/count_lines.py src/parquet_wrapper.cpp

Reports code/comment/blank line counts, a convenience for repository
metrics. With no arguments it prints four independent summaries -- `src/` (the library), `test/`
(the test suite), `app/` (the manual programs) and `tools/` (this tooling) -- each totalled on its
own so no group inflates another, followed by a cross-group table repeating the four totals and
adding the repository-wide one (the single number the independent summaries deliberately
withhold). Pass explicit files or directories for a single summary over just those, with no cross-
group table. It understands Fortran, C++ and Python/shell comment syntax, with one deliberate
simplification: only `#` marks a comment in a script, so a Python docstring counts as code
(treating triple-quoted strings as comments would misreport the generators here, whose emitted
Fortran lives in exactly such strings).
"""
import sys
from pathlib import Path

FORTRAN_EXTS = {".f90", ".F90"}
CPP_EXTS = {".cpp", ".cc", ".cxx", ".hpp", ".h", ".hh"}
SCRIPT_EXTS = {".py", ".sh", ".bash"}
#: Directory names never descended into: build artefacts and caches hold
#: copies of real sources, which would be counted twice.
SKIP_DIRS = {"__pycache__", "build", "ford-doc"}


def classify_hash(lines):
    """Python/shell: `#` to end of line, with no block-comment form."""
    code = comment = blank = 0
    for line in lines:
        stripped = line.strip()
        if not stripped:
            blank += 1
        elif stripped.startswith("#"):
            comment += 1
        else:
            code += 1
    return code, comment, blank


def classify_fortran(lines):
    code = comment = blank = 0
    for line in lines:
        stripped = line.strip()
        if not stripped:
            blank += 1
        elif stripped.startswith("!"):
            comment += 1
        else:
            code += 1
    return code, comment, blank


def classify_cpp(lines):
    code = comment = blank = 0
    in_block_comment = False
    for line in lines:
        stripped = line.strip()
        if not stripped and not in_block_comment:
            blank += 1
            continue

        pos = 0
        n = len(stripped)
        saw_code = False
        saw_comment = False
        while pos < n:
            if in_block_comment:
                end = stripped.find("*/", pos)
                saw_comment = True
                if end == -1:
                    pos = n
                else:
                    in_block_comment = False
                    pos = end + 2
                continue

            line_comment = stripped.find("//", pos)
            block_start = stripped.find("/*", pos)

            if line_comment == -1 and block_start == -1:
                if stripped[pos:n].strip():
                    saw_code = True
                pos = n
            elif block_start == -1 or (line_comment != -1 and line_comment < block_start):
                if stripped[pos:line_comment].strip():
                    saw_code = True
                saw_comment = True
                pos = n
            else:
                if stripped[pos:block_start].strip():
                    saw_code = True
                saw_comment = True
                in_block_comment = True
                pos = block_start + 2

        if saw_code:
            code += 1
        elif saw_comment:
            comment += 1
        else:
            blank += 1
    return code, comment, blank


def collect_files(paths):
    files = []
    for p in paths:
        path = Path(p)
        if path.is_dir():
            for ext in FORTRAN_EXTS | CPP_EXTS | SCRIPT_EXTS:
                files.extend(
                    f for f in sorted(path.rglob(f"*{ext}"))
                    if not (SKIP_DIRS & set(f.parts) or any(part.startswith(".") for part in f.parts))
                )
        elif path.is_file():
            # An explicitly named file is counted whatever it is: naming it IS
            # the request, so no directory or extension filter applies.
            files.append(path)
    return sorted(set(files))


def classify(path, lines):
    """Dispatches on extension. An unrecognized one falls to the C++ rules,
    which are the most permissive (`//` and `/* */`, both common enough)."""
    if path.suffix in FORTRAN_EXTS:
        return classify_fortran(lines)
    if path.suffix in SCRIPT_EXTS:
        return classify_hash(lines)
    return classify_cpp(lines)


def report(label, paths):
    """Prints one independent code/comment/blank summary for `paths`, under
    a `label` heading. Returns its `{code, comment, blank}` totals, or None
    if no files were found -- which is what lets the caller add them up
    across groups without recounting anything."""
    files = collect_files(paths)
    print(f"=== {label} ({', '.join(paths)}) ===")
    if not files:
        print("No .f90/.cpp/.h/.py/.sh files found.")
        print()
        return None

    totals = {"code": 0, "comment": 0, "blank": 0}
    rows = []
    for f in files:
        text = f.read_text(errors="replace").splitlines()
        code, comment, blank = classify(f, text)
        total = code + comment + blank
        rows.append((str(f), total, code, comment, blank))
        totals["code"] += code
        totals["comment"] += comment
        totals["blank"] += blank

    name_w = max(len(r[0]) for r in rows) + 2
    header = f"{'file':<{name_w}}{'total':>8}{'code':>8}{'comment':>9}{'blank':>8}"
    print(header)
    print("-" * len(header))
    for name, total, code, comment, blank in rows:
        print(f"{name:<{name_w}}{total:>8}{code:>8}{comment:>9}{blank:>8}")

    grand_total = totals["code"] + totals["comment"] + totals["blank"]
    print("-" * len(header))
    print(f"{'TOTAL':<{name_w}}{grand_total:>8}{totals['code']:>8}{totals['comment']:>9}{totals['blank']:>8}")

    if grand_total:
        pct = lambda n: f"{100.0 * n / grand_total:.1f}%"
        print()
        print(f"code: {totals['code']} ({pct(totals['code'])})  "
              f"comment: {totals['comment']} ({pct(totals['comment'])})  "
              f"blank: {totals['blank']} ({pct(totals['blank'])})")
    print()
    return totals


def report_groups(groups):
    """Prints one line per group, then the repository-wide total.

    The point of the last line is that it is the ONE number the four
    independent summaries above deliberately do not give: each of those is
    totalled on its own so no group inflates another, which leaves nothing
    saying how big the repository is altogether."""
    rows = [(label, t) for label, t in groups if t is not None]
    if not rows:
        return
    overall = {"code": 0, "comment": 0, "blank": 0}
    for _, t in rows:
        for key in overall:
            overall[key] += t[key]

    name_w = max(max(len(label) for label, _ in rows), len("TOTAL (all groups)")) + 2
    header = f"{'group':<{name_w}}{'total':>8}{'code':>8}{'comment':>9}{'blank':>8}"
    print("=== summary ===")
    print(header)
    print("-" * len(header))
    for label, t in rows:
        total = t["code"] + t["comment"] + t["blank"]
        print(f"{label:<{name_w}}{total:>8}{t['code']:>8}{t['comment']:>9}{t['blank']:>8}")

    grand_total = overall["code"] + overall["comment"] + overall["blank"]
    print("-" * len(header))
    print(f"{'TOTAL (all groups)':<{name_w}}{grand_total:>8}{overall['code']:>8}"
          f"{overall['comment']:>9}{overall['blank']:>8}")
    if grand_total:
        pct = lambda n: f"{100.0 * n / grand_total:.1f}%"
        print()
        print(f"code: {overall['code']} ({pct(overall['code'])})  "
              f"comment: {overall['comment']} ({pct(overall['comment'])})  "
              f"blank: {overall['blank']} ({pct(overall['blank'])})")
    print()


def main():
    args = sys.argv[1:]
    if args:
        # One group, whose own TOTAL line is already the whole answer -- a
        # cross-group summary of a single group would just repeat it.
        return 0 if report("summary", args) is not None else 1

    groups = [
        ("library (src)", report("library (src)", ["src"])),
        ("test suite (test)", report("test suite (test)", ["test"])),
        ("manual programs (app)", report("manual programs (app)", ["app"])),
        ("tooling (tools)", report("tooling (tools)", ["tools"])),
    ]
    report_groups(groups)
    return 0 if any(t is not None for _, t in groups) else 1


if __name__ == "__main__":
    sys.exit(main())
