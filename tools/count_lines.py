#!/usr/bin/env python3
"""Counts code/comment/blank lines for this repository's Fortran (.f90) and
C++ (.cpp/.hpp/.h) source files.

A line is classified as:
  - blank:    empty after stripping whitespace.
  - comment:  every non-whitespace character on the line is part of a
              comment (Fortran `!...`; C++ `//...` or inside a `/* ... */`
              block, including block-comment delimiter lines).
  - code:     anything else, including a line that mixes code and a
              trailing comment (e.g. `x = 1  ! set x`) -- it has real
              code on it, so it counts as code, not comment.

Usage:
    tools/count_lines.py [paths...]

With no arguments, reports two independent summaries: src/ (the library
itself) and test/ (the test suite) -- each counted and totaled on its own,
not combined, so the test suite's size never inflates the library's own
count or vice versa. Pass explicit paths (files or directories) to instead
report a single independent summary over just those paths, e.g.:
    tools/count_lines.py src/parquet_wrapper.cpp
"""
import sys
from pathlib import Path

FORTRAN_EXTS = {".f90", ".F90"}
CPP_EXTS = {".cpp", ".cc", ".cxx", ".hpp", ".h", ".hh"}


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
            for ext in FORTRAN_EXTS | CPP_EXTS:
                files.extend(sorted(path.rglob(f"*{ext}")))
        elif path.is_file():
            files.append(path)
    return sorted(set(files))


def report(label, paths):
    """Prints one independent code/comment/blank summary for `paths`, under
    a `label` heading. Returns True if any files were found."""
    files = collect_files(paths)
    print(f"=== {label} ({', '.join(paths)}) ===")
    if not files:
        print("No .f90/.cpp/.h files found.")
        print()
        return False

    totals = {"code": 0, "comment": 0, "blank": 0}
    rows = []
    for f in files:
        text = f.read_text(errors="replace").splitlines()
        if f.suffix in FORTRAN_EXTS:
            code, comment, blank = classify_fortran(text)
        else:
            code, comment, blank = classify_cpp(text)
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
    return True


def main():
    args = sys.argv[1:]
    if args:
        ok = report("summary", args)
        return 0 if ok else 1

    ok_src = report("library (src)", ["src"])
    ok_test = report("test suite (test)", ["test"])
    return 0 if (ok_src or ok_test) else 1


if __name__ == "__main__":
    sys.exit(main())
