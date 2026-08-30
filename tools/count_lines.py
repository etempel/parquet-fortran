#!/usr/bin/env python3
"""Counts code/comment/blank lines for this repository's sources, and prose/fenced/blank
lines for its Markdown documentation.

A SOURCE line is classified as:
  - blank:    empty after stripping whitespace.
  - comment:  every non-whitespace character on the line is part of a
              comment (Fortran `!...`; C++ `//...` or inside a `/* ... */`
              block, including block-comment delimiter lines; Python/shell
              `#...`, shebang included).
  - code:     anything else, including a line that mixes code and a
              trailing comment (e.g. `x = 1  ! set x`) -- it has real
              code on it, so it counts as code, not comment.

A MARKDOWN line is classified as fenced (inside a ``` block, delimiters and any blank lines
within it included -- the same rule the C++ classifier already applies to a blank line inside a
block comment), blank, or prose.

Known limitation, deliberate: a Python docstring counts as CODE, since
only `#` marks a comment here. Treating triple-quoted strings as comments
would badly misreport the generators under tools/, whose emitted Fortran
lives in exactly such strings and is their whole payload -- overstating a
script's code by its own docstring is much the smaller error.

Usage:
    tools/count_lines.py [paths...]

With no arguments it prints, in order: five independent source summaries -- `src/` (the library),
`test/` (the test suite), `app/` (the manual programs), `bench/` (the benchmarks and probes) and
`tools/` (this tooling) -- each totalled on its own so no group inflates another; a cross-group
table repeating those five and adding the source-wide total (the single number the independent
summaries deliberately withhold); then a DOCUMENTATION block over every `*.md` in the repository,
split into what ships and the `feature_*.md` planning documents, which are git-ignored scratch and
are reported apart rather than folded in; and finally a code-versus-documentation summary.

Pass explicit files or directories for a single summary over just those, with no cross-group table
and no documentation block, e.g.:
    tools/count_lines.py src/parquet_wrapper.cpp

It understands Fortran, C++, Python/shell and Markdown; an unrecognised extension falls back to
the C++ rules, which are the most permissive.
"""
import sys
from pathlib import Path

FORTRAN_EXTS = {".f90", ".F90"}
CPP_EXTS = {".cpp", ".cc", ".cxx", ".hpp", ".h", ".hh"}
SCRIPT_EXTS = {".py", ".sh", ".bash"}
MARKDOWN_EXTS = {".md"}
SOURCE_EXTS = FORTRAN_EXTS | CPP_EXTS | SCRIPT_EXTS
#: Directory names never descended into: build artefacts and caches hold
#: copies of real sources, which would be counted twice.
#:
#: `test_run` and `dependencies` matter only to the repository-wide Markdown
#: sweep, and matter a great deal there: `test_run/` is where every `tools/*.sh`
#: puts its throwaway FPM_BUILD_DIR, so it accumulates one `test-drive/README.md`
#: per build tree AND, when a benchmark checks this library out beside another,
#: entire copies of this repository's own CLAUDE.md/README.md/CONTRIBUTING.md.
#: Measured before this entry existed: 54 of 113 Markdown files found came from
#: there. The source groups never noticed because each names its own directory;
#: only a sweep from `.` reaches it.
SKIP_DIRS = {"__pycache__", "build", "ford-doc", "test_run", "dependencies"}


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


def classify_markdown(lines):
    """Markdown: a ``` fence and everything inside it is `fenced`, the rest
    is `prose` or `blank`. A blank line INSIDE a fence counts as fenced --
    it is part of the example -- which is the same rule classify_cpp
    already applies to a blank line inside a block comment.

    Returns the same (a, b, c) triple as every other classifier, read here
    as (fenced, prose, blank)."""
    fenced = prose = blank = 0
    in_fence = False
    for line in lines:
        stripped = line.strip()
        if stripped.startswith("```") or stripped.startswith("~~~"):
            in_fence = not in_fence
            fenced += 1
        elif in_fence:
            fenced += 1
        elif not stripped:
            blank += 1
        else:
            prose += 1
    return fenced, prose, blank


def collect_files(paths, exts=None):
    """Gathers the files under `paths` whose extension is in `exts`
    (default: every source extension). A named FILE is always counted,
    whatever it is -- naming it IS the request."""
    if exts is None:
        exts = SOURCE_EXTS
    files = []
    for p in paths:
        path = Path(p)
        if path.is_dir():
            for ext in exts:
                files.extend(
                    f for f in sorted(path.rglob(f"*{ext}"))
                    if not (SKIP_DIRS & set(f.parts) or any(part.startswith(".") for part in f.parts))
                )
        elif path.is_file():
            files.append(path)
    return sorted(set(files))


def classify(path, lines):
    """Dispatches on extension. An unrecognized one falls to the C++ rules,
    which are the most permissive (`//` and `/* */`, both common enough)."""
    if path.suffix in FORTRAN_EXTS:
        return classify_fortran(lines)
    if path.suffix in SCRIPT_EXTS:
        return classify_hash(lines)
    if path.suffix in MARKDOWN_EXTS:
        return classify_markdown(lines)
    return classify_cpp(lines)


def report(label, paths, exts=None, cols=("code", "comment", "blank"), files=None, display=None):
    """Prints one independent three-way line summary for `paths`, under a
    `label` heading, and returns its totals keyed by `cols` -- which is what
    lets the caller add them up across groups without recounting anything.
    Returns None if no files were found.

    `cols` names the three columns (source files are code/comment/blank,
    Markdown is fenced/prose/blank); `files` supplies an explicit list in
    place of collecting one, and `display` the text shown in the heading."""
    if files is None:
        files = collect_files(paths, exts)
    print(f"=== {label} ({display or ', '.join(paths)}) ===")
    if not files:
        print("No matching files found.")
        print()
        return None

    a_col, b_col, c_col = cols
    totals = {a_col: 0, b_col: 0, c_col: 0}
    rows = []
    for f in files:
        text = f.read_text(errors="replace").splitlines()
        a, b, c = classify(f, text)
        rows.append((str(f), a + b + c, a, b, c))
        totals[a_col] += a
        totals[b_col] += b
        totals[c_col] += c

    name_w = max(len(r[0]) for r in rows) + 2
    header = f"{'file':<{name_w}}{'total':>8}{a_col:>8}{b_col:>9}{c_col:>8}"
    print(header)
    print("-" * len(header))
    for name, total, a, b, c in rows:
        print(f"{name:<{name_w}}{total:>8}{a:>8}{b:>9}{c:>8}")

    grand_total = sum(totals.values())
    print("-" * len(header))
    print(f"{'TOTAL':<{name_w}}{grand_total:>8}{totals[a_col]:>8}{totals[b_col]:>9}{totals[c_col]:>8}")

    if grand_total:
        pct = lambda n: f"{100.0 * n / grand_total:.1f}%"
        print()
        print(f"{a_col}: {totals[a_col]} ({pct(totals[a_col])})  "
              f"{b_col}: {totals[b_col]} ({pct(totals[b_col])})  "
              f"{c_col}: {totals[c_col]} ({pct(totals[c_col])})")
    print()
    return totals


def report_groups(groups, cols=("code", "comment", "blank"), title="summary",
                  total_label="TOTAL (all groups)"):
    """Prints one line per group, then the combined total.

    The point of the last line is that it is the ONE number the independent
    summaries above deliberately do not give: each of those is totalled on
    its own so no group inflates another, which leaves nothing saying how
    big the whole is."""
    rows = [(label, t) for label, t in groups if t is not None]
    if not rows:
        return None
    a_col, b_col, c_col = cols
    overall = {a_col: 0, b_col: 0, c_col: 0}
    for _, t in rows:
        for key in overall:
            overall[key] += t[key]

    name_w = max(max(len(label) for label, _ in rows), len(total_label)) + 2
    header = f"{'group':<{name_w}}{'total':>8}{a_col:>8}{b_col:>9}{c_col:>8}"
    print(f"=== {title} ===")
    print(header)
    print("-" * len(header))
    for label, t in rows:
        print(f"{label:<{name_w}}{sum(t.values()):>8}{t[a_col]:>8}{t[b_col]:>9}{t[c_col]:>8}")

    grand_total = sum(overall.values())
    print("-" * len(header))
    print(f"{total_label:<{name_w}}{grand_total:>8}{overall[a_col]:>8}"
          f"{overall[b_col]:>9}{overall[c_col]:>8}")
    if grand_total:
        pct = lambda n: f"{100.0 * n / grand_total:.1f}%"
        print()
        print(f"{a_col}: {overall[a_col]} ({pct(overall[a_col])})  "
              f"{b_col}: {overall[b_col]} ({pct(overall[b_col])})  "
              f"{c_col}: {overall[c_col]} ({pct(overall[c_col])})")
    print()
    return overall


def markdown_groups():
    """Every `*.md` in the repository, split into what ships and the
    `feature_*.md` planning documents.

    The split is by NAME rather than by a hand-kept list, because
    `.gitignore` draws the same line (`feature_*.md`, with `feature_risks.md`
    negated back in) -- so a new planning document lands on the right side
    with nothing to remember. They are reported apart rather than folded in
    because they are working material carried between machines, never
    published: counting them as documentation would make the figure depend
    on which planning documents a given checkout happens to hold."""
    everything = collect_files(["."], MARKDOWN_EXTS)
    shipped = [f for f in everything if not f.name.startswith("feature_")]
    planning = [f for f in everything if f.name.startswith("feature_")]
    return shipped, planning


def summarize(source_totals, doc_totals, planning_totals):
    """The last block: how much of this repository is code and how much is
    the documentation of it. Blank lines are excluded from BOTH sides, so
    the two figures are comparable -- a blank line is neither."""
    code = source_totals["code"]
    comments = source_totals["comment"]
    markdown = doc_totals["fenced"] + doc_totals["prose"] if doc_totals else 0
    documentation = comments + markdown

    label_w = 44
    print("=== summary: code vs documentation ===")
    print(f"{'':<{label_w}}{'lines':>10}")
    print("-" * (label_w + 10))
    print(f"{'code':<{label_w}}{code:>10}")
    print(f"{'documentation':<{label_w}}{documentation:>10}")
    print(f"{'    of which comments in source':<{label_w}}{comments:>10}")
    print(f"{'    of which Markdown':<{label_w}}{markdown:>10}")
    print("-" * (label_w + 10))
    if code:
        print(f"{'ratio documentation : code':<{label_w}}{documentation / code:>9.2f} : 1")
    print()
    print("Blank lines are excluded from both figures -- a blank line is neither.")
    if planning_totals:
        scratch = planning_totals["fenced"] + planning_totals["prose"]
        print(f"The {scratch} non-blank lines of feature_*.md planning documents are NOT counted "
              f"above: they are\nworking material, never published, and vary from checkout to "
              f"checkout.")
    print()


def main():
    args = sys.argv[1:]
    if args:
        # One group, whose own TOTAL line is already the whole answer -- a
        # cross-group summary of a single group would just repeat it.
        files = collect_files(args, SOURCE_EXTS | MARKDOWN_EXTS)
        # Label the columns for what was actually found. An all-Markdown request
        # gets fenced/prose/blank; anything else keeps code/comment/blank, under
        # which a Markdown file in a MIXED request reads as fenced->code and
        # prose->comment. That is the honest reading of a mixed total, and the
        # no-argument report never mixes them.
        md_only = bool(files) and all(f.suffix in MARKDOWN_EXTS for f in files)
        cols = ("fenced", "prose", "blank") if md_only else ("code", "comment", "blank")
        return 0 if report("summary", args, cols=cols, files=files) is not None else 1

    groups = [
        ("library (src)", report("library (src)", ["src"])),
        ("test suite (test)", report("test suite (test)", ["test"])),
        ("manual programs (app)", report("manual programs (app)", ["app"])),
        ("benchmarks (bench)", report("benchmarks (bench)", ["bench"])),
        ("tooling (tools)", report("tooling (tools)", ["tools"])),
    ]
    source_totals = report_groups(groups, title="summary: source", total_label="TOTAL (all sources)")

    shipped, planning = markdown_groups()
    doc_cols = ("fenced", "prose", "blank")
    doc_totals = report("documentation (Markdown)", [], MARKDOWN_EXTS, doc_cols,
                        files=shipped, display="doc/ and every other *.md that ships")
    planning_totals = report("planning documents (Markdown)", [], MARKDOWN_EXTS, doc_cols,
                             files=planning, display="feature_*.md, git-ignored working material")
    report_groups([("documentation (ships)", doc_totals),
                   ("planning (feature_*.md)", planning_totals)],
                  cols=doc_cols, title="summary: documentation",
                  total_label="TOTAL (all Markdown)")

    if source_totals is not None:
        summarize(source_totals, doc_totals, planning_totals)
    return 0 if any(t is not None for _, t in groups) else 1


if __name__ == "__main__":
    sys.exit(main())
