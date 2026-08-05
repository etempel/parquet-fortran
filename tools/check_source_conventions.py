#!/usr/bin/env python3
"""Checks six structural invariants of this repository that no compiler and no runtime test can see.

Each one protects a property whose violation compiles cleanly, passes the whole test suite, and
fails somewhere else entirely -- in a user's program, on another compiler, or in a coverage report.
That is what makes them worth a static check rather than a test:

  1. `parquet_table` must have NO allocatable component (feature_risks.md Risk-13).
     The type is finalizable, so every allocatable component it gains makes the compiler generate a
     deeper recursive walk for its `intent(out)` entry and its FINAL -- and this project has three
     confirmed compiler bugs in exactly that machinery on exactly this type (gfortran leaving an
     OpenMP `private()` copy uninitialized; `%detached` surviving an `intent(out)` reset; ifx
     segfaulting inside its own runtime on a nested derived-type component). New table state goes on
     `parquet_table_cache`, reached through the pointer. See CLAUDE.md, "New `parquet_table` state
     goes on the CACHE".

  2. Every pointer a table accessor hands out must be reached through `self%cache` (feature_risks.md Risk-12).
     `%col` needs no `target` attribute on the caller's table only because the pointer targets heap
     owned by the cache rather than the dummy argument, so F2018 15.5.2.4's "pointer to a dummy's
     target becomes undefined on return" never applies. An accessor that points at `self` directly
     reintroduces the `target` requirement with no compiler diagnostic and no runtime check: the
     library still compiles, every test still passes, and a caller's program corrupts memory.
     `target` is a requirement on the CALLER, which is why nothing on this side can catch it.

  3. Generated files must carry the conventions their generator is supposed to emit (feature_risks.md Risk-19).
     `--check` on the generators compares the committed file against the generator, so a TEMPLATE
     that omits a `!>` doc-comment or a `! GCOVR_EXCL_LINE` produces output that matches perfectly
     and is wrong in every kind it emits at once. Only `ford --warn docs.md` would reveal the first
     (and it is not run in CI, and is drowned in thousands of expected warnings -- see CLAUDE.md's
     "FORD doc-comment conventions"), and nothing at all reveals the second. A third generator
     inherits this check for free by adding its output to GENERATED_FILES below.

  4. The schema-less write declares `auto` sizes (feature_risks.md Risk-2).
     `build_table_schema` must never MEASURE a `col_size`/`array_size` -- declaring `auto` is what
     makes it unable to get one wrong, since the writer then resolves both from the data exactly as
     it would with no schema at all. A change that measures here gets string widths wrong first, and
     the sidecar MAML still looks right because it is emitted at close, after the writer resolved
     them -- so a round-trip test cannot see the difference.

  5. Row-group-scoped reads guard against a sort (feature_risks.md Risk-4).
     A sort permutation destroys row-group locality, so every row-group-scoped read must refuse
     while one is installed or it hands back rows in FILE order -- the right row count and the wrong
     rows. `check_row_group_valid` is what makes a procedure row-group-scoped, so requiring the two
     calls to travel together makes "did we guard all ~19 of them?" mechanical, and self-extending.

  6. print_stat's columns match its documentation (feature_risks.md Risk-5).
     `doc/pages/reading.md` documents the reader's stats table column by column, and that table is
     the only contract the format has -- exactly one assertion in the whole suite touches the output
     text, so a renamed or added column breaks no test. The two had already drifted when this check
     was written (the docs named a `prefetc` column the code calls `fetched`, and omitted four
     others), which is the failure it exists to stop repeating.

Usage:
    tools/check_source_conventions.py            # run every check
    tools/check_source_conventions.py --list     # print what each check covers, then exit

Exits nonzero and prints one line per violation, each naming file:line and what to do instead.
"""
import re
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parent.parent
SRC = REPO_ROOT / "src"

#: The type whose body must stay free of allocatable components, and the file it is declared in.
FINALIZABLE_TYPE = ("parquet_table", SRC / "parquet_tables.f90")

#: Files whose accessors hand pointers back to a caller. Every `self%`-rooted pointer source in
#: these must go through `%cache`.
TABLE_FILES = sorted(SRC.glob("parquet_tables*.f90"))

#: Committed output of tools/generate_parquet_columns.py / generate_parquet_tables.py /
#: generate_parquet_maml.sh. Add a new generator's output here; see the module docstring for why
#: the generators' own --check modes cannot cover this.
GENERATED_FILES = [
    SRC / "parquet_columns.f90",
    SRC / "parquet_columns_access.f90",
    SRC / "parquet_columns_mutate.f90",
    SRC / "parquet_tables.f90",
    SRC / "parquet_tables_access.f90",
    SRC / "parquet_tables_addcol.f90",
    SRC / "parquet_tables_materialize.f90",
    SRC / "parquet_maml_base.f90",
]

#: Marker every generated file carries, checked so that a file dropping out of generation (or being
#: listed here by mistake) is caught rather than silently checked as if it were still generated.
GENERATED_MARKERS = ("GENERATED FILE -- DO NOT EDIT BY HAND", "automatically generated")


def strip_comment(line):
    """Return `line` with any trailing Fortran comment removed, respecting quoted strings."""
    quote = None
    for i, ch in enumerate(line):
        if quote:
            if ch == quote:
                quote = None
        elif ch in ("'", '"'):
            quote = ch
        elif ch == "!":
            return line[:i]
    return line


def type_body_lines(path, type_name):
    """Yield (lineno, text) for every line of `type :: <type_name>`'s declaration part.

    Stops at the type's `contains` (bindings are not components) or at its `end type`.
    """
    lines = path.read_text().split("\n")
    start = None
    for i, line in enumerate(lines):
        if re.match(r"^\s*type\s*(,\s*[^:]*)?::\s*%s\s*$" % re.escape(type_name), line):
            start = i + 1
            break
    if start is None:
        return None
    body = []
    for i in range(start, len(lines)):
        stripped = strip_comment(lines[i]).strip().lower()
        if stripped == "contains" or stripped.startswith("end type"):
            break
        body.append((i + 1, lines[i]))
    return body


def check_no_allocatable_component():
    """feature_risks.md Risk-13 -- `parquet_table`'s own body must hold only scalars and the pointer."""
    problems = []
    type_name, path = FINALIZABLE_TYPE
    body = type_body_lines(path, type_name)
    if body is None:
        return ["%s: could not find `type :: %s` -- this check needs updating" % (path, type_name)]
    for lineno, line in body:
        code = strip_comment(line)
        if re.search(r"(^|[\s,])allocatable([\s,(]|$)", code, re.IGNORECASE):
            problems.append(
                "%s:%d: `%s` must have no allocatable component -- put the state on "
                "parquet_table_cache instead (three confirmed compiler bugs sit behind this "
                "rule; see CLAUDE.md, \"New `parquet_table` state goes on the CACHE\"):\n    %s"
                % (path.relative_to(REPO_ROOT), lineno, type_name, line.strip())
            )
    return problems


#: A pointer assignment `x => <source>`. Excludes the `=> null()` initializers and the
#: `procedure :: x => y` / `generic ::` / `final ::` binding forms, none of which are pointer
#: assignments at all.
POINTER_ASSIGN = re.compile(r"^\s*([A-Za-z_]\w*(?:\s*%\s*\w+)*(?:\([^)]*\))?)\s*=>\s*(\S.*)$")
#: A call whose receiver hands out a pointer: `call <receiver>%data_ptr(p)` / `%string_column(p)`.
POINTER_CALL = re.compile(r"^\s*call\s+(\S.*?)%(data_ptr|string_column)\s*\(", re.IGNORECASE)


def check_pointers_go_through_cache():
    """feature_risks.md Risk-12 -- a returned pointer must target cache-owned heap, never `self`."""
    problems = []
    for path in TABLE_FILES:
        for lineno, raw in enumerate(path.read_text().split("\n"), start=1):
            code = strip_comment(raw)
            stripped = code.strip()
            if not stripped or re.match(r"^\s*(procedure|generic|final|import)\b", stripped, re.I):
                continue
            source = None
            match = POINTER_ASSIGN.match(code)
            if match and "null()" not in match.group(2).lower():
                source = match.group(2)
            else:
                match = POINTER_CALL.match(code)
                if match:
                    source = match.group(1)
            if source is None:
                continue
            # Only `self`-rooted sources are in scope: a local alias or a dummy argument is not a
            # component of the caller's table, so it carries no lifetime hazard of this kind.
            if not re.match(r"^\s*self\s*%", source, re.IGNORECASE):
                continue
            if re.search(r"%\s*cache\b", source, re.IGNORECASE):
                continue
            problems.append(
                "%s:%d: a pointer handed out by a table accessor must be reached through "
                "`self%%cache`, not from `self` directly -- otherwise the caller needs `target` "
                "on their table and nothing diagnoses its absence (feature_risks.md Risk-12):\n    %s"
                % (path.relative_to(REPO_ROOT), lineno, stripped)
            )
    return problems


def check_generated_file_conventions():
    """feature_risks.md Risk-19 -- a template omission is invisible to the generators' --check modes."""
    problems = []
    for path in GENERATED_FILES:
        if not path.exists():
            problems.append("%s: listed as generated but does not exist" % path.relative_to(REPO_ROOT))
            continue
        text = path.read_text()
        lines = text.split("\n")
        if not any(marker in text for marker in GENERATED_MARKERS):
            problems.append(
                "%s:1: listed in GENERATED_FILES but carries no generated-file banner -- either "
                "the banner was dropped (edit the generator) or this file is no longer generated "
                "and should leave the list" % path.relative_to(REPO_ROOT)
            )
            continue
        for i, line in enumerate(lines):
            stripped = line.strip()
            # Every interface body needs its own doc-comment: FORD's undocumented-entity warnings
            # are opt-in and not run in CI, so an undocumented one is otherwise invisible.
            if re.match(r"^module\s+(subroutine|function)\s+\w+", stripped, re.IGNORECASE):
                j = i - 1
                while j >= 0 and not lines[j].strip():
                    j -= 1
                previous = lines[j].strip() if j >= 0 else ""
                if not previous.startswith("!>") and not previous.startswith("!!"):
                    problems.append(
                        "%s:%d: this interface body has no `!>` doc-comment -- add it to the "
                        "generator's template, not here (feature_risks.md Risk-19):\n    %s"
                        % (path.relative_to(REPO_ROOT), i + 1, stripped[:100])
                    )
            # An `end module`/`end submodule` line can never be executed, so it is excluded from
            # coverage by convention; a generator dropping the marker adds one uncoverable line per
            # file it emits.
            if re.match(r"^end\s+(module|submodule)\b", stripped, re.IGNORECASE):
                if "GCOVR_EXCL_LINE" not in line:
                    problems.append(
                        "%s:%d: an `end module`/`end submodule` line is never executed and must "
                        "carry `! GCOVR_EXCL_LINE` -- add it to the generator's template "
                        "(feature_risks.md Risk-19):\n    %s"
                        % (path.relative_to(REPO_ROOT), i + 1, stripped[:100])
                    )
    return problems


#: A procedure header. Matches both the abbreviated `module procedure NAME` form and a full
#: `[module] subroutine NAME(...)` / `function NAME(...)`, including the prefixes this project uses.
PROC_START = re.compile(
    r"^\s*(?:module\s+)?(?:pure\s+|impure\s+|elemental\s+|recursive\s+)*"
    r"(?:procedure|subroutine|function)\s+(\w+)", re.IGNORECASE)
PROC_END = re.compile(r"^\s*end\s+(?:procedure|subroutine|function)\s+(\w+)", re.IGNORECASE)


def procedure_bodies(path):
    """Yield (name, first_line_number, body_text) for every procedure in `path`.

    Nested (contained) procedures are yielded too, and their text also appears in the enclosing
    procedure's body -- which is what the callers below want: a guard is just as good in a helper
    the procedure calls inline as in the procedure itself.
    """
    lines = path.read_text().split("\n")
    stack = []
    found = []
    for lineno, line in enumerate(lines, start=1):
        match = PROC_START.match(line)
        if match and not re.match(r"^\s*end\b", line, re.IGNORECASE):
            stack.append([match.group(1), lineno, []])
            continue
        end = PROC_END.match(line)
        if end and stack and stack[-1][0].lower() == end.group(1).lower():
            name, start, body = stack.pop()
            found.append((name, start, "\n".join(body)))
            continue
        for frame in stack:
            frame[2].append(line)
    return found


def check_schemaless_write_declares_auto():
    """feature_risks.md Risk-2 -- the generated schema must never carry a MEASURED size.

    A schema-less `parquet_write_table` builds a `parquet_schema` from the resident columns'
    descriptors and declares `col_size:`/`array_size:` as `auto`, so the writer resolves both from
    the data exactly as it would with no schema at all. That is what makes the generator unable to
    get a size wrong: it never computes one. A future change that "improves" this by measuring the
    column here takes on the one job the current shape avoids -- and gets it wrong first for
    strings, whose declared width is a maximum over values it would have to scan. The sidecar MAML
    still records real numbers, because it is emitted at CLOSE, after the writer resolved them,
    which is exactly why the mistake would not show up in a round-trip test.
    """
    problems = []
    path = SRC / "parquet_tables_write.f90"
    bodies = [b for b in procedure_bodies(path) if b[0] == "build_table_schema"]
    if not bodies:
        return ["%s: could not find `build_table_schema` -- this check needs updating"
                % path.relative_to(REPO_ROOT)]
    for _name, start, body in bodies:
        for offset, line in enumerate(body.split("\n")):
            code = strip_comment(line)
            for keyword in ("col_size", "array_size"):
                for value in re.findall(r"%s\s*=\s*([A-Za-z_]\w*|\d+)" % keyword, code):
                    if value != "parquet_size_auto":
                        problems.append(
                            "%s:%d: build_table_schema must declare `%s` as `parquet_size_auto`, "
                            "never a measured value -- the writer resolves it from the data, and a "
                            "size computed here is one the generator can get wrong "
                            "(feature_risks.md Risk-2):\n    %s"
                            % (path.relative_to(REPO_ROOT), start + offset + 1, keyword,
                               code.strip())
                        )
    return problems


def check_row_group_reads_guard_against_sort():
    """feature_risks.md Risk-4 -- a row-group-scoped read must refuse while a sort is active.

    A sort permutation destroys row-group locality (sorted row 5 may come from row group 47), so
    every row-group-scoped operation has to refuse while one is installed, or it silently hands back
    rows in file order -- the right NUMBER of rows and the wrong ones. There are ~19 such sites and
    the risk is one omission.

    `check_row_group_valid` is what makes a procedure row-group-scoped in the first place: it is
    called by everything that takes a `row_group` argument, and by nothing else. So requiring the
    two calls to travel together turns "did we guard all of them?" into something mechanical, and it
    extends itself -- a new row-group-scoped read validates its row group as a matter of course, and
    is then required to carry the sort guard too.
    """
    problems = []
    for path in sorted(SRC.glob("parquet_*.f90")):
        for name, start, body in procedure_bodies(path):
            if "call check_row_group_valid" not in body:
                continue
            if "call check_reader_no_sort" in body:
                continue
            problems.append(
                "%s:%d: `%s` is row-group-scoped (it calls check_row_group_valid) but never calls "
                "check_reader_no_sort -- under a sort it would hand back rows in FILE order, with "
                "the right row count and the wrong rows (feature_risks.md Risk-4)"
                % (path.relative_to(REPO_ROOT), start, name)
            )
    return problems


def check_print_stat_columns_documented():
    """feature_risks.md Risk-5 -- print_stat's column set and its documentation must agree.

    `parquet_reader_print_stat`'s output is documented column by column in
    `doc/pages/reading.md`, and that table is the only contract it has: the format is otherwise
    asserted in exactly one place in the whole suite (a `sample:` substring), so a renamed or added
    column breaks no test at all. It had already drifted when this check was written -- the docs
    named a `prefetc` column that the code calls `fetched`, and omitted `qcmin`, `qcmax`, `qcmiss`
    and `filter` entirely -- which is the failure this exists to stop repeating.

    Comparing the two SETS rather than asserting the printed header line is deliberate: the header
    is padded to each column's widest cell, so its exact text depends on the data, and a test that
    matched it literally would be brittle in a way that teaches people to delete it.
    """
    problems = []
    cpp = SRC / "parquet_wrapper.cpp"
    doc = REPO_ROOT / "doc" / "pages" / "reading.md"
    text = cpp.read_text()
    match = re.search(r"std::vector<std::string>\s+headers\s*=\s*\{(.*?)\}\s*;", text, re.S)
    if not match:
        return ["%s: could not find print_stat's `headers` vector -- this check needs updating"
                % cpp.relative_to(REPO_ROOT)]
    code_columns = re.findall(r'"([^"]+)"', match.group(1))
    # The documented set: the first cell of every row of the column table, which spells each name
    # in backticks and pairs two of them (`min` / `max`) on one row.
    documented = set()
    for line in doc.read_text().split("\n"):
        row = re.match(r"^\s*\|\s*((?:`\w+`\s*/?\s*)+)\|", line)
        if row:
            documented.update(re.findall(r"`(\w+)`", row.group(1)))
    missing = [c for c in code_columns if c not in documented]
    extra = sorted(documented - set(code_columns))
    for column in missing:
        problems.append(
            "doc/pages/reading.md: print_stat prints a `%s` column that the documentation does not "
            "describe -- add a row for it (feature_risks.md Risk-5)" % column)
    for column in extra:
        problems.append(
            "doc/pages/reading.md: the documentation describes a `%s` column that print_stat does "
            "not print -- it was renamed or removed (feature_risks.md Risk-5)" % column)
    return problems


def check_print_settings_documented():
    """`parquet_print_settings`'s output and `doc/pages/settings.md` must name the same things.

    Same failure mode as `check_print_stat_columns_documented` above, one module over: the dump is
    a user's quickest way to see what the library will do, and the guide page is the only contract
    it has. The test suite asserts that each name *appears* in the output, which catches a dropped
    row but not an undocumented new one -- so a knob added in a later milestone can print happily
    while the page never mentions it.

    Both sides are read as SETS of names. The code side is every string literal handed to
    `print_one`; the doc side is the fenced sample dump plus the read-only limits table, which is
    where a reader actually looks. Matching sets rather than the literal block is deliberate: the
    output is column-padded, so its exact text would make this brittle in the way that gets a check
    deleted rather than fixed.
    """
    problems = []
    src = SRC / "parquet_settings.f90"
    doc = REPO_ROOT / "doc" / "pages" / "settings.md"
    printed = re.findall(r'call\s+print_one\s*\(\s*u\s*,\s*"([^"]+)"', src.read_text())
    if not printed:
        return ["%s: could not find any print_one call -- this check needs updating"
                % src.relative_to(REPO_ROOT)]
    doc_text = doc.read_text()
    for name in printed:
        if name not in doc_text:
            problems.append(
                "doc/pages/settings.md: parquet_print_settings prints `%s` but the guide page never "
                "names it -- add it to the sample dump or the limits table" % name)
    # The reverse direction: a name the page presents as printable that the dump does not print.
    # Only the fenced sample dump is checked here, since the prose legitimately names procedures
    # and concepts that are not rows of the output.
    fenced = re.search(r"```\nparquet-fortran settings\n(.*?)```", doc_text, re.S)
    if not fenced:
        return problems + ["doc/pages/settings.md: could not find the sample parquet_print_settings "
                           "output block -- this check needs updating"]
    for line in fenced.group(1).split("\n"):
        # A row is indented and carries a name plus a value; a section header ("limits
        # (read-only)") sits at column 0, which is the only thing distinguishing the two.
        if not line.startswith("  "):
            continue
        cell = line.split()
        if len(cell) == 2 and cell[0] not in printed:
            problems.append(
                "doc/pages/settings.md: the sample output shows a `%s` row that "
                "parquet_print_settings does not print -- it was renamed or removed" % cell[0])
    return problems


CHECKS = (
    ("parquet_table has no allocatable component", check_no_allocatable_component),
    ("table pointers are reached through %cache", check_pointers_go_through_cache),
    ("generated files carry their conventions", check_generated_file_conventions),
    ("the schema-less write declares auto sizes", check_schemaless_write_declares_auto),
    ("row-group reads guard against a sort", check_row_group_reads_guard_against_sort),
    ("print_stat's columns match its documentation", check_print_stat_columns_documented),
    ("print_settings matches its documentation", check_print_settings_documented),
)


def main(argv):
    if "--list" in argv:
        for name, _ in CHECKS:
            print(name)
        return 0
    if len(argv) > 1:
        print("check_source_conventions.py: unknown argument (try --list)", file=sys.stderr)
        return 2
    failures = 0
    for name, check in CHECKS:
        problems = check()
        status = "FAIL" if problems else "ok"
        print("[%s] %s" % (status, name))
        for problem in problems:
            print("  %s" % problem)
        failures += len(problems)
    if failures:
        print("\n%d violation(s) found." % failures)
        return 1
    print("\nAll %d source-convention checks passed." % len(CHECKS))
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
