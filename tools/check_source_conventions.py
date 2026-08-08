#!/usr/bin/env python3
"""Checks twelve structural invariants of this repository that no compiler and no runtime test sees.

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

  7. print_settings matches its documentation (feature_risks.md Risk-41).
     Every row parquet_print_settings emits must be named in doc/pages/settings.md. Matched by the
     SHAPE of a row call rather than by a list of helper names, because the list went blind twice --
     see check_print_settings_documented.

  8. Every setting is actually read (feature_risks.md Risk-41).
     A `cfg_*` that nothing consults still round-trips through its own getter, still reports the
     right factory value, and silently does nothing. This finds the "nothing reads it" half; the
     observed-effect tests in test/test_settings.f90 find the "reads it wrongly" half.

  9. No direct printing outside the emit channels.
     Every message goes through parquet_emit_info/_warning/_error_context, which is what makes
     `verbosity` and `message_stream` apply everywhere. A print written the old way still appears at
     default settings, so the suite stays green and only users who changed a setting are affected.

 10. The row-group sizing arithmetic exists once (feature_risks.md Risk-43).
     Its two callers serve different writers. A re-inlined copy takes the built-in constant instead
     of the setting, so parquet_set_target_row_group_bytes governs one kind of write and not the
     other -- every row correct, the row-group count wrong, nothing failing.

 11. Every settable knob has an environment variable (feature_risks.md Risk-44).
     `parquet_settings_from_env` applies one PARQUET_FORTRAN_* variable per knob. A knob left out of
     that sequence makes its variable silently do nothing -- indistinguishable, from the user's side,
     from the setting being broken. Reuses parquet_print_settings' rows as the knob list, so a new
     knob fails this, the documentation check and the is-it-read check together.

 12. src/ is a single C++ translation unit (CLAUDE.md's TU-split note).
     Every process-global in parquet_wrapper.cpp is a file-scope `static`, so a second `.cpp` would
     get its own copy of each. Not a ban on splitting -- the note firing at the moment someone does.

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
TEST = REPO_ROOT / "test"
TOOLS = REPO_ROOT / "tools"

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
    # Matched by SHAPE (`call print_<anything>(u, "name"`), not against a list of helper names. An
    # earlier version named them, and silently went blind twice: once when print_text arrived for
    # the non-integer rows, and again when print_big arrived for the int64 ones -- on that occasion
    # it reported two of five new rows as undocumented and passed the other three, which is worse
    # than failing outright. A new row helper is now picked up with no edit here.
    printed = re.findall(r'call\s+print_\w+\s*\(\s*u\s*,\s*"([^"]+)"', src.read_text())
    if not printed:
        return ["%s: could not find any `call print_*(u, \"name\"` row -- either the helpers were "
                "renamed to a different shape, or parquet_print_settings no longer prints rows this "
                "way; this check needs updating" % src.relative_to(REPO_ROOT)]
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


def check_settings_are_read():
    """feature_risks.md Risk-41 -- a setting that is stored but never read passes every test.

    A `parquet_settings` knob is a module variable that some *other* procedure has to consult. If
    nothing ever reads it -- the read site was never added, or was dropped in a later refactor --
    then `set` followed by `get` still returns what was set, the factory-default assertion still
    holds, and the knob silently does nothing. That is worse than a knob that visibly fails.

    The rule: every `cfg_*` variable must be READ somewhere other than the procedure that writes it.
    Deliberately not "read outside the module" -- `cfg_arrow_threads_initial` is written by
    parquet_set_arrow_threads and read by parquet_reset_settings, which is a legitimate shape, and a
    file-scoped rule would need a false exemption for it on day one.

    This catches only "nothing reads it". Whether the read site does the right thing is what the
    per-knob observed-effect tests in test/test_settings.f90 are for; neither check subsumes the
    other.
    """
    problems = []
    settings = SRC / "parquet_settings.f90"
    text = settings.read_text()
    names = re.findall(r"^\s*(?:integer|logical|character\([^)]*\)|real\([^)]*\))\s*,\s*save\s*::\s*(cfg_\w+)",
                       text, re.M)
    if not names:
        return ["%s: found no `cfg_* ` settings variables -- this check needs updating"
                % settings.relative_to(REPO_ROOT)]
    # Where each name is assigned, so an assignment does not count as a read of itself.
    sources = sorted(SRC.glob("*.f90"))
    for name in names:
        read_somewhere = False
        for path in sources:
            for line in path.read_text().split("\n"):
                code = strip_comment(line)
                if name not in code:
                    continue
                # An assignment `name = ...` is a write; anything else mentioning it is a read.
                if re.match(r"^\s*%s\s*=(?!=)" % re.escape(name), code):
                    continue
                read_somewhere = True
                break
            if read_somewhere:
                break
        if not read_somewhere:
            problems.append(
                "src/parquet_settings.f90: `%s` is written but never read -- the setting it backs "
                "does nothing, and a set/get test would not notice (feature_risks.md Risk-41)" % name)
    return problems


#: Procedures allowed to write to a unit directly: the emit channels themselves, and the solicited
#: printers, which format many lines to a caller-chosen unit and ask parquet_output_is_suppressed
#: rather than routing through a channel. Every other write in src/ has to go through a channel, or
#: the two output settings silently do not apply to it.
DIRECT_PRINT_ALLOWED = {
    "parquet_emit_info", "parquet_emit_warning", "parquet_emit_error_context",
    "parquet_print_settings", "print_one", "print_text",
    "table_print_stat", "schema_print_schema_info", "col_print", "psv_print",
}


def check_no_direct_printing():
    """A warning printed directly ignores both output settings, and nothing else would notice.

    After S3 every message the library emits goes through one of three channels in
    parquet_settings, which is what makes `verbosity` and `message_stream` apply everywhere without
    each call site testing them. A new warning written the old way -- `print '(a)', "WARNING: ..."`
    -- still appears at default settings, so the test suite stays green; it is only wrong for the
    users who changed a setting, and only in a way nobody runs into until they do.

    That makes it a static-check problem rather than a test problem: the property is "no site does
    this", which a test cannot express.

    The allow-list is the solicited printers plus the channels themselves. It is small and changes
    about as often as the library gains a printer; adding to it should be a deliberate act, which is
    why it lives here next to the reason rather than as a marker comment at the site.
    """
    problems = []
    pattern = re.compile(r"^\s*(?:print\s*[\'\"(]|write\s*\(\s*(?:\*|output_unit|error_unit)\s*,)")
    proc_start = re.compile(
        r"^\s*(?:module\s+procedure\s+(\w+)|(?:recursive\s+|pure\s+|impure\s+|elemental\s+)*"
        r"(?:module\s+)?(?:subroutine|function)\s+(\w+)"
        r"|(?:integer|logical|real|character)[^:]*::\s*(\w+)\s*\()")
    for path in sorted(SRC.glob("*.f90")):
        current = ""
        for lineno, line in enumerate(path.read_text().split("\n"), start=1):
            m = proc_start.match(line)
            if m:
                current = next(g for g in m.groups() if g) if any(m.groups()) else current
            code = strip_comment(line)
            if not pattern.match(code):
                continue
            if current in DIRECT_PRINT_ALLOWED:
                continue
            problems.append(
                "%s:%d: writes to a unit directly inside `%s` -- route it through "
                "parquet_emit_warning/_info/_error_context, or both output settings silently will "
                "not apply to it:\n    %s"
                % (path.relative_to(REPO_ROOT), lineno, current or "<file scope>", line.strip()))
    return problems


#: The four constants that make up the row-group sizing arithmetic in src/parquet_wrapper.cpp. Each
#: may be DEFINED exactly once, however many times it is read.
ROW_GROUP_SIZING_CONSTANTS = (
    "kTargetRowGroupBytes", "kMinAutoChunkSizeRows", "kMaxAutoChunkSizeRows",
    "kMaxFloorOvershootFactor",
)


def check_row_group_sizing_not_duplicated():
    """feature_risks.md Risk-43 -- a second copy of the sizing arithmetic ignores the setting.

    Row groups are sized from a byte target by chunk_size_from_bytes_per_row, and it has two callers
    that serve DIFFERENT writers: close_parquet_writer's whole-table write, and
    estimate_chunk_size_from_schema's estimate for the streaming parquet_new_row_group path. Until
    S4 the first of those restated the whole function body inline, four constants included.

    That is a silent failure rather than a visible one. A re-inlined copy takes the built-in
    constant instead of g_target_row_group_bytes, so parquet_set_target_row_group_bytes governs one
    kind of write and not the other: the files come out with the wrong number of row groups, every
    row is present and correct, and nothing fails.

    The rule is therefore about DEFINITIONS, not uses -- reading a constant in ten places is fine,
    declaring it in two is the bug.
    """
    problems = []
    path = SRC / "parquet_wrapper.cpp"
    text = path.read_text()
    for name in ROW_GROUP_SIZING_CONSTANTS:
        pattern = re.compile(r"^\s*static\s+constexpr\s+\w+(?:\s+\w+)*\s+%s\s*=" % re.escape(name), re.M)
        hits = pattern.findall(text)
        if len(hits) > 1:
            problems.append(
                "src/parquet_wrapper.cpp: `%s` is defined %d times -- the row-group sizing "
                "arithmetic must exist once, in chunk_size_from_bytes_per_row, or "
                "parquet_set_target_row_group_bytes silently governs only some writes "
                "(feature_risks.md Risk-43)" % (name, len(hits)))
        elif not hits:
            problems.append(
                "src/parquet_wrapper.cpp: `%s` is not defined at all -- if the row-group sizing "
                "constants were renamed, update ROW_GROUP_SIZING_CONSTANTS in this script" % name)
    return problems


def check_env_covers_every_setting():
    """feature_risks.md Risk-44 -- a knob with no environment variable is silently unreachable.

    `parquet_settings_from_env` applies one `PARQUET_FORTRAN_*` variable per knob. A knob left out
    of that sequence is not a compile error and not a test failure: the variable simply does
    nothing, which from the user's side is indistinguishable from the setting not working at all.

    The rule reuses `parquet_print_settings`' own rows as the list of knobs -- the same list
    `check_print_settings_documented` matches against the guide -- so the three checks agree by
    construction about what a knob is, and a knob added in a future milestone fails all three at
    once: undocumented, unread, and unreachable from the environment.

    Only the SETTINGS rows count. The read-only `parquet_max_*` limits are printed by the same
    procedure and are deliberately not settable from anywhere, so they are excluded by name rather
    than by position (a positional split would break the first time the two sections are reordered).
    """
    problems = []
    src = SRC / "parquet_settings.f90"
    text = src.read_text()
    rows = re.findall(r'call\s+print_\w+\s*\(\s*u\s*,\s*"([^"]+)"', text)
    knobs = [r for r in rows if not r.startswith("parquet_max_")]
    if not knobs:
        return ["src/parquet_settings.f90: found no settings rows in parquet_print_settings -- "
                "this check needs updating"]
    body = re.search(r"subroutine parquet_settings_from_env\b(.*?)end subroutine parquet_settings_from_env",
                     text, re.S)
    if body is None:
        return ["src/parquet_settings.f90: could not find parquet_settings_from_env -- this check "
                "needs updating"]
    for knob in knobs:
        var = "PARQUET_FORTRAN_" + knob.upper()
        if var not in body.group(1):
            problems.append(
                "src/parquet_settings.f90: `%s` is a settable knob but parquet_settings_from_env "
                "never reads `%s` -- the variable would silently do nothing, which a user cannot "
                "tell apart from the setting being broken (feature_risks.md Risk-44)" % (knob, var))
    return problems


def check_single_cpp_translation_unit():
    """CLAUDE.md's TU-split note -- splitting parquet_wrapper.cpp silently forks every file-scope
    `static`, and nothing diagnoses it.

    Splitting the file was considered and deliberately declined (CONTRIBUTING.md's "Features
    considered but not implemented"). This check exists because the hazard is invisible if it ever
    IS revisited: every process-global `static` at file scope means one instance PER TRANSLATION
    UNIT, so a second `.cpp` gets its own copy of every one. It compiles, it links, and there is no
    warning.

    Two families are affected, and the second is the worse one:

      * the `g_debug_*` test-only overrides -- a `parquet_debug_set_*` setter would write to a
        different object than the guard reads, so the override stops working and the error scenario
        that depends on it starts testing nothing while reporting green;
      * the settings mirrored from parquet_settings (`g_verbosity`, `g_message_stream`,
        `g_sort_parallel_min_rows`, `g_sort_counting_path`, `g_sort_counting_bucket_limit`,
        `g_target_row_group_bytes`, `g_statistics_prescreen`) -- a user's setting would apply to
        some of the library and not the rest, with the Fortran getters still reporting it correctly
        (feature_risks.md Risk-42).

    So this check is not a ban on splitting the file. It is the note firing at the moment somebody
    does it: promote both families to `extern` globals with one definition in a shared internal
    header, re-run the error scenarios and test/test_settings.f90's observed-effect tests, then
    update this check to match the new layout.
    """
    units = sorted(SRC.glob("*.cpp"))
    if len(units) == 1:
        return []
    if not units:
        return ["src/: no .cpp translation unit found -- this check needs updating"]
    return [
        "src/: %d C++ translation units (%s), but every process-global in parquet_wrapper.cpp is a "
        "file-scope `static`, so each unit now has its OWN copy of the g_debug_* overrides and of "
        "the settings mirrored from parquet_settings -- read CLAUDE.md's \"If "
        "src/parquet_wrapper.cpp is ever split into multiple translation units\" before going "
        "further, then promote both families to extern and update this check"
        % (len(units), ", ".join(u.name for u in units))
    ]


def check_no_per_element_shared_ptr():
    """A per-ELEMENT helper in parquet_wrapper.cpp must take `const arrow::Array *`, never a
    `const std::shared_ptr<arrow::Array> &`.

    A `std::shared_ptr` parameter looks free and is not: every `std::static_pointer_cast` inside
    such a helper builds a new shared_ptr, which is an atomic increment and an atomic decrement.
    In a helper called once per row that is the dominant cost of the whole operation --
    `real_family_value_at` measured at **13.6 ns per row** to read one double this way, against
    **1.8 ns** once it took a raw pointer, and `evaluate_nodes` was 75-93% of the cost of installing
    a row filter (feature_table_parallel.md section 17.10).

    **Nothing fails when this is undone.** Every answer stays identical and every test still passes;
    only the clock moves, and only under a benchmark nobody runs by default. That is exactly why it
    is a lint check rather than a comment -- and why the check keys on the SHAPE (a parameter pair
    of an array and an index) rather than on a list of helper names, which would go blind to the
    next one added (see CLAUDE.md's "A static check that enumerates names goes stale silently").

    The caller always owns a reference for the duration of the loop, so the loop needs the pointer,
    not a share of the ownership. If a genuinely per-element helper ever does need to extend the
    array's lifetime, it is the exception that should be argued in a comment -- and this check
    updated to name it -- rather than silently reverted.
    """
    path = SRC / "parquet_wrapper.cpp"
    if not path.exists():
        return ["src/parquet_wrapper.cpp: not found -- this check needs updating"]
    text = path.read_text(encoding="utf-8", errors="replace")
    # A per-element helper is one taking an Arrow array AND an element index. Matched by shape:
    # `(const std::shared_ptr<arrow::Array> &<name>, int64_t <idx>` on one line.
    pattern = re.compile(
        r"^\s*static\s+[^\n(]*?\b(\w+)\s*\(\s*const\s+std::shared_ptr<arrow::Array>\s*&\s*\w+\s*,"
        r"\s*(?:const\s+)?int(?:64_t|32_t)?\s+\w*(?:idx|index|i)\b",
        re.MULTILINE,
    )
    problems = []
    for match in pattern.finditer(text):
        line = text.count("\n", 0, match.start()) + 1
        problems.append(
            "src/parquet_wrapper.cpp:%d: `%s` takes an Arrow array by const shared_ptr& alongside "
            "an element index, which makes it a per-element helper paying two atomic refcount "
            "operations per call -- take `const arrow::Array *` instead (measured 13.6 ns/row vs "
            "1.8 ns/row; see feature_table_parallel.md section 17.10)" % (line, match.group(1))
        )
    return problems


def check_no_per_element_string_alloc():
    """No bulk loop anywhere in src/ may materialize string elements through `%get`/`%to_string`.

    Those return a `character(len=:), allocatable`, so calling one per element turns an O(payload)
    byte copy into one heap allocation, one fill, one copy and one free **per row**. Measured on
    4 M elements: `%to_character` 0.168 s -> 0.038 s (4.3x), `%build_from` 0.305 s -> 0.033 s (9.3x),
    and the sort-key extraction 1.31x-1.58x once it stopped doing this. One allocation per element
    costs roughly 0.11 s per 4 M elements, and on a large machine per-element overhead measured
    ~1.8x worse still.

    **Nothing fails when this comes back.** Results stay byte-identical, the suite stays green, and
    the allocating form reads as ordinary idiomatic Fortran -- only a benchmark notices. That is what
    makes it a lint check rather than a comment, and it is the Fortran twin of
    `check_no_per_element_shared_ptr` above (feature_risks.md Risk-60).

    The rule is about the LOOP, not about `%get`, which is exactly right for its own job of returning
    one element. Alternatives that need no allocation: `elem_bounds` inside `parquet_strings` itself,
    `%copy_buffers` for a bulk consumer that wants the packed layout, and `%compare(i, j)` for a scan
    that only needs an ordering.

    **This is a RATCHET, not a clean-slate rule.** `KNOWN_REMAINING` below records how many instances
    each file still has. A file gaining one fails the check; a file whose count drops fails it too,
    with a note to lower the number. So the list can only shrink, a new consumer anywhere in `src/`
    is caught immediately, and nothing goes stale silently -- which a bare allowlist would
    (CLAUDE.md, "A static check that enumerates names goes stale silently").

    The remaining entries are work, not exemptions. Two shapes among them are worth knowing before
    picking one up: a loop that only needs an ORDERING can use `%compare(i, j)`, and one that wants
    the whole packed payload can use `%copy_buffers`. The one genuine hold-out is
    `parquet_check_qc_string_compact` (`parquet_write_string.f90`), whose loop needs each element's
    VALUE to pass to `parquet_qc_string_satisfies` against a declared bound -- removing that
    allocation needs a second comparison primitive and surgery on a qc helper shared with the padded
    string path, behind three early-return guards. See feature_string_parallel.md S10.
    """
    # file -> instances still to be converted. Lower a number when you fix one; never raise one.
    KNOWN_REMAINING = {
        "parquet_tables_access.f90": 13,
        "parquet_write_string.f90": 3,
        "parquet_sorting_unique.f90": 1,
        "parquet_columns_string.f90": 1,
    }
    problems = []
    found = {}
    call_re = re.compile(r"call\s+\w+(?:%\w+)*%(get_i32|get_i64|get|to_string)\s*\(", re.I)
    do_re = re.compile(r"^\s*(?:\w+\s*:\s*)?do\b", re.I)
    enddo_re = re.compile(r"^\s*end\s*do\b", re.I)
    proc_re = re.compile(
        r"^\s*(?:pure\s+|elemental\s+|impure\s+|recursive\s+|module\s+)*"
        r"(?:subroutine|function)\s+(\w+)", re.I)
    endproc_re = re.compile(r"^\s*end\s+(?:subroutine|function)\b", re.I)
    # Only files that actually handle parquet_string_column storage can trip this; scanning all of
    # src/ is deliberate, so a NEW consumer in a file nobody thought of is caught too.
    for path in sorted(SRC.glob("*.f90")):
        lines = path.read_text(encoding="utf-8", errors="replace").splitlines()
        depth = 0
        proc = ""
        for n, line in enumerate(lines, 1):
            code = line.split("!", 1)[0]
            m = proc_re.match(code)
            if m:
                proc = m.group(1)
            elif endproc_re.match(code):
                proc = ""
            if do_re.match(code):
                depth += 1
            elif enddo_re.match(code):
                depth = max(0, depth - 1)
            elif depth > 0 and call_re.search(code):
                found.setdefault(path.name, []).append((n, proc or "this loop"))
    for name, hits in sorted(found.items()):
        allowed = KNOWN_REMAINING.get(name, 0)
        if len(hits) > allowed:
            for n, proc in hits[allowed:]:
                problems.append(
                    "src/%s:%d: `%s` materializes a string element through `%%get`/`%%to_string` "
                    "inside a loop, which allocates a deferred-length string per row -- use "
                    "`%%copy_buffers` for a bulk copy, `%%compare` for an ordering scan, or "
                    "`elem_bounds` inside parquet_strings (see feature_risks.md Risk-60)"
                    % (name, n, proc)
                )
    for name, allowed in sorted(KNOWN_REMAINING.items()):
        actual = len(found.get(name, []))
        if actual < allowed:
            problems.append(
                "src/%s: only %d per-element string allocation(s) remain but "
                "check_no_per_element_string_alloc's KNOWN_REMAINING still says %d -- lower it, so "
                "the ratchet cannot slip back" % (name, actual, allowed)
            )
    return problems


def check_scenario_list_is_complete():
    """Every scenario error_scenarios.f90 dispatches on must be named in run_error_scenarios.sh.

    The shell runner's `scenarios=(...)` array is documented as a complete, independently
    maintained mirror of that `select case`, and two things now depend on it being one:

      * CI runs the script, which is what proves each *listed* name is still recognized (an
        unrecognized one exits with the 97 sentinel);
      * test_errors.f90's prime_error_scenarios reads the same array to decide what to pre-run in
        parallel, which is where a full `fpm test`'s speedup comes from.

    Neither covers the other direction. A scenario added to error_scenarios.f90 and driven from a
    test, but never added to the array, is invisible to the script and silently forfeits priming --
    it just falls back to spawning on demand, so nothing fails and nothing says anything. That is
    the drift this check closes.

    The names are derived by SHAPE rather than from a list kept here: the `select case
    (trim(scenario))` block is located and its `case ("...")` labels read until `case default`, so
    a new scenario is picked up with no edit (CLAUDE.md, "A static check that enumerates names goes
    stale silently"). An empty result is treated as a failure for the same reason -- it means the
    dispatch moved, not that the invariant holds.

    `concurrency_scenarios=(...)` counts as listed too: those genuinely need a real OpenMP race and
    are deliberately excluded from priming, but they are still named in the script and still run.
    """
    dispatch_file = TEST / "error_scenarios.f90"
    runner = TOOLS / "run_error_scenarios.sh"
    for path in (dispatch_file, runner):
        if not path.is_file():
            return ["%s: not found -- this check needs updating" % path.relative_to(REPO_ROOT)]

    lines = dispatch_file.read_text(encoding="utf-8").splitlines()
    start = None
    for i, line in enumerate(lines):
        if re.match(r"\s*select case\s*\(\s*trim\(\s*scenario\s*\)\s*\)\s*$", line):
            start = i
            break
    if start is None:
        return [
            "test/error_scenarios.f90: no `select case (trim(scenario))` dispatch found -- the "
            "scenario dispatch moved or was renamed, so this check can no longer see it"
        ]

    dispatched = []
    for line in lines[start + 1:]:
        if re.match(r"\s*case\s+default\s*$", line):
            break
        found = re.match(r'\s*case\s*\(\s*"([a-z0-9_]+)"\s*\)\s*$', line)
        if found:
            dispatched.append(found.group(1))
    if not dispatched:
        return [
            "test/error_scenarios.f90: the `select case (trim(scenario))` dispatch yielded no "
            "`case (\"...\")` labels -- its shape changed, so this check is now blind"
        ]

    listed = set(re.findall(r'"([a-z0-9_]+):[01]"', runner.read_text(encoding="utf-8")))
    listed |= set(re.findall(r'^\s*"([a-z0-9_]+)"\s*$', runner.read_text(encoding="utf-8"), re.M))
    missing = [name for name in dispatched if name not in listed]
    if not missing:
        return []
    return [
        "tools/run_error_scenarios.sh: %d scenario(s) dispatched by test/error_scenarios.f90 are "
        "not named in its scenarios=() or concurrency_scenarios=() arrays, so the script does not "
        "run them and test_errors.f90's prime_error_scenarios cannot pre-run them: %s"
        % (len(missing), ", ".join(missing))
    ]


CHECKS = (
    ("parquet_table has no allocatable component", check_no_allocatable_component),
    ("table pointers are reached through %cache", check_pointers_go_through_cache),
    ("generated files carry their conventions", check_generated_file_conventions),
    ("the schema-less write declares auto sizes", check_schemaless_write_declares_auto),
    ("row-group reads guard against a sort", check_row_group_reads_guard_against_sort),
    ("print_stat's columns match its documentation", check_print_stat_columns_documented),
    ("print_settings matches its documentation", check_print_settings_documented),
    ("every setting is actually read", check_settings_are_read),
    ("no direct printing outside the emit channels", check_no_direct_printing),
    ("the row-group sizing arithmetic exists once", check_row_group_sizing_not_duplicated),
    ("src/ is a single C++ translation unit", check_single_cpp_translation_unit),
    ("every setting has an environment variable", check_env_covers_every_setting),
    ("no per-element helper takes a shared_ptr", check_no_per_element_shared_ptr),
    ("no per-element string allocation in a bulk loop", check_no_per_element_string_alloc),
    ("every error scenario is named in the shell runner", check_scenario_list_is_complete),
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
