#!/usr/bin/env python3
"""Checks structural invariants of this repository that no compiler and no runtime test sees.

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
     `doc/pages/io/reading.md` documents the reader's stats table column by column, and that table is
     the only contract the format has -- exactly one assertion in the whole suite touches the output
     text, so a renamed or added column breaks no test. The two had already drifted when this check
     was written (the docs named a `prefetc` column the code calls `fetched`, and omitted four
     others), which is the failure it exists to stop repeating.

  7. print_settings matches its documentation (feature_risks.md Risk-41).
     Every row parquet_print_settings emits must be named in doc/pages/operating/settings.md. Matched by the
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

 13. Every `intent(inout)` temporal setter assigns every component (feature_risks.md Risk-70).
     The setters on parquet_date/parquet_time/parquet_timestamp take `class(...), intent(inout)`
     rather than `intent(out)`, because a POLYMORPHIC `intent(out)` dummy makes the compiler
     default-initialise the element through the runtime on every elemental call -- measured at 3.6x
     on a whole date-column read. That trade moves an obligation from the compiler to the source:
     `intent(out)` reset every component for free, whereas under `intent(inout)` a component a
     setter does not assign silently keeps the element's PREVIOUS value. Adding a component to one
     of these types is what makes that bite, and it bites in every setter at once. Both sides are
     derived from the source here -- the component list from the type declaration, the setter list
     from the declarations themselves -- so a new component or a new setter is covered with no edit.

     The check also enforces the converse, which is the half that protects correctness rather than
     speed: a setter with a caught-failure `return` that leaves `self` unassigned must KEEP
     `intent(out)`, because that is precisely what makes a failed `%parse` (or a null-propagating
     `%set`) yield a null element instead of a stale one.

 14. The doc/pages index files agree with the page tree.
     The user guide is nested (group directories with their own index.md), and FORD renders a page
     only if the `ordered_subpage:` chain reaches it -- a group directory with no index.md is
     skipped SILENTLY: warning on stderr, exit 0, every page in the group absent from the
     published site (verified against FORD 7.0.13). Each index also mirrors its frontmatter order
     in a hand-written bullet list, and the top-level index carries a flat every-page list on top
     of that -- eight hand-synced pairs in all, none visible to any compiler or test. Both sides
     of every comparison are derived from the filesystem, never from a hardcoded page list.

 15. No doc/pages code fence is indented.
     python-markdown -- the engine FORD drives -- does not recognise a ``` fence carrying any
     leading whitespace: the fence is emitted literally inside a `<p>` and the example is published
     as running prose. Indenting an example under the bullet it belongs to is what every other
     markdown renderer wants, FORD exits 0, and nothing reads the generated HTML, so the only
     evidence is the published page. Six blocks across three pages had shipped that way.

The numbered notes above are the ones whose rationale needs more than a line; they are NOT the
complete list, and deliberately carry no count, because a hardcoded one drifts silently every time a
check is added (this docstring said "twelve" while CHECKS held fifteen). `--list` prints all of them.

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


#: The files holding `parquet_column`'s own implementation, typed tier included.
COLUMN_FILES = sorted(SRC.glob("parquet_columns*.f90"))

#: Prefix of the typed (non-polymorphic) accessor tier -- see feature_ifx.md.
TYPED_PREFIX = "parquet_column_"

#: The bindings whose typed twin exists, i.e. the ones a per-cell path must NOT reach through a
#: type-bound call. Derived by shape below rather than trusted from here: this list is only used
#: to phrase the message, and an entry with no matching `parquet_column_<name>` generic is
#: reported, so the list cannot quietly go stale in either direction.
TYPED_TWINNED_BINDINGS = (
    "get_at", "set_at", "get_elem", "set_elem",
    "is_null", "set_null", "clear_null", "data_ptr", "string_column",
)


def _typed_generics_declared():
    """The `parquet_column_*` generic names `parquet_columns` actually exports."""
    text = (SRC / "parquet_columns.f90").read_text()
    return set(re.findall(r"^\s*interface\s+(" + TYPED_PREFIX + r"\w+)\s*$", text, re.M)) | \
        set(re.findall(r"^\s*public\s*::\s*(" + TYPED_PREFIX + r"\w+)\s*$", text, re.M))


def _class_column_procedures():
    """Module procedures declared with a `class(parquet_column)` dummy.

    A typed body calling one of these hands a `type(parquet_column)` to a `class` dummy, which is
    the exact conversion the typed tier exists to remove -- so it silently restores the whole cost.
    """
    text = (SRC / "parquet_columns.f90").read_text()
    names = set()
    current = None
    for line in text.split("\n"):
        code = strip_comment(line)
        header = re.match(r"^\s*module\s+(?:subroutine|function)\s+(\w+)\s*\(", code, re.I)
        if header:
            current = header.group(1)
            continue
        if re.match(r"^\s*end\s+(?:subroutine|function)\b", code, re.I):
            current = None
            continue
        if current and re.search(r"^\s*class\s*\(\s*parquet_column\s*\)", code, re.I):
            names.add(current)
    return names


def _column_bindings():
    """Every type-bound name on `parquet_column` (`generic ::` names and `procedure ::` bindings).

    Reads the type's `contains` section, which is exactly the part `type_body_lines` stops at --
    that helper answers "which COMPONENTS does this type have", and this needs the bindings.
    """
    lines = (SRC / "parquet_columns.f90").read_text().split("\n")
    names, inside = set(), False
    for line in lines:
        code = strip_comment(line)
        if re.match(r"^\s*type\s*(,\s*[^:]*)?::\s*parquet_column\s*$", code, re.I):
            inside = True
            continue
        if not inside:
            continue
        if re.match(r"^\s*end\s+type\b", code, re.I):
            break
        match = re.match(r"^\s*generic\s*::\s*(\w+)\s*=>", code, re.I)
        if match:
            names.add(match.group(1).lower())
            continue
        match = re.match(r"^\s*procedure\s*(?:,[^:]*?)?\s*::\s*(\w+)", code, re.I)
        if match:
            names.add(match.group(1).lower())
    return names


def check_no_type_bound_column_access():
    """feature_ifx.md -- no per-cell path may reach a `type(parquet_column)` through a binding.

    Two halves, because the cost comes back through either one and NOTHING fails when it does:
    every answer stays correct, every test stays green, and only an ifx `objdump` store count
    shows the ~35 ns per call returning.

      1. The TABLE layer must call the typed generics, never `%values%<binding>`.
      2. The typed tier itself must call the TYPED guards and helpers, never a `class`-dummy
         procedure and never a binding on its own `col` dummy.

    Matched by shape, not from an enumerated list of procedure names, per this file's own rule.
    """
    problems = []
    generics = _typed_generics_declared()
    for name in TYPED_TWINNED_BINDINGS:
        if TYPED_PREFIX + name not in generics:
            problems.append(
                "src/parquet_columns.f90: no public `%s%s` -- this check's binding list has gone "
                "stale, or the typed tier lost a family (feature_ifx.md)" % (TYPED_PREFIX, name)
            )
    # An empty derivation means the source moved, not that the invariant holds -- fail rather than
    # report a vacuous [ok] (this file's own "a static check that enumerates names goes stale
    # silently" rule; both sets came back empty once during development).
    class_procs = _class_column_procedures()
    bindings = _column_bindings()
    if not class_procs:
        problems.append("src/parquet_columns.f90: found no `class(parquet_column)` procedure -- "
                        "this check can no longer see the spec and is passing vacuously")
    if not bindings:
        problems.append("src/parquet_columns.f90: found no type-bound procedure on parquet_column "
                        "-- this check can no longer see the type and is passing vacuously")

    # 1. The table layer.
    binding_re = re.compile(r"%\s*values\s*%\s*(" + "|".join(TYPED_TWINNED_BINDINGS) + r")\s*\(", re.I)
    for path in TABLE_FILES:
        for lineno, raw in enumerate(path.read_text().split("\n"), start=1):
            match = binding_re.search(strip_comment(raw))
            if match:
                problems.append(
                    "%s:%d: reaches column storage through the type-bound `%%values%%%s(...)`. "
                    "Call `%s%s(<designator>%%values, ...)` instead -- a `type(parquet_column)` "
                    "actual passed to a `class` dummy makes ifx build a runtime type descriptor in "
                    "this procedure's prologue, unconditionally, on every call (~35 ns; "
                    "feature_ifx.md). Nothing fails if this regresses:\n    %s"
                    % (path.relative_to(REPO_ROOT), lineno, match.group(1), TYPED_PREFIX,
                       match.group(1), raw.strip())
                )

    # 2. The typed tier itself.
    for path in COLUMN_FILES:
        text = path.read_text()
        # Bodies live in the SUBMODULES; `parquet_columns.f90` is the spec and holds only
        # declarations -- including `module procedure <name>` lines inside generic interface
        # blocks, which are not bodies at all and would otherwise be read as one.
        if not re.search(r"^\s*submodule\s*\(\s*parquet_columns\s*\)", text, re.M | re.I):
            continue
        current = None
        for lineno, raw in enumerate(text.split("\n"), start=1):
            code = strip_comment(raw)
            header = re.match(r"^\s*module\s+procedure\s+(\w+)\s*$", code, re.I)
            if header:
                current = header.group(1)
                continue
            if re.match(r"^\s*end\s+(?:procedure|interface)\b", code, re.I):
                current = None
                continue
            if not current or not current.lower().startswith(TYPED_PREFIX):
                continue
            for called in re.findall(r"\b(\w+)\s*\(", code):
                if called in class_procs:
                    problems.append(
                        "%s:%d: typed procedure `%s` calls `%s`, which takes a "
                        "`class(parquet_column)` dummy -- that rebuilds the descriptor block this "
                        "tier exists to remove. Call the `%s%s` form (feature_ifx.md §7.2):\n    %s"
                        % (path.relative_to(REPO_ROOT), lineno, current, called,
                           TYPED_PREFIX, called, raw.strip())
                    )
            for bound in re.findall(r"\bcol\s*%\s*(\w+)\s*\(", code, re.I):
                if bound.lower() in bindings:
                    problems.append(
                        "%s:%d: typed procedure `%s` makes the type-bound call `col%%%s(...)` on "
                        "its own non-polymorphic dummy, which is the conversion this tier exists "
                        "to remove. Call the typed form instead (feature_ifx.md §8.2):\n    %s"
                        % (path.relative_to(REPO_ROOT), lineno, current, bound, raw.strip())
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
    `doc/pages/io/reading.md`, and that table is the only contract it has: the format is otherwise
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
    doc = REPO_ROOT / "doc" / "pages" / "io" / "reading.md"
    text = cpp.read_text()
    match = re.search(r"std::vector<std::string>\s+headers\s*=\s*\{(.*?)\}\s*;", text, re.S)
    if not match:
        return ["%s: could not find print_stat's `headers` vector -- this check needs updating"
                % cpp.relative_to(REPO_ROOT)]
    code_columns = re.findall(r'"([^"]+)"', match.group(1))
    # The documented set: the first cell of every row of print_stat's OWN column table, which
    # spells each name in backticks and pairs two of them (`min` / `max`) on one row.
    #
    # Anchored on that table's `| column | meaning |` header rather than on the row shape alone.
    # The shape is not distinctive -- any two-column table whose first cell is one backticked word
    # matches it -- so an unanchored scan reads unrelated tables on the same page as if they were
    # this one, and reports their rows as columns print_stat has stopped printing. That happened as
    # soon as the page gained a physical-type mapping table.
    lines = doc.read_text().split("\n")
    start = None
    for i, line in enumerate(lines):
        if re.match(r"^\s*\|\s*column\s*\|\s*meaning\s*\|\s*$", line):
            start = i + 1
            break
    if start is None:
        return ["doc/pages/io/reading.md: could not find print_stat's `| column | meaning |` table "
                "header -- the documentation moved and this check has gone blind on it"]
    documented = set()
    for line in lines[start:]:
        if not line.strip().startswith("|"):
            break
        row = re.match(r"^\s*\|\s*((?:`\w+`\s*/?\s*)+)\|", line)
        if row:
            documented.update(re.findall(r"`(\w+)`", row.group(1)))
    missing = [c for c in code_columns if c not in documented]
    extra = sorted(documented - set(code_columns))
    for column in missing:
        problems.append(
            "doc/pages/io/reading.md: print_stat prints a `%s` column that the documentation does not "
            "describe -- add a row for it (feature_risks.md Risk-5)" % column)
    for column in extra:
        problems.append(
            "doc/pages/io/reading.md: the documentation describes a `%s` column that print_stat does "
            "not print -- it was renamed or removed (feature_risks.md Risk-5)" % column)
    return problems


def check_print_settings_documented():
    """`parquet_print_settings`'s output and `doc/pages/operating/settings.md` must name the same things.

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
    doc = REPO_ROOT / "doc" / "pages" / "operating" / "settings.md"
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
                "doc/pages/operating/settings.md: parquet_print_settings prints `%s` but the guide page never "
                "names it -- add it to the sample dump or the limits table" % name)
    # The reverse direction: a name the page presents as printable that the dump does not print.
    # Only the fenced sample dump is checked here, since the prose legitimately names procedures
    # and concepts that are not rows of the output.
    fenced = re.search(r"```\nparquet-fortran settings\n(.*?)```", doc_text, re.S)
    if not fenced:
        return problems + ["doc/pages/operating/settings.md: could not find the sample parquet_print_settings "
                           "output block -- this check needs updating"]
    for line in fenced.group(1).split("\n"):
        # A row is indented and carries a name plus a value; a section header ("limits
        # (read-only)") sits at column 0, which is the only thing distinguishing the two.
        if not line.startswith("  "):
            continue
        cell = line.split()
        if len(cell) == 2 and cell[0] not in printed:
            problems.append(
                "doc/pages/operating/settings.md: the sample output shows a `%s` row that "
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
        `g_sort_counting_path`, `g_sort_counting_bucket_limit`,
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


def check_parquet_strings_stays_leaf():
    """`parquet_strings` must not reach `parquet_bindings`, however indirectly.

    The module is documented (doc/pages/types/string-columns.md) as usable without the
    Arrow/Parquet C++ stack: a project that wants compact string storage and nothing else can
    depend on it alone. That promise is a property of the `use` GRAPH, not of this file's own
    imports, and it has already been broken once by a one-line import that looked harmless --
    `use parquet_settings` (for the verbosity flag and the thread cap), where `parquet_settings`
    imports `parquet_bindings` in order to mirror the C++-side knobs.

    **Nothing catches that.** It compiles cleanly, every test passes, and the whole library links
    because the library obviously has Arrow. The failure appears only in a downstream project that
    depends on this module alone, as an undefined-symbol wall naming `create_parquet_reader` and
    the rest of parquet_wrapper.cpp -- which reads as a build misconfiguration rather than as a
    dependency defect, so it is unlikely to be reported back as one.

    So: walk the `use` graph from src/parquet_strings.f90 and fail if parquet_bindings is
    reachable. The fix, if this fires, is not to delete the import but to move whatever is needed
    into `parquet_settings_base` (a leaf holding the settings state that a no-C++ module may read)
    and import that instead -- see its header.
    """
    use_re = re.compile(r"^\s*use\s*(?:,\s*intrinsic\s*)?(?:::)?\s*([A-Za-z_]\w*)", re.M)
    seen, queue, edges = set(), ["parquet_strings"], {}
    while queue:
        mod = queue.pop()
        if mod in seen:
            continue
        seen.add(mod)
        path = SRC / (mod + ".f90")          # filename == unit name is enforced elsewhere
        if not path.is_file():
            continue                          # intrinsic or external module: nothing to walk
        for used in use_re.findall(path.read_text()):
            used = used.lower()
            edges.setdefault(mod, set()).add(used)
            queue.append(used)
    if "parquet_bindings" not in seen:
        return []
    # Reconstruct one shortest path for the message -- "it is reachable" is not actionable on its
    # own, and the offending edge is usually several modules away from parquet_strings.
    trail, frontier = {"parquet_strings": ["parquet_strings"]}, ["parquet_strings"]
    while frontier:
        mod = frontier.pop(0)
        for used in sorted(edges.get(mod, ())):
            if used not in trail:
                trail[used] = trail[mod] + [used]
                frontier.append(used)
    chain = " -> ".join(trail.get("parquet_bindings", ["parquet_strings", "parquet_bindings"]))
    return [
        "src/parquet_strings.f90: reaches parquet_bindings through %s, so a program whose only "
        "import is `use parquet_strings` can no longer link without the Arrow/Parquet C++ stack -- "
        "the independence doc/pages/types/string-columns.md promises. Move whatever is needed into "
        "parquet_settings_base (a leaf) and import that instead" % chain
    ]


def check_parquet_random_stays_leaf():
    """`parquet_random` may import a project module only if that module is itself standalone-
    compilable, and only if both standalone scripts already list it.

    Two checks compile single files with no dependency resolver and no Arrow install, and they are
    the only evidence for properties nothing else can reach. `tools/check_random_kernels.sh` builds
    both arms of the route (e) `#ifdef` fork and asserts they agree — the fork exists because
    gfortran has been observed miscompiling the unprotected Philox round at -O3, silently, and no
    other check compiles the wrapping arm at all. `tools/check_exp_key.sh` does the same for the
    frozen `-log(u)` transform.

    **The failure this closes has already happened.** The weighted draw briefly lived in
    `parquet_random`, which put `use parquet_sorting` in it — hence `parquet_bindings`, hence Arrow.
    Every configuration in the kernel check then died on a missing `parquet_sorting.mod` and the
    script exited saying it proved nothing, while `fpm build` and `fpm test` stayed green. The only
    signal was a check that had quietly stopped being able to run.

    **The rule is NOT "imports nothing outside `iso_fortran_env`", and that wording was wrong.**
    `parquet_settings_base` imports `omp_lib` inside `#ifdef _OPENMP` — yet it was the FIRST entry
    in `check_random_kernels.sh`'s SRC list until the `parquet_sampling` split, and that script
    passed throughout, because it compiles with no `-fopenmp` so the import never happens. The
    literal wording would have condemned a module with a demonstrated track record, for a reason
    unrelated to the hazard. The hazard is a dependency the scripts cannot satisfy, which in
    practice means reaching `parquet_bindings`.

    So this checks two things:

    1. Every project module `parquet_random` reaches, transitively, must itself reach nothing but
       COMPILER-SUPPLIED modules. That admits `parquet_expkey`, a future `parquet_ziggurat`, and
       `parquet_settings_base`; it rejects `parquet_sorting`, which reaches `parquet_bindings`.
    2. Every such module must appear in BOTH scripts' `SRC` lists. They are plain ordered compiles,
       so a missing entry is a hard failure and a stale one is worse — the script then silently
       compiles a different set than this rule believes.

    If clause 1 fires, the fix is to move whatever needed the import into `src/parquet_sampling.f90`
    — never to add the offending module to a script's SRC list, which would make this check pass
    while destroying the property it measures. If clause 2 fires, add the module to both scripts.
    """
    use_re = re.compile(r"^\s*use\s*(?:,\s*intrinsic\s*)?(?:::)?\s*([A-Za-z_]\w*)", re.M)
    # Modules the compiler supplies. A dependency on one of these is not a dependency the
    # standalone scripts have to satisfy, so the walk stops here rather than at iso_fortran_env.
    SUPPLIED = {"iso_fortran_env", "iso_c_binding", "ieee_arithmetic", "ieee_exceptions",
                "ieee_features", "omp_lib", "omp_lib_kinds"}
    root = SRC / "parquet_random.f90"
    if not root.is_file():
        return ["tools/check_source_conventions.py: src/parquet_random.f90 is missing, so the "
                "leaf check cannot run -- this check must never pass by finding nothing"]

    def imports_of(mod):
        f = SRC / (mod + ".f90")
        return sorted({m.lower() for m in use_re.findall(f.read_text())}) if f.is_file() else []

    # Transitive closure of PROJECT modules reachable from parquet_random, keeping one shortest
    # chain per module: "it is reachable" is not actionable when the offending edge is several
    # modules away, which is the same reasoning check_parquet_strings_stays_leaf gives.
    trail = {m: ["parquet_random", m] for m in imports_of("parquet_random")}
    frontier, reached = list(trail), set()
    while frontier:
        mod = frontier.pop(0)
        if mod in SUPPLIED or mod in reached or not (SRC / (mod + ".f90")).is_file():
            continue
        reached.add(mod)
        for nxt in imports_of(mod):
            if nxt not in trail:
                trail[nxt] = trail[mod] + [nxt]
                frontier.append(nxt)

    problems = []
    # The hazard, stated once with the chain that causes it.
    if "parquet_bindings" in reached:
        problems.append(
            "src/parquet_random.f90 reaches parquet_bindings through %s, so it can no longer be "
            "compiled standalone and tools/check_random_kernels.sh / tools/check_exp_key.sh cannot "
            "build the kernel alone -- the only check comparing the two arms of the route (e) fork. "
            "Move whatever needs the import into src/parquet_sampling.f90; do NOT add a module to a "
            "script's SRC list to silence this" % " -> ".join(trail["parquet_bindings"]))
    # An import that is neither compiler-supplied nor a file in src/ is a dependency the scripts
    # have no way to satisfy at all.
    for mod in sorted(reached):
        outside = sorted({m for m in imports_of(mod)
                          if m not in SUPPLIED and not (SRC / (m + ".f90")).is_file()})
        if outside:
            problems.append(
                "src/parquet_random.f90 reaches %s (via %s), which imports %s -- neither a "
                "compiler-supplied module nor a file in src/, so the standalone scripts cannot "
                "satisfy it" % (mod, " -> ".join(trail[mod]), ", ".join(outside)))
    if problems:
        return problems

    # Clause 2: every script that COMPILES parquet_random must list everything the closure names.
    #
    # The script list is derived rather than enumerated, because an enumerated one goes stale in
    # the direction that stops checking: a new standalone script would simply never be looked at.
    # `tools/check_exp_key.sh` is correctly excluded by this rule and must stay excluded -- it
    # compiles `parquet_expkey` ALONE, which is the whole reason the frozen transform can be swept
    # across compilers at all, and requiring parquet_random's closure there would force that
    # script to grow the very dependency it exists without.
    scripts = sorted(p for p in (REPO_ROOT / "tools").glob("*.sh")
                     if "src/parquet_random.f90" in p.read_text())
    if not scripts:
        return ["no tools/*.sh compiles src/parquet_random.f90, so the leaf rule's SRC "
                "cross-check has nothing to check -- this check must never pass by finding "
                "nothing (check_random_kernels.sh is the one that must be there)"]
    for sp in scripts:
        text = sp.read_text()
        for mod in sorted(reached):
            if ("src/%s.f90" % mod) not in text:
                problems.append(
                    "src/parquet_random.f90 reaches %s, but tools/%s does not list "
                    "src/%s.f90 in SRC -- that script is a plain ordered compile with no "
                    "dependency resolver, so it will fail to build or will silently compile a "
                    "different set than this rule assumes" % (mod, sp.name, mod))

    # And the frozen transform's own check must stay standalone, which is a property of that
    # script rather than of parquet_random: the moment it compiles the generator too, it stops
    # being able to sweep `exp_key` on a machine with no Arrow and the sweep quietly narrows.
    ek = REPO_ROOT / "tools" / "check_exp_key.sh"
    if not ek.is_file():
        problems.append("tools/check_exp_key.sh is missing, so the frozen transform is no longer "
                        "swept across compilers at all")
    elif "src/parquet_random.f90" in ek.read_text():
        problems.append("tools/check_exp_key.sh now compiles src/parquet_random.f90, which defeats "
                        "its purpose: it exists to compile src/parquet_expkey.f90 ALONE, so the "
                        "frozen -log(u) transform can be swept under compilers and flag sets "
                        "without the rest of this library being buildable")
    return problems


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

    **`KNOWN_REMAINING` is now empty: the debt is cleared and the rule is absolute.** It is kept as a
    mechanism rather than deleted, so that a future violation too large to fix in one change can be
    recorded as debt that may only shrink -- which is what an allowlist could not do.

    Four primitives exist so that a new call site never needs the allocating shape, and knowing which
    one fits is most of the work of clearing a violation: `%length(i)` measures without allocating,
    `%copy_to(i, dest)` fills a fixed-length slot, `%append_from(src, i)` moves one element between
    columns (null state included), and `%compare(i, j)` orders two elements. A loop that wants the
    whole packed payload takes `%copy_buffers` instead. `parquet_check_qc_string_compact` was once
    judged a permanent hold-out on the grounds that it needs each element's VALUE; it does, and
    `%copy_to` into a scratch buffer sized once from the column's longest element supplies it, so the
    judgement was wrong. See feature_string_parallel.md S10.
    """
    # file -> instances still to be converted. Lower a number when you fix one; never raise one.
    KNOWN_REMAINING = {}
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


#: The element-domain file whose setters trade `intent(out)` for `intent(inout)`, and the types in
#: it that rule applies to. Components are read from the declarations, never listed here.
TEMPORAL_FILE = SRC / "parquet_temporal.f90"
TEMPORAL_TYPES = ("parquet_date", "parquet_time", "parquet_timestamp")


def temporal_setters():
    """Every elemental subroutine in TEMPORAL_FILE whose passed-object dummy is one of the temporal
    types, with its declared intent and body.

    Yields `(name, lineno, type_name, intent, body_text)`. Derived entirely from the source: no
    procedure is named in this file, so a setter added later is covered without an edit here.
    """
    lines = TEMPORAL_FILE.read_text().split("\n")
    header = re.compile(r"^\s{4}(?:impure\s+)?elemental\s+subroutine\s+(\w+)")
    dummy = re.compile(r"class\((parquet_\w+)\),\s*intent\((out|inout)\)\s*::\s*self")
    name = None
    for i, line in enumerate(lines):
        match = header.match(line)
        if match:
            name, start, body = match.group(1), i + 1, []
            continue
        if name is None:
            continue
        if re.match(r"^\s{4}end\s+subroutine\s+%s\b" % re.escape(name), line):
            text = "\n".join(body)
            found = dummy.search(text)
            if found and found.group(1) in TEMPORAL_TYPES:
                yield name, start, found.group(1), found.group(2), text
            name = None
        else:
            body.append(line)


def check_temporal_setters_assign_all():
    """feature_risks.md Risk-70 -- an `intent(inout)` setter must leave no component stale."""
    problems = []
    if not TEMPORAL_FILE.exists():
        return ["%s: not found -- this check needs updating" % TEMPORAL_FILE]
    components = {}
    for type_name in TEMPORAL_TYPES:
        body = type_body_lines(TEMPORAL_FILE, type_name)
        if body is None:
            return [
                "%s: could not find `type :: %s` -- this check needs updating"
                % (TEMPORAL_FILE.relative_to(REPO_ROOT), type_name)
            ]
        names = []
        for _, line in body:
            found = re.match(r"^\s+(?:integer|logical|real|character)[^:]*::\s*(\w+)", strip_comment(line))
            if found:
                names.append(found.group(1))
        components[type_name] = names
    setters = list(temporal_setters())
    if not setters:
        # An empty result means the declarations moved, not that the invariant holds -- see
        # CLAUDE.md, "A static check that enumerates names goes stale silently".
        return [
            "%s: found no elemental temporal setter at all -- the declaration shape changed and "
            "this check has gone blind" % TEMPORAL_FILE.relative_to(REPO_ROOT)
        ]
    for name, lineno, type_name, intent, body in setters:
        code = "\n".join(strip_comment(line) for line in body.split("\n"))
        returns = re.search(r"\breturn\b", code) is not None
        if intent == "out":
            # Keeping intent(out) is only justified by a path that returns without assigning self.
            if not returns:
                problems.append(
                    "%s:%d: `%s` has no caught-failure return, so its `class(%s), intent(out) :: "
                    "self` costs a per-element runtime default-initialisation for nothing -- use "
                    "intent(inout) and assign every component (see CLAUDE.md, \"A polymorphic "
                    "intent(out) dummy is not free\")"
                    % (TEMPORAL_FILE.relative_to(REPO_ROOT), lineno, name, type_name)
                )
            continue
        if returns:
            problems.append(
                "%s:%d: `%s` is `intent(inout)` but has a path that RETURNS without assigning "
                "self -- that path used to yield a null element and now yields a stale one. "
                "Restore intent(out), or assign every component before returning."
                % (TEMPORAL_FILE.relative_to(REPO_ROOT), lineno, name)
            )
            continue
        # Delegating to another setter with `self` as the actual argument assigns everything the
        # callee assigns; the callee is checked in its own right, so that discharges the obligation.
        if re.search(r"call\s+\w+\s*\(\s*self\s*,", code):
            continue
        missing = [c for c in components[type_name] if not re.search(r"self%%%s\s*=" % re.escape(c), code)]
        if missing:
            problems.append(
                "%s:%d: `%s` takes `class(%s), intent(inout) :: self` but never assigns %s -- "
                "under intent(inout) that component keeps whatever the element held before, so a "
                "reused element comes back stale. Assign every component, or delegate to a setter "
                "that does (feature_risks.md Risk-70)."
                % (
                    TEMPORAL_FILE.relative_to(REPO_ROOT),
                    lineno,
                    name,
                    type_name,
                    ", ".join("`%s`" % c for c in missing),
                )
            )
    return problems


def _page_index_bullet_targets(body):
    """First-link targets of every bullet line in an index.md body, in order, anchors stripped."""
    targets = []
    for line in body.split("\n"):
        if not re.match(r"^\s*[-*]\s", line):
            continue
        found = re.search(r"\]\(([^)#\s]+)(?:#[^)]*)?\)", line)
        if found:
            targets.append(found.group(1))
    return targets


def _parse_page_index(path):
    """Split one doc/pages index.md into its `ordered_subpage:` entries and its body text.

    Returns (entries, body, problem) -- problem is a string when the file's frontmatter shape
    could not be parsed at all, in which case entries/body are unusable. FORD's frontmatter is
    not strict YAML (`ordered_subpage:` repeats), so this parses line by line between the two
    `---` fences.
    """
    lines = path.read_text().split("\n")
    if not lines or lines[0].strip() != "---":
        return None, None, "no frontmatter fence on line 1"
    entries = []
    for i in range(1, len(lines)):
        if lines[i].strip() == "---":
            return entries, "\n".join(lines[i + 1:]), None
        found = re.match(r"^ordered_subpage:\s*(\S+)\s*$", lines[i])
        if found:
            entries.append(found.group(1))
    return None, None, "frontmatter fence never closes"


def check_doc_page_index_consistency():
    """The doc/pages index files, their bullet lists, and the directory contents must all agree.

    Under FORD's nested-subpage mechanism a page exists on the generated site only if the chain of
    `ordered_subpage:` entries reaches it, and a group directory whose index.md is missing is
    SKIPPED SILENTLY -- warning on stderr, exit 0, the whole group absent from the site (verified
    against FORD 7.0.13; see feature_doc.md's "FORD nesting" experiments). Every index.md also
    duplicates its frontmatter order in a hand-written bullet list, and the top-level index carries
    a third hand-written list (every content page at a glance). None of that is visible to any
    compiler, test, or FORD itself. Everything here is derived from the filesystem, never from a
    hardcoded page list, and an empty parse fails rather than passes -- see CLAUDE.md, "A static
    check that enumerates names goes stale silently".
    """
    problems = []
    pages = REPO_ROOT / "doc" / "pages"
    top = pages / "index.md"
    if not top.is_file():
        return ["doc/pages/index.md: missing -- FORD has no user-guide landing page at all"]

    group_dirs = sorted(
        d for d in pages.iterdir() if d.is_dir() and not d.name.startswith(".")
    )
    # Rule 4 first (a group with no index.md vanishes from the site with exit status 0).
    index_files = [top]
    for d in group_dirs:
        if (d / "index.md").is_file():
            index_files.append(d / "index.md")
        else:
            problems.append(
                "doc/pages/%s/: no index.md -- FORD silently skips the whole group (every page "
                "in it disappears from the generated site while the build still reports success)"
                % d.name
            )

    entries_of = {}
    for index in index_files:
        rel = index.relative_to(REPO_ROOT)
        entries, body, problem = _parse_page_index(index)
        if problem:
            problems.append("%s: %s -- cannot verify this index at all" % (rel, problem))
            continue
        if not entries:
            # The empty parse fails deliberately: no entries almost certainly means the
            # frontmatter format moved, not that the guide really has no pages.
            problems.append("%s: no ordered_subpage: entries found" % rel)
            continue
        entries_of[index] = entries

        here = index.parent
        siblings = sorted(
            [p.name for p in here.iterdir() if p.suffix == ".md" and p.name != "index.md"]
            + [p.name for p in here.iterdir() if p.is_dir() and not p.name.startswith(".")]
        )
        # Rule 1: every sibling page/directory appears exactly once...
        for name in siblings:
            n = entries.count(name)
            if n == 0:
                problems.append(
                    "%s: %s exists on disk but is not in ordered_subpage: -- FORD will not "
                    "render it" % (rel, name)
                )
            elif n > 1:
                problems.append("%s: %s appears %d times in ordered_subpage:" % (rel, name, n))
        # Rule 2: ...and every entry exists on disk.
        for entry in entries:
            if entry not in siblings:
                problems.append(
                    "%s: ordered_subpage: names %s, which does not exist in %s/"
                    % (rel, entry, here.relative_to(REPO_ROOT))
                )

        # Rule 3: the body bullet list mirrors ordered_subpage, same pages, same order. A bullet
        # target `name.html` is the sibling page name.md; `grp/index.html` is the child group grp.
        # Any other target shape (the top index's flat all-pages list uses `grp/name.html`) is not
        # part of this list.
        tokens = []
        for target in _page_index_bullet_targets(body):
            found = re.fullmatch(r"([A-Za-z0-9_.-]+)\.html", target)
            if found:
                tokens.append(found.group(1) + ".md")
                continue
            found = re.fullmatch(r"([A-Za-z0-9_.-]+)/index\.html", target)
            if found:
                tokens.append(found.group(1))
        if not tokens:
            problems.append(
                "%s: found no bullet list mirroring ordered_subpage: -- the body format moved "
                "and this check has gone blind on it" % rel
            )
        elif tokens != entries:
            problems.append(
                "%s: body bullet list disagrees with ordered_subpage: -- frontmatter order is "
                "[%s], bullet order is [%s]" % (rel, ", ".join(entries), ", ".join(tokens))
            )

    # Rule 5, top-level index only: the flat every-page-at-a-glance list holds each group's
    # content pages exactly once, in depth-first ordered_subpage order, and nothing else.
    if top in entries_of and not any(p.startswith("doc/pages/index.md:") for p in problems):
        dir_entries = [e for e in entries_of[top] if (pages / e).is_dir()]
        if dir_entries:
            expected = []
            for grp in dir_entries:
                grp_index = pages / grp / "index.md"
                for entry in entries_of.get(grp_index, []):
                    if entry.endswith(".md"):
                        expected.append("%s/%s.html" % (grp, entry[: -len(".md")]))
            _, body, _ = _parse_page_index(top)
            actual = [
                t
                for t in _page_index_bullet_targets(body)
                if re.fullmatch(r"[A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+\.html", t)
                and not t.endswith("/index.html")
            ]
            if not actual:
                problems.append(
                    "doc/pages/index.md: has group subpages but no flat every-page-at-a-glance "
                    "list (targets of the form grp/name.html) -- the third hand-written list is "
                    "missing or its format moved"
                )
            elif actual != expected:
                problems.append(
                    "doc/pages/index.md: the flat every-page-at-a-glance list disagrees with a "
                    "depth-first walk of the groups -- expected [%s], found [%s]"
                    % (", ".join(expected), ", ".join(actual))
                )
    return problems


def check_no_indented_code_fence():
    """A fenced code block in doc/pages/ must start at column 0 or it is not rendered as code.

    python-markdown -- the engine FORD drives -- does not recognise a ``` fence that carries any
    leading whitespace. The fence is emitted LITERALLY inside a `<p>`, the enclosing list is closed
    before it, and because the block has become prose a blank line inside the example splits it into
    two paragraphs. The published page shows the code lines run together as running text with a
    stray ``` in them. Measured against python-markdown 3.3.4 and 3.4.4 with the extensions
    fpm.toml's [extra.ford] configures: 2 spaces broken, 4 spaces broken (an indented code block
    CONTAINING the ``` line), column 0 correct.

    Nothing catches this anywhere else. The source looks right -- indenting an example under the
    bullet it belongs to is what every other markdown renderer wants -- FORD exits 0, and no test
    reads the generated HTML. Six blocks across three pages were published this way.

    The scan is by shape rather than by a page list, and an empty scan FAILS: no pages found almost
    certainly means the guide moved, not that the guide has no pages (CLAUDE.md, "A static check
    that enumerates names goes stale silently"). Fences already at column 0 are tracked so that a
    ``` shown INSIDE an example is not mistaken for a real one.
    """
    problems = []
    pages = sorted((REPO_ROOT / "doc" / "pages").glob("**/*.md"))
    if not pages:
        return ["doc/pages/: no .md pages found -- this check has gone blind on the guide"]
    for page in pages:
        rel = page.relative_to(REPO_ROOT)
        in_fence = False
        for lineno, line in enumerate(page.read_text().split("\n"), start=1):
            if re.match(r"^```", line):
                in_fence = not in_fence
                continue
            if in_fence:
                continue
            if re.match(r"^\s+```", line):
                problems.append(
                    "%s:%d: indented code fence -- python-markdown renders it as PROSE, ``` and "
                    "all. Put the fence and its content at column 0 (the list closes before it), "
                    "or restructure the bullet into a subheading." % (rel, lineno)
                )
    return problems


def check_maml_keys_case_insensitive():
    """A MAML block header must never be recognized by a literal case-SENSITIVE comparison.

    Every MAML key is case-insensitive -- `parquet_find_maml_section` lowercases both sides, so
    `Extra:` validates exactly like `extra:`. The block LOCATORS did not: they compared
    `trim(adjustl(line)) == "extra:"` against a lowercase literal, so a MAML spelling its section
    `Extra:` passed validation while its `protected_cols:`, `col_map:`, `remap:`, `filter:` and
    `sort:` were silently never found -- no warning, and a file that looks right. The sharpest
    case is `protected_cols:`, where the Null protection a user asked for simply disappears.

    So a header is matched with `parquet_maml_key_matches` (parquet_core's subtree) or
    `maml_key_matches` (parquet_tables_maml.f90's own twin -- that file deliberately carries its
    own parsing primitives). Two copies of the predicate cannot drift in any harmful way; a NEW
    site forgetting both is the real hazard, and it is what this check catches. See
    feature_risks.md Risk-91.

    Matched by shape, not by a file list, and an empty scan FAILS (CLAUDE.md, "A static check that
    enumerates names goes stale silently"). Only a `"<word>:"` literal counts -- a comparison
    against a variable, or against a value rather than a key, is none of this check's business.
    """
    problems = []
    sources = sorted((REPO_ROOT / "src").glob("*.f90"))
    if not sources:
        return ["src/: no .f90 sources found -- this check has gone blind"]
    # A literal key comparison: == "something:" (or /=), where the literal ends in a colon.
    pattern = re.compile(r'[=/]=\s*"[A-Za-z_][A-Za-z0-9_]*:"')
    scanned = 0
    for source in sources:
        rel = source.relative_to(REPO_ROOT)
        for lineno, line in enumerate(source.read_text().split("\n"), start=1):
            code = line.split("!")[0]
            if not pattern.search(code):
                continue
            scanned += 1
            problems.append(
                "%s:%d: a MAML block header compared against a lowercase literal -- a "
                "capitalized `Extra:`/`Fields:` would then be silently ignored. Use "
                "parquet_maml_key_matches (or maml_key_matches in parquet_tables_maml.f90) "
                "instead: %s" % (rel, lineno, code.strip())
            )
    return problems


def _logical_lines(path):
    """Yield (lineno of the first physical line, joined code) with comments and `&` folded away."""
    out = []
    buf = ""
    start = None
    for lineno, raw in enumerate(path.read_text().split("\n"), start=1):
        code = strip_comment(raw).strip()
        if not code and not buf:
            continue
        if start is None:
            start = lineno
        if code.endswith("&"):
            buf += code[:-1] + " "
            continue
        buf += code
        if buf.strip():
            out.append((start, buf.strip()))
        buf = ""
        start = None
    return out


def _split_args(text):
    """Split an argument list on its TOP-LEVEL commas, respecting nesting and quotes."""
    parts, depth, cur, quote = [], 0, "", None
    for ch in text:
        if quote:
            cur += ch
            if ch == quote:
                quote = None
            continue
        if ch in ("'", '"'):
            quote = ch
            cur += ch
        elif ch in "([":
            depth += 1
            cur += ch
        elif ch in ")]":
            depth -= 1
            cur += ch
        elif ch == "," and depth == 0:
            parts.append(cur.strip())
            cur = ""
        else:
            cur += ch
    if cur.strip():
        parts.append(cur.strip())
    return parts


#: A procedure header, with or without an argument list. Three shapes have to parse or the frame
#: stack unwinds against the wrong procedure and later signatures are silently misattributed:
#:   - `[pure|elemental|module ...] subroutine NAME(args)`
#:   - a TYPED function, `[module] logical function NAME(args)` / `integer(int64) function ...`
#:     -- over a hundred of these, and omitting the type prefix leaves every one unparsed;
#:   - `module procedure NAME`, the abbreviated implementation form, whose `end procedure` must
#:     pop the frame it opened. It carries no argument list, so it is recorded as a frame but
#:     never as a SIGNATURE -- the interface body in parquet_core.f90 is the canonical one, and
#:     letting the empty form overwrite it is what made this check miss every generic.
_PROC_PREFIX = r"(?:(?:pure|impure|elemental|recursive|non_recursive|module)\s+)*"
_TYPE_PREFIX = r"(?:(?:integer|real|logical|complex|character|double\s+precision|type|class)" \
               r"(?:\s*\([^)]*\))?\s+)?"
_PROC_OPEN = re.compile(
    r"^" + _PROC_PREFIX + r"(?:"
    r"subroutine\s+([a-z_]\w*)\s*(?:\(([^)]*)\))?"
    r"|" + _TYPE_PREFIX + r"function\s+([a-z_]\w*)\s*(?:\(([^)]*)\))?"
    r"|(procedure)\s+([a-z_]\w*)\s*$"
    r")",
    re.I,
)
_PROC_CLOSE = re.compile(r"^end\s*(?:subroutine|function|procedure)\b", re.I)
_INTENT = re.compile(r"\bintent\s*\(\s*(in\s*out|out|in)\s*\)", re.I)
#: `interface <generic>` opening a NAMED generic (a bare `interface` is a plain interface block).
_GENERIC_OPEN = re.compile(r"^(?:abstract\s+)?interface\s+([a-z_]\w*)\s*$", re.I)
_GENERIC_MEMBER = re.compile(r"^module\s+procedure\s+([a-z_]\w*)\s*$", re.I)
#: A type-bound binding: `procedure :: name => impl`, `generic :: name => impl1, impl2`, and the
#: bare `procedure :: name` form whose binding name IS the procedure name. Missing that last form
#: is what left `col%append_buffers` unresolvable when this check was first written.
_BINDING = re.compile(r"^(?:generic|procedure)\s*(?:,[^:(]*)?::\s*(.+)$", re.I)
#: `call name(...)` or `call obj%binding(...)`.
_CALL = re.compile(r"\bcall\s+([a-z_]\w*(?:%[a-z_]\w*)*)\s*\(", re.I)
#: An actual argument that is a plain variable reference -- the only kind that can alias.
_PLAIN_ACTUAL = re.compile(r"^[a-z_]\w*(?:%[a-z_]\w*)*$", re.I)
_KEYWORD_ACTUAL = re.compile(r"^([a-z_]\w*)\s*=(?!=|>)\s*(.*)$", re.I)


def _fortran_signatures(paths):
    """Index every procedure, named generic and type-bound binding across `paths`.

    Returns (signatures, generics, bindings, unbalanced):
      signatures  name -> (ordered dummy names, {dummy: "in"|"out"|"inout"})
      generics    generic name -> [specific names]
      bindings    binding name -> [implementation names]
      unbalanced  parser-failure messages -- a file whose frames did not all close

    Interface bodies and full definitions both land in `signatures`; they agree on dummy names by
    construction here (a submodule that restates an interface restates it verbatim), so their
    intents are merged rather than treated as a conflict.

    `unbalanced` is what stops this going quietly wrong: a header shape the regex does not know
    pushes no frame, its `end` pops somebody else's, and every later signature in that file is
    attributed to the wrong procedure -- producing both false negatives and false positives with
    nothing to show for it. Two such shapes were live when the check was written.
    """
    signatures, generics, bindings, unbalanced = {}, {}, {}, []
    for path in paths:
        stack = []
        generic_name = None
        for _, code in _logical_lines(path):
            generic_open = _GENERIC_OPEN.match(code)
            if generic_open:
                generic_name = generic_open.group(1).lower()
                continue
            if re.match(r"^end\s*interface\b", code, re.IGNORECASE):
                generic_name = None
                continue
            member = _GENERIC_MEMBER.match(code)
            if member and generic_name and not stack:
                generics.setdefault(generic_name, []).append(member.group(1).lower())
                continue
            binding = _BINDING.match(code)
            if binding and not stack:
                declared = binding.group(1)
                if "=>" in declared:
                    name, _, targets = declared.partition("=>")
                    bindings.setdefault(name.strip().lower(), []).extend(
                        t.strip().lower() for t in _split_args(targets)
                    )
                else:
                    for name in _split_args(declared):
                        bare = name.strip().lower()
                        if bare:
                            bindings.setdefault(bare, []).append(bare)
                continue
            if _PROC_CLOSE.match(code):
                if stack:
                    name, dummies, intents, abbreviated = stack.pop()
                    if not abbreviated:
                        previous = signatures.get(name)
                        if previous is None or previous[0] != dummies:
                            signatures[name] = (dummies, intents)
                        else:
                            merged = dict(previous[1])
                            merged.update(intents)
                            signatures[name] = (dummies, merged)
                continue
            opened = _PROC_OPEN.match(code)
            if opened:
                abbreviated = bool(opened.group(5))
                name = (opened.group(1) or opened.group(3) or opened.group(6)).lower()
                raw_args = opened.group(2) or opened.group(4) or ""
                dummies = [d.strip().lower() for d in _split_args(raw_args) if d.strip()]
                stack.append([name, dummies, {}, abbreviated])
                continue
            if stack and "::" in code:
                found = _INTENT.search(code)
                if found:
                    intent = found.group(1).lower().replace(" ", "")
                    for declared in _split_args(code.split("::", 1)[1]):
                        bare = declared.split("(")[0].split("=")[0].strip().lower()
                        if bare:
                            stack[-1][2][bare] = intent
        if stack:
            unbalanced.append(
                "%s: %d procedure header(s) left unclosed (%s) -- the signature parser in "
                "check_no_aliased_output_argument has lost track of this file, so its intents "
                "are unreliable. Fix the parser rather than the source."
                % (path.name, len(stack), ", ".join(frame[0] for frame in stack[:3]))
            )
    return signatures, generics, bindings, unbalanced


def _writable_positions(callee, count, signatures, generics, bindings):
    """Which of `count` actual-argument positions can the callee DEFINE? None if unresolvable.

    Returns a dict {position: intent} covering only the positions whose dummy is intent(out) or
    intent(inout). A generic or a type-bound binding is resolved against every candidate specific
    and the answers unioned, so a position counts as writable if ANY specific can write it.
    """
    if "%" in callee:
        candidates = bindings.get(callee.rsplit("%", 1)[1])
        offset = 1  # the passed-object dummy is implicit at the call site
    else:
        candidates = generics.get(callee) or ([callee] if callee in signatures else None)
        offset = 0
    if not candidates:
        return None
    writable = {}
    resolved_any = False
    for specific in candidates:
        for name in generics.get(specific, [specific]):
            signature = signatures.get(name)
            if signature is None:
                continue
            resolved_any = True
            dummies, intents = signature
            for position in range(count):
                index = position + offset
                if index < len(dummies):
                    intent = intents.get(dummies[index])
                    if intent in ("out", "inout"):
                        writable[position] = intent
    return writable if resolved_any else None


def check_no_aliased_output_argument():
    """One variable must never be passed to two dummies when either of them can be DEFINED.

    F2018 15.5.2.13 forbids it, and no compiler here diagnoses it. The shape that shipped was

        call parquet_resolve_output_name(writer, outname, outname)

    whose dummies are `character(len=*), intent(in) :: name` and `character(len=:), allocatable,
    intent(out) :: output_name`. `intent(out)` on an allocatable deallocates it ON ENTRY, so the
    callee's own `intent(in)` dummy is left pointing at freed memory and the `trim(name)` inside it
    reads it. On macOS the block read back as blanks, so the column name became "" and the metadata
    update it guarded silently matched nothing and did nothing; on CI's glibc the same read
    segfaulted inside parquet_get_enabled_column_index. One bug, one silent failure and one crash,
    depending only on the allocator.

    Nothing else can catch this. It compiles clean, and the whole suite passed on the machine where
    it merely produced the wrong answer.

    Scope: an actual that is a plain variable reference, which is the only kind that can alias. The
    check is deliberately built the other way round from "resolve every call" -- duplicates are
    found FIRST (there are single digits of them tree-wide) and only then resolved, so an
    unresolvable callee is REPORTED rather than skipped. Resolving every call site instead would
    leave thousands unresolved behind an [ok], which is exactly how a static check goes blind (see
    CLAUDE.md, "A static check that enumerates names goes stale silently"). Overlap between a
    parent and its own component (`f(t%cache, t%cache%reader)`) is a related hazard this does NOT
    cover -- see CLAUDE.md's "A component and its parent cannot both be actual arguments of one
    call", where the shipped API takes an optional argument specifically to avoid it.
    """
    problems = []
    sources = sorted(SRC.glob("*.f90")) + sorted(TEST.glob("*.f90"))
    if not sources:
        return ["src/ and test/: no .f90 sources found -- this check has gone blind"]
    signatures, generics, bindings, unbalanced = _fortran_signatures(sources)
    problems.extend(unbalanced)
    if not signatures:
        return ["no procedure signatures parsed -- this check has gone blind"]
    for source in sources:
        rel = source.relative_to(REPO_ROOT)
        for lineno, code in _logical_lines(source):
            for call in _CALL.finditer(code):
                depth, end = 0, None
                for i in range(call.end() - 1, len(code)):
                    if code[i] == "(":
                        depth += 1
                    elif code[i] == ")":
                        depth -= 1
                        if depth == 0:
                            end = i
                            break
                if end is None:
                    continue
                actuals = _split_args(code[call.end():end])
                # Keyword arguments are mapped by position too: a repeated actual is what matters,
                # and every duplicate found here so far is positional.
                plain = []
                for position, actual in enumerate(actuals):
                    keyword = _KEYWORD_ACTUAL.match(actual)
                    text = keyword.group(2).strip() if keyword else actual
                    plain.append(text.lower() if _PLAIN_ACTUAL.match(text) else None)
                repeated = {a for a in plain if a and plain.count(a) > 1}
                if not repeated:
                    continue
                callee = call.group(1).lower()
                writable = _writable_positions(
                    callee, len(actuals), signatures, generics, bindings
                )
                for actual in sorted(repeated):
                    positions = [i for i, a in enumerate(plain) if a == actual]
                    if writable is None:
                        problems.append(
                            "%s:%d: `call %s` passes '%s' at argument positions %s, and this "
                            "check cannot resolve %s to check its dummies' intents. Argument "
                            "aliasing is only legal when NEITHER dummy is ever defined "
                            "(F2018 15.5.2.13) -- confirm that by hand, and give the callee a "
                            "signature this check can find."
                            % (rel, lineno, callee, actual,
                               [p + 1 for p in positions], callee)
                        )
                        continue
                    written = {p: writable[p] for p in positions if p in writable}
                    if written:
                        problems.append(
                            "%s:%d: `call %s` passes '%s' at argument positions %s, where "
                            "position(s) %s are intent(%s). One variable given to two dummies "
                            "when either can be defined is illegal (F2018 15.5.2.13) and is not "
                            "diagnosed -- an allocatable intent(out) dummy is deallocated on "
                            "entry, leaving the other dummy pointing at freed memory. Use a "
                            "separate variable for the input."
                            % (rel, lineno, callee, actual, [p + 1 for p in positions],
                               [p + 1 for p in sorted(written)],
                               "/".join(sorted(set(written.values()))))
                        )
    return problems


def check_build_tree_names_carry_the_compiler():
    """A wrapper that names its own `FPM_BUILD_DIR` must put the COMPILER in the name.

    `tools/run_error_scenarios.sh` resolves its executable with
    `find "${FPM_BUILD_DIR:-build}" -type f -name error_scenarios | head -n 1`. fpm keeps different
    compilers' objects apart in its own per-compiler subdirectory, but that lookup does not -- so a
    build tree named for the CONFIGURATION alone collects one `error_scenarios` per toolchain the
    moment someone runs the wrapper under a second `FPM_FC`, and the lookup then picks one
    arbitrarily.

    **The failure is a false green, not an error**: the run prints "error scenarios : PASS" having
    tested the other compiler's binary. It cost real coverage twice before it was fixed -- one
    machine deleted the trees by hand between ifx and gfortran, and another declined to run a second
    compiler at all rather than risk it.

    Every wrapper carries a compiler tag today. This check exists so the next one does too, because
    nothing about writing a new benchmark wrapper suggests the tree name is load-bearing, and the
    consequence of omitting it is invisible until two toolchains are compared. Matched by SHAPE --
    any `FPM_BUILD_DIR=` assignment naming a literal path -- rather than from a list of known
    scripts, so a new wrapper is covered without editing this check.
    """
    assign_re = re.compile(r"""FPM_BUILD_DIR=["']?([^"'\s]+)""")
    # A tree name is usually held in a variable; follow one level, allowing the
    # `local`/`export`/`declare` prefixes a shell function uses.
    DECL_RE = r"^\s*(?:local\s+|export\s+|declare\s+\S+\s+)?%s=(.+)$"
    problems = []
    for path in sorted(TOOLS.glob("*.sh")):
        text = path.read_text(encoding="utf-8", errors="replace")
        for lineno, line in enumerate(text.splitlines(), 1):
            if "FPM_BUILD_DIR:-" in line:      # a READ with a default, not an assignment
                continue
            m = assign_re.search(line)
            if not m:
                continue
            value = m.group(1)
            # A tree named by a variable defined elsewhere in the script is resolved by looking at
            # what that variable itself expands to, so `bdir` and friends are followed one level.
            names = re.findall(r"\$\{?(\w+)", value)
            expanded = value
            for name in names:
                for dm in re.finditer(DECL_RE % re.escape(name), text, re.M):
                    expanded += " " + dm.group(1)
            if not re.search(r"\bfc\b|FC_TAG|\btag\b|compiler", expanded, re.I):
                problems.append("%s:%d: build tree '%s' does not vary by compiler"
                                % (path.relative_to(REPO_ROOT), lineno, value))
    if problems:
        return ["a benchmark wrapper's build tree must include the compiler, or a second toolchain's",
                "error_scenarios binary lands in the same tree and `find ... | head -n 1` picks one",
                "arbitrarily -- a false green rather than an error:"] + problems
    return []



def check_fill_size_kind():
    """`parquet_random`'s bulk routines must take an array length as `size(v, kind=int64)`.

    `size(v)` without `kind=` returns a DEFAULT-kind integer, which wraps for an array of 2**31
    elements or more. In a fill loop that failure is silent, and it came in two flavours -- both
    measured on this module before the fix:

      * 2**31 elements exactly gave a length of -2147483648, which tripped the zero-size guard, so
        the routine returned having written NOTHING and the caller's `intent(out)` array was left
        undefined;
      * 2**32 + 8 elements gave a length of 8, so eight values were written and the remaining
        4.29 billion were left undefined.

    Neither raises anything, and neither is reachable from `fpm test`: the smallest array that gets
    there is 2**31 `real32` values, about 8.6 GB. That size is ordinary for the data this library
    exists to handle -- one draw per row of a three-billion-row table lands squarely in it. Coverage
    cannot see it either, because the line executes normally, just with a wrapped value. So this
    static check is the cheap guard, and `tools/test_random_large_fill.sh` is the end-to-end proof
    that has to be run by hand.

    **Scoped to `src/parquet_random.f90` and `src/parquet_sampling.f90` on purpose** (the bulk fills
    live in one, the permutation/resample fills in the other). Within them the match is by SHAPE -- any
    bare `size(x)` at all -- so a bulk routine added later is covered without editing this check.
    It is NOT repo-wide: `src/` carries about 200 other bare `size(...)` calls, nearly all of them
    on arrays whose length is bounded by construction (a column's width, a field count, a schema's
    size). Those have not been audited, and asserting a 200-entry debt this check has not verified
    would be worse than leaving them alone. Whether any of them takes an unbounded caller array is
    a separate question worth its own pass.
    """
    paths = [SRC / "parquet_random.f90", SRC / "parquet_sampling.f90"]
    missing = [p.name for p in paths if not p.is_file()]
    if missing:
        return ["tools/check_source_conventions.py: src/%s not found -- this check has gone stale "
                "and is silently testing part of its scope or none of it" % ", src/".join(missing)]
    problems = []
    for path in paths:
        text = path.read_text(encoding="utf-8", errors="replace")
        for lineno, line in enumerate(text.splitlines(), 1):
            code = line.split("!", 1)[0]
            for m in re.finditer(r"\bsize\s*\(\s*([A-Za-z_]\w*)\s*\)", code):
                problems.append("%s:%d: size(%s) has no kind= -- a default-kind length wraps above "
                                "2**31 elements" % (path.relative_to(REPO_ROOT), lineno, m.group(1)))
    if problems:
        return ["a bulk array length in parquet_random/parquet_sampling must be taken as",
                "size(v, kind=int64): a",
                "default-kind result wraps above 2**31 elements and fails SILENTLY, writing nothing",
                "or writing a short prefix of the caller's intent(out) array:"] + problems
    return []


def check_allocate_extent_kind():
    """An `allocate` extent taken from `size(...)` must ask for an `int64` result.

    `size(x)` without `kind=` returns a DEFAULT-kind integer, which wraps above 2**31 elements. In
    an allocate extent that is worse than the silent short-fill it causes elsewhere, because the
    loop that follows usually gets its bound RIGHT:

        allocate(arr(size(rows)))                 ! wraps: negative -> zero-length array
        do k = 1, size(rows, kind=int64)          ! correct: runs the full count
            call parquet_column_get_at(..., arr(k))   ! writes past the end

    That is a heap overflow on a valid call, not a wrong answer. It was found in exactly this
    shape at ten sites in the generated table accessors plus both `table_valid_mask_rows` variants,
    where the loop bound on the very next line already carried `kind=int64` -- so the hazard was
    understood and the allocate was simply missed. Element assignment does not reallocate, which
    is what makes this class worse than the sibling `arr = p` shape, where intrinsic assignment to
    an allocatable resizes and hides the mistake.

    Matched by SHAPE across all of `src/`, not from a list of files: any `size(` inside an
    `allocate(` without a `kind=`. Writing `kind=int64` costs nothing even where the extent is
    provably small, so there is no exemption list to go stale -- and a check with no exemptions is
    one nobody has to maintain. Deliberately narrower than "every `size()` call": `src/` carries
    roughly 200 of those, nearly all on arrays bounded by construction, and only the allocate
    shape turns a wrapped length into an out-of-bounds write.
    """
    problems = []
    for path in sorted(SRC.glob("*.f90")):
        text = path.read_text(encoding="utf-8", errors="replace")
        for lineno, line in enumerate(text.splitlines(), 1):
            code = line.split("!", 1)[0]
            if "allocate(" not in code:
                continue
            tail = code[code.index("allocate("):]
            for m in re.finditer(r"\bsize\s*\(([^()]*(?:\([^()]*\))?[^()]*)\)", tail):
                if "kind=" not in m.group(1):
                    problems.append("%s:%d: allocate extent uses size(%s) with no kind="
                                    % (path.relative_to(REPO_ROOT), lineno, m.group(1).strip()))
    if problems:
        return ["an allocate extent taken from size(...) must use kind=int64: a default-kind result",
                "wraps above 2**31 elements, and the loop that fills the array usually does NOT --",
                "which makes the write go past the end rather than merely stop short:"] + problems
    return []


def check_noinline_directives_are_paired():
    """Every `noinline` marker in `parquet_random` must carry BOTH compilers' spellings.

    Three procedures in `src/parquet_random.f90` are deliberately kept OUT of line --
    `int_reduce_retry`, `int_reduce32_retry` and `int_at_narrow32` -- and each carries a matched
    pair of directives, one for GCC and one for Intel. The pairing is what this checks, by shape
    rather than by name, so a fourth such procedure is covered the day it is added.

    **Why a lint check and not a comment.** Losing one of the two spellings is invisible: the file
    compiles, every test passes, every value is identical, and one compiler silently inlines a body
    that must not be inlined. The cost is real and was measured four separate times while the
    32-bit grid was being adopted -- a bulk fill's shape pushing `int_reduce` past GCC's inline
    budget cost the SCALAR draw 9 %; the narrow arm inlined into `int_at_impl` cost the WIDE scalar
    draw 13 %; the fix for that cost the stream fills 60 %; and so on. Each was found only with
    `objdump`. See `feature_risks.md` Risk-114 and Risk-115.

    A directive is an ordinary comment to a compiler that does not know it, so carrying both is
    free; carrying one is a silent, compiler-specific de-optimisation.
    """
    path = SRC / "parquet_random.f90"
    if not path.is_file():
        return ["tools/check_source_conventions.py: src/parquet_random.f90 not found -- this check "
                "has gone stale and is silently testing nothing"]
    text = path.read_text(encoding="utf-8", errors="replace")
    gcc = set(re.findall(r"^!GCC\$ ATTRIBUTES noinline :: (\w+)", text, re.M))
    ifx = set(re.findall(r"^!DIR\$ ATTRIBUTES NOINLINE :: (\w+)", text, re.M))
    problems = []
    # The empty-result guard: if the pattern stops matching, that is a moved file, not a clean bill.
    if not gcc and not ifx:
        problems.append(
            "no noinline directives found in src/parquet_random.f90 at all -- either they were "
            "removed (a silent de-optimisation on both compilers) or this check's pattern has gone "
            "stale; either way it must not report success")
    for name in sorted(gcc - ifx):
        problems.append("%s has !GCC$ ATTRIBUTES noinline but no matching "
                        "!DIR$ ATTRIBUTES NOINLINE -- ifx will inline it" % name)
    for name in sorted(ifx - gcc):
        problems.append("%s has !DIR$ ATTRIBUTES NOINLINE but no matching "
                        "!GCC$ ATTRIBUTES noinline -- gfortran will inline it" % name)
    return problems


CHECKS = (
    ("benchmark build-tree names carry the compiler", check_build_tree_names_carry_the_compiler),
    ("parquet_table has no allocatable component", check_no_allocatable_component),
    ("MAML block headers are matched case-insensitively", check_maml_keys_case_insensitive),
    ("table pointers are reached through %cache", check_pointers_go_through_cache),
    ("no per-cell path reaches a column through a binding", check_no_type_bound_column_access),
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
    ("parquet_strings does not reach parquet_bindings", check_parquet_strings_stays_leaf),
    ("parquet_random imports nothing from src/", check_parquet_random_stays_leaf),
    ("no per-element helper takes a shared_ptr", check_no_per_element_shared_ptr),
    ("no per-element string allocation in a bulk loop", check_no_per_element_string_alloc),
    ("every error scenario is named in the shell runner", check_scenario_list_is_complete),
    ("every intent(inout) temporal setter assigns all components", check_temporal_setters_assign_all),
    ("doc/pages index files agree with the page tree", check_doc_page_index_consistency),
    ("no doc/pages code fence is indented", check_no_indented_code_fence),
    ("no call aliases one variable onto a writable dummy", check_no_aliased_output_argument),
    ("parquet_random takes array lengths as int64", check_fill_size_kind),
    ("allocate extents from size() use int64", check_allocate_extent_kind),
    ("noinline directives carry both spellings", check_noinline_directives_are_paired),
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
